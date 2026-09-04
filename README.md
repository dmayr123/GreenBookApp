# Green Book Drug Finder

An R/Shiny dashboard over FDA's **Approved Animal Drug Products (Green Book)**,
built for practising veterinarians.

FDA publishes this data through [Animal Drugs @ FDA][adafda], but its search is
hard to use: punctuation is significant (`CA-1`, `-CA-1` and `CA1` return
different results), multi-word queries are treated as one literal string, and
results come back unranked. This app fixes those three things and reorganises
the data around the question a vet actually asks — *what is approved for this
species, and what does the label say?*

[adafda]: https://animaldrugsatfda.fda.gov/adafda/views/#/search

## What it shows

Landing page: pick a species (the seven FDA CVM major species plus goats,
sheep, fish, honeybees, rabbits and other minor species), or search directly.

For each drug:

| Field | Source |
| --- | --- |
| NDC | DailyMed SPL (joined by name — see *Known limitations*) |
| Current status | ADAFDA application status |
| Proprietary name | ADAFDA |
| Active ingredients | ADAFDA |
| Labeler / sponsor | ADAFDA |
| Product type | Collapsed from ADAFDA type + status |
| Labelled use, dose, indication | ADAFDA, filtered to the chosen species |
| Withdrawal period | ADAFDA |
| FOI summaries, product labels, SPL | ADAFDA document endpoints |
| Pioneer product | ADAFDA `pioneerApplicationNumber` |
| Professional guidance | Curated map in `data/reference/guidelines.csv` |

Product type is collapsed from FDA's separate type and status codes into four
categories — **NADA / Approved**, **ANADA / Generic**, **Conditional
Approval**, **Emergency Use Authorization** — with withdrawal reported
separately, because a withdrawn generic is still a generic.

## Setup

Requires R ≥ 4.3.

```r
install.packages(c(
  "shiny", "bslib", "reactable", "dplyr", "tidyr", "stringr", "purrr",
  "readr", "tibble", "jsonlite", "httr2", "arrow", "fs", "glue"
))
```

Then, from the project root:

```r
source("R/01_fetch_adafda.R")     # ~15 min, ~5,000 requests
source("R/02_tidy_greenbook.R")   # ~1 min
source("R/03_ndc_dailymed.R")     # optional, ~25 min; adds NDC codes
shiny::runApp("app")
```

`01` caches every response under `data/raw/`, so an interrupted run resumes
where it stopped and a re-run costs nothing.

## How the data is obtained

Animal Drugs @ FDA is an AngularJS front end over a public, unauthenticated
REST API. FDA offers no bulk download, so the pipeline calls the same
endpoints the site's own JavaScript calls:

| Endpoint | Purpose |
| --- | --- |
| `POST /advancedSearchForExcelPdf` | Whole catalogue in one request (~2,425 applications) |
| `GET /retrievePreviewBean/{id}` | Full detail: sponsor, ingredients, species, dose, documents |
| `GET /spllink/{id}` | Structured Product Label links |
| `GET /document/downloadFoi/{id}` | FOI summary PDFs |
| `GET /codes/application_type` | NADA / ANADA / CNADA / EUA |
| `GET /monthlyUpdates` | Monthly publication index |

All requests send a descriptive `User-Agent` and sleep between calls.

## Tidy output

`02_tidy_greenbook.R` writes one parquet file per grain to `data/processed/`:

```
applications     one row per application
products         one row per proprietary name
ingredients      one row per application x active ingredient
product_species  one row per product x species, carrying the use class
dosing           one row per product x indication (dose + indication)
documents        one row per downloadable FOI / label / SPL
search_index     denormalised, one row per product, drives the search box
```

FDA's species vocabulary is inconsistent — `Equids` (105 products) and
`Horses` (2 products) are separate labels for the same animal, and
`Sheep  (Domestic)` contains a double space. `R/species_taxonomy.R` maps every
raw label onto one of thirteen groups; unrecognised labels fall into
"Other minor species" rather than disappearing.

## Monthly updates

FDA republishes the Green Book monthly. `R/04_monthly_update.R` fetches the
catalogue (one request), diffs it against the copy on disk, re-fetches detail
records only for applications that are new or changed, rebuilds the tidy
tables, and writes a dated changelog to `data/changelog/`.

The changelog calls out conditional approvals that converted to full approval
(CNADA → NADA), new approvals, and newly withdrawn products.

```bash
Rscript R/04_monthly_update.R
```

This is registered as a Windows scheduled task, **GreenBook Monthly Update**,
running `scripts/update_greenbook.cmd` on the 6th of each month at 09:00 and
logging to `logs/update_YYYY-MM.log`. FDA does not publish on a fixed day, so
the 6th leaves room for the new edition to land first.

```powershell
schtasks /Query /TN "GreenBook Monthly Update"   # check it
schtasks /Run   /TN "GreenBook Monthly Update"   # run it now
schtasks /Delete /TN "GreenBook Monthly Update" /F
```

The wrapper hard-codes the R path (`R-4.5.2`). Update it after an R upgrade or
the task will fail — it logs the reason rather than failing silently.

### FDA's application type is wrong for 4 conditional approvals

**FDA's `applicationType` field cannot be trusted to identify conditional
approvals.** It reports 7 of the 11 conditional approvals in the catalogue.
These four are typed `N` (full NADA) but are conditionally approved:

| Product | Application | FDA type | Confirmed by |
| --- | --- | --- | --- |
| CANALEVIA-CA1 | 141-552 | `N` | DailyMed: "Marketing Status: Conditional New Animal Drug Application" |
| Varenzin-CA1 | 141-571 | `N` | FDA's own indication text: "Conditionally approved for the control of nonregenerative anemia..." |
| Credelio Quattro-CA1 | 141-619 | `N` | DailyMed: "conditionally approved by FDA pending a full demonstration of effectiveness under application number 141-619" |
| Baytril 100-CA1 | 141-527 | `N` | FDA's own indication text (product is voluntarily withdrawn) |

This matters clinically: conditional approval means effectiveness has **not**
been fully demonstrated, so presenting one as a full approval misleads the
prescriber.

`detect_conditional()` in `R/02_tidy_greenbook.R` therefore unions three
signals rather than trusting the type field:

1. the mandatory `-CA1` suffix in the proprietary name (catches all 11),
2. "conditionally approved" appearing in FDA's own indication or limitation
   text,
3. `applicationType == "C"`.

Where signal 3 disagrees with the others, `applications$fdaTypeDisagrees` is
set and the drug page states plainly that FDA's own database types the
application incorrectly.

Note that a search for `CA1` also returns `Tetroxy HCA-1400` and
`Tetroxy HCA-1772`. Those are correctly classified ANADA generics; they match
only because "HCA-1400" contains the letters `ca1`.

### Comparison operators in dose text

ADAFDA stores `≤` and `≥` as the literal strings `lessThanEqualTo` and
`greaterThanEqualTo`, and its own front end converts them back before display.
The pipeline does the same in `decode_fda_signs()`. Without it a dose read
"for dogs weighing lessThanEqualTo 140 pounds" — affecting 7 products.

## Publishing as a website

An ordinary Shiny app needs a running R process, which GitHub Pages does not
provide. This project therefore also builds a **shinylive** version: R itself
is compiled to WebAssembly and the app runs entirely in the visitor's browser,
so the "server" is only a static file host.

```bash
Rscript scripts/build_static_site.R   # -> docs/
```

`.github/workflows/update-and-deploy.yml` runs that monthly (and on any push
that touches the app or pipeline), then publishes to GitHub Pages. To enable
it: **Settings → Pages → Source: GitHub Actions**.

The built site is uploaded as a Pages artifact, never committed — it is ~68 MB,
most of it the WebAssembly R runtime, and committing it monthly would grow the
repository without bound. `docs/` and `build/` are gitignored.

### Why the app reads RDS, not parquet

The pipeline writes every table twice: `.parquet` as the archival copy, and a
`.rds` twin that the app reads. shinylive decides what to ship by scanning the
app source for package references, so a single `arrow` reference in `global.R`
put the (large) arrow WebAssembly build into every visitor's download for a
code path the browser never takes. Reading RDS with base R removed it:

| | before | after |
| --- | --- | --- |
| Data payload | 3.6 MB | **1.15 MB** |
| Site on disk | 106 MB | **68 MB** |

If you add a package to the app, check what it drags into the bundle:

```bash
ls docs/shinylive/webr/packages/
```

### Trade-offs of the static build

- **First visit is slow.** The browser downloads the R runtime and packages
  (tens of MB) before the app starts. It is cached afterwards, so repeat
  visits are fast — but a vet opening it once on clinic wifi will wait.
- **No server means no server costs, and no usage limits.**
- All data is public FDA data, so shipping it to the browser is fine.

If first-load time matters more than free hosting, deploy the *ordinary* Shiny
app instead — [Posit Connect Cloud](https://connect.posit.cloud) publishes
directly from a GitHub repo and has a free tier, and shinyapps.io is the
older equivalent. Both run real R servers, so the app starts instantly and
`docs/` is not needed at all.

## Known limitations

**NDC coverage is partial.** The Green Book is organised by application
number and carries no NDC at all; NDCs live in the labeller's Structured
Product Label. `03_ndc_dailymed.R` joins them from DailyMed by normalised
trade name. Where the DailyMed title matches exactly, the NDC is shown
plainly; where only the name stem matched, the app labels it
*"matched by name stem — verify before use"*. Products with no DailyMed match
show "Not listed in DailyMed" rather than a guess.

**FDA's Index is not included.** The Index of Legally Marketed Unapproved New
Animal Drugs for Minor Species is a separate FDA list, not exposed through the
ADAFDA API, so "Index" does not appear as a status. Adding it needs its own
source.

**Dose statements are not keyed to species by FDA.** ADAFDA groups doses under
free-text population headers ("Beef cattle 2 months of age and older") with no
species code attached. The app matches those headers against the species
labels textually; when it cannot attribute them confidently it shows all
labelled doses and says so, rather than hiding doses it is unsure about.

**Guideline links are curated, not exhaustive.** `data/reference/guidelines.csv`
is a map of drug class and species to publishing organisation. It is a plain
CSV — add rows to extend it.

Links are checked **by content, not status code**. `scripts/check_guideline_links.R`
fetches each URL and requires the string in the `expect` column to appear on
the page. This matters: the International Veterinary Epilepsy Task Force's
former organisation domain returned HTTP 200 while actually redirecting to an
unsecured parked page with nothing to do with the task force. A status-code
check called that link healthy and shipped a dead reference to a clinician.

**Prefer a direct link to the open-access paper over an organisation's home
page.** Organisation domains lapse and get re-registered; a paper's DOI or PMC
identifier does not. The IVETF entry now points at the consensus report
itself.

```bash
Rscript scripts/check_guideline_links.R
```

Verdicts are `verified` (expected text found), `blocked` (403 — the site
refuses automated requests; not the same as broken), or `CONTENT MISMATCH`
(the page loads but is not what it should be — what a parked domain looks
like). The check runs monthly in CI and never fails the build.

### Guideline age limit

**A linked paper or guideline must not be more than 15 years old.** Clinical
recommendations go stale, and an out-of-date consensus statement is worse than
no link because it carries the issuing body's authority without its current
position.

This is enforced in `app/global.R` at load time (`GUIDELINE_MAX_AGE_YEARS`),
not by an annual clean-up — a rule that depends on someone remembering to run
something once a year is a rule that eventually lapses. The cutoff advances on
its own each January, and expired rows are dropped with a warning naming them.

Set the `published` column to the publication year for a dated paper. Leave it
blank for an organisation hub (AVMA's policy index, AAHA's guidelines page):
those are continuously revised, carry no single publication date, and are
never expired by age. The monthly link check reports anything expiring within
three years, so it can be replaced with a newer edition rather than silently
vanishing.

## Licensing and content

Green Book data is US Government work and not subject to copyright.

This app deliberately contains **no content from Plumb's Veterinary Drug
Handbook** or any other commercial formulary. Plumb's (© 2023 Educational
Concepts LLC / Wiley) was used only as a factual reference when assigning
pharmacologic classes to active ingredients; no text, dose or monograph from
it is reproduced or redistributed here. Extra-label dosing is out of scope by
design — the app shows FDA-labelled use only, and says so on every drug page.

## Clinical disclaimer

This tool reformats FDA's published data. It is not a substitute for reading
the approved label, and it does not provide extra-label dosing. Any use
outside the labelled species, dose, route or indication is extra-label and is
the prescriber's professional responsibility under AMDUCA.
