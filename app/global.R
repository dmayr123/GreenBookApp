# ---------------------------------------------------------------------------
# global.R -- loaded once when the Shiny app starts
#
# Everything here is read-only and shared across sessions. The processed
# parquet files are small enough (a few MB) to sit in memory, which keeps the
# whole app free of database plumbing and makes every filter a plain dplyr
# call against a data frame.
# ---------------------------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(reactable)
library(readr)
library(fs)

# The app lives in app/, the pipeline and data live one level up. Resolving
# the project root here means the app runs whether it is launched from the
# project root or from inside app/.
APP_ROOT <- if (dir_exists("R") && dir_exists("data")) "." else ".."

src  <- function(f) source(file.path(APP_ROOT, "R", f), local = FALSE)
proc <- function(f) file.path(APP_ROOT, "data", "processed", f)
ref  <- function(f) file.path(APP_ROOT, "data", "reference", f)

src("species_taxonomy.R")
src("drug_classes.R")
src("search.R")

# -- data --------------------------------------------------------------------

#' Load one processed table.
#'
#' The pipeline writes every table twice: parquet as the archival copy, and an
#' RDS twin for the app. The app reads only RDS, deliberately.
#'
#' shinylive builds the static site by scanning this source for package
#' references and shipping whatever it finds. Reading parquet here -- even
#' behind a `requireNamespace()` guard -- would put the arrow WebAssembly
#' build into every visitor's download for a code path the browser never
#' takes. RDS is read by base R, so this file names no data package at all.
read_table <- function(stem) {
  f <- proc(paste0(stem, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}

need <- c("search_index", "products", "applications", "dosing",
          "product_species", "documents", "ingredients")
missing <- need[!file.exists(proc(paste0(need, ".rds")))]
if (length(missing)) {
  stop("Missing processed data: ", paste(missing, collapse = ", "),
       "\nRun:  Rscript R/01_fetch_adafda.R  then  Rscript R/02_tidy_greenbook.R")
}

# What changed at FDA in each monthly refresh: new approvals, type or status
# changes, withdrawals, and conditional approvals converting to full approval.
# Drug changes only — guideline link and age checks are a maintenance concern
# and stay in the build logs.
UPDATE_LOG <- read_table("update_log") %||%
  tibble(runDate = as.Date(character()), kind = character(),
         applicationNumber = integer(), proprietaryName = character(),
         applicationType = character(), sponsorName = character())

UPDATE_RUNS <- read_table("update_runs") %||%
  tibble(runDate = as.Date(character()), nAdded = integer(),
         nChanged = integer(), nWithdrawn = integer(), nConverted = integer())

#' The most recent refresh, or NULL before one has run.
latest_run <- function() {
  if (nrow(UPDATE_RUNS) == 0) return(NULL)
  UPDATE_RUNS |> arrange(desc(runDate)) |> slice(1)
}

# Ingredient -> pharmacologic class, precomputed by the pipeline. Classifying
# on demand meant running ~60 regexes every time a drug page opened.
INGREDIENT_CLASSES <- read_table("ingredient_classes") %||%
  tibble(activeIngredientName = character(), drugClass = character())

LABEL_LINKS  <- read_table("label_links") %||%
  tibble(proprietaryNameId = integer(), tier = integer(), sourceName = character(),
         citation = character(), whatItIs = character(), url = character(),
         link_status = character())

SEARCH_INDEX <- read_table("search_index")
PRODUCTS     <- read_table("products")
APPLICATIONS <- read_table("applications")
DOSING       <- read_table("dosing")
SPECIES      <- read_table("product_species")
DOCUMENTS    <- read_table("documents")
INGREDIENTS  <- read_table("ingredients")

# NDC is an optional enrichment (03_ndc_dailymed.R); the app must work without
# it, showing "not listed" rather than failing to start.
NDC <- read_table("ndc") %||%
  tibble(proprietaryNameId = integer(), ndc = character(), setid = character(),
         matchType = character(), splTitle = character(), dailymedUrl = character())

# A linked paper or guideline must not be more than this many years old.
# Clinical recommendations go stale; an out-of-date consensus statement is
# worse than no link, because it carries the authority of the issuing body
# without its current position.
GUIDELINE_MAX_AGE_YEARS <- 15

#' Drop dated guidelines that have aged out.
#'
#' Enforced here, at load, rather than by an annual clean-up of the CSV. A
#' rule that depends on someone remembering to run something once a year is a
#' rule that eventually lapses silently; this way the cutoff moves on its own
#' every time the app starts.
#'
#' Rows with no `published` year are organisation hubs (AVMA's policy index,
#' AAHA's guidelines page). Those are continuously revised and carry no single
#' publication date, so they are never expired by age.
drop_expired_guidelines <- function(g, today = Sys.Date()) {
  if (!"published" %in% names(g) || nrow(g) == 0) return(g)
  cutoff <- as.integer(format(today, "%Y")) - GUIDELINE_MAX_AGE_YEARS

  expired <- !is.na(g$published) & g$published < cutoff
  if (any(expired)) {
    warning(sprintf(
      "Dropped %d guideline link(s) older than %d years (published before %d): %s",
      sum(expired), GUIDELINE_MAX_AGE_YEARS, cutoff,
      paste(unique(g$title[expired]), collapse = "; ")))
  }
  g[!expired, , drop = FALSE]
}

GUIDELINES <- if (file.exists(ref("guidelines.csv"))) {
  read_csv(ref("guidelines.csv"), show_col_types = FALSE) |>
    drop_expired_guidelines()
} else {
  tibble(match_type = character(), match_value = character(),
         organization = character(), org_type = character(),
         title = character(), url = character(), published = integer(),
         link_status = character(), note = character())
}

DATA_BUILT <- format(file.info(proc("search_index.parquet"))$mtime, "%d %b %Y")

CATEGORIES <- c("NADA / Approved", "ANADA / Generic", "Conditional Approval",
                "Emergency Use Authorization")

# -- display helpers ---------------------------------------------------------

#' Badge colour for the collapsed product category.
category_class <- function(x) {
  case_when(
    x == "Conditional Approval"        ~ "cat-conditional",
    x == "ANADA / Generic"             ~ "cat-generic",
    x == "NADA / Approved"             ~ "cat-approved",
    x == "Emergency Use Authorization" ~ "cat-eua",
    TRUE                               ~ "cat-other"
  )
}

# Conditional-approval detection lives in R/02_tidy_greenbook.R
# (detect_conditional) and reaches the app as the `isConditional` and
# `fdaTypeDisagrees` columns on APPLICATIONS. It is deliberately not
# recomputed here: FDA's applicationType field is wrong for 4 of the 11
# conditional approvals, and the correction must be applied once, in the
# pipeline, so the app and any other consumer of the tables agree.

#' Format an application number the way FDA writes it: 141262 -> "141-262".
#'
#' Matches the `appNumber` filter in FDA's own front end (zero-pad to six
#' digits, hyphen after the third), so a number copied from here can be pasted
#' straight into their search box.
fda_app_number <- function(n) {
  n <- suppressWarnings(as.integer(n))
  if (length(n) == 0 || is.na(n)) return(NA_character_)
  v <- sprintf("%06d", n)
  paste0(substr(v, 1, 3), "-", substr(v, 4, 6))
}

# Animal Drugs @ FDA is a single-page app whose per-drug route
# (#/previewsearch/{appNum}) only resolves with search state already loaded --
# opening it directly redirects to the search page. There is therefore no URL
# that links to one product, and any link claiming to do so is misleading.
# The app links to the search page and shows the formatted application number
# to paste, which is the most a link can honestly promise.
ADAFDA_SEARCH <- "https://animaldrugsatfda.fda.gov/adafda/views/#/search"

#' Render a value, or a muted placeholder when it is missing.
or_none <- function(x, placeholder = "Not listed") {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(str_trim(x[1]))) {
    return(span(class = "muted", placeholder))
  }
  x[1]
}

#' FDA free-text fields arrive as HTML fragments. They come from a government
#' API rather than user input, and we render only the small set of formatting
#' tags FDA emits, so `HTML()` is appropriate here.
fda_html <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(NULL)
  HTML(x[1])
}

#' Species tiles for the landing page, annotated with how many products each
#' one actually has. A tile with no products is still shown but dimmed, so the
#' absence is visible rather than confusing.
species_tile_counts <- function() {
  counts <- SPECIES |>
    distinct(proprietaryNameId, speciesGroup) |>
    count(speciesGroup, name = "n")

  SPECIES_GROUPS |>
    left_join(counts, by = c("group" = "speciesGroup")) |>
    mutate(n = coalesce(n, 0L))
}

SPECIES_TILES <- species_tile_counts()
