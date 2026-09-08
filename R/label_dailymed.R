# ---------------------------------------------------------------------------
# label_dailymed.R -- resolve each product to its actual label on DailyMed
#
# The Structured Product Label is the approved label itself: indications,
# dosing, withdrawal periods, the package insert content. It is what a
# clinician actually wants, so it outranks a manufacturer's marketing page.
#
# Matching is harder than it looks. A trade name like "Meloxicam Injection"
# returns a dozen DailyMed labels from a dozen labellers, and linking to the
# wrong company's label would show the wrong strengths and the wrong
# withdrawal times. Three rules are applied in descending confidence, and the
# rule used is recorded on every row so the app can say how it was matched:
#
#   1. exact  -- the normalised trade name equals the label's name
#   2. sponsor -- the stem matches and FDA's sponsor matches the SPL labeller.
#                 This is the strong one: FDA tells us whose product it is.
#   3. sole   -- the stem resolves to exactly one label, so there is nothing
#                 to confuse it with
#
# Anything else gets no direct label and falls back to a DailyMed search.
# ---------------------------------------------------------------------------

library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(fs)

DM_CACHE <- path("data", "raw", "dailymed")

dm_norm <- function(x) str_replace_all(tolower(coalesce(x, "")), "[^a-z0-9]+", "")

#' Read every SPL record the DailyMed crawl cached.
#'
#' Returns an empty frame when the cache is absent, so a first run without it
#' degrades to search links rather than failing.
read_dailymed_cache <- function() {
  if (!dir_exists(DM_CACHE)) return(tibble(
    stem = character(), setid = character(), splName = character(),
    splForm = character(), splLabeler = character(),
    splKey = character(), labelerKey = character()))

  files <- dir_ls(DM_CACHE, glob = "*.json")
  out <- map_dfr(files, function(f) {
    stem <- path_ext_remove(path_file(f))
    x <- tryCatch(fromJSON(f, simplifyVector = FALSE), error = function(e) list())
    if (length(x) == 0) return(NULL)
    map_dfr(x, function(e) {
      if (is.null(e$setid)) return(NULL)
      title <- e$title %||% ""
      tibble(
        stem = stem,
        setid = e$setid,
        # "NAME (INGREDIENT) FORM [LABELLER]" -- name is everything before the
        # first parenthesis, labeller is the trailing bracketed segment.
        splName = str_squish(str_remove(title, "\\s*\\(.*$")),
        # The dose form sits between the ingredient parenthesis and the
        # labeller bracket: "NUFLOR (FLORFENICOL) INJECTION, SOLUTION [MERCK]".
        splForm = str_squish(str_remove(
          str_extract(title, "(?<=\\))[^\\[]*") %||% "", "^\\s*")),
        splLabeler = str_squish(str_remove_all(
          str_extract(title, "\\[[^]]*\\]$") %||% "", "[\\[\\]]"))
      )
    })
  })
  if (nrow(out) == 0) return(out)
  out |> mutate(splKey = dm_norm(splName), labelerKey = dm_norm(splLabeler))
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Companies that appear under different names in FDA's data and DailyMed's.
#'
#' Animal health businesses are bought and renamed constantly, so the sponsor
#' FDA records and the labeller DailyMed records are often the same company
#' written two ways -- Intervet's labels are filed by Merck, Fort Dodge's by
#' Zoetis. Each row lists one company's aliases; membership of the same row
#' counts as a match.
#' Every row must correspond to a real acquisition or rename that can be
#' named. An invented one silently links a vet to a different company's label:
#' an earlier version grouped Phibro with Huvepharma, which are unrelated, and
#' a Phibro product duly resolved to a Huvepharma label. Do not add a row
#' without a specific corporate relationship behind it.
COMPANY_ALIASES <- list(
  # Intervet trades as Merck Animal Health; Merck merged with Schering-Plough.
  c("intervet", "merck", "msd", "scheringplough"),
  # Zoetis was spun out of Pfizer, which had acquired Wyeth and Fort Dodge.
  c("zoetis", "pfizer", "fortdodge", "wyeth"),
  # Elanco was Eli Lilly's animal health arm, and later acquired Novartis
  # Animal Health and Bayer Animal Health.
  c("elanco", "lilly", "novartisanimal", "bayeranimal"),
  # Boehringer Ingelheim acquired Merial.
  c("boehringer", "merial"),
  # Dechra acquired Putney.
  c("dechra", "putney")
)

# Words that appear in company names without identifying the company. They are
# removed before comparison: matching on a leading fragment paired
# "Pharmaceutical Ventures, Ltd." with "AX Pharmaceutical Corp" on the shared
# string "pharmac", and pointed a veterinary product at a human API supplier's
# label.
GENERIC_COMPANY_WORDS <- c(
  "pharmaceuticals", "pharmaceutical", "laboratories", "laboratory", "labs",
  "lab", "animal", "animals", "health", "healthcare", "veterinary", "vet",
  "products", "product", "ventures", "group", "holdings", "international",
  "incorporated", "corporation", "company", "limited", "inc", "llc", "ltd",
  "corp", "co", "usa", "us", "gmbh", "ag", "bv", "as", "sa", "nv", "aps",
  "eood", "ad", "spa", "srl", "kg", "plc", "division", "subsidiary"
)

#' The identifying part of a company name.
#'
#' Splits on the original word boundaries, drops the generic words, and
#' rejoins. "Phibro Animal Health Corp." becomes "phibro"; "Pharmaceutical
#' Ventures, Ltd." becomes nothing at all, which is the honest answer -- that
#' name carries no distinctive token to match on.
company_core <- function(name) {
  words <- str_split(str_to_lower(coalesce(name, "")), "[^a-z0-9]+")[[1]]
  words <- words[nzchar(words) & !words %in% GENERIC_COMPANY_WORDS]
  # Registration numbers DailyMed appends are not identity either.
  words <- words[!str_detect(words, "^[0-9]+$")]
  paste(words, collapse = "")
}

#' Do an FDA sponsor and a DailyMed labeller denote the same company?
#'
#' Compared on the identifying part of each name, because the same company is
#' written "Huvepharma EOOD" and "Huvepharma, Inc (619153559)", then falling
#' back to the alias table for renames and acquisitions.
#'
#' Takes raw names, not normalised keys, so the word boundaries needed to strip
#' generic terms still exist.
same_company <- function(sponsor, labeler) {
  s <- company_core(sponsor)
  l <- company_core(labeler)
  # No distinctive token on either side means no defensible match.
  if (nchar(s) < 4 || nchar(l) < 4) return(FALSE)

  if (str_detect(s, fixed(substr(l, 1, 7))) ||
      str_detect(l, fixed(substr(s, 1, 7)))) return(TRUE)

  any(map_lgl(COMPANY_ALIASES, function(grp) {
    any(str_detect(s, grp)) && any(str_detect(l, grp))
  }))
}

#' Is a label's dose form compatible with the product's?
#'
#' Merck files two Nuflor labels, and FDA lists five Nuflor products. A Type A
#' medicated article for swine and a concentrate solution both matched the
#' cattle injection label on name alone, which put an injectable label -- with
#' its route, its dose and its withdrawal period -- on a product mixed into
#' feed.
#'
#' Whether a product is injectable is decided from its route, not its dose
#' form. FDA's dose-form wording is inconsistent -- Excede is recorded as
#' "Sterile suspension" with no mention of injection -- so testing the form
#' text rejected correct matches. The route is unambiguous: a product given
#' intramuscularly is injectable whatever the form column says.
#'
#' The test is otherwise deliberately narrow: reject only on a clear
#' contradiction, so vocabulary this function does not recognise costs no
#' matches.
INJECTABLE_ROUTES <- paste(
  "intravenous", "intramuscular", "subcutaneous", "intraperitoneal",
  "intra-?articular", "intra-?lesional", "intracardiac", "epidural",
  "intramammary", "intrauterine", sep = "|")

compatible_form <- function(product_form, product_routes, label_form) {
  if (is.na(label_form) || !nzchar(coalesce(label_form, ""))) return(TRUE)
  lf <- str_to_lower(label_form)

  # An injectable label belongs only on an injectable product, and vice versa.
  routes <- str_to_lower(coalesce(product_routes, ""))
  if (nzchar(routes)) {
    if (str_detect(lf, "inject") != str_detect(routes, INJECTABLE_ROUTES)) {
      return(FALSE)
    }
  }

  if (is.na(product_form) || !nzchar(coalesce(product_form, ""))) return(TRUE)
  pf <- str_to_lower(product_form)

  # A feed article is not a tablet, a capsule or an ointment.
  feed <- str_detect(pf, "type a|type b|type c|medicated feed|premix")
  if (feed && str_detect(lf, "tablet|capsule|ointment|cream|suppositor")) {
    return(FALSE)
  }

  # An oral solid is not a topical preparation.
  if (str_detect(pf, "tablet|capsule|bolus") &&
      str_detect(lf, "ointment|cream|shampoo|otic|ophthalmic|topical")) {
    return(FALSE)
  }
  TRUE
}

#' Product -> DailyMed label, with the matching rule recorded.
resolve_dailymed_labels <- function(products, applications, stem_of) {
  dm <- read_dailymed_cache()
  empty <- tibble(proprietaryNameId = integer(), setid = character(),
                  splName = character(), matchBasis = character())
  if (nrow(dm) == 0) return(empty)

  # Application numbers read off each candidate label
  # (scripts/build_dailymed_appnumbers.R). This is the authoritative key: a
  # veterinary label cites the FDA application it was approved under, and that
  # is the same number the Green Book is organised by.
  appnum_file <- file.path("data", "reference", "dailymed_appnumbers.csv")
  appnums <- if (file.exists(appnum_file)) {
    readr::read_csv(appnum_file, show_col_types = FALSE)
  } else {
    tibble(setid = character(), appNumbers = character())
  }
  dm <- dm |> left_join(appnums, by = "setid")

  p <- products |>
    select(proprietaryNameId, applicationId, proprietaryName, doseFormName, routes) |>
    left_join(applications |> select(applicationId, sponsorName, applicationNumber),
              by = "applicationId") |>
    mutate(stem = stem_of(proprietaryName),
           nameKey = dm_norm(proprietaryName),
           sponsorKey = dm_norm(sponsorName),
           appPadded = sprintf("%06d", as.integer(applicationNumber)))

  #' Choose the closest label when a product matches several.
  #'
  #' One brand can carry separate labels per species: Zoetis publishes both
  #' "EXCEDE STERILE" (cattle and horses) and "EXCEDE FOR SWINE STERILE". Both
  #' share the stem and the sponsor, so both survive the earlier filters, and
  #' taking whichever came first gave the cattle and horse product the swine
  #' label -- different withdrawal times, different species entirely.
  #'
  #' Candidates are ranked by edit distance between the full trade name and
  #' the label name, so "EXCEDE" prefers "EXCEDE STERILE" over "EXCEDE FOR
  #' SWINE STERILE". A tie means the evidence does not distinguish them, and
  #' the product is left with no direct label rather than a guessed one.
  #' Rank by prefix containment, then by how much the label name adds.
  #'
  #' Edit distance is the wrong metric here: it rewards short strings, so
  #' "EXCEDE STERILE" scored better than "EXCEDE FOR SWINE STERILE" for the
  #' swine product purely by being shorter. What actually identifies a label
  #' is that the trade name is a *prefix* of the label name, DailyMed titles
  #' being the trade name followed by dose form.
  #'
  #' Rank 0 -- the trade name begins the label name ("EXCEDE FOR SWINE"
  #'           within "EXCEDE FOR SWINE STERILE"). Among these the label that
  #'           adds least wins, so plain "EXCEDE" takes "EXCEDE STERILE"
  #'           rather than the swine label.
  #' Rank 1 -- the label name begins the trade name, for names FDA records
  #'           more fully than DailyMed does.
  #' Rank 2 -- neither; the weakest evidence.
  pick <- function(df, basis) {
    if (nrow(df) == 0) return(empty)
    df |>
      mutate(
        .lab = dm_norm(splName),
        .rank = case_when(
          str_starts(.lab, fixed(nameKey))  ~ 0L,
          str_starts(nameKey, fixed(.lab))  ~ 1L,
          TRUE                              ~ 2L),
        .extra = abs(nchar(.lab) - nchar(nameKey))
      ) |>
      group_by(proprietaryNameId) |>
      # Two separate filters, deliberately. Combining them into one call
      # evaluates both minima over the unfiltered group, so a candidate that
      # wins on rank but not on length is discarded alongside one that wins on
      # length but not rank -- which dropped both Excede labels and left the
      # swine product with none. Filtering by rank first makes min(.extra)
      # recompute over the survivors.
      filter(.rank == min(.rank)) |>
      filter(.extra == min(.extra)) |>
      # Still more than one candidate: the evidence does not separate them, so
      # leave the product without a direct label rather than guess a species.
      filter(n_distinct(setid) == 1) |>
      slice(1) |>
      ungroup() |>
      transmute(proprietaryNameId, setid, splName, matchBasis = basis)
  }

  #' Every filter that must hold before a label can be considered.
  #'
  #' The application-number test is the decisive one. Where a label states
  #' which application it belongs to and that is not this product's, it is not
  #' this product's label however well the names agree -- which is what put
  #' the Nuflor cattle injection label on the swine medicated article. Labels
  #' that cite no application number (42 of 669) fall back to the name,
  #' company and dose-form evidence.
  admissible <- function(df) {
    df |>
      mutate(
        .appKnown = !is.na(appNumbers) & nzchar(coalesce(appNumbers, "")),
        .appMatch = .appKnown & map2_lgl(appNumbers, appPadded,
                                         ~ .y %in% str_split(.x, "\\|")[[1]])
      ) |>
      filter(!.appKnown | .appMatch) |>
      filter(map2_lgl(sponsorName, splLabeler, same_company),
             pmap_lgl(list(doseFormName, routes, splForm), compatible_form))
  }

  # Rule 1: the label cites this product's application number.
  #
  # Joined on the application number itself rather than on the name stem. The
  # stem is only ever a device for finding candidates, and gating on it loses
  # correct answers: Nuflor-S is filed under the stem "Nuflor" but the product
  # stems to "NuflorS", so a stem join missed its own label. The application
  # number does not care how either side spells the name.
  #
  # Where one application covers several products and several labels -- Nuflor
  # and Nuflor-S share NADA 141-063 -- pick() then separates them on the name.
  appnum_index <- dm |>
    filter(!is.na(appNumbers), nzchar(appNumbers)) |>
    distinct(setid, splName, splKey, splForm, splLabeler, appNumbers) |>
    mutate(appList = str_split(appNumbers, "\\|")) |>
    tidyr::unnest(appList) |>
    filter(nzchar(appList))

  by_appnum <- p |>
    inner_join(appnum_index, by = c("appPadded" = "appList"),
               relationship = "many-to-many") |>
    admissible() |>
    filter(.appMatch) |>
    pick("application number on the label")

  # The labeller must correspond to FDA's sponsor even on an exact name match.
  # DailyMed carries human labels too, and trade names collide across the two:
  # "Gastrografin" matched a Bracco Diagnostics human contrast agent for a
  # product FDA lists under Zoetis. Sending a vet to a human drug's label is
  # exactly the sort of confident wrongness this app must not produce.
  exact <- p |>
    anti_join(by_appnum, by = "proprietaryNameId") |>
    inner_join(dm, by = c("stem", "nameKey" = "splKey"),
               relationship = "many-to-many") |>
    admissible() |>
    pick("exact trade name")

  # FDA's sponsor and DailyMed's labeller are the same company written two
  # ways, so compare on a leading fragment rather than demanding equality:
  # "Norbrook Laboratories, Ltd." against "NORBROOK LABORATORIES LIMITED".
  by_sponsor <- p |>
    anti_join(by_appnum, by = "proprietaryNameId") |>
    anti_join(exact, by = "proprietaryNameId") |>
    inner_join(dm, by = "stem", relationship = "many-to-many") |>
    admissible() |>
    pick("sponsor matches labeller")

  # A stem with only one label behind it still has to belong to the right
  # company, for the same reason.
  sole <- p |>
    anti_join(by_appnum, by = "proprietaryNameId") |>
    anti_join(exact, by = "proprietaryNameId") |>
    anti_join(by_sponsor, by = "proprietaryNameId") |>
    inner_join(dm |> group_by(stem) |> filter(n_distinct(splKey) == 1) |> ungroup(),
               by = "stem", relationship = "many-to-many") |>
    admissible() |>
    pick("only label under this name")

  bind_rows(by_appnum, exact, by_sponsor, sole) |>
    distinct(proprietaryNameId, .keep_all = TRUE)
}
