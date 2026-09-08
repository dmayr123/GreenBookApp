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

#' Product -> DailyMed label, with the matching rule recorded.
resolve_dailymed_labels <- function(products, applications, stem_of) {
  dm <- read_dailymed_cache()
  empty <- tibble(proprietaryNameId = integer(), setid = character(),
                  splName = character(), matchBasis = character())
  if (nrow(dm) == 0) return(empty)

  p <- products |>
    select(proprietaryNameId, applicationId, proprietaryName) |>
    left_join(applications |> select(applicationId, sponsorName),
              by = "applicationId") |>
    mutate(stem = stem_of(proprietaryName),
           nameKey = dm_norm(proprietaryName),
           sponsorKey = dm_norm(sponsorName))

  pick <- function(df, basis) {
    if (nrow(df) == 0) return(empty)
    df |>
      group_by(proprietaryNameId) |> slice(1) |> ungroup() |>
      transmute(proprietaryNameId, setid, splName, matchBasis = basis)
  }

  # The labeller must correspond to FDA's sponsor even on an exact name match.
  # DailyMed carries human labels too, and trade names collide across the two:
  # "Gastrografin" matched a Bracco Diagnostics human contrast agent for a
  # product FDA lists under Zoetis. Sending a vet to a human drug's label is
  # exactly the sort of confident wrongness this app must not produce.
  exact <- p |>
    inner_join(dm, by = c("stem", "nameKey" = "splKey"),
               relationship = "many-to-many") |>
    filter(map2_lgl(sponsorName, splLabeler, same_company)) |>
    pick("exact trade name")

  # FDA's sponsor and DailyMed's labeller are the same company written two
  # ways, so compare on a leading fragment rather than demanding equality:
  # "Norbrook Laboratories, Ltd." against "NORBROOK LABORATORIES LIMITED".
  by_sponsor <- p |>
    anti_join(exact, by = "proprietaryNameId") |>
    inner_join(dm, by = "stem", relationship = "many-to-many") |>
    filter(map2_lgl(sponsorName, splLabeler, same_company)) |>
    pick("sponsor matches labeller")

  # A stem with only one label behind it still has to belong to the right
  # company, for the same reason.
  sole <- p |>
    anti_join(exact, by = "proprietaryNameId") |>
    anti_join(by_sponsor, by = "proprietaryNameId") |>
    inner_join(dm |> group_by(stem) |> filter(n_distinct(splKey) == 1) |> ungroup(),
               by = "stem", relationship = "many-to-many") |>
    filter(map2_lgl(sponsorName, splLabeler, same_company)) |>
    pick("only label under this name")

  bind_rows(exact, by_sponsor, sole) |>
    distinct(proprietaryNameId, .keep_all = TRUE)
}
