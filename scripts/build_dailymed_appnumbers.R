# ---------------------------------------------------------------------------
# build_dailymed_appnumbers.R -- read the application number off each label
#
# Every veterinary label on DailyMed cites the FDA application it was approved
# under: "NADA 141-063", "ANADA 200-747". That number is the same key the
# Green Book is organised by, so it identifies a label exactly -- no name
# matching, no guessing about species or dose form.
#
# This exists because name matching kept producing plausible wrong answers.
# Merck files two Nuflor labels while FDA lists five Nuflor products, so a
# Type A medicated article for swine matched the cattle injection label. The
# application number settles it: that label cites 141-063 and the medicated
# article is 141-264, so it is simply not the same product.
#
# Only labels that survive the company filter are fetched -- 669 rather than
# 7,915 -- and the XML endpoint is used rather than the rendered page, a third
# of the size. Results are cached, so the monthly refresh only pays for labels
# it has not seen.
#
# Output: data/reference/dailymed_appnumbers.csv
#
# Run:  Rscript scripts/build_dailymed_appnumbers.R
# ---------------------------------------------------------------------------

library(httr2)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(readr)
library(fs)

UA <- paste0("GreenBookApp/0.1 (R ", getRversion(),
             "; veterinary drug reference; +https://github.com/dmayr123/GreenBookApp)")

OUT <- path("data", "reference", "dailymed_appnumbers.csv")
DM_XML <- "https://dailymed.nlm.nih.gov/dailymed/services/v2/spls/"

#' Application numbers cited anywhere in a label, normalised to digits.
#'
#' FDA writes them several ways -- "NADA 141-063", "NADA141063", "ANADA
#' #200-747" -- so the separators are stripped and only the six digits kept.
extract_app_numbers <- function(txt) {
  if (is.null(txt) || is.na(txt)) return(character())
  hits <- str_extract_all(txt, regex("A?NADA[ #:\\-]*[0-9]{3}[ -]?[0-9]{3}",
                                     ignore_case = TRUE))[[1]]
  if (length(hits) == 0) return(character())
  unique(str_extract(str_remove_all(hits, "[^0-9]"), "[0-9]{6}"))
}

fetch_many <- function(setids, max_active = 6) {
  reqs <- map(setids, function(s) {
    request(paste0(DM_XML, s, ".xml")) |>
      req_user_agent(UA) |> req_timeout(45) |>
      req_retry(max_tries = 2, backoff = ~ 2^.x)
  })
  resps <- req_perform_parallel(reqs, max_active = max_active, on_error = "continue")
  map_chr(resps, function(r) {
    if (inherits(r, "error") || is.null(r)) return(NA_character_)
    tryCatch(resp_body_string(r), error = function(e) NA_character_)
  })
}

build_appnumbers <- function(batch = 60) {
  source("R/label_dailymed.R", local = TRUE)
  source("R/02_tidy_greenbook.R", local = TRUE)

  products <- readRDS("data/processed/products.rds")
  apps     <- readRDS("data/processed/applications.rds")
  dm       <- read_dailymed_cache()
  if (nrow(dm) == 0) { message("No DailyMed cache; nothing to do."); return(invisible(NULL)) }

  # Only labels that could actually be chosen for some product.
  cand <- products |>
    select(proprietaryNameId, applicationId, proprietaryName) |>
    left_join(apps |> select(applicationId, sponsorName), by = "applicationId") |>
    mutate(stem = label_stem(proprietaryName)) |>
    inner_join(dm, by = "stem", relationship = "many-to-many") |>
    filter(map2_lgl(sponsorName, splLabeler, same_company)) |>
    distinct(setid)

  known <- if (file_exists(OUT)) read_csv(OUT, show_col_types = FALSE) else
    tibble(setid = character(), appNumbers = character())
  todo <- setdiff(cand$setid, known$setid)

  message(sprintf("%d candidate labels; %d already known, %d to fetch",
                  nrow(cand), nrow(known), length(todo)))
  if (length(todo) == 0) return(invisible(known))

  out <- known
  for (start in seq(1, length(todo), by = batch)) {
    chunk <- todo[start:min(start + batch - 1, length(todo))]
    bodies <- fetch_many(chunk)
    out <- bind_rows(out, tibble(
      setid = chunk,
      appNumbers = map_chr(bodies, ~ paste(extract_app_numbers(.x), collapse = "|"))
    ))
    message(sprintf("  %d / %d", min(start + batch - 1, length(todo)), length(todo)))
  }

  out <- out |> distinct(setid, .keep_all = TRUE)
  dir_create(path_dir(OUT))
  write_csv(out, OUT)

  found <- sum(nzchar(coalesce(out$appNumbers, "")))
  message(sprintf("\nWrote %d labels -> %s\n  %d cite an application number, %d do not",
                  nrow(out), OUT, found, nrow(out) - found))
  invisible(out)
}

if (sys.nframe() == 0) build_appnumbers()
