# ---------------------------------------------------------------------------
# 07_safety_alerts.R -- FDA recalls and "Dear Veterinarian" letters
#
# Two public FDA sources, matched to Green Book products:
#
#   Recalls, Market Withdrawals & Safety Alerts   FDA's press-release recall
#       list, as the JSON feed behind its own table. Filtered to Animal &
#       Veterinary; most of those are pet food, which matches nothing here.
#   Letters to Veterinary Professionals           CVM's "Dear Veterinarian"
#       letters: safety conditions, adverse event notices, product-specific
#       advice.
#
# Matching is by name, because neither source carries an application number:
#
#   A recall matches when the recalling firm agrees with the product's
#   sponsor AND the recall names the product or its active ingredient. Both,
#   because "dexmedetomidine" alone would flag every generic, and a firm alone
#   would flag its whole catalog.
#
#   A letter matches on the product's brand name (Librela, Zenrelia) and,
#   failing that, on active ingredient (fenbendazole, tylosin phosphate).
#
# Runs weekly with the shortage check. Stops rather than writing an empty
# table if either source changes shape, as 05_availability.R does.
#
# Writes data/processed/safety_alerts.{rds,parquet}
#        data/processed/safety_alerts_meta.{rds,parquet}
# ---------------------------------------------------------------------------

library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(fs)
library(arrow)
library(httr2)
library(jsonlite)
library(rvest)

source("R/search.R")         # norm_text()
source("R/drug_classes.R")   # ingredient_base()

proc_dir <- function(...) path("data", "processed", ...)

UA <- paste0(
  "Mozilla/5.0 (compatible; GreenBookApp/0.1; R ", getRversion(), "; ",
  "veterinary drug reference; +https://github.com/dmayr123/GreenBookApp)"
)

FDA <- "https://www.fda.gov"
SRC <- list(
  recalls = list(
    title = "FDA Recalls, Market Withdrawals & Safety Alerts",
    page  = paste0(FDA, "/safety/recalls-market-withdrawals-safety-alerts"),
    feed  = paste0(FDA, "/datatables-json/recalls-market-withdrawals.json")),
  letters = list(
    title = "FDA Letters to Veterinary Professionals",
    page  = paste0(FDA, "/animal-veterinary/product-safety-information/",
                   "letters-veterinary-professionals"))
)

# Recalls older than this are left out: a lot recalled four years ago is long
# gone from shelves, and an old alert at the top of a drug page trains people
# to skip the box.
RECALL_WINDOW_YEARS <- 3

# First words of trade names that are too generic to identify a product in
# free text.
STEM_STOP <- c(
  "sodium", "injection", "injectable", "solution", "sterile", "tablets",
  "tablet", "medicated", "premix", "liquid", "powder", "suspension",
  "ointment", "veterinary", "animal", "equine", "canine", "feline", "bovine",
  "horse", "cattle", "swine", "poultry", "chicken", "turkey", "super",
  "natural", "original", "advanced", "total", "ultra", "complete", "formula",
  "country", "health", "safe", "guard", "clear", "control", "drops", "paste",
  "spray", "shampoo", "water", "soluble", "oral", "topical", "type", "brand",
  "generic", "sterling", "prime", "first", "companion", "universal", "agri"
)

write_table <- function(df, stem) {
  dir_create(proc_dir())
  write_parquet(df, proc_dir(paste0(stem, ".parquet")))
  saveRDS(df, proc_dir(paste0(stem, ".rds")), compress = "gzip")
}

fetch <- function(url) {
  request(url) |> req_user_agent(UA) |> req_timeout(90) |>
    req_retry(max_tries = 4, backoff = function(i) 5 * i) |>
    req_perform() |> resp_body_string()
}

strip_html <- function(x) {
  x |> str_replace_all("<[^>]+>", " ") |> str_replace_all("&amp;", "&") |>
    str_replace_all("&#0?39;|&rsquo;|’", "'") |> str_squish()
}

#' The words a product can be recognized by in free text: its trade-name stem,
#' and the base names of its active ingredients.
product_terms <- function() {
  need <- proc_dir(c("products.rds", "applications.rds", "ingredients.rds"))
  if (!all(file_exists(need))) stop("Run R/02_tidy_greenbook.R first.")
  products     <- readRDS(need[1])
  applications <- readRDS(need[2])
  ingredients  <- readRDS(need[3])

  bases <- ingredients |>
    distinct(applicationId, activeIngredientName) |>
    mutate(base = ingredient_base(activeIngredientName)) |>
    group_by(applicationId) |>
    summarize(bases = list(unique(base)), ings = list(unique(activeIngredientName)),
              .groups = "drop")

  # A trade name that is just the ingredient ("Pimobendan Chewable Tablets",
  # "Lincomycin 20") names no brand, so it must match through the ingredient
  # rule -- otherwise a letter about Vetmedin lands on every pimobendan generic.
  ingredient_words <- unique(word(unlist(bases$bases), 1))

  words <- function(n, k) {
    w <- n |> str_remove_all("[®™©']") |> str_to_lower() |>
      str_replace_all("[^a-z0-9 ]", " ") |> str_squish() |> str_split(" ")
    map_chr(w, ~ if (length(.x) >= k) paste(.x[1:k], collapse = " ") else NA_character_)
  }

  products |>
    select(proprietaryNameId, applicationId, proprietaryName) |>
    inner_join(applications |> select(applicationId, sponsorName), by = "applicationId") |>
    left_join(bases, by = "applicationId") |>
    mutate(
      stem1 = words(proprietaryName, 1),
      stem2 = words(proprietaryName, 2),
      firm  = firm_key(sponsorName)
    ) |>
    # A first word shared by several products is a brand family -- MoorMan's,
    # Safe-Guard, Purina -- and a recall of "MoorMan's feeds" is not a recall
    # of every MoorMan's medicated block. Those need their first two words.
    add_count(stem1, name = "family") |>
    mutate(
      stem = case_when(
        is.na(stem1) | nchar(stem1) < 5 | stem1 %in% STEM_STOP |
          stem1 %in% ingredient_words              ~ NA_character_,
        family >= 3                                ~ stem2,
        TRUE                                       ~ stem1)
    ) |>
    select(-stem1, -stem2, -family)
}

#' A firm's distinctive first word: "Vetoquinol USA" and "Vetoquinol N.-A.
#' Inc." both give "vetoquinol".
firm_key <- function(x) {
  x |> str_to_lower() |> str_remove_all("[^a-z ]") |> str_squish() |>
    str_extract("^[a-z]+")
}

#' Does `text` contain `term` as a whole word (or run of words)?
has_word <- function(text, term) {
  !is.na(term) & nzchar(term) &
    str_detect(text, regex(paste0("\\b", str_escape(term), "\\b"), ignore_case = TRUE))
}

# -- recalls -----------------------------------------------------------------

read_recalls <- function() {
  raw <- fromJSON(fetch(SRC$recalls$feed))
  need <- c("path", "field_change_date_2", "field_brand_name",
            "field_product_description", "field_recall_reason",
            "field_company_name", "field_regulated_product_field")
  if (!is.data.frame(raw) || !all(need %in% names(raw)) || nrow(raw) == 0) {
    stop("FDA's recall feed no longer has the expected fields (",
         paste(need, collapse = ", "), "). Update read_recalls().")
  }

  raw |>
    as_tibble() |>
    filter(str_detect(field_regulated_product_field, "Animal")) |>
    transmute(
      date    = as.Date(field_change_date_2, format = "%m/%d/%Y"),
      brand   = strip_html(field_brand_name),
      product = strip_html(field_product_description),
      reason  = strip_html(field_recall_reason),
      firm    = strip_html(field_company_name),
      url     = paste0(FDA, str_replace_all(path, fixed("\\/"), "/"))
    ) |>
    filter(!is.na(date), date >= Sys.Date() - round(365.25 * RECALL_WINDOW_YEARS))
}

match_recalls <- function(recalls, terms) {
  map_dfr(seq_len(nrow(recalls)), function(i) {
    r <- recalls[i, ]
    text <- paste(r$brand, r$product)
    fk <- firm_key(r$firm)
    cand <- terms |> filter(firm == fk)
    if (nrow(cand) == 0) return(NULL)

    by_name <- has_word(text, cand$stem)
    by_ing  <- map_lgl(cand$bases, function(b) any(nchar(b) >= 5 & has_word(text, b)))
    hit <- cand[by_name | by_ing, ]
    if (nrow(hit) == 0) return(NULL)

    tibble(proprietaryNameId = hit$proprietaryNameId, kind = "recall",
           title = sprintf("Recall: %s", r$product),
           # The product description, not the brand field: FDA often puts the
           # firm's name there, which read "Vetoquinol USA recalled Vetoquinol".
           detail = sprintf("%s recalled %s (%s). Recalls usually cover specific lots; check the lot numbers in FDA's notice.",
                            r$firm, r$product, str_to_lower(r$reason)),
           date = r$date, url = r$url,
           matchedBy = if_else(by_name[by_name | by_ing], "firm and product name",
                               "firm and active ingredient"))
  })
}

# -- letters -----------------------------------------------------------------

read_letters <- function() {
  pg <- read_html(fetch(SRC$letters$page))
  # The letters are the page's own links into product-safety-information;
  # the side navigation's links carry a nav class and are skipped.
  a <- html_elements(pg, "a[href*='/animal-veterinary/product-safety-information/']")
  a <- a[!str_detect(coalesce(html_attr(a, "class"), ""), "nav")]
  letters <- tibble(title = str_squish(html_text2(a)),
                    url = paste0(FDA, html_attr(a, "href"))) |>
    mutate(url = str_replace(url, paste0(FDA, FDA), FDA)) |>
    filter(nchar(title) > 25) |>
    distinct(url, .keep_all = TRUE)

  if (nrow(letters) < 3) {
    stop("Found only ", nrow(letters), " letters on ", SRC$letters$page,
         ". FDA may have restructured the page; update read_letters().")
  }
  letters
}

match_letters <- function(letters, terms) {
  map_dfr(seq_len(nrow(letters)), function(i) {
    l <- letters[i, ]
    by_name <- has_word(l$title, terms$stem)

    hit <- if (any(by_name)) {
      terms[by_name, ] |> mutate(matchedBy = "product name")
    } else {
      # Every ingredient the title names must be in the product, so a letter
      # about the fenbendazole-lincomycin combination does not land on every
      # fenbendazole dewormer. A title that names the salt ("tylosin
      # phosphate", the feed form) is held to that salt, not to every tylosin.
      full <- unique(str_to_lower(unlist(terms$ings)))
      full <- full[str_detect(full, " ") & has_word(l$title, full)]
      named <- unique(unlist(terms$bases))
      named <- named[nchar(named) >= 5 & has_word(l$title, named)]
      if (length(full)) {
        terms |>
          filter(map_lgl(ings, ~ all(full %in% str_to_lower(.x)))) |>
          mutate(matchedBy = "active ingredient")
      } else if (length(named)) {
        terms |>
          filter(map_lgl(bases, ~ all(named %in% .x))) |>
          mutate(matchedBy = "active ingredient")
      } else return(NULL)
    }
    if (nrow(hit) == 0) return(NULL)

    tibble(proprietaryNameId = hit$proprietaryNameId, kind = "letter",
           title = l$title, detail = NA_character_, date = as.Date(NA),
           url = l$url, matchedBy = hit$matchedBy)
  })
}

check_safety_alerts <- function() {
  message("=== FDA recalls and veterinary letters: ", format(Sys.time()), " ===")
  terms <- product_terms()

  recalls <- read_recalls()
  letters <- read_letters()

  alerts <- bind_rows(match_recalls(recalls, terms), match_letters(letters, terms)) |>
    distinct(proprietaryNameId, kind, url, .keep_all = TRUE)
  if (nrow(alerts) == 0) {
    alerts <- tibble(proprietaryNameId = integer(), kind = character(),
                     title = character(), detail = character(), date = as.Date(character()),
                     url = character(), matchedBy = character())
  }

  checked <- Sys.Date()
  meta <- tibble(
    source = c("recalls", "letters"),
    title  = c(SRC$recalls$title, SRC$letters$title),
    url    = c(SRC$recalls$page, SRC$letters$page),
    checkedDate = checked,
    nListed  = c(nrow(recalls), nrow(letters)),
    nMatched = c(n_distinct(alerts$url[alerts$kind == "recall"]),
                 n_distinct(alerts$url[alerts$kind == "letter"])))

  write_table(alerts, "safety_alerts")
  write_table(meta, "safety_alerts_meta")

  cat("\n## FDA recalls and veterinary letters,", format(checked, "%d %B %Y"), "\n\n")
  cat(sprintf("- Animal & veterinary recalls in the last %d years: %d, matched to Green Book products: %d\n",
              RECALL_WINDOW_YEARS, meta$nListed[1], meta$nMatched[1]))
  cat(sprintf("- Letters to veterinary professionals: %d, matched: %d\n",
              meta$nListed[2], meta$nMatched[2]))
  invisible(alerts)
}

if (sys.nframe() == 0) check_safety_alerts()
