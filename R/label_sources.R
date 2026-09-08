# ---------------------------------------------------------------------------
# label_sources.R -- resolve a product label link for every drug
#
# Search order. The actual label comes first, because that is what a
# prescriber is looking for -- a manufacturer's product page is a route to the
# label at best, and marketing copy at worst:
#
#   1. the Structured Product Label on DailyMed -- the approved label itself,
#      the package insert content
#   2. other FDA-published labelling (Blue Bird label, FDA-hosted labelling)
#   3. the manufacturer's own page for this product
#   4. the manufacturer's catalogue, when they publish no per-product page
#   5. the FDA FOI summary
#   6. a DailyMed search by trade name, when nothing above is on file
#
# Actual labelling outranks the FOI summary deliberately. An **FOI summary is
# not the product label**: it is FDA's freedom-of-information summary of the
# approval, useful for understanding the basis of approval but not for
# checking a dose or a withdrawal period. Ranking it below real labelling
# means the primary link on a drug page is the document a clinician actually
# needs, and the FOI is still offered underneath.
#
# Every link states what the document is and names its source, so no document
# is ever presented as something it is not.
# ---------------------------------------------------------------------------

library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(readr)

# -- tier 1: manufacturer websites -------------------------------------------

#' Sponsor name pattern -> manufacturer product site.
#'
#' Matched as a case-insensitive regex against FDA's sponsor name, because the
#' same company appears under several spellings. Two traps this has to survive:
#'
#'   * FDA's catalogue contains "Boehringer lngelheim" -- a lowercase L where
#'     the capital I belongs -- in two separate sponsor records. The pattern
#'     accepts either character.
#'   * Many sponsors no longer exist (Fort Dodge, Mallinckrodt Veterinary,
#'     Wyeth, Roche Vitamins). They are deliberately absent: sending a vet to
#'     a defunct company's domain, which may since have been re-registered by
#'     someone else, is worse than falling through to an FDA document.
#'
#' `link_status` records how the URL was checked. "blocked" means the site
#' refuses automated requests, which is not the same as broken.
MANUFACTURER_SITES <- tribble(
  ~pattern,                          ~manufacturer,                  ~url,                                                   ~link_status,
  "^zoetis",                         "Zoetis",                       "https://www.zoetisus.com/products",                    "verified",
  "phibro",                          "Phibro Animal Health",         "https://www.pahc.com/",                                "verified",
  "^intervet",                       "Merck Animal Health (Intervet)","https://www.merck-animal-health-usa.com/",             "verified",
  "huvepharma",                      "Huvepharma",                   "https://www.huvepharma.com/",                          "verified",
  "^elanco",                         "Elanco",                       "https://www.elanco.com/us/products",                   "verified",
  "bimeda",                          "Bimeda",                       "https://www.bimeda.com/",                              "verified",
  "cronus",                          "Cronus Pharma",                "https://cronuspharmausa.com/",                         "verified",
  "boehringer\\s+[il]ngelheim",      "Boehringer Ingelheim",         "https://www.boehringer-ingelheim.com/us/animal-health", "blocked",
  "dechra",                          "Dechra",                       "https://www.dechra-us.com/",                           "verified",
  "virbac",                          "Virbac",                       "https://us.virbac.com/",                               "verified",
  "pharmgate",                       "Pharmgate",                    "https://pharmgate.com/",                               "verified",
  "norbrook",                        "Norbrook",                     "https://www.norbrook.com/",                            "verified",
  "farnam",                          "Farnam",                       "https://www.farnam.com/",                              "verified",
  "ceva",                            "Ceva Animal Health",           "https://www.ceva.us/",                                 "verified",
  "aurora\\s+pharmaceutical",        "Aurora Pharmaceutical",        "https://www.aurorapharmaceutical.com/",                "verified",
  "med-?pharmex",                    "Med-Pharmex",                  "https://medpharmex.com/",                              "verified",
  "pegasus\\s+lab",                  "Pegasus Laboratories",         "https://www.pegasuslabs.com/",                         "verified",
  "sparhawk",                        "Sparhawk Laboratories",        "https://sparhawklabs.com/",                            "verified",
  "parnell",                         "Parnell",                      "https://parnell.com/",                                 "verified"
)

#' Attach the manufacturer site for each application, where one is known.
match_manufacturer <- function(applications) {
  sponsors <- applications |>
    distinct(applicationId, sponsorName) |>
    mutate(sponsorName = coalesce(sponsorName, ""))

  hits <- purrr::map_dfr(seq_len(nrow(MANUFACTURER_SITES)), function(i) {
    m <- MANUFACTURER_SITES[i, ]
    sponsors |>
      filter(str_detect(sponsorName, regex(m$pattern, ignore_case = TRUE))) |>
      transmute(applicationId, manufacturer = m$manufacturer,
                url = m$url, link_status = m$link_status)
  })

  # A sponsor matching two patterns would otherwise duplicate the product;
  # keep the first match in table order.
  hits |> distinct(applicationId, .keep_all = TRUE)
}

# -- assemble every label source, ranked -------------------------------------

DAILYMED_SEARCH <- "https://dailymed.nlm.nih.gov/dailymed/search.cfm?query="

#' Build one row per product per available label source.
#'
#' @return columns: proprietaryNameId, tier, sourceName, citation, whatItIs,
#'   url, link_status. Lower `tier` is preferred.
build_label_links <- function(products, applications, documents, ndc,
                              dailymed_labels = NULL) {

  prod <- products |>
    select(proprietaryNameId, applicationId, proprietaryName)

  # -- tier 1: the approved label itself ---------------------------------------
  #
  # Resolved by R/label_dailymed.R, which records how each match was made.
  # `ndc` still contributes its setids: those came from an exact-name or
  # single-label match during the NDC crawl and are equally trustworthy.
  dm_lab <- if (!is.null(dailymed_labels) && nrow(dailymed_labels) > 0) {
    dailymed_labels |> select(proprietaryNameId, setid, matchBasis)
  } else {
    tibble(proprietaryNameId = integer(), setid = character(),
           matchBasis = character())
  }

  dm_lab <- bind_rows(
    dm_lab,
    ndc |> distinct(proprietaryNameId, setid) |> filter(!is.na(setid)) |>
      mutate(matchBasis = "matched via NDC")
  ) |>
    distinct(proprietaryNameId, .keep_all = TRUE)

  # Final gate, applied to every candidate whatever produced it. The NDC table
  # is matched separately and by name, so without this a label that states it
  # belongs to a different application still reached the page: the Nuflor
  # cattle injection label (NADA 141-063) sat on the swine Type A medicated
  # article (NADA 141-264). A label that names its application is only ever
  # that application's label.
  appnum_file <- file.path("data", "reference", "dailymed_appnumbers.csv")
  if (file.exists(appnum_file)) {
    appnums <- readr::read_csv(appnum_file, show_col_types = FALSE)
    dm_lab <- dm_lab |>
      left_join(products |> select(proprietaryNameId, applicationId),
                by = "proprietaryNameId") |>
      left_join(applications |> select(applicationId, applicationNumber),
                by = "applicationId") |>
      left_join(appnums, by = "setid") |>
      mutate(
        .known = !is.na(appNumbers) & nzchar(coalesce(appNumbers, "")),
        .mine  = sprintf("%06d", as.integer(applicationNumber)),
        .match = .known & purrr::map2_lgl(appNumbers, .mine,
                                          ~ .y %in% strsplit(.x, "\\|")[[1]])
      ) |>
      filter(!.known | .match) |>
      select(proprietaryNameId, setid, matchBasis)
  }

  dm_exact <- prod |>
    inner_join(dm_lab, by = "proprietaryNameId") |>
    transmute(
      proprietaryNameId, tier = 1L,
      sourceName = "Product label (DailyMed)",
      citation   = "DailyMed, U.S. National Library of Medicine",
      whatItIs   = paste0(
        "The approved label and package insert for this product — matched by ",
        matchBasis),
      url = paste0("https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=", setid),
      link_status = "verified"
    )

  # -- tier 3: the manufacturer's own page for THIS product ---------------
  #
  # Resolved from manufacturer sitemaps and verified page by page
  # (scripts/build_manufacturer_links.R). Only some manufacturers publish
  # discoverable product pages, so this covers a minority of products; the
  # rest fall through to the catalogue below, which is honest about being a
  # starting point rather than a link to the product itself.
  pages_file <- file.path("data", "reference", "manufacturer_product_pages.csv")
  product_pages <- if (file.exists(pages_file)) {
    readr::read_csv(pages_file, show_col_types = FALSE)
  } else {
    tibble(proprietaryNameId = integer(), manufacturer = character(),
           url = character())
  }

  man_page <- prod |>
    inner_join(product_pages |> select(proprietaryNameId, manufacturer, url),
               by = "proprietaryNameId") |>
    transmute(
      proprietaryNameId, tier = 3L,
      sourceName = paste0(manufacturer, " — product page"),
      citation   = paste0(manufacturer, " (manufacturer)"),
      whatItIs   = "The manufacturer's own page for this product, with its label and prescribing information",
      url, link_status = "verified"
    )

  # -- tier 4: manufacturer catalogue, when no product page exists ---------
  man <- prod |>
    anti_join(man_page, by = "proprietaryNameId") |>
    inner_join(match_manufacturer(applications), by = "applicationId") |>
    transmute(
      proprietaryNameId, tier = 4L,
      sourceName = manufacturer,
      citation   = paste0(manufacturer, " (manufacturer)"),
      whatItIs   = "Manufacturer's product catalogue — this manufacturer does not publish a direct link to each product",
      url, link_status
    )

  # -- tier 2: other FDA-published labelling -------------------------------
  fda_lbl <- prod |>
    inner_join(documents |> filter(docType == "Product Label") |>
                 group_by(applicationId) |> slice(1) |> ungroup() |>
                 select(applicationId, url),
               by = "applicationId") |>
    transmute(
      proprietaryNameId, tier = 2L,
      sourceName = "Product label (FDA)",
      citation   = "U.S. FDA, Animal Drugs @ FDA",
      whatItIs   = "Labelling published by FDA for this application",
      url, link_status = "verified"
    )

  bbl <- prod |>
    inner_join(documents |> filter(docType == "Blue Bird Label") |>
                 group_by(applicationId) |> slice(1) |> ungroup() |>
                 select(applicationId, url),
               by = "applicationId") |>
    transmute(
      proprietaryNameId, tier = 2L,
      sourceName = "Blue Bird label",
      citation   = "U.S. FDA, Animal Drugs @ FDA",
      whatItIs   = "FDA-published label for a medicated feed",
      url, link_status = "verified"
    )

  # -- tier 5: FOI summary -------------------------------------------------
  #
  # Ranked below real labelling: this is the basis-of-approval summary, not a
  # document to check a dose against.
  foi <- prod |>
    inner_join(documents |> filter(docType == "FOI Summary") |>
                 group_by(applicationId) |> slice(1) |> ungroup() |>
                 select(applicationId, url),
               by = "applicationId") |>
    transmute(
      proprietaryNameId, tier = 5L,
      sourceName = "FDA FOI summary",
      citation   = "U.S. FDA, Animal Drugs @ FDA",
      whatItIs   = "FDA freedom-of-information approval summary — not the label itself",
      url, link_status = "verified"
    )

  # -- tier 6: fallback search ---------------------------------------------
  dm_search <- prod |>
    anti_join(dm_exact, by = "proprietaryNameId") |>
    transmute(
      proprietaryNameId, tier = 6L,
      sourceName = "Search DailyMed",
      citation   = "DailyMed, U.S. National Library of Medicine",
      whatItIs   = "Label search by trade name — no exact label match on file",
      url = paste0(DAILYMED_SEARCH,
                   vapply(str_remove_all(proprietaryName, "[®™©]") |> str_squish(),
                          utils::URLencode, character(1), reserved = TRUE)),
      link_status = "verified"
    )

  bind_rows(man_page, man, dm_exact, fda_lbl, bbl, foi, dm_search) |>
    arrange(proprietaryNameId, tier, sourceName) |>
    distinct(proprietaryNameId, url, .keep_all = TRUE)
}
