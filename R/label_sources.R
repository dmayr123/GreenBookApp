# ---------------------------------------------------------------------------
# label_sources.R -- resolve a product label link for every drug
#
# Search order, in the order a veterinarian would trust the source:
#
#   1. the manufacturer's own website
#   2. the Structured Product Label -- the labeller's full approved label
#   3. other FDA-published labelling (Blue Bird label, FDA-hosted labelling)
#   4. the FDA FOI summary
#   5. a DailyMed search by trade name, when nothing above is on file
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
build_label_links <- function(products, applications, documents, ndc) {

  prod <- products |>
    select(proprietaryNameId, applicationId, proprietaryName)

  # -- tier 1: manufacturer ---------------------------------------------------
  man <- prod |>
    inner_join(match_manufacturer(applications), by = "applicationId") |>
    transmute(
      proprietaryNameId, tier = 1L,
      sourceName = manufacturer,
      citation   = paste0(manufacturer, " (manufacturer)"),
      whatItIs   = "Manufacturer's product catalogue — open this product's page for its label",
      url, link_status
    )

  # -- tier 2: the labeller's own approved label -------------------------------
  #
  # The DailyMed page renders the Structured Product Label submitted by the
  # labeller: the complete current label, including indications, dosing and
  # withdrawal periods. It needs an exact setid, which comes from the NDC
  # match; a trade-name search is the tier-5 fallback when there is none.
  dm_exact <- prod |>
    inner_join(ndc |> distinct(proprietaryNameId, setid) |>
                 filter(!is.na(setid)) |>
                 group_by(proprietaryNameId) |> slice(1) |> ungroup(),
               by = "proprietaryNameId") |>
    transmute(
      proprietaryNameId, tier = 2L,
      sourceName = "Product label (DailyMed)",
      citation   = "DailyMed, U.S. National Library of Medicine",
      whatItIs   = "The labeller's Structured Product Label — the full approved label",
      url = paste0("https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=", setid),
      link_status = "verified"
    )

  # -- tier 3: other FDA-published labelling -----------------------------------
  fda_lbl <- prod |>
    inner_join(documents |> filter(docType == "Product Label") |>
                 group_by(applicationId) |> slice(1) |> ungroup() |>
                 select(applicationId, url),
               by = "applicationId") |>
    transmute(
      proprietaryNameId, tier = 3L,
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
      proprietaryNameId, tier = 3L,
      sourceName = "Blue Bird label",
      citation   = "U.S. FDA, Animal Drugs @ FDA",
      whatItIs   = "FDA-published label for a medicated feed",
      url, link_status = "verified"
    )

  # -- tier 4: FOI summary -----------------------------------------------------
  #
  # Ranked below real labelling: this is the basis-of-approval summary, not a
  # document to check a dose against.
  foi <- prod |>
    inner_join(documents |> filter(docType == "FOI Summary") |>
                 group_by(applicationId) |> slice(1) |> ungroup() |>
                 select(applicationId, url),
               by = "applicationId") |>
    transmute(
      proprietaryNameId, tier = 4L,
      sourceName = "FDA FOI summary",
      citation   = "U.S. FDA, Animal Drugs @ FDA",
      whatItIs   = "FDA freedom-of-information approval summary — not the label itself",
      url, link_status = "verified"
    )

  # -- tier 5: fallback search -------------------------------------------------
  dm_search <- prod |>
    anti_join(dm_exact, by = "proprietaryNameId") |>
    transmute(
      proprietaryNameId, tier = 5L,
      sourceName = "Search DailyMed",
      citation   = "DailyMed, U.S. National Library of Medicine",
      whatItIs   = "Label search by trade name — no exact label match on file",
      url = paste0(DAILYMED_SEARCH,
                   vapply(str_remove_all(proprietaryName, "[®™©]") |> str_squish(),
                          utils::URLencode, character(1), reserved = TRUE)),
      link_status = "verified"
    )

  bind_rows(man, dm_exact, fda_lbl, bbl, foi, dm_search) |>
    arrange(proprietaryNameId, tier, sourceName) |>
    distinct(proprietaryNameId, url, .keep_all = TRUE)
}
