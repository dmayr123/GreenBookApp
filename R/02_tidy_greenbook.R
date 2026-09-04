# ---------------------------------------------------------------------------
# 02_tidy_greenbook.R -- reshape the raw ADAFDA payloads into tidy tables
#
# The raw detail record is deeply nested and repeats itself: one application
# holds many proprietary names, each of which holds a species map, a route
# list, and a nested indication/dose structure. We flatten that into one table
# per grain so the app can filter with ordinary joins:
#
#   applications     one row per application (the NADA/ANADA/CNADA)
#   products         one row per proprietary name
#   ingredients      one row per application x active ingredient
#   product_species  one row per product x species  (carries the use class)
#   dosing           one row per product x indication  (dose + indication)
#   documents        one row per downloadable FOI / label / SPL
#   search_index     one row per product, denormalised, for the search box
#
# Free-text fields arrive as HTML fragments. We keep the HTML for display and
# derive a plain-text twin for searching, because searching raw HTML makes
# "<i>" match a query for "i".
# ---------------------------------------------------------------------------

library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)
library(fs)
library(arrow)

source("R/species_taxonomy.R")
source("R/drug_classes.R")

raw_dir  <- function(...) path("data", "raw", ...)
proc_dir <- function(...) path("data", "processed", ...)

#' Write one processed table in both formats.
#'
#' Parquet is the archival copy -- compact, typed, and readable from Python or
#' DuckDB if these tables are ever analysed outside this project.
#'
#' RDS is what the app reads. Keeping a base-R-readable copy means `global.R`
#' never has to mention `arrow`, which matters for the WebAssembly build:
#' shinylive decides what to ship by scanning source for package references,
#' so a single `requireNamespace("arrow")` in the app would add a large
#' package to every visitor's download. xz compression is chosen because the
#' cost is paid once here and saved on every page load.
write_table <- function(df, stem) {
  write_parquet(df, proc_dir(paste0(stem, ".parquet")))
  saveRDS(df, proc_dir(paste0(stem, ".rds")), compress = "xz")
}

ADAFDA_PUBLIC <- "https://animaldrugsatfda.fda.gov/adafda/app/search/public"

# -- text helpers ------------------------------------------------------------

#' Restore the comparison operators FDA stores as words.
#'
#' ADAFDA persists "≤" and "≥" as the literal strings "lessThanEqualTo" and
#' "greaterThanEqualTo", and its own front end swaps them back before display
#' (see convertSigns() in searchResultService.js). Without the same step a
#' dose reads "for dogs weighing lessThanEqualTo 140 pounds", which is both
#' wrong-looking and harder to read quickly -- and these appear in dosing
#' text, where a misread threshold matters.
decode_fda_signs <- function(x) {
  x |>
    str_replace_all("lessThanEqualTo", "≤") |>
    str_replace_all("greaterThanEqualTo", "≥")
}

#' Strip HTML tags and decode the handful of entities FDA actually emits.
strip_html <- function(x) {
  x |>
    decode_fda_signs() |>
    str_replace_all("<br\\s*/?>", " ") |>
    str_replace_all("</p>", " ") |>
    str_replace_all("<[^>]*>", "") |>
    str_replace_all("&nbsp;", " ") |>
    str_replace_all("&amp;", "&") |>
    str_replace_all("&lt;", "<") |>
    str_replace_all("&gt;", ">") |>
    str_replace_all("&quot;", '"') |>
    str_replace_all("&#39;", "'") |>
    str_squish()
}

#' Canonical form used for matching.
#'
#' This is the fix for the Green Book's worst search behaviour: on FDA's site
#' "CA-1", "-CA-1" and "CA1" are three different queries. Collapsing to
#' lowercase alphanumerics makes them one. Trademark symbols, curly quotes and
#' the trailing newlines FDA stores in its name fields all disappear here too.
norm_text <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[‘’“”]", "") |>
    str_replace_all("[^a-z0-9]+", "") |>
    coalesce("")
}

#' Trim the stray newlines and trademark glyphs FDA stores inside name fields.
clean_name <- function(x) {
  x |>
    str_replace_all("[\r\n\t]", " ") |>
    str_squish()
}

# `NULL` and empty lists are everywhere in this payload; these keep the
# extraction code readable.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
chr1 <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)[1]

# -- collapsed product category ---------------------------------------------

# A conditionally approved product is required to carry a "-CA1" suffix in its
# proprietary name. This regex is permissive about spacing, case and the
# trailing digit so "-CA1", "-ca 1" and a future "-CA2" all match.
CA_SUFFIX <- regex("-\\s*CA\\s*[0-9]+", ignore_case = TRUE)

#' Identify conditionally approved applications.
#'
#' FDA's `applicationType` field cannot be trusted for this. It reports only 7
#' of the 11 conditional approvals in the catalogue: CANALEVIA-CA1,
#' Varenzin-CA1, Credelio Quattro-CA1 and Baytril 100-CA1 are all typed "N"
#' (full NADA) despite being conditional. That was confirmed three ways --
#' the mandatory "-CA1" name suffix, FDA's own indication text ("Conditionally
#' approved for the control of nonregenerative anemia..." for Varenzin), and
#' the DailyMed label, which states "Marketing Status: Conditional New Animal
#' Drug Application" for CANALEVIA.
#'
#' Presenting a conditional approval as a full approval is a clinically
#' meaningful error -- conditional approval means effectiveness has not yet
#' been fully demonstrated -- so all three signals are unioned rather than
#' trusting the single structured field.
#'
#' Returns one row per application with the flag and its provenance.
detect_conditional <- function(applications, products, dosing) {
  by_name <- products |>
    filter(str_detect(coalesce(proprietaryName, ""), CA_SUFFIX)) |>
    distinct(applicationId) |>
    mutate(condByName = TRUE)

  by_text <- dosing |>
    filter(str_detect(
      coalesce(paste(limitationHtml, indicationHtml, dosageHtml), ""),
      regex("conditionally approved", ignore_case = TRUE))) |>
    distinct(applicationId) |>
    mutate(condByLabel = TRUE)

  applications |>
    select(applicationId, applicationType) |>
    left_join(by_name, by = "applicationId") |>
    left_join(by_text, by = "applicationId") |>
    mutate(
      condByName  = coalesce(condByName, FALSE),
      condByLabel = coalesce(condByLabel, FALSE),
      condByType  = applicationType == "C",
      isConditional = condByName | condByLabel | condByType,
      # True when FDA's own type field contradicts the other evidence. The app
      # surfaces this so a vet can see the discrepancy rather than silently
      # trusting either side.
      fdaTypeDisagrees = isConditional & !coalesce(condByType, FALSE)
    ) |>
    select(applicationId, isConditional, fdaTypeDisagrees,
           condByName, condByLabel, condByType)
}

#' Collapse FDA's application type + status into the four categories a
#' clinician actually distinguishes between.
#'
#' FDA exposes type (NADA/ANADA/CNADA/EUA) and status (Approved / Granted /
#' Revoked / Voluntarily Withdrawn) as separate codes, which means the site
#' shows combinations that mean the same thing clinically. Withdrawal is
#' reported separately from the category because a withdrawn generic is still
#' a generic -- the vet needs to know both facts, not one merged one.
#'
#' `is_conditional` comes from detect_conditional() and overrides the type
#' field, which is unreliable for exactly this distinction.
collapse_category <- function(application_type, status_code,
                              is_conditional = FALSE) {
  case_when(
    is_conditional          ~ "Conditional Approval",
    application_type == "C" ~ "Conditional Approval",
    application_type == "A" ~ "ANADA / Generic",
    application_type == "N" ~ "NADA / Approved",
    application_type == "E" ~ "Emergency Use Authorization",
    TRUE                    ~ "Other / Unclassified"
  )
}

marketing_status <- function(status_code, withdrawal_date) {
  case_when(
    status_code == "W" | !is.na(withdrawal_date) ~ "Voluntarily withdrawn",
    status_code == "R"                           ~ "Revoked",
    status_code %in% c("A", "G")                 ~ "Currently marketed",
    TRUE                                         ~ "Unknown"
  )
}

# -- readers -----------------------------------------------------------------

read_catalogue <- function() {
  fromJSON(raw_dir("catalogue.json"), simplifyDataFrame = TRUE) |>
    as_tibble() |>
    mutate(
      proprietaryName      = clean_name(proprietaryName),
      activeIngredientName = clean_name(activeIngredientName),
      sponsorName          = clean_name(sponsorName)
    )
}

read_beans <- function() {
  files <- dir_ls(raw_dir("beans"), glob = "*.json")
  message(sprintf("Parsing %d detail records ...", length(files)))
  map(files, ~ fromJSON(.x, simplifyVector = FALSE))
}

# -- extractors --------------------------------------------------------------

#' Sponsor (labeler / manufacturer) for one application.
extract_sponsor <- function(bean, app_id) {
  s <- bean$sponsorPreviewBean
  if (is.null(s)) return(NULL)
  tibble(
    applicationId = app_id,
    sponsorName   = clean_name(chr1(s$sponsorName)),
    sponsorCity   = chr1(s$city),
    sponsorState  = chr1(s$stateCode),
    sponsorCountry= chr1(s$countryName)
  )
}

extract_ingredients <- function(bean, app_id) {
  ing <- bean$ingredientsPreviewBean %||% list()
  if (length(ing) == 0) return(NULL)
  map_dfr(ing, function(i) tibble(
    applicationId        = app_id,
    activeIngredientName = clean_name(chr1(i$activeIngredientName)),
    toleranceHtml        = chr1(i$activeIngredientTolerance)
  ))
}

#' One row per proprietary name, with routes collapsed to a single string.
extract_products <- function(bean, app_id) {
  props <- bean$proprietaryPreviewBean %||% list()
  if (length(props) == 0) return(NULL)
  map_dfr(props, function(p) tibble(
    applicationId     = app_id,
    proprietaryNameId = as.integer(p$proprietaryNameId %||% NA),
    proprietaryName   = clean_name(chr1(p$proprietaryName)),
    doseFormName      = chr1(p$doseFormName),
    # "RX", "OTC" or "VFD" -- the dispensing class, not the approval status.
    dispensingStatus  = chr1(p$statusDescription),
    specifications    = chr1(p$specifications),
    routes            = paste(unlist(p$routes %||% list()), collapse = ", "),
    withdrawalHtml    = paste(unlist(p$withdrawals %||% list()), collapse = " ")
  ))
}

#' Species map -> tidy rows.
#'
#' FDA keys this map as "Cattle:920", where the value is the labelled use
#' class for that species. We split the key and keep both halves: the name is
#' what the user picks on the home page, the class is what qualifies it
#' ("Beef calves 2 months of age and older").
extract_species <- function(bean, app_id) {
  props <- bean$proprietaryPreviewBean %||% list()
  map_dfr(props, function(p) {
    sp <- p$species %||% list()
    if (length(sp) == 0) return(NULL)
    keys <- names(sp)
    tibble(
      applicationId     = app_id,
      proprietaryNameId = as.integer(p$proprietaryNameId %||% NA),
      speciesName       = str_trim(str_remove(keys, ":[0-9]+$")),
      speciesCode       = str_extract(keys, "(?<=:)[0-9]+$"),
      useClass          = map_chr(sp, ~ chr1(.x))
    )
  })
}

#' Indication / dose rows.
#'
#' `ailHeader` groups doses by the population they apply to ("Beef cattle 2
#' months of age and older"); each group holds one or more indication+dose
#' pairs. That header is the closest thing FDA gives us to a species-specific
#' dose statement, so it is preserved rather than flattened away.
extract_dosing <- function(bean, app_id) {
  props <- bean$proprietaryPreviewBean %||% list()
  map_dfr(props, function(p) {
    headers <- p$ailHeader %||% list()
    if (length(headers) == 0) return(NULL)
    map_dfr(headers, function(h) {
      ails <- h$ails %||% list()
      if (length(ails) == 0) return(NULL)
      map_dfr(ails, function(a) tibble(
        applicationId     = app_id,
        proprietaryNameId = as.integer(p$proprietaryNameId %||% NA),
        populationHeader  = chr1(h$ailHeader),
        dosageHtml        = chr1(a$dosageAmount),
        indicationHtml    = chr1(a$indication),
        limitationHtml    = chr1(a$limitation)
      ))
    })
  })
}

#' Downloadable documents, with the URL the FDA app itself uses.
extract_documents <- function(bean, app_id) {
  d <- bean$documents %||% list()

  foi <- map_dfr(d$foi %||% list(), function(f) tibble(
    applicationId = app_id,
    docType  = "FOI Summary",
    docId    = as.integer(f$foiId %||% NA),
    title    = paste0("FOI Summary (", chr1(f$approvalType) %||% "", ")"),
    docDate  = chr1(f$approvalDate),
    summaryHtml = chr1(f$summary),
    url      = paste0(ADAFDA_PUBLIC, "/document/downloadFoi/", chr1(f$foiId))
  ))

  labeling <- map_dfr(d$labeling %||% list(), function(f) tibble(
    applicationId = app_id,
    docType = "Product Label",
    docId   = as.integer(f$labelingId %||% NA),
    title   = chr1(f$fileName) %||% "Product label",
    docDate = NA_character_, summaryHtml = NA_character_,
    url     = paste0(ADAFDA_PUBLIC, "/document/downloadLabeling/", chr1(f$labelingId))
  ))

  bbl <- map_dfr(d$blueBirdLabel %||% list(), function(f) tibble(
    applicationId = app_id,
    docType = "Blue Bird Label",
    docId   = as.integer(f$blueBirdLabelId %||% NA),
    title   = chr1(f$fileName) %||% "Blue Bird label",
    docDate = NA_character_, summaryHtml = NA_character_,
    url     = paste0(ADAFDA_PUBLIC, "/document/downloadBBL/", chr1(f$blueBirdLabelId))
  ))

  ea <- map_dfr(d$eaFonsi %||% list(), function(f) tibble(
    applicationId = app_id,
    docType = "Environmental Assessment",
    docId   = as.integer(f$eaFonsiId %||% NA),
    title   = chr1(f$fileName) %||% "EA / FONSI",
    docDate = NA_character_, summaryHtml = NA_character_,
    url     = paste0(ADAFDA_PUBLIC, "/document/downloadFonsi/", chr1(f$eaFonsiId))
  ))

  bind_rows(foi, labeling, bbl, ea)
}

extract_pioneer <- function(bean, app_id) {
  pn <- bean$application$pioneerApplicationNumber %||% 0
  tibble(applicationId = app_id, pioneerApplicationNumber = as.integer(pn))
}

# -- SPL links ---------------------------------------------------------------

read_spl <- function() {
  f <- raw_dir("spl_links.json")
  if (!file_exists(f)) return(tibble())
  raw <- fromJSON(f, simplifyVector = FALSE)
  map_dfr(raw, function(r) {
    spl <- r$spl %||% list()
    if (length(spl) == 0) return(NULL)
    map_dfr(spl, function(s) tibble(
      applicationId = as.integer(r$applicationId),
      docType = "SPL Label (DailyMed)",
      docId   = as.integer(s$splXmlId %||% NA),
      title   = clean_name(chr1(s$linkName)) %||% "Structured Product Label",
      docDate = NA_character_, summaryHtml = NA_character_,
      url     = paste0(ADAFDA_PUBLIC, "/spl/file/", chr1(s$splXmlId), "/",
                       utils::URLencode(clean_name(chr1(s$linkName)), reserved = TRUE))
    ))
  })
}

# -- build -------------------------------------------------------------------

build_all <- function() {
  catalogue <- read_catalogue()
  beans     <- read_beans()

  ids <- as.integer(str_remove(path_file(names(beans)), "\\.json$"))

  message("Flattening ...")
  products   <- map2_dfr(beans, ids, extract_products)
  ingredients<- map2_dfr(beans, ids, extract_ingredients)
  species    <- map2_dfr(beans, ids, extract_species)
  dosing     <- map2_dfr(beans, ids, extract_dosing)
  documents  <- map2_dfr(beans, ids, extract_documents)
  sponsors   <- map2_dfr(beans, ids, extract_sponsor)
  pioneers   <- map2_dfr(beans, ids, extract_pioneer)
  documents  <- bind_rows(documents, read_spl())

  conditional <- detect_conditional(catalogue, products, dosing)

  applications <- catalogue |>
    select(applicationId, applicationNumber, applicationType,
           applicationStatusCode, publishDate, voluntaryWithdrawalDate) |>
    left_join(sponsors, by = "applicationId") |>
    left_join(pioneers, by = "applicationId") |>
    left_join(conditional, by = "applicationId") |>
    mutate(
      isConditional    = coalesce(isConditional, FALSE),
      fdaTypeDisagrees = coalesce(fdaTypeDisagrees, FALSE),
      category      = collapse_category(applicationType, applicationStatusCode,
                                        isConditional),
      marketStatus  = marketing_status(applicationStatusCode, voluntaryWithdrawalDate),
      # An application whose pioneer number points at itself (or at 0) is the
      # pioneer; only generics carry a meaningful pointer.
      pioneerApplicationNumber = if_else(
        pioneerApplicationNumber %in% c(0L, NA_integer_) |
          pioneerApplicationNumber == applicationNumber,
        NA_integer_, pioneerApplicationNumber
      )
    )

  # The app renders the *Html columns directly, so the sign decoding has to be
  # applied to them too -- not only to the plain-text twins derived below.
  dosing <- dosing |>
    mutate(across(c(dosageHtml, indicationHtml, limitationHtml),
                  decode_fda_signs))
  products <- products |>
    mutate(across(c(withdrawalHtml, specifications), decode_fda_signs))
  ingredients <- ingredients |>
    mutate(toleranceHtml = decode_fda_signs(toleranceHtml))
  documents <- documents |>
    mutate(summaryHtml = decode_fda_signs(summaryHtml))

  # Plain-text twins for search / display.
  dosing <- dosing |>
    mutate(dosage     = strip_html(dosageHtml),
           indication = strip_html(indicationHtml),
           limitation = strip_html(limitationHtml))

  products <- products |>
    mutate(withdrawalPeriod = strip_html(withdrawalHtml),
           specifications   = strip_html(specifications))

  ingredients <- ingredients |>
    mutate(tolerance = strip_html(toleranceHtml))

  documents <- documents |>
    mutate(summaryText = strip_html(summaryHtml)) |>
    filter(!is.na(docId))

  # One denormalised row per product drives the search box. Building it once
  # here keeps the app's reactive path to a single filter over a flat table.
  # Species groups collapse FDA's inconsistent labels (Equids vs Horses) onto
  # the tiles shown on the landing page.
  species <- assign_species_group(species)

  # Pharmacologic class drives which professional guidelines are offered.
  ingredient_classes <- classify_ingredients(ingredients$activeIngredientName)
  class_by_app <- ingredients |>
    left_join(ingredient_classes, by = "activeIngredientName",
              relationship = "many-to-many") |>
    group_by(applicationId) |>
    summarise(drugClasses = paste(sort(unique(drugClass)), collapse = "; "),
              .groups = "drop")

  ing_by_app <- ingredients |>
    group_by(applicationId) |>
    summarise(ingredients = paste(unique(activeIngredientName), collapse = "; "),
              .groups = "drop")

  sp_by_prod <- species |>
    group_by(proprietaryNameId) |>
    summarise(speciesList   = paste(sort(unique(speciesName)),  collapse = "; "),
              speciesGroups = paste(sort(unique(speciesGroup)), collapse = "; "),
              .groups = "drop")

  ind_by_prod <- dosing |>
    group_by(proprietaryNameId) |>
    summarise(indications = paste(unique(indication), collapse = " | "),
              .groups = "drop")

  search_index <- products |>
    left_join(applications, by = "applicationId") |>
    left_join(ing_by_app,   by = "applicationId") |>
    left_join(class_by_app, by = "applicationId") |>
    left_join(sp_by_prod,   by = "proprietaryNameId") |>
    left_join(ind_by_prod,  by = "proprietaryNameId") |>
    mutate(
      # Every field a user might type into one canonical haystack. Because
      # both the haystack and the query go through norm_text(), punctuation
      # differences such as "CA-1" vs "-CA-1" can no longer split a match.
      searchKey = norm_text(paste(
        proprietaryName, ingredients, sponsorName, applicationNumber,
        doseFormName, speciesList, routes
      )),
      searchKeyWide = norm_text(paste(
        proprietaryName, ingredients, sponsorName, applicationNumber,
        doseFormName, speciesList, routes, indications, specifications
      ))
    )

  dir_create(proc_dir())
  write_table(applications,  "applications")
  write_table(products,      "products")
  write_table(ingredients,   "ingredients")
  write_table(species,       "product_species")
  write_table(dosing,        "dosing")
  write_table(documents,     "documents")
  write_table(search_index,  "search_index")

  message(sprintf(
    paste0("Wrote:\n  applications %d\n  products %d\n  ingredients %d\n",
           "  species rows %d\n  dosing rows %d\n  documents %d"),
    nrow(applications), nrow(products), nrow(ingredients),
    nrow(species), nrow(dosing), nrow(documents)
  ))

  invisible(list(applications = applications, products = products,
                 species = species, dosing = dosing, documents = documents,
                 search_index = search_index))
}

if (sys.nframe() == 0) build_all()
