# ---------------------------------------------------------------------------
# 06_drug_flags.R -- regulatory and safety flags from curated reference lists
#
# Matches each product's active ingredients against five small lists kept in
# data/reference/, each transcribed from its primary source:
#
#   prohibited_extralabel.csv   21 CFR 530.41, drugs prohibited from
#                               extra-label use in food-producing animals
#   controlled_substances.csv   DEA schedules (21 CFR 1308; DEA's
#                               alphabetical controlled substances list)
#   mdr1_drugs.csv              WSU's MDR1 (ABCB1) problem-drug lists for
#                               dogs and cats
#   antimicrobial_classes.csv   FDA's medically important (GFI #152
#                               Appendix A) and not-medically-important classes
#   gfi263_applications.csv     applications FDA moved from OTC to Rx under
#                               GFI #263 (June 2023)
#
# No network: these lists change rarely, and when they do the change is a
# reviewed edit to a CSV, not something a scraper should apply unseen. Runs on
# every build, full or weekly, because it takes a second.
#
# Writes data/processed/drug_flags.{rds,parquet}, one row per product x flag.
# ---------------------------------------------------------------------------

library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)
library(fs)
library(arrow)

source("R/drug_classes.R")   # ingredient_base()

proc_dir <- function(...) path("data", "processed", ...)
ref_dir  <- function(...) path("data", "reference", ...)

URLS <- c(
  cfr530   = "https://www.ecfr.gov/current/title-21/section-530.41",
  dea      = "https://www.deadiversion.usdoj.gov/schedules/orangebook/c_cs_alpha.pdf",
  implant  = "https://www.ecfr.gov/current/title-21/section-1308.26",
  gfi152   = "https://www.fda.gov/regulatory-information/search-fda-guidance-documents/cvm-gfi-152-evaluating-safety-antimicrobial-new-animal-drugs-regard-their-microbiological-effects",
  gfi263   = "https://www.fda.gov/animal-veterinary/antimicrobial-resistance/list-approved-new-animal-drug-applications-affected-gfi-263"
)

write_table <- function(df, stem) {
  dir_create(proc_dir())
  write_parquet(df, proc_dir(paste0(stem, ".parquet")))
  saveRDS(df, proc_dir(paste0(stem, ".rds")), compress = "gzip")
}

read_ref <- function(f) {
  read.csv(ref_dir(f), stringsAsFactors = FALSE, na.strings = "") |> as_tibble()
}

#' Every (ingredient, reference row) pair whose pattern matches, on the
#' lower-cased ingredient name.
match_ref <- function(ings, ref) {
  map_dfr(seq_len(nrow(ref)), function(i) {
    hit <- str_detect(ings$ing, regex(ref$pattern[i], ignore_case = TRUE))
    if ("exclude_pattern" %in% names(ref) && !is.na(ref$exclude_pattern[i])) {
      hit <- hit & !str_detect(ings$ing, regex(ref$exclude_pattern[i], ignore_case = TRUE))
    }
    if (!any(hit)) return(NULL)
    bind_cols(ings[hit, ], ref[rep(i, sum(hit)), ])
  })
}

#' FDA records ingredients per application, and one application can carry
#' products with different ingredients: M99 Etorphine and M50-50 Diprenorphine
#' share one, so without this the reversal agent inherited etorphine's
#' Schedule II. Where a product's own name names some of its application's
#' ingredients, keep only those. Names that mention none keep them all.
narrow_to_named <- function(ings) {
  ings |>
    group_by(proprietaryNameId) |>
    mutate(named = str_detect(str_to_lower(proprietaryName),
                              fixed(word(base, 1))) & nchar(word(base, 1)) >= 5,
           keep = if (any(named) && !all(named)) named else TRUE) |>
    ungroup() |>
    filter(keep) |>
    select(-named, -keep)
}

SCHEDULE_ORDER <- c("II", "III", "IV", "V")
# Excluded as implants, and downgraded in mixtures; see controlled_flags().
ANABOLIC <- "^(boldenone|mibolerone|stanozolol|testosterone|trenbolone)"
BARB_II  <- "^(pentobarbital|secobarbital)"

build_flags <- function() {
  need <- proc_dir(c("products.rds", "applications.rds", "ingredients.rds",
                     "product_species.rds"))
  if (!all(file_exists(need))) stop("Run R/02_tidy_greenbook.R first.")
  products     <- readRDS(need[1])
  applications <- readRDS(need[2])
  ingredients  <- readRDS(need[3])
  species      <- readRDS(need[4])

  # One row per product x ingredient: ingredients are recorded per
  # application, and every product on the application carries them.
  ings <- products |>
    select(proprietaryNameId, applicationId, doseFormName, dispensingStatus) |>
    inner_join(ingredients |> distinct(applicationId, activeIngredientName),
               by = "applicationId", relationship = "many-to-many") |>
    mutate(ing = str_to_lower(activeIngredientName),
           base = ingredient_base(activeIngredientName)) |>
    left_join(products |> select(proprietaryNameId, proprietaryName), by = "proprietaryNameId") |>
    narrow_to_named()

  sp_by_prod <- species |> distinct(proprietaryNameId, speciesGroup)
  companion <- function(pid, sp) {
    pid %in% sp_by_prod$proprietaryNameId[sp_by_prod$speciesGroup == sp]
  }

  # -- food-animal prohibitions ---------------------------------------------
  prohibited <- match_ref(ings, read_ref("prohibited_extralabel.csv")) |>
    group_by(proprietaryNameId, rule) |>
    summarize(ingredient = paste(unique(activeIngredientName), collapse = ", "),
              restriction = first(restriction), cfr = first(cfr_paragraph),
              .groups = "drop") |>
    transmute(proprietaryNameId, flag = "prohibited",
              badge = "Food-animal ELU restricted",
              title = sprintf("Extra-label use restricted in food animals (21 CFR %s)", cfr),
              detail = sprintf("%s: %s", ingredient, restriction),
              severity = "high", species = NA_character_, url = URLS[["cfr530"]])

  # -- DEA schedule ------------------------------------------------------------
  cs <- match_ref(ings, read_ref("controlled_substances.csv")) |>
    mutate(implant = str_detect(coalesce(doseFormName, ""), regex("implant", ignore_case = TRUE)),
           excluded = str_detect(ing, ANABOLIC) & implant)

  n_active <- ings |> count(proprietaryNameId, name = "nActive")
  n_ctrl   <- cs |> filter(!excluded) |> distinct(proprietaryNameId, ing) |>
    count(proprietaryNameId, name = "nControlled")

  controlled <- cs |>
    filter(!excluded) |>
    left_join(n_active, by = "proprietaryNameId") |>
    left_join(n_ctrl,   by = "proprietaryNameId") |>
    # Pentobarbital or secobarbital with any non-controlled active ingredient
    # is Schedule III (DEA codes 2271, 2316) -- the euthanasia solutions.
    mutate(schedule = if_else(str_detect(ing, BARB_II) & nActive > nControlled,
                              "III", schedule)) |>
    group_by(proprietaryNameId) |>
    summarize(schedule = SCHEDULE_ORDER[min(match(schedule, SCHEDULE_ORDER))],
              names = paste(unique(dea_name), collapse = "; "),
              note = paste(unique(na.omit(note)), collapse = " "),
              .groups = "drop") |>
    transmute(proprietaryNameId, flag = "controlled",
              badge = paste0("C-", schedule),
              title = sprintf("DEA Schedule %s controlled substance", schedule),
              detail = paste0(names, if_else(nzchar(note), paste0(". ", note), "")),
              severity = "info", species = NA_character_, url = URLS[["dea"]])

  # Implants that would otherwise be scheduled get said out loud, because a
  # vet who knows trenbolone is C-III will wonder why the badge is missing.
  implant_note <- cs |>
    filter(excluded) |>
    distinct(proprietaryNameId, activeIngredientName) |>
    group_by(proprietaryNameId) |>
    summarize(ingredient = paste(activeIngredientName, collapse = ", "), .groups = "drop") |>
    filter(!proprietaryNameId %in% controlled$proprietaryNameId) |>
    transmute(proprietaryNameId, flag = "controlled_excluded", badge = NA_character_,
              title = "Not a controlled substance in this form",
              detail = sprintf(paste0("%s is a Schedule III anabolic steroid, but ",
                                      "FDA-approved implants for cattle and other nonhuman ",
                                      "species are excluded from the schedules (21 CFR 1308.26)."),
                               ingredient),
              severity = "info", species = NA_character_, url = URLS[["implant"]])

  # -- MDR1 ----------------------------------------------------------------------
  # Shown on products labeled for the species, and -- for dogs only -- on
  # macrocyclic lactones whatever their label: a cattle ivermectin used in a
  # collie is the classic MDR1 poisoning, so the livestock product is where
  # that warning matters most. The cat notes stay on cat products.
  ML <- "^(ivermectin|milbemycin|moxidectin|selamectin|eprinomectin)"
  # FDA's species list for Ivomec Injection for Cattle includes dogs, so
  # "labeled for dogs" alone would call a 1% cattle injectable "safe at label
  # doses". WSU's safety finding is for the companion-animal formulations, so
  # a product also labeled for livestock or horses counts as a livestock
  # formulation whatever else its species list says.
  livestock <- sp_by_prod |>
    filter(!speciesGroup %in% c("dogs", "cats", "other_minor", "rabbits")) |>
    pull(proprietaryNameId) |> unique()

  mdr1 <- match_ref(ings, read_ref("mdr1_drugs.csv")) |>
    mutate(sp_group = if_else(species == "dog", "dogs", "cats"),
           labeled = map2_lgl(proprietaryNameId, sp_group, companion) &
                     !(str_detect(ing, ML) & proprietaryNameId %in% livestock)) |>
    filter(labeled | (species == "dog" & str_detect(ing, ML))) |>
    # A label-safe drug in a product not labeled for the species is a caution:
    # "safe at label doses" does not cover a livestock formulation.
    mutate(severity = case_when(
             category == "avoid"                ~ "high",
             category == "label_safe" & labeled ~ "info",
             TRUE                               ~ "caution"),
           status = case_when(
             category == "avoid"                ~ "Avoid",
             category == "consult"              ~ "Consult for dosing",
             category == "label_safe" & labeled ~ "Safe at label doses",
             category == "label_safe"           ~ paste0("Not a ", str_remove(sp_group, "s$"),
                                                         " formulation"),
             TRUE                               ~ "Risk if ingested"),
           note = if_else(category == "label_safe" & !labeled,
                          paste0(note, " This is not a ", str_remove(sp_group, "s$"),
                                 " heartworm or parasite-control formulation."), note)) |>
    distinct(proprietaryNameId, species, activeIngredientName, .keep_all = TRUE) |>
    transmute(proprietaryNameId, flag = "mdr1", badge = "MDR1",
              title = sprintf("MDR1 (ABCB1) %s: %s", if_else(species == "dog", "dogs", "cats"),
                              status),
              detail = sprintf("%s. %s", activeIngredientName, note),
              severity, species, url)

  # -- antimicrobials ------------------------------------------------------------
  classes <- match_ref(ings, read_ref("antimicrobial_classes.csv"))
  mi <- classes |>
    group_by(proprietaryNameId) |>
    summarize(mi = any(medically_important),
              mi_classes  = paste(unique(class[medically_important]), collapse = ", "),
              nmi_classes = paste(unique(class[!medically_important]), collapse = ", "),
              .groups = "drop")

  antimicrobial <- bind_rows(
    mi |> filter(mi) |>
      transmute(proprietaryNameId, flag = "antimicrobial",
                badge = "Medically important antimicrobial",
                title = "Medically important antimicrobial",
                detail = sprintf(paste0(
                  "%s: listed by FDA as important to human medicine (GFI #152 ",
                  "Appendix A). In food animals these require veterinary oversight: ",
                  "a VFD for use in feed, a prescription for use in water or in any ",
                  "other dosage form."), mi_classes),
                severity = "info", species = NA_character_, url = URLS[["gfi152"]]),
    mi |> filter(!mi) |>
      transmute(proprietaryNameId, flag = "antimicrobial_nmi", badge = NA_character_,
                title = "Not a medically important antimicrobial",
                detail = sprintf(paste0(
                  "%s: not listed in FDA's GFI #152 Appendix A, so not subject to the ",
                  "VFD and prescription requirements for medically important ",
                  "antimicrobials."), nmi_classes),
                severity = "info", species = NA_character_, url = URLS[["gfi152"]])
  )

  gfi <- read_ref("gfi263_applications.csv") |>
    filter(action == "Supplement Approved")
  gfi263 <- products |>
    inner_join(applications |> select(applicationId, applicationNumber), by = "applicationId") |>
    filter(applicationNumber %in% gfi$applicationNumber) |>
    transmute(proprietaryNameId, flag = "gfi263", badge = NA_character_,
              title = "Moved from OTC to prescription (GFI #263)",
              detail = paste0(
                "FDA brought this product under veterinary oversight: from June 11, ",
                "2023 it is labeled for prescription use only.",
                if_else(coalesce(dispensingStatus, "") == "OTC",
                        " FDA's Green Book record for it still reads OTC.", "")),
              severity = "info", species = NA_character_, url = URLS[["gfi263"]])

  flags <- bind_rows(prohibited, controlled, implant_note, mdr1, antimicrobial, gfi263) |>
    arrange(proprietaryNameId, flag)

  write_table(flags, "drug_flags")
  message(paste(capture.output(print(count(flags, flag))), collapse = "\n"))
  invisible(flags)
}

if (sys.nframe() == 0) build_flags()
