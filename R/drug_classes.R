# ---------------------------------------------------------------------------
# drug_classes.R -- assign a pharmacologic class to each active ingredient
#
# FDA does not publish a drug class alongside the Green Book listing, but the
# class is what determines which professional guideline applies: a vet looking
# at phenobarbital wants the epilepsy consensus statement, and one looking at
# ceftiofur wants the antimicrobial stewardship policy.
#
# Classification here is stem-based. International non-proprietary names are
# built from agreed stems (-cillin, -floxacin, -coxib, -barbital), so matching
# the stem is both accurate and self-maintaining: a newly approved ingredient
# usually classifies correctly without anyone editing this file. Ingredients
# whose stems are not diagnostic are listed explicitly.
#
# An ingredient may hold more than one class (ketamine is anaesthetic and
# analgesic), so the result is long, not wide.
# ---------------------------------------------------------------------------

library(dplyr)
library(tibble)
library(stringr)
library(purrr)
library(tidyr)

#' Regex -> class. Evaluated case-insensitively against the ingredient name.
#'
#' Order does not matter; every rule that matches contributes a class.
DRUG_CLASS_RULES <- tribble(
  ~pattern, ~class,

  # -- anti-infectives ------------------------------------------------------
  "cillin|cef[a-z]*|ceftiofur|penem|bactam",                 "antimicrobial",
  "mycin|micin|floxacin|cycline|sulfa|sulfadi|sulfame",       "antimicrobial",
  "tylosin|tilmicosin|tulathromycin|gamithromycin|tildipirosin", "antimicrobial",
  "florfenicol|chloramphenicol|novobiocin|bacitracin",        "antimicrobial",
  "monensin|lasalocid|salinomycin|narasin|virginiamycin",     "antimicrobial",
  "trimethoprim|ormetoprim|nitrofurantoin|metronidazole",     "antimicrobial",
  "conazole|amphotericin|griseofulvin|terbinafine|nystatin",  "antifungal",

  # -- antiparasitics -------------------------------------------------------
  "ivermectin|moxidectin|doramectin|eprinomectin|selamectin", "antiparasitic",
  "milbemycin|abamectin|emamectin",                           "antiparasitic",
  "fenbendazole|albendazole|oxfendazole|febantel|levamisole", "antiparasitic",
  "praziquantel|pyrantel|morantel|epsiprantel|clorsulon",     "antiparasitic",
  "afoxolaner|fluralaner|sarolaner|lotilaner|esafoxolaner",   "antiparasitic",
  "imidacloprid|fipronil|permethrin|dinotefuran|pyriproxyfen","antiparasitic",
  "amitraz|coumaphos|diazinon|phosmet|spinosad|nitenpyram",   "antiparasitic",
  "amprolium|ponazuril|toltrazuril|decoquinate|robenidine",   "antiparasitic",

  # -- anti-inflammatory / analgesia ---------------------------------------
  "coxib|profen|flunixin|meloxicam|phenylbutazone|aspirin",   "nsaid",
  "ketoprofen|tolfenamic|dipyrone|metamizole",                "nsaid",
  "buprenorphine|butorphanol|morphine|fentanyl|methadone",    "analgesic",
  "hydromorphone|oxymorphone|tramadol|grapiprant|codeine",    "analgesic",
  "prednis|dexamethasone|triamcinolone|methylprednisolone",   "corticosteroid",
  "betamethasone|hydrocortisone|isoflupredone|flumethasone",  "corticosteroid",

  # -- neurology / anaesthesia ---------------------------------------------
  "barbital|phenytoin|levetiracetam|zonisamide|imepitoin",    "anticonvulsant",
  "potassium bromide|gabapentin|pregabalin",                  "anticonvulsant",
  "isoflurane|sevoflurane|halothane|propofol|alfaxalone",     "anesthetic",
  "ketamine|tiletamine|etomidate|thiopental",                 "anesthetic",
  "caine$|lidocaine|bupivacaine|mepivacaine|procaine",        "anesthetic",
  "ketamine",                                                 "analgesic",
  "xylazine|detomidine|medetomidine|dexmedetomidine|romifidine","sedative",
  "acepromazine|diazepam|midazolam|zolazepam|trazodone",      "sedative",
  "atipamezole|yohimbine|tolazoline|naloxone|flumazenil",     "reversal agent",

  # -- cardiovascular / renal ----------------------------------------------
  "pimobendan|digoxin|dobutamine|dopamine",                   "cardiac",
  "pril$|benazepril|enalapril|ramipril|captopril",            "cardiac",
  "olol$|atenolol|sotalol|propranolol|carvedilol",            "cardiac",
  "diltiazem|amlodipine|mexiletine|amiodarone|torsemide",     "cardiac",
  "furosemide|spironolactone|hydrochlorothiazide",            "cardiac",

  # -- endocrine / metabolic -----------------------------------------------
  "trilostane|mitotane|levothyroxine|methimazole|insulin",    "endocrine",
  "desmopressin|cabergoline|altrenogest|melengestrol",        "endocrine",
  "gonadotropin|deslorelin|buserelin|dinoprost|cloprostenol", "reproductive",
  "oxytocin|progesterone|estradiol|testosterone|trenbolone",  "reproductive",
  "somatotropin|zeranol|ractopamine",                         "production",

  # -- behavior / GI / other ----------------------------------------------
  "fluoxetine|clomipramine|sertraline|paroxetine|selegiline", "behavioral",
  "maropitant|ondansetron|metoclopramide|dolasetron",         "antiemetic",
  "omeprazole|famotidine|ranitidine|sucralfate|pantoprazole", "gastrointestinal",
  "cyclosporine|oclacitinib|lokivetmab|azathioprine",         "immunomodulator",
  "vaccine|bacterin|antigen|antitoxin|antiserum",             "vaccine",
  "euthanasia|pentobarbital sodium and phenytoin",            "euthanasia"
)

#' Classify a character vector of ingredient names.
#'
#' Returns a long tibble of ingredient x class. Ingredients that match no rule
#' get the class "unclassified" so they still appear in class filters rather
#' than dropping out of the app.
classify_ingredients <- function(ingredient_names) {
  ing <- unique(ingredient_names[!is.na(ingredient_names)])
  if (length(ing) == 0) return(tibble(activeIngredientName = character(),
                                      drugClass = character()))

  hits <- map_dfr(seq_len(nrow(DRUG_CLASS_RULES)), function(i) {
    rule <- DRUG_CLASS_RULES[i, ]
    matched <- ing[str_detect(ing, regex(rule$pattern, ignore_case = TRUE))]
    if (length(matched) == 0) return(NULL)
    tibble(activeIngredientName = matched, drugClass = rule$class)
  }) |>
    distinct()

  unmatched <- setdiff(ing, hits$activeIngredientName)
  bind_rows(
    hits,
    tibble(activeIngredientName = unmatched, drugClass = "unclassified")
  ) |>
    arrange(activeIngredientName, drugClass)
}

#' Guidelines that apply to a product.
#'
#' Matches on the product's drug classes and on the species it is labeled
#' for. A class rule may also carry a `species_scope`: the equine
#' antimicrobial guidance from AAEP is relevant to an equine antimicrobial,
#' not to every antimicrobial, so a scoped rule only fires when the product is
#' actually labeled for that species. Rules with an empty scope
#' (AVMA, ACVIM, IVETF, WSAVA) apply regardless.
#'
#' De-duplication is by URL rather than by title, because the same
#' organization page is reached by both a class rule and a species rule and
#' should be offered once.
match_guidelines <- function(guidelines, classes = character(),
                             species_groups = character()) {
  if (nrow(guidelines) == 0) return(guidelines)

  scope <- if ("species_scope" %in% names(guidelines)) {
    guidelines$species_scope
  } else {
    rep(NA_character_, nrow(guidelines))
  }
  in_scope <- is.na(scope) | !nzchar(scope) | scope %in% species_groups

  by_class <- guidelines |>
    filter(match_type == "class", match_value %in% classes, in_scope)
  by_species <- guidelines |>
    filter(match_type == "species", match_value %in% species_groups)

  bind_rows(by_class, by_species) |>
    distinct(url, .keep_all = TRUE) |>
    arrange(org_type, organization)
}
