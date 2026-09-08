# ---------------------------------------------------------------------------
# species_taxonomy.R -- map FDA's species labels onto the groups a vet picks
#
# FDA's species vocabulary is inconsistent in ways that matter for a picker UI:
#   * "Equids" (105 products) and "Horses" (2 products) are separate labels
#     for the same animal, so a naive Horses filter loses 98% of the results.
#   * "Sheep  (Domestic)" contains a double space.
#   * Honeybees are filed under "Bees".
# The lookup below is therefore explicit rather than derived: each raw FDA
# label is assigned to exactly one group, and anything unrecognized falls into
# "Other minor species" rather than silently vanishing from the picker.
# ---------------------------------------------------------------------------

library(dplyr)
library(tibble)
library(stringr)

#' The species groups offered on the landing page, in display order.
#'
#' `major` marks the seven species FDA CVM designates as major; the rest are
#' minor species the user specifically asked to surface (goats, sheep, fish,
#' honeybees, rabbits) plus a catch-all.
SPECIES_GROUPS <- tribble(
  ~group,                ~label,              ~icon,   ~major,
  "cattle",              "Cattle",            "🐄",    TRUE,
  "horses",              "Horses",            "🐎",    TRUE,
  "swine",               "Swine",             "🐖",    TRUE,
  "chickens",            "Chickens",          "🐔",    TRUE,
  "turkeys",             "Turkeys",           "🦃",    TRUE,
  "dogs",                "Dogs",              "🐕",    TRUE,
  "cats",                "Cats",              "🐈",    TRUE,
  "goats",               "Goats",             "🐐",    FALSE,
  "sheep",               "Sheep",             "🐑",    FALSE,
  "fish",                "Fish & aquatics",   "🐟",    FALSE,
  "honeybees",           "Honeybees",         "🐝",    FALSE,
  "rabbits",             "Rabbits",           "🐇",    FALSE,
  "other_minor",         "Other minor species","🦆",   FALSE
)

#' Raw FDA species label -> group.
#'
#' Matching is done on the normalized label (lowercase, punctuation and
#' whitespace removed) so "Sheep  (Domestic)" and "Sheep (Domestic)" collapse
#' to the same key.
SPECIES_LOOKUP <- tribble(
  ~raw,                            ~group,
  "Cattle",                        "cattle",
  "Equids",                        "horses",
  "Horses",                        "horses",
  "Swine",                         "swine",
  "Chickens",                      "chickens",
  "Turkeys",                       "turkeys",
  "Dogs",                          "dogs",
  "Cats (Domestic)",               "cats",
  "Cats",                          "cats",
  "Goats",                         "goats",
  "Sheep  (Domestic)",             "sheep",
  "Sheep (Domestic)",              "sheep",
  "Sheep",                         "sheep",
  "Fish",                          "fish",
  "Cold Blooded Aquatic Animals",  "fish",
  "Crustaceans",                   "fish",
  "Bees",                          "honeybees",
  "Honey Bees",                    "honeybees",
  "Rabbits",                       "rabbits",
  # Everything below is a genuine minor species with only a handful of
  # products; grouping them keeps the picker to thirteen tiles.
  "Pheasants",                     "other_minor",
  "Quail",                         "other_minor",
  "Ducks",                         "other_minor",
  "Chukar Partridges",             "other_minor",
  "Rodents",                       "other_minor",
  "Primates",                      "other_minor",
  "Mustelids (Weasel Family)",     "other_minor",
  "Unspecified",                   "other_minor"
)

norm_species <- function(x) {
  x |> str_to_lower() |> str_replace_all("[^a-z0-9]+", "")
}

#' Attach a `speciesGroup` column to a table holding raw `speciesName`.
#'
#' Unrecognized labels are mapped to "other_minor" so a new FDA species label
#' degrades to "findable under Other" instead of disappearing.
assign_species_group <- function(df, col = "speciesName") {
  # Several raw labels differ only in punctuation or spacing ("Sheep
  # (Domestic)" vs "Sheep  (Domestic)") and therefore collapse to the same
  # normalized key. Deduplicating here keeps the join one-to-one; without it
  # every sheep product would be duplicated in the output.
  key <- SPECIES_LOOKUP |>
    mutate(k = norm_species(raw)) |>
    distinct(k, .keep_all = TRUE) |>
    select(k, speciesGroup = group)

  df |>
    mutate(.k = norm_species(.data[[col]])) |>
    left_join(key, by = c(".k" = "k")) |>
    mutate(speciesGroup = coalesce(speciesGroup, "other_minor")) |>
    select(-.k)
}
