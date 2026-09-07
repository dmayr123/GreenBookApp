# ---------------------------------------------------------------------------
# search.R -- the drug search that replaces the Green Book's
#
# Three things make FDA's own search frustrating, and each has a fix here:
#
#  1. Punctuation is significant. "CA-1", "-CA-1" and "CA1" return different
#     result sets. Fix: both the query and the indexed text are reduced to
#     lowercase alphanumerics before matching, so all three are one query.
#
#  2. Multi-word queries behave as a single literal. Searching
#     "fluralaner cattle" finds nothing because no field contains that string.
#     Fix: the query is split into tokens and every token must match somewhere
#     in the record (AND), which is what users expect from a search box.
#
#  3. Results arrive unranked, so an exact trade-name hit can sit below
#     dozens of incidental mentions. Fix: matches are scored, exact and
#     prefix hits on the proprietary name first.
# ---------------------------------------------------------------------------

library(dplyr)
library(stringr)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Canonical form for matching. Must stay identical to the `norm_text()` used
#' when the index was built in 02_tidy_greenbook.R, or nothing will match.
norm_text <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[‘’“”]", "") |>
    str_replace_all("[^a-z0-9]+", "") |>
    coalesce("")
}

#' Split a query into normalised tokens.
#'
#' Splitting happens on the raw query, before punctuation is stripped, so
#' "fluralaner, cattle" yields two tokens rather than one run-on string.
#' Tokens that normalise to nothing (a lone hyphen) are dropped.
tokenize_query <- function(query) {
  if (is.null(query) || !nzchar(str_trim(query))) return(character())
  parts <- str_split(str_trim(query), "\\s+")[[1]]
  toks <- norm_text(parts)
  toks[nzchar(toks)]
}

#' Score one candidate set against the query tokens.
#'
#' Scoring is deliberately simple and explainable: a vet should be able to see
#' why a row ranked where it did. Points are additive across tokens.
score_matches <- function(idx, tokens, full_key = "") {
  # These are precomputed columns on the index (02_tidy_greenbook.R). Falling
  # back to normalising on the fly keeps the function usable against an older
  # index, but the fast path is the one that runs in the app.
  name_key <- idx$nameKey %||% norm_text(idx$proprietaryName)
  ing_key  <- idx$ingKey  %||% norm_text(idx$ingredients)
  app_key  <- idx$appKey  %||% norm_text(idx$applicationNumber)

  score <- numeric(nrow(idx))

  # The whole query with its spaces removed. This is weighted far above any
  # individual token because a short token is a weak signal: searching "ca 1"
  # gives "ca" a prefix bonus on every drug starting with those letters
  # (Carbam, Carprofen, Carnidazole), which would otherwise outrank the CA1
  # products the user was plainly looking for.
  if (nzchar(full_key)) {
    score <- score +
      300 * (name_key == full_key) +
      150 * (str_starts(name_key, fixed(full_key)) & name_key != full_key) +
      100 * str_detect(name_key, fixed(full_key))
  }

  for (tk in tokens) {
    score <- score +
      # Whole trade name is exactly the token -- almost always what was meant.
      100 * (name_key == tk) +
      # Trade name starts with the token: "panoquell" -> "PANOQUELL-CA1".
      40  * (str_starts(name_key, fixed(tk)) & name_key != tk) +
      20  * str_detect(name_key, fixed(tk)) +
      # Application number typed directly.
      60  * (app_key == tk) +
      15  * str_detect(ing_key, fixed(tk)) +
      # Sponsor, dose form, species, route.
      5   * str_detect(idx$searchKey, fixed(tk))
  }
  score
}

#' Search the product index.
#'
#' @param idx        the `search_index` table from 02_tidy_greenbook.R
#' @param query      free text
#' @param deep       also match indications and strength text; off by default
#'                   because it is much noisier
#' @param species_group optional species filter, applied before scoring
#' @param categories    optional vector of collapsed categories to keep
#' @param include_withdrawn keep voluntarily withdrawn products
#'
#' Returns the matching rows ordered by score. An empty query returns the
#' filtered set unranked, which is what the species-browse view wants.
search_drugs <- function(idx, query = "", deep = FALSE,
                         species_group = NULL, categories = NULL,
                         include_withdrawn = FALSE) {

  out <- idx
  if (!include_withdrawn) {
    out <- filter(out, marketStatus != "Voluntarily withdrawn")
  }
  if (!is.null(categories) && length(categories)) {
    out <- filter(out, category %in% categories)
  }
  if (!is.null(species_group) && length(species_group) &&
      !identical(species_group, "any")) {
    out <- filter(out, str_detect(coalesce(speciesGroups, ""),
                                  fixed(species_group)))
  }

  tokens <- tokenize_query(query)
  if (length(tokens) == 0) {
    return(arrange(mutate(out, .score = 0), proprietaryName))
  }

  haystack <- if (deep) out$searchKeyWide else out$searchKey
  full_key <- norm_text(query)

  # AND across tokens: every token must appear somewhere in the record. A
  # record matching the collapsed whole query is kept regardless, so a user
  # who types "ca 1" still finds the CA1 products even though the two loose
  # tokens would otherwise have to match independently.
  keep <- rep(TRUE, nrow(out))
  for (tk in tokens) keep <- keep & str_detect(haystack, fixed(tk))
  if (nzchar(full_key)) keep <- keep | str_detect(haystack, fixed(full_key))
  out <- out[keep, , drop = FALSE]
  if (nrow(out) == 0) return(mutate(out, .score = numeric()))

  out$.score <- score_matches(out, tokens, full_key)
  arrange(out, desc(.score), proprietaryName)
}

#' Suggestions for the type-ahead box.
#'
#' Draws from trade names and ingredients, prefix-matched on the normalised
#' form so typing "ca1" surfaces "PANOQUELL-CA1".
suggest_terms <- function(idx, query, n = 8) {
  tk <- tokenize_query(query)
  if (length(tk) == 0) return(character())
  last <- tail(tk, 1)

  pool <- unique(c(idx$proprietaryName,
                   unlist(str_split(coalesce(idx$ingredients, ""), ";\\s*"))))
  pool <- pool[nzchar(pool)]
  key <- norm_text(pool)

  hit <- pool[str_starts(key, fixed(last))]
  head(unique(hit), n)
}
