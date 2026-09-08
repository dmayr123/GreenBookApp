# ---------------------------------------------------------------------------
# 03_ndc_dailymed.R -- attach NDC codes from DailyMed
#
# Animal Drugs @ FDA does not carry NDC codes at all: the Green Book is
# organised by application number, and the NDC lives in the labeller's
# Structured Product Label. DailyMed publishes those labels, so the NDC has
# to be joined in from there.
#
# We use DailyMed's REST API rather than its bulk animal release, which is
# 1.27 GB of SPL XML -- far too heavy to re-download every month for a field
# that changes rarely. The API costs two calls per product but the results are
# cached per product on disk, so the monthly refresh only pays for products
# whose label actually changed.
#
# Matching is by normalised proprietary name. That is imperfect: a label whose
# DailyMed title differs from the Green Book trade name will not match, and
# such products simply carry no NDC rather than a guessed one. `matchType`
# records how each NDC was found so the app can show its provenance.
# ---------------------------------------------------------------------------

library(httr2)
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(fs)
library(arrow)

DAILYMED <- "https://dailymed.nlm.nih.gov/dailymed/services/v2"
UA <- paste0("GreenBookApp/0.1 (R ", getRversion(),
             "; veterinary drug reference; +https://github.com/dmayr123/GreenBookApp)")

cache_dir <- function(...) path("data", "raw", "dailymed", ...)
proc_dir  <- function(...) path("data", "processed", ...)

#' Write the NDC table in both formats, matching write_table() in
#' 02_tidy_greenbook.R.
#'
#' Parquet is the archival copy; the RDS twin is what the app reads, so that
#' global.R never has to reference arrow. That keeps arrow out of the
#' WebAssembly build, where shinylive would otherwise ship it to every visitor.
write_ndc <- function(df) {
  write_parquet(df, proc_dir("ndc.parquet"))
  saveRDS(df, proc_dir("ndc.rds"), compress = "xz")
}

norm_text <- function(x) {
  x |> str_to_lower() |> str_replace_all("[^a-z0-9]+", "") |> coalesce("")
}

dm_get <- function(url) {
  tryCatch(
    request(url) |>
      req_user_agent(UA) |>
      req_timeout(45) |>
      req_retry(max_tries = 3, backoff = ~ 2^.x) |>
      req_perform() |>
      resp_body_string(),
    error = function(e) NULL
  )
}

#' The most distinctive word in a trade name, used as the DailyMed query.
#'
#' DailyMed's `drug_name` search is a contains-match, so searching the base
#' word ("Panoquell") finds the suffixed label ("PANOQUELL CA1") that an exact
#' search for "PANOQUELL-CA1" would miss. Conditional-approval suffixes and
#' trademark glyphs are stripped for the same reason.
search_stem <- function(name) {
  name |>
    str_remove_all("[®™©]") |>
    str_remove_all("(?i)[-\\s]*CA[-\\s]?[0-9]+\\s*$") |>
    str_squish() |>
    str_split("\\s+") |>
    map_chr(~ .x[1]) |>
    str_remove_all("[^A-Za-z0-9]")
}

#' Build (but do not perform) a configured request.
dm_req <- function(url) {
  request(url) |>
    req_user_agent(UA) |>
    req_timeout(45) |>
    req_retry(max_tries = 3, backoff = ~ 2^.x)
}

#' Perform many requests concurrently, returning response bodies or NULL.
#'
#' DailyMed responds in roughly 1.4 s, so a sequential crawl of ~5,000 calls
#' would take over two hours. The work is entirely latency-bound, so running a
#' handful of requests at once cuts that to about twenty minutes.
#' `max_active` is kept modest deliberately: this is a public NLM service and
#' the goal is to stop waiting on latency, not to saturate it.
dm_get_many <- function(urls, max_active = 6) {
  if (length(urls) == 0) return(list())
  resps <- req_perform_parallel(map(urls, dm_req), max_active = max_active,
                                on_error = "continue")
  map(resps, function(r) {
    if (inherits(r, "error") || is.null(r)) return(NULL)
    tryCatch(resp_body_string(r), error = function(e) NULL)
  })
}

#' Fetch and cache every uncached stem, in batches.
#'
#' Two parallel rounds per batch: first the name searches, then the NDC
#' lookups for every SPL those searches returned.
prefetch_stems <- function(stems, batch = 60) {
  dir_create(cache_dir())
  todo <- stems[!file_exists(cache_dir(paste0(stems, ".json")))]
  if (length(todo) == 0) return(invisible(NULL))
  message(sprintf("  %d stems to fetch", length(todo)))

  for (start in seq(1, length(todo), by = batch)) {
    chunk <- todo[start:min(start + batch - 1, length(todo))]

    searches <- dm_get_many(paste0(
      DAILYMED, "/spls.json?drug_name=",
      vapply(chunk, utils::URLencode, character(1), reserved = TRUE)))

    spl_lists <- map(searches, function(res) {
      if (is.null(res)) return(list())
      tryCatch(fromJSON(res, simplifyDataFrame = FALSE)$data %||% list(),
               error = function(e) list())
    })

    # Flatten to one NDC request per SPL, fetch them all at once, then put the
    # answers back against the stem they came from.
    setids <- unlist(map(spl_lists, ~ map_chr(.x, "setid")), use.names = FALSE)
    ndc_by_setid <- list()
    if (length(setids)) {
      bodies <- dm_get_many(paste0(DAILYMED, "/spls/", setids, "/ndcs.json"))
      ndc_by_setid <- set_names(map(bodies, function(nd) {
        if (is.null(nd)) return(character())
        tryCatch(
          unlist(map(fromJSON(nd, simplifyDataFrame = FALSE)$data$ndcs, "ndc")) %||%
            character(),
          error = function(e) character())
      }), setids)
    }

    walk2(chunk, spl_lists, function(stem, spls) {
      enriched <- map(spls, function(s) list(
        setid = s$setid, title = s$title,
        ndcs = ndc_by_setid[[s$setid]] %||% character()
      ))
      write(toJSON(enriched, auto_unbox = TRUE), cache_dir(paste0(stem, ".json")))
    })

    message(sprintf("  %d / %d", min(start + batch - 1, length(todo)), length(todo)))
  }
  invisible(NULL)
}

#' Read one cached stem back as setid + NDC rows.
lookup_name <- function(stem) {
  if (is.na(stem) || !nzchar(stem)) return(NULL)
  f <- cache_dir(paste0(stem, ".json"))
  if (!file_exists(f)) return(NULL)

  cached <- tryCatch(fromJSON(f, simplifyVector = FALSE),
                     error = function(e) list())
  if (length(cached) == 0) return(NULL)

  map_dfr(cached, function(e) {
    ndcs <- unlist(e$ndcs)
    tibble(
      stem      = stem,
      setid     = e$setid,
      splTitle  = e$title,
      # The DailyMed title is "NAME (INGREDIENT) FORM [LABELLER]"; the trade
      # name is everything before the first parenthesis and the labeller is
      # the trailing bracketed segment.
      splName   = str_squish(str_remove(e$title, "\\s*\\(.*$")),
      # Dose form sits between the ingredient parenthesis and the labeller
      # bracket, and is what keeps an injectable label's NDC codes off a
      # medicated feed article.
      splForm   = str_squish(str_extract(e$title, "(?<=\\))[^\\[]*") %||% ""),
      splLabeler = str_squish(str_remove_all(
        str_extract(e$title, "\\[[^]]*\\]$") %||% "", "[\\[\\]]")),
      ndc       = if (length(ndcs)) ndcs else NA_character_
    )
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Build the product -> NDC table.
build_ndc_table <- function() {
  # same_company(): DailyMed carries human labels alongside veterinary ones,
  # and trade names collide across the two. Matching on name alone attached a
  # Bracco Diagnostics human contrast agent's NDC codes to a Zoetis
  # veterinary product. The labeller must correspond to FDA's sponsor.
  source("R/label_dailymed.R", local = TRUE)

  products <- read_parquet(proc_dir("products.parquet"))
  applications <- read_parquet(proc_dir("applications.parquet"))

  products <- products |>
    left_join(applications |> select(applicationId, sponsorName),
              by = "applicationId") |>
    mutate(stem = search_stem(proprietaryName),
           nameKey = norm_text(proprietaryName),
           sponsorKey = norm_text(sponsorName))

  stems <- unique(products$stem)
  stems <- stems[!is.na(stems) & nzchar(stems)]
  message(sprintf("Querying DailyMed for %d distinct name stems ...", length(stems)))

  prefetch_stems(stems)
  dm <- map_dfr(stems, lookup_name)

  if (nrow(dm) == 0) {
    warning("DailyMed returned nothing; writing an empty NDC table.")
    out <- tibble(proprietaryNameId = integer(), ndc = character(),
                  setid = character(), matchType = character(),
                  dailymedUrl = character())
    write_ndc(out)
    return(invisible(out))
  }

  dm <- mutate(dm, splKey = norm_text(splName),
                   labelerKey = norm_text(splLabeler))

  # An exact name match is trustworthy only when the labeller is also the
  # sponsor FDA recorded.
  exact <- products |>
    inner_join(dm, by = c("stem", "nameKey" = "splKey"),
               relationship = "many-to-many") |>
    filter(map2_lgl(sponsorName, splLabeler, same_company),
           pmap_lgl(list(doseFormName, routes, splForm), compatible_form)) |>
    mutate(matchType = "exact name")

  # A stem-only match is trustworthy in exactly one case: when the stem
  # resolves to a single SPL, so there is nothing to confuse it with. Applied
  # unconditionally it is worse than useless -- the stem "Carprofen" matches
  # every labeller's carprofen product, which attached 2,923 NDCs to one
  # entry. Better to show no NDC and a DailyMed search link than a wrong one.
  unambiguous <- dm |>
    group_by(stem) |>
    filter(n_distinct(splKey) == 1) |>
    ungroup()

  loose <- products |>
    anti_join(exact, by = "proprietaryNameId") |>
    inner_join(unambiguous, by = "stem", relationship = "many-to-many") |>
    filter(map2_lgl(sponsorName, splLabeler, same_company),
           pmap_lgl(list(doseFormName, routes, splForm), compatible_form)) |>
    mutate(matchType = "name stem (single label)")

  out <- bind_rows(exact, loose) |>
    filter(!is.na(ndc)) |>
    transmute(
      proprietaryNameId, ndc, setid, matchType, splTitle,
      dailymedUrl = paste0(
        "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=", setid)
    ) |>
    distinct()

  write_ndc(out)
  message(sprintf("Wrote %d NDC rows covering %d products (%d exact).",
                  nrow(out), n_distinct(out$proprietaryNameId),
                  sum(out$matchType == "exact name")))
  invisible(out)
}

if (sys.nframe() == 0) build_ndc_table()
