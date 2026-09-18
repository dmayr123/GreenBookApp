# ---------------------------------------------------------------------------
# 08_adverse_events.R -- FDA CVM adverse event reports, via openFDA
#
# For each active ingredient in the Green Book, and each species a product
# containing it is labeled for, counts the adverse event reports submitted to
# FDA's Center for Veterinary Medicine and the most-reported signs (VeDDRA
# terms). The drug page shows these with the caveats that make them honest:
# a report is not proof the drug caused the sign, and counts track how widely
# a drug is used as much as how safe it is.
#
# Queries go by base ingredient name ("butorphanol", not "butorphanol
# tartrate"): openFDA's phrase search matches every salt form, and the reports
# name the ingredient inconsistently.
#
# Rate limits. Without an API key openFDA allows 1,000 requests a day; a full
# refresh is about 1,200. So each run spends at most REQUEST_BUDGET requests
# on the stalest ingredients and keeps everything else from the previous run:
# a fresh start completes in two runs, and after that each run tops up. Set
# the OPENFDA_API_KEY environment variable (a free key from open.fda.gov) to
# lift the limit to 120,000 a day and refresh everything at once.
#
# openFDA's adverse event data is itself updated quarterly, so an entry is
# only re-fetched once it is REFRESH_DAYS old.
#
# Writes data/processed/adverse_events.{rds,parquet}        top signs per ingredient x species
#        data/processed/adverse_events_index.{rds,parquet}  report counts and fetch dates
# ---------------------------------------------------------------------------

library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(fs)
library(arrow)
library(httr2)

source("R/drug_classes.R")   # ingredient_base()

proc_dir <- function(...) path("data", "processed", ...)

API <- "https://api.fda.gov/animalandveterinary/event.json"
API_KEY <- Sys.getenv("OPENFDA_API_KEY")
REQUEST_BUDGET <- if (nzchar(API_KEY)) 100000 else 900
REFRESH_DAYS <- 28
TOP_SIGNS <- 8

# Green Book species group -> openFDA's animal.species value.
SPECIES_MAP <- c(dogs = "Dog", cats = "Cat", horses = "Horse", cattle = "Cattle",
                 swine = "Pig", chickens = "Chicken", turkeys = "Turkey",
                 sheep = "Sheep", goats = "Goat", rabbits = "Rabbit")

UA <- paste0(
  "Mozilla/5.0 (compatible; GreenBookApp/0.1; R ", getRversion(), "; ",
  "veterinary drug reference; +https://github.com/dmayr123/GreenBookApp)"
)

write_table <- function(df, stem) {
  dir_create(proc_dir())
  write_parquet(df, proc_dir(paste0(stem, ".parquet")))
  saveRDS(df, proc_dir(paste0(stem, ".rds")), compress = "gzip")
}

# Counted across the run, so the budget holds however the work divides.
spent <- 0L
rate_limited <- FALSE

#' One openFDA count query. Returns a tibble of term/count, empty when openFDA
#' finds no matching reports (it answers that with a 404).
count_query <- function(search, field, limit) {
  if (spent >= REQUEST_BUDGET || rate_limited) return(NULL)
  spent <<- spent + 1L
  Sys.sleep(0.3)   # 240 requests a minute is the keyless ceiling
  req <- request(API) |>
    req_url_query(search = search, count = field, limit = limit) |>
    req_user_agent(UA) |> req_timeout(60) |>
    req_retry(max_tries = 3, is_transient = function(r) resp_status(r) %in% c(500, 502, 503),
              backoff = function(i) 5 * i) |>
    req_error(is_error = function(r) FALSE)
  if (nzchar(API_KEY)) req <- req_url_query(req, api_key = API_KEY)
  resp <- req_perform(req)

  status <- resp_status(resp)
  if (status == 404) return(tibble(term = character(), count = integer()))
  if (status == 429) {
    rate_limited <<- TRUE
    message("  openFDA rate limit reached; keeping previous data for the rest.")
    return(NULL)
  }
  if (status >= 400) stop("openFDA returned HTTP ", status, " for ", search)
  body <- resp_body_json(resp, simplifyVector = TRUE)
  if (is.null(body$results) || length(body$results) == 0)
    return(tibble(term = character(), count = integer()))
  as_tibble(body$results)
}

ingredient_search <- function(base) {
  sprintf('drug.active_ingredients.name:"%s"', base)
}

#' Ingredient bases and the openFDA species each should be looked up for.
wanted_pairs <- function() {
  need <- proc_dir(c("products.rds", "ingredients.rds", "product_species.rds"))
  if (!all(file_exists(need))) stop("Run R/02_tidy_greenbook.R first.")
  products    <- readRDS(need[1])
  ingredients <- readRDS(need[2])
  species     <- readRDS(need[3])

  products |>
    select(proprietaryNameId, applicationId) |>
    inner_join(ingredients |> distinct(applicationId, activeIngredientName),
               by = "applicationId", relationship = "many-to-many") |>
    inner_join(species |> distinct(proprietaryNameId, speciesGroup),
               by = "proprietaryNameId", relationship = "many-to-many") |>
    filter(speciesGroup %in% names(SPECIES_MAP)) |>
    mutate(base = ingredient_base(activeIngredientName),
           species = unname(SPECIES_MAP[speciesGroup])) |>
    # Bases openFDA's query syntax cannot take, or too short to mean anything.
    filter(nchar(base) >= 4, str_detect(base, "^[a-z0-9][a-z0-9 .-]*$")) |>
    distinct(base, species)
}

refresh_base <- function(base, want_species) {
  sp <- count_query(ingredient_search(base), "animal.species", 25)
  if (is.null(sp)) return(NULL)

  counts <- tibble(species = want_species) |>
    left_join(sp |> rename(species = term, nReports = count), by = "species") |>
    mutate(nReports = coalesce(as.integer(nReports), 0L))

  # count_query() returns NULL only when the budget or rate limit stopped it
  # (no reports is an empty tibble), so NULL here means "cut short". A base
  # cut short is retried next run rather than saved half-done.
  interrupted <- FALSE
  signs <- map_dfr(counts$species[counts$nReports > 0], function(s) {
    q <- sprintf('%s AND animal.species:"%s"', ingredient_search(base), s)
    r <- count_query(q, "reaction.veddra_term_name.exact", TOP_SIGNS)
    if (is.null(r)) { interrupted <<- TRUE; return(NULL) }
    if (nrow(r) == 0) return(NULL)
    tibble(base = base, species = s, term = r$term, count = as.integer(r$count),
           rank = seq_len(nrow(r)))
  })
  if (interrupted) return(NULL)

  list(index = counts |> mutate(base = base, fetched = Sys.Date()),
       signs = signs)
}

check_adverse_events <- function() {
  message("=== openFDA animal adverse events: ", format(Sys.time()), " ===")
  message(if (nzchar(API_KEY)) "  using OPENFDA_API_KEY" else
            sprintf("  no API key; budget %d requests this run", REQUEST_BUDGET))

  pairs <- wanted_pairs()

  old_index <- if (file_exists(proc_dir("adverse_events_index.rds")))
    readRDS(proc_dir("adverse_events_index.rds")) else
    tibble(base = character(), species = character(), nReports = integer(),
           fetched = as.Date(character()))
  old_signs <- if (file_exists(proc_dir("adverse_events.rds")))
    readRDS(proc_dir("adverse_events.rds")) else
    tibble(base = character(), species = character(), term = character(),
           count = integer(), rank = integer())

  # Stalest first; a base is stale if any wanted species is missing or old.
  todo <- pairs |>
    left_join(old_index |> select(base, species, fetched), by = c("base", "species")) |>
    group_by(base) |>
    summarize(species = list(species),
              oldest = if (any(is.na(fetched))) as.Date("1900-01-01") else min(fetched),
              .groups = "drop") |>
    filter(oldest <= Sys.Date() - REFRESH_DAYS) |>
    arrange(oldest, base)
  message(sprintf("  %d ingredients to refresh of %d", nrow(todo), n_distinct(pairs$base)))

  results <- list()
  for (i in seq_len(nrow(todo))) {
    if (spent >= REQUEST_BUDGET || rate_limited) break
    res <- refresh_base(todo$base[i], todo$species[[i]])
    if (!is.null(res)) results[[todo$base[i]]] <- res
  }
  done <- names(results)

  index <- bind_rows(
    old_index |> filter(!base %in% done),
    map_dfr(results, "index")
  ) |>
    semi_join(pairs, by = c("base", "species")) |>
    select(base, species, nReports, fetched)
  signs <- bind_rows(
    old_signs |> filter(!base %in% done),
    map_dfr(results, "signs")
  ) |>
    semi_join(pairs, by = c("base", "species"))

  write_table(signs, "adverse_events")
  write_table(index, "adverse_events_index")

  remaining <- nrow(todo) - length(done)
  cat(sprintf("\n## openFDA adverse events, %s\n\n", format(Sys.Date(), "%d %B %Y")))
  cat(sprintf("- Refreshed %d ingredients using %d requests.\n", length(done), spent))
  cat(sprintf("- %d ingredient x species pairs on file.\n", nrow(index)))
  if (remaining > 0) {
    cat(sprintf("- %d ingredients left for the next run%s.\n", remaining,
                if (!nzchar(API_KEY)) " (set OPENFDA_API_KEY to do them all at once)" else ""))
  }
  invisible(index)
}

if (sys.nframe() == 0) check_adverse_events()
