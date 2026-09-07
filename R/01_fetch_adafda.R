# ---------------------------------------------------------------------------
# 01_fetch_adafda.R -- pull the Green Book from Animal Drugs @ FDA
#
# ADAFDA (https://animaldrugsatfda.fda.gov) is an AngularJS front end over a
# public, unauthenticated REST API. There is no bulk download offered on the
# FDA website, but the API the app itself uses will return the whole catalogue,
# so we call it directly rather than scraping the rendered pages.
#
# Two calls do the heavy lifting:
#   POST .../advancedSearchForExcelPdf  -> every application, one payload
#   GET  .../retrievePreviewBean/{id}   -> the full detail record for one
#
# Everything lands in data/raw/ as untouched JSON. Nothing here reshapes the
# data; that is 02_tidy_greenbook.R's job. Keeping the two apart means a
# schema surprise from FDA shows up as a tidy-step failure against a payload
# we still have on disk, instead of a silent hole in the app.
# ---------------------------------------------------------------------------

library(httr2)
library(jsonlite)
library(purrr)
library(fs)

ADAFDA_BASE <- "https://animaldrugsatfda.fda.gov/adafda/app/search/public"

# FDA asks that automated users identify themselves.
UA <- paste0(
  "GreenBookApp/0.1 (R ", getRversion(), "; veterinary drug reference; ",
  "+https://github.com/dmayr123/GreenBookApp)"
)

raw_dir <- function(...) path("data", "raw", ...)

#' One configured request against the ADAFDA API.
#'
#' The service is occasionally slow rather than broken, hence the generous
#' timeout and the retry on transient status codes.
adafda_req <- function(path_suffix) {
  request(paste0(ADAFDA_BASE, "/", path_suffix)) |>
    req_user_agent(UA) |>
    req_timeout(60) |>
    req_retry(max_tries = 4, backoff = ~ 2^.x)
}

# -- the whole catalogue in one POST ----------------------------------------

# The search form sends every field; nulls mean "unconstrained". Sending the
# complete object (rather than just the fields we care about) matches what the
# browser does and avoids relying on server-side defaults.
EMPTY_CRITERIA <- list(
  basicSearchTerm = NULL, applicationNumber = NULL, sponsorName = NULL,
  activeIngredientName = NULL, applicationStatusCode = NULL,
  applicationStatusValue = NULL, indication = NULL, proprietaryName = NULL,
  doseFormName = NULL, routeName = NULL, speciesName = NULL,
  isExact = FALSE, sortField = "applicationNumber", sortDirection = "false",
  pageSize = NULL, pageNumber = NULL
)

#' Fetch the application index (~2,400 rows).
#'
#' `...ForExcelPdf` is the endpoint behind the site's "export" button, so it
#' ignores paging and returns the complete result set.
fetch_catalogue <- function() {
  message("Fetching application catalogue ...")
  body <- toJSON(EMPTY_CRITERIA, auto_unbox = TRUE, null = "null")

  resp <- adafda_req("advancedSearchForExcelPdf") |>
    req_body_raw(body, type = "application/json") |>
    req_perform()

  txt <- resp_body_string(resp)
  dir_create(raw_dir())
  write(txt, raw_dir("catalogue.json"))

  ids <- fromJSON(txt)$applicationId
  message(sprintf("  %d applications", length(ids)))
  ids
}

# -- per-application detail --------------------------------------------------

#' Fetch one detail record, returning NULL rather than stopping on failure.
#'
#' A handful of application IDs 404 or return malformed JSON. One bad record
#' should not abandon a 40-minute crawl, so failures are collected and
#' reported at the end instead of thrown.
fetch_one_bean <- function(id) {
  out <- raw_dir("beans", paste0(id, ".json"))
  if (file_exists(out) && file_info(out)$size > 0) return(TRUE)

  res <- tryCatch(
    adafda_req(paste0("retrievePreviewBean/", id)) |>
      req_perform() |>
      resp_body_string(),
    error = function(e) NULL
  )
  if (is.null(res)) return(FALSE)

  write(res, out)
  TRUE
}

#' Crawl every detail record.
#'
#' Cached per-ID on disk so an interrupted run resumes where it stopped.
#' `sleep` is deliberate politeness towards a government server that has no
#' documented rate limit -- do not remove it.
fetch_all_beans <- function(ids, sleep = 0.08) {
  dir_create(raw_dir("beans"))
  message(sprintf("Fetching %d detail records ...", length(ids)))

  ok <- logical(length(ids))
  for (i in seq_along(ids)) {
    ok[i] <- fetch_one_bean(ids[i])
    Sys.sleep(sleep)
    if (i %% 200 == 0) message(sprintf("  %d / %d", i, length(ids)))
  }

  failed <- ids[!ok]
  if (length(failed)) {
    warning(sprintf("%d detail records failed: %s",
                    length(failed), paste(head(failed, 20), collapse = ", ")))
  }
  message(sprintf("  %d / %d retrieved", sum(ok), length(ids)))
  invisible(failed)
}

# -- small reference tables --------------------------------------------------

#' Fetch the code lists and the monthly-update index.
#'
#' The monthly-update index is what 04_monthly_update.R diffs against to decide
#' whether FDA has published a new Green Book edition.
fetch_reference <- function() {
  message("Fetching code lists ...")
  for (nm in c("codes/application_type", "codes/application_status",
               "monthlyUpdates")) {
    txt <- adafda_req(nm) |> req_perform() |> resp_body_string()
    write(txt, raw_dir(paste0(gsub("/", "_", nm), ".json")))
  }
}

#' Fetch SPL (product label) links for each application.
#'
#' Kept separate from the detail crawl because it is a different endpoint and
#' many applications have none.
fetch_spl_links <- function(ids, sleep = 0.05) {
  out <- raw_dir("spl_links.json")
  if (file_exists(out)) {
    message("SPL links already cached; skipping.")
    return(invisible(NULL))
  }
  message(sprintf("Fetching SPL links for %d applications ...", length(ids)))

  links <- map(ids, function(id) {
    res <- tryCatch(
      adafda_req(paste0("spllink/", id)) |>
        req_perform() |> resp_body_string(),
      error = function(e) "[]"
    )
    Sys.sleep(sleep)
    list(applicationId = id, spl = fromJSON(res, simplifyDataFrame = FALSE))
  })

  write(toJSON(links, auto_unbox = TRUE), out)
  invisible(NULL)
}

# -- entry point -------------------------------------------------------------

fetch_everything <- function() {
  ids <- fetch_catalogue()
  fetch_reference()
  fetch_all_beans(ids)
  fetch_spl_links(ids)
  message("Raw fetch complete -> data/raw/")
  invisible(ids)
}

if (sys.nframe() == 0) fetch_everything()
