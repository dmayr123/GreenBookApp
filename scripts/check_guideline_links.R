# ---------------------------------------------------------------------------
# check_guideline_links.R -- verify guideline links by CONTENT, not status code
#
# A 200 response proves nothing. ivetf.org returned HTTP 200 while actually
# redirecting to a parked domain ("ww547.ivetf.org") that has nothing to do
# with the International Veterinary Epilepsy Task Force. Checking only the
# status code marked that link "verified" and shipped a dead reference to a
# clinician.
#
# So each row in data/reference/guidelines.csv carries an `expect` column: a
# string that must appear in the fetched page for the link to count as good.
# The check fetches, strips tags, and looks for it.
#
# Sites that block automated requests (several veterinary associations sit
# behind bot protection) are reported as "blocked" -- explicitly *not* the
# same as verified, and not the same as broken.
#
# Run:  Rscript scripts/check_guideline_links.R
# ---------------------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(httr2)

UA <- paste0("Mozilla/5.0 (compatible; GreenBookApp link check; R ",
             getRversion(), ")")

strip_tags <- function(html) {
  html |>
    str_replace_all("<script[^>]*>.*?</script>", " ") |>
    str_replace_all("<[^>]*>", " ") |>
    str_squish()
}

#' Fall back to the system curl binary.
#'
#' Some association sites negotiate TLS in a way httr2 fails on here, and
#' reporting those as "unreachable" would be a false alarm about a link that
#' is actually fine. curl reaches them, so a failure is only believed when
#' both clients fail.
fetch_via_curl <- function(url) {
  out <- tryCatch(
    system2("curl", c("-sL", "--max-time", "25", "-A", shQuote(UA),
                      "-w", shQuote("\\n__STATUS__%{http_code}"),
                      shQuote(url)),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  if (length(out) == 0) return(list(status = NA_integer_, text = "", final = url))
  body <- paste(out, collapse = "\n")
  status <- suppressWarnings(as.integer(
    str_match(body, "__STATUS__([0-9]{3})\\s*$")[, 2]))
  list(status = status,
       text = strip_tags(str_remove(body, "__STATUS__[0-9]{3}\\s*$")),
       final = url)
}

fetch_text <- function(url) {
  res <- tryCatch(
    request(url) |>
      req_user_agent(UA) |>
      req_timeout(30) |>
      req_retry(max_tries = 2) |>
      req_perform(),
    error = function(e) e
  )
  if (inherits(res, "error")) return(fetch_via_curl(url))
  list(
    status = resp_status(res),
    final  = res$url,
    text   = tryCatch(strip_tags(resp_body_string(res)), error = function(e) "")
  )
}

# Must match GUIDELINE_MAX_AGE_YEARS in app/global.R.
GUIDELINE_MAX_AGE_YEARS <- 15

#' Report dated guidelines at or approaching the age cutoff.
#'
#' The app drops expired links at load, so this exists to make the pruning
#' visible rather than silent, and to give warning before a link disappears --
#' an expiring consensus statement usually needs replacing with its newer
#' edition, not simply removing.
check_ages <- function(g, today = Sys.Date()) {
  year_now <- as.integer(format(today, "%Y"))
  cutoff <- year_now - GUIDELINE_MAX_AGE_YEARS

  dated <- g |>
    filter(!is.na(published)) |>
    distinct(organization, title, url, published) |>
    mutate(age = year_now - published,
           state = case_when(
             published < cutoff       ~ "EXPIRED - remove or replace",
             published < cutoff + 3   ~ "expiring within 3 years",
             TRUE                     ~ "current"))

  message(sprintf("\nAge check (limit %d years; cutoff = published %d or later):",
                  GUIDELINE_MAX_AGE_YEARS, cutoff))
  if (nrow(dated) == 0) {
    message("  no dated entries; all links are organisation hubs.")
  } else {
    print(as.data.frame(dated |> select(organization, published, age, state,
                                        title)), right = FALSE)
  }

  expired <- dated |> filter(state == "EXPIRED - remove or replace")
  if (nrow(expired)) {
    warning(sprintf("%d guideline link(s) exceed %d years and are dropped by ",
                    nrow(expired), GUIDELINE_MAX_AGE_YEARS),
            "the app at load: ", paste(expired$title, collapse = "; "))
  }
  invisible(dated)
}

check_links <- function(path = "data/reference/guidelines.csv") {
  g <- read_csv(path, show_col_types = FALSE)
  if (!"expect" %in% names(g)) {
    stop("guidelines.csv needs an `expect` column naming text that must ",
         "appear on each page.")
  }
  check_ages(g)

  targets <- g |> distinct(url, expect, organization)
  message(sprintf("Checking %d distinct URLs by content ...", nrow(targets)))

  res <- pmap_dfr(targets, function(url, expect, organization) {
    r <- fetch_text(url)
    found <- nzchar(r$text) &&
      str_detect(r$text, regex(expect, ignore_case = TRUE))
    verdict <- case_when(
      is.na(r$status)                      ~ "unreachable",
      r$status %in% c(403, 429)            ~ "blocked",
      r$status >= 400                      ~ "http error",
      found                                ~ "verified",
      # A page that loads but lacks the expected text is the dangerous case:
      # this is what a parked or rebranded domain looks like.
      TRUE                                 ~ "CONTENT MISMATCH"
    )
    tibble(organization, url, status = r$status, verdict,
           redirected_to = if (!identical(r$final, url)) r$final else NA_character_)
  })

  print(as.data.frame(res), right = FALSE)

  bad <- res |> filter(verdict == "CONTENT MISMATCH")
  if (nrow(bad)) {
    warning(sprintf("%d link(s) load but do not contain the expected text: %s",
                    nrow(bad), paste(bad$url, collapse = ", ")))
  }
  invisible(res)
}

if (sys.nframe() == 0) check_links()
