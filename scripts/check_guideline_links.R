# ---------------------------------------------------------------------------
# check_guideline_links.R -- verify guideline links by CONTENT, not status code
#
# A 200 response proves nothing. The International Veterinary Epilepsy Task
# Force's former organisation domain returned HTTP 200 while actually
# redirecting to an unsecured parked page with nothing to do with the task
# force. Checking only the status code marked that link "verified" and shipped
# a dead reference to a clinician. The entry now points at the consensus paper
# itself; the old domain is deliberately not recorded anywhere in this repo.
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

#' Check the manufacturer sites used for tier-1 product label links.
#'
#' Animal health companies are acquired and rebranded often -- Bayer's animal
#' health business went to Elanco, Fort Dodge's to Zoetis -- so a manufacturer
#' URL that was right when written can quietly start pointing somewhere else.
#' Each entry is checked for its own name appearing on the page.
check_manufacturers <- function() {
  if (!file.exists("R/label_sources.R")) return(invisible(NULL))
  source("R/label_sources.R", local = TRUE)

  sites <- MANUFACTURER_SITES |> distinct(manufacturer, url)
  message(sprintf("\nChecking %d manufacturer sites by content ...", nrow(sites)))

  res <- pmap_dfr(sites, function(manufacturer, url) {
    r <- fetch_text(url)
    # Match on the first word of the company name: "Merck Animal Health
    # (Intervet)" will not appear verbatim, but "Merck" will.
    key <- str_extract(manufacturer, "^[A-Za-z-]+")
    found <- nzchar(r$text) && str_detect(r$text, regex(key, ignore_case = TRUE))
    insecure <- !str_starts(url, "https://") ||
      (!is.na(r$final) && str_starts(r$final, "http://"))
    tibble(manufacturer, url, status = r$status,
           verdict = case_when(
             insecure                  ~ "INSECURE - not HTTPS",
             is.na(r$status)           ~ "unreachable",
             r$status %in% c(403, 429) ~ "blocked",
             r$status >= 400           ~ "http error",
             found                     ~ "verified",
             TRUE                      ~ "CONTENT MISMATCH"))
  })

  print(as.data.frame(res), right = FALSE)
  bad <- res |> filter(verdict %in% c("CONTENT MISMATCH", "INSECURE - not HTTPS"))
  if (nrow(bad)) {
    warning(sprintf("%d manufacturer site(s) need review: %s",
                    nrow(bad), paste(bad$manufacturer, collapse = ", ")))
  }
  invisible(res)
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

    # A guideline link must be HTTPS, both as written and after any redirect.
    # A lapsed domain that has been re-registered typically lands on a plain
    # HTTP parked page, so an insecure destination is treated as a failure in
    # its own right rather than being reported as merely "verified".
    insecure <- !str_starts(url, "https://") ||
      (!is.na(r$final) && str_starts(r$final, "http://"))

    verdict <- case_when(
      insecure                             ~ "INSECURE - not HTTPS",
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

  bad <- res |> filter(verdict %in% c("CONTENT MISMATCH", "INSECURE - not HTTPS"))
  if (nrow(bad)) {
    warning(sprintf(
      "%d link(s) failed: not HTTPS, or loaded without the expected text: %s",
      nrow(bad), paste(bad$url, collapse = ", ")))
  }
  invisible(res)
}

if (sys.nframe() == 0) {
  check_links()
  check_manufacturers()
}
