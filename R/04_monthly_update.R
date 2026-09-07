# ---------------------------------------------------------------------------
# 04_monthly_update.R -- refresh against FDA's monthly Green Book publication
#
# FDA republishes the Green Book in its entirety every month. A full re-crawl
# takes ~15 minutes and ~5,000 requests against a government server, so this
# script does the polite thing instead: pull the catalogue (one request),
# diff it against the copy on disk, and re-fetch detail records only for
# applications that are new or whose type/status changed.
#
# The diff is also the point of the exercise. A conditional approval moving
# from CNADA to NADA, or a product being voluntarily withdrawn, is exactly the
# kind of change a practising vet needs told about -- so every run writes a
# dated changelog rather than silently overwriting the data.
#
# Run:  Rscript R/04_monthly_update.R
# ---------------------------------------------------------------------------

library(jsonlite)
library(dplyr)
library(stringr)
library(fs)
library(readr)
library(glue)
library(tibble)

source("R/01_fetch_adafda.R")

CHANGELOG <- path("data", "changelog")
# Defined here rather than borrowed from 02_tidy_greenbook.R, which is only
# sourced later in the run.
proc_dir  <- function(...) path("data", "processed", ...)

#' Compare the freshly fetched catalogue against the one already on disk.
compare_catalogues <- function(old_path, new_json) {
  new <- as_tibble(fromJSON(new_json)) |>
    select(applicationId, applicationNumber, applicationType,
           applicationStatusCode, proprietaryName, sponsorName,
           voluntaryWithdrawalDate)

  # An empty result still has to carry the columns a populated one would,
  # including the `_old` twins the join below produces. write_changelog()
  # filters on applicationType_old, so returning a bare `new[0, ]` here made
  # the very first run -- the one with nothing to compare against -- fail with
  # "object 'applicationType_old' not found".
  empty_changed <- new[0, ] |>
    mutate(applicationType_old = character(),
           applicationStatusCode_old = character(),
           voluntaryWithdrawalDate_old = character())

  if (!file_exists(old_path)) {
    message("No previous catalogue; treating every application as new.")
    return(list(new = new, added = new, changed = empty_changed,
                withdrawn = empty_changed, refetch = new$applicationId))
  }

  old <- as_tibble(fromJSON(old_path)) |>
    select(applicationId, applicationType, applicationStatusCode,
           voluntaryWithdrawalDate)

  added <- anti_join(new, old, by = "applicationId")

  joined <- inner_join(new, old, by = "applicationId",
                       suffix = c("", "_old"))

  # Compare through coalesce() rather than `!=` alone: FDA leaves status codes
  # null for some applications, and `NA != "A"` is NA, which filter() drops --
  # so a genuine change to or from a null status would be missed silently.
  changed <- joined |>
    filter(
      coalesce(applicationType, "") != coalesce(applicationType_old, "") |
      coalesce(applicationStatusCode, "") != coalesce(applicationStatusCode_old, "")
    )

  withdrawn <- joined |>
    filter(is.na(voluntaryWithdrawalDate_old), !is.na(voluntaryWithdrawalDate))

  list(
    new = new, added = added, changed = changed, withdrawn = withdrawn,
    refetch = unique(c(added$applicationId, changed$applicationId,
                       withdrawn$applicationId))
  )
}

#' Structured record of one month's changes, for the app to render.
#'
#' The markdown changelog is for a person reading the repository; this is the
#' same information as data, so the landing page can show what changed without
#' parsing prose. History accumulates and is capped, because the app ships
#' this file to every visitor.
write_update_log <- function(diff, keep_months = 24, verified = FALSE) {
  # Hard refusal unless the caller is the real monthly run.
  #
  # This exists because a test that simulated a month of changes wrote its
  # invented rows straight into data/processed, and the app then displayed
  # them as fact -- reporting a conditional approval as converted and a
  # marketed product as withdrawn. On a clinical tool that is the most
  # dangerous class of bug there is: confidently wrong drug status.
  #
  # run_monthly_update() passes verified = TRUE after diffing the catalogue it
  # actually fetched from FDA. Nothing else can write this file, so a
  # simulation can no longer reach the app no matter how it is invoked.
  if (!isTRUE(verified)) {
    warning("write_update_log(): refusing to write. Only run_monthly_update() ",
            "may write the update log, because it is the only caller that has ",
            "diffed a catalogue actually fetched from FDA. Simulated or test ",
            "diffs must never reach data/processed.")
    return(invisible(NULL))
  }

  f <- proc_dir("update_log.rds")
  stamp <- Sys.Date()

  row_of <- function(df, kind) {
    if (nrow(df) == 0) return(NULL)
    tibble(
      runDate = stamp, kind = kind,
      applicationNumber = as.integer(df$applicationNumber),
      proprietaryName = str_squish(coalesce(df$proprietaryName, "")),
      applicationType = coalesce(df$applicationType, NA_character_),
      sponsorName = str_squish(coalesce(df$sponsorName, ""))
    )
  }

  conversions <- if (all(c("applicationType_old", "applicationType") %in%
                         names(diff$changed))) {
    diff$changed |> filter(applicationType_old == "C", applicationType == "N")
  } else diff$changed[0, ]

  # Conversions are reported on their own and removed from the generic
  # "changed" bucket, so a CNADA becoming a full NADA is not buried among
  # routine status edits -- it is the change vets most need to see.
  # A withdrawal is also a status change, and a conversion is also a type
  # change. Reporting either twice makes a small month look busier than it was
  # and buries the specific line that matters, so each application is listed
  # once under its most specific heading.
  changed_other <- diff$changed
  if (nrow(changed_other)) {
    already <- c(conversions$applicationNumber, diff$withdrawn$applicationNumber)
    changed_other <- changed_other |> filter(!applicationNumber %in% already)
  }

  # Bind onto a typed empty frame so the columns exist even in a month with no
  # changes at all. bind_rows() of nothing but NULLs returns a frame with zero
  # *columns*, not an empty typed one, and the filter below then fails on a
  # missing runDate -- the same trap that broke the first-ever run.
  empty <- tibble(
    runDate = as.Date(character()), kind = character(),
    applicationNumber = integer(), proprietaryName = character(),
    applicationType = character(), sponsorName = character())

  new_rows <- bind_rows(
    empty,
    row_of(conversions,     "Conditional approval became full approval"),
    row_of(diff$added,      "New application"),
    row_of(changed_other,   "Type or status changed"),
    row_of(diff$withdrawn,  "Voluntarily withdrawn")
  )

  prev <- if (file_exists(f)) readRDS(f) else empty
  # A re-run on the same day replaces that day's rows rather than duplicating.
  prev <- prev |> filter(runDate != stamp)

  out <- bind_rows(prev, new_rows) |>
    filter(runDate >= stamp - keep_months * 31) |>
    arrange(desc(runDate), kind, applicationNumber)

  dir_create(proc_dir())
  saveRDS(out, f, compress = "xz")

  # Every run is recorded, including quiet ones, so the app can say "checked
  # on this date, nothing changed" rather than showing a stale month.
  rf <- proc_dir("update_runs.rds")
  runs <- if (file_exists(rf)) readRDS(rf) else
    tibble(runDate = as.Date(character()), nAdded = integer(),
           nChanged = integer(), nWithdrawn = integer(), nConverted = integer())
  runs <- runs |>
    filter(runDate != stamp) |>
    bind_rows(tibble(runDate = stamp, nAdded = nrow(diff$added),
                     nChanged = nrow(changed_other),
                     nWithdrawn = nrow(diff$withdrawn),
                     nConverted = nrow(conversions))) |>
    arrange(desc(runDate)) |>
    head(keep_months)
  saveRDS(runs, rf, compress = "xz")

  message(sprintf("Update log -> %s (%d rows this run)", f, nrow(new_rows)))
  invisible(out)
}

#' Human-readable summary of one month's changes.
write_changelog <- function(diff) {
  dir_create(CHANGELOG)
  stamp <- format(Sys.Date(), "%Y-%m-%d")
  f <- path(CHANGELOG, glue("{stamp}.md"))

  fmt <- function(df, cols = c("applicationNumber", "proprietaryName",
                               "applicationType", "sponsorName")) {
    if (nrow(df) == 0) return("_none_\n")
    df |>
      select(any_of(cols)) |>
      mutate(across(everything(), ~ str_replace_all(coalesce(as.character(.x), ""),
                                                    "[\r\n|]", " "))) |>
      (\(d) paste0(
        "| ", paste(names(d), collapse = " | "), " |\n",
        "| ", paste(rep("---", ncol(d)), collapse = " | "), " |\n",
        paste(apply(d, 1, \(r) paste0("| ", paste(r, collapse = " | "), " |")),
              collapse = "\n"), "\n"))()
  }

  # Conditional approvals converting to full approval are called out
  # separately because that is the single change vets most want flagged.
  #
  # Guarded on the column being present: the changelog is a report, and a
  # shape it did not expect should not be able to abort a data refresh that
  # has already succeeded.
  conversions <- if (all(c("applicationType_old", "applicationType") %in%
                         names(diff$changed))) {
    diff$changed |> filter(applicationType_old == "C", applicationType == "N")
  } else {
    diff$changed[0, ]
  }

  txt <- glue(
    "# Green Book update -- {stamp}\n\n",
    "Source: Animal Drugs @ FDA, fetched {format(Sys.time(), '%Y-%m-%d %H:%M')}.\n\n",
    "- New applications: **{nrow(diff$added)}**\n",
    "- Type or status changed: **{nrow(diff$changed)}**\n",
    "- Newly withdrawn: **{nrow(diff$withdrawn)}**\n",
    "- Conditional -> full approval: **{nrow(conversions)}**\n\n",
    "## Conditional approvals that became full approvals\n\n{fmt(conversions)}\n",
    "## New applications\n\n{fmt(diff$added)}\n",
    "## Type or status changed\n\n{fmt(diff$changed)}\n",
    "## Newly withdrawn\n\n{fmt(diff$withdrawn)}\n"
  )

  write_file(txt, f)
  message("Changelog -> ", f)
  f
}

run_monthly_update <- function() {
  message("=== Green Book monthly update: ", format(Sys.time()), " ===")

  old_path <- raw_dir("catalogue.json")

  # Fetch the catalogue to a temporary location so a failure part-way through
  # cannot leave us with a half-written baseline to diff against next month.
  body <- toJSON(EMPTY_CRITERIA, auto_unbox = TRUE, null = "null")
  txt <- adafda_req("advancedSearchForExcelPdf") |>
    req_body_raw(body, type = "application/json") |>
    req_perform() |>
    resp_body_string()

  tmp <- file_temp(ext = "json")
  write(txt, tmp)

  diff <- compare_catalogues(old_path, tmp)
  message(glue("  new: {nrow(diff$added)}  changed: {nrow(diff$changed)}  ",
               "withdrawn: {nrow(diff$withdrawn)}"))

  write_changelog(diff)
  # verified = TRUE: `diff` came from the catalogue fetched above, not a fixture.
  write_update_log(diff, verified = TRUE)

  if (length(diff$refetch)) {
    message(glue("Re-fetching {length(diff$refetch)} detail records ..."))
    # Drop the cached copies so fetch_one_bean() actually re-downloads them.
    walk(diff$refetch, function(id) {
      f <- raw_dir("beans", paste0(id, ".json"))
      if (file_exists(f)) file_delete(f)
    })
    fetch_all_beans(diff$refetch)
  } else {
    message("No detail records need re-fetching.")
  }

  # Promote the new catalogue only once the detail crawl has succeeded.
  file_copy(tmp, old_path, overwrite = TRUE)
  fetch_reference()

  message("Rebuilding tidy tables ...")
  # 02_tidy_greenbook.R only auto-runs when executed as a script
  # (`sys.nframe() == 0`), which is false once it is sourced from inside this
  # function -- so build_all() has to be called explicitly. Sourcing into the
  # global environment keeps its helpers reachable when it calls them.
  source("R/02_tidy_greenbook.R")
  build_all()

  message("=== update complete ===")
  invisible(diff)
}

if (sys.nframe() == 0) run_monthly_update()
