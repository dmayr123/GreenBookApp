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

source("R/01_fetch_adafda.R")

CHANGELOG <- path("data", "changelog")

#' Compare the freshly fetched catalogue against the one already on disk.
compare_catalogues <- function(old_path, new_json) {
  new <- as_tibble(fromJSON(new_json)) |>
    select(applicationId, applicationNumber, applicationType,
           applicationStatusCode, proprietaryName, sponsorName,
           voluntaryWithdrawalDate)

  if (!file_exists(old_path)) {
    message("No previous catalogue; treating every application as new.")
    return(list(new = new, added = new, changed = new[0, ],
                withdrawn = new[0, ], refetch = new$applicationId))
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
  conversions <- diff$changed |>
    filter(applicationType_old == "C", applicationType == "N")

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
