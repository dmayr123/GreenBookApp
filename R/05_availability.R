# ---------------------------------------------------------------------------
# 05_availability.R -- FDA animal drug shortages and discontinuations
#
# Reads the two tables FDA's Center for Veterinary Medicine publishes:
#
#   Current and Resolved Animal Drug Shortages
#   Discontinued Animal Drugs
#
# and matches each entry to Green Book products, so a drug page can say
# "FDA lists this as in shortage" at the top, with the reason, the date and the
# sponsor's phone number.
#
# Runs weekly (see .github/workflows/update-and-deploy.yml), separately from
# the monthly Green Book refresh: shortages start and end far more often than
# approvals change, and a month-old shortage list is not much use.
#
# What this is not: distributor backorder data. Covetrus, MWI, Patterson and
# the rest publish stock status only inside logged-in clinic accounts. FDA's
# list covers shortages sponsors report to FDA, which is narrower -- the app
# says so wherever it shows a clean result.
#
# Writes data/processed/availability.{rds,parquet}       one row per product x FDA entry
#        data/processed/availability_meta.{rds,parquet}  one row per FDA page checked
#
# Fails loudly. If FDA restructures a page so the table or its columns are not
# found, this stops rather than writing an empty list -- an empty list would
# render on every drug page as "not in shortage", which is the one wrong answer
# that looks exactly like a right one. The previous run's files are left in
# place, and their check date keeps showing how old they are.
# ---------------------------------------------------------------------------

library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(fs)
library(arrow)
library(httr2)
library(rvest)

source("R/search.R")   # norm_text(), so names normalize the way search does

proc_dir <- function(...) path("data", "processed", ...)

UA <- paste0(
  "Mozilla/5.0 (compatible; GreenBookApp/0.1; R ", getRversion(), "; ",
  "veterinary drug reference; +https://github.com/dmayr123/GreenBookApp)"
)

SOURCES <- list(
  shortage = list(
    url = paste0("https://www.fda.gov/animal-veterinary/product-safety-information/",
                 "current-and-resolved-animal-drug-shortages"),
    title = "FDA Current and Resolved Animal Drug Shortages",
    # FDA's own header text. Matched after normalization, so a change in
    # spacing or capitalization does not break it -- but a renamed or dropped
    # column does, on purpose.
    cols = c(drug = "Drug", product = "Prop Name", firm = "Firm/Customer Service #",
             state = "In Shortage/Resolved", began = "Date Shortage Began",
             resolved = "Date Shortage Resolved", reason = "Reason for Shortage",
             appNo = "(A)NADA")
  ),
  discontinued = list(
    url = paste0("https://www.fda.gov/animal-veterinary/product-safety-information/",
                 "discontinued-animal-drugs"),
    title = "FDA Discontinued Animal Drugs",
    cols = c(drug = "Drug", product = "Prop Name", appNo = "(A)NADA",
             firm = "Firm/Customer Service #", posted = "Posting Date",
             info = "Additional Information")
  )
)

write_table <- function(df, stem) {
  dir_create(proc_dir())
  write_parquet(df, proc_dir(paste0(stem, ".parquet")))
  saveRDS(df, proc_dir(paste0(stem, ".rds")), compress = "gzip")
}

fetch_page <- function(url) {
  request(url) |>
    req_user_agent(UA) |>
    req_timeout(60) |>
    req_retry(max_tries = 4, backoff = function(i) 5 * i) |>
    req_perform() |>
    resp_body_string() |>
    read_html()
}

#' FDA writes dates as 6/1/2025; blank cells come through as "" or NA.
fda_date <- function(x) {
  x <- str_trim(as.character(x))
  as.Date(ifelse(nzchar(coalesce(x, "")), x, NA), format = "%m/%d/%Y")
}

#' "Content current as of: 07/28/2026" -- when FDA last edited the page.
page_updated <- function(pg) {
  txt <- html_text2(pg)
  m <- str_match(txt, "Content current as of:\\s*(\\d{1,2}/\\d{1,2}/\\d{4})")[, 2]
  fda_date(m)
}

#' Pull the one data table off a page and rename FDA's columns to ours.
read_fda_table <- function(pg, src) {
  tables <- html_elements(pg, "table")
  if (length(tables) == 0) {
    stop("No table found on ", src$url, ". FDA may have restructured the page.")
  }

  want <- norm_text(src$cols)
  hit <- detect(tables, function(t) {
    have <- norm_text(html_text2(html_elements(t, "th")))
    all(want %in% have)
  })
  if (is.null(hit)) {
    stop("The table on ", src$url, " no longer has the expected columns (",
         paste(src$cols, collapse = ", "), "). Update SOURCES in R/05_availability.R.")
  }

  # html_table() flattens <br> to nothing, gluing the firm to its phone
  # number ("Cephazone Pharma, LLC800-587-4306"). Swap in a separator first.
  xml2::xml_add_sibling(html_elements(hit, "br"), "span", "; ", .where = "after")
  d <- html_table(hit, convert = FALSE)
  names(d) <- names(src$cols)[match(norm_text(names(d)), want)]
  d <- d[, !is.na(names(d)), drop = FALSE]

  d |>
    mutate(across(everything(), ~ str_squish(as.character(.x)))) |>
    filter(nzchar(drug) | nzchar(product))
}

#' "Zoetis Inc., 888-963-8471" -> firm + phone.
split_firm <- function(x) {
  phone <- str_extract(x, "\\(?\\d{3}\\)?[-. ]?\\d{3}-\\d{4}")
  firm  <- x |>
    str_remove(fixed(coalesce(phone, ""))) |>
    str_remove("[,;\\s]+$") |>
    str_squish()
  tibble(firm = firm, phone = phone)
}

#' "110-048" -> 110048. FDA writes "N/A" for products with no application,
#' such as an unapproved or human-labeled product.
app_number <- function(x) {
  d <- str_remove_all(coalesce(x, ""), "[^0-9]")
  suppressWarnings(as.integer(ifelse(nchar(d) >= 5, d, NA)))
}

#' Match FDA entries to Green Book products.
#'
#' By application number first. One application can carry several products --
#' 113-645 is both Estrumate and Heifex -- and FDA names the one in shortage,
#' so within an application the match narrows to products whose name
#' contains FDA's (or is contained in it). If none does, every product on the
#' application is flagged rather than none: a shortage shown one product too
#' broadly is safer than a shortage hidden.
#'
#' With no application number, fall back to the product name, but only where
#' the sponsor agrees, since generic names ("Epinephrine 1 mg/mL") repeat
#' across firms.
match_products <- function(entries, products, applications) {
  prods <- products |>
    select(proprietaryNameId, applicationId, proprietaryName) |>
    inner_join(applications |> select(applicationId, applicationNumber, sponsorName),
               by = "applicationId") |>
    mutate(nameKey = norm_text(proprietaryName),
           sponsorKey = str_sub(norm_text(sponsorName), 1, 6))

  map_dfr(seq_len(nrow(entries)), function(i) {
    e <- entries[i, ]
    fdaKey <- norm_text(e$fdaProduct)
    name_hit <- function(k) nzchar(k) & nzchar(fdaKey) &
      (str_detect(fdaKey, fixed(k)) | str_detect(k, fixed(fdaKey)))
    # map_lgl, because str_detect() vectorizes over the pattern too.
    name_hits <- function(keys) map_lgl(keys, name_hit)

    cand <- if (!is.na(e$applicationNumber)) {
      on_app <- prods |> filter(applicationNumber == e$applicationNumber)
      named  <- on_app |> filter(name_hits(nameKey))
      if (nrow(named)) named else on_app
    } else {
      firmKey <- str_sub(norm_text(e$firm), 1, 6)
      prods |> filter(name_hits(nameKey), sponsorKey == firmKey)
    }

    if (nrow(cand) == 0) return(NULL)
    tibble(proprietaryNameId = cand$proprietaryNameId,
           matchedBy = if (!is.na(e$applicationNumber)) "application number"
                       else "product name and sponsor",
           entryId = e$entryId)
  })
}

check_availability <- function() {
  message("=== FDA animal drug availability: ", format(Sys.time()), " ===")

  need <- proc_dir(c("products.rds", "applications.rds"))
  if (!all(file_exists(need))) {
    stop("Run R/02_tidy_greenbook.R first; this matches against its tables.")
  }
  products     <- readRDS(need[1])
  applications <- readRDS(need[2])

  pages <- imap(SOURCES, function(src, key) {
    message("  fetching ", src$title, " ...")
    pg <- fetch_page(src$url)
    list(table = read_fda_table(pg, src), updated = page_updated(pg))
  })

  sh <- pages$shortage$table
  shortages <- bind_cols(sh, split_firm(sh$firm) |> rename(firmName = firm)) |>
    transmute(
      source = "shortage",
      kind = case_when(
        # A resolution date is the stronger signal: FDA has been known to
        # leave "Currently in shortage" in the state column after resolving.
        !is.na(fda_date(resolved))             ~ "resolved",
        str_detect(str_to_lower(state), "resolv") ~ "resolved",
        TRUE                                   ~ "shortage"),
      fdaIngredient = drug, fdaProduct = product,
      firm = firmName, phone,
      reason = na_if(reason, ""),
      began = fda_date(began), resolved = fda_date(resolved),
      posted = as.Date(NA), info = NA_character_,
      applicationNumber = app_number(appNo))

  dc <- pages$discontinued$table
  discontinued <- bind_cols(dc, split_firm(dc$firm) |> rename(firmName = firm)) |>
    transmute(
      source = "discontinued", kind = "discontinued",
      fdaIngredient = drug, fdaProduct = product,
      firm = firmName, phone,
      reason = NA_character_, began = as.Date(NA), resolved = as.Date(NA),
      posted = fda_date(posted), info = na_if(info, ""),
      applicationNumber = app_number(appNo))

  entries <- bind_rows(shortages, discontinued) |>
    mutate(entryId = row_number(),
           sourceUrl = map_chr(source, ~ SOURCES[[.x]]$url))

  matches <- match_products(entries, products, applications)
  availability <- entries |>
    inner_join(matches, by = "entryId", relationship = "one-to-many") |>
    select(proprietaryNameId, kind, fdaIngredient, fdaProduct, firm, phone,
           reason, began, resolved, posted, info, applicationNumber,
           matchedBy, sourceUrl) |>
    distinct()

  unmatched <- entries |> filter(!entryId %in% matches$entryId)

  checked <- Sys.Date()
  meta <- imap_dfr(SOURCES, function(src, key) {
    listed <- entries |> filter(source == key)
    tibble(source = key, title = src$title, url = src$url,
           pageUpdated = pages[[key]]$updated,
           checkedDate = checked,
           nListed = nrow(listed),
           nMatched = sum(listed$entryId %in% matches$entryId),
           unmatched = paste(unmatched$fdaProduct[unmatched$source == key],
                             collapse = "; "))
  })

  write_table(availability, "availability")
  write_table(meta, "availability_meta")

  # Markdown, so the workflow can tee it straight into the run summary.
  cat("\n## FDA drug availability check,", format(checked, "%d %B %Y"), "\n\n")
  cat("| List | FDA page updated | Entries | Matched to Green Book |\n")
  cat("|---|---|---|---|\n")
  for (i in seq_len(nrow(meta))) {
    cat(sprintf("| %s | %s | %d | %d |\n", meta$title[i],
                format(meta$pageUpdated[i], "%d %b %Y"),
                meta$nListed[i], meta$nMatched[i]))
  }
  now_short <- entries |> filter(kind == "shortage")
  if (nrow(now_short)) {
    cat("\n**In shortage:** ",
        paste(sprintf("%s (since %s)", now_short$fdaProduct,
                      format(now_short$began, "%d %b %Y")), collapse = "; "),
        "\n", sep = "")
  }
  if (nrow(unmatched)) {
    cat("\nNot matched to a Green Book product (usually unapproved or ",
        "human-labeled products): ", paste(unmatched$fdaProduct, collapse = "; "),
        "\n", sep = "")
  }

  invisible(availability)
}

if (sys.nframe() == 0) check_availability()
