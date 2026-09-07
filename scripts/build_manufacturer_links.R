# ---------------------------------------------------------------------------
# build_manufacturer_links.R -- resolve each product to its manufacturer's
# own product page, rather than to the manufacturer's home page.
#
# Manufacturers publish sitemaps listing every page they have, so the product
# pages can be discovered rather than guessed. The alternative -- constructing
# URLs from a naming convention -- breaks silently the moment a company
# reorganises its site, and a confident link to a 404 is worse than no link.
#
# Two rules keep this honest:
#
#   * Match on the trade name's first word against the URL's last path
#     segment, and only when that word is distinctive (4+ characters). A short
#     stem matches half a catalogue.
#   * Fetch every candidate page and require the trade name to appear on it.
#     A sitemap entry proves a URL was published, not that it still resolves
#     or still concerns that product.
#
# Output: data/reference/manufacturer_product_pages.csv -- a plain, editable
# CSV, so a wrong or missing row can be corrected by hand without re-running.
#
# Run:  Rscript scripts/build_manufacturer_links.R
# ---------------------------------------------------------------------------

library(httr2)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(readr)
library(xml2)
library(fs)

UA <- paste0("GreenBookApp/0.1 (R ", getRversion(),
             "; veterinary drug reference; +https://github.com/dmayr123/GreenBookApp)")

#' Where each manufacturer publishes its pages, and which URLs are products.
#'
#' `product_pattern` is matched against the URL path. Sites that keep products
#' under a distinct prefix are filtered on it; Merck publishes a dedicated
#' product sitemap, so everything in it is a product.
#' Only manufacturers that actually publish their product pages appear here.
#'
#' Elanco, Ceva and Boehringer Ingelheim are deliberately absent. Their
#' sitemaps list news and policy pages but not individual products, and
#' Elanco's product URLs are not guessable either -- every constructed form
#' returns 404. Their products keep the catalogue link, which is honest about
#' being a starting point, rather than a fabricated deep link.
MANUFACTURER_SITEMAPS <- tribble(
  ~manufacturer,                    ~sitemap,                                                      ~product_pattern,
  "Zoetis",                         "https://www.zoetisus.com/sitemap.xml",                        "/products/",
  "Virbac",                         "https://us.virbac.com/sitemap.xml",                           "/products/",
  "Dechra",                         "https://www.dechra-us.com/sitemap.xml",                       "/our-products/",
  "Merck Animal Health (Intervet)", "https://www.merck-animal-health-usa.com/product-sitemap.xml", "/"
)

get_xml <- function(url) {
  tryCatch(
    request(url) |> req_user_agent(UA) |> req_timeout(45) |>
      req_retry(max_tries = 2) |> req_perform() |> resp_body_string() |>
      read_xml(),
    error = function(e) NULL)
}

#' Every <loc> in a sitemap, following one level of sitemap index.
sitemap_urls <- function(url, depth = 0) {
  doc <- get_xml(url)
  if (is.null(doc)) return(character())
  locs <- xml2::xml_text(xml2::xml_find_all(doc, "//*[local-name()='loc']"))
  if (length(locs) == 0) return(character())

  # A sitemap index points at further sitemaps; follow once, and only the
  # children that look like product listings, to avoid pulling a whole site.
  if (grepl("sitemapindex", as.character(doc)[1]) && depth < 1) {
    kids <- locs[str_detect(locs, "product")]
    if (length(kids) == 0) kids <- locs
    return(unique(unlist(map(unique(kids), sitemap_urls, depth = depth + 1))))
  }
  unique(locs)
}

#' The last meaningful path segment of a URL.
url_slug <- function(u) {
  p <- str_remove(u, "^https?://[^/]+")
  p <- str_remove(p, "[?#].*$")
  parts <- str_split(str_remove(p, "/$"), "/")[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) return("")
  tolower(tail(parts, 1))
}

norm_key <- function(x) str_replace_all(tolower(coalesce(x, "")), "[^a-z0-9]+", "")

#' The distinctive first word of a trade name.
#'
#' Trademark glyphs and the conditional-approval suffix are dropped first, so
#' "PANOQUELL®-CA1" and "Rimadyl® Caplets" both reduce to their brand.
brand_stem <- function(name) {
  name |>
    str_remove_all("[®™©]") |>
    str_remove_all("(?i)[-\\s]*CA[-\\s]?[0-9]+\\s*$") |>
    str_squish() |>
    str_split("\\s+") |> map_chr(~ .x[1]) |>
    str_remove_all("[^A-Za-z0-9]") |>
    tolower()
}

#' How strongly does a candidate page check out?
#'
#'   "content"   the brand name appears in the returned HTML
#'   "listed"    the page loads and is not a soft 404, but the brand is not in
#'               the HTML -- normal for a client-rendered site. Zoetis returns
#'               200 for /products/petcare/rimadyl/ with no "Rimadyl" anywhere
#'               in the source, because the page assembles itself in the
#'               browser. Requiring the name in the HTML rejected valid product
#'               pages, so provenance carries the weight instead: the URL came
#'               from the manufacturer's own sitemap and its slug matches the
#'               brand.
#'   "dead"      unreachable, an error status, or a soft 404
#'
#' Soft 404s are checked explicitly because several of these sites return 200
#' for a missing page.
page_status <- function(url, needle) {
  resp <- tryCatch(
    request(url) |> req_user_agent(UA) |> req_timeout(30) |> req_perform(),
    error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) >= 400) return("dead")

  txt <- tryCatch(resp_body_string(resp), error = function(e) "")
  if (!is.character(txt) || length(txt) != 1 || is.na(txt)) return("listed")

  # Pages served with a declared charset their bytes do not honour make
  # str_detect() return NA rather than TRUE/FALSE, which aborted the run.
  # Forcing UTF-8 with substitution keeps a mis-encoded page checkable.
  txt <- iconv(txt, from = "UTF-8", to = "UTF-8", sub = " ")
  plain <- str_replace_all(txt, "<[^>]*>", " ")

  soft404 <- str_detect(plain, regex(
    "page not found|page can'?t be found|404 error|sorry, we can'?t find",
    ignore_case = TRUE))
  if (isTRUE(soft404)) return("dead")

  if (isTRUE(str_detect(norm_key(plain), fixed(needle)))) "content" else "listed"
}

build_links <- function(out = "data/reference/manufacturer_product_pages.csv") {
  products <- readRDS("data/processed/products.rds")
  apps     <- readRDS("data/processed/applications.rds")
  source("R/label_sources.R", local = TRUE)

  prod <- products |>
    left_join(apps |> select(applicationId, sponsorName), by = "applicationId") |>
    inner_join(match_manufacturer(apps) |> select(applicationId, manufacturer),
               by = "applicationId") |>
    mutate(stem = brand_stem(proprietaryName)) |>
    filter(nchar(stem) >= 4)

  results <- list()
  for (i in seq_len(nrow(MANUFACTURER_SITEMAPS))) {
    m <- MANUFACTURER_SITEMAPS[i, ]
    mine <- prod |> filter(manufacturer == m$manufacturer)
    if (nrow(mine) == 0) next

    message(sprintf("\n%s: reading sitemap ...", m$manufacturer))
    urls <- sitemap_urls(m$sitemap)
    urls <- urls[str_detect(urls, fixed(m$product_pattern))]
    message(sprintf("  %d candidate product pages", length(urls)))
    if (length(urls) == 0) next

    slugs <- tibble(url = urls, slug = map_chr(urls, url_slug)) |>
      filter(nzchar(slug)) |>
      mutate(slugKey = norm_key(slug))

    # A product matches a page when the page's slug is the brand stem, or
    # begins with it. Shortest slug wins, so "rimadyl" beats
    # "rimadyl-chewable-tablets-faq" -- the plainer URL is the product page.
    hits <- mine |>
      select(proprietaryNameId, proprietaryName, stem, manufacturer) |>
      inner_join(slugs, by = character(), relationship = "many-to-many") |>
      filter(slugKey == stem | str_starts(slugKey, stem)) |>
      group_by(proprietaryNameId) |>
      slice_min(nchar(slugKey), with_ties = FALSE) |>
      ungroup()

    message(sprintf("  %d of %d products matched a page",
                    nrow(hits), nrow(mine)))
    if (nrow(hits)) results[[m$manufacturer]] <- hits
  }

  matched <- bind_rows(results)
  if (nrow(matched) == 0) {
    message("No manufacturer product pages resolved.")
    return(invisible(NULL))
  }

  # Verify each distinct page once, not once per product.
  pages <- matched |> distinct(url, stem)
  message(sprintf("\nChecking %d distinct pages ...", nrow(pages)))
  pages$status <- map2_chr(pages$url, pages$stem, function(u, s) {
    r <- page_status(u, s); Sys.sleep(0.15); r
  })
  message(sprintf("  %d name confirmed on page, %d reachable, %d dead",
                  sum(pages$status == "content"), sum(pages$status == "listed"),
                  sum(pages$status == "dead")))

  final <- matched |>
    inner_join(pages |> filter(status != "dead") |> distinct(url, status),
               by = "url") |>
    transmute(proprietaryNameId, proprietaryName, manufacturer, url,
              verification = status) |>
    # One link per product. The cross join can pair a product with the same
    # page twice when a sitemap lists a URL more than once.
    distinct(proprietaryNameId, .keep_all = TRUE) |>
    arrange(manufacturer, proprietaryName)

  dir_create(path_dir(out))
  write_csv(final, out)
  message(sprintf("\nWrote %d product-page links for %d products -> %s",
                  nrow(final), n_distinct(final$proprietaryNameId), out))
  invisible(final)
}

if (sys.nframe() == 0) build_links()
