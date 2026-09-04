# ---------------------------------------------------------------------------
# build_static_site.R -- compile the app to a static site for GitHub Pages
#
# GitHub Pages serves files, not processes, so an ordinary Shiny app cannot
# run there. shinylive gets round that by shipping the R interpreter itself as
# WebAssembly: the app is then executed by the visitor's browser, and the
# "server" is only a static file host.
#
# That imposes two constraints this script exists to satisfy:
#
#  1. The app must be self-contained. At runtime there is a virtual filesystem
#     containing exactly what we bundle, so R/ and data/ are copied inside the
#     app directory rather than referenced from the project root.
#  2. Every byte is downloaded by the visitor. The processed parquet files are
#     bundled, so the build reports their size and refuses to ship the raw
#     API payloads.
#
# Output: docs/  (GitHub Pages "deploy from branch -> /docs" reads this)
#
# Run:  Rscript scripts/build_static_site.R
# ---------------------------------------------------------------------------

library(fs)
library(glue)

stopifnot(dir_exists("R"), dir_exists("app"), dir_exists("data/processed"))

BUILD <- "build/app"
SITE  <- "docs"

message("Assembling self-contained app in ", BUILD, " ...")
if (dir_exists("build")) dir_delete("build")
dir_create(path(BUILD, "R"))
dir_create(path(BUILD, "data", "processed"))
dir_create(path(BUILD, "data", "reference"))

# global.R resolves APP_ROOT by looking for R/ and data/ next to itself, so
# copying both inside the app directory makes "." the correct root with no
# code change.
file_copy(dir_ls("app", glob = "*.R"), BUILD, overwrite = TRUE)

# Only the modules the running app sources. The numbered pipeline scripts
# (01-04) fetch and reshape data; they are useless in a browser and, because
# shinylive decides what to bundle by scanning source for library() calls,
# including them would drag arrow, httr2 and jsonlite into every visitor's
# download for no benefit.
APP_MODULES <- c("search.R", "species_taxonomy.R", "drug_classes.R")
file_copy(path("R", APP_MODULES), path(BUILD, "R"), overwrite = TRUE)

# Only the processed tables ship. The raw payloads under data/raw are ~200 MB
# and are regenerable, so bundling them would punish every visitor.
#
# Only the RDS twins ship. The pipeline writes parquet alongside them as the
# archival copy, but the app reads RDS and bundling parquet would double the
# payload for files no visitor opens.
rds <- dir_ls("data/processed", glob = "*.rds")
if (length(rds) == 0) {
  stop("No .rds tables in data/processed. Run Rscript R/02_tidy_greenbook.R first.")
}
file_copy(rds, path(BUILD, "data", "processed"), overwrite = TRUE)
for (f in rds) {
  message(glue("    {path_file(f)}: ",
               "{prettyunits::pretty_bytes(file_info(f)$size)}"))
}

file_copy("data/reference/guidelines.csv",
          path(BUILD, "data", "reference"), overwrite = TRUE)

payload <- sum(file_info(dir_ls(BUILD, recurse = TRUE, type = "file"))$size)
message(glue("  bundled payload: {prettyunits::pretty_bytes(payload)}"))

message("Exporting to ", SITE, " (downloads WebAssembly packages on first run) ...")
# shinylive::export() adds to an existing destination rather than replacing
# it, so a package that is no longer a dependency would linger from an earlier
# build and still be shipped. Clearing first makes the output a true function
# of the current source.
if (dir_exists(SITE)) dir_delete(SITE)
shinylive::export(appdir = BUILD, destdir = SITE)

# GitHub Pages runs Jekyll by default, which ignores files and folders whose
# names begin with an underscore -- and shinylive emits a _shinylive/ tree
# holding the R runtime. Without this file the deployed site loads a page that
# can never start.
writeLines("", path(SITE, ".nojekyll"))

total <- sum(file_info(dir_ls(SITE, recurse = TRUE, type = "file"))$size)
message(glue("\nStatic site written to {SITE}/ ({prettyunits::pretty_bytes(total)} on disk)"))
message("Preview locally:  Rscript -e 'httpuv::runStaticServer(\"docs\", port = 8080)'")
