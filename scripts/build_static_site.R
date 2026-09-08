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

# shinylive also ships an /edit/ entry point: a live code editor showing the
# app's source, which a visitor can modify and re-run in their own browser.
# Nothing they do there can alter the deployed site -- the change lives only in
# their browser -- and the source is public anyway. It is removed regardless,
# because this is a clinical reference: an editable view invites someone to
# alter a dose or an indication and screenshot the result as though it came
# from the published tool. Visitors get the read-only app.
edit_dir <- path(SITE, "edit")
if (dir_exists(edit_dir)) {
  dir_delete(edit_dir)
  message("Removed the shinylive /edit/ editor from the published site.")
}

# -- customise the page shell ------------------------------------------------
#
# shinylive emits a bare shell titled "Shiny App" with no explanation of the
# wait. Both matter for a link sent to colleagues: the browser tab is the
# app's name to anyone who bookmarks it, and the first visit spends up to a
# minute downloading the R runtime before anything appears. Without a message
# that looks like a broken page.
#
# The notice hides itself once the app's own heading is on screen, and has a
# hard timeout and a dismiss link so it can never trap a visitor behind it.
shell <- readLines(path(SITE, "index.html"), warn = FALSE)

shell <- sub("<title>Shiny App</title>",
             "<title>Green Book Drug Finder</title>", shell, fixed = TRUE)

# A bookmark takes its name from <title> and its icon from the favicon.
# shinylive ships neither, so a saved link would sit in the bookmarks bar as a
# blank page icon -- the thing that makes a bookmark hard to find again. The
# capsule mark is drawn as SVG rather than shipped as a bitmap so it stays
# sharp at every size, with a manifest so an "Add to Home Screen" on a phone
# gets a sensible short name instead of the URL.
writeLines(c(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64">',
  '  <rect width="64" height="64" rx="14" fill="#0b6b5e"/>',
  '  <g transform="rotate(-45 32 32)">',
  '    <rect x="17" y="24" width="30" height="16" rx="8" fill="#ffffff"/>',
  '    <path d="M32 24h7a8 8 0 0 1 8 8 8 8 0 0 1-8 8h-7z" fill="#7fd4bd"/>',
  '  </g>',
  '</svg>'
), path(SITE, "favicon.svg"))

writeLines(c(
  '{',
  '  "name": "Green Book Drug Finder",',
  '  "short_name": "Green Book",',
  '  "description": "FDA-approved animal drug products, searchable by species.",',
  '  "start_url": "./",',
  '  "display": "standalone",',
  '  "background_color": "#f6f8f9",',
  '  "theme_color": "#0b6b5e",',
  '  "icons": [{ "src": "favicon.svg", "sizes": "any", "type": "image/svg+xml" }]',
  '}'
), path(SITE, "manifest.webmanifest"))

shell <- sub(
  "</head>",
  paste0(
    '  <link rel="icon" href="./favicon.svg" type="image/svg+xml" />\n',
    '  <link rel="apple-touch-icon" href="./favicon.svg" />\n',
    '  <link rel="manifest" href="./manifest.webmanifest" />\n',
    '  <meta name="theme-color" content="#0b6b5e" />\n',
    '  <meta name="apple-mobile-web-app-title" content="Green Book" />\n',
    '  <meta name="description" content="FDA-approved animal drug products, ',
    'searchable by species. Not a substitute for the approved label or for ',
    'Animal Drugs @ FDA." />\n',
    '</head>'),
  shell, fixed = TRUE)

LOADING_HTML <- '
<style>
  #gb-loading {
    position: fixed; inset: 0; z-index: 9999;
    display: flex; align-items: center; justify-content: center;
    background: #f6f8f9; color: #1c2b33;
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
    padding: 1.5rem; text-align: center;
  }
  #gb-loading .box { max-width: 30rem; }
  #gb-loading h1 { font-size: 1.4rem; margin: 0 0 .6rem; letter-spacing: -.01em; }
  #gb-loading p { margin: 0 0 .7rem; line-height: 1.55; color: #40525b; font-size: .95rem; }
  #gb-loading .small { font-size: .82rem; color: #7b8b94; }
  #gb-loading .bar {
    height: 3px; background: #dfe6e9; border-radius: 999px;
    overflow: hidden; margin: 1.1rem 0 .9rem;
  }
  #gb-loading .bar span {
    display: block; height: 100%; width: 35%; background: #0b6b5e;
    border-radius: 999px; animation: gbslide 1.6s ease-in-out infinite;
  }
  @keyframes gbslide {
    0% { transform: translateX(-100%); } 100% { transform: translateX(340%); }
  }
  @media (prefers-reduced-motion: reduce) {
    #gb-loading .bar span { animation: none; width: 100%; }
  }
  #gb-loading a { color: #0b6b5e; }
</style>
<div id="gb-loading">
  <div class="box">
    <h1>Green Book Drug Finder</h1>
    <div class="bar"><span></span></div>
    <p><strong>First visit takes a little while to load.</strong> This app runs
       R inside your browser, so it downloads the software it needs before it
       can start &mdash; usually 30 to 60 seconds, longer on a slow
       connection.</p>
    <p class="small">Your browser keeps it after that, so every later visit
       opens straight away. Please leave this tab open.</p>
    <p class="small">Still here after a minute or two?
       <a href="#" id="gb-dismiss">Hide this and show the app</a> &mdash;
       it may already have loaded behind this notice.</p>
  </div>
</div>
<script>
  (function () {
    var el = document.getElementById("gb-loading");
    if (!el) return;
    var done = false;
    function hide() {
      if (done) return;
      done = true;
      el.style.transition = "opacity .35s ease";
      el.style.opacity = "0";
      setTimeout(function () { el.remove(); }, 400);
    }
    document.getElementById("gb-dismiss")
      .addEventListener("click", function (e) { e.preventDefault(); hide(); });

    // Deciding when the app is up is harder than it looks, and getting it
    // wrong is worse than showing no notice at all: shinylive mounts the app
    // inside an iframe, so scanning only the outer document never finds it,
    // and the notice then covers a perfectly working app until its timeout
    // expires. That is indistinguishable from the app hanging.
    // (Apostrophes are avoided in this block: it is an R single-quoted
    // string, and one would end it.)
    //
    // Three independent signals, any of which means "stop covering the page":
    //   1. the landing-page heading is in this document
    //   2. it is in a readable iframe
    //   3. an iframe has rendered a meaningful amount of anything at all,
    //      which covers the case where the heading text changes
    function appVisible() {
      try {
        if (document.body.innerText.indexOf("Browse by species") !== -1) return true;
      } catch (e) {}
      var frames = document.querySelectorAll("iframe");
      for (var i = 0; i < frames.length; i++) {
        try {
          var d = frames[i].contentDocument;
          if (!d || !d.body) continue;
          var t = d.body.innerText || "";
          if (t.indexOf("Browse by species") !== -1) return true;
          if (t.replace(/\\s/g, "").length > 200) return true;
        } catch (e) { /* cross-origin frame: cannot read, try the next */ }
      }
      return false;
    }

    var poll = setInterval(function () {
      if (appVisible()) { clearInterval(poll); hide(); }
    }, 400);

    // Short stop. If the app really is still loading the page underneath is
    // blank and harmless, whereas a notice that outlives the app it describes
    // makes a working tool look broken.
    setTimeout(function () { clearInterval(poll); hide(); }, 45000);
  })();
</script>
'

shell <- sub("<body>", paste0("<body>\n", LOADING_HTML), shell, fixed = TRUE)
writeLines(shell, path(SITE, "index.html"))
message("Set the page title and added the first-load notice.")

total <- sum(file_info(dir_ls(SITE, recurse = TRUE, type = "file"))$size)
message(glue("\nStatic site written to {SITE}/ ({prettyunits::pretty_bytes(total)} on disk)"))
message("Preview locally:  Rscript -e 'httpuv::runStaticServer(\"docs\", port = 8080)'")
