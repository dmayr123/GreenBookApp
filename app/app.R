# ---------------------------------------------------------------------------
# app.R -- Green Book drug finder
#
# Three views, driven by a single `view` reactive value:
#   home     species tiles, or a quick search that skips straight to results
#   results  the filtered, ranked product list
#   detail   one product: identity, labeled use by species, documents, links
#
# The species choice is deliberately sticky across views. A vet who starts by
# picking "Cattle" is asking every subsequent question in a cattle context, so
# the dosing shown on the detail page is filtered to cattle unless they clear
# it. That is the main thing this app does that the FDA site does not.
# ---------------------------------------------------------------------------

source("global.R", local = FALSE)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

app_css <- "
:root { --gb-ink:#1c2b33; --gb-accent:#0b6b5e; --gb-line:#dfe6e9; }
body {
  background:#f6f8f9;
  /* Single quotes throughout this stylesheet: it is a double-quoted R string,
     and a double quote here ends it. That mistake once shipped a file R could
     not parse, which reached the browser as a blank page. */
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto,
               'Helvetica Neue', Arial, sans-serif;
}
.gb-hero { padding: 2rem 0 1rem; }
.gb-hero h1 { font-weight: 700; letter-spacing:-.02em; color:var(--gb-ink); }
.gb-sub { color:#5a6b74; max-width: 46rem; }

.species-grid {
  display:grid; grid-template-columns:repeat(auto-fill,minmax(140px,1fr));
  gap:.75rem; margin-top:1rem;
}
.species-tile {
  border:1px solid var(--gb-line); border-radius:14px; background:#fff;
  padding:1rem .5rem; text-align:center; cursor:pointer; width:100%;
  transition:transform .08s ease, box-shadow .08s ease, border-color .08s;
}
.species-tile:hover { transform:translateY(-2px);
  box-shadow:0 6px 18px rgba(12,40,50,.10); border-color:var(--gb-accent); }
.species-tile.empty { opacity:.45; }
.species-tile .icon { font-size:2rem; line-height:1; display:block; }
.species-tile .label { font-weight:600; color:var(--gb-ink); margin-top:.4rem;
  display:block; font-size:.95rem; }
.species-tile .count { color:#7b8b94; font-size:.78rem; }
.species-tile .major { font-size:.66rem; text-transform:uppercase;
  letter-spacing:.06em; color:var(--gb-accent); }

.badge-cat { font-size:.72rem; font-weight:700; padding:.22rem .55rem;
  border-radius:999px; white-space:nowrap; }
.cat-approved   { background:#e4f1ec; color:#0b6b5e; }
.cat-generic    { background:#e7eef7; color:#23558c; }
.cat-conditional{ background:#fdf0dc; color:#8a5310; }
.cat-eua        { background:#f6e6f0; color:#7c2a5c; }
.cat-other      { background:#eceff1; color:#546069; }
.badge-withdrawn{ background:#fbe6e6; color:#8f2626; font-size:.72rem;
  font-weight:700; padding:.22rem .55rem; border-radius:999px; }

/* Set directly rather than through a Bootstrap theme variable, so bslib does
   not have to recompile Sass on every visitor's first load. */
.btn-primary { background-color:var(--gb-accent); border-color:var(--gb-accent); }
.btn-primary:hover, .btn-primary:focus {
  background-color:#095448; border-color:#095448;
}
.form-check-input:checked { background-color:var(--gb-accent);
  border-color:var(--gb-accent); }
a { color:var(--gb-accent); }

/* Bootstrap and reactable each set their own font stack; without this the
   result table and the drug page keep the browser default while the rest of
   the page uses Inter. The product names in the results list were the most
   visible case. */
body, .card, .rt-table, .rt-th, .rt-td, .btn, .form-control, .form-select,
h1, h2, h3, h4, h5, h6, label, .modal-content {
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto,
               'Helvetica Neue', Arial, sans-serif;
}

.muted { color:#8a969d; font-style:italic; }
.update-line { display:flex; align-items:baseline; gap:.5rem; flex-wrap:wrap;
  margin-top:1rem; font-size:.9rem; color:#5a6b74; }
.update-line .update-dot { width:.5rem; height:.5rem; border-radius:50%;
  background:#c3ccd1; flex:0 0 auto; align-self:center; }
.update-line.has-changes { color:var(--gb-ink); }
.update-line.has-changes .update-dot { background:var(--gb-accent); }
.update-line strong { color:var(--gb-ink); }
.update-tbl { width:100%; border-collapse:collapse; font-size:.88rem; }
.update-tbl th { text-align:left; font-size:.7rem; text-transform:uppercase;
  letter-spacing:.08em; color:#7b8b94; padding:0 .8rem .4rem 0;
  border-bottom:1.5px solid var(--gb-line); }
.update-tbl td { padding:.45rem .8rem .45rem 0;
  border-bottom:1px solid var(--gb-line); vertical-align:top; }
.update-tbl td:first-child { font-weight:600; }
.kind-tag { display:inline-block; font-size:.68rem; font-weight:700;
  text-transform:uppercase; letter-spacing:.05em; padding:.14rem .45rem;
  border-radius:999px; white-space:nowrap; }
.kind-conv { background:#fdf0dc; color:#8a5310; }
.kind-new  { background:#e4f1ec; color:#0b6b5e; }
.kind-chg  { background:#e7eef7; color:#23558c; }
.kind-wd   { background:#fbe6e6; color:#8f2626; }
/* The product name in a result row: a real button, styled as the heading it
   reads as. Focus is visible because a keyboard user needs to see where they
   are, which is the whole point of making it focusable. */
.row-open {
  background:none; border:0; padding:0; margin:0; font:inherit;
  font-weight:700; color:var(--gb-ink); text-align:left; cursor:pointer;
}
.row-open:hover { color:var(--gb-accent); text-decoration:underline; }
.row-open:focus-visible {
  outline:2px solid var(--gb-accent); outline-offset:2px; border-radius:2px;
}
.field-label { font-size:.72rem; text-transform:uppercase; letter-spacing:.07em;
  color:#7b8b94; font-weight:700; margin-bottom:.15rem; }
.field-value { margin-bottom:.9rem; color:var(--gb-ink); }
.dose-card { border:1px solid var(--gb-line); border-left:4px solid var(--gb-accent);
  border-radius:10px; background:#fff; padding:.9rem 1rem; margin-bottom:.75rem; }
.dose-pop { font-weight:700; color:var(--gb-ink); margin-bottom:.35rem; }
.extralabel { border-left-color:#b0762a; }
.doc-link { display:block; padding:.45rem 0; border-bottom:1px solid var(--gb-line); }
.label-primary { border:1px solid var(--gb-line); border-left:4px solid var(--gb-accent);
  border-radius:10px; background:#fff; padding:.9rem 1rem; }
.label-primary a { font-weight:700; font-size:1.02rem; }
.src-badge { display:inline-block; font-size:.68rem; font-weight:700;
  text-transform:uppercase; letter-spacing:.06em; padding:.16rem .5rem;
  border-radius:999px; background:#e4f1ec; color:#0b6b5e; margin-left:.4rem;
  vertical-align:2px; }
.src-badge.tier2 { background:#e7eef7; color:#23558c; }
.src-badge.tier3 { background:#e7eef7; color:#23558c; }
.src-badge.tier4 { background:#f2eef7; color:#5a3d8a; }
.src-badge.tier5 { background:#eceff1; color:#546069; }
.what-is { color:#5a6b74; font-size:.83rem; margin-top:.2rem; }
.cite { color:#7b8b94; font-size:.78rem; }
.gb-footer { color:#7b8b94; font-size:.82rem; padding:2rem 0 1rem; }
.disclaimer { background:#fff8e6; border:1px solid #f0dfae; border-radius:10px;
  padding:.75rem 1rem; font-size:.85rem; color:#6b551f; }
"

ui <- page_fluid(
  # No web font. font_google() *downloads* the font files when the theme is
  # built, which needs curl -- and the WebAssembly build has no curl, so the
  # app died on startup with "Downloading Google Font files requires either
  # the curl package or capabilities('libcurl')". A system font stack costs
  # nothing to fetch, cannot fail, and renders the same on every platform a
  # vet is likely to use.
  # Default Bootstrap 5, not a customized theme. Passing `primary` makes bslib
  # recompile Bootstrap's Sass at startup: 2.0 s here against 0.63 s for the
  # default, and R runs several times slower again in WebAssembly, so it is
  # paid out of the visitor's first load. The one thing `primary` bought was
  # the green Search button, which the stylesheet below sets directly.
  theme = bs_theme(version = 5),
  tags$head(
    # Inter, loaded the way a web page loads a font: a stylesheet link the
    # browser fetches. This is not font_google(), which downloads the font
    # files inside R when the theme is built and needs curl -- that is what
    # crashed the WebAssembly build. Nothing is fetched by R here, and if
    # Google Fonts is unreachable the stack below falls back to the system
    # font rather than failing.
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com",
              crossorigin = NA),
    tags$link(rel = "stylesheet",
              href = paste0("https://fonts.googleapis.com/css2",
                            "?family=Inter:wght@400;500;600;700&display=swap")),
    tags$style(HTML(app_css)),
    tags$title("Green Book Drug Finder"),
    # Enter must run the search. A Shiny textInput is not inside a form, so it
    # does nothing on Enter unless wired up -- and typing a drug name then
    # pressing Enter is what everyone does first.
    #
    # Bound on `document` rather than on the input, because the pages are
    # rendered by renderUI and the box does not exist when this script runs.
    tags$script(HTML("
      document.addEventListener('keydown', function (e) {
        if (e.key !== 'Enter' || e.isComposing) return;
        var t = e.target;
        if (!t || !t.id) return;
        if (t.id === 'home_query') {
          e.preventDefault();
          // Flush the typed value to the server before the button event, so a
          // fast typist who hits Enter on the last character still searches
          // for what they typed rather than for one character less.
          if (window.jQuery) jQuery(t).trigger('change');
          var btn = document.getElementById('home_go');
          if (btn) btn.click();
        } else if (t.id === 'query') {
          // Results already filter as you type; stop Enter doing anything odd
          // and drop focus so the keyboard closes on a phone.
          e.preventDefault();
          t.blur();
        }
      });
    "))
  ),
  uiOutput("page")
)

# -- home --------------------------------------------------------------------

#' The "what changed" line under the title.
#'
#' Shows the headline counts from the most recent refresh and opens the full
#' report. A quiet month still says so with its date, because "checked on the
#' 6th, nothing changed" is information -- silence would be indistinguishable
#' from the update having stopped running.
update_banner <- function() {
  run <- latest_run()
  if (is.null(run)) return(NULL)

  total <- run$nAdded + run$nChanged + run$nWithdrawn + run$nConverted
  when <- format(run$runDate, "%d %B %Y")

  bits <- c(
    if (run$nConverted > 0) sprintf("%d conditional → full approval", run$nConverted),
    if (run$nAdded > 0)     sprintf("%d new", run$nAdded),
    if (run$nChanged > 0)   sprintf("%d status change%s", run$nChanged,
                                    if (run$nChanged == 1) "" else "s"),
    if (run$nWithdrawn > 0) sprintf("%d withdrawn", run$nWithdrawn)
  )

  div(class = if (total > 0) "update-line has-changes" else "update-line",
    span(class = "update-dot"),
    if (total > 0) {
      tagList(strong(sprintf("Monthly update, %s: ", when)),
              paste(bits, collapse = " · "), " ",
              actionLink("show_update", "See the report"))
    } else {
      tagList(sprintf("Checked %s — no drug changes this month. ", when),
              actionLink("show_update", "See update history"))
    }
  )
}

home_ui <- function() {
  tagList(
    div(class = "gb-hero",
      h1("Green Book Drug Finder"),
      # Deliberately makes no completeness claim. An earlier version opened
      # "Every FDA-approved ... drug", which asserts the extract is exhaustive
      # -- something no periodic copy of an external database can guarantee,
      # and not a claim worth defending on a clinical tool.
      p(class = "gb-sub",
        "Search for drugs by species, or search directly."),
      # Sits under the title and above the search box, so the month's changes
      # are the first thing offered rather than something to go looking for.
      update_banner()
    ),
    card(
      card_body(
        h5("Quick drug search"),
        div(class = "d-flex gap-2",
          div(style = "flex:1",
              textInput("home_query", NULL, width = "100%",
                        placeholder = "Trade name, active ingredient, sponsor or application number")),
          div(actionButton("home_go", "Search", class = "btn-primary"))
        ),
        div(class = "text-muted small",
            "Punctuation and spacing are ignored, so ", tags$code("CA-1"), ", ",
            tags$code("-CA-1"), " and ", tags$code("CA1"), " all find the same products.")
      )
    ),
    div(class = "mt-4",
      h5("Browse by species"),
      p(class = "text-muted small mb-0",
        "Choosing a species filters the labeled dose and indication shown on each drug."),
      div(class = "species-grid",
        pmap(SPECIES_TILES, function(group, label, icon, major, n) {
          actionButton(
            inputId = paste0("sp_", group),
            label = tagList(
              span(class = "icon", icon),
              span(class = "label", label),
              span(class = "count", if (n > 0) paste(n, "products") else "no products"),
              if (isTRUE(major)) span(class = "major", "major species") else NULL
            ),
            class = paste("species-tile", if (n == 0) "empty" else "")
          )
        })
      )
    ),
    # Stated on the landing page as well as on each drug page, because a
    # visitor who searches once may never scroll far enough to see it there.
    div(class = "disclaimer mt-4",
      strong("Not a substitute for the approved product label or for "),
      strong("Animal Drugs @ FDA. "),
      "This tool reformats a periodic extract of FDA's published data and may ",
      "lag the current FDA record. It shows FDA-labeled use only. Verify ",
      "against the current approved label and against ",
      tags$a(href = "https://animaldrugsatfda.fda.gov/adafda/views/#/search",
             target = "_blank", rel = "noopener", "Animal Drugs @ FDA"),
      " before prescribing."),
    div(class = "gb-footer",
      sprintf("Data from Animal Drugs @ FDA, built %s. ", DATA_BUILT),
      sprintf("%s applications, %s products.",
              format(nrow(APPLICATIONS), big.mark = ","),
              format(nrow(PRODUCTS), big.mark = ","))
    )
  )
}

# -- results -----------------------------------------------------------------

#' The results page.
#'
#' Every control is seeded from the caller's saved state rather than from a
#' fixed default. renderUI rebuilds this whole page each time the view
#' changes, so a control that defaults to empty comes back empty: opening a
#' drug and pressing "Back to results" discarded the search, the product-type
#' filters and both toggles, and dumped the user into the full catalog. The
#' state lives in the server and is passed back in here.
results_ui <- function(species_label, state) {
  tagList(
    div(class = "d-flex align-items-center gap-2 mt-3 mb-2",
      actionLink("back_home", "← Home"),
      if (!is.null(species_label))
        span(class = "badge-cat cat-approved", species_label) else NULL
    ),
    card(card_body(
      layout_columns(
        col_widths = c(5, 3, 4),
        textInput("query", "Search", width = "100%", value = state$query,
                  placeholder = "Trade name, ingredient, sponsor, application no."),
        selectInput("species_sel", "Species", width = "100%",
                    selected = state$species,
                    choices = c("Any species" = "any",
                                setNames(SPECIES_GROUPS$group, SPECIES_GROUPS$label))),
        checkboxGroupInput("cats", "Product type", inline = TRUE,
                           choices = CATEGORIES, selected = state$cats)
      ),
      div(class = "d-flex gap-3",
        checkboxInput("deep", "Also search indications and strengths", state$deep),
        checkboxInput("withdrawn", "Include voluntarily withdrawn", state$withdrawn)
      )
    )),
    div(class = "mt-2 mb-2", textOutput("result_count")),
    card(card_body(reactableOutput("results")))
  )
}

# -- detail ------------------------------------------------------------------

field <- function(label, value) {
  div(div(class = "field-label", label), div(class = "field-value", value))
}

detail_ui <- function() {
  tagList(
    div(class = "d-flex align-items-center gap-3 mt-3 mb-2",
        actionLink("back_results", "← Back to results"),
        actionLink("back_home2", "Home")),
    uiOutput("detail")
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  view      <- reactiveVal("home")
  species   <- reactiveVal("any")   # sticky across views
  selected  <- reactiveVal(NULL)    # proprietaryNameId

  # Search state, held outside the UI so it survives renderUI rebuilding the
  # page. Kept in step with the controls by the observers below; the controls
  # are seeded from it whenever the results page is drawn.
  state <- reactiveValues(query = "", species = "any", cats = CATEGORIES,
                          deep = FALSE, withdrawn = FALSE)

  output$page <- renderUI({
    switch(view(),
      home    = home_ui(),
      results = results_ui(species_label(), reactiveValuesToList(state)),
      detail  = detail_ui()
    )
  })

  # ignoreNULL matters: while the results page is being torn down its inputs
  # read NULL for a moment, and without this the saved state would be wiped by
  # the very navigation it exists to survive.
  observeEvent(input$query,     state$query     <- input$query,
               ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$cats,      state$cats      <- input$cats,
               ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$deep,      state$deep      <- input$deep,
               ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$withdrawn, state$withdrawn <- input$withdrawn,
               ignoreInit = TRUE, ignoreNULL = TRUE)

  species_label <- reactive({
    g <- species()
    if (is.null(g) || g == "any") return(NULL)
    SPECIES_GROUPS$label[SPECIES_GROUPS$group == g]
  })

  # -- navigation ------------------------------------------------------------

  # One observer per tile. The tiles are fixed at startup, so registering them
  # once here is simpler than a JS bridge and keeps the ids greppable.
  walk(SPECIES_GROUPS$group, function(g) {
    observeEvent(input[[paste0("sp_", g)]], {
      species(g)
      view("results")
    }, ignoreInit = TRUE)
  })

  observeEvent(input$home_go, {
    # Set the saved state and let the results page seed itself from it. The
    # previous version deferred an updateTextInput() until after the UI
    # existed, which worked but only for this one control and only on this one
    # path.
    state$query <- input$home_query %||% ""
    state$cats <- CATEGORIES
    species("any")
    view("results")
  })

  # -- monthly update report -------------------------------------------------

  kind_tag <- function(kind) {
    cls <- case_when(
      str_detect(kind, "full approval")  ~ "kind-conv",
      str_detect(kind, "New")            ~ "kind-new",
      str_detect(kind, "withdrawn")      ~ "kind-wd",
      TRUE                               ~ "kind-chg")
    sprintf('<span class="kind-tag %s">%s</span>', cls, kind)
  }

  observeEvent(input$show_update, {
    run <- latest_run()
    latest <- if (is.null(run)) UPDATE_LOG[0, ] else
      UPDATE_LOG |> filter(runDate == run$runDate)

    body <- if (nrow(latest) == 0) {
      tagList(
        p(if (is.null(run)) "No refresh has run yet."
          else sprintf("Checked %s. FDA published no changes to drug approvals, statuses or withdrawals since the previous check.",
                       format(run$runDate, "%d %B %Y"))),
        p(class = "text-muted small",
          "This report covers drug changes only.")
      )
    } else {
      tagList(
        p(class = "text-muted small",
          sprintf("Changes FDA published between the previous check and %s.",
                  format(run$runDate, "%d %B %Y"))),
        div(style = "overflow-x:auto",
          tags$table(class = "update-tbl",
            tags$thead(tags$tr(
              tags$th("Product"), tags$th("Change"),
              tags$th("Application"), tags$th("Sponsor"))),
            tags$tbody(map(seq_len(nrow(latest)), function(i) {
              tags$tr(
                tags$td(latest$proprietaryName[i]),
                tags$td(HTML(kind_tag(latest$kind[i]))),
                tags$td(latest$applicationNumber[i]),
                tags$td(latest$sponsorName[i]))
            }))
          ))
      )
    }

    # Earlier months, so a missed report is still reachable.
    history <- if (nrow(UPDATE_RUNS) > 1) {
      tagList(
        hr(),
        div(class = "field-label", "Previous checks"),
        div(class = "text-muted small",
          map(seq_len(min(nrow(UPDATE_RUNS), 12))[-1], function(i) {
            r <- UPDATE_RUNS[i, ]
            n <- r$nAdded + r$nChanged + r$nWithdrawn + r$nConverted
            div(sprintf("%s — %s", format(r$runDate, "%d %b %Y"),
                        if (n == 0) "no changes" else sprintf("%d change%s", n,
                          if (n == 1) "" else "s")))
          }))
      )
    } else NULL

    showModal(modalDialog(
      title = "Monthly drug update",
      size = "l", easyClose = TRUE,
      footer = modalButton("Close"),
      body, history
    ))
  })

  observeEvent(input$back_home,    { view("home") })
  observeEvent(input$back_home2,   { view("home") })
  observeEvent(input$back_results, { view("results") })

  observeEvent(input$species_sel, { species(input$species_sel) },
               ignoreInit = TRUE, ignoreNULL = TRUE)

  # The dropdown is seeded from `state$species` when the page is drawn, so it
  # already shows whatever tile was pressed. This just keeps the two in step.
  observeEvent(species(), { state$species <- species() })

  # -- results ---------------------------------------------------------------

  # The results list filters as you type. Without a pause, every keystroke ran
  # a full search and re-rendered the table -- 300 ms of work to type one word
  # here, and several times that in the browser build, where R runs
  # interpreted WebAssembly. 250 ms is below the point a pause is noticeable
  # but long enough that a typed word costs one search instead of nine.
  # Read from the saved state, not from the inputs. During navigation the
  # inputs are momentarily NULL, and reading them directly briefly searched
  # for nothing with no categories selected -- which is what flashed the whole
  # catalog on the way back from a drug page.
  query_d <- debounce(reactive(state$query %||% ""), 250)

  hits <- reactive({
    search_drugs(
      SEARCH_INDEX,
      query             = query_d(),
      deep              = isTRUE(state$deep),
      species_group     = species(),
      categories        = state$cats,
      include_withdrawn = isTRUE(state$withdrawn)
    )
  })

  output$result_count <- renderText({
    n <- nrow(hits())
    # The debounced value, so the count and the table always describe the
    # same query rather than the caption running a keystroke ahead.
    q <- str_trim(query_d())
    sprintf("%s product%s%s", format(n, big.mark = ","),
            if (n == 1) "" else "s",
            if (nzchar(q)) paste0(" matching \"", q, "\"") else "")
  })

  output$results <- renderReactable({
    d <- hits()
    if (nrow(d) == 0) {
      return(reactable(data.frame(Result = "No products match those filters.")))
    }

    tbl <- d |>
      transmute(
        proprietaryNameId,
        Product     = proprietaryName,
        Ingredients = coalesce(ingredients, ""),
        Type        = category,
        Status      = marketStatus,
        Labeler     = coalesce(sponsorName, ""),
        Form        = coalesce(doseFormName, ""),
        Species     = coalesce(speciesList, ""),
        Application = applicationNumber
      )

    reactable(
      tbl,
      searchable = FALSE, highlight = TRUE, compact = TRUE,
      defaultPageSize = 25, showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, 100),
      onClick = JS("function(rowInfo){
        Shiny.setInputValue('row_clicked',
          rowInfo.row.proprietaryNameId, {priority:'event'});
      }"),
      rowStyle = list(cursor = "pointer"),
      columns = list(
        proprietaryNameId = colDef(show = FALSE),
        # The product name is a real <button>, not just styled text. Rows are
        # opened by a click handler on the row, which a mouse user never
        # notices but which leaves a keyboard or screen-reader user able to
        # search and unable to open anything. A button is focusable, announced
        # as a control, and activates on Enter and Space for free.
        Product = colDef(
          minWidth = 160, html = TRUE,
          cell = function(value, index) {
            sprintf(
              paste0('<button type="button" class="row-open" ',
                     'onclick="Shiny.setInputValue(&quot;row_clicked&quot;, %d, ',
                     '{priority:&quot;event&quot;})">%s</button>'),
              tbl$proprietaryNameId[index], htmltools::htmlEscape(value))
          }),
        Ingredients = colDef(minWidth = 150),
        Type = colDef(minWidth = 130, html = TRUE, cell = function(v) {
          sprintf('<span class="badge-cat %s">%s</span>', category_class(v), v)
        }),
        Status = colDef(minWidth = 120, html = TRUE, cell = function(v) {
          if (identical(v, "Voluntarily withdrawn"))
            '<span class="badge-withdrawn">Withdrawn</span>' else v
        }),
        Labeler = colDef(minWidth = 150),
        Form = colDef(minWidth = 110),
        Species = colDef(minWidth = 150),
        Application = colDef(minWidth = 100)
      )
    )
  })

  observeEvent(input$row_clicked, {
    selected(as.integer(input$row_clicked))
    view("detail")
  })

  # Open the pioneer product of the drug currently on screen. Resolved here
  # from the selection rather than captured at render time, so it cannot go
  # stale if the user navigates before clicking.
  observeEvent(input$go_pioneer, {
    pid <- selected(); req(pid)
    prod <- PRODUCTS |> filter(proprietaryNameId == pid) |> slice(1)
    if (nrow(prod) == 0) return()
    app <- APPLICATIONS |> filter(applicationId == prod$applicationId) |> slice(1)
    if (nrow(app) == 0 || is.na(app$pioneerApplicationNumber)) return()

    pio <- APPLICATIONS |>
      filter(applicationNumber == app$pioneerApplicationNumber) |> slice(1)
    if (nrow(pio) == 0) return()
    pio_prod <- PRODUCTS |> filter(applicationId == pio$applicationId) |> slice(1)
    if (nrow(pio_prod) == 0) return()

    selected(pio_prod$proprietaryNameId)
  })

  # -- detail ----------------------------------------------------------------

  output$detail <- renderUI({
    pid <- selected()
    req(pid)

    prod <- PRODUCTS |> filter(proprietaryNameId == pid) |> slice(1)
    if (nrow(prod) == 0) return(p("Product not found."))

    app  <- APPLICATIONS |> filter(applicationId == prod$applicationId) |> slice(1)
    ings <- INGREDIENTS  |> filter(applicationId == prod$applicationId)
    sp   <- SPECIES      |> filter(proprietaryNameId == pid)
    docs <- DOCUMENTS    |> filter(applicationId == prod$applicationId)
    ndc  <- NDC          |> filter(proprietaryNameId == pid)
    labels <- LABEL_LINKS |> filter(proprietaryNameId == pid) |> arrange(tier)

    # Dosing is filtered to the chosen species when we can tell which
    # population headers belong to it. FDA does not link ail headers to
    # species codes, so we match on the header text naming the species or its
    # use classes -- and fall back to showing everything rather than hiding
    # doses we cannot confidently attribute.
    dose <- DOSING |> filter(proprietaryNameId == pid)
    g <- species()
    dose_note <- NULL
    if (!is.null(g) && g != "any" && nrow(dose) > 0) {
      sp_g <- sp |> filter(speciesGroup == g)
      if (nrow(sp_g) > 0) {
        terms <- norm_text(c(sp_g$speciesName, sp_g$useClass,
                             SPECIES_GROUPS$label[SPECIES_GROUPS$group == g]))
        terms <- unique(terms[nzchar(terms)])
        hdr <- norm_text(dose$populationHeader)
        keep <- map_lgl(hdr, function(h) any(str_detect(h, fixed(terms))) ||
                                          !nzchar(h))
        if (any(keep)) {
          dose <- dose[keep, , drop = FALSE]
          dose_note <- sprintf("Showing doses labeled for %s.", species_label())
        } else {
          dose_note <- paste0(
            "FDA does not separate this product's dose statements by species, ",
            "so all labeled doses are shown.")
        }
      }
    }

    pioneer <- if (!is.na(app$pioneerApplicationNumber)) {
      APPLICATIONS |> filter(applicationNumber == app$pioneerApplicationNumber) |> slice(1)
    } else NULL

    # Looked up rather than recomputed; see INGREDIENT_CLASSES in global.R.
    classes <- if (nrow(ings)) {
      INGREDIENT_CLASSES |>
        filter(activeIngredientName %in% ings$activeIngredientName) |>
        pull(drugClass) |> unique()
    } else character()
    guides <- match_guidelines(GUIDELINES, classes, unique(sp$speciesGroup))

    tagList(
      # -- identity ----------------------------------------------------------
      card(card_body(
        div(class = "d-flex justify-content-between align-items-start flex-wrap gap-2",
          div(
            h3(prod$proprietaryName, class = "mb-1"),
            div(class = "text-muted",
                sprintf("%s %s", app$applicationType, app$applicationNumber))
          ),
          div(class = "d-flex gap-2 align-items-center",
            span(class = paste("badge-cat", category_class(app$category)), app$category),
            if (app$marketStatus == "Voluntarily withdrawn")
              span(class = "badge-withdrawn", "Voluntarily withdrawn") else NULL
          )
        ),
        # Where FDA's structured type field contradicts the evidence, say so
        # explicitly. The vet is being shown "Conditional Approval" while
        # FDA's own website shows "NADA", and needs to know why.
        if (isTRUE(app$fdaTypeDisagrees))
          div(class = "disclaimer mt-2",
            strong("Conditionally approved. "),
            sprintf(paste0(
              "Effectiveness has not been fully demonstrated. Note that FDA's ",
              "Animal Drugs @ FDA database types this application as \"%s\", ",
              "which is incorrect — the conditional status is confirmed by the ",
              "mandatory -CA1 name suffix%s. Always read the label."),
              app$applicationType,
              if (isTRUE(app$condByLabel))
                " and by FDA's own indication text" else
                " and by the product's DailyMed label"))
        else NULL,
        hr(),
        layout_columns(
          col_widths = c(4, 4, 4),
          # NDC is joined from DailyMed by name, so it is genuinely absent for
          # most products. A search link is more use to a vet than an empty
          # field, and far more use than a guessed code.
          field("NDC", if (nrow(ndc)) {
            codes <- unique(ndc$ndc)
            tagList(
              paste(head(codes, 6), collapse = ", "),
              if (length(codes) > 6)
                span(class = "muted", sprintf(" +%d more", length(codes) - 6))
              else NULL,
              if (any(ndc$matchType != "exact name"))
                div(class = "muted small",
                    "matched to a single DailyMed label by name stem")
              else NULL,
              div(tags$a(href = ndc$dailymedUrl[1], target = "_blank",
                         rel = "noopener", class = "small", "View on DailyMed"))
            )
          } else {
            tagList(
              span(class = "muted", "Not listed in DailyMed"),
              div(tags$a(
                href = paste0(
                  "https://dailymed.nlm.nih.gov/dailymed/search.cfm?query=",
                  utils::URLencode(prod$proprietaryName, reserved = TRUE)),
                target = "_blank", rel = "noopener", class = "small",
                "Search DailyMed for this product"))
            )
          }),
          field("Current status", app$marketStatus),
          field("Product type", app$category)
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          field("Active ingredients",
                if (nrow(ings)) paste(unique(ings$activeIngredientName), collapse = ", ")
                else or_none(NA)),
          field("Labeler / sponsor", or_none(app$sponsorName)),
          field("Dispensing status", or_none(prod$dispensingStatus))
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          field("Dosage form", or_none(prod$doseFormName)),
          field("Route", or_none(prod$routes)),
          field("Strength / specifications", or_none(prod$specifications))
        )
      )),

      # -- product label -----------------------------------------------------
      # Ranked manufacturer -> FOI -> other cited source. Every source is
      # shown, not just the winner: a vet who cannot reach the manufacturer's
      # site needs the fallbacks visible rather than hidden behind the ranking.
      card(card_body(
        h5("Product label"),
        if (nrow(labels) == 0) p(class = "muted", "No label source located.")
        else tagList(
          div(class = "label-primary",
            tags$a(href = labels$url[1], target = "_blank", rel = "noopener",
                   labels$sourceName[1]),
            span(class = paste0("src-badge tier", min(labels$tier[1], 5)),
                 switch(as.character(min(labels$tier[1], 5)),
                        "1" = "Manufacturer", "2" = "Approved label",
                        "3" = "FDA label",    "4" = "FDA FOI", "Search")),
            div(class = "what-is", labels$whatItIs[1]),
            div(class = "cite", "Source: ", labels$citation[1],
                if (identical(labels$link_status[1], "blocked"))
                  " — this site blocks automated checks; open it in your browser"
                else NULL)
          ),
          if (nrow(labels) > 1) tagList(
            div(class = "field-label", style = "margin-top:.9rem", "Other sources"),
            div(map(2:nrow(labels), function(i) {
              tags$a(class = "doc-link", href = labels$url[i], target = "_blank",
                     rel = "noopener",
                     strong(labels$sourceName[i]),
                     span(class = "what-is", style = "display:block",
                          labels$whatItIs[i]),
                     span(class = "cite", "Source: ", labels$citation[i]))
            }))
          ) else NULL
        )
      )),

      # -- species and labeled use -----------------------------------------
      card(card_body(
        h5("Labeled species and use class"),
        if (nrow(sp) == 0) p(class = "muted", "No species listed.") else
          div(map(seq_len(nrow(sp)), function(i) {
            div(class = "mb-1",
              strong(sp$speciesName[i]),
              if (!is.na(sp$useClass[i]) && nzchar(sp$useClass[i]))
                span(" — ", sp$useClass[i]) else NULL)
          }))
      )),

      # -- dosing ------------------------------------------------------------
      card(card_body(
        h5("Labeled dose and indication"),
        div(class = "disclaimer mb-3",
          strong("FDA-labeled use. "),
          "Everything in this section is taken from the approved label as ",
          "published by FDA. Any use outside these species, doses, routes or ",
          "indications is extra-label and is your professional responsibility ",
          "under AMDUCA. This app does not provide extra-label dosing.",
          br(), br(),
          strong("This tool is not a substitute for the approved product "),
          strong("label or for Animal Drugs @ FDA. "),
          "It reformats FDA's published data and may lag the current record. ",
          "Verify against the label and ",
          tags$a(href = "https://animaldrugsatfda.fda.gov/adafda/views/#/search",
                 target = "_blank", rel = "noopener", "Animal Drugs @ FDA"),
          " before prescribing."),
        if (!is.null(dose_note)) p(class = "text-muted small", dose_note) else NULL,
        if (nrow(dose) == 0) p(class = "muted", "No dose statements published for this product.")
        else div(map(seq_len(nrow(dose)), function(i) {
          div(class = "dose-card",
            if (!is.na(dose$populationHeader[i]) && nzchar(dose$populationHeader[i]))
              div(class = "dose-pop", dose$populationHeader[i]) else NULL,
            div(class = "field-label", "Indication"),
            div(class = "field-value", fda_html(dose$indicationHtml[i]) %||%
                                        or_none(dose$indication[i])),
            div(class = "field-label", "Dose"),
            div(class = "field-value", fda_html(dose$dosageHtml[i]) %||%
                                        or_none(dose$dosage[i])),
            if (!is.na(dose$limitation[i]) && nzchar(dose$limitation[i]))
              tagList(div(class = "field-label", "Limitations"),
                      div(class = "field-value", fda_html(dose$limitationHtml[i])))
            else NULL
          )
        })),
        if (!is.na(prod$withdrawalPeriod) && nzchar(prod$withdrawalPeriod))
          tagList(hr(), div(class = "field-label", "Withdrawal period"),
                  div(class = "field-value", fda_html(prod$withdrawalHtml)))
        else NULL
      )),

      # -- documents ---------------------------------------------------------
      card(card_body(
        h5("Documents and further reading"),
        if (nrow(docs) == 0) p(class = "muted", "No documents published for this application.")
        else div(map(seq_len(nrow(docs)), function(i) {
          tags$a(class = "doc-link", href = docs$url[i], target = "_blank",
                 rel = "noopener",
                 strong(docs$docType[i]), " — ", docs$title[i],
                 if (!is.na(docs$docDate[i])) span(class = "text-muted",
                                                   paste0(" (", docs$docDate[i], ")")) else NULL)
        })),
        hr(),
        div(class = "field-label", "Pioneer product"),
        div(class = "field-value",
          if (!is.null(pioneer) && nrow(pioneer) > 0) {
            # Opens the pioneer inside this app. It used to link out to FDA,
            # which cannot address a single product, so the link went to a
            # search page with no mention of the pioneer at all.
            pio_prod <- PRODUCTS |>
              filter(applicationId == pioneer$applicationId) |> slice(1)
            tagList(
              sprintf("Generic of %s (%s). ",
                      if (nrow(pio_prod)) pio_prod$proprietaryName else "an earlier application",
                      fda_app_number(pioneer$applicationNumber)),
              if (nrow(pio_prod))
                actionLink("go_pioneer",
                           sprintf("Open %s", pio_prod$proprietaryName))
              else NULL
            )
          } else span(class = "muted",
                      "This is the pioneer product (no earlier application listed).")
        ),
        div(class = "field-label", "Look this up at FDA"),
        div(class = "field-value",
          "Animal Drugs @ FDA cannot link to a single product, so search it by ",
          "application number: ",
          tags$code(fda_app_number(app$applicationNumber)), " — ",
          tags$a(href = ADAFDA_SEARCH, target = "_blank", rel = "noopener",
                 "open Animal Drugs @ FDA"))
      )),

      # -- guidelines --------------------------------------------------------
      card(card_body(
        h5("Professional guidance"),
        p(class = "text-muted small",
          "Matched on this product's drug class and labeled species. ",
          "These are links to the publishing organization, not a statement ",
          "that a guideline endorses this product."),
        if (nrow(guides) == 0) p(class = "muted", "No mapped guidance for this drug class.")
        else div(map(seq_len(nrow(guides)), function(i) {
          tags$a(class = "doc-link", href = guides$url[i], target = "_blank",
                 rel = "noopener",
                 strong(guides$organization[i]), " — ", guides$title[i])
        })),
        hr(),
        p(class = "text-muted small mb-0",
          "For extra-label dosing, consult Plumb's Veterinary Drug Handbook ",
          "or another licensed formulary. This app deliberately does not ",
          "reproduce copyrighted formulary content.")
      ))
    )
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

shinyApp(ui, server)
