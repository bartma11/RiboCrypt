observatory_ui <- function(id, all_exp_meta, gene_names_init, browser_options) {
    ns <- shiny::NS(id)
    shiny::tabPanel(
        title = "Observatory",
        icon = shiny::icon("chart-line"),
        shiny::tabsetPanel(
            shiny::tabPanel(
                "UMAP",
                shiny::fluidRow(
                    umap_plot_controls_ui(ns("umapPlot"), all_exp_meta, browser_options)
                ),
                shiny::fluidRow(
                    umap_plot_ui(ns("umapPlot"))
                ),
                shiny::fluidRow(
                    shiny::column(
                        2,
                        sampleSelectionsUi(ns("sampleSelection"))
                    ),
                ),
                shiny::fluidRow(
                    sampleTableUi(ns("sampleTable"))
                )
            ),
            shiny::tabPanel(
                "Browser",
                browserPlotUi(
                    ns("browserPlot"),
                    gene_names_init = gene_names_init,
                    browser_options = browser_options
                )
            )
        )
    )
}

observatory_server <- function(id, metadata, browser_options) {
    shiny::moduleServer(id, function(input, output, session) {
        rSelection <- shiny::reactiveVal(NULL)
        rFilteredSelection <- shiny::reactiveVal(NULL)

        rSelectedPoints <- umap_plot_module_server(
            "umapPlot",
            metadata
        )

        shiny::observe({
            rSelection(rSelectedPoints())
        }) %>% shiny::bindEvent(rSelectedPoints())

        sampleTableServer(
            "sampleTable",
            metadata,
            rSelection,
            rFilteredSelection
        )

        rSelectedSamples <- sampleSelectionsServer(
            "sampleSelection",
            metadata,
            rSelection,
            rFilteredSelection
        )

        browserPlotServer(
            "browserPlot",
            browser_options,
            # TODO: This is a hacky way to get the data from the UMAP plot,
            # find a better way to do this
            shiny::reactive(input[["umapPlot-dff"]]),
            rSelectedSamples
        )
    })
}
