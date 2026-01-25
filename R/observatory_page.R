observatory_ui <- function(id, all_exp_meta, browser_options) {
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
                "Table",
                h2("Table")
            ),
            shiny::tabPanel(
                "Coverage",
                h2("Coverage")
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

        # browserPlotServer(
        #     "browserPlot",
        #     browser_options,
        #     shiny::reactive(input$dff),
        #     rSelectedSamples
        # )
    })
}
