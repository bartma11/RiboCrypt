megaBrowserPlotUi <- function(id, gene_names_init, browser_options) {
    ns <- shiny::NS(id)
    all_isoforms <- subset(gene_names_init, label == browser_options["default_gene"])
    shiny::div(
        shiny::fluidRow(
            shiny::column(2, gene_input_select(ns, FALSE, browser_options)),
            shiny::column(2, tx_input_select(ns, FALSE, all_isoforms)),
            shiny::column(1, NULL, plot_button(ns("go")))
        ),
        shiny::fluidRow(
            plotly::plotlyOutput(ns("plot"))
        )
    )
}

megaBrowserPlotServer <- function(id, browser_options, df, metadata, rSelectedSamples) {
    shiny::moduleServer(id, function(input, output, session) {
        introns_width <- as.numeric(browser_options["collapsed_introns_width"])
        gene_name_list <- shiny::reactive({
            get_gene_name_categories(df())
        })
        ns <- shiny::NS(id)
        # Main plot controller, this code is only run if 'plot' is pressed
        controller <- shiny::reactive(
            click_plot_browser_allsamp_controller(
                df = df,
                gene_name_list = gene_name_list,
                selectedGene = input$gene,
                selectedTx = input$tx,
                regionType = c("leader+cds", "leader", "trailer", "cds", "mrna")[1],
                motif = NULL,
                extendLeaders = 0,
                extendTrailers = 0,
                displayAnnot = c(FALSE, TRUE)[1],
                viewMode = list(FALSE, "tx", "genomic")[1],
                otherTx = FALSE,
                collapsedIntrons = c(FALSE, TRUE)[1],
                collapsedIntronsWidth = introns_width,
                genomicRegion = "",
                clusters = 5,
                ratioInterval = NULL,
                orderByMetadataField = colnames(metadata)[1],
                otherGene = FALSE,
                enrichmentTerm = "Clusters (Order factor 1)",
                normalization = "maxNormalized",
                kmer = 9,
                minCount = 100,
                frame = FALSE,
                summaryTrack = FALSE,
                plotType = "plotly",
                heatmapColor = "Matrix(black, green, red)",
                colorMult = 3
            )
        ) %>% shiny::bindEvent(input$go)

        # Main plot, this code is only run if 'plot' is pressed
        table <- shiny::reactive(compute_collection_table_shiny(controller, metadata)) %>% shiny::bindEvent(controller())

        plot_object <- shiny::reactive(get_meta_browser_plot(
            table()$table,
            input$heatmap_color,
            input$clusters,
            input$color_mult,
            input$plotType
        )) %>% shiny::bindEvent(table())

        output$plot <- plotly::renderPlotly({
            get_meta_browser_plot_full(table()$table,
                plot_object(), controller()$id,
                controller()$dff, controller()$summary_track,
                controller()$display_annot,
                region_type = controller()$region_type,
                shiny::isolate(input$plotType), controller()$tx_annotation,
                controller()$display_region,
                controller()$annotation,
                controller()$viewMode,
                controller()$collapsed_introns_width
            )
        }) %>% shiny::bindEvent(plot_object())
    })
}
