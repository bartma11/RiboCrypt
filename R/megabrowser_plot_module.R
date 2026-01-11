megaBrowserPlotUi <- function(id) {
    ns <- shiny::NS(id)
    shiny::div(
        shiny::fluidRow(
            plotly::plotlyOutput(ns("plot"))
        )
    )
}

megaBrowserPlotServer <- function(id, df, metadata, gene_name_list) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- shiny::NS(id)
        # Main plot controller, this code is only run if 'plot' is pressed
        controller <- shiny::reactive(
            click_plot_browser_allsamp_controller(
                df = df,
                gene_name_list = gene_name_list,
                selectedGene = input$gene,
                selectedTx = input$tx,
                regionType = input$region_type,
                motif = input$motif,
                extendLeaders = input$extendLeaders,
                extendTrailers = input$extendTrailers,
                displayAnnot = input$display_annot,
                viewMode = input$viewMode,
                otherTx = input$other_tx,
                collapsedIntrons = input$collapsed_introns,
                collapsedIntronsWidth = input$collapsed_introns_width,
                genomicRegion = input$genomic_region,
                clusters = input$clusters,
                ratioInterval = input$ratio_interval,
                metadata = metadata,
                otherGene = input$other_gene,
                enrichmentTerm = input$enrichment_term,
                normalization = input$normalization,
                kmer = input$kmer,
                minCount = input$min_count,
                frame = input$frame,
                summaryTrack = input$summary_track,
                plotType = input$plotType,
                heatmapColor = input$heatmap_color,
                colorMult = input$color_mult
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
