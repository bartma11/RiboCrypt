megaBrowserPlotUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    shiny::fluidRow(
      plotly::plotlyOutput(ns("plot"))
    )
  )
}

megaBrowserPlotServer <- function(id, metadata, gene_name_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)
    allsamples_observer_controller(input, output, session)
    # Main plot controller, this code is only run if 'plot' is pressed
    controller <- shiny::eventReactive(input$go,
      click_plot_browser_allsamp_controller(input, df, gene_name_list),
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )
    # Main plot, this code is only run if 'plot' is pressed
    table <- shiny::reactive(compute_collection_table_shiny(controller,
      metadata = metadata
    )) %>%
      shiny::bindCache(controller()$table_hash) %>%
      shiny::bindEvent(controller()$table_hash, ignoreInit = FALSE, ignoreNULL = TRUE)

    plot_object <- shiny::reactive(get_meta_browser_plot(
      table()$table,
      shiny::isolate(input$heatmap_color),
      shiny::isolate(input$clusters),
      shiny::isolate(input$color_mult),
      shiny::isolate(input$plotType)
    )) %>%
      shiny::bindEvent(table(), ignoreInit = FALSE, ignoreNULL = TRUE)

    output$myPlotlyPlot <- plotly::renderPlotly({
      shiny::req(input$plotType == "plotly")
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
    }) %>%
      shiny::bindCache(controller()$table_plot) %>%
      shiny::bindEvent(plot_object(), ignoreInit = FALSE, ignoreNULL = TRUE)
  })
}
