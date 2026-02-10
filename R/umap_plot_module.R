umap_plot_controls_ui <- function(id, all_exp_meta, browser_options) {
    ns <- shiny::NS(id)
    shiny::fluidRow(
        shiny::column(2, umap_plot_type(ns)),
        shiny::column(2, experiment_input_select(all_exp_meta$name, ns, browser_options)),
        shiny::column(2, umap_color_by_input_select(ns)),
        shiny::column(1, plot_button(ns("goUmapPlot")))
    )
}

umap_plot_ui <- function(id) {
    ns <- shiny::NS(id)
    plotly::plotlyOutput(ns("plot")) %>% shinycssloaders::withSpinner(color = "#0dc5c1")
}

umap_plot_module_server <- function(id, all_exp_meta) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        umap_data <- shiny::reactive({
            load_data_umap(input$dff, input$umap_col)
        }) %>% shiny::bindEvent(input$goUmapPlot, ignoreInit = FALSE, ignoreNULL = TRUE)
        output$plot <- {
            plotly::renderPlotly({
                generated_plot <- {
                    if (input$umap_plot_type == "UMAP") {
                        umap_plot(umap_data())
                    } else {
                        umap_centroids_plot(umap_data())
                    }
                }
                htmlwidgets::onRender(generated_plot, fetchJS("umap_plot_extension.js"), ns("selectedPoints"))
            }) %>%
                shiny::bindEvent(umap_data(), ignoreInit = FALSE, ignoreNULL = TRUE)
        }

        return(shiny::reactive(input$selectedPoints))
    })
}

umap_plot <- function(dt_umap, color.by = attr(dt_umap, "color.by")) {
    color.by.temp <- "color_column"
    names(color.by.temp) <- stringr::str_to_title(paste(color.by, collapse = " | "))
    color.by <- color.by.temp

    gg <- ggplot(dt_umap, aes(x = `UMAP 1`, y = `UMAP 2`, color = color_column)) +
        geom_point() +
        cowplot::theme_cowplot() +
        scale_fill_viridis_b() +
        labs(color = names(color.by))
    text_aes <- aes(text = paste0(
        "Bioproject ", dt_umap$BioProject, "\n",
        "Run ID: ", dt_umap$sample, "\n",
        "Author: ", dt_umap$author, "\n",
        "Inhibitor: ", dt_umap$inhibitors, "\n",
        "Tissue | CellLine: ", dt_umap$tissues_cell_lines
    ))
    plotly::ggplotly(gg + text_aes, tooltip = "text")
}

umap_centroids_plot <- function(dt_umap) {
    centroids <- dt_umap[, .(centroid_x = mean(`UMAP 1`), centroid_y = mean(`UMAP 2`)), by = color_column]
    coords <- as.matrix(centroids[, .(centroid_x, centroid_y)])
    rownames(coords) <- centroids$color_column
    dmat <- as.matrix(dist(coords))
    dt_dist <- as.data.table(as.table(dmat))
    setnames(dt_dist, c("Group1", "Group2", "Distance"))
    dt_dist <- dt_dist[Group1 != Group2]
    dt_dist[, Comparison := paste(Group1, "vs", Group2)]
    dt_dist <- dt_dist[as.integer(factor(Group1)) < as.integer(factor(Group2))]
    plot_ly(
        data = dt_dist,
        x = ~Group2,
        y = ~Group1,
        z = ~Distance,
        type = "heatmap",
        colorscale = "Viridis",
        hovertemplate = "Group1: %{y}<br>Group2: %{x}<br>Distance: %{z}<extra></extra>"
    ) %>%
        layout(
            title = "Upper Triangle: Pairwise Distance Between Group Centroids",
            xaxis = list(title = "Group 2", tickangle = 45),
            yaxis = list(title = "Group 1", autorange = "reversed")
        )
}
