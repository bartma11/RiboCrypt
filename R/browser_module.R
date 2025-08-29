getBrowserHashStrings <- function(browserController, dff) {
  full_names <- ORFik:::name_decider(dff, naming = "full")
  hash_bottom <- paste(browserController$selectedTx, browserController$showAllTxAnnotations,
                       browserController$addUORFs,  browserController$addTranslons,
                       browserController$extendTrailers, browserController$extendLeaders,
                       browserController$genomicRegion, browserController$viewMode,
                       browserController$collapsedIntronsWidth,
                       browserController$customSequence, browserController$phyloP, browserController$mapability,
                       collapse = "|_|")
  # Until plot and coverage is split (bottom must be part of browser hash)
  hash_browser <- paste(hash_bottom,
                        full_names,
                        browserController$plotExportFormat,
                        browserController$summaryTrack, browserController$summaryTrackType,
                        browserController$kmer, browserController$framesType, browserController$withFrames,
                        browserController$useLogScale, browserController$zoomRange, browserController$framesSubset,
                        collapse = "|_|")
  hash_expression <- paste(full_names, browserController$selectedTx,
                           browserController$expressionPlot, browserController$extendTrailers,
                           browserController$extendLeaders, collapse = "|_|")
  hash_strings <- list(hash_bottom = hash_bottom, hash_browser = hash_browser,
                       hash_expression = hash_expression)
  stopifnot(all(lengths(hash_strings) == 1))
  return(hash_strings)
}

getPlotControls <- function(browserController) {
  print("- Browser controller")
  print(paste("here is gene!", browserController$selectedCds))
  print(paste("here is tx!", browserController$selectedTx))
  # Annotation
  display_region <- observed_tx_annotation(browserController$selectedTx, browserController$tx)
  tx_annotation <- observed_cds_annotation(browserController$selectedTx, browserController$tx,
                                           browserController$showAllTxAnnotations)
  cds_annotation <- observed_cds_annotation(browserController$selectedTx, browserController$cds,
                                            browserController$showAllTxAnnotations)
  uorf_annotation <- observed_uorf_annotation(browserController$selectedTx, browserController$experiment,
                                              browserController$showAllTxAnnotations, browserController$addUORFs)
  translon_annotation <- observed_translon_annotation(browserController$selectedTx, browserController$experiment,
                                                      browserController$showAllTxAnnotations, browserController$addTranslons)
  customRegions <- c(uorf_annotation, translon_annotation)
  
  # View controller
  collapsed_introns_width <- browserController$collapsedIntronsWidth
  if (!browserController$collapsedIntrons) collapsed_introns_width <- 0
  if (collapsed_introns_width > 0) {
    tx_annotation <- tx_annotation[tx_annotation %over% flankPerGroup(display_region)]
    display_region_gr <- reduce(unlistGrl(tx_annotation))
    display_region <- groupGRangesBy(display_region_gr, rep(names(display_region), length(display_region_gr)))
  }
  display_region <- genomic_string_to_grl(isolate(browserController$genomicRegion), display_region,
                                          max_size = 1e6, isolate(browserController$viewMode),
                                          isolate(browserController$extendLeaders),
                                          isolate(browserController$extendTrailers),
                                          collapsed_introns_width)
  zoom_range <- get_zoom_range(browserController$zoomRange, display_region,
                               max_size = 1e6, browserController$viewMode,
                               browserController$extendLeaders,
                               browserController$extendTrailers)
  
  if (!is.null(attr(zoom_range, "message"))) {
    showModal(modalDialog(
      title = "Invalid zoom range",
      attr(zoom_range, "message")
    ))
  }
  
  dff <- observed_exp_subset(browserController$selectedLibrary, browserController$libraries, browserController$experiment)
  if (nrow(dff) > 200) stop("Browser only supports up to 200 libraries for now, use megabrowser!")
  if (browserController$withFrames) {
    withFrames <- libraryTypes(dff, uniqueTypes = FALSE) %in% c("RFP", "RPF", "LSU", "TI")
  } else withFrames <- rep(FALSE, nrow(dff))
  
  # Hash strings for cache
  hash_strings <- getBrowserHashStrings(browserController, dff)
  
  reads <- try(filepath(dff, "bigwig", suffix_stem = c("_pshifted", "")))
  invalid_reads <- is(reads, "try-error") ||
    (!all(file.exists(unlist(reads, use.names = FALSE))) |
       any(duplicated(unlist(reads, use.names = FALSE))))
  if (invalid_reads) {
    reads <- filepath(dff, "bigwig", suffix_stem = c("_pshifted", ""),
                      base_folders = libFolder(dff, "all"))
  }
  frames_subset <- browserController$framesSubset
  use_all_frames <- length(frames_subset) == 0 || any(c("","all") %in% frames_subset)
  if (use_all_frames) frames_subset <- "all"
  
  reactiveValues(dff = dff,
                 display_region = display_region,
                 customRegions = customRegions,
                 extendTrailers = browserController$extendTrailers,
                 extendLeaders = browserController$extendLeaders,
                 export_format = browserController$plotExportFormat,
                 summary_track = browserController$summaryTrack,
                 summary_track_type = browserController$summaryTrackType,
                 viewMode = browserController$viewMode,
                 collapsed_introns_width = collapsed_introns_width,
                 kmerLength = browserController$kmer,
                 frames_type = browserController$framesType,
                 annotation = cds_annotation,
                 tx_annotation = tx_annotation,
                 reads = reads,
                 custom_sequence = browserController$customSequence,
                 log_scale = browserController$useLogScale,
                 phyloP = browserController$phyloP,
                 withFrames = withFrames,
                 zoom_range = zoom_range,
                 frames_subset = frames_subset,
                 mapability = browserController$mapability,
                 hash_bottom = hash_strings[["hash_bottom"]],
                 hash_browser = hash_strings[["hash_browser"]],
                 hash_expression = hash_strings[["hash_expression"]])
}

ui <- function(id, height = "500px") {
  ns <- NS(id)
  plotlyOutput(ns("plot"), height = height)
}

#' This is a browser server
#' @param input
#' @param output
#' @param session
#' @param browserGo
#' a boolean reactive value to trigger computation of the plot
#' @param browserController
#' Reactive values object containing:
#' experiment - a loaded ORFik experiment
#' cds - a GRangesList object with CDS'es from the experiment
#' tx - a GRangesList object with transcripts from the experiment
#' libraries - ???
#' selectedCds - ???
#' selectedTx - ???
#' selectedLibrary - ???
#' showAllTxAnnotations - a boolean switch to include/exclude annotations for transcripts other than the selected one
#' addUORFs - a boolean switch to include/exclude uORF annotaions
#' addTranslons - a boolean switch to include/exclude translon annotations
#' withFrames - a boolean swtich, ???
#' extendLeaders - an integer value, dictates how much leading part of the selected sequence will get extended
#' extendTrailers - an integer value, dictates how much trailing part of the selected sequence will get extended
#' collapsedIntrons - a boolean value, ???
#' collapsedIntronsWidth - an integer value, ???
#' genomicRegion - ???
#' customSequence - ???
#' viewMode - a string value that describes selected view mode, possible values are "genomic", ???
#' zoomRange - a numeric value, ???
#' useLogScale - a boolan switch to turn on/off using log scale
#' kmer - ???
#' framesType - ???
#' framesSubset - ???
#' summaryTrack - ???
#' summaryTrackType - ???
#' plotExportFormat - ???
#' phyloP - ???
#' mapability - ???
#' expressionPlot - ???
#' @param browserOptions
#' ???
server <- function(input, output, session, browserGo, browserController, browserOptions) {
  mainPlotControls <- reactive({
    getPlotControls(browserController)
  }) %>% bindEvent(browserGo,
                   ignoreInit = check_plot_on_start(browserOptions),
                   ignoreNULL = TRUE)

  bottom_panel <- reactive(bottom_panel_shiny(mainPlotControls))  %>%
    bindCache(mainPlotControls()$hash_bottom) %>%
    bindEvent(mainPlotControls(), ignoreInit = FALSE, ignoreNULL = TRUE)

  browser_plot <- reactive(browser_track_panel_shiny(mainPlotControls, bottom_panel(), session)) %>%
    bindCache(mainPlotControls()$hash_browser) %>%
    bindEvent(bottom_panel(), ignoreInit = FALSE, ignoreNULL = TRUE)

  output$plot <- renderPlotly(browser_plot()) %>%
    bindCache(mainPlotControls()$hash_browser) %>%
    bindEvent(browser_plot(), ignoreInit = FALSE, ignoreNULL = TRUE)
}