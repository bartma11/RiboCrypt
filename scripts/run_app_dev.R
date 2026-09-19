#!/usr/bin/env Rscript

project_dir <- normalizePath("/workspace/RiboCrypt", mustWork = TRUE)
app_file <- file.path(project_dir, "app_dev.R")
reload_pattern <- "[.](r|html?|js|css|png|jpe?g|gif)$"

port <- suppressWarnings(as.integer(Sys.getenv("RIBOCRYPT_PORT", "3838")))
if (is.na(port) || port < 1L || port > 65535L) {
  stop("RIBOCRYPT_PORT must be an integer from 1 through 65535.", call. = FALSE)
}

watch_roots <- file.path(project_dir, c("R", "inst"))
source_state <- function() {
  paths <- sort(unique(unlist(lapply(
    watch_roots,
    list.files,
    pattern = reload_pattern,
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  ))))
  info <- file.info(paths)
  stats <- paste(as.numeric(info$mtime), info$size, sep = ":")
  stats::setNames(stats, paths)
}

last_source_state <- source_state()
poll_source_changes <- function() {
  current_source_state <- source_state()
  if (!identical(current_source_state, last_source_state)) {
    last_source_state <<- current_source_state
    Sys.setFileTime(app_file, Sys.time())
  }
  later::later(poll_source_changes, delay = 0.5)
}
later::later(poll_source_changes, delay = 0.5)

options(
  shiny.autoload.r = FALSE,
  shiny.autoreload = TRUE
)

shiny::runApp(
  app_file,
  host = "0.0.0.0",
  port = port,
  launch.browser = FALSE
)
