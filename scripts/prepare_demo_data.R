#!/usr/bin/env Rscript

experiment_name <- "ORFik_tutorial_data"
demo_root <- file.path(path.expand("~/Bio_data"), "RiboCrypt_demo")
library_dir <- file.path(demo_root, "Homo_sapiens_sample")
experiment_dir <- ORFik::config()[["exp"]]

stop_with <- function(...) {
  stop(..., call. = FALSE)
}

find_one <- function(root, filename) {
  matches <- list.files(
    root,
    pattern = paste0("^", filename, "$"),
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(matches) != 1L) {
    stop_with(
      "Expected exactly one ", filename, " below ", root,
      "; found ", length(matches), "."
    )
  }
  normalizePath(matches, mustWork = TRUE)
}

existing_bigwigs <- function(experiment) {
  paths <- tryCatch(
    unlist(
      ORFik::filepath(
        experiment,
        "bigwig",
        suffix_stem = c("_pshifted", "")
      ),
      use.names = FALSE
    ),
    error = function(error) character()
  )
  unique(paths[file.exists(paths)])
}

extdata_dir <- system.file("extdata", package = "ORFik")
sample_source <- system.file(
  "extdata/Homo_sapiens_sample",
  package = "ORFik"
)
if (!nzchar(extdata_dir) || !dir.exists(extdata_dir)) {
  stop_with("The installed ORFik package does not contain extdata.")
}
if (!nzchar(sample_source) || !dir.exists(sample_source)) {
  stop_with("ORFik's bundled Homo_sapiens_sample directory was not found.")
}

txdb <- find_one(extdata_dir, "Homo_sapiens_dummy.gtf.db")
fasta <- find_one(extdata_dir, "Homo_sapiens_dummy.fasta")

dir.create(demo_root, recursive = TRUE, showWarnings = FALSE)
dir.create(experiment_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(library_dir)) {
  copied <- file.copy(sample_source, demo_root, recursive = TRUE)
  if (!isTRUE(copied) || !dir.exists(library_dir)) {
    stop_with("Could not copy the ORFik sample data to ", library_dir, ".")
  }
  message("Copied ORFik sample libraries to ", library_dir)
}

ofst_files <- list.files(
  library_dir,
  pattern = "[.]ofst$",
  recursive = TRUE,
  full.names = TRUE
)
if (length(ofst_files) == 0L || !all(file.exists(ofst_files))) {
  stop_with("No readable .ofst libraries were found in ", library_dir, ".")
}

experiment_file <- file.path(experiment_dir, paste0(experiment_name, ".csv"))
if (!file.exists(experiment_file)) {
  ORFik::create.experiment(
    dir = library_dir,
    saveDir = experiment_dir,
    exper = experiment_name,
    txdb = txdb,
    fa = fasta,
    organism = "Homo sapiens simulated",
    author = "Simulated by ORFik",
    types = "ofst",
    viewTemplate = FALSE
  )
  if (!file.exists(experiment_file)) {
    stop_with("ORFik did not create the expected experiment file: ", experiment_file)
  }
  message("Created experiment ", experiment_name)
} else {
  message("Reusing experiment ", experiment_name)
}

experiment <- ORFik::read.experiment(
  experiment_name,
  validate = TRUE,
  in.dir = experiment_dir
)

bigwigs <- existing_bigwigs(experiment)
if (length(bigwigs) == 0L) {
  message("Generating BigWig browser tracks...")
  ORFik::convert_to_bigWig(experiment)
  bigwigs <- existing_bigwigs(experiment)
}
if (length(bigwigs) == 0L) {
  stop_with("BigWig conversion completed without producing readable tracks.")
}

experiments <- ORFik::list.experiments(
  validate = TRUE,
  BPPARAM = BiocParallel::SerialParam(),
  dir = experiment_dir
)
if (!experiment_name %in% experiments$name) {
  stop_with(experiment_name, " was not returned by list.experiments().")
}

message(
  "Demo data ready: ", experiment_name, " with ", length(ofst_files),
  " OFST libraries and ", length(bigwigs), " BigWig tracks."
)

