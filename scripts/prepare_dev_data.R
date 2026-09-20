#!/usr/bin/env Rscript

experiment_name <- "ORFik_tutorial_data"
experiment_assembly <- "dummy"
demo_root <- file.path(path.expand("~/Bio_data"), "RiboCrypt_demo")
library_dir <- file.path(demo_root, "Homo_sapiens_sample")
reference_dir <- file.path(demo_root, "references")
index_file <- file.path(demo_root, "ribocrypt-demo.sqlite")
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

txdb_source <- find_one(extdata_dir, "Homo_sapiens_dummy.gtf.db")
fasta_source <- find_one(extdata_dir, "Homo_sapiens_dummy.fasta")
fasta_index_source <- paste0(fasta_source, ".fai")

dir.create(demo_root, recursive = TRUE, showWarnings = FALSE)
dir.create(reference_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(experiment_dir, recursive = TRUE, showWarnings = FALSE)

reference_sources <- c(txdb_source, fasta_source)
if (file.exists(fasta_index_source)) {
  reference_sources <- c(reference_sources, fasta_index_source)
}
for (source in reference_sources) {
  destination <- file.path(reference_dir, basename(source))
  if (!file.exists(destination) && !isTRUE(file.copy(source, destination))) {
    stop_with("Could not copy demo reference to ", destination, ".")
  }
}
txdb <- file.path(reference_dir, basename(txdb_source))
fasta <- file.path(reference_dir, basename(fasta_source))

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
experiment_exists <- file.exists(experiment_file)
experiment_needs_creation <- !experiment_exists
if (experiment_exists) {
  existing_experiment <- ORFik::read.experiment(
    experiment_file,
    validate = FALSE
  )
  existing_assembly <- if ("assembly" %in% methods::slotNames(existing_experiment)) {
    trimws(as.character(methods::slot(existing_experiment, "assembly")))
  } else {
    ""
  }
  existing_txdb <- normalizePath(
    as.character(methods::slot(existing_experiment, "txdb")),
    mustWork = FALSE
  )
  existing_fasta <- normalizePath(
    as.character(methods::slot(existing_experiment, "fafile")),
    mustWork = FALSE
  )
  experiment_needs_creation <- !nzchar(existing_assembly) ||
    !identical(existing_txdb, normalizePath(txdb, mustWork = TRUE)) ||
    !identical(existing_fasta, normalizePath(fasta, mustWork = TRUE))
}

if (experiment_needs_creation) {
  ORFik::create.experiment(
    dir = library_dir,
    saveDir = experiment_dir,
    exper = experiment_name,
    txdb = txdb,
    fa = fasta,
    organism = "Homo sapiens simulated",
    assembly = experiment_assembly,
    author = "Simulated by ORFik",
    types = "ofst",
    viewTemplate = FALSE
  )
  if (!file.exists(experiment_file)) {
    stop_with("ORFik did not create the expected experiment file: ", experiment_file)
  }
  message(if (experiment_exists) "Recreated experiment " else "Created experiment ",
    experiment_name)
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

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) != 1L) {
  stop_with("Could not determine the location of prepare_dev_data.R.")
}
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE))
importer <- file.path(script_dir, "import_orfik_experiments.R")
if (!file.exists(importer)) {
  stop_with("SQLite importer was not found: ", importer)
}

import_status <- system2(
  file.path(R.home("bin"), "Rscript"),
  args = vapply(
    c(importer, "--database", index_file, experiment_file),
    shQuote,
    character(1)
  )
)
if (!identical(import_status, 0L)) {
  stop_with("Could not create the demo SQLite index: ", index_file)
}

message(
  "Demo data ready: ", experiment_name, " with ", length(ofst_files),
  " OFST libraries and ", length(bigwigs), " BigWig tracks. SQLite index: ",
  index_file
)
