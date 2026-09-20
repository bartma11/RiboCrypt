testthat::skip_if_not_installed("DBI")
testthat::skip_if_not_installed("RSQLite")
testthat::skip_if_not_installed("jsonlite")
testthat::skip_if_not_installed("ORFik")

importer_candidates <- c(
  testthat::test_path("..", "..", "scripts", "import_orfik_experiments.R"),
  testthat::test_path(
    "..", "..", "00_pkg_src", "RiboCrypt", "scripts",
    "import_orfik_experiments.R"
  )
)
importer_script <- importer_candidates[file.exists(importer_candidates)][1L]
if (is.na(importer_script)) {
  stop("Could not locate scripts/import_orfik_experiments.R")
}
importer_script <- normalizePath(importer_script, mustWork = TRUE)

write_orfik_experiment <- function(
    path,
    name = "test experiment",
    organism = "Homo sapiens",
    assembly = "GRCh38",
    annotation = "references/genes.gtf.gz",
    fasta = "references/genome.fa.gz",
    author = "Example Author",
    result_folder = "results",
    libraries = data.frame(
      libtype = "RFP", stage = "HEK293", rep = "1", condition = "WT",
      fraction = "total", filepath = "reads/sample.bam", reverse = "",
      runID = "SRR000001", stringsAsFactors = FALSE
    )) {
  metadata <- rbind(
    c("experiment", name, "results", result_folder, "assembly", assembly,
      rep("", max(0L, ncol(libraries) - 6L))),
    c("annotation", annotation, "", "", "organism", organism,
      rep("", max(0L, ncol(libraries) - 6L))),
    c("genome", fasta, "", "", "author", author,
      rep("", max(0L, ncol(libraries) - 6L)))
  )
  table <- rbind(metadata, names(libraries), as.matrix(libraries))
  utils::write.table(table, path, sep = ",", row.names = FALSE,
    col.names = FALSE, quote = TRUE, na = "")
}

run_importer <- function(database, inputs, env = character()) {
  command <- file.path(R.home("bin"), "Rscript")
  output <- suppressWarnings(system2(
      command,
      c(importer_script, "--database", database, inputs),
      stdout = TRUE, stderr = TRUE, env = env
    ))
  status <- attr(output, "status") %||% 0L
  list(output = paste(output, collapse = "\n"), status = status)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

read_table <- function(conn, table) {
  DBI::dbReadTable(conn, table)
}

testthat::test_that("the importer creates schema v2 and maps ORFik metadata", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "references"))
  dir.create(file.path(root, "reads"))
  writeBin(charToRaw("fasta"), file.path(root, "references", "genome.fa.gz"))
  writeBin(charToRaw("gtf"), file.path(root, "references", "genes.gtf.gz"))
  writeBin(charToRaw("bam"), file.path(root, "reads", "sample.bam"))
  csv <- file.path(root, "experiment.csv")
  database <- file.path(root, "named-database")
  write_orfik_experiment(csv)

  result <- run_importer(database, csv)
  testthat::expect_equal(result$status, 0L, info = paste(result$output, collapse = "\n"))
  testthat::expect_match(result$output, "imported=1")
  testthat::expect_true(file.exists(database))

  conn <- DBI::dbConnect(RSQLite::SQLite(), database)
  withr::defer(DBI::dbDisconnect(conn))
  expected_tables <- c(
    "database_metadata", "genome_reference", "annotation", "experiment",
    "sample", "library", "asset", "reference_asset", "annotation_asset",
    "library_asset", "experiment_asset"
  )
  testthat::expect_setequal(DBI::dbListTables(conn), expected_tables)
  expected_indexes <- c(
    "idx_annotation_reference", "idx_experiment_reference",
    "idx_sample_experiment", "idx_library_sample", "idx_library_type",
    "idx_asset_format", "idx_reference_asset_asset", "idx_reference_asset_role",
    "idx_annotation_asset_asset", "idx_annotation_asset_role",
    "idx_library_asset_asset", "idx_library_asset_role",
    "idx_experiment_asset_asset", "idx_experiment_asset_role"
  )
  indexes <- DBI::dbGetQuery(conn,
    "SELECT name FROM sqlite_master WHERE type = 'index' AND name LIKE 'idx_%'")$name
  testthat::expect_setequal(indexes, expected_indexes)
  testthat::expect_equal(
    DBI::dbGetQuery(conn,
      "SELECT value FROM database_metadata WHERE key = 'schema_version'")$value,
    "2"
  )
  testthat::expect_equal(nrow(DBI::dbGetQuery(conn, "PRAGMA foreign_key_check")), 0L)

  reference <- read_table(conn, "genome_reference")
  testthat::expect_equal(reference[c("organism", "assembly")],
    data.frame(organism = "Homo sapiens", assembly = "GRCh38"))
  sample <- read_table(conn, "sample")
  testthat::expect_equal(sample$name, "HEK293 WT replicate 1 total")
  testthat::expect_equal(sample$replicate, "1")
  library <- read_table(conn, "library")
  testthat::expect_equal(library$library_type, "RFP")
  testthat::expect_equal(library$layout, "single_end")
  testthat::expect_true(is.na(library$strandedness))
  testthat::expect_equal(jsonlite::fromJSON(library$metadata)$runID, "SRR000001")

  assets <- read_table(conn, "asset")
  testthat::expect_setequal(assets$format, c("fasta", "gtf", "bam"))
  testthat::expect_true(all(grepl(paste0("^", root), assets$uri)))
  testthat::expect_true(all(!is.na(assets$size_bytes)))
  testthat::expect_equal(read_table(conn, "reference_asset")$role,
    "genome_sequence")
  testthat::expect_equal(read_table(conn, "annotation_asset")$role,
    "annotation_source")
  testthat::expect_equal(read_table(conn, "library_asset")$role,
    "aligned_reads")
  metadata <- jsonlite::fromJSON(read_table(conn, "experiment")$metadata)
  testthat::expect_equal(metadata$author, "Example Author")
  testthat::expect_equal(metadata$source_csv, normalizePath(csv))
})

testthat::test_that("paired markers and strand-specific coverage are mapped", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "tracks"))
  libraries <- data.frame(
    libtype = c("RNA", "CAGE"), stage = c("cell", "cell"),
    rep = c("1", "2"), condition = c("WT", "WT"),
    fraction = c("total", "total"),
    filepath = c("tracks/paired.cram", "tracks/plus.bigWig.gz"),
    reverse = c("paired-end", "tracks/minus.BW.GZ"),
    runID = c("RUN1", "RUN2"), stringsAsFactors = FALSE
  )
  csv <- file.path(root, "layouts.csv")
  database <- file.path(root, "layouts.sqlite")
  write_orfik_experiment(csv, annotation = "", fasta = "", libraries = libraries)

  result <- run_importer(database, csv)
  testthat::expect_equal(result$status, 0L, info = paste(result$output, collapse = "\n"))
  conn <- DBI::dbConnect(RSQLite::SQLite(), database)
  withr::defer(DBI::dbDisconnect(conn))
  library <- read_table(conn, "library")
  testthat::expect_setequal(library$layout, c("paired_end", "single_end"))
  links <- DBI::dbGetQuery(conn, "
    SELECT library.library_type, library_asset.role, library_asset.strand
    FROM library_asset JOIN library ON library.id = library_asset.library_id")
  cage <- links[links$library_type == "CAGE", ]
  testthat::expect_equal(cage$role, c("coverage", "coverage"))
  testthat::expect_setequal(cage$strand, c("forward", "reverse"))
  testthat::expect_true(is.na(links$strand[links$library_type == "RNA"]))
  testthat::expect_true(all(is.na(read_table(conn, "asset")$size_bytes)))
})

testthat::test_that("shared catalogs deduplicate and exact reimport is stable", {
  root <- withr::local_tempdir()
  first <- file.path(root, "first.csv")
  second <- file.path(root, "second.csv")
  database <- file.path(root, "experiments.sqlite")
  write_orfik_experiment(first, name = "first", fasta = "shared.fa",
    annotation = "shared.gtf")
  write_orfik_experiment(second, name = "second", fasta = "shared.fa",
    annotation = "shared.gtf", libraries = data.frame(
      libtype = "RNA", stage = "HEK", rep = "1", condition = "KO",
      fraction = "total", filepath = "second.bam", reverse = "",
      runID = "RUN2", stringsAsFactors = FALSE
    ))

  result <- run_importer(database, c(first, second))
  testthat::expect_equal(result$status, 0L, info = paste(result$output, collapse = "\n"))
  conn <- DBI::dbConnect(RSQLite::SQLite(), database)
  before <- lapply(c("genome_reference", "annotation", "experiment", "sample",
    "library", "asset", "library_asset"), function(table) read_table(conn, table))
  names(before) <- c("genome_reference", "annotation", "experiment", "sample",
    "library", "asset", "library_asset")
  testthat::expect_equal(nrow(before$genome_reference), 1L)
  testthat::expect_equal(nrow(before$annotation), 1L)
  DBI::dbDisconnect(conn)

  result <- run_importer(database, first)
  testthat::expect_equal(result$status, 0L, info = paste(result$output, collapse = "\n"))
  testthat::expect_match(result$output, "synchronized=1")
  conn <- DBI::dbConnect(RSQLite::SQLite(), database)
  withr::defer(DBI::dbDisconnect(conn))
  after <- lapply(names(before), function(table) read_table(conn, table))
  names(after) <- names(before)
  testthat::expect_identical(after, before)
})

testthat::test_that("modified imports reconcile libraries and orphan assets", {
  root <- withr::local_tempdir()
  csv <- file.path(root, "changing.csv")
  database <- file.path(root, "changing.sqlite")
  initial <- data.frame(
    libtype = c("RFP", "RNA"), stage = "cell", rep = c("1", "2"),
    condition = "WT", fraction = "total",
    filepath = c("old.bam", "keep.bam"), reverse = "",
    runID = c("OLD", "KEEP"), stringsAsFactors = FALSE
  )
  write_orfik_experiment(csv, annotation = "", fasta = "", libraries = initial)
  testthat::expect_equal(run_importer(database, csv)$status, 0L)

  changed <- data.frame(
    libtype = c("RFP", "CAGE"), stage = "cell", rep = c("1", "3"),
    condition = c("KO", "WT"), fraction = "total",
    filepath = c("new.bam", "new.wig"), reverse = "",
    runID = c("NEW", "CAGE"), stringsAsFactors = FALSE
  )
  write_orfik_experiment(csv, annotation = "", fasta = "", libraries = changed)
  result <- run_importer(database, csv)
  testthat::expect_equal(result$status, 0L, info = paste(result$output, collapse = "\n"))

  conn <- DBI::dbConnect(RSQLite::SQLite(), database)
  withr::defer(DBI::dbDisconnect(conn))
  testthat::expect_setequal(read_table(conn, "library")$library_type,
    c("RFP", "CAGE"))
  testthat::expect_setequal(basename(read_table(conn, "asset")$uri),
    c("new.bam", "new.wig"))
  testthat::expect_equal(nrow(DBI::dbGetQuery(conn, "PRAGMA foreign_key_check")), 0L)
})

testthat::test_that("input failures roll back independently and conflicts are skipped", {
  root <- withr::local_tempdir()
  valid <- file.path(root, "valid.csv")
  invalid <- file.path(root, "invalid.csv")
  database <- file.path(root, "batch.sqlite")
  write_orfik_experiment(valid, name = "valid", annotation = "", fasta = "")
  write_orfik_experiment(invalid, name = "invalid", organism = "",
    annotation = "", fasta = "")

  result <- run_importer(database, c(valid, invalid))
  testthat::expect_gt(result$status, 0L)
  testthat::expect_match(result$output, "imported=1")
  testthat::expect_match(result$output, "failed=1")
  conn <- DBI::dbConnect(RSQLite::SQLite(), database)
  testthat::expect_equal(read_table(conn, "experiment")$name, "valid")
  DBI::dbDisconnect(conn)

  duplicate <- file.path(root, "duplicate.csv")
  write_orfik_experiment(duplicate, name = "valid", annotation = "", fasta = "")
  conflict_db <- file.path(root, "conflict.sqlite")
  result <- run_importer(conflict_db, c(valid, duplicate))
  testthat::expect_gt(result$status, 0L)
  testthat::expect_match(result$output, "skipped-conflict=2")
  conn <- DBI::dbConnect(RSQLite::SQLite(), conflict_db)
  testthat::expect_equal(nrow(read_table(conn, "experiment")), 0L)
  DBI::dbDisconnect(conn)
})

testthat::test_that("unsupported existing schemas are rejected", {
  root <- withr::local_tempdir()
  csv <- file.path(root, "valid.csv")
  database <- file.path(root, "old.sqlite")
  write_orfik_experiment(csv, annotation = "", fasta = "")
  conn <- DBI::dbConnect(RSQLite::SQLite(), database)
  DBI::dbExecute(conn, "CREATE TABLE database_metadata (key TEXT, value TEXT)")
  DBI::dbExecute(conn, "INSERT INTO database_metadata VALUES ('schema_version', '1')")
  DBI::dbDisconnect(conn)

  result <- run_importer(database, csv)
  testthat::expect_gt(result$status, 0L)
  testthat::expect_match(result$output, "not schema version 2")
})
