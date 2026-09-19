#!/usr/bin/env Rscript

# Import ORFik experiment CSV files into the RiboCrypt SQLite schema v2.
# This file intentionally embeds the schema so it can be copied and run on its
# own. Runtime dependencies are ORFik, DBI, RSQLite, and jsonlite.

schema_sql <- "
PRAGMA foreign_keys = ON;

CREATE TABLE database_metadata (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL
);
INSERT INTO database_metadata (key, value) VALUES ('schema_version', '2');

CREATE TABLE genome_reference (
    id INTEGER PRIMARY KEY,
    key TEXT NOT NULL UNIQUE,
    organism TEXT NOT NULL,
    taxonomy_id INTEGER,
    assembly TEXT NOT NULL,
    metadata TEXT CHECK (metadata IS NULL OR json_valid(metadata)),
    created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now'))
);

CREATE TABLE annotation (
    id INTEGER PRIMARY KEY,
    key TEXT NOT NULL UNIQUE,
    genome_reference_id INTEGER NOT NULL,
    provider TEXT,
    version TEXT,
    metadata TEXT CHECK (metadata IS NULL OR json_valid(metadata)),
    created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    FOREIGN KEY (genome_reference_id) REFERENCES genome_reference(id)
      ON UPDATE CASCADE ON DELETE RESTRICT
);

CREATE TABLE experiment (
    id INTEGER PRIMARY KEY,
    key TEXT NOT NULL UNIQUE,
    name TEXT NOT NULL,
    genome_reference_id INTEGER NOT NULL,
    description TEXT,
    metadata TEXT CHECK (metadata IS NULL OR json_valid(metadata)),
    created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    FOREIGN KEY (genome_reference_id) REFERENCES genome_reference(id)
      ON UPDATE CASCADE ON DELETE RESTRICT
);

CREATE TABLE sample (
    id INTEGER PRIMARY KEY,
    key TEXT NOT NULL UNIQUE,
    experiment_id INTEGER NOT NULL,
    name TEXT,
    replicate TEXT,
    stage TEXT,
    condition TEXT,
    fraction TEXT,
    metadata TEXT CHECK (metadata IS NULL OR json_valid(metadata)),
    created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    FOREIGN KEY (experiment_id) REFERENCES experiment(id)
      ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE library (
    id INTEGER PRIMARY KEY,
    key TEXT NOT NULL UNIQUE,
    sample_id INTEGER NOT NULL,
    library_type TEXT NOT NULL,
    layout TEXT CHECK (layout IS NULL OR layout IN
      ('single_end', 'paired_end', 'unknown')),
    strandedness TEXT CHECK (strandedness IS NULL OR strandedness IN
      ('unstranded', 'forward', 'reverse', 'unknown')),
    metadata TEXT CHECK (metadata IS NULL OR json_valid(metadata)),
    created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    FOREIGN KEY (sample_id) REFERENCES sample(id)
      ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE asset (
    id INTEGER PRIMARY KEY,
    key TEXT UNIQUE,
    format TEXT NOT NULL,
    uri TEXT NOT NULL,
    checksum TEXT,
    checksum_type TEXT,
    size_bytes INTEGER CHECK (size_bytes IS NULL OR size_bytes >= 0),
    metadata TEXT CHECK (metadata IS NULL OR json_valid(metadata)),
    created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now'))
);

CREATE TABLE reference_asset (
    genome_reference_id INTEGER NOT NULL,
    asset_id INTEGER NOT NULL,
    role TEXT NOT NULL,
    metadata TEXT CHECK (metadata IS NULL OR json_valid(metadata)),
    PRIMARY KEY (genome_reference_id, asset_id, role),
    FOREIGN KEY (genome_reference_id) REFERENCES genome_reference(id)
      ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (asset_id) REFERENCES asset(id)
      ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE annotation_asset (
    annotation_id INTEGER NOT NULL,
    asset_id INTEGER NOT NULL,
    role TEXT NOT NULL,
    metadata TEXT CHECK (metadata IS NULL OR json_valid(metadata)),
    PRIMARY KEY (annotation_id, asset_id, role),
    FOREIGN KEY (annotation_id) REFERENCES annotation(id)
      ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (asset_id) REFERENCES asset(id)
      ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE library_asset (
    library_id INTEGER NOT NULL,
    asset_id INTEGER NOT NULL,
    role TEXT NOT NULL,
    strand TEXT CHECK (strand IS NULL OR strand IN ('forward', 'reverse')),
    metadata TEXT CHECK (metadata IS NULL OR json_valid(metadata)),
    PRIMARY KEY (library_id, asset_id, role),
    FOREIGN KEY (library_id) REFERENCES library(id)
      ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (asset_id) REFERENCES asset(id)
      ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE experiment_asset (
    experiment_id INTEGER NOT NULL,
    asset_id INTEGER NOT NULL,
    role TEXT NOT NULL,
    metadata TEXT CHECK (metadata IS NULL OR json_valid(metadata)),
    PRIMARY KEY (experiment_id, asset_id, role),
    FOREIGN KEY (experiment_id) REFERENCES experiment(id)
      ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (asset_id) REFERENCES asset(id)
      ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE INDEX idx_annotation_reference ON annotation(genome_reference_id);
CREATE INDEX idx_experiment_reference ON experiment(genome_reference_id);
CREATE INDEX idx_sample_experiment ON sample(experiment_id);
CREATE INDEX idx_library_sample ON library(sample_id);
CREATE INDEX idx_library_type ON library(library_type);
CREATE INDEX idx_asset_format ON asset(format);
CREATE INDEX idx_reference_asset_asset ON reference_asset(asset_id);
CREATE INDEX idx_reference_asset_role ON reference_asset(role);
CREATE INDEX idx_annotation_asset_asset ON annotation_asset(asset_id);
CREATE INDEX idx_annotation_asset_role ON annotation_asset(role);
CREATE INDEX idx_library_asset_asset ON library_asset(asset_id);
CREATE INDEX idx_library_asset_role ON library_asset(role);
CREATE INDEX idx_experiment_asset_asset ON experiment_asset(asset_id);
CREATE INDEX idx_experiment_asset_role ON experiment_asset(role);
"

usage <- function(status = 0L, message = NULL) {
  if (!is.null(message)) message(message)
  cat(
    "Usage: Rscript scripts/import_orfik_experiments.R ",
    "--database FILE EXPERIMENT.csv [EXPERIMENT.csv ...]\n",
    "\nOptions:\n",
    "  -d, --database FILE  SQLite database to create or update\n",
    "  -h, --help           Show this help\n",
    sep = "",
    file = if (status == 0L) stdout() else stderr()
  )
  quit(save = "no", status = status)
}

parse_args <- function(args) {
  database <- NULL
  inputs <- character()
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("-h", "--help")) usage()
    if (arg %in% c("-d", "--database")) {
      if (i == length(args)) usage(2L, paste0("Missing value for ", arg))
      if (!is.null(database)) usage(2L, "The database option was supplied twice")
      database <- args[[i + 1L]]
      i <- i + 2L
      next
    }
    if (startsWith(arg, "--database=")) {
      if (!is.null(database)) usage(2L, "The database option was supplied twice")
      database <- substring(arg, nchar("--database=") + 1L)
      i <- i + 1L
      next
    }
    if (startsWith(arg, "-")) usage(2L, paste("Unknown option:", arg))
    inputs <- c(inputs, arg)
    i <- i + 1L
  }
  if (is.null(database) || !nzchar(database)) usage(2L, "A database path is required")
  if (!length(inputs)) usage(2L, "At least one experiment CSV is required")
  list(database = database, inputs = inputs)
}

scalar_text <- function(x, field, required = FALSE) {
  if (length(x) != 1L || is.na(x)) x <- ""
  x <- trimws(as.character(x))
  if (required && !nzchar(x)) stop(field, " must not be empty", call. = FALSE)
  if (nzchar(x)) x else NULL
}

encoded <- function(x) {
  x <- if (is.null(x) || is.na(x)) "" else trimws(enc2utf8(as.character(x)))
  utils::URLencode(x, reserved = TRUE, repeated = TRUE)
}

stable_key <- function(prefix, ...) {
  paste(c(prefix, vapply(list(...), encoded, character(1))), collapse = "|")
}

is_external_uri <- function(path) {
  grepl("^[A-Za-z][A-Za-z0-9+.-]*://", path)
}

resolve_uri <- function(path, csv_dir) {
  path <- scalar_text(path, "path")
  if (is.null(path)) return(NULL)
  path <- path.expand(path)
  if (is_external_uri(path) || grepl("^data:", path, ignore.case = TRUE)) return(path)
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", path)) path <- file.path(csv_dir, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

asset_format <- function(uri) {
  path <- sub("[?#].*$", "", uri)
  repeat {
    next_path <- sub("\\.(gz|bgz|bz2|xz|zip)$", "", path, ignore.case = TRUE)
    if (identical(next_path, path)) break
    path <- next_path
  }
  ext <- tolower(sub("^.*\\.", "", basename(path)))
  formats <- c(
    fa = "fasta", fasta = "fasta", fna = "fasta",
    gtf = "gtf", gff = "gff", gff3 = "gff3",
    db = "sqlite", sqlite = "sqlite", sqlite3 = "sqlite",
    bam = "bam", cram = "cram", bed = "bed", ofst = "ofst",
    wig = "wig", bigwig = "bigwig", bw = "bigwig"
  )
  value <- unname(formats[ext])
  if (!length(value) || is.na(value)) {
    stop("Unsupported asset extension for: ", uri, call. = FALSE)
  }
  value
}

asset_role <- function(format, kind) {
  if (kind == "reference") return("genome_sequence")
  if (kind == "annotation") {
    return(if (format == "sqlite") "txdb" else "annotation_source")
  }
  if (format %in% c("wig", "bigwig")) "coverage" else "aligned_reads"
}

file_size_or_null <- function(uri) {
  if (is_external_uri(uri) || !file.exists(uri) || dir.exists(uri)) return(NULL)
  size <- file.info(uri)$size[[1L]]
  if (is.na(size)) NULL else as.numeric(size)
}

json_object <- function(values, null_if_empty = FALSE) {
  if (null_if_empty && !length(values)) return(NULL)
  as.character(jsonlite::toJSON(values, auto_unbox = TRUE,
    null = "null", na = "null"))
}

slot_text <- function(object, name) {
  if (!name %in% methods::slotNames(object)) return(NULL)
  scalar_text(methods::slot(object, name), name)
}

column_text <- function(data, candidates, required = FALSE) {
  present <- candidates[candidates %in% names(data)]
  if (!length(present)) {
    if (required) stop("Missing required column: ", candidates[[1L]], call. = FALSE)
    return(rep(NA_character_, nrow(data)))
  }
  as.character(data[[present[[1L]]]])
}

read_input <- function(path) {
  csv_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  object <- ORFik::read.experiment(csv_path, validate = FALSE)
  libraries <- tryCatch(
    as.data.frame(object, stringsAsFactors = FALSE),
    error = function(e) as.data.frame(methods::slot(object, "listData"),
      stringsAsFactors = FALSE)
  )
  if (!nrow(libraries)) stop("Experiment has no library rows", call. = FALSE)

  experiment_name <- scalar_text(slot_text(object, "experiment"),
    "experiment name", required = TRUE)
  organism <- scalar_text(slot_text(object, "organism"), "organism", required = TRUE)
  assembly <- scalar_text(slot_text(object, "assembly"), "assembly", required = TRUE)
  filepath <- column_text(libraries, "filepath", required = TRUE)
  libtype <- column_text(libraries, "libtype", required = TRUE)
  if (anyNA(filepath) || any(!nzchar(trimws(filepath)))) {
    stop("Every library must have a nonempty filepath", call. = FALSE)
  }
  if (anyNA(libtype) || any(!nzchar(trimws(libtype)))) {
    stop("Every library must have a nonempty libtype", call. = FALSE)
  }

  csv_dir <- dirname(csv_path)
  stage <- column_text(libraries, "stage")
  condition <- column_text(libraries, "condition")
  replicate <- column_text(libraries, c("rep", "replicate"))
  fraction <- column_text(libraries, "fraction")
  reverse <- column_text(libraries, "reverse")
  run_id <- column_text(libraries, c("runID", "Run", "run_id"))
  clean <- function(x) {
    x[is.na(x)] <- ""
    trimws(x)
  }

  rows <- data.frame(
    stage = clean(stage), condition = clean(condition),
    replicate = clean(replicate), fraction = clean(fraction),
    libtype = clean(libtype), filepath = clean(filepath),
    reverse = clean(reverse), run_id = clean(run_id),
    stringsAsFactors = FALSE
  )
  rows$filepath <- vapply(rows$filepath, resolve_uri, character(1), csv_dir = csv_dir)
  rows$reverse_uri <- vapply(rows$reverse, function(x) {
    if (!nzchar(x) || identical(tolower(x), "paired-end")) return(NA_character_)
    resolve_uri(x, csv_dir)
  }, character(1))

  genome_key <- stable_key("genome", organism, assembly)
  experiment_key <- stable_key("experiment", experiment_name)
  rows$sample_key <- mapply(
    function(stage, condition, replicate, fraction) stable_key(
      experiment_key, stage, condition, replicate, fraction
    ),
    rows$stage, rows$condition, rows$replicate, rows$fraction,
    USE.NAMES = FALSE
  )
  rows$library_key <- mapply(
    function(sample_key, libtype) stable_key(sample_key, libtype),
    rows$sample_key, rows$libtype, USE.NAMES = FALSE
  )
  if (anyDuplicated(rows$library_key)) {
    stop("Library metadata does not produce unique stable keys", call. = FALSE)
  }

  list(
    path = csv_path,
    name = experiment_name,
    key = experiment_key,
    organism = organism,
    assembly = assembly,
    genome_key = genome_key,
    author = slot_text(object, "author"),
    result_folder = slot_text(object, "resultFolder"),
    fasta = resolve_uri(slot_text(object, "fafile"), csv_dir),
    annotation = resolve_uri(slot_text(object, "txdb"), csv_dir),
    rows = rows
  )
}

same_value <- function(a, b) {
  if (length(a) == 0L) a <- NA
  if (length(b) == 0L) b <- NA
  (is.na(a) && is.na(b)) || (!is.na(a) && !is.na(b) && identical(as.character(a), as.character(b)))
}

db_params <- function(values) {
  unname(lapply(values, function(value) {
    if (is.null(value) || !length(value)) NA else value
  }))
}

upsert_entity <- function(conn, table, key, values) {
  quoted_table <- DBI::dbQuoteIdentifier(conn, table)
  row <- DBI::dbGetQuery(conn,
    paste0("SELECT * FROM ", quoted_table, " WHERE key = ?"),
    params = list(key)
  )
  values <- c(list(key = key), values)
  if (!nrow(row)) {
    columns <- names(values)
    sql <- paste0(
      "INSERT INTO ", quoted_table, " (",
      paste(DBI::dbQuoteIdentifier(conn, columns), collapse = ", "),
      ") VALUES (", paste(rep("?", length(values)), collapse = ", "), ")"
    )
    DBI::dbExecute(conn, sql, params = db_params(values))
    return(DBI::dbGetQuery(conn,
      paste0("SELECT id FROM ", quoted_table, " WHERE key = ?"),
      params = list(key)
    )$id[[1L]])
  }

  changed <- names(values)[!vapply(names(values), function(name) {
    same_value(row[[name]][[1L]], values[[name]])
  }, logical(1))]
  changed <- setdiff(changed, "key")
  if (length(changed)) {
    sql <- paste0(
      "UPDATE ", quoted_table, " SET ",
      paste0(DBI::dbQuoteIdentifier(conn, changed), " = ?", collapse = ", "),
      ", updated_at = strftime('%Y-%m-%dT%H:%M:%fZ', 'now') WHERE key = ?"
    )
    DBI::dbExecute(conn, sql,
      params = c(db_params(values[changed]), list(key)))
  }
  row$id[[1L]]
}

upsert_asset <- function(conn, uri) {
  format <- asset_format(uri)
  key <- stable_key("asset", uri)
  id <- upsert_entity(conn, "asset", key, list(
    format = format, uri = uri, checksum = NULL, checksum_type = NULL,
    size_bytes = file_size_or_null(uri), metadata = NULL
  ))
  list(id = id, format = format)
}

insert_relationship <- function(conn, table, values) {
  columns <- names(values)
  quoted_table <- DBI::dbQuoteIdentifier(conn, table)
  sql <- paste0(
    "INSERT OR IGNORE INTO ", quoted_table, " (",
    paste(DBI::dbQuoteIdentifier(conn, columns), collapse = ", "),
    ") VALUES (", paste(rep("?", length(values)), collapse = ", "), ")"
  )
  DBI::dbExecute(conn, sql, params = db_params(values))
}

reconcile_library_assets <- function(conn, library_id, desired) {
  existing <- DBI::dbGetQuery(conn, "
    SELECT asset_id, role, strand
    FROM library_asset WHERE library_id = ?", params = list(library_id))
  signature <- function(asset_id, role, strand) {
    paste(asset_id, role, ifelse(is.na(strand), "", strand), sep = "\r")
  }
  existing_signatures <- if (nrow(existing)) {
    signature(existing$asset_id, existing$role, existing$strand)
  } else character()
  desired_signatures <- vapply(desired, function(link) {
    signature(link$asset_id, link$role,
      if (is.null(link$strand)) NA_character_ else link$strand)
  }, character(1))

  stale <- which(!existing_signatures %in% desired_signatures)
  for (i in stale) {
    DBI::dbExecute(conn, "
      DELETE FROM library_asset
      WHERE library_id = ? AND asset_id = ? AND role = ?",
      params = list(library_id, existing$asset_id[[i]], existing$role[[i]]))
  }
  missing <- which(!desired_signatures %in% existing_signatures)
  for (i in missing) {
    link <- desired[[i]]
    insert_relationship(conn, "library_asset", list(
      library_id = library_id, asset_id = link$asset_id,
      role = link$role, strand = link$strand, metadata = NULL
    ))
  }
}

sample_name <- function(row) {
  labels <- c(
    if (nzchar(row$stage)) row$stage,
    if (nzchar(row$condition)) row$condition,
    if (nzchar(row$replicate)) paste("replicate", row$replicate),
    if (nzchar(row$fraction)) row$fraction
  )
  if (length(labels)) paste(labels, collapse = " ") else NULL
}

sync_input <- function(conn, input) {
  existed <- nrow(DBI::dbGetQuery(conn,
    "SELECT id FROM experiment WHERE key = ?", params = list(input$key))) > 0L

  genome_id <- upsert_entity(conn, "genome_reference", input$genome_key, list(
    organism = input$organism, taxonomy_id = NULL,
    assembly = input$assembly, metadata = NULL
  ))

  if (!is.null(input$fasta)) {
    asset <- upsert_asset(conn, input$fasta)
    if (asset$format != "fasta") stop("FASTA path is not a FASTA asset", call. = FALSE)
    insert_relationship(conn, "reference_asset", list(
      genome_reference_id = genome_id, asset_id = asset$id,
      role = "genome_sequence", metadata = NULL
    ))
  }

  if (!is.null(input$annotation)) {
    asset <- upsert_asset(conn, input$annotation)
    if (!asset$format %in% c("gtf", "gff", "gff3", "sqlite")) {
      stop("Annotation path is not GTF, GFF, or SQLite", call. = FALSE)
    }
    annotation_key <- stable_key(input$genome_key, input$annotation)
    annotation_id <- upsert_entity(conn, "annotation", annotation_key, list(
      genome_reference_id = genome_id, provider = NULL,
      version = NULL, metadata = NULL
    ))
    insert_relationship(conn, "annotation_asset", list(
      annotation_id = annotation_id, asset_id = asset$id,
      role = asset_role(asset$format, "annotation"), metadata = NULL
    ))
  }

  experiment_metadata <- json_object(list(
    author = input$author,
    result_folder = input$result_folder,
    source_csv = input$path
  ))
  experiment_id <- upsert_entity(conn, "experiment", input$key, list(
    name = input$name, genome_reference_id = genome_id,
    description = NULL, metadata = experiment_metadata
  ))
  cleanup_candidates <- DBI::dbGetQuery(conn, "
    SELECT DISTINCT library_asset.asset_id
    FROM library_asset
    JOIN library ON library.id = library_asset.library_id
    JOIN sample ON sample.id = library.sample_id
    WHERE sample.experiment_id = ?", params = list(experiment_id))$asset_id

  desired_samples <- character()
  desired_libraries <- character()
  for (i in seq_len(nrow(input$rows))) {
    row <- input$rows[i, , drop = FALSE]
    sample_id <- upsert_entity(conn, "sample", row$sample_key, list(
      experiment_id = experiment_id, name = sample_name(row),
      replicate = scalar_text(row$replicate, "replicate"),
      stage = scalar_text(row$stage, "stage"),
      condition = scalar_text(row$condition, "condition"),
      fraction = scalar_text(row$fraction, "fraction"), metadata = NULL
    ))
    desired_samples <- c(desired_samples, row$sample_key)

    paired_marker <- identical(tolower(row$reverse), "paired-end")
    library_metadata <- if (nzchar(row$run_id)) {
      json_object(list(runID = row$run_id))
    } else NULL
    library_id <- upsert_entity(conn, "library", row$library_key, list(
      sample_id = sample_id, library_type = row$libtype,
      layout = if (paired_marker) "paired_end" else "single_end",
      strandedness = NULL, metadata = library_metadata
    ))
    desired_libraries <- c(desired_libraries, row$library_key)

    desired_links <- list()
    forward <- upsert_asset(conn, row$filepath)
    if (!forward$format %in% c("bam", "cram", "bed", "ofst", "wig", "bigwig")) {
      stop("Library filepath has an unsupported library format", call. = FALSE)
    }
    has_reverse <- !is.na(row$reverse_uri)
    desired_links[[1L]] <- list(
      asset_id = forward$id,
      role = asset_role(forward$format, "library"),
      strand = if (has_reverse) "forward" else NULL
    )
    if (has_reverse) {
      reverse <- upsert_asset(conn, row$reverse_uri)
      if (!reverse$format %in% c("bam", "cram", "bed", "ofst", "wig", "bigwig")) {
        stop("Reverse filepath has an unsupported library format", call. = FALSE)
      }
      desired_links[[2L]] <- list(
        asset_id = reverse$id,
        role = asset_role(reverse$format, "library"), strand = "reverse"
      )
    }

    reconcile_library_assets(conn, library_id, desired_links)
  }

  existing_libraries <- DBI::dbGetQuery(conn, "
    SELECT library.id, library.key
    FROM library JOIN sample ON sample.id = library.sample_id
    WHERE sample.experiment_id = ?", params = list(experiment_id))
  stale_libraries <- existing_libraries$id[!existing_libraries$key %in% desired_libraries]
  if (length(stale_libraries)) {
    placeholders <- paste(rep("?", length(stale_libraries)), collapse = ",")
    DBI::dbExecute(conn, paste0("DELETE FROM library WHERE id IN (", placeholders, ")"),
      params = as.list(stale_libraries))
  }

  existing_samples <- DBI::dbGetQuery(conn,
    "SELECT id, key FROM sample WHERE experiment_id = ?",
    params = list(experiment_id))
  stale_samples <- existing_samples$id[!existing_samples$key %in% desired_samples]
  if (length(stale_samples)) {
    placeholders <- paste(rep("?", length(stale_samples)), collapse = ",")
    DBI::dbExecute(conn, paste0("DELETE FROM sample WHERE id IN (", placeholders, ")"),
      params = as.list(stale_samples))
  }

  if (length(cleanup_candidates)) {
    placeholders <- paste(rep("?", length(cleanup_candidates)), collapse = ",")
    DBI::dbExecute(conn, paste0("
      DELETE FROM asset
      WHERE id IN (", placeholders, ")
        AND NOT EXISTS (SELECT 1 FROM reference_asset WHERE asset_id = asset.id)
        AND NOT EXISTS (SELECT 1 FROM annotation_asset WHERE asset_id = asset.id)
        AND NOT EXISTS (SELECT 1 FROM library_asset WHERE asset_id = asset.id)
        AND NOT EXISTS (SELECT 1 FROM experiment_asset WHERE asset_id = asset.id)"),
      params = as.list(cleanup_candidates))
  }

  violations <- DBI::dbGetQuery(conn, "PRAGMA foreign_key_check")
  if (nrow(violations)) stop("Foreign-key integrity check failed", call. = FALSE)
  if (existed) "synchronized" else "imported"
}

initialize_or_validate <- function(conn, was_empty) {
  DBI::dbExecute(conn, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(conn, "PRAGMA busy_timeout = 5000")
  tables <- DBI::dbListTables(conn)
  if (was_empty && !length(tables)) {
    DBI::dbWithTransaction(conn, {
      statements <- trimws(strsplit(schema_sql, ";", fixed = TRUE)[[1L]])
      for (statement in statements[nzchar(statements)]) DBI::dbExecute(conn, statement)
    })
    return(invisible(NULL))
  }
  if (!"database_metadata" %in% tables) {
    stop("Existing database has no schema-version metadata", call. = FALSE)
  }
  version <- DBI::dbGetQuery(conn,
    "SELECT value FROM database_metadata WHERE key = 'schema_version'")
  if (nrow(version) != 1L || !identical(as.character(version$value[[1L]]), "2")) {
    stop("Existing database is not schema version 2", call. = FALSE)
  }
  required <- c(
    "genome_reference", "annotation", "experiment", "sample", "library",
    "asset", "reference_asset", "annotation_asset", "library_asset",
    "experiment_asset"
  )
  missing <- setdiff(required, tables)
  if (length(missing)) stop("Schema-v2 database is missing tables: ",
    paste(missing, collapse = ", "), call. = FALSE)
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  database_parent <- dirname(args$database)
  if (!dir.exists(database_parent)) {
    usage(2L, paste("Database parent directory does not exist:", database_parent))
  }
  if (dir.exists(args$database)) {
    usage(2L, paste("Database path is a directory:", args$database))
  }
  invalid_inputs <- args$inputs[!file.exists(args$inputs) | dir.exists(args$inputs)]
  if (length(invalid_inputs)) usage(2L, paste("Invalid input CSV:", invalid_inputs[[1L]]))
  if (!all(vapply(c("ORFik", "DBI", "RSQLite", "jsonlite"),
      requireNamespace, logical(1), quietly = TRUE))) {
    stop("Packages ORFik, DBI, RSQLite, and jsonlite are required", call. = FALSE)
  }

  parsed <- vector("list", length(args$inputs))
  errors <- rep(NA_character_, length(args$inputs))
  for (i in seq_along(args$inputs)) {
    value <- tryCatch(read_input(args$inputs[[i]]), error = function(e) {
      errors[[i]] <<- conditionMessage(e)
      NULL
    })
    parsed[i] <- list(value)
  }

  valid_indexes <- which(vapply(parsed, Negate(is.null), logical(1)))
  conflict <- integer()
  if (length(valid_indexes)) {
    keys <- vapply(parsed[valid_indexes], `[[`, character(1), "key")
    paths <- vapply(parsed[valid_indexes], `[[`, character(1), "path")
    duplicated_keys <- unique(keys[duplicated(keys) | duplicated(keys, fromLast = TRUE)])
    for (key in duplicated_keys) {
      indexes <- valid_indexes[keys == key]
      if (length(unique(paths[keys == key])) > 1L) conflict <- c(conflict, indexes)
    }
  }

  database_existed <- file.exists(args$database)
  was_empty <- !database_existed || file.info(args$database)$size[[1L]] == 0
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = args$database)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  initialize_or_validate(conn, was_empty)

  statuses <- rep("failed", length(args$inputs))
  statuses[conflict] <- "skipped-conflict"
  for (i in seq_along(args$inputs)) {
    if (is.null(parsed[[i]]) || i %in% conflict) next
    status <- tryCatch(
      DBI::dbWithTransaction(conn, sync_input(conn, parsed[[i]])),
      error = function(e) {
        errors[[i]] <<- conditionMessage(e)
        "failed"
      }
    )
    statuses[[i]] <- status
  }

  for (i in seq_along(args$inputs)) {
    detail <- if (statuses[[i]] == "skipped-conflict") {
      "duplicate experiment key in different input CSVs"
    } else errors[[i]]
    cat(sprintf("%s: %s%s\n", args$inputs[[i]], statuses[[i]],
      if (!is.na(detail)) paste0(" (", detail, ")") else ""))
  }
  counts <- table(factor(statuses,
    levels = c("imported", "synchronized", "skipped-conflict", "failed")))
  cat(sprintf(
    "Summary: imported=%d synchronized=%d skipped-conflict=%d failed=%d\n",
    counts[[1L]], counts[[2L]], counts[[3L]], counts[[4L]]
  ))
  if (any(statuses %in% c("failed", "skipped-conflict"))) {
    quit(save = "no", status = 1L)
  }
}

if (!identical(Sys.getenv("RIBOCRYPT_IMPORTER_NO_MAIN"), "1")) {
  tryCatch(main(), error = function(e) {
    message("Error: ", conditionMessage(e))
    quit(save = "no", status = 1L)
  })
}
