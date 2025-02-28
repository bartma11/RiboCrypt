
#' Load a ORFik collection table
#' @param path the path to gene counts
#' @return a data.table in long format
load_collection <- function(path) {
  table  <- fst::read_fst(path)
  setDT(table)
  table[, position := 1:.N, by = library]
  table[, `:=`(library, factor(library, levels = unique(library), ordered = TRUE))]
  return(table)
}
#' Normalize collection table
#'
#' @param table a data.table in long format
#' @param normalization a character string, which mode, for options see RiboCrypt:::normalizations
#' @param lib_sizes named integer vector, default NULL. If given will do a pre tpm normalization
#' for full library sizes
#' @param kmer integer, default 1L (off), if > 1 will smooth out signal with sliding window size kmer.
#' @param add_logscore logical, default TRUE, adds a log(score + 1) to table
#' @return a data.table of normalized results
normalize_collection <- function(table, normalization, lib_sizes = NULL,
                                 kmer = 1L, add_logscore = TRUE) {
  # Sliding window
  if (kmer > 1) table <- smoothenMultiSampCoverage(table, kmer = kmer)
  # Make tpm
  if (!is.null(lib_sizes)) {
    if (is.character(lib_sizes)) lib_sizes <- readRDS(lib_sizes)
    table[, score_tpm := ((count * 1000)  / lib_sizes[as.integer(library)]) * 10^6]
  } else table[, score_tpm := count]
  # Transcript normalization mode
  norm_opts <- normalizations("metabrowser")
  if (normalization == norm_opts[1]) {
    table[,score := score_tpm / sum(score_tpm), by = library]
    table[,score := score * 1e6]
  } else if (normalization == norm_opts[2]) {
    table[,score := score_tpm / max(score_tpm), by = library]
  } else if (normalization == norm_opts[3]) {
    table[, score := (score_tpm - mean(score_tpm)) / sd(score_tpm), by = library]
  } else table[, score := score_tpm]
  table[is.na(score), score := 0]
  if (add_logscore) table[,logscore := log(score + 1)]
  return(table)
}

match_collection_to_exp <- function(metadata, df) {
  matchings <- chmatch(metadata$Run, runIDs(df))
  matchings <- matchings[!is.na(matchings)]
  if (length(matchings) != nrow(df))
    stop("Metadata does not contain information on all collection samples!")
  return(matchings)
}

#' Cast a collection table to wide format
#' @param table a data.table in long format
#' @param value.var which column to use as scores, default "logscore"
#' @return a table in wide format
collection_to_wide <- function(table, value.var = "logscore") {
  # Remove columns not to be casted to wide format
  # table[, score_tpm := NULL]
  # table[, score := NULL]
  # table[, count := NULL]
  # To wide format
  dtable <- dcast(table, position ~ library, value.var = value.var)
  dtable[, position := NULL]
  return(dtable)
}

#' Get collection table normalized in wide format
#'
#' @inheritParams load_collection
#' @inheritParams normalize_collection
#' @inheritParams collection_to_wide
#' @param as_list logical, default FALSE. Return as list of size 2,
#' count data.table and metadata data.table Set to TRUE if you need metadata
#' subset (needed if you subset the table, to get correct matching)
#' @return a data.table in long or wide (default) format, if as list, it is a
#' list of size 2 (see argument as_list)
compute_collection_table <- function(path, lib_sizes, df,
                                     metadata_field, normalization,
                                     kmer, metadata, min_count = 0, format = "wide",
                                     value.var = "logscore", as_list = FALSE) {
  table <- load_collection(path)
  # Normalize
  if (min_count > 0) {
    lib_names <- unique(table$library)
    filt_libs <- table[,.(count = sum(count)),library][count >= min_count,]$library
    table <- table[library %in% filt_libs]
    if (length(filt_libs) == 0)
      stop("Count filter too strict, no libraries with that much reads for this transcript!")
  }
  table <- normalize_collection(table, normalization, lib_sizes, kmer)
  ## # Sort table by metadata column selected
  # Match metadata table and collection runIDs
  matchings <- match_collection_to_exp(metadata, df)
  meta_sub <- metadata[matchings, metadata_field, with = FALSE][[1]]
  meta_order <- order(meta_sub)

  table[, library := factor(library, levels = levels(library)[meta_order], ordered = TRUE)]
  if (min_count > 0) {
    meta_sub <- meta_sub[lib_names %in% filt_libs]
  }
  # Cast to wide format and return
  if (format == "wide") {
    table <- collection_to_wide(table, value.var = value.var)
  }
  if (as_list) return(list(table = table, metadata_field = meta_sub))
  return(table)
}
