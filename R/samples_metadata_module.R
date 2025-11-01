fromCSVFile <- function(filePath) {
  raw <- data.table::fread(filePath)
  withFilteredColumns <- raw[, !(colnames(raw) %in% c("BioProject", "BioSample", "Study_Pubmed_id", "name")), with = F]
}