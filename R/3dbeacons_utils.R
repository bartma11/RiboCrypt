# 3d-beacons api description
# https://www.ebi.ac.uk/pdbe/pdbe-kb/3dbeacons/api

beacon_main_api_url <- function() {
  'https://www.ebi.ac.uk/pdbe/pdbe-kb/3dbeacons/api/uniprot/summary/'
}

valid_providers <- function() {
  c("pdbe", "ped", "swissmodel",
    "alphafold", "sasbdb", "alphafill",
    "hegelab", "modelarchive", "isoformio", 
    "levylab", "all")
}

#' Fetch summary of uniprot id
#' @param qualifier uniprot ids
#' @param provider "pdbe", alternatives: "alphafold", "all"
#' @return a character of json
#' @importFrom RCurl getURI
#' @importFrom jsonlite fromJSON
fetch_summary <- function(qualifier, provider = "all") {
  # browser()
  provider_url <-
  if (provider == "all") {
    ""
  } else paste0("?provider=", provider)

  paste0(beacon_main_api_url(), qualifier, ".json", provider_url) %>% 
    RCurl::getURI() %>% 
    jsonlite::fromJSON()
}

#' @param summ 3dbeacons summary for a UniProt id
#' @return vector of strings representing model ids 
model_identifiers_from_summary <- function(summ) {
  summ$structures$summary$model_identifier
}

#' @param summ 3dbeacons summary for a UniProt id
#' @return vector of strings representing model categories
model_categories_from_summary <- function(summ) {
  summ$structures$summary$model_category
}

#' @param summ 3dbeacons summary for a UniProt id
#' @return vector of strings representing urls of model files
model_urls_from_summary <- function(summ) {
  summ$structures$summary$model_url
}

#' @param summ 3dbeacons summary for a UniProt id
#' @return vector of strings representing file extensions of model files
model_formats_from_summary <- function(summ) {
  summ$structures$summary$model_format
}

#' Fetch 3dbeacons data about models for a UniProt id
#' @param qualifier UniProt id
#' @param provider optional, "all" by default, for available provides see valid_providers in this file
#' @return a data table where each row contains id, category, url and file format of a model
fetch_models_for_structure <- function(qualifier, provider = "all") {
  summ <- fetch_summary(qualifier, provider)
  model_id <- model_identifiers_from_summary(summ)
  model_category <- model_categories_from_summary(summ)
  model_url <- model_urls_from_summary(summ)
  model_format <- model_formats_from_summary(summ)
  data.table(model_id, model_category, model_url, model_format)
}