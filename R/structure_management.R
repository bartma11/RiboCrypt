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

#' Fetch summary of UniProt id
#' @param qualifier UniProt id
#' @param provider optional, "all" by default, for available provides see valid_providers in this file
#' @return a character of json
#' @importFrom RCurl getURI
#' @importFrom jsonlite fromJSON
fetch_3dbeacons_summary <- function(qualifier, provider = "all") {
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
#' @return a data table where each row contains id, category, uri and file format of a model
fetch_3dbeacons_models_for_structure <- function(qualifier, provider = "all") {
  summ <- fetch_3dbeacons_summary(qualifier, provider)
  model_id <- model_identifiers_from_summary(summ)
  model_category <- model_categories_from_summary(summ)
  model_uri <- model_urls_from_summary(summ) 
  model_format <- mapply(tolower, model_formats_from_summary(summ))
  data.table(model_id, model_category, model_uri, model_format)
}

#' Fetch data about models available in a local directory
#' @param directory path to directory where structure files are stored
#' @return a data table where each row contains id, category, uri and                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           file format of a model
fetch_local_models_for_structure <- function(directory) {
  model_uri <- file.path(list.files(directory))
  model_category <- rep(NULL, length(model_uri))
  model_id <- mapply(
    function(elem) {
      base::strsplit("/") %>% 
        tail() %>%
        base:strsplit(".") %>%
        head()
      },
    model_uri
    )
  model_format <- mapply(
    function(elem) {
      base::strsplit("/") %>% 
        tail() %>%
        base:strsplit(".") %>%
        tail() %>%
        tolower()
    },
    model_uri
  )
  data.table(model_id, model_category, model_uri, model_format)
}