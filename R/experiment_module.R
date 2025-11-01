# fromORFikExperiment <- function(experiment) {
#   return(
#     list(
#       codingSequences = function() {
#         ORFik::loadRegion(experiment, "cds")
#       },
#       transcripts = function() {
#         ORFik::loadRegion(experiment, "tx")
#       },
      # getExperimentByName = function(experimentName) {
      #   loadedExperiment <- ORFik::read.experiment(
      #     experimentName,
      #     output.env = environment(),
      #     validate = FALSE,
      #     in.dir = experimentDirectory
      #   )
      #   return(fromORFikExperiment(loadedExperiment))
      # }
#     )
#   )
# }

# selectedCollectionLibrarySizes <- lib_sizes <- file.path(QCfolder(selectedExperiment), "totalCounts_mrna.rds")
# geneNames <- get_gene_name_categories(selectedExperiment)
# selectedTx <- names(tx[1])
# selectedCollectionPath <- collection_path_from_exp(selectedExperiment, selectedTx)