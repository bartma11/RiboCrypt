fromORFikExperimentList <- function(experimentList) {
  return(
    list(
      names = experimentList$name,
      organisms = unique.default(experimentList$organism),
      metaExperiments = fromORFikExperimentList(
        experimentList[grep("all_samples-", name),]
        ),
      getExperimentsByOrganism = function(organismName) {
        return(fromORFikExperimentList(experimentList[organism == organismName]))
      }
    )
  )
}