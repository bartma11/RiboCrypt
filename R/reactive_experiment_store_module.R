storeServer <- function(id, experiments) {
  
  createSelectComponent <- function(rChoices, session = getDefaultReactiveDomain()) {
    result <- list(
      choices = rChoices,
      selected = reactiveVal()
    )
    
    observe({
      req(result$choices())
      result$selected(result$choices()[1])
    }) %>% bindEvent(result$choices())
    
    return(result)
  }
  
  moduleServer(id, function(input, output, session) {
    organismComponent <- createSelectComponent(
      reactive(unique(experiments$organism))
      )

    experimentComponent <- createSelectComponent(
      reactive({
        req(organismComponent$selected())
        experiments[organism == organismComponent$selected()]$name
      })
    )
    
    loadedExperiment <- reactive({
      req(experimentComponent$selected())
      read.experiment(experimentComponent$selected(), validate = FALSE)
    })

    libraryComponent <- createSelectComponent(
      reactive({
        req(loadedExperiment())
        bamVarName(loadedExperiment())
      })
    )
    
    loadedGenes <- reactive({
      req(loadedExperiment())
      get_gene_name_categories(loadedExperiment())
    })
    
    geneComponent <- createSelectComponent(
      reactive({
        loadedGenes()$label
      })
    )
    
    txComponent <- createSelectComponent(
      reactive({
        req(geneComponent$choices())
        req(geneComponent$selected())
        tx_from_gene_list(loadedGenes(), geneComponent$selected())
      })
    )
    
    # TODO these are needed only for the actual plot to get the annotations
    # These do not belong here
    # cdsChoices <- reactive({
    #   loadRegion(loadedExperiment(), "cds")
    # })
    # 
    # txChoices <- reactive({
    #   loadRegion(loadedExperiment(), "tx")
    # })
    
    
    store <- list(
      organism = organismComponent,
      experiment = experimentComponent,
      libraries = libraryComponent,
      gene = geneComponent,
      tx = txComponent,
      loadedExperiment = loadedExperiment,
      loadedGenes = loadedGenes
    )
    
    return(store)
  })
}