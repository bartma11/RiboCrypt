reactiveSelectUi <- function(id, label = "", multiple = FALSE, options = NULL) {
  ns <- NS(id)
  selectizeInput(ns("serverSelectizeInput"), choices = NULL, selected = NULL, label = label, multiple = multiple, options = options)
}


#' Variable selection module with server-side processing.
#' It synchronizes an existing reactiveSelectUi, that uses the same id, with provided reactive values.
#'
#' @param id an id to create a new Shiny namespace
#' @param rValues a list containing two reactive components, choices and selected, selected must be a reactiveVal
reactiveSelectServer <- function(id, rValues) {
  moduleServer(id, function(input, output, session) {
    
    updatedInput <- reactiveVal(FALSE)
    receivedValue <- reactiveVal(FALSE)
    
    observe({
      req(rValues$choices())
      if(receivedValue()) {
        receivedValue(FALSE)
      } else {
        updateSelectizeInput(session, "serverSelectizeInput", choices = rValues$choices(), selected = rValues$selected(), server = TRUE)
        updatedInput(TRUE)
      }
    }) %>% bindEvent(rValues$selected(), ignoreNULL = FALSE)
    
    observe({
      if(updatedInput()) {
        updatedInput(FALSE)
      } else {
        if(!is.null(input$serverSelectizeInput) && !is.null(rValues$selected())) {
          req(input$serverSelectizeInput != rValues$selected())
        } else {
          req(!is.null(input$serverSelectizeInput) || !is.null(rValues$selected()))
        }
        rValues$selected(input$serverSelectizeInput)
        receivedValue(TRUE)
      }
    }) %>% bindEvent(input$serverSelectizeInput, ignoreNULL = FALSE)
  })
}