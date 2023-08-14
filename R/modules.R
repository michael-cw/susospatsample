###################################
## Modules
###################################
## Stratum Variable Selection & Update
## UI
stratumVariableUI<-function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  selectizeInput(ns("strat_var"), "Stratification Variable", choices = c(""),
                 options = list(
                   placeholder = 'Load Boundary file first',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  )
  
}

## server
stratumVariableUpdateSvr <- function(input, output, session, dataset = NULL) {
  FF<-dataset
  if (is.null(FF)) {
    updateSelectizeInput(session = session, 
                      "strat_var", 
                      "Stratification Variable", 
                      choices = c(""),
                      options = list(
                        placeholder = 'Load Boundary file first',
                        onInitialize = I('function() { this.setValue(""); }')
                      )
    ) 
  } else {
    shiny::validate(need(FF, message = F))
    #################################
    ## Domain
    updateSelectizeInput(session = session, 
                         inputId = "strat_var",
                         label = "Stratification Variable",
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable bellow',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
    )
  }
}