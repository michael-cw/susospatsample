#' Shiny UI module for upload of text (csv, tab) file 
#'
#' Files can be provided unzipped or zip compressed.
#'
#' @param id Namespace identifier
#' @param label File input label
#' @param accept Mime type (i.e. must be one of)
#'
#'
#'
#' @noRd
#' @keywords internal
zipFileInput_ui <- function(id, label, accept) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("frame_file"), label, multiple = F,
              accept = accept)
  )
}

#' Shiny server module for upload of text (csv, tab) file
#'
#'
#' @param input standard shiny input
#' @param output standard shiny output
#' @param session standard shiny session object
#' @param sep column separator, one of , ; or "\\t"
#' @param zipInput TRUE if upload is zip file
#'
#'
#' @return returns datatable with upload data by using data.table's fread function
#'
#'
#' @noRd
#' @keywords internal

zipFileInput_server <- function(id, sep = ",", zipInput = FALSE) {
  moduleServer(id, function(input, output, session) {
    userFile <- reactive({
      # If no file is selected, don't do anything
      validate(need(input$frame_file, message = FALSE))
      input$frame_file
    })
    
    frameFile<-reactive({
      if(!zipInput){
        # csv file
        filein<-data.table::fread(userFile()$datapath, sep = sep)
      } else if(zipInput) {
        # zip file
        shpFiles<-tempdir()
        
        ## 1. Unpack File
        unlink(paste0(shpFiles, "/*"))
        zip::unzip(userFile()$datapath, 
                   exdir = shpFiles,
                   overwrite = T,
                   junkpaths = T)
        fileList<-dir(shpFiles, recursive = TRUE, full.names = T)
        csvFile<-fileList[grep(".csv$", fileList)]
        if (length(csvFile)==0) {
          showModal(modalDialog(
            title = "Wrong Format!",
            "Your file is not provided in the correct format (ESRI Shapefile, Single Folder, zip). 
            Please check and upload again.",
            easyClose = TRUE,
            footer = NULL
          ))
          req(FALSE)
        } 
        ## 2. Read csv into data.table
        filein<-data.table::fread(file = csvFile, sep = sep)
      }
      return(filein)
    })
    
    return(frameFile)
  })
}


