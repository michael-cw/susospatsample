#' Shiny UI module for download of zip file from file path
#'
#'
#'
#' @param id Namespace identifier
#' @param label File input label
#' @param style Mime type (i.e. must be one of)
#'
#' @return file input
#'
#'
#'
#' @noRd
#' @keywords internal
#'
#'
zipFileDwl_ui <- function(id,
                          label="Download Tiles",
                          style = "color: #FFFFFF;width: 180px;background-color: #1976D2;border-color: #1976D2") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    useShinyjs(),
    disabled(
      downloadButton(ns("dwl_zip"),
                     label = label,
                     style=style)
    )
  )
}

#' Shiny server module for upload of text (csv, tab) file
#'
#'
#' @param file_name Name for zip file
#' @param path_to_zip file path to zip folder created by modal_createshape or modal_createbasemap
#'
#'
#' @return returns datatable with upload data by using data.table's fread function
#'
#'
#' @noRd
#' @keywords internal
#'
#'

zipFileDwl_server <- function(id, file_name = reactive({NULL}), path_to_zip = reactive({NULL})) {
  moduleServer(id, function(input, output, session) {
    # Enable download when file is available
    observe({
      req(path_to_zip())
      shinyjs::enable("dwl_zip")
    })
    # download file
    output$dwl_zip <- downloadHandler( filename = function() {
      # SYT<-stringr::str_remove_all(Sys.time(), "([:punct:])|([:space:])")
      # SEE<-sample_seed()
      # paste0(paste("Shapefiles", SYT,"seed" ,SEE, sep="_"), ".zip")
      file_name()
    },
    content = function(file) {
      req(path_to_zip())
      file.copy(path_to_zip(), file)
    }, contentType = "application/zip")
  })
}
