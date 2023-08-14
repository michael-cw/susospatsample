#' shiny module for  download of tabular data as zip file
#'
#'
#'
#' @description description this module also works with large data sets,
#' as it uses two separate functions for the download.
#' The first function generates the file, and the actual
#' download is activated by a shinyjs js function, which
#' is only activated after the file creation is
#' finished
#'
#' @noRd
#' @keywords internal


# ui
dwl_dataUI<-function(id,
                     label = "Download Data",
                     style = "color: #FFFFFF; width: 180px;background-color: #1976D2;border-color: #0d47a1") {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(10,
             ## 1. Action button to start the dwl
             actionButton(ns("generateDownloadOfTable"),
                          label = label,
                          icon("download"), width = "100%",
                          style=style)
      ),
      column(1)
    ),
    fluidRow(
      ## 2. DWL Button is INVISIBLE, activated by shinyjs::click
      downloadButton(ns("dwl_table"), "Not visible", style=invisibleButton)
    )
  )
  ####################FIN UI####################################################
}
# server
download_csv_server <- function(id,
                                file_name = NULL,
                                content = NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      # # enable download button
      # observe({
      #   req(content())
      #   shinyjs::enable("generateDownloadOfTable")
      # })

      observeEvent(input$generateDownloadOfTable, {
        shiny::validate(need(content(), message = F))

        fn<-file.path(tempdir(), paste0(file_name(), ".csv"))
        data.table::fwrite(content(), file = fn)
        fnzip<-file.path(tempdir(), paste0(file_name(), ".zip"))
        zip::zip(zipfile=fnzip, files=fn, mode = "cherry-pick")

        ## 2.5. Click DWL button
        shinyjs::click("dwl_table")
      })

      output$dwl_table <- downloadHandler(
        filename = function() {
          paste0(file_name(), ".zip")
        },
        content = function(file) {
          fn<-file.path(tempdir(), paste0(file_name(), ".csv"))
          fnzip<-file.path(tempdir(), paste0(file_name(), ".zip"))
          on.exit(
            file.remove(fnzip),
            file.remove(fn)
          )
          file.copy(fnzip, file)
        }, contentType = "application/zip")

      ####################FIN SERVER####################################################




    }
  )
}
