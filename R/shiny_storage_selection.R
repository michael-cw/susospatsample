#' shiny module for  storage selection
#'
#'
#'
#' @description this module also works with large data sets,
#' as it uses two separate functions for the download.
#' The first function generates the file, and the actual
#' download is activated by a shinyjs js function, which
#' is only activated after the file creation is
#' finished
#'
#' @noRd
#' @keywords internal


# ui
datastoreUI<-function(id,
                      styledbset = styleActButtonActivate,
                      styledbreset = "color: #FFFFFF;background-color: #7f0000;border-color: #7f0000") {
  ns <- NS(id)
  tagList(
    fluidRow(h4("Data Storage")),
    fluidRow(shiny::helpText("
                            Specify how the data should be stored. In case you are
                            not sure choose Local Files, however if your data is stored
                            at a local PostGIS-PostgreSQL server or you are using
                            the docker installation then choose PostgreSQL. For large files
                            it is generally recommended to have a local database installation
                            or to use the docker installation.
                            ")),
    fluidRow(
      column(2),
      column(8,
             radioButtons(ns("datastore"),
                          label = "Data Storage of Input Data?",
                          choices = c("PostgreSQL"=1, "Local Files"=2),
                          selected = "", inline = T
             )
      ),
      column(2)
    ),
    conditionalPanel("input.datastore==1", ns = ns,
                     # Postgres settings --> DB must be setup externally
                     fluidRow(
                       column(6,
                              textInput(
                                ns("dbname"),
                                "Provide DB name:",
                                placeholder = "Database Name"
                              )),
                       column(6,
                              textInput(
                                ns("dbhost"),
                                "Provide DB host:",
                                placeholder = "Database Host"
                              )
                       )
                     ),
                     fluidRow(
                       column(6,
                              textInput(
                                ns("dbuser"),
                                "Provide DB user:",
                                placeholder = "Database User"
                              )),
                       column(6,
                              passwordInput(
                                ns("dbpass"),
                                "Provide DB password:",
                                placeholder = "Database Password"
                              )
                       )
                     ),
                     fluidRow(
                       actionButton(
                         ns("dbset"),
                         "Confirm Database Connection!",
                         icon("check-square"),
                         width = "100%",
                         style=styledbset)
                     ), br(),
                     fluidRow(
                       shinyjs::hidden(
                         actionButton(
                           ns("dbreset"),
                           "Reset Database Connection!",
                           icon("ban"),
                           width = "100%",
                           style=styledbreset)
                       )
                     )
    ),
    fluidRow(
      column(1),
      column(10,
             DT::dataTableOutput(ns("storageSummary"))
      ),
      column(1)
    ),br()
  )
  ####################FIN UI####################################################
}
# server
datastoreSRV <- function(id,
                         home_dir = path.expand("~")) {
  moduleServer(
    id,
    function(input, output, session) {
      #########################################
      ##  Table formats & styles
      #########################################
      ## 1. General
      smTab<-list(dom="t")

      ##  2. Info table (no selection, first column is Names)
      infoTable<-.%>% formatStyle(1,  color = '#FFFFFF',
                                  backgroundColor = '#0d47a1',
                                  fontWeight = 'bold')

      inputTable<-.%>% formatStyle(2,
                                   fontWeight = 'bold',
                                   textAlign = 'center')


      ##  3. View table (no selcetion, all columns the same)
      viewTable<-.%>% formatStyle(1,  color = '#FFFFFF',
                                  backgroundColor = '#33D2FF',
                                  fontWeight = 'bold')


      #################################################################################
      ##              Data Base Set-up
      storemode<-reactiveVal(NULL); shppath<-reactiveVal(NULL); raspath<-reactiveVal(NULL)
      observeEvent(input$datastore, {
        req(input$datastore)
        removeNotification("no_storage")
        if(input$datastore=="1"){
          ################
          # uses postgres

          # Export Storagetype parameter only --> rest after confirmation
          storemode("pg")

        } else if (input$datastore=="2") {
          ################
          # uses local
          # - directories are created in home
          #home_dir <- path.expand("~")

          # Create a directory in the home directory to store the data
          data_dir <- file.path(home_dir)
          if(!dir.exists(data_dir)){
            # !only if not exists
            # 1. Data dir
            dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
          }
          # 2. Data Type Dir
          # 2.1 Shape
          shp_dir <- file.path(data_dir, "shapefiles")
          if(!dir.exists(shp_dir)){
            dir.create(shp_dir, recursive = TRUE, showWarnings = FALSE)
          }
          # 2.2 Raster
          ras_dir <- file.path(data_dir, "rasterfiles")
          if(!dir.exists(ras_dir)){
            dir.create(ras_dir, recursive = TRUE, showWarnings = FALSE)
          }
          # 3. Export paramters
          storemode("local"); shppath(shp_dir); raspath(ras_dir)
          shinyjs::enable(id = "showDBshape")
        }
      }, ignoreInit = T)

      # DB settings-->confirm & check
      DBname<-reactiveVal(NULL); DBhost<-reactiveVal(NULL); DBuser<-reactiveVal(NULL); DBpass<-reactiveVal(NULL)
      observeEvent(input$dbset, {
        stm<-storemode()
        req(stm)
        # check validity
        if(stm=="pg") {
          tab<-tryCatch(
            {writeSFtoDB(listTables = T, user = input$dbuser)},
            error = function(e) {
              shiny::showNotification("Invalid Credentials!",
                                      type = "warning")
              return(NULL)
            }
          )
        }
        # set parameters
        if (!is.null(tab)) {
          # set parameters
          DBname(input$dbname)
          DBhost(input$dbhost)
          DBuser(input$dbuser)
          DBpass(input$dbpass)
          shinyjs::show("dbreset")
          shinyjs::disable("dbset")
        }
      }, ignoreInit = T)

      # DB reset
      observeEvent(input$dbreset, {
        DBname(NULL)
        DBhost(NULL)
        DBuser(NULL)
        DBpass(NULL)
        storemode(NULL)
        shinyjs::hide("dbreset")
        shinyjs::enable("dbset")

      }, ignoreInit = T)

      # Table Summary
      output$storageSummary<-DT::renderDataTable({
        shiny::validate(need(storemode(), message = "Select Storag first!"))
        if(!is.null(storemode()) && storemode()=="local"){
          shinyjs::enable(id = "showDBshape")
          tab<-cbind(c("Storage Mode", "Shape File Storag:", "Raster Storage"),c("Local Directory",shppath(), raspath()))

        } else if(!is.null(storemode()) && storemode()=="pg"){
          shinyjs::enable(id = "showDBshape")
          tab<-cbind(c("Storage Mode", "DB Host:", "DB Name"),c("PostGres-PostGIS",DBhost(), DBname()))
        }
        DT::datatable(tab, smTab, selection = "none", rownames = F,
                      colnames = c("",""),
                      style = "bootstrap") %>% infoTable
      })

      ####################FIN SERVER####################################################
      # return list
      list(
        storemode=storemode,
        shppath=shppath,
        raspath=raspath,
        DBname=DBname,
        DBhost=DBhost,
        DBuser=DBuser,
        DBpass=DBpass
      )

    }
  )
}


# # TESTING ONLY
# ui <- fluidPage(
#   shinyjs::useShinyjs(),
#   datastoreUI("my_modal")
# )
#
# server <- function(input, output, session) {
#   storemode<-reactiveVal(NULL); shppath<-reactiveVal(NULL); raspath<-reactiveVal(NULL)
#   DBname<-reactiveVal(NULL); DBhost<-reactiveVal(NULL); DBuser<-reactiveVal(NULL); DBpass<-reactiveVal(NULL)
#
#   storageSetting <- datastoreSRV("my_modal")
#
#
#   observe({
#     # local store
#     storemode(storageSetting$storemode())
#     shppath(storageSetting$shppath())
#     raspath(storageSetting$raspath())
#     # remote store
#     DBname(storageSetting$DBname())
#     DBhost(storageSetting$DBhost())
#     DBuser(storageSetting$DBuser())
#     DBpass(storageSetting$DBpass())
#     DBname(storageSetting$DBname())
#
#
#   })
#
#   observe({
#     if(!is.null(storemode()) && storemode()=="local"){
#       print(storemode())
#       print(shppath())
#       print(raspath())
#       shinyjs::enable(id = "showDBshape")
#
#     } else if(!is.null(storemode()) && storemode()=="pg"){
#       print(DBname())
#       print(DBhost())
#       print(DBuser())
#       print(DBpass())
#       shinyjs::enable(id = "showDBshape")
#
#     }
#
#   })
#
# }
#
# shiny::shinyApp(ui,server)
