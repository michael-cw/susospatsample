#' Ui function for Spatial Sampling App
#'
#' @noRd
#' @keywords internal
#'



main_ui <- function(request) {
  fpwww <- system.file("www", package = "susospatsample")
  fluidPage(
    title = "Spatial Sampling Application",
    ##    Title BAR (logo, bg color font color etc)
    useShinyjs(),
    ## shiny alert conditional on version
    if (utils::packageVersion("shinyalert") < 3) shinyalert::useShinyalert(),
    startupModalUI("startupModal"),
    fluidRow(
      column(2, div(
        style = "height:20px; text-align: center;",
        img(src = file.path("www", "logoWBDG.png"))
      )),
      column(10, div(
        style = "background-color:#0d47a1; margin-left:5%; margin-top:0px; margin-bottom:0px;
                                                    height: 60px; padding: 30 0 10 0;",
        h1("Spatial Sampling Application", align = "center", style = "color:#FFFFFF; margin:0 0 0 0;")
      ))
    ),
    ## CUSTOM CSS file
    includeCSS(file.path(fpwww, "styles.css")),
    # uses titlePanel, but title is handled above
    titlePanel(""),
    div(
      id = "resetAll",
      sidebarLayout(
        ##########################################################################
        sidebarPanel(
          width = 3, title = "Input parameters",
          tabsetPanel(
            type = "tabs", id = "Settings", selected = "Admin",
            tabPanel(
              "Sample",
              fluidRow(br(), br()),
              fluidRow(
                column(
                  6,
                  radioButtons("sampType", "Type of Sampling?", c("None", "Random Grid", "Random Cluster"), selected = "None", inline = F)
                ),
                column(
                  6,
                  # after selection --> Stratified confirmation
                  conditionalPanel(
                    "input.sampType=='Random Grid'|input.sampType=='Random Cluster'",
                    radioButtons("strat", "Is the file stratified?", c("Yes", "No"), selected = "No", inline = F)
                  ),
                  # otherwise intro help text
                  conditionalPanel(
                    "input.sampType=='None'",
                    helpText("RANDOM GRID refers to sampling at random from an in-app generated grid or an uploaded raster image in GeoTif format, RANDOM CLUSTER samples randomly from a provide cluster map and/or population data.")
                  )
                )
              ),
              conditionalPanel(
                "input.sampType=='Random Grid'|input.sampType=='Random Cluster'",
                fluidRow(
                  column(
                    6,
                    # cell size only for grid and points NOT raster, raster aggregation is selected bellow
                    conditionalPanel(
                      "(input.sampType=='Random Grid' & input.popUpType!=2 | input.sampType=='Random Grid' & input.popCreate=='No')",
                      numericInput("cell_side", "Cell size in meters",
                        min = 100, max = 5000, value = 1000, step = 100
                      )
                    )
                  ),
                  column(
                    6,
                    conditionalPanel(
                      "input.strat=='Yes'",
                      stratumVariableUI(id = "strVarSel")
                    )
                  )
                ),
                fluidRow(
                  column(
                    6,
                    # Upload population data! --> ATTENTION: when RAND CLUSTER, population data can also be column, then this needs to be NO!!
                    conditionalPanel(
                      "input.sampType=='Random Grid'|input.sampType=='Random Cluster'",
                      radioButtons("popCreate", "Upload population data?",
                        c("Yes", "No"),
                        selected = "No", inline = T
                      )
                    )
                  ),
                  column(
                    6,
                    # Type of population data: Points, Raster, Build Area Polygons (not active yet)
                    conditionalPanel(
                      "input.popCreate=='Yes'",
                      selectInput("popUpType", "How is population data provided",
                        choices = c(
                          "Georeferenced Points" = 1,
                          "Raster Image" = 2,
                          "Build-Up Area" = 3
                        ),
                        selected = 2
                      )
                    )
                  )
                ),
                fluidRow(
                  column(width = 2),
                  column(
                    width = 8,
                    # MODIFY Button: when grid, creation of grid, when cluster, extraction of population, MODIFY button not active when
                    #                population data is include in shape file
                    conditionalPanel(
                      "input.sampType=='Random Grid'|input.sampType=='Random Cluster'",
                      shinyjs::disabled(
                        actionButton("modify", "Modify the data set",
                          width = "100%",
                          icon("sync"),
                          style = "color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1"
                        )
                      )
                    )
                  ),
                  column(width = 2)
                ),
                br(),
                fluidRow(
                  column(width = 2),
                  column(
                    width = 8,
                    # SAMPLING Button: usually only active AFTER frame is created, except for clustern when population is included
                    actionButton("genSamp", "Create Sample Population",
                      width = "100%",
                      icon("sync"),
                      style = "color: #FFFFFF;background-color: #0d47a1; border-color: #0d47a1"
                    )
                  ),
                  column(width = 2)
                ),
                fluidRow(br(), br()),
                # BOUNDARY FILE INPUT
                fluidRow(
                  column(3),
                  column(
                    6,
                    h3("Area Boundary files")
                  ),
                  column(3)
                ),
                fluidRow(
                  column(
                    width = 6,
                    conditionalPanel(
                      "input.sampType=='Random Grid'",
                      # GADM 3 is included https://gadm.org/data.html
                      selectizeInput("gadmISO3_sel",
                        "Country Boundaries",
                        choices = NULL,
                        options = list(
                          placeholder = "Requires Points Data",
                          onInitialize = I('function() { this.setValue(""); }')
                        )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    # Shape File Upload
                    fileInput("new_shape", "Upload Shapefile",
                      multiple = F,
                      accept = (c("application/zip", ".zip"))
                    )
                  )
                ),
                #  POPULATION DATA INPUT
                conditionalPanel(
                  "input.popCreate=='Yes'",
                  div(
                    id = "popData",
                    fluidRow(
                      column(3),
                      column(
                        6,
                        h3("Population Data")
                      ),
                      column(3)
                    ),
                    conditionalPanel(
                      "input.popUpType==1",
                      fluidRow(
                        column(width = 2),
                        column(
                          width = 8,
                          zipFileInput_ui("popPointUpload",
                            "Upload Points Data",
                            accept = c(
                              "application/zip", ".zip",
                              "application/gzip", ".gz",
                              "application/x-gzip",
                              "x-zip-compressed"
                            )
                          ),
                          helpText("Only .zip compressions are accepted by the application.File must be packed as single file with one column named 'latitude', and one column named 'longitude', names are case sensitive.")
                        ),
                        column(width = 2)
                      ),
                      h3("OR USE",
                        style = "color: #0d47a1; text-align: center;"
                      ),
                      div(
                        shiny::HTML('<a href="https://sites.research.google/open-buildings/" target="_blank"> Open Buildings Website (opens in new tab!)</a>'),
                        style = "text-align: center;"
                      ),
                      br(),
                      fluidRow(
                        column(width = 2),
                        column(
                          width = 8,
                          modal_googleai_ui("gridPoints")
                        ),
                        column(width = 2)
                      ), br(),
                      conditionalPanel(
                        "input.sampType=='Random Grid'",
                        fluidRow(
                          column(1),
                          column(
                            8,
                            radioButtons(
                              inputId = "agg_var_type_point",
                              label = "Agreggation Type",
                              choices = c("Mean", "Total", "SD"),
                              selected = "Mean", inline = T
                            )
                          ),
                          column(3)
                        ),
                        fluidRow(
                          column(1),
                          column(
                            10,
                            selectizeInput(
                              inputId = "agg_var_sel_point",
                              label = "Select variable for Aggregation",
                              choices = NULL,
                              options = list(
                                placeholder = "Requires Points Data",
                                onInitialize = I('function() { this.setValue(""); }')
                              )
                            )
                          ),
                          column(1)
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.popUpType==2",
                      fluidRow(
                        column(width = 2),
                        column(
                          width = 8,
                          fileInput("pop_file_raster", "Upload Raster Data",
                            multiple = F,
                            accept = (c(
                              "application/zip", ".zip",
                              "x-zip-compressed"
                            ))
                          )
                        ),
                        column(width = 2)
                      ),
                      fluidRow(
                        column(width = 2),
                        column(
                          width = 8,
                          helpText("Raster data must be in Lat Long (decimal) coordinates.")
                        ),
                        column(width = 2)
                      ),
                      fluidRow(
                        column(width = 1),
                        column(
                          width = 5,
                          radioButtons("catRaster",
                            "Is the raster categorical?",
                            c("Yes", "No"),
                            inline = T,
                            selected = "No"
                          )
                        ),
                        column(
                          width = 5,
                          numericInput("aggregateRaster", "Aggregate Raster?",
                            value = 1, step = 1, min = 1, max = 10
                          )
                        ),
                        column(width = 1)
                      ), br(), br(),
                      fluidRow(
                        column(width = 2),
                        column(
                          width = 8,
                          actionButton("showDBraster",
                            "Show Existing Raster",
                            width = "100%",
                            icon("sync"),
                            style = "color: #FFFFFF;background-color: #0d47a1; border-color: #0d47a1"
                          )
                        ),
                        column(width = 2)
                      ), br(), br()
                    ),
                    conditionalPanel(
                      "input.popUpType==3",
                      fluidRow(
                        column(width = 2),
                        column(
                          width = 8,
                          fileInput("pop_file_build", "Upload Build Up Data",
                            multiple = F,
                            accept = (c(
                              "application/zip", ".zip",
                              "application/gzip", ".gz",
                              "application/x-gzip",
                              "x-zip-compressed"
                            ))
                          ),
                          helpText("Only .gz compressions are accepted by the application. File must be packed as single file, with WKT geometry column")
                        ),
                        column(width = 2)
                      )
                    )
                  )
                ),
                fluidRow(
                  column(width = 2),
                  column(
                    width = 8,
                    actionButton("showDetails",
                      "Show Input Details",
                      width = "100%",
                      icon("sync"),
                      style = "color: #FFFFFF;background-color: #0d47a1; border-color: #0d47a1"
                    )
                  ),
                  column(width = 2)
                ), br(), br(),
                fluidRow(
                  column(width = 2),
                  column(
                    width = 8,
                    actionButton("showDBshape",
                      "Show Existing Shape",
                      width = "100%",
                      icon("database"),
                      style = "color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1"
                    )
                  ),
                  column(width = 2)
                )
              )
            ),
            tabPanel(
              "Sample 2nd Stage",
              fluidRow(br(), br()),
              modal_spsample2stage_ui("my_modal")
            ),
            tabPanel("Admin",
              icon = icon("toolbox"),
              modal_createbasemap_provider_ui("mapfordwl"),
              br(),
              # Storage Setting Module
              datastoreUI("setstorage"),
              br()
            )
          )
        ),

        ##########################################################################
        ## MAIN PANEL
        mainPanel(
          fluidPage(
            fluidRow(
              column(
                8,
                # Map Panel
                conditionalPanel(
                  "(input.sampType!='None' | input.Settings=='Sample 2nd Stage') & input.Settings!='Admin'",
                  # mapModuleUI("baseMap", height = "1080px")
                  shiny::uiOutput("MAP_UI")
                ),
                # Admin Panel
                conditionalPanel(
                  "input.Settings=='Admin'",
                  mapadminUI("susomapassign")
                )
              ),
              column(
                4,
                tabsetPanel(
                  id = "tables",
                  tabPanel(
                    value = "base", title = "FRAME", width = 7,
                    fluidRow(br()),
                    conditionalPanel(
                      "input.popCreate=='Yes'",
                      fluidRow(
                        column(4),
                        column(4),
                        column(4, shinyjs::hidden(
                          actionButton("viewRaster", "Show raster image",
                            icon("eye"),
                            style = "color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1"
                          )
                        ))
                      )
                    ),
                    fluidRow(
                      column(1),
                      column(
                        10,
                        DT::dataTableOutput("popInfoTab", height = "540px")
                      ),
                      column(1)
                    )
                  ),
                  tabPanel(
                    value = "sample", title = "SAMPLE",
                    fluidRow(
                      column(1),
                      column(11,
                        DT::dataTableOutput("sample_table"),
                        height = "540px"
                      )
                    ), br(), br(),
                    fluidRow(
                      column(4),
                      column(
                        6,
                        hidden(
                          div(
                            id = "div_samplingreport",
                            dwl_reportUI("samplingReport")
                          )
                        )
                      ),
                      column(2)
                    )
                  ),
                  tabPanel(
                    value = "ressources", title = "SURVEY SOLUTIONS",
                    br(),
                    fluidRow(
                      column(4),
                      column(
                        4,
                        h3("Survey Resources",
                          style = "color: #0d47a1;text-align: center; width: 200px"
                        )
                      ),
                      column(4)
                    ),
                    fluidRow(
                      column(1),
                      column(
                        10,
                        helpText("This section allows you to generate and download the geospatial ressources required to conduct your survey with Survey Solutions. If you already have a survey solutions server, please provide your API user credentials in the Admin section, so the files can be transferred directly.")
                      ),
                      column(1)
                    ),
                    br(),
                    fluidRow(
                      column(4),
                      column(
                        6,
                        conditionalPanel(
                          "(input.sampType=='Random Cluster' | input.sampType== 'Random Grid')",
                          dwl_dataUI("frame_download",
                            label = "Download Frame"
                          )
                        )
                      ),
                      column(4)
                    ),
                    conditionalPanel(
                      "input.conf3==1",
                      fluidRow(
                        column(4),
                        column(
                          4,
                          h4("Map Tiles",
                            style = "color: #0d47a1; text-align: center; width: 180px"
                          )
                        ),
                        column(4)
                      ),
                      fluidRow(
                        column(4),
                        column(
                          4,
                          modal_createbasemap_ui("mapfordwl")
                        ),
                        column(4)
                      ),
                      br(),
                      fluidRow(
                        column(4),
                        column(
                          4,
                          zipFileDwl_ui("basemap", label = "Download Tiles")
                        ),
                        column(4)
                      ),
                      br(),
                      fluidRow(
                        column(4),
                        column(
                          4,
                          h4("Area Boundaries",
                            style = "color: #0d47a1; text-align: center; width: 180px"
                          )
                        ),
                        column(4)
                      ),
                      fluidRow(
                        column(4),
                        column(
                          6,
                          modal_createshape_ui("shapesfordwl")
                        ),
                        column(4)
                      ), br(),
                      fluidRow(
                        column(4),
                        column(
                          6,
                          zipFileDwl_ui("shapeboundaries", label = "Download Boundaries")
                        ),
                        column(2)
                      ),
                      br(),
                      fluidRow(
                        column(4),
                        column(
                          4,
                          h4("Sampling Weights",
                            style = "color: #0d47a1;text-align: center; width: 180px"
                          )
                        ),
                        column(4)
                      ),
                      fluidRow(
                        column(4),
                        column(
                          6,
                          dwl_dataUI("sample_download",
                            label = "Download Sample"
                          )
                        ),
                        column(2)
                      )
                    )
                  ),
                  tabPanel(
                    value = "admin", title = "ADMIN",
                    div(
                      id = "adminrightpan",
                      mapadminUI2("susomapassign")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  ################################################################################################################################
}
