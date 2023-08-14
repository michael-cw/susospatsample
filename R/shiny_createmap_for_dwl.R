#' shiny module UI for  the creation of basemaps for download or api
#'
#' @description Map creation UI
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_create_basemap
#' @noRd
#'

modal_createbasemap_ui <- function(id,
                                   style = "color: #FFFFFF; width: 180px;background-color: #1976D2;border-color: #0d47a1") {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    actionButton(ns("showMapModal"),
                 "Create Basemaps",
                 #width = "100%",
                 icon("map"),
                 style=style
    )
  )
}

#' shiny module UI for  the creation of basemaps for download or api
#'
#' @description Map Provider UI
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_create_basemap
#' @noRd
#'
#'
modal_createbasemap_provider_ui <- function(id,
                                            style = "color: #FFFFFF; width: 180px;background-color: #1976D2;border-color: #0d47a1") {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(h4("Basemap Service"), style = "text-align: center;"),
    fluidRow(
      br(),
      selectizeInput(
        ns("base_provider"),
        "Map API",
        choices = c("ESRI World Imagery" = 1,
                    "Open Street Map (OSM)" = 2,
                    "Mapbox" = 3,
                    "Bing" = 4,
                    "ESRI Tile Package" = 5),
        options = list(
          placeholder = 'Select Provider!',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    ),
    br(),
    # Bing & Mapbox
    conditionalPanel(sprintf("input['%s'] == 3 | input['%s'] == 4", ns("base_provider"), ns("base_provider")),
                     fluidRow(
                       passwordInput(ns("base_key"),
                                     "For MapDeck and Bing please provide API key!",
                                     placeholder = "API Key")
                     )
    ),
    # ESRI map server
    conditionalPanel(sprintf("input['%s'] == 5", ns("base_provider")),
                     fluidRow(
                       column(6,
                              textInput(ns("arc_service"),
                                        "Service URL:",
                                        value = "https://tiledbasemaps.arcgis.com/arcgis/rest/services/World_Imagery/MapServer")
                       ),
                       column(6,
                              textInput(ns("arc_portal"),
                                        "Portal URL:",
                                        value = "https://www.arcgis.com/sharing/rest/generateToken")
                       )
                     ),
                     fluidRow(
                       column(6,
                              textInput(ns("arc_user"),
                                        "User Name:",
                                        placeholder = "ArcGis User Name")
                       ),
                       column(6,
                              passwordInput(ns("arc_pw"),
                                            "Password:",
                                            placeholder = "ArcGis User Password")
                       )
                     )
    ),
    br(),
    # OSM & ESRI (no credentials)
    conditionalPanel(sprintf("input['%s'] != ''", ns("base_provider")),
                     fluidRow(actionButton(ns("base_set"),
                                           "Confirm Basemap Service!",
                                           icon("check-square"), width = "100%",
                                           style=styleActButtonActivate)
                     ),
                     fluidRow(
                       column(1),
                       column(10,
                              DT::dataTableOutput(ns("baseMapSummary"))
                       ),
                       column(1)
                     ),br(),
                     fluidRow(
                       shinyjs::hidden(
                         actionButton(ns("base_reset"),
                                      "Reset Basemap Service!",
                                      icon("ban"), width = "100%",
                                      style="color: #FFFFFF;background-color: #7f0000;border-color: #7f0000")
                       )
                     )
    )
  )
}


#' shiny module server for  the creation of basemaps for download or api
#'
#' @description Map Provider server
#'
#'
#' @rdname mod_create_basemap
#' @noRd
#'
modal_createbasemap_server <- function(id,
                                       fpp = path.expand(file.path("~", "spatsample", "basemaps_tif")),
                                       fppTPK = path.expand(file.path("~", "spatsample", "basemaps_tpk")),
                                       fppTPKerror = path.expand(file.path("~", "spatsample", "mapError")),
                                       tpkLoadError = file.path(fppTPKerror, "loadTPK.txt"),
                                       tpkDWLError = file.path(fppTPKerror, "dwl.txt"),
                                       tpkCPError = file.path(fppTPKerror, "copy.txt"),
                                       shape_boundaries = reactive({ NULL }),
                                       sampType=reactive({"Random Grid"}),
                                       sample_seed = reactive({floor(runif(1, 1000,9999))})) {
  moduleServer(id, function(input, output, session) {


    # table style
    smTab<-list(dom="t")
    infoTable<-.%>% formatStyle(1,  color = '#FFFFFF',
                                backgroundColor = '#0d47a1',
                                fontWeight = 'bold')
    ## CSS/UI Styles
    styleDwlButton<-c("color: #FFFFFF;  width: 180px;background-color: #1976D2;
                  border-color: #1976D2;
                  margin:0 20% 0 20%;")
    smTabDir<-list(dom="t", pagelength=500, scrollY="250px", scrollcollapse=TRUE, paging=FALSE)

    action_btn_close <-c("color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1")
    styleActButton<-c("color: #FFFFFF; background-color: #7f0000;
                  border-color: #7f0000; margin:0 20% 0 20%;")

    # Reactive Values function for return
    TPKpath<-reactiveVal(NULL)

    # PROVIDER & CREDENTIALS
    # selecting
    baseMapService<-eventReactive(input$base_set, {
      req(input$base_provider)
      switch (input$base_provider,
              "1" = "esri",
              "2" = "osm",
              "3" = "mapbox",
              "4" = "bing",
              "5" = "esritpk"
      )
    })

    # disable/hide buttons
    observeEvent(input$base_set, {
      shinyjs::show("base_reset")
      shinyjs::disable("base_set")
      shinyjs::disable("base_provider")
      resetVal(FALSE)
      # all others
      if(input$base_provider %in% c(1,2,3,4)) {
        shinyjs::hide("base_key")
        shinyjs::hide("arc_service")
        shinyjs::hide("arc_portal")
        shinyjs::hide("arc_user")
        shinyjs::hide("arc_pw")
      } else if (input$base_provider==5) {
        # esri
        shinyjs::disable("arc_service")
        shinyjs::disable("arc_portal")
        shinyjs::hide("arc_user")
        shinyjs::hide("arc_pw")
        shinyjs::hide("base_key")

      }
    }, ignoreInit = T)

    # get credentials
    baseMapCredentials<-eventReactive(input$base_set, {
      if(input$base_provider %in% c(1,2)) {
        tab<-"Not required!"
      } else if(input$base_provider %in% c(3,4)) {
        tab<-ifelse((input$base_key==""), "Not Provided!", "Provided!")
      } else if (input$base_provider==5) {
        # esri
        tab<-ifelse((input$arc_user==""|input$arc_pw==""), "Not All Credentials Provided!", "User & Password Provided!")

      }
      return(tab)

    })

    baseMapCredentialsFull<-eventReactive(input$base_set, {
      if(input$base_provider %in% c(1,2)) {
        tab<-"Not required!"
      } else if(input$base_provider %in% c(3,4)) {
        tab<-ifelse((input$base_key==""), "Not Provided!", "Provided!")
      } else if (input$base_provider==5) {
        # esri
        tab<-ifelse((input$arc_user==""|input$arc_pw==""), "Not All Credentials Provided!", "User & Password Provided!")

      }
      return(tab)

    })

    # reset provider
    resetVal<-reactiveVal(FALSE)
    observeEvent(input$base_reset,{
      resetVal(TRUE)
      # hide the reset button
      shinyjs::hide("base_reset")
      # enable the selction and confirm button
      shinyjs::enable("base_set")
      shinyjs::enable("base_provider")
      # enable other buttons
      if(input$base_provider %in% c(1,2,3,4)) {
        shinyjs::show("base_key")
        shinyjs::show("arc_service")
        shinyjs::show("arc_portal")
        shinyjs::show("arc_user")
        shinyjs::show("arc_pw")
        ## RESET KEY FIELD
        updateTextInput(session = session,
                        inputId = "base_key",
                        #label = "For MapDeck and Bing your have to provide your own API key!",
                        value = ""
                        #placeholder = "API Key"
        )
      } else if (input$base_provider==5) {
        # esri
        shinyjs::enable("arc_service")
        shinyjs::enable("arc_portal")
        shinyjs::show("arc_user")
        shinyjs::show("arc_pw")
        shinyjs::show("base_key")
        ## RESET KEY FIELD
        updateTextInput(session = session,
                        inputId = "arc_pw",
                        #label = "For MapDeck and Bing your have to provide your own API key!",
                        value = ""
                        #placeholder = "API Key"
        )

      }

      ## RESET KEY FIELD
      updateTextInput(session = session,
                      inputId = "arc_pw",
                      #label = "For MapDeck and Bing your have to provide your own API key!",
                      value = ""
                      #placeholder = "API Key"
      )
      ## ii. set input to NULL
      baseMapServiceSet(NULL)
    }, ignoreInit = T)
    # set value
    baseMapServiceSet<-reactiveVal(NULL)
    observe({
      shiny::validate(need(baseMapService(), message = F))
      tab<-baseMapService()
      baseMapServiceSet(tab)
    })
    # summary table
    output$baseMapSummary<-DT::renderDataTable({
      shiny::validate(need(baseMapServiceSet(), message = F))
      tab<-cbind(c("Basemap Service", "Credentials:"),c(baseMapService(),baseMapCredentials()))
      DT::datatable(tab, smTab, selection = "none", rownames = F,
                    colnames = c("",""),
                    style = "bootstrap") %>% infoTable
    })



    # CREATION OF BASEMAPS
    # show the modal when the "Launch modal" button is clicked
    modaltitle<-reactive({
      #req(input$base_provider)
      if(input$base_provider==1) {
        mod_title<-"This will create the ESRI World Imagery (.tif) for the Survey Solutions Interviewer application:"
      } else if(input$base_provider==2) {
        mod_title<-"This will create the OSM basemap (.tif) for the Survey Solutions Interviewer application:"
      } else if(input$base_provider==3) {
        mod_title<-"This will create the Mapbox basemap (.tif) for the Survey Solutions Interviewer application:"
      } else if(input$base_provider==4) {
        mod_title<-"This will create the Bing Maps basemap (.tif) for the Survey Solutions Interviewer application:"
      } else if(input$base_provider==5) {
        mod_title<-"This will create the ESRI Tile Package (.tpk) for the Survey Solutions Interviewer application:"
      }

      return(mod_title)
    })

    observeEvent(input$showMapModal, {
      ns<-NS(id)
      if((input$base_provider=="" || resetVal())) {
        shiny::showNotification("Select basemap provider first!", type = "warning")
        req(FALSE)
      }
      req(modaltitle())

      # text elements
      tpkInfohelptext<-c("Starting this process will create the map tiles for the offline maps used in Survey
                   Solutions for the sample area. This process can take several hours for large areas. If an area exceeds the treshold
                   of 150000 tiles, the area is split up. This process is continued, until each of the tile
                   packages is below the treshold. The maximum zoom level is 18.")
      mapSplitHelpText<-c("This will create a single file with a map for all segments. This approach is only
                    suitable if the target population area is very small. Otherwise we recommend
                    to split the segments.")
      indivcluidtext<-c("You can either choose to use a system generated cluster id, or specify your own variable for the cluster id.
                  Please be aware, that the variable values must be unique across the whole frame.")
      indivcluidYestext<-c("You can either use your own cluster ID, or use a system generated one.
                            If you use your own ID, then this ID must be unique across the whole frame.")
      subsegtext<-c("You can create similar sized sub-segments for cluster,
                                    in case you consider sub-sampling
                                    within a grid cell or to facilitate fieldwork.")

      # generate ID choices for cluster
      if(sampType()=="Random Cluster") {
        req(shape_boundaries())
        tab<-names(shape_boundaries())
        idchoices<-setNames(tab, tab)

        ## ii. Show Modal
        showModal(modalDialog(title =tags$div(
          HTML(glue::glue("<center><font color='#7f0000'><big>{title}</big></font></center>", title=modaltitle()))),
          tagList(
            fluidPage(
              ## A. Survey Solutions
              fluidRow(
                column(6,
                       radioButtons(ns("split_segments"),
                                    label = "Split Sample into individual Segments?",
                                    choices = c("Yes", "No"),
                                    selected = 'No', inline = T
                       ), br(),
                       conditionalPanel(sprintf("input['%s'] == 5", ns("base_provider")),
                                        numericInput(ns("map.level"),
                                                     "Zoom Level (1-19)?",
                                                     value = 18,
                                                     width = "100%", min = 1, max = 19), br(),
                                        helpText(tpkInfohelptext)
                       ),
                       conditionalPanel(sprintf("input['%s'] == 'Yes'", ns("split_segments")),
                                        numericInput(
                                          inputId = ns("bound_segments"),
                                          label = "Create Sub-Segments within Segments?",
                                          value = 4, step = 1
                                        ),
                                        helpText(subsegtext)
                       )
                ),
                column(6,
                       conditionalPanel(sprintf("input['%s'] == 'No'", ns("split_segments")),
                                        helpText(mapSplitHelpText)
                       ),
                       conditionalPanel(sprintf("input['%s'] == 'Yes'", ns("split_segments")),
                                        radioButtons(ns("cluster_id"),
                                                     label = "Use individual cluster ID for file name?",
                                                     choices = c("Yes", "No"),
                                                     selected = "No", inline = T
                                        ), br(),
                                        conditionalPanel(sprintf("input['%s'] == 'No'", ns("cluster_id")),
                                                         helpText(indivcluidtext)
                                        ),
                                        conditionalPanel(sprintf("input['%s'] == 'Yes'", ns("cluster_id")),
                                                         selectizeInput(
                                                           inputId = ns("cluster_id_sel"),
                                                           label = "Select variable for cluster ID:",
                                                           choices = idchoices
                                                         ),
                                                         helpText(indivcluidYestext)
                                        )
                       )
                )
              )
            )
          ),
          footer = tagList(
            fluidRow(
              column(10,
                     actionButton(ns("create_raster"),"Initate Basemap Creation?",
                                  width = "100%",
                                  icon("play"),
                                  style="color: #FFFFFF; background-color: #7f0000; border-color: #7f0000")
              ),
              column(2,
                     actionButton(ns("cancel"),"Cancel",
                                  style=action_btn_close)
              )
            )
          ),
          easyClose = T, size = "l"
        ))
      } else if(sampType()=="Random Grid"){
        ## ii. Show Modal
        showModal(modalDialog(title =tags$div(
          HTML(glue::glue("<center><font color='#7f0000'><big>{title}</big></font></center>", title=modaltitle()))),
          tagList(
            fluidPage(
              ## A. Survey Solutions
              fluidRow(
                column(6,
                       radioButtons(ns("split_segments"),
                                    label = "Split Sample into individual Segments?",
                                    choices = c("Yes", "No"),
                                    selected = 'No', inline = T
                       ), br(),
                       conditionalPanel(sprintf("input['%s'] == 5", ns("base_provider")),
                                        numericInput(ns("map.level"),
                                                     "Zoom Level (1-19)?",
                                                     value = 18,
                                                     width = "100%", min = 1, max = 19), br(),
                                        helpText(tpkInfohelptext)
                       )
                ),
                column(6,
                       conditionalPanel(sprintf("input['%s'] == 'No'", ns("split_segments")),
                                        helpText(mapSplitHelpText)
                       ),
                       conditionalPanel(sprintf("input['%s'] == 'Yes'", ns("split_segments")),
                                        selectizeInput(
                                          inputId = ns("bound_segments"),
                                          label = "Create Sub-Segments? (Single Sided)",
                                          choices = c("1", "2", "3", "4")
                                        ),
                                        helpText(subsegtext)
                       )
                )
              ),
              br(),
              ## 3. GRID CELL SELECTION
              fluidRow(column(1),
                       column(10),
                       column(1)
              )
            )
          ),
          footer = tagList(
            fluidRow(
              column(10,
                     actionButton(ns("create_raster"),"Initate Basemap Creation?",
                                  width = "100%",
                                  icon("play"),
                                  style="color: #FFFFFF; background-color: #7f0000; border-color: #7f0000")
              ),
              column(2,
                     actionButton(ns("cancel"),"Cancel",
                                  style=action_btn_close)
              )
            )
          ),
          easyClose = T, size = "l"
        ))
      }

    })
    #observe({print(input$cluster_id_sel)})
    # close modal (no action)
    observeEvent(input$cancel, {
      removeModal()
    })

    ## generat files
    observeEvent(input$create_raster, {
      samp_raster_shp<-shape_boundaries()
      req(samp_raster_shp, baseMapCredentialsFull())
      # check long lat for input
      if(!sf::st_is_longlat(samp_raster_shp)) samp_raster_shp<-samp_raster_shp %>% st_transform(4326)

      ################################################################
      ##  1. Check the size, and decrease if required
      withProgress(message = 'Basemap generation in progress',
                   detail = 'This may take a while...', value = 0, {
                     # samp_raster_shp<-st_as_sf(samp_raster_shp)
                     #samp_raster_shp<-splitShapTile(samp_raster_shp, zoomMax = input$map.level)
                     if (baseMapService()=="esritpk") {
                       # check tpk file storage
                       if(!dir.exists(fppTPK)) {
                         dir.create(fppTPK, recursive = T)
                       }

                       # check error log storage
                       if(!dir.exists(fppTPKerror)) {
                         dir.create(fppTPKerror, recursive = T)
                       }
                       incProgress(0.2)
                       ################################################################
                       ##  2. Load Files
                       ## server and creds!!!!!!!!!!

                       ML<-paste0("17-", input$map.level)

                       if(input$split_segments=="No"){
                         areaName<-paste0("area_map")
                         fn<-file.path(fppTPK, paste0(areaName, "_id_", 1, ".tpk"))
                         if(file.exists(fn)) {
                           tmpFile<-fn
                           incProgress(0.8/length(st_geometry(samp_raster_shp)))
                           return(tmpFile)
                         }

                         # Create Map
                         try(
                           {TPKlink<-loadTPK_SF(input.shape = samp_raster_shp, mapLEVELS = "1-19")},
                           silent = F,
                           outFile = file(tpkLoadError, "at")
                         )
                         tmpFile<-fn
                         try(
                           {download.file(url = TPKlink, destfile = tmpFile, method = "auto", quiet = T)}, silent = F,
                           outFile = file(tpkDWLError, "at")
                         )
                         incProgress(0.8/length(st_geometry(samp_raster_shp)))
                         # create zip file
                         zfile<-tempfile("mapfordownload", fileext = ".zip")
                         zip::zip(zipfile=zfile, files=tmpFile, mode = "cherry-pick")
                         # return path for zip file
                         TPKpath(zfile)

                       } else if (input$split_segments=="Yes") {
                         tmpFile<-character(length = length(st_geometry(samp_raster_shp)))

                         for (i in seq_along(st_geometry(samp_raster_shp))) {
                           # File Name
                           # areaName<-samp_raster_shp[i, "GRIDID"] %>%
                           #   select(GRIDID) %>%
                           #   st_set_geometry(NULL)
                           # areaName<-areaName[1,"GRIDID"]
                           area<-samp_raster_shp[i,]
                           areaName<-ifelse(
                             ## for grid use grid codes!
                             sampType()== "Random Grid",
                             paste0("seg_",area$GRIDID),
                             ifelse(
                               input$cluster_id=="No",
                               paste0("seg_",area$CID),
                               paste0("seg_",area[[input$cluster_id_sel]])
                             )
                           )
                           fn<-file.path(fppTPK, paste0(areaName, "_id_", i, ".tpk"))
                           # check if file exists, if it exists, store path and next
                           if(file.exists(fn)) {
                             tmpFile[i]<-fn
                             incProgress(0.8/length(st_geometry(samp_raster_shp)))
                             next()
                           }
                           # Create Map
                           try(
                             {TPKlink<-loadTPK_SF(input.shape = area, mapLEVELS = "1-19")},
                             silent = F,
                             outFile = file(tpkLoadError, "at")
                           )
                           tmpFile[i]<-fn
                           try(
                             {download.file(url = TPKlink, destfile = tmpFile[i], method = "auto", quiet = T)}, silent = F,
                             outFile = file(tpkDWLError, "at")
                           )
                           # try(
                           #   {file.copy(tmpFile[i],file.path(fppTPK, paste0(areaName, "_id_", i, ".tpk")))}, silent = F,
                           #   outFile = file(tpkCPError, "at")
                           # )
                           incProgress(0.8/length(st_geometry(samp_raster_shp)))
                         }

                         CHECKtmpFile<<-tmpFile

                         # create zip file
                         zfile<-tempfile("mapfordownload", fileext = ".zip")
                         zip::zip(zipfile=zfile, files=tmpFile, mode = "cherry-pick")
                         # return path for zip file
                         TPKpath(zfile)
                       }
                     } else {
                       # Check/create directory
                       if(!dir.exists(fpp)) {
                         dir.create(fpp, recursive = T)
                       }
                       # Check for split
                       if(input$split_segments=="No"){
                         thp<-samp_raster_shp
                         areaName<-paste0("area_map")
                         thp$areaName<-areaName
                         # Write to file
                         suppressWarnings(
                           check<-tryCatch(
                             {getStaticMapAsRaster(shape = thp, byShape = F,
                                                   file_path = fpp,
                                                   mapservice = baseMapService(),
                                                   singleMap = T,
                                                   name_var = "areaName")},
                             error = function(e) return("error happens here!")
                           )
                         )
                         req(check)
                         zfile<-tempfile("mapfordownload", fileext = ".zip")
                         zip::zip(zipfile=zfile, files=check, mode = "cherry-pick")
                         incProgress(0.8/length(st_geometry(samp_raster_shp)))
                         # return file path for zip for download
                         TPKpath(zfile)
                       } else if (input$split_segments=="Yes") {
                         tmpFile<-character(length = length(st_geometry(samp_raster_shp)))

                         for (i in seq_along(st_geometry(samp_raster_shp))) {
                           thp<-samp_raster_shp[i,]
                           # File name
                           areaName<-ifelse(
                             ## for grid use grid codes!
                             sampType()== "Random Grid",
                             paste0("seg_",thp$GRIDID),
                             ifelse(
                               input$cluster_id=="No",
                               paste0("seg_",thp$CID),
                               paste0("seg_",thp[[input$cluster_id_sel]])
                             )
                           )
                           # add areaName to data
                           thp$GRIDID<-areaName
                           # create path
                           fn<-file.path(fpp, paste0("seg_",areaName, "_ALL", ".tif"))
                           # check if file exists, if it exists, store path and next
                           if(file.exists(fn)) {
                             tmpFile[i]<-fn
                             incProgress(0.8/length(st_geometry(samp_raster_shp)))
                             next()
                           }
                           if(sampType()== "Random Grid" && as.numeric(input$bound_segments)>0) {
                             thp<- sf::st_sf(
                               geometry=sf::st_make_grid(thp, n = as.numeric(input$bound_segments)),
                               GRIDID = areaName
                             )
                           } else if(sampType()== "Random Cluster" && as.numeric(input$bound_segments)>0) {
                             req(sample_seed())
                             suppressWarnings(
                               thp<- split_poly(thp, as.numeric(input$bound_segments), sample_seed())
                             )
                             ## add label
                             thp$label<-seq_along(st_geometry(thp))
                           }

                           suppressWarnings(
                             check<-tryCatch(
                               {getStaticMapAsRaster(shape = thp,
                                                     file_path = fpp,
                                                     mapservice = baseMapService(),
                                                     singleMap = T,
                                                     name_var = "GRIDID")},
                               error = function(e) return(e)
                             )
                           )
                           incProgress(0.8/length(st_geometry(samp_raster_shp)))
                         }
                         dirpath<-dirname(check[[1]])
                         fs<-list.files(dirpath, full.names = T, pattern = "_ALL.tif")
                         zfile<-tempfile("mapfordownload", fileext = ".zip")
                         zip::zip(zipfile=zfile, files=fs, mode = "cherry-pick")
                         # return path for zip file
                         TPKpath(zfile)
                       }
                     }
                   })

    }, ignoreInit = T)

    # return the file paths as reactiveValues object
    return(TPKpath)
  })
}

