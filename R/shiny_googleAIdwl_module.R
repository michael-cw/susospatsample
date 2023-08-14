#' shiny module for  download of Google AI build up area data
#'
#'
#'
#' @description Modal dialogue to download google AI build up area data with map for selection.
#' Requires boundaries as input and returns POINTS data.table for rasterization and aggregation within polygon.
#'
#' @details uses leaflet mini map selection module from shiny_map_module_leaflet.R
#'
#'
#' @return Returns points data and list of variables for aggregation, adds a column of 1s for simple count
#'
#' @noRd
#' @keywords internal




# module UI
modal_googleai_ui <- function(id, style = "color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1") {
  ns <- NS(id)
  tagList(
    actionButton(ns("showGoogleAIShape"),
                 "Load Google AI Open Building",
                 width = "100%",
                 icon("download"),
                 style=style
    )
  )
}

# module server
modal_googleai_server <- function(id,
                                  shape_boundaries = reactive({ NULL }),
                                  google_ai_score_url = getOption("google_ai_score_url"),
                                  google_ai_score_map_url = getOption("google_ai_score_map_url")) {
  moduleServer(id, function(input, output, session) {
    ## CSS/UI Styles
    styleDwlButton<-c("color: #FFFFFF;  width: 180px;background-color: #1976D2;
                  border-color: #1976D2;
                  margin:0 20% 0 20%;")
    smTabDir<-list(dom="t", pagelength=500, scrollY="250px", scrollcollapse=TRUE, paging=FALSE)

    action_btn_close <-c("color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1")
    styleActButton<-c("color: #FFFFFF; background-color: #7f0000;
                  border-color: #7f0000; margin:0 20% 0 20%;")

    # Reactive value return
    modal_values <- reactiveValues()

    ## 1.1. LOAD from GOOGLE AI
    ## --> Delivers Same Points Data as upload above
    gscore<-reactive({
      data.table::fread(google_ai_score_url, showProgress = F,
                        sep = ",")
    })
    # show the modal when the "Launch modal" button is clicked
    observeEvent(input$showGoogleAIShape, {
      # uses leaflet mini map module!
      ns<-NS(id)
      google_ai_score<-gscore()
      google_ai_score_levels<-stringr::str_subset(names(google_ai_score), "confidence_threshold_[0-9][0-9]%")
      ## ii. Show Modal
      showModal(modalDialog(title =tags$div(
        HTML("<center><font color='#0d47a1'><big>Boundary Files</big></font></center>")),
        mini_ampUI(ns("mapModulePopUP_googleAI")),
        br(),
        fluidRow(
          column(4,br(),
                 actionButton(ns("load_gridFiles"),"Load Selected",
                              style=styleDwlButton)
          ),
          column(4,
                 selectInput(inputId = ns("conf_level"),
                             label = "Apply confidence treshold",
                             choices = c("None", google_ai_score_levels),
                             selected = "None")
          ),
          column(4,br(),
                 actionButton(ns("reset_gridFiles"),"Reset Selection",
                              style=styleDwlButton)
          )
        ),
        footer = tagList(actionButton(ns("close1"),"Close Viewer",
                                      icon("window-close"),
                                      style=action_btn_close)),
        easyClose = T, size = "l"
      ))
    })

    #####################################################################
    ## 1.1.2 GOOGLE AI POINTS DATA

    ## A. Build Map with s2 level 4 grid cells for data selection
    ## A.1. Read full grid from Google directly--> Provided as option in run-up
    googleAIgrid<-reactive({
      AI<-sf::st_read(google_ai_score_map_url)
      AI$FileInGB<-round(AI$size_mb/1024, 2)
        AI<-AI %>% st_transform(st_crs(shape_boundaries))
        AI<-AI %>% st_make_valid()
        AI<-AI[shape_boundaries,]
    })
    # Leaflet mini-map module server
    aiSelFullOuter<-mini_mapServer("mapModulePopUP_googleAI",
                                   polygonLayer = googleAIgrid,
                                   polygonNames = reactive("size_mb"))

    gridPointsSpatial<-reactiveVal(NULL)
    observeEvent(input$load_gridFiles, {
      removeModal()
      tab<-req(googleAIgrid())
      withProgress(message = 'Loading AI points and transforming it into spatial, this may take a while!', value = 0.1,{
        tab %>% sf::st_set_geometry(NULL)
        tab<-data.table::data.table(tab)
        rIDs<-aiSelFullOuter$ID
        tab<-tab[rIDs]
        tile_id<-tab$tile_id
        google_ai_score<-gscore()
        ###############
        ## get score for subset
        if(input$conf_level!="None") {
          google_ai_score_level<-stringr::str_subset(names(google_ai_score), input$conf_level)
          ai_score<-google_ai_score[,.(s2_token, score=get(google_ai_score_level))]
          setkey(ai_score, s2_token)
        }
        tab<-tab %>% dplyr::select(c("tile_id", "tile_url"))
        #tab<-tab[unique(tab$tile_url),]
        if(length(tab$tile_id)==1) {
          tmpFile<-data.table::fread(as.character(tab$tile_url), showProgress = F)

          if(input$conf_level!="None")
            if(tab$tile_id %in% ai_score$s2_token) {
              score<-ai_score[tab$tile_id, score]
              tmpFile<-tmpFile[confidence>=score]
            }

          incProgress(0.5)

        } else if (length(tab$tile_id)>1){
          tmpFile<-list()
          for (i in 1:length(tab$tile_id)) {
            tmpFile[[i]]<-(fread(as.character(tab$tile_url[i]), showProgress = F))
            incProgress(round(0.5/length(tab)))
          }
          tmpFile<-rbindlist(tmpFile, fill = T)
        }

        tmpFile<-st_as_sf(tmpFile,
                          coords = c("longitude", "latitude"),
                          crs = 4326)

        tmpFile$count<-1
        incProgress(0.7)
      })
      tmpFile<-tmpFile[shape_boundaries,]
      gridPointsSpatial(tmpFile)

    })


    # RETURN the modal_values reactiveValues object
    return(gridPointsSpatial)
  })
}
