#' shiny module for 2nd stage spatially balanced sampling using spsurvey
#'
#'
#'
#' @description Module to create the 2nd stage sample from listing units for each cluster. Available sampling modes are:
#' - spatial random sample
#' - spatially balanced sample
#' - spatially balanced sample with minimum distance
#'
#' @return Returns points data and list of variables for aggregation, adds a column of 1s for simple count
#'
#' @noRd
#' @keywords internal




# MODUL UI
modal_spsample2stage_ui <- function(id, style = "color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1") {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(4,
               zipFileInput_ui(ns("userupload"), "Upload Data", accept = "text/csv")
        ),
        column(4,
               shinyjs::disabled(
                 selectInput(inputId = ns("clustervar"),
                             label = "Select Cluster Variable",
                             choices = c("Load Data First!")
                 )
               )
        ),
        column(4,
               numericInput(inputId = ns("sampSEED"),label = "Set Seed",
                            value = floor(runif(1, 1000,9999)),
                            min=0, step = 1)
        )
      ),
      fluidRow(
        column(4,
               selectInput(inputId = ns("samplemode"),
                           label = "Select Sampling Mode",
                           choices = c("Balanced",
                                       "Balanced w. minimum distance",
                                       "Random")
               )
        ),
        column(4,
               shinyjs::disabled(
                 numericInput(inputId = ns("mindist"),label = "Minimum distance in m", step = 1, value = 10)
               )
        ),
        column(4,
               numericInput(inputId = ns("n_sample"),label = "Cluster Sample Size", step = 1, value = 12)
        )
      ),
      fluidRow(
        column(6,
               shinyjs::disabled(
                 actionButton(ns("createSample"),
                              "Generate Balanced Sample",
                              icon("gears"), width = "100%",
                              style=style
                 )
               )
        ),
        column(6,
               dwl_dataUI(ns("dwlsample"), style = style)
        )
      )
    )
  )
}

# MODUL SERVER
modal_spsample2stage_server <- function(id, shape_boundaries = reactive({ NULL })) {
  moduleServer(id, function(input, output, session) {
    # get frame data from upload
    framedata<-zipFileInput_server("userupload")

    # update cluster variable selection
    observe({
      req(framedata())
      ## enable selection
      shinyjs::enable("clustervar")
      # get all names
      cluvar<-names(framedata())
      # remove lat long
      cluvar<-cluvar[!(grepl(x = (cluvar), pattern = "(latitude)|(longitude)", ignore.case = T))]
      # update select
      cluvar<-setNames(cluvar, cluvar)
      updateSelectInput(
        inputId = "clustervar", label = "Select Cluster Variable", choices = cluvar
      )

    })

    # enable minimum distance field
    observeEvent(input$samplemode, {
      if(input$samplemode=="Balanced w. minimum distance"){
        shinyjs::enable("mindist")
      } else {
        shinyjs::disable("mindist")
      }
    })

    # enable sampling button
    observe({
      req(input$clustervar!="Load Data First!")
      shinyjs::enable("createSample")
    })

    # transform frame data to spatial & project
    framedatasf<-reactive({
      req(framedata())
      frdat<<-framedata()
      shiny::showNotification("Transforming data to spatial.")
      # identify lat/long
      lat<<-names(frdat)[grepl(x = names(frdat), pattern = "latitude", ignore.case = T)]
      long<<-names(frdat)[grepl(x = names(frdat), pattern = "longitude", ignore.case = T)]
      # drop missing
      frdat <- frdat %>% dplyr::filter(!is.na(.data[[lat[2]]]) & !is.na(.data[[long[2]]]))
      # generate cluster count
      frdat<-frdat %>% group_by(.data[[input$clustervar]]) %>% mutate(CLUSTERCOUNT = n())
      frdat<-as.data.frame(frdat)
      frdat<-st_as_sf(frdat, coords = c(long[2], lat[2]), crs=4326)

      # project data
      # suppressMessages(
      #   #crsproj<-crsuggest::suggest_top_crs(frdat, units = "m", inherit_gcs = T, output = "epsg")
      # )
      # frdat<-frdat %>% st_transform(crsproj)

      # ATTENTION on current system crssuggest does not work
      frdat<-project_to_utm(frdat)

      return(frdat)
    })

    # TAKE SAMPLE
    mapsampledata<-reactiveVal(NULL)
    dwlsampledata<-reactiveVal(NULL)
    sample_seed<-reactiveVal(NULL)
    observeEvent(input$createSample, {
      req(framedatasf()); req(input$sampSEED); req(input$clustervar)
      frdatsf<-framedatasf()
      shiny::showNotification("Taking the sample.")
      ## get seed
      sample_seed(input$sampSEED)
      ## balanced sampling check
      ss<-input$n_sample

      ## split sample in units with less than sample size
      frdatsf_takeall<-frdatsf[frdatsf$CLUSTERCOUNT<=ss,]
      frdatsf<-frdatsf[frdatsf$CLUSTERCOUNT>ss,]

      ## debug
      # CHECKfrdatsf<<-frdatsf #seed 4671 sample produces error with balance check

      ## create sample sizes for cluster
      strata_n<-rep(ss, length(unique(frdatsf[[input$clustervar]])))
      names(strata_n)<-unique(frdatsf[[input$clustervar]])

      ## take sample
      if(input$samplemode=="Balanced"){
        set.seed(input$sampSEED)
        samp<-spsurvey::grts(
          frdatsf,
          n_base = strata_n,
          stratum_var = input$clustervar
        )
        # ATTENTION, GIVES ERROR IN CERTAIN SAMPLES. CHECK LATER!
        # bal_score_eq<-spsurvey::sp_balance(samp$sites_base, frdatsf, metrics = "rmse")
        # print(bal_score_eq)

      } else if(input$samplemode=="Balanced w. minimum distance"){
        set.seed(input$sampSEED)
        req(input$mindist)
        samp<-spsurvey::grts(
          frdatsf,
          mindis = input$mindist,
          n_base = strata_n,
          stratum_var = input$clustervar
        )
        # ATTENTION, GIVES ERROR IN CERTAIN SAMPLES. CHECK LATER!
        # bal_score_dist<-spsurvey::sp_balance(samp$sites_base, frdatsf)
        # print(bal_score_dist)

      } else if(input$samplemode=="Random") {
        set.seed(input$sampSEED)
        #random sample
        samp<-spsurvey::irs(
          frdatsf,
          n_base = strata_n,
          stratum_var = input$clustervar
        )
        # ATTENTION, GIVES ERROR IN CERTAIN SAMPLES. CHECK LATER!
        # bal_score_rand<-spsurvey::sp_balance(samp$sites_base, frdatsf)
        # print(bal_score_rand)
      }

      ## prepare for map and download
      tabsf<-samp$sites_base %>% st_transform(4326)
      mapsampledata(tabsf)
      tab<-as.data.frame(tabsf %>% st_set_geometry(NULL))
      #tab<-cbind(tab, st_coordinates(tabsf))
      dwlsampledata(tab)

    })

    # DOWNLOAD
    download_csv_server("dwlsample",
                        content = dwlsampledata,
                        file_name = reactive(
                          paste0("SpatSamp_2stage_",
                                 input$samplemode,
                                 ifelse(input$samplemode=="Balanced w. minimum distance",
                                        paste0("_mindist_", input$mindist, "m"), ""),
                                 "_seed_",
                                 input$sampSEED)
                          ))
    # RETURN FOR MAP
    return(mapsampledata)
  })
}

# # TESTING ONLY
# ui <- fluidPage(
#   shinyjs::useShinyjs(),
#   modal_spsample2stage_ui("my_modal"),
#   leaflet::leafletOutput("map")
# )
#
# server <- function(input, output, session) {
#
#   gridPointsSpatial <- modal_spsample2stage_server("my_modal", shape_boundaries = reactive({ NULL }))
#
#
#   output$map<-leaflet::renderLeaflet({
#     req(gridPointsSpatial())
#     m<-mapview::mapview(gridPointsSpatial(), col.regions = "green")
#
#     m@map
#
#   })
#
# }
#
# shiny::shinyApp(ui,server)
