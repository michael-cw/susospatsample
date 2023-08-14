#' shiny module for  the creation of shape boundaries for download or api
#'
#'
#'
#' @description Modal dialogue to download google AI build up area data with map for selection.
#' Requires boundaries as input and returns POINTS data.table for rasterization.
#'
#' @return Returns points data and list of variables for aggregation, adds a column of 1s for simple count
#'
#' @noRd
#' @keywords internal




# module UI
modal_createshape_ui <- function(id,
                                 style = "color: #FFFFFF; width: 180px;background-color: #1976D2;border-color: #0d47a1") {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    actionButton(ns("showShapeModal"),
                 "Create Boundary Files",
                 #width = "100%",
                 icon("download"),
                 style=style
    )
  )
}

# module server
modal_createshape_server <- function(id, shape_boundaries = reactive({ NULL }),
                                     sampType=reactive({"Random Grid"}),
                                     sample_seed = reactive({NULL})) {
  moduleServer(id, function(input, output, session) {
    #########################################
    ## CSS/UI Styles
    styleDwlButton<-c("color: #FFFFFF;  width: 180px;background-color: #1976D2;
                  border-color: #1976D2;
                  margin:0 20% 0 20%;")
    smTabDir<-list(dom="t", pagelength=500, scrollY="250px", scrollcollapse=TRUE, paging=FALSE)

    action_btn_close <-c("color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1")
    styleActButton<-c("color: #FFFFFF; background-color: #7f0000;
                  border-color: #7f0000; margin:0 20% 0 20%;")


    # create a reactiveValues object to store the modal input values
    modal_values <- reactiveVal(NULL)

    # show the modal when the "Launch modal" button is clicked
    observeEvent(input$showShapeModal, {
      ns<-NS(id)

      # generate ID choices for cluster
      if(sampType()=="Random Cluster") {
        req(shape_boundaries())
        tab<-names(shape_boundaries())
        idchoices<-setNames(tab, tab)

        ## ii. Show Modal
        showModal(modalDialog(title =tags$div(
          HTML("<center><font color='#0d47a1'><big>Create Area Boundaries</big></font></center>")),
          fluidRow(
            column(6,
                   radioButtons(ns("split_segments"),
                                label = "Split Sample into individual Segments?",
                                choices = c("Yes", "No"),
                                selected = "No", inline = T
                   ),br(),
                   helpText("You can either split the boundaries into individual files, or keep them as a
                          single file."),
                   conditionalPanel(sprintf("input['%s'] == 'Yes'", ns("split_segments")),
                                    numericInput(
                                      inputId = ns("bound_segments"),
                                      label = "Create Sub-Segments within Segments?",
                                      value = 4, step = 1
                                    ),
                                    helpText("You can create similar sized sub-segments for cluster,
                                    in case you consider sub-sampling
                                    within a grid cell or to facilitate fieldwork.")
                   )
            ),
            column(6,
                   radioButtons(ns("cluster_id"),
                                label = "Use individual cluster ID?",
                                choices = c("Yes", "No"),
                                selected = "No", inline = T
                   ), br(),
                   conditionalPanel(sprintf("input['%s'] == 'Yes'", ns("cluster_id")),
                                    selectizeInput(
                                      inputId = ns("cluster_id_sel"),
                                      label = "Select variable for cluster ID:",
                                      choices = idchoices
                                    ),
                                    helpText("You can either use your own cluster ID, or use a system generated one.
                                             If you use your own ID, then this ID must be unique across the whole frame.")
                   )
            )
          ),
          br(),
          fluidRow(
            column(8,
                   actionButton(ns("create_boundaries"),"Create boundaries",
                                style=styleDwlButton)
            ),
            column(4,
                   actionButton(ns("cancel"),"Cancel",
                                style=action_btn_close)
            )
          ),
          footer = NULL,
          easyClose = T, size = "l"
        ))
      } else if(sampType()=="Random Grid"){
        ## ii. Show Modal
        showModal(modalDialog(title =tags$div(
          HTML("<center><font color='#0d47a1'><big>Create Area Boundaries</big></font></center>")),
          fluidRow(
            column(6,
                   radioButtons(ns("split_segments"),
                                label = "Split Sample into individual Segments?",
                                choices = c("Yes", "No"),
                                selected = "No", inline = T
                   ),br(),
                   helpText("You can either split the boundaries into individual files, or keep them as a
                          single file.")
            ),
            column(6,
                   conditionalPanel(sprintf("input['%s'] == 'Yes'", ns("split_segments")),
                                    selectizeInput(
                                      inputId = ns("bound_segments"),
                                      label = "Create Sub-Segments within Segments?",
                                      choices = c(0, 2, 3, 4)
                                    ),
                                    helpText("You can create sub-segments for Grid Cells, in case you consider sub-sampling
                                  within a grid cell or to facilitate fieldwork.")
                   )
            )
          ),
          br(),
          fluidRow(
            column(8,
                   actionButton(ns("create_boundaries"),"Create boundaries",
                                style=styleDwlButton)
            ),
            column(4,
                   actionButton(ns("cancel"),"Cancel",
                                style=action_btn_close)
            )
          ),
          footer = NULL,
          easyClose = T, size = "l"
        ))
      }

    })
    #observe({print(input$cluster_id_sel)})
    # close modal (no action)
    observeEvent(input$cancel, {
      removeModal()
    })
    # create file
    observeEvent(input$create_boundaries, {
      removeModal()

      withProgress(message = 'Generating Boundary File(s)',
                   value = 0,
                   {
                     ## vector for file pathes
                     fs <- c()
                     ## sample shapes input
                     samp_raster_shp<-shape_boundaries()
                     ## set temp dir
                     temp.dir<-file.path(tempdir(), "boundaries")
                     if(!dir.exists(temp.dir)) dir.create(temp.dir)
                     ## clear temp dir at start
                     unlink(file.path(temp.dir, "*"))
                     ## clear temp dir after zip
                     on.exit(unlink(file.path(temp.dir, "*")))
                     shiny::validate(need(samp_raster_shp, message = F))

                     #################################
                     ##  Projection to WEBMERCATOR
                     WEBMER_CRS<-("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
                     samp_raster_shp<-st_transform(samp_raster_shp, WEBMER_CRS)

                     #################################
                     ## Download Directory Setup
                     p1poly<-samp_raster_shp
                     #DSN<-paste0("area_boundaries")
                     fname<-paste0("area_boundaries", ".shp")
                     #print(fname)
                     #if (!dir.exists(DSN)) dir.create(DSN, recursive = T)

                     #################################
                     ##  Approach 1: Single shape
                     if(input$split_segments=="No"){
                       # Transform to lat long
                       p1poly<-p1poly %>% st_transform(4326)
                       # Write to file
                       suppressWarnings(
                         sf::st_write(dsn = file.path(temp.dir, fname),
                                      obj = p1poly, delete_layer = T,
                                      driver="ESRI Shapefile", quiet = T)
                       )
                       fs<-c(fs, list.files(temp.dir, full.names = T, pattern = "(.dbf$)|(.tif$)|(.prj$)|(.shp$)|(.shx$)"))
                       zfile<-tempfile("shapefordownload", fileext = ".zip")
                       zip::zip(zipfile=zfile, files=fs, mode = "cherry-pick")
                       # return file path for zip for download
                       modal_values(zfile)
                     } else if (input$split_segments=="Yes") {
                       #################################
                       ##  Approach 2.1: Multiple shapes, No Split
                       n_seg<-nrow(p1poly)
                       for (i in seq_along(st_geometry(p1poly))) {
                         area<-p1poly[i,]
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
                         #DSN1<-file.path(DSN, areaName)
                         #dir.create(DSN1, recursive = T)
                         #################################
                         ##  Approach 2.2: Multiple shapes, Split
                         if(sampType()== "Random Grid" && as.numeric(input$bound_segments)>0) {

                           area<- sf::st_sf(
                             geometry=sf::st_make_grid(area, n = as.numeric(input$bound_segments)),
                             CID = areaName
                           )
                           ## add label
                           area$label<-seq_along(st_geometry(area))
                         } else if(sampType()== "Random Cluster" && as.numeric(input$bound_segments)>0) {
                           req(sample_seed())

                           suppressWarnings(
                             area<- tryCatch(
                               {split_poly(area, as.numeric(input$bound_segments), sample_seed())},
                               error = function(e) {
                                 shiny::showNotification(paste0(areaName, "could not be split."), type = "warning")
                                 return(area)
                               })
                           )
                           ## add label
                           area$label<-seq_along(st_geometry(area))
                         }
                         areaName<-paste0(areaName, ".shp")
                         # Transform to lat long
                         area<-area %>% st_transform(4326)
                         # Write to file
                         suppressWarnings(
                           sf::st_write(dsn = file.path(temp.dir, areaName),
                                        obj = area, delete_layer = T,
                                        driver="ESRI Shapefile", quiet = T)
                         )
                       }
                       fs<-c(fs, list.files(temp.dir, full.names = T, pattern = "(.dbf$)|(.tif$)|(.prj$)|(.shp$)|(.shx$)"))
                       zfile<-tempfile("shapefordownload", fileext = ".zip")
                       zip::zip(zipfile=zfile, files=fs, mode = "cherry-pick")
                       # return file path for zip for download
                       modal_values(zfile)
                     }

                   })
    }, ignoreInit = T)




    # return the modal_values reactiveValues object
    return(modal_values)
  })
}

# module helper
# 1. split poly
split_poly <- function(sf_poly, n_areas, seed = floor(runif(1, 1000,9999))){
  if(!sf::st_is_longlat(sf_poly)) sf_poly<-sf::st_transform(sf_poly, 4326)
  set.seed(seed)
  # create random points
  points_rnd <- sf::st_sample(sf_poly, size = 10000)
  #k-means clustering
  points <- do.call(rbind, sf::st_geometry(points_rnd)) %>%
    as_tibble() %>% setNames(c("lon","lat"))
  k_means <- tryCatch(
    {kmeans(points, centers = n_areas)},
    error = function(e) return(NULL))
  # exit on error and return input shape
  if(!is.null(k_means)) {
    return(sf_poly)
  }
  # create voronoi polygons
  voronoi_polys <- dismo::voronoi(k_means$centers, ext = sf_poly)
  # clip to sf_poly
  raster::crs(voronoi_polys) <- raster::crs(sf_poly)
  voronoi_sf <- sf::st_as_sf(voronoi_polys)
  equal_areas <- sf::st_intersection(voronoi_sf, sf_poly)
  equal_areas$area <- sf::st_area(equal_areas)
  return(equal_areas)
}

