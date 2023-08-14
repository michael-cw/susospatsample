#' mapdeck UI Function
#'
#' @description Module for mapdeck integration into shiny
#'
#' @param id Internal parameters for {shiny}.
#'
#' @rdname internal
#' @noRd
#'
#'
mapModuleUI<-function(id, width = "100%", height = "880px") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fluidRow(
      shinycssloaders::withSpinner(
        mapdeckOutput(ns("baseMap1"), width = "100%", height = "880px")
      )
    )
  )
}

#' mapdeck Server Functions
#'
#' @rdname mod_mapdeck
#'
#' @noRd
mapModuleSvr <- function(id,
                         #key = reactive(NULL),
                         mainMap = reactive(NULL),
                         mainMapPts = reactive(NULL),
                         updateMap = reactive(NULL),
                         updateMapPts = reactive(NULL),
                         updateMapLine = reactive(NULL),
                         updateMapGrid = reactive(NULL),
                         updateMapArcs = reactive(NULL),
                         grouping1 = reactive(NULL),
                         updateGroup = reactive(NULL),
                         polyId = reactive(NULL),
                         z_var = reactive(NULL),
                         maptype = "satellite",
                         fillcolor = NULL,
                         fill_opacity = 0.6,
                         stroke_from_opacity = 0.6,
                         stroke_from = NULL,
                         stroke_width = 4,
                         palette = NULL,
                         layeridpts = NULL,
                         clearpts = FALSE,
                         cleargrd = FALSE,
                         clearpol = FALSE,
                         cleararc = FALSE,
                         tooltip = NULL,
                         clickpts = F,
                         startlocation = c(0, 0),
                         startzoom = 1,
                         zoom.transition = 7,
                         legend = FALSE,
                         legend_options = NULL,
                         legend_format = NULL,
                         transitions = list(
                           polygon = 2000,
                           fill_colour = 2000,
                           stroke_width = 2000,
                           elevation = 2000
                         )) {
  ############################################################################################################################
  ## 1. Check inputs
  #stopifnot(is.reactive(updateMap)); stopifnot(is.reactive(updateGroup)); stopifnot(is.reactive(z_var))
  moduleServer(
    id,
    function(input, output, session) {
      ########################################
      ## 1. Plot the basemap --> hidden for start-up in UI
      output$baseMap1<-mapdeck::renderMapdeck({
        m<-mapdeck(location = startlocation, zoom = startzoom,  #token = key(), key is set with set token
                   style = mapdeck_style(maptype))
        return(m)
      })
      ########################################
      ## 2. Update Map
      ## 2.1 POLYGONS
      observe({
        Boundaries<-updateMap()
        grouping2<-updateGroup()
        popvar<-z_var()
        # shiny::validate(need(Boundaries, message = F),
        #                 need(grouping2, message = F))
        ############################################################
        ## MAP --LEAF_GL
        ## map inputs
        req(!is.null(grouping2))
        req(!is.null(Boundaries))
        ## Adjust TRANSFORMATION
        Boundaries<-Boundaries %>% st_transform(4326)
        # remove empty geometries
        Boundaries<-Boundaries[ !sf::st_is_empty(Boundaries), ]

        if ((grouping2=="")){
          ## map updated via leaflet proxy
          if(is.null(popvar)) {
            mapdeck_update(map_id = session$ns("baseMap1")) %>%
              clear_polygon(layer_id = "pols") %>%
              add_polygon(data = Boundaries,
                          stroke_colour = "#FF0000",
                          stroke_width = 10,
                          fill_colour = "#0000FF80",
                          fill_opacity = 0.5,
                          layer_id  = "pols",
                          update_view = T,
                          focus_layer = T)
          } else {
            ###########################################
            ## elevation
            Boundaries$ele <- Boundaries[[popvar]]
            Boundaries<-Boundaries[!is.na(Boundaries$ele),]
            ## start is 10000*10000*1000 box for adjustment
            b_area<-as.numeric((st_area(st_as_sfc(st_bbox(Boundaries))))^0.5)
            b_adjust<-round((b_area)/(max(Boundaries$ele)*5), 3)
            Boundaries$ele<-Boundaries$ele*b_adjust
            ## Get Centroids
            loc<-loc_center(Boundaries)
            ## calculate zoom
            bb<-st_bbox(Boundaries)
            zoom<-RgoogleMaps::MaxZoom(bb[c(2,4)], bb[c(1,3)])
            ############################################
            mapdeck_update(map_id = session$ns("baseMap1")) %>%
              clear_polygon(layer_id = "pols") %>%
              add_polygon(data = Boundaries,
                          fill_colour = "ele",
                          fill_opacity = 0.5,
                          layer_id  = "pols",
                          elevation = "ele",
                          update_view = T,
                          focus_layer = T)
          }
        } else if (grouping2!=""){
          if(is.null(popvar)) {
            mapdeck_update(map_id = session$ns("baseMap1")) %>%
              clear_polygon(layer_id = "pols") %>%
              add_polygon(data = Boundaries,
                          stroke_colour = NULL,"#FF0000FF",
                          stroke_width = 10,
                          fill_colour = grouping2,
                          fill_opacity  = 0.5,
                          focus_layer = T,
                          layer_id = "pols",
                          update_view = T)
          } else {
            ###########################################
            ## elevation
            Boundaries$ele <- Boundaries[[popvar]]
            Boundaries<-Boundaries[!is.na(Boundaries$ele),]
            ## start is 10000*10000*1000 box for adjustment
            b_area<-as.numeric((st_area(st_as_sfc(st_bbox(Boundaries))))^0.5)
            b_adjust<-round((b_area)/(max(Boundaries$ele)*5), 3)*0.1
            Boundaries$ele<-Boundaries$ele*b_adjust
            ## Get Centroids
            loc<-loc_center(Boundaries)
            ## calculate zoom
            bb<-st_bbox(Boundaries)
            zoom<-RgoogleMaps::MaxZoom(bb[c(2,4)], bb[c(1,3)])
            ############################################
            mapdeck_update(map_id = session$ns("baseMap1")) %>%
              clear_polygon(layer_id = "pols") %>%
              add_polygon(data = Boundaries, #transitions =
                          fill_colour = "ele",
                          tooltip = tooltip,
                          fill_opacity = 0.9,
                          palette = "inferno",
                          light_settings = list(
                            lightsPosition = c(-150, 75, 0)
                            , numberOfLights = 4
                            , ambientRatio = 0.4
                          ),
                          layer_id  = "pols",
                          elevation = "ele",
                          update_view = FALSE,
                          transitions = NULL,
                          id = "judsel",
                          auto_highlight = T,
                          highlight_colour = "#ffe3910b"
              )
          }
        }

      })
      ##################################
      ## 2.2 LINES
      observe({
        Boundaries<-updateMapLine()
        grouping2<-updateGroup()
        popvar<-z_var()
        # shiny::validate(need(Boundaries, message = F),
        #                 need(grouping2, message = F))
        ############################################################
        ## MAP --LEAF_GL
        ## map inputs
        req(!is.null(grouping2))
        req(!is.null(Boundaries))

        ## Adjust TRANSFORMATION
        Boundaries<-Boundaries %>% st_transform(4326)
        if ((grouping2=="")){
          ## map updated via leaflet proxy
          if(is.null(popvar)) {
            mapdeck_update(map_id = session$ns("baseMap1")) %>%
              clear_polygon(layer_id = "lin") %>%
              add_path(data = Boundaries,
                       stroke_colour = "#FF0000FF",
                       stroke_width = 2,
                       layer_id  = "lin",
                       update_view = T,
                       focus_layer = T)
          }
        } else if (grouping2!=""){
          if(is.null(popvar)) {
            mapdeck_update(map_id = session$ns("baseMap1")) %>%
              clear_polygon(layer_id = "lin") %>%
              add_path(data = Boundaries,
                       stroke_colour = grouping2,
                       stroke_width = 2,
                       focus_layer = T,
                       layer_id = "lin",
                       update_view = T)
          }
        }

      })
      ########################################
      ## 2.2.1 Update View if pop
      observe({
        Boundaries<-updateMap()
        popvar<-z_var()
        req(Boundaries)
        req(popvar)

        ############################################
        ## Get Centroids
        loc<-loc_center(Boundaries)
        ## calculate zoom
        bb<-st_bbox(Boundaries)
        zoom<-RgoogleMaps::MaxZoom(bb[c(2,4)], bb[c(1,3)])
        ############################################
        mapdeck_update(map_id = session$ns("baseMap1")) %>%
          mapdeck_view(pitch = 60, zoom = zoom.transition, location = loc,
                       duration = 3000, transition = "fly")
      })
      ########################################
      ## 2.3 ADD points
      observe({
        pts<-updateMapPts()
        req(pts)
        #if(is.null(fillcolor)) fillcolor<-"#5de30b"
        shiny::validate(need(fillcolor, message = F))
        shiny::validate(need(layeridpts, message = F))
        pts$UID<-rownames(pts)

        ############################################
        ## Get Centroids
        loc<-loc_center(pts)
        ## calculate zoom
        bb<-st_bbox(pts)
        zoom<-RgoogleMaps::MaxZoom(bb[c(2,4)], bb[c(1,3)])
        ############################################
        mapdeck_update(map_id = session$ns("baseMap1")) %>%
          add_scatterplot(pts,
                          layer_id = layeridpts,
                          id = "UID",
                          tooltip = tooltip,
                          radius = 5,
                          radius_min_pixels = 5,
                          fill_opacity = fill_opacity,
                          focus_layer = T,
                          update_view = F,
                          fill_colour = fillcolor,
                          palette = palette,
                          auto_highlight = T,
                          legend = legend,
                          legend_options = legend_options,
                          legend_format = legend_format,
                          highlight_colour = "#ffe3910b")
      })
      # ## 2.3.1 CLEAR points
      observe({
        if(clearpts) {
          mapdeck_update(map_id = session$ns("baseMap1")) %>%
            clear_scatterplot()
        }
      })
      ########################################
      ## 2.3.2 Capture POINT click events
      ## i. create input string for click
      # clickScatter<-eventReactive(input$baseMap1_scatterplot_click, {
      #   return(input$baseMap1_scatterplot_click)
      # })
      ########################################
      ## 2.4 ADD grid
      observe({
        pts<-updateMapGrid()
        req(pts)
        popvar<-z_var()
        req(popvar)
        #if(is.null(fillcolor)) fillcolor<-"#5de30b"
        pts$UID<-rownames(pts)
        ###########################################
        ## elevation
        pts$ele <- pts[[popvar]]
        pts<-pts[!is.na(pts$ele),]
        ## start is 10000*10000*1000 box for adjustment
        b_area<-as.numeric((st_area(st_as_sfc(st_bbox(pts))))^0.5)
        b_adjust<-round((b_area)/(max(pts$ele)*5), 3)
        pts$ele<-pts$ele*b_adjust
        ############################################
        ## Get Centroids
        loc<-loc_center(pts)
        ## calculate zoom
        bb<-st_bbox(pts)
        zoom<-RgoogleMaps::MaxZoom(bb[c(2,4)], bb[c(1,3)])
        ############################################
        mapdeck_update(map_id = session$ns("baseMap1")) %>%
          #update_style (style = mapdeck_style("dark")) %>%
          add_grid(pts, elevation_function = "sum",
                   elevation = "ele",
                   #elevation_scale = 20,
                   cell_size = 1000,
                   layer_id = "grid",
                   focus_layer = T,
                   legend = TRUE,
                   colour = "ele",
                   colour_function = "sum",
                   update_view = T,
                   auto_highlight = T,
                   #opacity = 0.8,
                   highlight_colour = "#e3910bcc")
      })
      ## 2.4.1 CLEAR grid
      observe({
        if(cleargrd) {
          mapdeck_update(map_id = session$ns("baseMap1")) %>%
            clear_grid(layer_id = "grid")
        }
      })

      ########################################
      ## 2.4 ADD ARC
      observe({
        pts<-updateMapArcs()
        req(pts)

        ############################################
        ## Get Centroids
        loc<-loc_center(pts)
        ## calculate zoom
        bb<-st_bbox(pts)
        zoom<-RgoogleMaps::MaxZoom(bb[c(2,4)], bb[c(1,3)])
        ############################################
        mapdeck_update(map_id = session$ns("baseMap1")) %>%
          #update_style (style = mapdeck_style("dark")) %>%
          add_arc(pts,layer_id = "arc", tooltip = tooltip,
                  origin = "geometry",
                  destination = "destination",
                  update_view = T,
                  auto_highlight = F,
                  stroke_from_opacity = stroke_from_opacity,
                  stroke_from = stroke_from,
                  stroke_width = stroke_width,
                  legend = legend,
                  legend_options = legend_options,
                  legend_format = legend_format,
                  highlight_colour = "#e3910bcc")
      })
      ## 2.4.1 CLEAR points
      observe({
        if(cleararc) {
          mapdeck_update(map_id = session$ns("baseMap1")) %>%
            clear_arc(layer_id = "arc")
        }
      })


      #################################################################### F I N #################################################################
    }
  )}
