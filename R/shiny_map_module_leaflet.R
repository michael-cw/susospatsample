#' @noRd
#' @keywords internal
#'

mapUI <- function(id, width = "100%", height = "90vh") {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), width = width, height = height)
  )
}


#' @noRd
#' @keywords internal
#'

mapServer <- function(id,
                      updateMap = reactive(NULL),
                      updateGroup = reactive(NULL),
                      updateMapPts = reactive(NULL),
                      z_var = reactive(NULL),
                      startlat = reactive(NULL),
                      startlong = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        leaflet::setView(lat = 38.899063, lng = -77.042386, zoom = 12)
    })

    lfp <- leafletProxy("map", session = session)
    observe({
      if (is.null(startlat()) | is.null(startlong())) {
        # startlat = 38.899063
        # startlong = -77.042386
        # lfp %>%
        #   leaflet::setView(lng = 38.899063, lat = -77.042386, zoom = 12)
      } else {
        lfp %>%
          leaflet::setView(lng = startlong(), lat = startlat(), zoom = 12)
      }
    })

    observe({
      sf_poly <- (updateMap())
      filcolvar <- (updateGroup())

      popvar <- z_var()
      req(!is.null(sf_poly))
      req(!is.null(filcolvar))

      # to latlong if not
      if (!sf::st_is_longlat(sf_poly)) {
        sf_poly <- sf_poly %>%
          sf::st_transform(4326)
      }

      if ((filcolvar == "")) {
        if (is.null(popvar)) {
          # no z variable
          bounds <- sf::st_bbox(sf_poly) %>% as.character()

          lfp %>%
            clearShapes() %>%
            fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
            addPolygons(
              data = sf_poly,
              # fillColor = ~filcol(sf_poly[[filcolvar]]),
              # color = ~filcol(sf_poly[[filcolvar]]),
              fillOpacity = 0.8
            )
        } else {
          bounds <- sf::st_bbox(sf_poly) %>% as.character()

          lfp %>%
            clearShapes() %>%
            fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
            addPolygons(
              data = sf_poly,
              popup = ~ htmltools::htmlEscape(sf_poly[[popvar]]),
              # fillColor = ~filcol(sf_poly[[filcolvar]]),
              # color = ~filcol(sf_poly[[filcolvar]]),
              fillOpacity = 0.8
            )
        }
      } else {
        if (is.null(popvar)) {
          if (!is.numeric(sf_poly[[filcolvar]])) {
            filcol <- leaflet::colorFactor("RdYlBu", as.factor(sf_poly[[filcolvar]]))

            bounds <- sf::st_bbox(sf_poly) %>% as.character()

            lfp %>%
              clearShapes() %>%
              fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
              addPolygons(
                data = sf_poly,
                fillColor = ~ filcol(sf_poly[[filcolvar]]),
                color = ~ filcol(sf_poly[[filcolvar]]),
                fillOpacity = 0.8
              )
          } else {
            filcol <- leaflet::colorNumeric("magma", sf_poly[[filcolvar]])

            bounds <- sf::st_bbox(sf_poly) %>% as.character()

            lfp %>%
              clearShapes() %>%
              fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
              addPolygons(
                data = sf_poly,
                fillColor = ~ filcol(sf_poly[[filcolvar]]),
                color = ~ filcol(sf_poly[[filcolvar]]),
                fillOpacity = 0.8
              )
          }
        } else {
          if (!is.numeric(sf_poly[[filcolvar]])) {
            filcol <- leaflet::colorFactor("RdYlBu", as.factor(sf_poly[[filcolvar]]))

            bounds <- sf::st_bbox(sf_poly) %>% as.character()

            lfp %>%
              clearShapes() %>%
              fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
              addPolygons(
                data = sf_poly,
                popup = ~ htmltools::htmlEscape(sf_poly[[popvar]]),
                fillColor = ~ filcol(sf_poly[[filcolvar]]),
                color = ~ filcol(sf_poly[[filcolvar]]),
                fillOpacity = 0.8
              )
          } else {
            filcol <- leaflet::colorNumeric("magma", sf_poly[[filcolvar]])

            bounds <- sf::st_bbox(sf_poly) %>% as.character()

            lfp %>%
              clearShapes() %>%
              fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
              addPolygons(
                data = sf_poly,
                popup = ~ htmltools::htmlEscape(sf_poly[[popvar]]),
                fillColor = ~ filcol(sf_poly[[filcolvar]]),
                color = ~ filcol(sf_poly[[filcolvar]]),
                fillOpacity = 0.8
              )
          }
        }
      }
    })
  })
}


#' Leaflet min map for selection
#'
#'
#' @noRd
#' @keywords internal
#'
#'

# UI for the map module
mini_ampUI <- function(id) {
  # css style
  styleDwlButton <- c("color: #FFFFFF; width: 50%;
                    background-color: #1976D2;
                    border-color: #1976D2;
                    display: block;
                    margin-left: auto;
                    margin-right: auto;")
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        7,
        leafletOutput(ns("map"))
      ),
      column(
        5,
        DT::dataTableOutput(ns("subStratTable"))
      )
    ), br(),
    fluidRow(
      actionButton(ns("reset_create_subset"), "Reset Selection",
        style = styleDwlButton
      )
    )
  )
}

# Server function for the map module
mini_mapServer <- function(id, polygonLayer = reactive(NULL), polygonNames = reactive((NULL))) {
  moduleServer(id, function(input, output, session) {
    # table style
    smTab <- list(dom = "t")
    infoTable <- . %>% formatStyle(1,
      color = "#FFFFFF",
      backgroundColor = "#0d47a1",
      fontWeight = "bold"
    )


    polySelFullOuter <- reactiveValues(ID = numeric(0), subStrat = NULL)
    polySelFullOuterVAL <- reactiveVal(numeric(0))


    output$map <- renderLeaflet({
      example_sf <- req(polygonLayer())
      id <- req(polygonNames())
      # colors
      # Create a color palette based on the number of polygons
      colors <- RColorBrewer::brewer.pal(n = nrow(example_sf), name = "Set1")
      color_mapping <- colorFactor(colors, example_sf[[id]])

      leaflet(example_sf) %>%
        addTiles() %>%
        addPolygons(
          data = example_sf,
          layerId = 1:nrow(example_sf),
          color = ~ color_mapping(example_sf[[id]]),
          weight = 1,
          popup = ~ htmltools::htmlEscape(example_sf[[id]]),
          fillColor = ~ color_mapping(example_sf[[id]])
        )
    })

    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click

      polySel <- req(as.vector(input$map_shape_click$id))
      ID <- polySelFullOuter$ID
      polySelFull <- c(ID, polySel)

      DF <- polygonLayer()
      DF <- data.table(st_set_geometry(DF, NULL))
      # polySelFull<-polySelFull[,1]
      df_sub <- DF[polySelFull]
      ## NO double selection
      df_sub <- unique(df_sub)
      polySelFull <- unique(polySelFull)

      polySelFullOuter$ID <- polySelFull
      polySelFullOuter$subStrat <- df_sub
    })

    ##  1.4. Display Table
    output$subStratTable <- DT::renderDataTable(
      {
        tabl <- polySelFullOuter$subStrat
        shiny::validate(need(tabl, message = "Select Regions on Map!"))
        tabl <- tabl %>% dplyr::select(polygonNames())
        DTtab <- DT::datatable(tabl, smTab,
          selection = "none", rownames = T, escape = T, width = 30,
          style = "bootstrap"
        ) %>% infoTable()
        return(DTtab)
      },
      server = F
    )


    # reset
    observeEvent(input$reset_create_subset,
      {
        polySelFullOuter$subStrat <- c("")
        polySelFullOuter$ID <- NULL
      },
      ignoreInit = TRUE
    )

    # RETURN
    return(polySelFullOuter)
  })
}
