#' Start the Survey Solutions Spatial Sampling Application
#'
#' @description A wrappter function to start the application. Please make sure you have read the
#' documentation on how to use the app under <https://datanalytics.worldbank.org/SpatialSamplingManual/>
#'
#' @details
#' This application is part of the large set of tools, to facilitate survey implementation with
#' [Survey Solutions](https://docs.mysurvey.solutions/). Enumeration Areas, sampled for example
#' with the JDC-Survey Solutions Spatial Sampling application, can be used to enumerate all buildings
#' visible within the boundaries on a Google map. The enumerate buildings can then be used for the
#' second stage sampling frame and to draw the survey units within the cluster from it.
#'
#' @inherit shiny::runApp
#' @param mapwidget.option Selection of map at start-up, mapdeck or leaflet, defaults to leaflet if NULL
#' @param google_ai_score_map_url Url for base map to google AI, unlikely to change!
#' @param google_ai_score_url URL for google AI scores, unlikely to change!
#' @param bufferForSuSoBounds Buffer for Survey Solutions
#' [Geofencing](https://docs.mysurvey.solutions/syntax-guide/questions/syntax-guide-gps-questions/)
#' @param pointsLimit Limit for points data to be aggregated to area
#' @param systemCheck Check system working memory and adjust application parameters. If you want to override this check, set it to
#' FALSE.
#'
#' @export

runSpatSampleApp <- function(launch.browser = T,
                             mapwidget.option = c("leaflet", "mapdeck"),
                             google_ai_score_url = "https://storage.googleapis.com/open-buildings-data/v1/score_thresholds_s2_level_4.csv",
                             google_ai_score_map_url = "https://sites.research.google/open-buildings/tiles.geojson",
                             bufferForSuSoBounds = 5,
                             pointsLimit = 100000,
                             systemCheck = TRUE) {
  # system Check
  if(systemCheck) {
    cat("Checking Working Memory\n\n")
    osmem<-get_total_physical_memory()
    if(is.na(osmem)) {
      cat("Could not read Memory, will set it to 16 GB\n\n")
      osmem<-32
    }
    cat("Total Physical Memory:", osmem, "GB\n\n")
  } else {
    osmem<-32
  }
  # add resource pathes
  shiny::addResourcePath("www", system.file("www", package = "susospatsample"))
  shiny::addResourcePath("ui_inputs", system.file("ui_inputs", package = "susospatsample"))
  shiny::addResourcePath("rmdfiles", system.file("rmdfiles", package = "susospatsample"))

  # variables check
  stopifnot(
    # numeric inputs
    is.numeric(bufferForSuSoBounds),
    is.numeric(pointsLimit),
    # web
    curl::has_internet(),
    !httr::http_error(google_ai_score_url),
    !httr::http_error(google_ai_score_map_url)
  )

  # option check
  mapwidget.option <- match.arg(mapwidget.option)


  # get original options
  original_options <- list(
    shiny.maxRequestSize = getOption("shiny.maxRequestSize"),
    # You might want to store your original spinner.color.background if it's set somewhere in your code
    spinner.color.background = getOption("spinner.color.background"),
    mapwidget.option = getOption("mapwidget.option"),
    google_ai_score_url = getOption("google_ai_score_url"),
    google_ai_score_map_url = getOption("google_ai_score_map_url"),
    bufferForSuSoBounds = getOption("bufferForSuSoBounds"),
    pointsLimit = getOption("pointsLimit")
  )
  # change options and revert on stop
  changeoptions <- function() {
    options(
      # Temporary change of environment options
      shiny.maxRequestSize = osmem * 1024^3,
      spinner.color.background = "#0d47a1",
      mapwidget.option = mapwidget.option,
      google_ai_score_url = google_ai_score_url,
      google_ai_score_map_url = google_ai_score_map_url,
      bufferForSuSoBounds = bufferForSuSoBounds,
      pointsLimit = pointsLimit
    )
    # shiny::shinyOptions(shiny.maxRequestSize = 500000 * 1024^2)

    # revert to original state at the end
    shiny::onStop(function() {
      if (!is.null(original_options)) {
        options(original_options)
      }
    })
  }
  # create app & run
  appObj <- shiny::shinyApp(ui = main_ui, server = main_server, onStart = changeoptions)
  shiny::runApp(appObj, launch.browser = launch.browser, quiet = T)
}
