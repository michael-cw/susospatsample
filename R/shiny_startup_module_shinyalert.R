
# Module UI
startupModalUI <- function(id) {
  ns <- NS(id)
  shiny::tagList()
}

# Module server function
startupModalSRV <- function(id,
                            useronly = FALSE,
                            welcometitle = "Welcome to the Survey Solutions Spatial Sampling Application!",
                            welcomemessage = shiny::h4("Please make sure you have read the relevant documentation,
                          available under: https://datanalytics.worldbank.org/SpatialSamplingManual/
                          To continue please provide your Mapbox API key and your username."),
                            apikeyhead = "Mapbox API key missing!",
                            apikeymessage = "You have not provided a valid key.
                         Without a Mapbox API key, it won't be possible use mapdeck.
                         Please restart the app with the leaflet map option, or retrieve a valid Mapbox
                         API key and refresh the page to provide the correct key.") {
  moduleServer(id, function(input, output, session) {
    mapkey <- reactiveVal(NULL)
    user <- reactiveVal(NULL)

    # conditional, if user or user & api key is required
    modalfields<-function(id, uo = useronly) {
      ns <- NS(id)
      if(!uo) {
        tagList(
          shiny::passwordInput(
            inputId = ns("mapkey"),
            label = "",
            placeholder = "API key"
          ), br(),
          shiny::textInput(
            inputId = ns("user"),
            label = "",
            placeholder = "User Name"
          )
        )
      } else if(uo) {
        tagList(
          shiny::textInput(
            inputId = ns("user"),
            label = "",
            placeholder = "User Name"
          )
        )
      }
    }

    observe({
      ns <- NS(id)
      shinyalert::shinyalert(
        inputId = "startup",
        title = paste(welcometitle),
        text = tagList(
          welcomemessage,
          modalfields(id = id, uo = useronly)
        ),
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE)
    })

    observeEvent(input$startup, {
      if (!useronly && input$mapkey == "") {
        shinyalert::shinyalert(paste(apikeyhead),
                               apikeymessage,
                               closeOnEsc = TRUE,
                               closeOnClickOutside = TRUE,
                               html = FALSE,
                               type = "error",
                               showConfirmButton = TRUE,
                               showCancelButton = FALSE,
                               confirmButtonText = "OK",
                               confirmButtonCol = "#AEDEF4",
                               timer = 0,
                               imageUrl = "",
                               animation = TRUE)
        req(FALSE)
      } else if (input$user == "") {
        shinyalert::shinyalert(paste("User name missing!"),
                               "You have not provided a user name. Since this is required
                               to store the files in your personal directory,
                               please refresh the page and provide a user name.",
                               closeOnEsc = TRUE,
                               closeOnClickOutside = TRUE,
                               html = FALSE,
                               type = "error",
                               showConfirmButton = TRUE,
                               showCancelButton = FALSE,
                               confirmButtonText = "OK",
                               confirmButtonCol = "#AEDEF4",
                               timer = 0,
                               imageUrl = "",
                               animation = TRUE)
        req(FALSE)
      } else {
        # mapkey if selected
        if(!useronly) mapkey(input$mapkey)
        # user
        usr <- sanitize_string(input$user)
        user(usr)
      }
    })

    return(
      list(
        key = mapkey,
        user = user
      )
    )
  })
}

#' helper for string sanitation for directory path
#'
#' @rdname internal
#' @noRd
#'

sanitize_string <- function(input_string, repl_for_whitespace = "_") {
  sanitized_string <- stringr::str_replace_all(input_string, "[^a-zA-Z0-9_]", stringr::fixed(" ")) %>%
    iconv("UTF-8", "ASCII", sub = "") %>%
    stringr::str_squish() %>%
    stringr::str_replace_all(stringr::fixed(" "), repl_for_whitespace)

  return(sanitized_string)
}
