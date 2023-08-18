#' Modal with error
#'
#' @noRd
#' @keywords internal
#'

.runWithModalOnError <- function(func) {
  result <- tryCatch(
    {
      func
    },
    error = function(err) {
      # Display the error message in a Shiny modal
      shiny::showModal(modalDialog(
        title = HTML("<div align='center'>ERROR</div>"),
        HTML(paste("<div align='center'>An error occurred:", err$message, "</div>")),
        easyClose = TRUE
      ))
      return(NULL)
    }
  )

  return(result)
}
