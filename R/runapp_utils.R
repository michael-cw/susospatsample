#' Get system working memory
#'
#' This is used to configure certain options at start-up
#'
#'
#' @noRd
#' @keywords internal

get_total_physical_memory <- function() {

  # For Windows
  get_memory_windows <- function() {
    # Run the 'systeminfo' command and get the output
    sys_info <- system('systeminfo', intern = TRUE)

    # Find the line that contains 'Available Physical Memory'
    memory_line <- grep("Total Physical Memory", sys_info, value = TRUE)

    if(length(memory_line) == 0) {
      stop("Failed to retrieve available memory. Are you sure you are running this on Windows?")
    }

    # Extract the numeric value from the memory line
    pattern <- "\\d{1,3}(?:,\\d{3})*(?:\\.\\d+)?"
    memory_value <- regmatches(memory_line, regexpr(pattern, memory_line))

    # Remove commas and convert to numeric
    memory_numeric <- as.numeric(gsub(",", ".", memory_value))

    return(memory_numeric)
  }

  # For Linux
  get_memory_linux <- function() {
    memory_info <- system('free -m', intern = TRUE)
    memory_line <- grep("^Mem:", memory_info, value = TRUE)
    if(length(memory_line) == 0) return(NA)

    # Extract the numeric value from the memory line
    pattern <- "\\d+"
    memory_value <- regmatches(memory_line, regexpr(pattern, memory_line))
    memory_value <- round(as.numeric(memory_value)/1024, 3) #divid int mb
    return(memory_value)
  }

  # For macOS hw.memsize: 34359738368
  get_memory_mac <- function() {
    memory_info <- system('sysctl hw.memsize', intern = TRUE)
    if(length(memory_info) == 0) return(NA)
    memory_value <- round(as.numeric(gsub("\\D", "", memory_info))/ (1024 * 1024 * 1024), 3)
    return(memory_value)
  }

  # Determine the operating system
  os <- Sys.info()['sysname']

  # Call the appropriate function based on the operating system
  if(os == "Windows") {
    return(get_memory_windows())
  } else if(os == "Linux") {
    return(get_memory_linux())
  } else if(os == "Darwin") { # macOS is identified as Darwin in R
    return(get_memory_mac())
  } else {
    warning("Unsupported operating system")
    return(NA)
  }
}
