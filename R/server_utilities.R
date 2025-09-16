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


#' @title Create spatially balanced samples using spsurvey GRTS with sf objects and write to CSV
#'
#' @description
#' This function takes a data.table with latitude and longitude coordinates,
#' converts it to an sf object, transforms it to UTM (if not already),
#' calculates the sample size per sequence, generates a specified number of
#' spatially balanced samples using the spsurvey package's GRTS method,
#' and writes each sample to a separate CSV file.
#' The function ensures non-overlapping samples across sequences.
#'
#' @param dt A data.table object containing spatial data. It must have a unique
#'   ID column.
#' @param seq_length An integer specifying the number of spatially balanced
#'   samples to create (e.g., 12 for 12 different samples). This implies
#'   how many files will be generated.
#' @param file_prefix A character string to be used as the prefix for
#'   the output CSV file names (e.g., "grts_sample"). The files will be named
#'   like "file_prefix1.csv", "file_prefix2.csv", etc.
#' @param latitude_var A character string specifying the name of the column
#'   in `dt` that contains latitude coordinates.
#' @param longitude_var A character string specifying the name of the column
#'   in `dt` that contains longitude coordinates.
#' @param id_var A character string specifying the name of the column in `dt`
#'   that contains a unique identifier for each point. This is crucial for
#'   `spsurvey`.
#' @param crs_value An integer or character string specifying the Coordinate
#'   Reference System (CRS) for the input spatial data. For lat/lon, 4326 is
#'   common (WGS84). E.g., `4326` or `"EPSG:4326"`.
#' @param output_dir A character string specifying the output directory
#'   where the CSV files will be saved. Defaults to "./data/out_spsurvey_sf_samples/".
#' @param return_result TRUE or FALSE. If TRUE, the function will return a list of
#' sampled data.tables instead of writing to files. Defaults to FALSE.
#'
#'
#' @return Invisible NULL. The function's primary purpose is to write files.
#' @examples
#' \dontrun{
#' # Create a dummy data.table with lat/lon and a unique ID
#' set.seed(1234)
#' spatial_data_sf <- data.table::data.table(
#'   PlotID = paste0("Plot_", 1:1000), # Unique ID column
#'   AttributeA = rnorm(1000),
#'   AttributeB = sample(c("Forest", "Grassland", "Water"), 1000, replace = TRUE),
#'   Latitude = runif(1000, 34.0, 36.0),
#'   Longitude = runif(1000, -84.0, -82.0)
#' )
#'
#' # Ensure the output directory exists or can be created
#' output_dir_path <- "./data/out_spsurvey_sf_samples_project/"
#' if (!dir.exists(output_dir_path)) {
#'   dir.create(output_dir_path, recursive = TRUE)
#' }
#'
#' split_and_write_spatbalance_dt(
#'   dt = spatial_data_sf,
#'   seq_length = 20, # Create 20 spatially balanced samples
#'   file_prefix = "plot_grts_sample_sf",
#'   latitude_var = "Latitude",
#'   longitude_var = "Longitude",
#'   id_var = "PlotID",
#'   crs_value = 4326, # WGS84 latitude/longitude
#'   output_dir = output_dir_path
#' )
#' # This will create 20 files, each with 50 rows (1000 / 20 = 50)
#'
#' # Example with non-perfectly divisible data
#' set.seed(5678)
#' spatial_data_sf_odd <- data.table::data.table(
#'   LocationID = paste0("Loc_", 1:1005),
#'   Measurement = runif(1005, 30.0, 31.0),
#'   Lat = runif(1005, 30.0, 31.0),
#'   Lon = runif(1005, -90.0, -89.0)
#' )
#'
#' output_dir_path_odd <- "./data/out_spsurvey_sf_samples_odd/"
#' if (!dir.exists(output_dir_path_odd)) {
#'   dir.create(output_dir_path_odd, recursive = TRUE)
#' }
#'
#' split_and_write_spatbalance_dt(
#'   dt = spatial_data_sf_odd,
#'   seq_length = 10,
#'   file_prefix = "location_grts_sample_sf_odd",
#'   latitude_var = "Lat",
#'   longitude_var = "Lon",
#'   id_var = "LocationID",
#'   crs_value = 4326,
#'   output_dir = output_dir_path_odd
#' )
#' # This will create 10 files, some with 100 rows, some with 101 rows.
#' }
##' @noRd
#' @keywords internal
#'
split_and_write_spatbalance_dt <- function(
    dt,
    seq_length,
    file_prefix,
    latitude_var,
    longitude_var,
    id_var,
    crs_value,
    output_dir = "./data/out_spsurvey_sf_samples/",
    return_result = FALSE
) {
  
  # --- 1. Input Validation ---
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required. Please install it with install.packages('data.table')")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required. Please install it with install.packages('sf')")
  }
  if (!requireNamespace("spsurvey", quietly = TRUE)) {
    stop("The 'spsurvey' package is required. Please install it with install.packages('spsurvey')")
  }
  
  if (!data.table::is.data.table(dt)) {
    stop("The 'dt' argument must be a data.table.")
  }
  if (!is.numeric(seq_length) || length(seq_length) != 1 || seq_length <= 0) {
    stop("The 'seq_length' argument must be a single positive integer.")
  }
  if (!is.character(file_prefix) || length(file_prefix) != 1 || nchar(file_prefix) == 0) {
    stop("The 'file_prefix' argument must be a non-empty character string.")
  }
  if (!is.character(latitude_var) || length(latitude_var) != 1 || nchar(latitude_var) == 0) {
    stop("The 'latitude_var' argument must be a non-empty character string specifying the latitude column name.")
  }
  if (!is.character(longitude_var) || length(longitude_var) != 1 || nchar(longitude_var) == 0) {
    stop("The 'longitude_var' argument must be a non-empty character string specifying the longitude column name.")
  }
  if (!is.character(id_var) || length(id_var) != 1 || nchar(id_var) == 0) {
    stop("The 'id_var' argument must be a non-empty character string specifying the unique ID column name.")
  }
  if(!is.null(output_dir)) {
    if (!is.character(output_dir) || length(output_dir) != 1 || nchar(output_dir) == 0) {
      stop("The 'output_dir' argument must be a non-empty character string.")
    }
  }
  if (missing(crs_value)) {
    stop("The 'crs_value' argument is required to define the coordinate system of your data.")
  }
  
  # Check if required columns exist
  required_cols <- c(latitude_var, longitude_var, id_var)
  missing_cols <- required_cols[!(required_cols %in% names(dt))]
  if (length(missing_cols) > 0) {
    stop(paste0("Missing required column(s) in data.table: ", paste(missing_cols, collapse = ", ")))
  }
  
  # Check for unique IDs
  if (data.table::uniqueN(dt[[id_var]]) != nrow(dt)) {
    stop(paste0("The column '", id_var, "' does not contain unique identifiers for all rows. GRTS sampling requires unique IDs."))
  }
  
  # Create the output directory if it doesn't exist
  if(!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      message(paste0("Creating output directory: ", output_dir))
      dir.create(output_dir, recursive = TRUE)
    }
  }
  # --- Calculate sample_size_per_seq ---
  total_rows <- nrow(dt)
  if (total_rows == 0) {
    message("Input data.table has 0 rows. No samples will be generated.")
    return(invisible(NULL))
  }
  if (seq_length > total_rows) {
    stop("seq_length cannot be greater than the total number of rows in dt when splitting into non-overlapping samples, as each sample must contain at least one point.")
  }
  
  base_sample_size <- floor(total_rows / seq_length)
  remainder <- total_rows %% seq_length
  
  sample_sizes_for_sequences <- rep(base_sample_size, seq_length)
  if (remainder > 0) {
    sample_sizes_for_sequences[1:remainder] <- sample_sizes_for_sequences[1:remainder] + 1
  }
  
  message(paste0("Total data points: ", total_rows))
  message(paste0("Dividing into ", seq_length, " spatially balanced samples."))
  message(paste0("Calculated sample sizes per sequence will be: ", paste(sort(unique(sample_sizes_for_sequences)), collapse = ", ")))
  
  
  # --- 2. Convert data.table to sf object and transform to UTM ---
  dt_for_sf <- data.table::copy(dt)
  
  sites_sf <- sf::st_as_sf(dt_for_sf,
                           coords = c(longitude_var, latitude_var),
                           crs = crs_value)
  
  # Ensure the ID column is part of the sf object attributes
  sites_sf[[id_var]] <- dt_for_sf[[id_var]]
  
  # Determine UTM zone and transform
  # This part of the code needs to be executed once for the initial transformation.
  # The bounding box of the entire dataset is used to determine the UTM zone.
  bbox_lon <- (st_bbox(sites_sf)[1] + st_bbox(sites_sf)[3]) / 2 # Mid-longitude of the data
  
  # Function to get UTM zone from longitude
  long2UTM <- function(long) {
    (floor((long + 180)/6) %% 60) + 1
  }
  
  utmZone <- long2UTM(bbox_lon)
  
  # Determine if it's Southern Hemisphere for the EPSG code and PROJ string
  is_south <- st_bbox(sites_sf)[2] < 0 # Check if the lowest latitude is south of equator
  
  # Construct the EPSG code for UTM zone (e.g., 326xx for North, 327xx for South)
  # sprintf("%02d", utmZone) ensures two digits for zone (e.g., 01 instead of 1)
  utm_epsg <- ifelse(is_south, sprintf("327%02d", utmZone), sprintf("326%02d", utmZone))
  
  # Attempt to use the EPSG code directly for transformation
  target_crs <- as.numeric(utm_epsg)
  
  message(paste0("Transforming data to UTM Zone ", utmZone, " (EPSG:", target_crs, ")"))
  sites_sf_proj <- sf::st_transform(sites_sf, crs = target_crs)
  
  
  # --- 3. Generate Spatially Balanced Samples using GRTS ---
  message("Starting generation of spatially balanced samples...")
  
  # We will iterate and remove sampled points from the sf object to ensure non-overlapping samples
  current_sites_sf_proj <- sites_sf_proj # Work with a mutable copy of the projected sf object
  if (return_result) {
    sampled_data_list <- list() # Initialize an empty list to store results
  }
  for (i in 1:seq_length) {
    current_sample_size <- sample_sizes_for_sequences[i]
    
    if (current_sample_size == 0) {
      message(paste0("Sample ", i, " has a size of 0. Skipping GRTS call and writing an empty file."))
      sampled_dt <- dt[0] # Create an empty data.table with same columns as dt
      if(!is.null(output_dir)){
        file_name <- sprintf("%s%d.csv", file_prefix, i)
        file_path <- file.path(output_dir, file_name)
        data.table::fwrite(sampled_dt, file_path)
        message(paste0("Wrote 0 rows to ", file_path))
      }
      next # Skip to the next iteration
    }
    
    # Ensure there are enough points remaining to draw the current sample size
    if (nrow(current_sites_sf_proj) < current_sample_size) {
      message(paste0("Warning: Not enough remaining points (", nrow(current_sites_sf_proj), ") to draw a sample of size ", current_sample_size, " for sequence ", i, ". Drawing all remaining points."))
      current_sample_size <- nrow(current_sites_sf_proj) # Draw all remaining points
      if (current_sample_size == 0) {
        message(paste0("No points remaining for sample ", i, ". Skipping GRTS call and writing an empty file."))
        sampled_dt <- dt[0]
        if(!is.null(output_dir)){
          file_name <- sprintf("%s%d.csv", file_prefix, i)
          file_path <- file.path(output_dir, file_name)
          data.table::fwrite(sampled_dt, file_path)
          message(paste0("Wrote 0 rows to ", file_path))
        }
        next # Skip to the next iteration
      }
    }
    
    
    message(paste0("Generating sample ", i, " of ", seq_length, " with target size ", current_sample_size, " from ", nrow(current_sites_sf_proj), " available points..."))
    
    # Perform GRTS sampling with the sf object
    grts_sample_result <- spsurvey::grts(
      sframe = current_sites_sf_proj, # Pass the sf object directly
      n_base = current_sample_size
    )
    
    # The output of grts is typically an sf object.
    # The sampled site IDs will be in the column specified by DesignID.
    sampled_ids <- grts_sample_result$sites_base[[id_var]]
    
    # Subset the original data.table using these IDs
    sampled_dt <- dt[dt[[id_var]] %in% sampled_ids]
    
    # Define the output file path
    if(return_result) {
      # If return_result is TRUE, store the sampled data.tables in a list
      if (!exists("sampled_data_list")) {
        sampled_data_list <- list()
      }
      sampled_data_list[[i]] <- sampled_dt
    } else {
      if(!is.null(output_dir)){
      file_name <- sprintf("%s%d.csv", file_prefix, i)
      file_path <- file.path(output_dir, file_name)
      
      # Write the sampled data.table to CSV
      data.table::fwrite(sampled_dt, file_path)
      message(paste0("Wrote ", nrow(sampled_dt), " spatially balanced (GRTS) rows to ", file_path))
      }
    }
    # Remove sampled IDs from the current_sites_sf_proj object to ensure non-overlapping samples
    current_sites_sf_proj <- current_sites_sf_proj[!(current_sites_sf_proj[[id_var]] %in% sampled_ids), ]
    message(paste0("Remaining points in sampling frame: ", nrow(current_sites_sf_proj)))
  }
  # --- 4. Return or finalize ---
  if (return_result) {
    message("Returning sampled data.tables as a list.")
    return(sampled_data_list)
  }
  invisible(NULL)
}