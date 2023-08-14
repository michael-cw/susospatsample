#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @importFrom cluster clara
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom data.table as.data.table copy data.table fread fwrite getDTthreads is.data.table rbindlist setkey setkeyv setnames setorderv
#' @importFrom dismo voronoi
#' @importFrom dplyr as_tibble bind_cols filter group_by mutate n n_distinct select summarise
#' @importFrom DT datatable dataTableOutput dataTableProxy formatStyle renderDataTable replaceData selectRows
#' @importFrom fasterize fasterize raster
#' @importFrom flextable style
#' @importFrom foreach foreach `%dopar%`
#' @importFrom geosphere distHaversine
#' @importFrom ggmap get_map ggmap
#' @importFrom ggplot2 aes annotate coord_equal geom_histogram geom_sf geom_vline ggplot ggtitle scale_color_discrete scale_fill_discrete scale_fill_viridis_c scale_y_continuous theme
#' @importFrom grDevices topo.colors
#' @importFrom htmltools br div h1 h3 h4 HTML img includeCSS tagList
#' @importFrom httr GET POST write_disk build_url content parse_url
#' @importFrom jsonlite fromJSON unbox
#' @importFrom leaflet renderLeaflet addPolygons addProviderTiles addTiles clearShapes colorNumeric fitBounds leaflet leafletOutput leafletProxy setView colorFactor providers
#' @importFrom lwgeom st_split
#' @importFrom magick image_convert image_read image_write
#' @importFrom mapdeck add_arc add_grid add_path add_polygon add_scatterplot clear_arc clear_grid clear_polygon clear_scatterplot mapdeck mapdeck_style mapdeck_update mapdeck_view mapdeckOutput renderMapdeck
#' @importFrom mapview mapview mapviewOutput npts
#' @importFrom Matrix head mean print summary which
#' @importFrom officer body_add_break body_add_gg body_add_par body_add_table read_docx set_doc_properties body_add
#' @importFrom parallel clusterEvalQ detectCores makeCluster parLapply parSapply stopCluster
#' @importFrom plyr . join
#' @importFrom raster aggregate as.data.frame as.factor bbox crop crs cut extent fromDisk getValues is.factor isLonLat levels mean merge modal ncell ncol nrow print proj4string projectRaster raster rasterize rasterToPolygons res spplot subset summary tmpDir unique values writeRaster xFromCell xres yFromCell yres
#' @importFrom readr read_csv
#' @importFrom rhandsontable hot_col hot_to_r renderRHandsontable rhandsontable rHandsontableOutput set_data
#' @importFrom rmapshaper ms_simplify
#' @importFrom sampling getdata strata
#' @import sf
## #' @importFrom sf st_area st_as_sf st_as_sfc st_bbox st_buffer st_cast st_centroid st_coordinates st_crop st_crs st_geometry st_geometry_type st_intersection st_is_empty st_is_longlat st_is_valid st_linestring st_make_grid st_make_valid st_read st_sample st_set_geometry st_sf st_sfc st_simplify st_transform st_write
#' @importFrom shinyalert shinyalert useShinyalert
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinydashboard box updateTabItems
#' @importFrom shinyjs click disable disabled enable hidden hide hideElement reset show showElement useShinyjs
#' @importFrom sp bbox CRS merge proj4string split spplot summary
#' @importFrom spex qm_rasterToPolygons
#' @importFrom stars geom_stars read_stars st_as_stars st_dimensions st_rasterize st_set_bbox st_warp write_stars
#' @importFrom stats complete.cases kmeans median runif sd setNames
#' @importFrom stringr str_split
#' @importFrom SurveySolutionsAPI suso_get_api_key
#' @importFrom terra aggregate as.data.frame as.factor crop crs ext is.factor levels mean median merge modal ncell ncol nrow rast rasterize readRDS res split subset summary unique values vect voronoi writeRaster xFromCell xres yFromCell yres
#' @importFrom tools package_dependencies
## #' @importFrom utils download.file head unzip URLencode zip
## #' @importFrom zip unzip zip


## all for shapeUpload DB
## #' @import DBI
#' @import rmapshaper
## #' @import RPostgres
## #' @import RPostgreSQL
## #' @import rpostgis
#'

## @importFrom geos as_geos_geometry geos_strtree geos_contains_matrix
## usethis namespace: end
NULL
