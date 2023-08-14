#' Shiny map module elper functions
#'
#' @description helper functions for map modules:
#' - location center
#' - point aggregation w sf
#' - point aggregation w geos
#'
#' @rdname internal
#' @noRd


## Get center
loc_center<-function(SHP=NULL) {
  suppressWarnings(
    cLonLat<-SHP %>%
      st_transform(3857) %>%
      st_centroid(warn=F) %>%
      st_transform(4326) %>%
      st_coordinates()
  )
  lon=median(cLonLat[,1])
  lat=median(cLonLat[,2])
  cLonLat<-c(lon=lon, lat=lat)
  return(cLonLat)
}

## aggregate points in boundaries
aggregatPointsToBoundssf<-function(bounds = NULL, points = NULL, cores = data.table::getDTthreads()) {

  # 1. Project --> faster!
  points<-project_to_utm(points)

  # 1.1. check crs
  oldcrs<-NULL
  if(st_crs(points)!=st_crs(bounds)) {
    oldcrs<-st_crs(bounds)
    bounds<-bounds %>% st_transform(st_crs(points))
  }
  cl<-parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)

  ##  2. N simu and packages
  simu<-nrow(bounds)
  pack_dp_sp<-c("sf", "dplyr")


  final<-foreach(i=1:simu, .packages = pack_dp_sp,
                 .combine=c,
                 .multicombine = F,
                 #.export = c("a"),
                 #.verbose = T,
                 .errorhandling="pass") %dopar% {
                   tmpbound<-bounds[i,]
                   tmppoints<-st_crop(points, st_bbox(tmpbound))
                   tmppoints<-tmppoints[tmpbound, op=st_within]
                   pop<-nrow(tmppoints)
                   return(pop)
                 }

  parallel::stopCluster(cl)
  bounds$Pop<-final
  if(!is.null(oldcrs)) bounds<-bounds %>% st_transform(oldcrs)
  return(bounds)
}

aggregatPointsToBoundsgeos<-function(bounds = NULL, points = NULL, cores = data.table::getDTthreads()) {

  # 1. Project --> faster!
  points<-project_to_utm(points)

  # 1.1. check crs
  oldcrs<-NULL
  if(st_crs(points)!=st_crs(bounds)) {
    oldcrs<-st_crs(bounds)
    bounds<-bounds %>% st_transform(st_crs(points))
  }

  boundssf<-bounds
  # bounds<-as_geos_geometry(bounds)
  # points<-as_geos_geometry(points)
  #points<-geos_strtree(points)

  cl<-parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)

  ##  2. N simu and packages
  simu<-nrow(bounds)
  intCounter<-1:simu
  pack_dp_sp<-c("sf", "dplyr", "geos")


  final<-foreach(i=1:simu, .packages = pack_dp_sp,
                 .combine=c,
                 .multicombine = F,
                 #.export = c("a"),
                 #.verbose = T,
                 .errorhandling="pass") %dopar% {
                   tmpbound<-bounds[i,]
                   tmpbound<-geos::as_geos_geometry(tmpbound)
                   tmppoints<-geos::as_geos_geometry(points)
                   tmppoints<-geos::geos_strtree(tmppoints)
                   tmppoints<-geos::geos_contains_matrix(tmpbound, tmppoints)

                   pop<-length(tmppoints[[1]])
                   return(pop)
                 }

  parallel::stopCluster(cl)
  boundssf$Pop<-final
  if(!is.null(oldcrs)) boundssf<-boundssf %>% st_transform(oldcrs)
  return(boundssf)
}

