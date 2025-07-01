create_grid_ras_old_SF<-function(shp, cellsize=1000, stratum = NULL, bigN=FALSE, inShinyApp=T, return.Raster=F){

  ##  1. Projections
  ##  1.1 Function to detect UTM zone
  ##    -->take max longitude
  maxLong<-st_bbox(shp)[3]
  long2UTM <- function(long=maxLong) {
    (floor((long + 180)/6) %% 60) + 1
  }

  utmZone<-long2UTM()
  epsg<-ifelse(st_bbox(shp)[4]<=0, sprintf("327%02d", utmZone) ,sprintf("326%02d", utmZone))
  crs=(paste0("+proj=utm +south +zone=", utmZone, " +ellps=WGS84 +towgs84=0,0,0, +init=epsg:",epsg))
  shp_crs<-st_crs(shp)
  ##  2. Create raster based on METERS
  ##    --> Project file to UTM
  shp<-shp %>% st_transform(as.numeric(epsg))

  if (inShinyApp) incProgress(0.1)


  if (is.null(stratum)){
    ## 2.1. No STRATUM
    #grid[] = rep(1, n_cells)
    if (inShinyApp) incProgress(0.1)

    grid<-st_rasterize(shp, dx=cellsize, dy=cellsize)
    #grid<-fasterize(shp, grid, fun="max")
    ##################################
    ##  MAYBe plot RASTER instead of POLY?
    if (!return.Raster) {
      grid<-st_as_sf(grid)
      #grid<-qm_rasterToPolygons(grid)
      data.table::setnames(grid, 1, "stratum_numeric")
      grid<-grid[!is.na(grid$stratum_numeric),]
      grid<-grid %>% st_transform(shp_crs)
    }
    if (inShinyApp) incProgress(0.4)
    #grid<-spTransform(grid, shp_crs)
  } else {
    ## 2.2. STRATUM
    ##  Stratum ID must be numeric --> transform if not
    if (inShinyApp) incProgress(0.1)
    con<-FALSE
    if (!is.numeric(shp[,stratum, drop = T])) {
      if(!is.factor(shp[,stratum, drop = T])) shp[,stratum]<-as.factor(shp[,stratum, drop=T])
      oldStrat<-levels(shp[,stratum, drop=T])
      oldNames<-data.frame(n=paste0(oldStrat), id=1:length(oldStrat))
      names(oldNames)<-c(stratum, "stratum_numeric")
      shp[,"stratum_numeric"]<-as.numeric((shp[,stratum, drop=T]))
      con<-TRUE
    } else {
      shp[,"stratum_numeric"]<-shp[,stratum, drop=T]
    }
    if (bigN) {
      ##  FOR LARGE
      if (inShinyApp) incProgress(0.1)
      gridList<-list()
      ## A. Parallel setup (LINUX)
      cores<-data.table::getDTthreads()
      cl<-parallel::makeCluster(cores)
      doParallel::registerDoParallel(cl)
      pack_dp_sp<-c("sp", paste0(package_dependencies("sp")[[1]]))
      pack_dp_sp<-c(pack_dp_sp, "raster", "sp", "rgdal", "broom", "data.table", "plyr", "dplyr", "shiny")
      shp$stratum<-as.factor(shp@data[,stratum])
      gridList<-foreach(st=oldStrat, .packages = pack_dp_sp,
                        .combine="c",
                        .multicombine = T,
                        #.export = c("a"),
                        #.verbose = T,
                        .errorhandling="pass") %dopar% {
                          ##  create different GRID

                          shp_sub<-shp[shp$stratum==st,]
                          st_num<-as.numeric(shp_sub$stratum[1])
                          shp_sub<-aggregate(shp_sub)
                          grid<-raster(crs=crs)
                          extend_grid<-extent(bbox(shp_sub))
                          extent(grid)<-extend_grid
                          xl<-cellsize
                          yl<-cellsize
                          res(grid)<-c(xl, yl)
                          n_cells<-grid@nrows*grid@ncols

                          ######################################
                          ##  Reprojection takes place BEFOR filling the grid
                          grid[] = rep(st_num, n_cells)
                          # grid<-projectRaster(grid, crs = shp_crs)
                          # shp<-spTransform(shp, shp_crs)
                          #
                          grid<-rasterize(shp_sub, grid, mask = T)

                          if (!return.Raster) {
                            grid<-rasterToPolygons(grid)
                            names(grid@data)<-c("ID")
                            grid<-grid[!is.na(grid$ID),]
                          }
                          grid<-projectRaster(grid, crs = shp_crs)
                          return(grid)
                          # gridList[[st]]<-spTransform(grid, shp_crs)
                        }
      parallel::stopCluster(cl)
      if (inShinyApp) incProgress(0.4)
      gridList$tolerance<-1
      gridList$filename<-paste0("data/TEMP_RAS/main_raster_", Sys.time(), ".tif")
      gridList$overwrite<-TRUE
      grid<-do.call(merge, gridList)
    } else {
      ##  MULITCORE LOOP for extraction
      #future::plan(future::sequential)
      cores<-data.table::getDTthreads()-1
      cl<-parallel::makeCluster(cores)
      doParallel::registerDoParallel(cl)
      simu<-nrow(shp)
      pack_dp_sp<-c("stars",
                    "plyr", "dplyr", "sf")
      if (inShinyApp) incProgress(0.1)
      final<-foreach(i=1:simu, .packages = pack_dp_sp,
                     .combine="c",
                     .multicombine = T,
                     #.export = c("a"),
                     #.verbose = T,
                     .errorhandling="pass") %dopar% {
                       tmp.poly<-shp[i,]
                       ## Check Size within Strat
                       ## if dimension x/y are smaller cellsize, adjust cellsize
                       xdim<-floor(st_bbox(tmp.poly)["xmax"]-st_bbox(tmp.poly)["xmin"])
                       ydim<-floor(st_bbox(tmp.poly)["ymax"]-st_bbox(tmp.poly)["ymin"])
                       if(xdim<cellsize | ydim<cellsize) {
                         ## Must be lower than 1/2
                         celllim<-floor(min(xdim, ydim)/2)

                         ## Decrease until fit
                         i = 2
                         cellsize_replace<-cellsize
                         while(cellsize_replace>celllim) {
                           i=i+1

                           if(cellsize %% i == 0){
                             cellsize_replace<-cellsize/i
                           }
                         }
                         tmp.poly.ras<-st_rasterize(tmp.poly, dy=cellsize_replace, dx=cellsize_replace)
                       } else{
                         tmp.poly.ras<-st_rasterize(tmp.poly, dy=cellsize, dx=cellsize)
                       }
                       tmp.mask.poly<-st_as_sf(tmp.poly.ras)
                       tmp.mask.poly$stratum_numeric<-tmp.poly$stratum_numeric
                       tmp.mask.poly$stratum<-tmp.mask.poly$stratum_numeric
                       tmp.mask.poly$Cells<-nrow(tmp.mask.poly)
                       tmp.mask.poly$Pop<-tmp.mask.poly$Cells
                       tmp.mask.poly<-tmp.mask.poly %>% st_transform(shp_crs)
                       return(list(tmp.mask.poly))
                     }
      parallel::stopCluster(cl)

      ##  MULITCORE LOOP for putting things together.
      #future::plan(future::sequential)
      cores<-data.table::getDTthreads()-1
      cl<-parallel::makeCluster(cores)
      clusterEvalQ(cl,
                   {requireNamespace("sf")}
                   )
      popListShp<- final[parSapply(cl, seq.int(length(final)), function(x) class(final[[x]])[1]=="sf")]
      parallel::stopCluster(cl)

      grid<-rbind.parallel(popListShp, cores)

      if (inShinyApp) incProgress(0.4)
    }
  }
  if (inShinyApp) setProgress(1)
  return(grid)
}


####################################################################################
## Fast rbind for sf objects
## from: https://stackoverflow.com/questions/7224938/can-rbind-be-parallelized-in-r
####################################################################################
# rbind.parallel <- function(list,ncore)
# {
#   do.call.rbind<-function(x){do.call(rbind,x)}
#   cl<-parallel::makeCluster(ncore)
#   list.split<-split(list,rep(1:ncore,length(list)+1)[1:length(list)])
#   list.join<-parallel::parLapply(cl,list.split,do.call.rbind)
#   parallel::stopCluster(cl)
#   list.out<-do.call(rbind,list.join)
#   return(list.out)
# }

project_to_utm<-function(shp) {
  ## 1. get zone
  maxLong<-st_bbox(shp)[3]
  long2UTM <- function(long=maxLong) {
    (floor((long + 180)/6) %% 60) + 1
  }
  ## 2. Create CRS string
  utmZone<-long2UTM()
  epsg<-ifelse(st_bbox(shp)[4]<=0, sprintf("327%2d", utmZone) ,sprintf("326%2d", utmZone))
  crs=(paste0("+proj=utm +south +zone=", utmZone, " +ellps=WGS84 +towgs84=0,0,0, +init=epsg:",epsg))
  ## 3. Transform
  shp<-tryCatch(
    {shp %>% st_transform(crs)},
    error = function(e) {shp %>% st_transform(as.numeric(epsg))})
  return(shp)
}


long2UTM <- function(long=maxLong) {
  (floor((long + 180)/6) %% 60) + 1
}




