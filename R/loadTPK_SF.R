#' tile_map_package
#'
#' @description Creates ESRI tile map packages (.tpk)
#'
#' @param input.shape the boundary shape file
#' @param mapLEVELS the maplevels in the form 1-19, or 10-19 etc.
#' @param arc.user The ArcGIS online username
#' @param arc.user The ArcGIS online passsword
#' @param arc.user The ArcGIS online service URL
#' @param arc.user The ArcGIS online portal URL
#' @param arc.user The ArcGIS online domain URL
#'
#' @return Returns a list of file pathes to download the tpk files after generation.
#'
#' @noRd

loadTPK_SF<-function(input.shape=NULL, mapLEVELS="1-19", arc.user = NULL, arc.pw = NULL, serviceURL=NULL,
                     portalURL = NULL, domain.ServiceURL = NULL) {

  mapShape<-input.shape

  ##########################################################################################
  ##  REQUEST
  webM.crs<-("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")

  mapShape<-st_transform(mapShape, webM.crs)
  mapShapeBB<-st_bbox(mapShape)
  ##  (xmin, ymin, xmax, ymax)
  mapEXT= paste(mapShapeBB[1],mapShapeBB[2],mapShapeBB[3],mapShapeBB[4], sep = ",")
  aJsonFile<-tempfile()
  ##########################################################################################

  ##########################################################################################
  # 1. POST for token
  # 1.1. Portal token
  p.spec<-list(username=arc.user,
               password=arc.pw,
               client="referer",
               referer= serviceURL,
               expiration=20,
               f="json")
  p.token<-POST(url = portalURL, body= p.spec, write_disk(aJsonFile, overwrite = T))
  p.token<-fromJSON(aJsonFile)
  # 1.2. Server token
  s.spec<-list(token=p.token$token,
               serverURL=domain.ServiceURL,
               f="json")
  s.token<-POST(url = portalURL, body=s.spec, write_disk(aJsonFile, overwrite = T))
  s.token<-fromJSON(aJsonFile)

  ##########################
  # 2. Request tile package
  ex.spec<-list(f="json",
                tilePackage= "true",
                exportBy="LevelID",
                exportExtent=mapEXT,
                levels=mapLEVELS,
                token=s.token$token)
  ex.url <- paste0(serviceURL, "/exportTiles")
  ex.job<-GET(url=ex.url, query=ex.spec, write_disk(aJsonFile, overwrite = T))
  ex.job<-fromJSON(aJsonFile)

  # 3. Check job
  j.spec<-list(token=s.token$token,
               f="json")
  j.url <- paste0(serviceURL, "/jobs/",
                  ex.job$jobId)
  j.status<-"notYet"
  while (j.status!="esriJobSucceeded") {
    j.job<-GET(url=j.url, query=j.spec, write_disk(aJsonFile, overwrite = T))
    j.job<-fromJSON(aJsonFile)
    j.status<-j.job$jobStatus
    print(j.status)
    Sys.sleep(1)
  }

  # 4. Export
  dwl.url <-  paste0(serviceURL, "/jobs/",
                     ex.job$jobId, "/results/out_service_url")
  dwl.spec<-list(token=s.token$token,
                 f="json")
  dwl.job<-GET(dwl.url, query=dwl.spec, write_disk(aJsonFile, overwrite = T))
  dwl.job<-fromJSON(aJsonFile)
  dwl.link<-paste0(dwl.job$value, "/Layers.tpk")
  return(dwl.link)
}


checkTPKsizeSF<-function(input.shape=NULL, mapLEVELS="1-19", arc.user = NULL, arc.pw = NULL, serviceURL=NULL,
                         portalURL = NULL, domain.ServiceURL = NULL) {

  mapShape<-input.shape
  ##########################################################################################
  ##    SETTINGS
  ##########################################################################################
  ##  ACCESS

  ##  REQUEST
  webM.crs<-("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")

  mapShape<-st_transform(mapShape, webM.crs)
  mapShapeBB<-st_bbox(mapShape)
  ##  (xmin, ymin, xmax, ymax)
  mapEXT= paste(mapShapeBB[1],mapShapeBB[2],mapShapeBB[3],mapShapeBB[4], sep = ",")
  aJsonFile<-tempfile()
  ##########################################################################################

  ##########################################################################################
  # 1. POST for token
  # 1.1. Portal token
  p.spec<-list(username=arc.user,
               password=arc.pw,
               client="referer",
               referer= serviceURL,
               expiration=20,
               f="json")
  p.token<-POST(url = portalURL, body = p.spec, write_disk(aJsonFile, overwrite = T))
  p.token<-fromJSON(aJsonFile)
  # 1.2. Server token
  s.spec<-list(token=p.token$token,
               serverURL=domain.ServiceURL,
               f="json")
  s.token<-POST(url = portalURL, body =s.spec, write_disk(aJsonFile, overwrite = T))
  s.token<-fromJSON(aJsonFile)

  ##########################
  # 2. Request tile package
  # 2.1. Check Size
  ex.spec<-list(f="json",
                tilePackage= "true",
                exportBy="LevelID",
                exportExtent=mapEXT,
                levels=mapLEVELS,
                token=s.token$token)
  ch.url <- paste0(serviceURL, "/estimateExportTilesSize")
  ch.job<-GET(url=ch.url, query=ex.spec, write_disk(aJsonFile, overwrite = T))
  ch.job<-fromJSON(aJsonFile)
  print(mapLEVELS)
  # 3. Check job
  # a) initate
  j.spec<-list(token=s.token$token,
               f="json")
  j.url <- paste0(serviceURL, "/jobs/",
                  ch.job$jobId)
  j.status<-"notYet"
  while (j.status!="esriJobSucceeded") {
    j.job<-GET(url=j.url, query=j.spec, write_disk(aJsonFile, overwrite = T))
    j.job<-fromJSON(aJsonFile)
    j.status<-j.job$jobStatus
    if (length(j.status)==0 || j.status=="esriJobFailed") break()
  }

  if (length(j.status)==0 || j.status=="esriJobFailed") {
    j.tiles=180000
  } else{
    # b) tile count
    res.j.url <- paste0(serviceURL, "/jobs/",
                        ch.job$jobId, "/results/out_service_url")
    res.j.job<-GET(url=res.j.url, query=j.spec, write_disk(aJsonFile, overwrite = T))
    res.j.job<-fromJSON(aJsonFile)
    j.tiles<-res.j.job$value$totalTilesToExport
  }
  return(j.tiles)
}

splitShapTile<-function(inputFile=samp_raster_shp, zoomMax=19,
                        arc.user = arc.user,
                        arc.pw = arc.pw,
                        serviceURL = serviceURL,
                        portalURL = portalURL,
                        domain.ServiceURL = domain.ServiceURL,
                        targetTileSize =150000){
  library(sf);library(lwgeom)
  samp_raster_shp<-inputFile
  samp_raster_shp_list<-list()
  ML<-paste0("1-", zoomMax)

  # project to web mercator
  if(sf::st_is_longlat(samp_raster_shp)) samp_raster_shp<-st_transform(samp_raster_shp, 3857)

  # add column if there is none
  if(length(samp_raster_shp)<=2) samp_raster_shp$one<-1; samp_raster_shp$two<-2

  # check if single shape or multiple-->single shape no loop
  if(nrow(samp_raster_shp)==1) {
    tmpShp<-samp_raster_shp
    tileSize<-checkTPKsizeSF(input.shape = tmpShp, mapLEVELS = ML)
    # tmpShp_DF<-as.data.frame(tmpShp)
    # tmpShp_DF<-tmpShp_DF[rep(seq_len(nrow(tmpShp_DF)), each=2),]
    tmpShp_crs<-st_crs(tmpShp)$epsg
    #####################################
    ##  SPLIT shapes to desired tile size
    while(tileSize>targetTileSize) {
      tmpShp_DF<-as.data.frame(tmpShp %>% st_set_geometry(NULL))
      tmpShp_DF<-tmpShp_DF[rep(seq_len(nrow(tmpShp_DF)), each=2),]
      bb<-lapply(seq.int(nrow(tmpShp)),
                 function(x) st_bbox(tmpShp[x,]))
      mbblat<-lapply(seq.int(nrow(tmpShp)),
                     function(x) mean(c(st_bbox(tmpShp[x,])[2], st_bbox(tmpShp[x,])[4])))
      pt<-lapply(seq.int(nrow(tmpShp)),
                 function(x) st_sfc(st_linestring(matrix(c(bb[[x]][1], bb[[x]][3], mbblat[[x]],mbblat[[x]]), nrow = 2, ncol = 2 ))))
      tmpShp_split<-lapply(seq.int(nrow(tmpShp)),
                           function(x) st_split(st_geometry(tmpShp[x,]), pt[[x]]))
      tmpShp_split<-lapply(seq.int(nrow(tmpShp)),
                           function (y) lapply(seq.int(length(tmpShp_split[[y]][[1]])), function(x) st_cast(tmpShp_split[[y]][[1]][[x]] , "POLYGON")))
      tmpShp_split<-unlist(tmpShp_split, recursive = F)
      dfLength<-length(tmpShp_split)
      tmpShp_split<-do.call(st_sfc, tmpShp_split)
      tmpShp_split<-st_sf(tmpShp_DF[seq.int(dfLength),],geometry=tmpShp_split, crs = tmpShp_crs)
      tileSize<-sapply(seq.int(length(tmpShp_split$geom)),function(x) checkTPKsizeSF(input.shape = tmpShp_split[x,],
                                                                                     mapLEVELS = ML))
      tileSize<-max(tileSize)
      tmpShp<-tmpShp_split
      print(tileSize)
      samp_raster_shp_split<-tmpShp_split
    }
  } else {
    for (i in seq_along(st_geometry(samp_raster_shp))) {
      tmpShp<-samp_raster_shp[i,]
      tileSize<-checkTPKsizeSF(input.shape = tmpShp, mapLEVELS = ML)
      # tmpShp_DF<-as.data.frame(tmpShp)
      # tmpShp_DF<-tmpShp_DF[rep(seq_len(nrow(tmpShp_DF)), each=2),]
      tmpShp_crs<-st_crs(tmpShp)$epsg
      #####################################
      ##  SPLIT shapes to desired tile size
      while(tileSize>targetTileSize) {
        tmpShp_DF<-as.data.frame(tmpShp %>% st_set_geometry(NULL))
        tmpShp_DF<-tmpShp_DF[rep(seq_len(nrow(tmpShp_DF)), each=2),]
        bb<-lapply(seq.int(nrow(tmpShp)),
                   function(x) st_bbox(tmpShp[x,]))
        mbblat<-lapply(seq.int(nrow(tmpShp)),
                       function(x) mean(c(st_bbox(tmpShp[x,])[2], st_bbox(tmpShp[x,])[4])))
        pt<-lapply(seq.int(nrow(tmpShp)),
                   function(x) st_sfc(st_linestring(matrix(c(bb[[x]][1], bb[[x]][3], mbblat[[x]],mbblat[[x]]), nrow = 2, ncol = 2 ))))
        tmpShp_split<-lapply(seq.int(nrow(tmpShp)),
                             function(x) st_split(st_geometry(tmpShp[x,]), pt[[x]]))
        tmpShp_split<-lapply(seq.int(nrow(tmpShp)),
                             function (y) lapply(seq.int(length(tmpShp_split[[y]][[1]])), function(x) st_cast(tmpShp_split[[y]][[1]][[x]] , "POLYGON")))
        tmpShp_split<-unlist(tmpShp_split, recursive = F)
        dfLength<-length(tmpShp_split)
        tmpShp_split<-do.call(st_sfc, tmpShp_split)
        tmpShp_split<-st_sf(tmpShp_DF[seq.int(dfLength),],geometry=tmpShp_split, crs = tmpShp_crs)
        tileSize<-sapply(seq.int(length(tmpShp_split$geom)),function(x) checkTPKsizeSF(input.shape = tmpShp_split[x,],
                                                                                       mapLEVELS = ML))
        tileSize<-max(tileSize)
        tmpShp<-tmpShp_split
        print(tileSize)
      }
      samp_raster_shp_list[[i]]<-tmpShp
    }
    samp_raster_shp_split<-do.call(rbind, samp_raster_shp_list)
  }
  return(samp_raster_shp_split)
}



## alternative with voronoi from stackoverflow
splitShapTile2<-function(inputFile=samp_raster_shp, zoomMax=19,
                         arc.user = arc.user,
                         arc.pw = arc.pw,
                         serviceURL = serviceURL,
                         portalURL = portalURL,
                         domain.ServiceURL = domain.ServiceURL,
                         targetTileSize =150000){
  library(sf);library(lwgeom)
  samp_raster_shp<-inputFile
  samp_raster_shp_list<-list()
  ML<-paste0("1-", zoomMax)

  # project to latlong required for split function
  if(!sf::st_is_longlat(samp_raster_shp)) samp_raster_shp<-st_transform(samp_raster_shp, 4326)

  # add column if there is none
  if(length(samp_raster_shp)<=2) samp_raster_shp$one<-1; samp_raster_shp$two<-2

  # check if single shape or multiple-->single shape no loop
  if(nrow(samp_raster_shp)==1) {
    tmpShp<-samp_raster_shp
    tileSize<-checkTPKsizeSF(input.shape = tmpShp, mapLEVELS = ML)
    # tmpShp_DF<-as.data.frame(tmpShp)
    # tmpShp_DF<-tmpShp_DF[rep(seq_len(nrow(tmpShp_DF)), each=2),]
    tmpShp_crs<-st_crs(tmpShp)$epsg
    #####################################
    ##  SPLIT shapes to desired tile size
    k=2
    while(tileSize>targetTileSize) {
      tmpShp_split<-split_poly(tmpShp, k)
      tileSize<-sapply(seq.int(nrow(tmpShp_split)),function(x) checkTPKsizeSF(input.shape = tmpShp_split[x,],
                                                                              mapLEVELS = ML))
      tileSize<-max(tileSize)
      k<-k+1
    }
    samp_raster_shp_split<-tmpShp_split

  } else {
    for (i in seq_along(st_geometry(samp_raster_shp))) {
      tmpShp<-samp_raster_shp[i,]
      tileSize<-checkTPKsizeSF(input.shape = tmpShp, mapLEVELS = ML)
      # tmpShp_DF<-as.data.frame(tmpShp)
      # tmpShp_DF<-tmpShp_DF[rep(seq_len(nrow(tmpShp_DF)), each=2),]
      tmpShp_crs<-st_crs(tmpShp)$epsg
      #####################################
      ##  SPLIT shapes to desired tile size
      k=2
      while(tileSize>targetTileSize) {
        tmpShp_split<-split_poly(tmpShp, k)
        tileSize<-sapply(seq.int(nrow(tmpShp_split)),function(x) checkTPKsizeSF(input.shape = tmpShp_split[x,],
                                                                                mapLEVELS = ML))
        tileSize<-max(tileSize)
        k<-k+1
      }
      samp_raster_shp_list[[i]]<-tmpShp
    }
    samp_raster_shp_split<-do.call(rbind, samp_raster_shp_list)
  }
  return(samp_raster_shp_split)
}
