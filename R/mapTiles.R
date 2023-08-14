##########################
##  1. loop over shapes
##  2. check number of tiles
##  3. if exceeds split polygon
##  4. check number of tiles...

splitShapTile<-function(inputFile=samp_raster_shp, zoomMax=19,
                        arc.user = arc.user,
                        arc.pw = arc.pw,
                        serviceURL = serviceURL,
                        portalURL = portalURL,
                        domain.ServiceURL = domain.ServiceURL,
                        targetTileSize =150000){
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
