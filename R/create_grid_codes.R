####################
## Lambert_Azimuthal_Equal_Area
####################

create_lae_newid<-function(sf_points=NULL, cell = 1000, crs = 3035) {
  ## 0. Create ID
  sf_points$id<-1:nrow(sf_points)
  sf_points1<-sf_points %>% dplyr::select(id)
  ## 1. transform
  sf_points1<-sf_points1 %>% st_transform(crs = crs)
  ## 1.1. If POLY extract poly
  if(st_geometry_type(sf_points1, by_geometry = F) == "POLYGON" |
     st_geometry_type(sf_points1, by_geometry = F) == "MULTIPOLYGON") {
    sf_points1<-as.data.frame(st_coordinates(st_centroid(sf_points1)))
    sf_points1<-st_as_sf(sf_points1, coords = c("X", "Y"), crs = crs)
  }
  ## 2. get coordinates
  tmp.coord<-data.table::data.table(st_coordinates(sf_points1));rm(sf_points1)
  tmp.coord<-cbind(sf_points, tmp.coord$X, tmp.coord$Y)
  data.table::setnames(tmp.coord, "tmp.coord.X", "X", skip_absent = T)
  data.table::setnames(tmp.coord, "tmp.coord.Y", "Y", skip_absent = T)
  tmp.coord<-tmp.coord[!is.na(tmp.coord$X),]
  #tmp.coord<-tmp.coord[!is.na(tmp.coord$Y),]
  
  pref<-ifelse(cell/1000<1, paste0(cell, "m"), paste0(cell/1000, "km"))
  tmp.coord$GRD_NEWID<-sprintf("%s%s%i%s%i",
                               pref,
                               ifelse(floor(tmp.coord$X/cell)>=0, "E", "W"),
                               abs(floor(tmp.coord$X/cell)),
                               ifelse(floor(tmp.coord$Y/cell)>=0, "N", "S"),
                               abs(floor(tmp.coord$Y/cell)))
  
  ### RETURN ALL even when ID is missing.
  tmp.coord<-tmp.coord %>% 
    dplyr::select(id, GRD_NEWID) %>% 
    st_set_geometry(NULL)
  sf_points<-merge(sf_points, 
                   tmp.coord, 
                   all.x = T, 
                   by = "id")
  return(sf_points)
}
