################################
## RASTER FUNCTIONS

## 1. Fast raster replace NA (attention Memory!)
ras_NA_to_0<-function(rf=rst) {
  rfDF <- raster::values(rf)
  crsold<-raster::proj4string(rf)
  rfDF[is.na(rfDF)]<-0
  raster::values(rf)<-rfDF
  tmpfile<- tempfile(fileext = ".tif")
  writeRaster(rf, tmpfile, overwrite=TRUE, format = "GTiff")
  rm(rf)
  rf<-raster(tmpfile)
  sp::proj4string(rf)<-crsold
  return(rf)
}

ras_strat_kmeans<-function(rf=pop_raster, n_strata=10) {
  rstDF <- raster::values(rf)
  km<-kmeans(rstDF, centers = n_strata, iter.max = 50)
  kmClust <- vector(mode = "integer", length = ncell(rf))
  kmClust<- km$cluster
  tmpRstKM <- raster(rf[[1]])
  raster::values(tmpRstKM) <- kmClust
  return(tmpRstKM)
}

ras_strat_clara<-function(rf=pop_raster, n_strata=10) {
  rstDF <- raster::values(rf)
  km<-clara(rstDF, k = n_strata, metric = "manhattan")
  kmClust <- vector(mode = "integer", length = ncell(rf))
  kmClust<- km$cluster
  tmpRstKM <- raster(rf[[1]])
  raster::values(tmpRstKM) <- kmClust
  return(tmpRstKM)
}
