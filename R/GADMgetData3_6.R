#############################################
##  New function to load GADM data
##  replaces getData in raster for GADM only

# https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_AUT_0_sp.rds
# https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_AUT_1_sp.rds

GADM.getData<-function(name="GADM",country,level,path, sp.Library="sp"){
  version<-"3.6"
  v<-as.numeric(version)*10
  DIR<-ifelse(sp.Library=="sp", "/Rsp/", "/Rsf/")
  tf<-tempfile()

  ##  1. Create path
  file.name<-paste0("gadm", v, "_", country, "_", level, "_", sp.Library, ".rds")
  f.path<-file.path(path, file.name)
  url <- paste0("https://biogeo.ucdavis.edu/data/gadm", version, DIR, file.name)

  ##  2. check local availability
  if (file.exists(f.path)) {
    GADM.map<- readRDS(f.path)
    st_crs(GADM.map)<-st_crs(GADM.map)
    return(GADM.map)
  } else {
    GADM.header<-GET(url, write_disk(f.path))
    if (GADM.header$status_code==200){
      GADM.map<- readRDS(f.path)
      st_crs(GADM.map)<-st_crs(GADM.map)
      return(GADM.map)
    } else {
      "No Map available! Are you sure you your request is correctly specified?"
    }
  }
}
