#' Loop for maps
#'
#' @noRd
#' @keywords internal
#'
.loopForMapsApi<-function(tab, ...) {
  if(!is.null(tab) && nrow(tab)==100) {
    tabList<-list()
    skip<-0
    nmaps<-100
    tabList[[paste0("skip_", skip)]]<-tab
    while(nmaps==100){
      skip<-skip+100
      tab<-SurveySolutionsAPI::suso_mapinfo(..., take = 100, skip = skip)
      nmaps<-nrow(tab)
      tabList[[paste0("skip_", skip)]]<-tab
    }
    tab<-data.table::rbindlist(tabList)
  }
  return(tab)
}
