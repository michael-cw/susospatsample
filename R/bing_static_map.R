#' R function to create basemaps for Survey Solutions
#'
#'
#'
#' @description Uses the basemaps package
#'
#' @return Filepath for download handler
#'
#' @noRd
#' @keywords internal



getStaticMapAsRaster <- function(shape = shape,
                                 mapservice = c("esri", "osm", "mapbox", "bing"),
                                 file_path = file.path(".", "data", "bingtest"),
                                 name_var = "CID",
                                 drawPolygon = FALSE,
                                 key = NULL,
                                 byShape = TRUE,
                                 singleMap = F) {
  ## 1. load the shape
  ## 1.1 File path, if non temporary

  if (is.null(file_path)) {
    fp <- tempdir()
  } else {
    if (dir.exists(file_path)) {
      fp <- file_path
    } else {
      dir.create(file_path)
      fp <- file_path
    }
  }

  if (byShape) {
    file_name <- shape %>%
      dplyr::select(.data[[name_var]]) %>%
      st_set_geometry(NULL)
    file_name <- file_name[1, 1]

    fn_list <- character(nrow(shape))
    keepShape <- shape
    for (i in 1:nrow(keepShape)) {
      shape <- keepShape[i, ]

      ## 2. package basemaps
      ext <- shape %>%
        st_transform(4326) %>%
        st_bbox()
      if (mapservice == "esri" | is.null(key)) {
        ras3 <- basemaps::basemap_stars(ext = ext, map_service = "esri", map_type = "world_imagery")
        fn <- sprintf("%s_%s_%d.tif", mapservice, file_name, i)
        fn <- file.path(fp, fn)
        fn_list[i] <- fn
        write_stars(ras3, dsn = fn, type = "Byte")
      } else if (mapservice == "osm") {
        ras4 <- basemaps::basemap_stars(ext = ext, map_service = "osm", map_type = "streets")
        fn <- sprintf("%s_%s_%d.tif", mapservice, file_name, i)
        fn <- file.path(fp, fn)
        fn_list[i] <- fn
        write_stars(ras4, dsn = fn, type = "Byte")
      } else if (mapservice == "mapbox") {
        ras6 <- basemaps::basemap_stars(
          ext = ext, map_service = "mapbox", force = F,
          map_type = "hybrid", map_token = key
        )
        fn <- sprintf("%s_%s_%d.tif", mapservice, file_name, i)
        fn <- file.path(fp, fn)
        fn_list[i] <- fn
        write_stars(ras6, dsn = fn, type = "Byte")
      } else if (mapservice == "bing") {
        ## 3. bing
        ## 3.1 lat long mapbounds
        ma <- paste(ext["ymin"], ext["xmin"], ext["ymax"], ext["xmax"], sep = ",")
        ## 3.1.1
        res_px <- 700

        ## 3.2. URL
        url <- parse_url("https://dev.virtualearth.net")
        url$path <- "/REST/v1/Imagery/Map/AerialWithLabels"

        if (drawPolygon) {
          ## No Polygon on BING
          # coords<-as.data.frame(st_coordinates(st_as_sfc(st_bbox(st_transform(shape, 4326)))))
          coords <- as.data.frame(st_coordinates(((st_transform(shape, 4326)))))


          coords$pair <- paste(coords$Y, coords$X, sep = ",")
          coords_str <- paste(coords$pair, collapse = "_")
          colorl <- "FFF5140C"
          ls <- 3
          colora <- "00009900"
          polyParameter <- paste("p", colora, colorl, ls, sep = ",")
          polyParameter <- paste(polyParameter, coords_str, sep = ";")
          url$query <- list(
            key = key,
            mapArea = URLencode(ma),
            mapSize = paste(res_px, res_px, sep = ","),
            format = "png",
            dc = polyParameter
          )
        } else {
          ## No Polygon on BING
          url$query <- list(
            key = key,
            mapArea = URLencode(ma),
            mapSize = paste(res_px, res_px, sep = ","),
            format = "png"
          )
        }

        ## 3.3.2 Build and Request
        httr_string <- build_url(url = url)
        tf <- tempfile(fileext = ".png")
        ## 3.3. GET
        # tf<-"./data/bingtest/checkdraw.png"
        map2 <- GET(httr_string)
        writeBin(content(map2, "raw", type = "image"), tf)
        image_write(image_convert(image_read(tf),
          format = "PNG24"
        ), tf)
        ras7 <- stars::read_stars(tf)
        unlink(tf)
        ras7 <- st_set_bbox(ras7, value = ext)
        fn <- sprintf("%s_%s_%d.tif", mapservice, file_name, i)
        fn <- file.path(fp, fn)
        fn_list[i] <- fn
        write_stars(ras7, dsn = fn, type = "Byte")
      }
    }
    ### create single map
    if (singleMap) {
      fn <- file.path(fp, paste0(file_name, "_ALL.tif"))
      # tmpras<-list(length(fn_list))
      # filestring<-character(length(fn_list))
      # for(i in 1:length(fn_list)) {
      #   tmpras[[i]]<-read_stars(fn_list[i])
      #   filestring[i]<-sprintf("tmpras[[%i]]", i)
      #
      # }
      # filestrings<-paste(filestring, collapse = ",")
      # filestrings<-paste0("st_mosaic(", filestrings, ")")
      # singleMap<-(eval(parse(text = filestrings)))

      # select maptype
      maptype <- switch(mapservice,
        "esri" = "world_imagery",
        "osm" = "streets",
        "mapbox" = "hybrid",
        "bing" = "na"
      )
      ext <- keepShape %>%
        st_transform(4326) %>%
        st_bbox()
      singleMap <- basemaps::basemap_stars(
        ext = ext, map_service = mapservice, force = F,
        map_type = maptype, map_token = key
      )
      write_stars(singleMap, dsn = fn, driver = "GTiff", type = "Byte")
    }
  } else {
    file_name <- shape %>%
      select(.data[[name_var]]) %>%
      st_set_geometry(NULL)
    file_name <- file_name[1, 1]

    ## 2. package basemaps
    ext <- shape %>%
      st_transform(4326) %>%
      st_bbox()
    if (mapservice == "esri") {
      ras3 <- basemaps::basemap_stars(ext = ext, map_service = "esri", map_type = "world_imagery")
      fn <- sprintf("%s_%s.tif", mapservice, file_name)
      fn <- file.path(fp, fn)
      fn_list <- fn
      write_stars(ras3, dsn = fn, type = "Byte")
    } else if (mapservice == "osm") {
      ras4 <- basemaps::basemap_stars(ext = ext, map_service = "osm", map_type = "streets")
      fn <- sprintf("%s_%s.tif", mapservice, file_name)
      fn <- file.path(fp, fn)
      fn_list <- fn
      write_stars(ras4, dsn = fn, type = "Byte")
    } else if (mapservice == "mapbox") {
      ras6 <- basemaps::basemap_stars(ext = ext, map_service = "mapbox", map_type = "hybrid", map_token = key)
      # write_stars(ras6, dsn = "./data/bingtest/mapbox_hybrid_Lat9236Lon569.tif")

      fn <- sprintf("%s_%s.tif", mapservice, file_name)
      fn <- file.path(fp, fn)
      fn_list <- fn
      write_stars(ras6, dsn = fn, type = "Byte")
    } else if (mapservice == "bing") {
      ## 3. bing
      ## 3.1 lat long mapbounds
      ma <- paste(ext["ymin"], ext["xmin"], ext["ymax"], ext["xmax"], sep = ",")
      ## 3.1.1
      res_px <- 700

      ## 3.2. URL
      url <- parse_url("https://dev.virtualearth.net")
      url$path <- "/REST/v1/Imagery/Map/AerialWithLabels"

      if (drawPolygon) {
        ## No Polygon on BING
        # coords<-as.data.frame(st_coordinates(st_as_sfc(st_bbox(st_transform(shape, 4326)))))
        coords <- as.data.frame(st_coordinates(((st_transform(shape, 4326)))))

        coords$pair <- paste(coords$Y, coords$X, sep = ",")
        coords_str <- paste(coords$pair, collapse = "_")
        colorl <- "FFF5140C"
        ls <- 3
        colora <- "00009900"
        polyParameter <- paste("p", colora, colorl, ls, sep = ",")
        polyParameter <- paste(polyParameter, coords_str, sep = ";")
        url$query <- list(
          key = key,
          mapArea = URLencode(ma),
          mapSize = paste(res_px, res_px, sep = ","),
          format = "png",
          dc = polyParameter
        )
      } else {
        ## No Polygon on BING
        url$query <- list(
          key = key,
          mapArea = URLencode(ma),
          mapSize = paste(res_px, res_px, sep = ","),
          format = "png"
        )
      }

      ## 3.3.2 Build and Request
      httr_string <- build_url(url = url)
      tf <- tempfile(fileext = ".png")
      ## 3.3. GET
      # tf<-"./data/bingtest/checkdraw.png"
      map2 <- GET(httr_string)
      writeBin(content(map2, "raw", type = "image"), tf)
      image_write(image_convert(image_read(tf),
        format = "PNG24"
      ), tf)
      ras7 <- stars::read_stars(tf)
      unlink(tf)
      ras7 <- st_set_bbox(ras7, value = ext)
      # write_stars(ras7, dsn = "./data/bingtest/bing9236_569_subcell.tif", driver = "GTiff")
      fn <- sprintf("%s_%s.tif", mapservice, file_name)
      fn <- file.path(fp, fn)
      fn_list <- fn
      write_stars(ras7, dsn = fn, type = "Byte")
    }
  }

  return(fn_list)
}
