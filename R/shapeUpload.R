######################
##  SHAPE UPLOAD v2.0
######################

shapeLoad2 <- function(path = NULL, lay = NUL, sp.Library = "sf") {
  outlist <- list()
  lay <- str_split(lay, pattern = ".shp")[[1]][1]
  if (sp.Library == "sp") {
    ###############################################################################################
    #   Upload the shape SP
    ###############################################################################################
    harare_landuse3 <- readOGR(dsn = path, layer = lay)
    outlist[[1]] <- ogrInfo(dsn = path, layer = lay)
    ##  Creat common id for tidy with rownames
    harare_landuse3$id <- as.numeric(rownames(harare_landuse3@data))
    suppressWarnings(
      harare_landuse3_df <- tidy(harare_landuse3)
    )
    harare_landuse3_df <- join(harare_landuse3_df, harare_landuse3@data, by = "id")
    harare_landuse3_df <- data.table(harare_landuse3_df)
    #############################################################################################################
    ## Create the grid from start
    ##    1. Grid is created from start
    ##    2. If modify buttion is used, then grid is created again.
    ############################################################################################################

    # sp_grd_strat<-create_grid_ras_old(shp=harare_landuse3_type1, cellsize =1000, stratum = "type1")
    # names(sp_grd_strat@data)<-c("type1_num", "type1")
    # sp_grd_strat$n_id<-as.numeric(row.names(sp_grd_strat@data))

    outlist[[2]] <- harare_landuse3
    outlist[[3]] <- harare_landuse3_df
    return(outlist)
  } else {
    ###############################################################################################
    #   Upload the shape SF
    ###############################################################################################
    harare_landuse3 <- st_read(dsn = path, layer = lay)
    outlist[[1]] <- st_crs(harare_landuse3)[[2]]
    outlist[[2]] <- harare_landuse3
    return(outlist)
  }
}
###############################################################################################
#                         Simplify Shape (if complicated, ATTENTION only for mapping)
###############################################################################################
ms_simpl_complexShape <- function(SHP = Boundaries, level = 100000, keep = NULL) {
  ## Simplify for Plot!
  ## 1. calculate complexity level in input shp
  complShp <- npts(SHP)
  ## 2. check keep, if NULL set keep to level
  if (is.null(keep)) keep <- round(level / complShp, 2)
  if (complShp > level) {
    ## Simplify
    suppressMessages(
      suppressWarnings(
        SHP <- rmapshaper::ms_simplify(
          SHP,
          sys = T,
          keep = keep,
          keep_shapes = T
        )
      )
    )
    ## Validate
    crsOld <- st_crs(SHP)
    ##  ii. Remove empty geometries first
    if (sum(st_is_empty(SHP)) > 0) {
      SHP <- SHP[!st_is_empty(SHP), ]
    }
    ##  iii. VALIDATION STEP with SF
    if (sum(sf::st_is_valid(SHP)) != nrow(SHP)) {
      suppressWarnings(
        SHP <- SHP %>%
          st_transform(3857) %>%
          st_make_valid() %>%
          # st_cast("POLYGON", warn = F, do_split = F) %>%
          st_buffer(0.0) %>%
          st_transform(crsOld)
      )
    }

    ##  iv. Remove empty geometries
    if (sum(st_is_empty(SHP)) > 0) {
      SHP <- SHP[!st_is_empty(SHP), ]
    }
  }
  return(SHP)
}

###############################################################################################
#                         VALIDATION AND TRANSFORMATION (INCL. db WRITE)
###############################################################################################
shapeLoad2_cleanToDB <- function(SHP = tmp.shp, shpName = NULL, writeToDB = T, localpath = NULL, ...) {
  crsOld <- st_crs(SHP)
  ##  ii. Remove empty geometries first
  if (sum(st_is_empty(SHP)) > 0) {
    SHP <- SHP[!st_is_empty(SHP), ]
  }
  ##  iii. VALIDATION STEP with SF
  if (sum(sf::st_is_valid(SHP)) != nrow(SHP)) {
    suppressWarnings(
      SHP <- SHP %>%
        st_transform(3857) %>%
        st_make_valid() %>%
        # st_cast("POLYGON", warn = F, do_split = F) %>%
        st_buffer(0.0) %>%
        st_transform(crsOld)
    )
  }

  ##  iv. Remove empty geometries
  if (sum(st_is_empty(SHP)) > 0) {
    SHP <- SHP[!st_is_empty(SHP), ]
  }

  ##########################
  ## V. Write ORIG file to DB
  ## a. prepare
  if (writeToDB) {
    tableName <- stringr::str_remove_all(shpName, ".shp")
    tableName <- iconv(tableName, from = "UTF-8", to = "ASCII//TRANSLIT")
    tableName <- stringr::str_remove_all(tableName, "[^[:alnum:]]")
    ## b. write & return list
    dbTableList <- writeSFtoDB(object = SHP, fn = tableName, localpath = localpath, ...)
  }

  ## vi. RMAPSHAPER MS simplify


  ##  vii. RE-VALIDATION STEP with SF
  # if (sum(st_is_valid(SHP))!=nrow(SHP)) {
  #   # showNotification("Adjusting Validity. This may take a while!",
  #   #                  typ = "message", id = "simplifyMap2", duration = NULL)
  #   suppressWarnings(
  #     SHP<-SHP %>%
  #       st_transform(3857) %>%
  #       st_make_valid() %>%
  #       st_cast("MULTIPOLYGON") %>%
  #       st_buffer(0.0) %>%
  #       st_transform(crsOld)
  #   )
  #
  # }

  return(SHP)
}
###############################################################################################
#   POSTGRES          POSTGRES          POSTGRES            POSTGRES
###############################################################################################
writeSFtoDB <- function(object = NULL, dbname = NULL, host = "localhost",
                        user = NULL, password = NULL,
                        fn = NULL, listTables = F, DBtype = "pg", localpath = NULL) {
  allTables <- NULL
  if (DBtype == "pg") {
    ## 1. Connection details
    con <- DBI::dbConnect(RPostgres::Postgres(),
                          host = host,
                          dbname = dbname,
                          port = 5432,
                          user = user,
                          password = password
    )
    ## 2. write the SF object
    if (!listTables) {
      if (is.null(fn)) stop("No file name provided!")
      st_write(object,
               dsn = con, layer = fn,
               layer_options = "OVERWRITE=true"
      )
    }
    ## 3. list all existing tables
    allTables <- data.table::data.table(rpostgis::pgListGeom(conn = con), key = "schema_name")
    allTables <- allTables["public"][]

    ## 4. Disconnect
    RPostgres::dbDisconnect(con)
  } else if (DBtype == "local" & !is.null(localpath)) {
    ##############################################
    ## Local Files

    if (!listTables) {
      if (dir.exists(localpath)) {
        st_write(object,
                 dsn = localpath, layer = paste0(fn, ".shp"),
                 driver = "ESRI Shapefile",
                 layer_options = "OVERWRITE=true"
        )
      } else {

      }
    } else if (listTables) {
      if (dir.exists(localpath)) {
        allTables <- list.files(localpath, pattern = ".shp$")
        if (length(allTables) > 0) {
          allPath <- file.path(localpath, allTables)
          allInfo <- file.info(allPath)
          allTables <- data.table(cbind(allTables, allInfo[, c(1, 4)]))
          names(allTables) <- c("table_name", "size", "creation_date")
        } else {
          allTables <- data.table(table_name = character(0), size = numeric(0), creation_date = character(0))
        }
      }
    }
  }
  ## 5. return
  return(allTables)
}

## read shp
readSHPfromDB <- function(dbname = NULL, host = "localhost",
                          user = NULL, password = NULL,
                          fn = NULL, listTables = F, inShinyApp = T, DBtype = "pg", localpath = NULL) {
  allTables <- NULL
  if (DBtype == "pg") {
    ## 1. Connection details
    con <- DBI::dbConnect(RPostgres::Postgres(),
                          host = host,
                          dbname = dbname,
                          port = 5432,
                          user = user,
                          password = password
    )
    ## 2. write the SF object
    if (!listTables) {
      if (is.null(fn)) stop("No file name provided!")
      if (inShinyApp) shiny::incProgress(0.4)
      allTables <- st_read(
        dsn = con, layer = fn,
        quiet = T
      )
      ## 4. Disconnect
      RPostgres::dbDisconnect(con)
    } else if (listTables) {
      if (inShinyApp) shiny::incProgress(0.2)

      allTables <- data.table::data.table(rpostgis::pgListGeom(conn = con), key = "schema_name")
      allTables <- allTables["public"][]
      ## 4. Disconnect
      RPostgres::dbDisconnect(con)
    }
  } else if (DBtype == "local" & !is.null(localpath)) {
    ##############################################
    ## Local Files

    if (!listTables) {
      if (dir.exists(localpath)) {
        allTables <- st_read(
          dsn = file.path(localpath, fn),
          quiet = T
        )
      } else {

      }
    } else if (listTables) {
      if (dir.exists(localpath)) {
        allTables <- list.files(localpath, pattern = ".shp$")
        if (length(allTables) > 0) {
          allPath <- file.path(localpath, allTables)
          allInfo <- file.info(allPath)
          allTables <- data.table(cbind(allTables, allInfo[, c(1, 4)]))
          names(allTables) <- c("table_name", "size", "creation_date")
        } else {
          allTables <- data.table(table_name = character(0), size = numeric(0), creation_date = character(0))
        }
      }
    }
  }
  ## 5. Return
  return(allTables)
}

################################################################################################
writeRAStoDB <- function(object = NULL, dbname = NULL, host = "localhost",
                         user = NULL, password = NULL,
                         fn = NULL, listTables = F, inShinyApp = T, DBtype = "pg", localpath = NULL) {
  allTables <- NULL
  if (DBtype == "pg") {
    ## 1. Connection details
    con <- DBI::dbConnect(RPostgres::Postgres(),
                          host = host,
                          dbname = dbname,
                          port = 5432,
                          user = user,
                          password = password
    )
    ## 2. write the SF object
    if (!listTables) {
      if (is.null(fn)) stop("No file name provided!")
      rpostgis::pgWriteRast(
        conn = con,
        name = fn,
        raster = object,
        overwrite = T,
        blocks = c(10)
      )
      ## 4. Disconnect
      RPostgres::dbDisconnect(con)
    } else if (listTables) {
      allTables <- data.table::data.table(rpostgis::pgListRast(conn = con), key = "schema_name")
      allTables <- allTables["public"][]
      ## 4. Disconnect
      RPostgres::dbDisconnect(con)
    }
  } else if (DBtype == "local" & !is.null(localpath)) {
    ##############################################
    ## Local Files

    if (!listTables) {
      if (dir.exists(localpath)) {
        raster::writeRaster(object,
                            filename = file.path(localpath, paste0(fn, ".tif")),
                            format = "GTiff",
                            overwrite = T
        )
      } else {
        stop("Local directory not available")
      }
    } else if (listTables) {
      if (dir.exists(localpath)) {
        allTables <- list.files(localpath, pattern = ".tif$")
        if (length(allTables) > 0) {
          allPath <- file.path(localpath, allTables)
          allInfo <- file.info(allPath)
          allTables <- data.table(cbind(allTables, allInfo[, c(1, 4)]))
          names(allTables) <- c("table_name", "size", "creation_date")
        } else {
          allTables <- data.table(table_name = character(0), size = numeric(0), creation_date = character(0))
        }
      }
    }
  }
  ## 5. return
  return(allTables)
}

readRASfromDB <- function(dbname = NULL, host = "localhost",
                          user = NULL, password = NULL,
                          fn = NULL, listTables = F, inShinyApp = T, DBtype = "pg", localpath = NULL) {
  allTables <- NULL
  if (DBtype == "pg") {
    con <- DBI::dbConnect(RPostgres::Postgres(),
                          host = host,
                          dbname = dbname,
                          port = 5432,
                          user = user,
                          password = password
    )
    ## 2. write the SF object
    if (!listTables) {
      if (is.null(fn)) stop("No file name provided!")
      allTables <- rpostgis::pgGetRast(
        conn = con,
        name = fn,
        bands = TRUE
      )
      ## 4. Disconnect
      RPostgres::dbDisconnect(con)
    } else if (listTables) {
      allTables <- data.table::data.table(rpostgis::pgListRast(conn = con), key = "schema_name")
      allTables <- allTables["public"][]
      ## 4. Disconnect
      RPostgres::dbDisconnect(con)
    }
  } else if (DBtype == "local" & !is.null(localpath)) {
    ##############################################
    ## Local Files

    if (!listTables) {
      if (dir.exists(localpath)) {
        fn <- tools::file_path_sans_ext(fn)
        allTables <- raster::raster(file.path(localpath, paste0(fn, ".tif")))
      } else {
        stop("Local directory not available")
      }
    } else if (listTables) {
      if (dir.exists(localpath)) {
        allTables <- list.files(localpath, pattern = ".tif$")
        if (length(allTables) > 0) {
          allPath <- file.path(localpath, allTables)
          allInfo <- file.info(allPath)
          allTables <- data.table(cbind(allTables, allInfo[, c(1, 4)]))
          names(allTables) <- c("table_name", "size", "creation_date")
        } else {
          allTables <- data.table(table_name = character(0), size = numeric(0), creation_date = character(0))
        }
      }
    }
  }

  return(allTables)
}
