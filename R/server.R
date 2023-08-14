#' Server function for Spatial Sampling App
#'
#' @noRd
#' @keywords internal
#'




## Server Function
main_server<-function(input, output, session) {


  #########################################
  ##  Table formats & styles
  #########################################
  ## 1. General
  smTab<-list(dom="t")

  ##  2. Info table (no selection, first column is Names)
  infoTable<-.%>% formatStyle(1,  color = '#FFFFFF',
                              backgroundColor = '#0d47a1',
                              fontWeight = 'bold')

  inputTable<-.%>% formatStyle(2,
                               fontWeight = 'bold',
                               textAlign = 'center')


  ##  3. View table (no selcetion, all columns the same)
  viewTable<-.%>% formatStyle(1,  color = '#FFFFFF',
                              backgroundColor = '#33D2FF',
                              fontWeight = 'bold')

  #########################################
  ## CSS/UI Styles
  styleDwlButton<-c("color: #FFFFFF;  width: 180px;background-color: #1976D2;
                  border-color: #1976D2;
                  margin:0 20% 0 20%;")
  smTabDir<-list(dom="t", pagelength=500, scrollY="250px", scrollcollapse=TRUE, paging=FALSE)

  action_btn_close <-c("color: #FFFFFF; background-color: #0d47a1; border-color: #0d47a1")
  styleActButton<-c("color: #FFFFFF; background-color: #7f0000;
                  border-color: #7f0000; margin:0 20% 0 20%;")


  #################################################################################
  ##              Start-up Set-up
  ## BOOK MARKS HAD BEEN DROPPED

  # Disable Buttons for Startup
  observe({
    shinyjs::disable(id = "modify")
    shinyjs::disable(id = "genSamp")
    ## Show local files is only shown when storage type is provided.
    shinyjs::disable(id = "showDBshape")
  })
  observe({
    shinyjs::hideElement(id = "popData")
  })

  ## START UP MODAL FOR MAPKEY AND USER
  startupkeyusr<-startupModalSRV("startupModal", useronly = ifelse(getOption("mapwidget.option")=="mapdeck", FALSE, TRUE))

  ## Create APP dir for storage of resources
  fp<-reactiveVal(NULL)
  fpp<-reactiveVal(NULL); fppTPK<-reactiveVal(NULL)
  fppTPKerror<-reactiveVal(NULL); tpkLoadError<-reactiveVal(NULL)
  tpkDWLError<-reactiveVal(NULL); tpkCPError<-reactiveVal(NULL)
  observeEvent(startupkeyusr$user(), {
    usr<-req(startupkeyusr$user())
    appdir<-file.path(tools::R_user_dir("susospatsample", which = "data"), "susospatsample")
    if(!dir.exists(appdir)){
      # !only if not exists
      # 1. Data dir
      dir.create(appdir, recursive = TRUE, showWarnings = FALSE)
    }
    # appdir
    appdir<-file.path(appdir, paste0(usr))
    fp(appdir)
    # tif maps
    tifdir<-file.path(appdir, "basemaps_tif")
    if(!dir.exists(tifdir)){
      # !only if not exists
      # 1. Data dir
      dir.create(tifdir, recursive = TRUE, showWarnings = FALSE)
    }
    fpp(appdir)
    # tpk maps
    tpkdir<-file.path(appdir, "basemaps_tpk")
    if(!dir.exists(tpkdir)){
      # !only if not exists
      # 1. Data dir
      dir.create(tpkdir, recursive = TRUE, showWarnings = FALSE)
    }
    fppTPK(appdir)
    # tpk error maps
    tpkErrodir<-file.path(appdir, "mapError")
    if(!dir.exists(tpkErrodir)){
      # !only if not exists
      # 1. Data dir
      dir.create(tpkErrodir, recursive = TRUE, showWarnings = FALSE)
    }
    fppTPKerror(appdir)

    notmessage<-HTML(
      sprintf(
        "Your files for this session will be stored in you personal user directory under, <b>%s/%s</b>.",
        appdir, usr
      ) %>%
        stringr::str_remove_all("\\n") %>%
        stringr::str_squish()
    )

    showNotification(
      ui = notmessage,
      duration = NULL,
      id = "userinfostart",
      type = "message",
      session = session,
      closeButton = T
    )
  })


  #################################################################################
  ##              Data Base Set-up
  storemode<-reactiveVal(NULL); shppath<-reactiveVal(NULL); raspath<-reactiveVal(NULL)
  DBname<-reactiveVal(NULL); DBhost<-reactiveVal(NULL); DBuser<-reactiveVal(NULL); DBpass<-reactiveVal(NULL)

  # Storage Setting Module
  storageSetting <- datastoreSRV("setstorage", home_dir=file.path(fp(), "upload"))

  # Storage setting get values
  observe({
    # local store
    #print(storageSetting$storemode())
    storemode(storageSetting$storemode())
    shppath(storageSetting$shppath())
    raspath(storageSetting$raspath())
    # remote store
    DBname(storageSetting$DBname())
    DBhost(storageSetting$DBhost())
    DBuser(storageSetting$DBuser())
    DBpass(storageSetting$DBpass())
    DBname(storageSetting$DBname())
  })
  # Enable Buttons
  observe({
    if(!is.null(storemode()) && storemode()=="local"){
      shinyjs::enable(id = "showDBshape")

    } else if(!is.null(storemode()) && storemode()=="pg"){
      shinyjs::enable(id = "showDBshape")

    }

  })

  # update GADM selection
  # observe({
  #   #fpdata<-system.file("ui_inputs", package = "susospatsample")
  #   fpdata<-file.path("C:/Users/micha/Documents/susospatsample","inst", "ui_inputs")
  #   ISO3<-data.table::as.data.table(readr::read_csv(file.path(fpdata, "iso3_country_minimal.csv")))
  #   gadmISO3<-rownames(ISO3)
  #   names(gadmISO3)<-ISO3$name
  #
  #   # update select
  #   shiny::updateSelectInput(session = session,
  #                            inputId = "gadmISO3_sel",
  #                            choices = gadmISO3)
  # })
  # #################################################################################
  ##              PopUp for Detail and Load Data - SHAPE
  # 1. SHOW Existing Files MODAL
  ## 1.1. Modal To show database
  observeEvent(input$showDBshape, {
    ## i. Reset inputs
    new_shp$harare_landuse3_type1<-NULL
    callModule(stratumVariableUpdateSvr,
               "strVarSel",
               dataset = NULL)
    ## ii. Show Modal
    showModal(modalDialog(title =tags$div(
      HTML("<center><font color='#0d47a1'><big>Boundary Files</big></font></center>")),
      fluidRow(
        DT::dataTableOutput("shpDirTable", height = 280),
        br(),
        actionButton("load_shape","Load Selected",
                     style=styleDwlButton)
      ),
      footer = tagList(actionButton("close1","Close Viewer",
                                    icon("window-close"),
                                    style=action_btn_close)),
      easyClose = T, size = "l"
    ))
  })
  # 1.2 LIST Shape files in Data base
  flSHP<-reactive({
    req(storemode())
    if(storemode()=="pg") {
      tab<-writeSFtoDB(listTables = T)
    } else if(storemode()=="local") {
      req(shppath())
      tab<-writeSFtoDB(listTables = T,
                       DBtype = storemode(),
                       localpath = shppath())
    }
    return(tab)
  })
  # 1.3. SHOW SELECTION table for shapes
  output$shpDirTable<-renderDataTable({
    shiny::validate(need(!is.null(flSHP()), message = "No Maps Available!"))
    tab<-isolate(flSHP())
    tab<-DT::datatable(tab, smTabDir ,selection = "single",  rownames = F,
                       style = "bootstrap")
    return(tab)
  }, server = TRUE)

  ## 1.4. DT UPDATE TABLE WITH PROXY
  shpDirTableProxy<-DT::dataTableProxy("shpDirTable", session = session)
  observeEvent(input$load_shape, {
    shiny::validate(need(!is.null(flSHP()), message = "No Maps Available!"))
    tab<-flSHP()
    replaceData(proxy = shpDirTableProxy,
                data = tab,
                resetPaging = TRUE,
                clearSelection = "all",
                rownames = F)
  }, ignoreInit = T)

  ## 1.5 CLICK EVENT shape selection
  shp_id<-eventReactive(input$load_shape, {
    shiny::validate(need(!is.null(flSHP()), message = "No Raster Available!"))
    tab<-isolate(flSHP())
    shiny::validate(need(input$shpDirTable_rows_selected, message = F))
    ## Disable upload & close modal
    shinyjs::disable("new_shape")
    shiny::removeModal()
    ## get name
    tableName<-tab[input$shpDirTable_rows_selected, "table_name"]
    return(tableName)
  })
  ###############################################
  # 2. Details of INPUT SHAPE
  observeEvent(input$showDetails, {
    showModal(modalDialog(title =tags$div(
      HTML("<center><font color='#0d47a1'><big>Details Boundary File</big></font></center>")),
      fluidRow(
        (DT::dataTableOutput("shpInfoTab"))
      ),
      footer = tagList(actionButton("close1","Close Viewer",
                                    icon("window-close"),
                                    style= action_btn_close)),
      easyClose = T, size = "l"
    ))
  })

  #################################################################################
  ##              PopUp for Detail and Load Data - RASTER
  ## 3. SHOW EXISTING RASTER
  ## 3.1. MODAL
  observeEvent(input$showDBraster, {
    showModal(modalDialog(title =tags$div(
      HTML("<center><font color='#0d47a1'><big>Raster Files</big></font></center>")),
      fluidRow(
        DT::dataTableOutput("rasDirTable", height = 280),
        br(),
        actionButton("load_raster","Load Selected",
                     style=styleDwlButton)
      ),
      footer = tagList(actionButton("close1","Close Viewer",
                                    icon("window-close"),
                                    style=action_btn_close)),
      easyClose = T, size = "l"
    ))
  })
  ## 3.2. list raster
  flRAS<-reactive({
    tab<-writeRAStoDB(listTables = T)
    return(tab)
  })
  ## 3.3. show raster
  output$rasDirTable<-renderDataTable({
    shiny::validate(need(!is.null(flRAS()), message = "No Raster Available!"))
    tab<-flRAS()
    DT::datatable(tab, smTabDir ,
                  selection = "single",
                  rownames = F,
                  style = "bootstrap")
  })
  ## 3.4. CLICK EVENT raster selection
  map_id<-eventReactive(input$load_raster, {
    shiny::validate(need(!is.null(flRAS()), message = "No Raster Available!"))
    tab<-flRAS()
    shiny::validate(need(input$rasDirTable_rows_selected, message = F))
    ## Disable upload & close modal
    shinyjs::disable("pop_file_raster")
    shiny::removeModal()
    ## get name
    tableName<-tab[input$rasDirTable_rows_selected, "table_name"]
    return(tableName)
  })
  #################################################################################
  ##            1.SHAPE FILE OPERATIONS                                           #
  ##                Shape file sources: User Upload, GADM                         #
  ##                Format: SF, data.table (for info)                             #
  ##                Display: Map (googlemaps)                                     #
  #################################################################################
  the_grd<-reactiveValues()
  new_shp<-reactiveValues()
  #################################################################################
  ##  1.1 USER UPLOAD                                                             #
  ##      - file is uploade and modified for mapping                              #
  ##      - store original file in DB
  #################################################################################
  ## 1. UPLOAD shape
  observeEvent(input$new_shape, {
    ## reset!
    callModule(stratumVariableUpdateSvr,
               "strVarSel",
               dataset = NULL)
    ## i. Validate input (zip, file type)
    shiny::validate(need(input$new_shape, message = "Select file first!"))
    shpFiles<-tempdir()
    unlink(paste0(shpFiles, "/*"))
    unzip(input$new_shape$datapath, exdir = shpFiles)
    fileList<-list.files(shpFiles)
    shpFile<-fileList[grep(".shp$", fileList)]
    nshp<-length(shpFile)
    if (nshp==0) {
      showModal(modalDialog(
        title = "Wrong Format!",
        "Your file is not provided in the correct format (ESRI Shapefile, Single Folder). Please check and upload again",
        easyClose = TRUE,
        footer = NULL
      ))

    } else if (nshp>1) {
      showModal(modalDialog(
        title = "More than one file!",
        "You uploaded more than one boundary files. Your files will be aggregated to a single file",
        easyClose = TRUE,
        footer = NULL
      ))

    }
    shiny::validate(need(shpFile, message = F))
    ## ii. Read the shape
    if (nshp==1) {
      tmp.shp<-shapeLoad2(path = shpFiles, lay = shpFile, sp.Library="sf")
    } else if (nshp>1) {
      tmp.shplist<-vector("list", length = nshp)
      for(i in 1:nshp) {
        tmp.shplist[[i]]<-sf::st_read(file.path(shpFiles, shpFile[i]), quiet = TRUE)
      }
      #CHECKtmp.shplist<<-tmp.shplist
      tmp.shp<-do.call(rbind, tmp.shplist)

      tmp.shp<-list(st_crs(tmp.shp)[[2]], tmp.shp)
    }
    new_shp$shp.source<-"Upload"

    ## iii. Check for storemode
    if(is.null(storemode())) {
      showNotification("Please select storage mode first!",
                       type = "warning", id = "no_storage", duration = NULL)
      req(FALSE)
    }
    ######################################################################
    ## clean shape file and send to DB
    showNotification("Testing for validity of polygons and applying corrections.",
                     type = "message", id = "simplifyMap3", duration = NULL)
    if(storemode()=="pg"){
      tmp.shp<-shapeLoad2_cleanToDB(SHP = tmp.shp[[2]],
                                    shpName = shpFile,
                                    writeToDB = T,
                                    DBtype = "pg", localpath = NULL)
    } else if(storemode()=="local"){
      req(shppath())
      tmp.shp<-shapeLoad2_cleanToDB(SHP = tmp.shp[[2]],
                                    shpName = shpFile[1],
                                    writeToDB = T,
                                    DBtype = "local", localpath = shppath())
    } else {
      tmp.shp<-shapeLoad2_cleanToDB(SHP = tmp.shp[[2]],
                                    shpName = shpFile,
                                    writeToDB = T)
    }
    removeNotification(id = "simplifyMap3")
    ################################################################################
    ## RETURN
    callModule(stratumVariableUpdateSvr,
               "strVarSel",
               dataset = tmp.shp)
    if (input$sampType=="Random Grid") {
      new_shp$harare_landuse3_type1<-tmp.shp
    } else if (input$sampType=="Random Cluster") {
      new_shp$harare_landuse3_type1<-tmp.shp
    }
  })
  #################################################################################
  ## 2. LOAD from DB shape
  #################################################################################
  observeEvent(input$load_shape, {
    ## reset!
    callModule(stratumVariableUpdateSvr,
               "strVarSel",
               dataset = NULL)
    shiny::validate(need(shp_id(), message = F))
    tableName<-shp_id()
    withProgress(message = 'Reading File', value = 0.2,
                 {
                   req(storemode())
                   if(storemode()=="pg"){
                     tmp.shp<-readSHPfromDB(fn = tableName$table_name, inShinyApp = T)
                   } else if(storemode()=="local") {
                     req(shppath())
                     tmp.shp<-readSHPfromDB(
                       fn = tableName$table_name,
                       inShinyApp = T,
                       DBtype = storemode(),
                       localpath = shppath()
                     )
                   }
                 }
    )
    new_shp$shp.source<-"DB"
    ######################################################################
    ## clean shape file and send to DB
    showNotification("Testing for validity of polygons and applying corrections.",
                     type = "message", id = "simplifyMap3", duration = NULL)

    tmp.shp<-shapeLoad2_cleanToDB(SHP = tmp.shp, writeToDB = F)

    if(!st_is_longlat(tmp.shp)) tmp.shp<-st_transform(tmp.shp, 4326)
    removeNotification(id = "simplifyMap3")
    ################################################################################
    ## RETURN
    callModule(stratumVariableUpdateSvr,
               "strVarSel",
               dataset = tmp.shp)
    if (input$sampType=="Random Grid") {
      new_shp$harare_landuse3_type1<-tmp.shp
    } else if (input$sampType=="Random Cluster") {
      ## Add stratum_numeric from strat_var for random cluster
      tmp.shp$stratum_numeric<-tmp.shp[[input$`strVarSel-strat_var`]]
      new_shp$harare_landuse3_type1<-tmp.shp
    }
  })


  #################################################################################
  ##  1.2 GADM DOWNLOAD  (onlye Level 1)                                          #
  #################################################################################
  observeEvent(input$gadmISO3_sel, {
    ## reset!
    withProgress(message = 'Loading map, this may take a while!', value = 0,{
      callModule(stratumVariableUpdateSvr,
                 "strVarSel",
                 dataset = NULL)
      ##  Load Country Codes for GADM
      ISO3<-as.data.table(read_csv("data/ui_inputs/iso3_country_minimal.csv"))
      iso3_sel<-ISO3[as.numeric(input$gadmISO3_sel), iso3]

      ## i. LOAD MAP
      ## Level 1 (standard)
      tmp.shp<-try(GADM.getData('GADM',country = iso3_sel, level = 1,  path = "data/GADMmaps", sp.Library = "sf"), silent = T)
      incProgress(0.2)
      ##  Check Level 0 if one fails
      if (class(tmp.shp)[1]=="try-error") try(GADM.getData('GADM', country = iso3_sel, level = 0,  path = "data/GADMmaps", sp.Library = "sf"), silent = T)
      ##  Stop if not found
      shiny::validate(need(class(tmp.shp)[1]!="try-error", message = "No GADM files available for this country!"))
      new_shp$shp.source<-"GADM"
      ##  SIMPLIFY SHAPE
      crsOld<-st_crs(tmp.shp)
      ##  ii. Remove empty geometries first
      if(sum(st_is_empty(tmp.shp))>0) {
        tmp.shp<-tmp.shp[!st_is_empty(tmp.shp),]
      }
      incProgress(0.2)
      ## iv. EXPORT
      ## update stratum selection
      callModule(stratumVariableUpdateSvr,
                 "strVarSel",
                 dataset = tmp.shp)
      ## data
      new_shp$harare_landuse3_type1<-tmp.shp
    })
  }, ignoreInit = T)


  #################################################################################
  ##  1.3 Summary Table Shape                                                     #
  #################################################################################
  shp.info<-reactive({
    ##  ii. shape info
    tmp.shp<-new_shp$harare_landuse3_type1
    shiny::validate(need(tmp.shp, message = F))
    shp.info<-list()
    shp.info$p4s<-st_crs(tmp.shp)[[2]]
    shp.info$nrows<-nrow(tmp.shp)
    shp.info$nitems<-length(tmp.shp)
    ex<-round(c(extent(tmp.shp)[1], extent(tmp.shp)[3], extent(tmp.shp)[2], extent(tmp.shp)[4]), 6)
    shp.info$extent<-ex
    return(shp.info)
  })
  shp.info.tabl<-reactive({
    shp.info<-shp.info()
    shp.source<-new_shp$shp.source
    shiny::validate(need(shp.info, message = "Please load shape file first!"),
                    need(shp.source, message = F))
    ex<-sprintf("%s Lon, %s Lat, %s Lon, %s Lat",shp.info$extent[1], shp.info$extent[2], shp.info$extent[3], shp.info$extent[4])
    tabl<-data.table(character(5), character(5))
    tabl[,V1:=c("Source", "Projection", "Features", "Fields", "Extent")]
    source<-shp.source
    tabl[,V2:=c(source,
                shp.info$p4s,
                as.character(shp.info$nrows),
                as.character(shp.info$nitems),
                ex)]
    ## for report
    return(as.data.frame(tabl))

  })
  output$shpInfoTab <- DT::renderDataTable({
    shp.info<-shp.info()
    shp.source<-new_shp$shp.source
    shiny::validate(need(shp.info, message = "Please load shape file first!"),
                    need(shp.source, message = F))
    ex<-sprintf("%s Lon, %s Lat, %s Lon, %s Lat",shp.info$extent[1], shp.info$extent[2], shp.info$extent[3], shp.info$extent[4])
    tabl<-data.table(character(5), character(5))
    tabl[,V1:=c("Source", "Projection", "Features", "Fields", "Extent")]
    source<-shp.source
    tabl[,V2:=c(source,
                shp.info$p4s,
                as.character(shp.info$nrows),
                as.character(shp.info$nitems),
                ex)]
    ## for report
    DT::datatable(tabl, smTab, selection = "single", rownames = F,
                  colnames = c("",""),
                  style = "bootstrap") %>% infoTable
  })

  #################################################################################
  ##  1.4. SHAPE MAP (MAPVIEW)                                                    #
  #################################################################################
  ##  i. Create Reactive Expression for data intake
  ##      - this includes also gridded file (all done here)
  ## A. Original DATA transform for maps
  ##    - ONLY for maps, do not use elsewher!
  harare_landuse3_type1<-reactive({
    tmp.shp.sf<-new_shp$harare_landuse3_type1
    shiny::validate(need(tmp.shp.sf, message = F))
    withProgress(message = 'Preparing Map', value = 0,{
      if (input$strat=="No"){
        incProgress(0.3)

        ## Aggregate if too large (currently if large than 1 MILLION points)
        incProgress(0.4)
        ## Simplify for very complex maps
        if(npts(tmp.shp.sf)/nrow(tmp.shp.sf)>100) {
          crsOld<-st_crs(tmp.shp.sf)

          #tmp.shp.sf<-ms_simpl_complexShape(tmp.shp.sf, keep = 0.01)
          system.time(tmp.shp.sf<-tmp.shp.sf %>%
                        st_transform(3857) %>%
                        st_simplify(dTolerance = 100) %>%
                        st_make_valid()
          )
          if(sum(st_is_empty(tmp.shp.sf))>0) {
            tmp.shp.sf<-tmp.shp.sf[!st_is_empty(tmp.shp.sf),]
            tmp.shp.sf<-tmp.shp.sf %>%
              #st_cast("MULTIPOLYGON", warn = F, do_split = F) %>%
              st_buffer(0.0) %>%
              st_transform(crsOld)
          } else {
            tmp.shp.sf<-tmp.shp.sf %>%
              st_buffer(0.0) %>%
              st_transform(crsOld)
          }
        }

      } else if (input$strat=="Yes"){
        incProgress(0.3)
        stratVar<-input$`strVarSel-strat_var`
        shiny::validate(need(stratVar!="", message = F))
        shiny::validate(need(length(grep(input$`strVarSel-strat_var`, names(tmp.shp.sf)))>0, message = F))
        ##########################################################
        ## Simplify for Plot only!
        ##########################################################

        incProgress(0.4)
        #checkboundaries<<-tmp.shp.sf
        ## Simplify for very complex maps
        if(npts(tmp.shp.sf)/nrow(tmp.shp.sf)>100) {
          crsOld<-st_crs(tmp.shp.sf)

          #tmp.shp.sf<-ms_simpl_complexShape(tmp.shp.sf, keep = 0.01)
          system.time(tmp.shp.sf<-tmp.shp.sf %>%
                        st_transform(3857) %>%
                        st_simplify(dTolerance = 100) %>%
                        st_make_valid()
          )
          if(sum(st_is_empty(tmp.shp.sf))>0) {
            tmp.shp.sf<-tmp.shp.sf[!st_is_empty(tmp.shp.sf),]
            tmp.shp.sf<-tmp.shp.sf %>%
              #st_cast("MULTIPOLYGON", warn = F, do_split = F) %>%
              st_buffer(0.0) %>%
              st_transform(crsOld)
          } else {
            tmp.shp.sf<-tmp.shp.sf %>%
              st_buffer(0.0) %>%
              st_transform(crsOld)
          }
        }
      }

      return(tmp.shp.sf)
    })
  })
  ##################################################################################################
  ## B. MAP
  ##################################################################################################
  ## CREATE MAP UI with renderUI
  output$MAP_UI<-shiny::renderUI({
    if(getOption("mapwidget.option")=="mapdeck"){
      tagList(
        mapModuleUI("baseMap", height = "90vh")
      )
    } else if(getOption("mapwidget.option")=="leaflet"){
      mapUI("baseMap_leaf")
    }
  })
  ## 1. Base Map
  ##################################
  ## 1. Build base map
  observeEvent(startupkeyusr$key(), {
    kk<-req(startupkeyusr$key())
    mapdeck::set_token(kk)
  })
  observe({
    if(getOption("mapwidget.option")=="mapdeck"){
      mapModuleSvr(id = "baseMap", #key = mapkey,
                   updateMap = reactive({NULL}),
                   updateGroup = reactive({NULL}),
                   z_var = reactive({NULL}))
    } else if(getOption("mapwidget.option")=="leaflet"){
      mapServer("baseMap_leaf", updateMap = reactive(NULL), updateGroup = reactive(NULL))
    }
  })

  ##################################
  ## 2. Enable modify button
  observe({
    # enable modify button --> only if no population!! AND GRID
    if(input$popCreate=="No" & input$sampType == "Random Grid"){
      shinyjs::enable("modify")
    }
  })

  ##################################
  ## 3. Update with first input layer
  observe({
    if(getOption("mapwidget.option")=="mapdeck"){
      mapModuleSvr(id = "baseMap",
                   updateMap = harare_landuse3_type1,
                   updateGroup = reactive({input$`strVarSel-strat_var`}),
                   polyId = reactive(NULL), z_var = reactive(NULL))
    } else if(getOption("mapwidget.option")=="leaflet"){
      mapServer("baseMap_leaf",
                updateMap = harare_landuse3_type1,
                updateGroup = reactive({input$`strVarSel-strat_var`}))
    }
  })

  ## 3.1. Add the POINTS
  observe({
    pts<-popFilePoint()

    shiny::validate(need(pts, message = F))
    # If number of Points exceeds limit --> AGGREGATION to cluster
    if(nrow(pts)>getOption("pointsLimit")){
      shiny::showNotification(paste0("The number of points exceeds the limit for map display, therefore
                                     the maps will be aggregate to the provided regions. Aggregating Map for ",
                                     nrow(pts),
                                     " units. This may take a while"), closeButton = F,
                              id = "rendBigP", type = "warning", duration = NULL)

      bounds<-harare_landuse3_type1_sub()

      shiny::validate(need(pts, message = F),
                      need(bounds, message = F))


      shp<-aggregatPointsToBoundsgeos(bounds = bounds, points = pts)
      if(!st_is_longlat(shp)) shp<-shp %>% st_transform(4326)



      # render map
      shiny::validate(need(shp, message = F))
      removeNotification("rendBigP")
      if(getOption("mapwidget.option")=="mapdeck"){
        mapModuleSvr(id = "baseMap", tooltip = "Pop",
                     updateMap = reactive({shp}), updateGroup = reactive({"Pop"}),
                     polyId = reactive(NULL), z_var = reactive("Pop")
        )
      } else if(getOption("mapwidget.option")=="leaflet"){
        mapServer("baseMap_leaf",
                  updateMap = reactive({shp}),
                  updateGroup = reactive({"Pop"}))
      }
    } else {
      shp<-pts
      if(getOption("mapwidget.option")=="mapdeck"){
        mapModuleSvr(
          id = "baseMap",
          updateMapPts=reactive({shp}),
          fillcolor = "red",
          layeridpts = "pointspop"
        )
      } else if(getOption("mapwidget.option")=="leaflet"){
        print("NOT YET")
        # mapServer("baseMap_leaf",
        #           updateMap = reactive({shp}),
        #           updateGroup = reactive({"Pop"}))
      }


    }


  })
  ##################################
  ## 4. Update Map after transformation
  observe({
    ##################################
    ## INPUTS
    if(input$sampType=="Random Grid" & input$popCreate == "No") {
      Boundaries2<-new_shp$sp_grd_strat
    } else {
      Boundaries2<-new_shp$sp_grd_strat_poly
    }
    shiny::validate(need(Boundaries2, message = F))
    ##################################
    ## Build basemap update when transformed
    if(getOption("mapwidget.option")=="mapdeck"){
      mapModuleSvr(id = "baseMap",
                   updateMap = reactive({Boundaries2}),
                   updateGroup = reactive({(ifelse(input$strat=="Yes", "stratum_numeric", ""))}),
                   polyId = reactive(NULL), z_var = reactive(NULL))
    } else if(getOption("mapwidget.option")=="leaflet"){
      print("NOT YET")
      mapServer("baseMap_leaf",
                updateMap = reactive({Boundaries2}),
                updateGroup = reactive({(ifelse(input$strat=="Yes", "stratum_numeric", ""))}))
    }

  })
  ##################################
  ## 5. Update Map after sample
  observe({
    Boundaries2<-samp_raster_shp()
    shiny::validate(need(Boundaries2, message = F),
                    need(!is.null(input$conf3), message = F))
    #   if(isolate(input$strat) == "Yes") LABEL<-Boundaries2[,"stratum_numeric", drop = T]
    ## Recast if Geometry
    if (st_geometry_type(Boundaries2, by_geometry = F)=="GEOMETRY") {
      Boundaries2<-Boundaries2 %>% st_cast("MULTIPOLYGON")
    }
    ## Check for PPS var
    if(input$sampPPS=="Yes") {
      POP<-"Pop"
    } else {
      POP<-NULL
    }
    if(getOption("mapwidget.option")=="mapdeck"){
      mapModuleSvr(id = "baseMap",
                   updateMap = reactive({Boundaries2}),
                   updateGroup = reactive({(ifelse(input$strat=="Yes", "stratum_numeric", ""))}),
                   polyId = reactive(NULL), z_var = reactive({POP}))
    } else if(getOption("mapwidget.option")=="leaflet"){
      print("NOT YET")
      # mapServer("baseMap_leaf",
      #           updateMap = reactive({shp}),
      #           updateGroup = reactive({"Pop"}))
    }
  })
  ################################################################################
  ##      enabel/disable elements
  observe({
    Boundaries<-harare_landuse3_type1()
    shinyjs::showElement(id="popData")
  })

  ##################################################################################
  ##  2. POINTS OPERATIONS                                                         #
  ##      - loading and preprocessing done here.                                   #
  ##      - overlap checked                                                        #
  ##################################################################################
  ## 1. Create the POINT layer
  ## 1.1. Load data from module
  zipInputPopPoints<-zipFileInput_server("popPointUpload", zipInput = T)
  popFilePoint<-reactive({
    if(is.null(gridPointsSpatial())){
      # FROM UPLOAD
      tmpFile<-zipInputPopPoints()
      shiny::validate(need(tmpFile, message = "Select file first!"))
      # add count variable for total
      tmpFile$count<-1
      # identify long/lat columns
      lat<-names(tmpFile)[grepl(x = names(tmpFile), pattern = "latitude", ignore.case = T)]
      long<-names(tmpFile)[grepl(x = names(tmpFile), pattern = "longitude", ignore.case = T)]
      # stop if not found
      if (length(lat)==0 | length(long)==0) {
        showModal(modalDialog(
          title = "Lat/Long coordinates not found!",
          "Your data does not include the cooridinates. Please upload the correct file!",
          easyClose = TRUE,
          footer = NULL
        ))
        req(FALSE)
      }
      # drop missing
      tmpFile <- tmpFile %>% dplyr::filter(!is.na(.data[[lat]]) & !is.na(.data[[long]]))
      tmpFile<-as.data.frame(tmpFile)
      tmpFile<-st_as_sf(tmpFile, coords = c(long, lat), crs=4326)

    } else {
      # FROM GOOGLE AI Module (may also come from other modules in the #future)
      tmpFile<-gridPointsSpatial()
      shiny::validate(need(tmpFile, message = F))
      # create info table
      tmp.Area<-list()
      tmp.Area$Area<-tryCatch(
        {sum(round(st_area(harare_landuse3_type1())/1000^2, 2), na.rm = T)},
        error = function(e) {"NA"}
      )
      tmp.Area$stratCount<-ifelse(input$strat=="Yes",
                                  nlevels(as.factor(harare_landuse3_type1()[,input$`strVarSel-strat_var`, drop=T])),
                                  0)
      tmp.Area$Nsquare<-nrow(tmpFile)
      tmp.Area$res<-nrow(harare_landuse3_type1())
      ## EXPORT
      ## to info table
      new_shp$tmp.Area<-tmp.Area
    }
    return(tmpFile)
  })
  # #########################################
  # ## 1.1. LOAD from GOOGLE AI
  # ## --> Delivers Same Points Data as upload above
  gridPointsSpatial <- modal_googleai_server("gridPoints", shape_boundaries = harare_landuse3_type1())


  ###################################################
  ## 2. UPDATE SELECT CHOICES for the agreggation variable
  observe({
    tmp<-popFilePoint()
    shiny::validate(need(tmp, message = F))
    #CHECKtmpFile<<-tmp
    if(input$sampType=='Random Grid') {
      v<-names(tmp %>% st_set_geometry(NULL))
      v<-setNames(object = v, v)
      updateSelectizeInput(
        session = session,
        inputId = "agg_var_sel_point",
        label = "Select variable for Aggregation",
        choices = v,
        selected = NULL,
        options = list(
          placeholder = '',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    } else if (input$sampType=='Random Cluster') {
      shinyjs::enable("modify")
      shinyjs::hide("agg_var_type_point")
      shinyjs::hide("agg_var_sel_point")

    }
  })

  ###################################################
  ## 3. RASTER CREATION from points
  ## 3.1. Create reactive for INIT
  rastaCreateReact<-reactive({
    tmp<-list(input$agg_var_sel_point, input$cell_side, input$agg_var_type_point)
    return(tmp)
  })
  ## 3.2. CREATE raster
  observeEvent(rastaCreateReact(), {
    tmp<-popFilePoint()
    shiny::validate(need(tmp, message = F))
    shiny::validate(need(input$agg_var_sel_point, message = F))
    withProgress(message = paste('Raster creation started with variable',
                                 toupper(input$agg_var_sel_point),
                                 'this may take a while!'),
                 value = 0.1,{

                   # 1. Transform to UTM projection
                   #    --> ATTENTION: subsetting is ok, however GAMBIA is still not working, epsg code with no name
                   bb_tmp<-st_bbox(tmp)
                   utmZone<-long2UTM(bb_tmp[3])
                   epsg<-ifelse(
                     bb_tmp[4]<=0,
                     sprintf("327%02d", utmZone) ,
                     sprintf("326%02d", utmZone)
                   )
                   # Get Proj4 string-->check on newer sytems
                   crs_wgs84utm11 <- sp::CRS(SRS_string = paste0("EPSG:", epsg))
                   tmp<-tmp %>% st_transform(crs_wgs84utm11)

                   ## 2. Calculate Statistics
                   ## 2.1. check for cell side to small
                   ##   --> Currently only cell side of 500 is supported
                   if((input$cell_side)<500) {
                     showModal(modalDialog(
                       title = "Resolution too small!",
                       "Please increase the resolution to a number 500 m and above!",
                       easyClose = TRUE,
                       footer = NULL
                     ))
                     req(FALSE)
                   }

                   ############################################
                   ## RASTER AGGREGATION
                   ## --> currently with raster and terra(total only)
                   if(input$agg_var_type_point == "Mean") {
                     # r<-raster(tmp, resolution = input$cell_side)
                     # tmp<-raster::rasterize(tmp, r, field = input$agg_var_sel_point, fun = mean, na.rm = T)
                     ###################################################
                     ## Mean better with terra:rasterize and length, around 70 seconds faster
                     ## i. Change output raster to terra rast
                     # tmp_crs<-st_crs(tmp) #tmp_crs$wkt
                     r<-terra::rast(terra::ext(tmp),
                                    resolution = input$cell_side,
                                    crs = attributes(unclass(crs_wgs84utm11))$projargs)
                     ## ii. Change simple feature to terra vector
                     tmp<-terra::vect(tmp)
                     #incProgress(0.3, 'Almost Done ...')
                     ## iii. Rasterize vector
                     tmp<-terra::rasterize(tmp, r,
                                           field = input$agg_var_sel_point,
                                           fun = mean, na.rm = T)
                     ## iv. Transform back to raster format --> may be changed later
                     tmp<-raster::raster(tmp)
                     ## SET CRS MANUALLY OTHERWISE NA!!!!!
                     raster::crs(tmp)<-attributes(unclass(crs_wgs84utm11))$projargs

                   } else if (input$agg_var_type_point == "Total"){
                     # r<-raster(tmp, resolution = input$cell_side)
                     #tmp<-raster::rasterize(tmp, r, field = input$agg_var_sel_point, fun = sum, na.rm = T)
                     ###################################################
                     ## Count better with terra:rasterize and length, around 70 seconds faster
                     ## i. Change output raster to terra rast
                     # tmp_crs<-st_crs(tmp) #tmp_crs$wkt
                     r<-terra::rast(terra::ext(tmp),
                                    resolution = input$cell_side,
                                    crs = attributes(unclass(crs_wgs84utm11))$projargs)
                     ## ii. Change simple feature to terra vector
                     tmp<-terra::vect(tmp)
                     #incProgress(0.3, 'Almost Done ...')
                     ## iii. Rasterize vector
                     tmp<-terra::rasterize(tmp, r,
                                           field = input$agg_var_sel_point,
                                           fun = sum, na.rm = T)
                     ## iv. Transform back to raster format --> may be changed later
                     tmp<-raster::raster(tmp)
                     ## SET CRS MANUALLY OTHERWISE NA!!!!!
                     raster::crs(tmp)<-attributes(unclass(crs_wgs84utm11))$projargs
                   } else if (input$agg_var_type_point == "SD") {
                     # r<-raster(tmp, resolution = input$cell_side)
                     # tmp<-raster::rasterize(tmp, r, field = input$agg_var_sel_point, fun = sd, na.rm = T)
                     ###################################################
                     ## Count better with terra:rasterize and length, around 70 seconds faster
                     ## i. Change output raster to terra rast
                     # tmp_crs<-st_crs(tmp) #tmp_crs$wkt
                     r<-terra::rast(terra::ext(tmp),
                                    resolution = input$cell_side,
                                    crs = attributes(unclass(crs_wgs84utm11))$projargs)
                     ## ii. Change simple feature to terra vector
                     tmp<-terra::vect(tmp)
                     #incProgress(0.3, 'Almost Done ...')
                     ## iii. Rasterize vector
                     tmp<-terra::rasterize(tmp, r,
                                           field = input$agg_var_sel_point,
                                           fun = sd, na.rm = T)
                     ## iv. Transform back to raster format --> may be changed later
                     tmp<-raster::raster(tmp)
                     ## SET CRS MANUALLY OTHERWISE NA!!!!!
                     raster::crs(tmp)<-attributes(unclass(crs_wgs84utm11))$projargs
                   }
                   ## 3. Replace NA to 0 (attention, size!)
                   tmp<-ras_NA_to_0(rf = tmp)
                   ###################################
                   ## 4. WRITE TO POSTGRESQL
                   ## --> Moved below to raster

                   new_pop$pop_points<-tmp
                 })
  }, ignoreInit = T)

  ##################################################################################
  ##  3. BUILDING OPERATIONS                                                         #
  ##      - loading and preprocessing done here.                                   #
  ##      - overlap checked                                                        #
  ##################################################################################
  observeEvent(input$pop_file_build, {
    shiny::validate(need(input$pop_file_build, message = "Select file first!"))
    shpFiles<-tempdir()

    ## 1. Unpack File
    unlink(paste0(shpFiles, "/*"))
    fileList<-unzip(input$pop_file_point$datapath, exdir = shpFiles)
    csvFile<-fileList[grep(".csv$", fileList)]
    if (length(csvFile)==0) {
      showModal(modalDialog(
        title = "Wrong Format!",
        "Your file is not provided in the correct format (ESRI Shapefile, Single Folder). Please check and upload again",
        easyClose = TRUE,
        footer = NULL
      ))
    }

    ## 2. Read into SHAPE
    tmpFile<-readr::read_csv(file = csvFile)
    tmpFile<-st_as_sf(tmpFile,
                      coords = c("longitude", "latitude"),
                      crs = 4326)

    ## 3. Create Raster
    ###############################################
    ## USE RASTER
    r<-raster(tmpFile, resolution = as.numeric(input$cell_side))
    if(input$agg_var_type == "Mean") {
      tmp<-raster::rasterize(tmp, r, field = agg_var_sel, fun = mean, na.rm = T)
    } else if (input$agg_var_type == "Total")(
      tmp<-raster::rasterize(tmp, r, field = agg_var_sel, fun = sum, na.rm = T)
    ) else if (input$agg_var_type == "SD") {
      tmp<-raster::rasterize(tmp, r, field = agg_var_sel, fun = sd, na.rm = T)
    }

    new_pop$pop_build<-NULL

  })

  ##################################################################################
  ##  2. RASTER OPERATIONS                                                         #
  ##      - loading and preprocessing done here.                                   #
  ##      - overlap checked                                                        #
  ##################################################################################
  ##  2.1.1 UPLOAD raster                                                           #
  ##################################################################################
  new_pop<-reactiveValues()

  ## All raster also POINTS go here!!!!!
  observe({
    #########################################
    ## 1. Load RASTER, must be provided first
    ##    - requires shapefile
    ##    - if RASTER is larger than SHAPE, crop raster
    withProgress(message = 'Loading raster, this may take a while!', value = 0,{
      harare_landuse3_type1<-new_shp$harare_landuse3_type1

      ## 1. Check inputs by selection
      if(input$popUpType==1) {
        pop_raster<-new_pop$pop_points
        shiny::validate(need(pop_raster, message = "Select file first!"),
                        need(harare_landuse3_type1, message = "Select file first!"))
        SYT<-stringr::str_remove_all(Sys.time(), "([:punct:])|([:space:])")
        shpFile<-paste(c("Pointdata", SYT), collapse = "_")
      } else if(input$popUpType==2) {
        shiny::validate(need(input$pop_file_raster, message = "Select file first!"),
                        need(harare_landuse3_type1, message = "Select file first!"))

        incProgress(0.3, 'Validating ...')
        #########################################
        # 2. Unpack/load raster
        shpFiles<-tempdir()
        unlink(paste0(shpFiles, "/*"))
        unzip(input$pop_file_raster$datapath, exdir = shpFiles)
        fileList<-list.files(shpFiles, pattern = ".tif$", full.names = T)
        shpFile<-fileList[1]

        shiny::validate(need(shpFile, message = F))
        #pop_raster<-raster(shpFile)

        pop_raster<-raster(shpFile)

        ### ATTENTION for the momen only latlong raster work
        crs(pop_raster)<-"EPSG:4326"

      } else if(input$popUpType==3) {

      }


      ### check for stratification reset
      if (input$strat=="Yes") shiny::validate(need(input$`strVarSel-strat_var`!="", message = F))


      #########################################
      # 3. AGGREGATING IF SELECTED
      if ((input$aggregateRaster)>1) {
        showNotification(paste("ATTENTION: AGGREGATING RASTER!", "This may take a while"),
                         id ="aggraster", type = "warning", session = session)
        pop_raster<-raster::aggregate(pop_raster, fact=(input$aggregateRaster),
                                      fun=sum,
                                      expand=TRUE,
                                      na.rm=TRUE)
        removeNotification(id ="aggraster")
      }
      incProgress(0.3)

      ###################################
      # 5. Transfrom to ident CRS
      shpCrsOld<-st_crs(harare_landuse3_type1)
      harare_landuse3_type1<-harare_landuse3_type1 %>% st_transform(raster::proj4string(pop_raster))
      # 5.1. CROP raster (and check overlap)
      shiny::validate(need(try(
        pop_raster<-crop(pop_raster, extent(harare_landuse3_type1))
      ), message = "Files Do not overlap! Uploaded raster needs to overlap with boundary file."))
      harare_landuse3_type1<-harare_landuse3_type1 %>% st_transform(shpCrsOld)
      # Replace NA w 0
      pop_raster<-ras_NA_to_0(rf = pop_raster)

      incProgress(0.2, 'Writing Raster to DB ....')
      ###################################
      ## WRITE TO POSTGRESQL
      ## 1. prepare
      tableName<-basename(shpFile)
      tableName<-stringr::str_remove_all(tableName, ".tif")
      tableName<-iconv(tableName, from = 'UTF-8', to = 'ASCII//TRANSLIT')
      tableName<-stringr::str_remove_all(tableName, "[^[:alnum:]]")
      ## 2. Writing
      # writeRAStoDB(object = pop_raster,
      #              fn = tableName)
      ###################################
      # Results for info table
      incProgress(0.1, 'Almost done ....')
      if (isLonLat(pop_raster)){
        res_m<-distHaversine(c(extent(pop_raster)[1],extent(pop_raster)[3]), c((extent(pop_raster)[1]+res(pop_raster)[1]), extent(pop_raster)[3]))
      } else {
        res_m<-res(pop_raster)
      }
      tmp.Area<-list()
      tmp.Area$Area<-tryCatch(
        {sum(round(st_area(harare_landuse3_type1)/1000^2, 2), na.rm = T)},
        error = function(e) {"NA"}
      )
      tmp.Area$stratCount<-ifelse(input$strat=="Yes", nlevels(as.factor(harare_landuse3_type1[,input$`strVarSel-strat_var`, drop=T])), 0)
      tmp.Area$Nsquare<-ncell(pop_raster)
      tmp.Area$res<-round(res_m[1])
      ## EXPORT
      ## to info table
      new_shp$tmp.Area<-tmp.Area
      ## to processing
      new_pop$pop_raster<-pop_raster
      incProgress(0.1)

      ## iii. UPDATE: selectinput
      if (input$catRaster=="Yes"){
        new_pop$rasCat<-as.character(unique(raster::values(pop_raster)))
      }
    })
  })
  ##################################################################################
  ##  2.1.1 LOAD  raster                                                           #
  ##################################################################################
  observeEvent(input$load_raster, {
    ###################################
    ## 1. Read file from DB
    harare_landuse3_type1<-new_shp$harare_landuse3_type1
    shiny::validate(need(harare_landuse3_type1, message = F),
                    need(map_id(), message = F))
    tableName<-map_id()

    withProgress(message = 'Loading raster, this may take a while!', value = 0,{
      pop_raster<-readRASfromDB(fn = tableName$table_name)
      incProgress(0.4, "Processing ....")
      ## 2. Process as usual
      ###########################
      ## 2.1. Aggregation (optional)
      if ((input$aggregateRaster)>1) {
        showNotification(paste("AGGREGATING RASTER!", "This may take a while"),
                         id ="aggraster", type = "warning", session = session)
        pop_raster<-raster::aggregate(pop_raster, fact=(input$aggregateRaster),
                                      fun=sum,
                                      expand=TRUE,
                                      na.rm=TRUE)
        removeNotification(id ="aggraster")

      }
      #########################################
      ## 2.2. Change resolution if smaller 100m
      ##  1. utm check
      # if (isLonLat(pop_raster)){
      #   res_m<-distHaversine(c(extent(pop_raster)[1],extent(pop_raster)[3]), c((extent(pop_raster)[1]+res(pop_raster)[1]), extent(pop_raster)[3]))
      # } else {
      #   res_m<-res(pop_raster)
      # }

      incProgress(0.3, "Processing ....")

      # Transfrom to ident CRS
      shpCrsOld<-st_crs(harare_landuse3_type1)
      harare_landuse3_type1<-harare_landuse3_type1 %>% st_transform(raster::proj4string(pop_raster))
      # Check Overlap
      shiny::validate(need(try(
        pop_raster<-crop(pop_raster, extent(harare_landuse3_type1))
      ), message = "Files Do not overlap!"))
      harare_landuse3_type1<-harare_landuse3_type1 %>% st_transform(shpCrsOld)
      # Replace NA w 0
      pop_raster<-ras_NA_to_0(rf = pop_raster)
      # Results for info table
      if (isLonLat(pop_raster)){
        res_m<-distHaversine(c(extent(pop_raster)[1],extent(pop_raster)[3]), c((extent(pop_raster)[1]+res(pop_raster)[1]), extent(pop_raster)[3]))
      } else {
        res_m<-res(pop_raster)
      }
      incProgress(0.3, "Processing ....")

      tmp.Area<-list()
      tmp.Area$Area<-tryCatch(
        {sum(round(st_area(harare_landuse3_type1)/1000^2, 2), na.rm = T)},
        error = function(e) {"NA"}
      )
      tmp.Area$stratCount<-ifelse(input$strat=="Yes", nlevels(as.factor(harare_landuse3_type1[,input$`strVarSel-strat_var`, drop=T])), 0)
      tmp.Area$Nsquare<-ncell(pop_raster)
      tmp.Area$res<-round(res_m[1])
      ## EXPORT
      ## to info table
      new_shp$tmp.Area<-tmp.Area
      ## to processing
      ## Bookmark
      new_pop$pop_raster<-pop_raster

      ## iii. UPDATE: selectinput
      if (input$catRaster=="Yes"){
        new_pop$rasCat<-as.character(unique(raster::values(pop_raster)))
      }
    })
  })
  ##################################################################################
  ## 2.2. Raster Quick View                                                        #
  ##################################################################################
  ## 2.2.1 Modal for Raster View
  observeEvent(input$popCreate, {
    hideElement("viewRaster")
    shinyjs::disable("modify")
  })


  observeEvent(input$viewRaster,{
    showModal(modalDialog(title =tags$div(
      HTML("<center><font color='#0d47a1'><big>Uploaded/Generated Raster Image</big></font></center>")),
      fluidRow(
        withSpinner(plotOutput("rasterPlot"))
      ),
      footer = tagList(actionButton("close1","Close Viewer",
                                    icon("window-close"),
                                    style=action_btn_close)),
      easyClose = T, size = "l"
    ))
  })

  observe({
    req(new_pop$pop_raster)
    pop_raster<-new_pop$pop_raster
    showElement("viewRaster")
    shinyjs::enable("modify")
  })
  ## 2.1.2. Raster View Plot
  output$rasterPlot<-renderPlot({
    req(new_pop$pop_raster)
    Value<-new_pop$pop_raster
    raster.map<-spplot(Value,col.regions=topo.colors(100, alpha = 0.7))
    return(raster.map)
  })

  ## 2.2.3. Close Modal
  observeEvent(input$close1,{
    removeModal()
  })
  ##################################################################################
  ## 2.3. Raster Info Table                                                        #
  ##################################################################################
  new_pop.pop.info<-reactiveVal( NULL)
  output$popInfoTab <- DT::renderDataTable({
    shp.info<-new_pop$pop.info
    shiny::validate(need(shp.info, message = F))
    tabl<-unlist(shp.info)
    if (length(tabl)==7){
      names(tabl)<-c("File Type", "Projection", "Total Population", "Number of non-empty Cells", "Average Population per cell","Extent","Resolution")
    } else if (length(tabl)==10){
      names(tabl)<-c("File Type", "Projection", "Total Population", "Number of non-empty Cells", "Average Population per cell","Extent (min. lat)", "Extent (min. long)", "Extent (max. lat)", "Extent (max. long)" ,"Resolution")
    } else if (length(tabl)==11) {
      tabl<-tabl[1:10]
      names(tabl)<-c("File Type", "Projection", "Total Population", "Number of non-empty Cells", "Average Population per cell","Extent (min. lat)", "Extent (min. long)", "Extent (max. lat)", "Extent (max. long)" ,"Resolution")
    }
    new_pop.pop.info( as.data.frame(tabl))
    DT::datatable(as.data.frame(tabl), smTab, selection = "single", rownames = T,
                  colnames = c("",""),
                  style = "bootstrap") %>% infoTable
  })


  ##############################GRID###############################################
  ##        3. CREATION of the GRID                                               #
  ##              - 2 main options: regular grid, raster                          #
  #################################################################################
  ##  3.1. Modal dialog for confirmation                                          #
  #################################################################################
  new_shp.tmp.Area<-reactiveVal( NULL)
  shpArea<-reactive({
    tmp.shp<-new_shp$harare_landuse3_type1
    shiny::validate(need(tmp.shp, message = F),
                    need(input$sampType=="Adaptive Grid"|input$sampType=="Random Grid", message = F))
    ar<-tryCatch(
      {sum(round(st_area(tmp.shp)/1000^2, 2), na.rm = T)},
      error = function(e) {"NA"}
    )
    ns<-tryCatch(
      {ceiling(sum(st_area(tmp.shp), na.rm = T)/(input$cell_side^2))},
      error = function(e) {"NA"}
    )

    tmp.Area<-list(Area=ar, Nsquare=ns)
    return(tmp.Area)
  })
  ## Cluster boundaries without Person
  observeEvent(input$sampType, {
    if(input$sampType=="Random Cluster" & input$popCreate=="No") {
      shinyjs::enable("genSamp")
    }
  })

  ##  2. Create MODAL to confirm
  observeEvent(input$modify, {
    tmp.shp<-new_shp$harare_landuse3_type1
    shiny::validate(need(tmp.shp, message = "Select file first!"))
    ## Check for storemode
    if(is.null(storemode())) {
      showNotification("Please select storage mode first!",
                       type = "warning", id = "no_storage", duration = NULL)
      req(FALSE)
    }
    if(input$strat=="Yes") {
      shiny::validate(need(input$`strVarSel-strat_var`, message = F))
    }
    if(input$popCreate=="No") {
      tmp.Area<-shpArea()
      tmp.Area$stratCount<-ifelse(input$strat=="Yes", nlevels(as.factor(tmp.shp[,input$`strVarSel-strat_var`, drop=T])), 0)
      tmp.Area$res<-(input$cell_side/1000)^2
    } else if (input$popCreate=="Yes") {
      tmp.Area<-new_shp$tmp.Area

      # check for population data with raster
      if (input$popUpType==2 && is.null(new_pop$pop_raster)) {
        showModal(modalDialog(
          title = "Missing Population Data!",
          "You must uploaded the population data first!",
          easyClose = TRUE,
          footer = NULL
        ))
        shiny::validate(need(!is.null(tmp.Area), message = "Select file first!"))
      }

      # check for population data with raster
      if (input$popUpType==1 && is.null(popFilePoint())) {
        showModal(modalDialog(
          title = "Missing Population Data!",
          "You must uploaded the population data first!",
          easyClose = TRUE,
          footer = NULL
        ))
        shiny::validate(need(!is.null(tmp.Area), message = "Select file first!"))
      }

    }

    # Info tables
    # 1. Raster
    if (input$popUpType==2) {
      tabl<-data.table(character(4), numeric(4))
      tabl[,V1:=c("Total Area (sqKM)", "Cell side length (m)", "Appr. Number of Squares", "Number of Strata")]
      tabl[,V2:=c(as.numeric(tmp.Area$Area),
                  tmp.Area$res,
                  as.numeric(tmp.Area$Nsquare),
                  tmp.Area$stratCount)]
      ##  FOR REPORT
      new_shp.tmp.Area( as.data.frame(tabl))
    }

    # 2. Points
    if (input$popUpType==1) {
      tabl<-data.table(character(4), numeric(4))
      tabl[,V1:=c("Total Area (sqKM)", "Number of Clusters", "Appr. Number of Points", "Number of Strata")]
      tabl[,V2:=c(as.numeric(tmp.Area$Area),
                  tmp.Area$res,
                  as.numeric(tmp.Area$Nsquare),
                  tmp.Area$stratCount)]

    }

    # Modal head
    if(input$sampType=="Random Grid") {
      modalHead<-"grid frame"

    } else if(input$sampType=="Random Cluster") {
      modalHead<-"cluster frame"

    }
    modalHeadFull<-paste0("<center><font color='#7f0000'><big>Attention, this will create the following ",
                          modalHead,
                          ":</big></font></center>")
    showModal(modalDialog(title =tags$div(
      HTML(modalHeadFull)),
      DT::renderDataTable({
        DT::datatable(tabl, options = list(dom = 't'), rownames = F,
                      colnames = c("","")) %>% infoTable
      }),
      conditionalPanel("input.strat=='Yes'",
                       fluidRow(
                         column(4,
                                radioButtons("subSTRAT", "Subset Regions", choices = c("Yes", "No"), inline = T,
                                             selected = "No")),
                         column(4,
                                conditionalPanel("input.subSTRAT=='Yes'",
                                                 helpText("By clicking on the map below, you can select the Strata for your survey. If you only require sub-regions, this will save you
                                                          processing time."))),
                         column(4)),
                       conditionalPanel("input.subSTRAT=='Yes'",
                                        # MinMap modal (leaflet)
                                        mini_ampUI("mapModulePopUP")
                       )
      ),
      footer = tagList(
        fluidRow(
          column(6,
                 actionButton("conf1","Confirm?",
                              icon("check"),
                              style="color: #FFFFFF;
                                          background-color: #7f0000;
                                          border-color: #7f0000")
          ),
          column(4)
        )
      ),
      easyClose = T, size = "l"
    )
    )
  }, ignoreInit = T)

  ########################################################################
  ##  3.1.1 MODAL (map for subsetting)                                   #
  ########################################################################
  # polySelFullOuter<-reactiveValues(ID=NULL, subStrat=NULL)
  # ##  1.1 Data Intake
  harare_landuse3_type1_sub<-reactive({
    ## USE SIMPLIFIED VERSION FROM BASE MAP
    DF<-harare_landuse3_type1()
    shiny::validate(need(DF, message = F),
                    need(input$`strVarSel-strat_var`!="", message = F))

    # cast if geometry
    dfgeo<-DF %>% sf::st_geometry_type(by_geometry = F)
    if(dfgeo=="GEOMETRY") DF %>% st_cast("MULTIPOLYGON")

    # Clean stratvar string
    DF[[input$`strVarSel-strat_var`]]<-stringr::str_remove_all(DF[[input$`strVarSel-strat_var`]], "[^[:alnum:]]")

    if(nrow(DF)!=length(unique(DF[,names(DF)==input$`strVarSel-strat_var`, drop = T]))){
      DF<-DF %>% group_by(.data[[input$`strVarSel-strat_var`]]) %>% summarise(Count=n())
      names(DF)<-c(input$`strVarSel-strat_var`,"Count","geometry")
    } else {
      DF<-DF %>%
        select(.data[[input$`strVarSel-strat_var`]])
      DF$Count=1
    }
    return(DF)
  })

  # Modaly for MAP/Selection Table
  polySelFullOuter<-mini_mapServer("mapModulePopUP",
                                   polygonLayer = harare_landuse3_type1_sub,
                                   polygonNames = reactive(input$`strVarSel-strat_var`))

  ########################################################################
  ####  CHECKPOINT & HANDOVER
  ########################################################################
  ## 1. Modify Data
  ##    - this triggers the actual processing!
  observeEvent(input$conf1, {
    if(input$subSTRAT == "Yes") {
      #############################
      ## sub stratum pre-processing
      rIDs<-polySelFullOuter$ID
      tmp.shp<-harare_landuse3_type1_sub()
      tmp.shp.main<-harare_landuse3_type1()
      shiny::validate(need(length(rIDs)>0, message = "Nothing selected"))
      #names(tmp.shp)<-c(input$`strVarSel-strat_var`,"Pop","geometry")
      ## SUBSET mini map
      tmp.shp$stratum_numeric<-tmp.shp[[input$`strVarSel-strat_var`]]
      tmp.shp<-tmp.shp[rIDs, ]
      ## MERGE sub with full set
      #tmp.shp<-merge(tmp.shp.main, st_set_geometry(tmp.shp, NULL))
      tmp.shp<-tmp.shp.main[tmp.shp,]
    } else {
      ##################################
      ## full frame pre-processing
      tmp.shp<-new_shp$harare_landuse3_type1
      tmp.shp$stratum_numeric<-tmp.shp[[input$`strVarSel-strat_var`]]
    }

    new_shp$harare_landuse3_type1_sub<-tmp.shp

  }, ignoreInit = T)
  #####################################################
  ## 2. Directly to sampling
  ##  2.1.1 Poly with no population directly
  observeEvent(input$`strVarSel-strat_var`, {
    if(input$sampType=="Random Cluster" & input$popCreate == "No") {
      cluster_poly<-new_shp$harare_landuse3_type1
      shiny::req(cluster_poly)
      ## Attention replace group_by/summarize with aggregate
      # popSizeStrat<-cluster_poly %>% group_by(.data[[input$`strVarSel-strat_var`]]) %>% summarise(Pop=n(), Cells=n())
      #showNotification("Aggregating Stratum")
      cluster_poly$ones<-1
      popSizeStrat<-aggregate(x = cluster_poly[, "ones"], by = list(cluster_poly[[input$`strVarSel-strat_var`]]), FUN = sum, na.rm=T)
      data.table::setnames(popSizeStrat, c("Group.1", "ones"), c(input$`strVarSel-strat_var`, "Pop"))
      popSizeStrat$Cells<-popSizeStrat$Pop
      #popSizeStrat$stratum_numeric<-popSizeStrat[[input$`strVarSel-strat_var`]]
      new_pop$popSizeStrat<-popSizeStrat
    }
  })

  #################################################################################
  ##  3.2. GRID/RASTER Creation                                                   #
  #################################################################################
  observeEvent(input$conf1,{
    removeModal()
    tmp.shp<-new_shp$harare_landuse3_type1_sub
    shiny::req(tmp.shp)
    #shiny::validate(need(input$cell_side, message = F))
    shinyjs::enable("dwl_frame_table")
    shinyjs::enable("genSamp")
    ########################################################################
    ##    i. REGULAR GRID
    ########################################################################
    if(input$popCreate=="No"){
      if (isolate(input$sampType=="Random Grid")){
        shiny::validate(need(input$cell_side, message = F))
        if (input$strat=="No"){
          ########################################################################
          ## Regular Grid (no stratification)
          withProgress(message = 'Creating Grid, this may take a while!', value = 0,{
            sp_grd_strat<-create_grid_ras_old_SF(tmp.shp, cellsize = input$cell_side)
            sp_grd_strat<-project_to_utm(sp_grd_strat)
            sp_grd_strat$Y<-(st_coordinates(st_centroid(sp_grd_strat))[,2])
            sp_grd_strat$X<-(st_coordinates(st_centroid(sp_grd_strat))[,1])
            sp_grd_strat$GRIDID<-sprintf("Lat%dLon%d", floor(sp_grd_strat$Y/input$cell_side),
                                         floor(sp_grd_strat$X/input$cell_side))
            sp_grd_strat$X<-NULL
            sp_grd_strat$Y<-NULL
          })
        } else if (input$strat=="Yes") {
          ########################################################################
          ## Regular Grid (with stratification)
          shiny::validate(need(input$`strVarSel-strat_var`, message = F),
                          need(length(grep(input$`strVarSel-strat_var`, names(tmp.shp)))>0, message = F))
          withProgress(message = 'Creating Grid, this may take a while!', value = 0,{
            tmp.shp<-tmp.shp %>% group_by(.data[[input$`strVarSel-strat_var`]]) %>% summarise(Count=n())
            sp_grd_strat<-create_grid_ras_old_SF(tmp.shp, cellsize = input$cell_side, stratum = input$`strVarSel-strat_var`)
            ## Creat GRIDID
            sp_grd_strat<-project_to_utm(sp_grd_strat)
            sp_grd_strat$Y<-(st_coordinates(st_centroid(sp_grd_strat))[,2])
            sp_grd_strat$X<-(st_coordinates(st_centroid(sp_grd_strat))[,1])
            sp_grd_strat$GRIDID<-sprintf("Lat%dLon%d", floor(sp_grd_strat$Y/input$cell_side),
                                         floor(sp_grd_strat$X/input$cell_side))
            sp_grd_strat$X<-NULL
            sp_grd_strat$Y<-NULL

          })

          popSizeStrat<-sp_grd_strat %>% dplyr::group_by(stratum_numeric) %>% dplyr::summarise(Pop=n(), Cells=n())
          new_pop$popSizeStrat<-popSizeStrat
        }
        ########################################################################
        ##  Regular grid (info table)

        pop.info<-list()
        pop.info$class<-"Grid"
        pop.info$p4s<-st_crs(sp_grd_strat)$proj4string
        pop.info$totPop<-NA
        pop.info$nCell<-nrow(sp_grd_strat)
        pop.info$avPop<-NA
        pop.info$extent<-round(st_bbox(sp_grd_strat), 6)
        pop.info$resolution<-input$cell_side
        new_pop$pop.info<-pop.info
        ########################################################################
        ##  Regular Grid (return)
        new_shp$sp_grd_strat<-sp_grd_strat
        #new_shp$sp_grd_strat_poly<-sp_grd_strat
      } else if(input$sampType=="Random Cluster") {
        tmp.shp$stratum_numeric<-tmp.shp[[input$`strVarSel-strat_var`]]
        new_shp$sp_grd_strat<-tmp.shp
      }
    } else if(input$popCreate=="Yes") {
      ########################################################################
      ##    ii. RASTER POPULATION
      ########################################################################
      pop_raster<-new_pop$pop_raster
      harare_landuse3_type1<-new_shp$harare_landuse3_type1_sub

      withProgress(message = 'Transforming Data, this may take a while!', value = 0,{
        if (isolate(input$sampType=="Random Grid")){
          #########################################################################
          ##        NO STRAT                                                      #
          #########################################################################
          if (input$strat=="No") {
            harare_landuse3_type1$ones<-1


            sp_grd_strat_poly<-harare_landuse3_type1 %>% group_by(ones) %>% summarise()

            incProgress(0.2)
            shpCrsOld<-st_crs(sp_grd_strat_poly)
            sp_grd_strat_poly<-st_transform(sp_grd_strat_poly, (raster::proj4string(pop_raster)))
            pop_raster<-crop(pop_raster, extent(sp_grd_strat_poly))
            incProgress(0.2)
            sp_grd_strat_poly.ras<-fasterize(sp_grd_strat_poly, pop_raster, fun="max")

            sp_grd_strat_poly.ras[]<-sp_grd_strat_poly.ras[]*pop_raster[]
            incProgress(0.2)
            popSize<-round(sum(sp_grd_strat_poly.ras[], na.rm = T), digits = 0)
            ras_points<-data.table(getValues((sp_grd_strat_poly.ras)))
            ras_points[,CID:=1:.N][,X:=floor(xFromCell(tmp.poly.ras, 1:nrow(ras_points))/1000)]
            ras_points[,Y:=floor(yFromCell(tmp.poly.ras, 1:nrow(ras_points))/1000)]
            ras_points[,GRIDID:=sprintf("Lat%dLon%d", Y, X)]
            ras_points[,X:=NULL][,Y:=NULL]
            ras_points<-copy(ras_points[!is.na(V1)])
            ras_points[,stratum_numeric:=tmp.poly$stratum_numeric]
            ras_points[,TOTPOP:=ceiling(sum(V1))]

            pop_raster_points<-ras_points
            sp_grd_strat_poly<-st_transform(sp_grd_strat_poly, shpCrsOld)
            sp_grd_strat_poly$Pop<-popSize

          } else if (input$strat=="Yes"){
            #########################################################################
            ##        STRAT                                                         #
            #########################################################################
            shiny::validate(need(input$`strVarSel-strat_var`, message = F))
            harare_landuse3_type1$ones<-1
            #######################################
            ## AGGREGATION!!
            ##  - RANDOM GRID always runs withing the STRATUM, even if cluster file!
            if(nrow(harare_landuse3_type1)>n_distinct(harare_landuse3_type1$stratum_numeric)){
              ## i. Number of elements in boundaries is higher than stratum -->agg to stratum
              sp_grd_strat_poly<-harare_landuse3_type1 %>%
                group_by(.data[[input$`strVarSel-strat_var`]], stratum_numeric) %>%
                summarise()
            } else {
              ## ii. Number of elements in boundaries is same --> only select stratum column and aggregate
              sp_grd_strat_poly<-harare_landuse3_type1 %>%
                select(.data[[input$`strVarSel-strat_var`]], stratum_numeric)
            }

            ## CRS Transformation:
            ## 1. For cropping use raster, but transform back then project area to utm
            crsOld<-st_crs(sp_grd_strat_poly)
            sp_grd_strat_poly<-st_transform(sp_grd_strat_poly, (raster::proj4string(pop_raster)))
            pop_raster<-raster::crop(pop_raster, raster::extent(sp_grd_strat_poly))
            sp_grd_strat_poly<-st_transform(sp_grd_strat_poly, crsOld)

            ## 2. reproject raster to metered UTM (zone is based on location)--> required for GRIDID
            sp_grd_strat_poly<-project_to_utm(sp_grd_strat_poly)
            pop_raster<-projectRaster(pop_raster, crs = st_crs(sp_grd_strat_poly)[1]$input)



            #######################################
            ## 3. MC for extraction ## MC MC MC not #future!!
            #future::plan(sequential) ## turn off #future?
            ## 3.1 parallel setup
            cores<-data.table::getDTthreads()
            cl<-parallel::makeCluster(cores)
            doParallel::registerDoParallel(cl)

            ##  2. N simu and packages
            simu<-nrow(sp_grd_strat_poly)
            pack_dp_sp<-c("raster", "sp", "rgdal", "broom",
                          "data.table", "plyr", "dplyr", "shiny", "sf", "fasterize", "spex")

            incProgress(0.3)
            final<-foreach(i=1:simu, .packages = pack_dp_sp,
                           #.combine=list,
                           .multicombine = F,
                           #.export = c("a"),
                           #.verbose = T,
                           .errorhandling="pass") %dopar% {
                             tmp.poly<-sp_grd_strat_poly[i,]
                             if (st_is_empty(tmp.poly)) {
                               return(NULL)
                             } else {
                               tmp.ras<-crop(pop_raster, extent(tmp.poly))
                               #if(is.null(tmp.ras)) next()

                               tmp.poly.ras<-fasterize(tmp.poly, tmp.ras, fun="max")
                               tmp.poly.ras[]<-tmp.poly.ras[]*tmp.ras[]
                               ras_points<-data.table(getValues((tmp.poly.ras)))
                               xres<-raster::xres(tmp.poly.ras)
                               yres<-raster::yres(tmp.poly.ras)
                               ras_points[,CID:=1:.N][,X:=floor(xFromCell(tmp.poly.ras, 1:nrow(ras_points))/xres)]
                               ras_points[,Y:=floor(yFromCell(tmp.poly.ras, 1:nrow(ras_points))/yres)]
                               ras_points[,GRIDID:=sprintf("Lat%dLon%d", Y, X)]
                               ras_points[,X:=NULL][,Y:=NULL]
                               ras_points<-copy(ras_points[!is.na(V1)])
                               ras_points[,stratum_numeric:=tmp.poly$stratum_numeric]
                               ras_points[,TOTPOP:=ceiling(sum(V1))]
                               return(ras_points)
                             }
                           }
            parallel::stopCluster(cl)

            incProgress(0.6)

            ########################
            ##  TO sampling, complete DATA table
            if(is.data.table(final)) {ras_points<-final} else {ras_points<- rbindlist(final)}
            popSize<-ceiling(sum(ras_points$V1))
            popSizeStrat<-ras_points %>% group_by(stratum_numeric) %>% summarise(Pop=ceiling(sum(V1)), Cells=n_distinct(CID))
            new_pop$popSizeStrat<-popSizeStrat

            popSizeStrat$stratum_numeric<-NULL
            sp_grd_strat_poly<-dplyr::bind_cols(sp_grd_strat_poly, popSizeStrat)
            setnames(ras_points, "V1", "Pop")
            rm(final)

            # ########################
            # ##  TO sampling, complete DATA table
            # sp_grd_strat_poly$Pop<-Pop
            # sp_grd_strat_poly$CID<-c(1:nrow(sp_grd_strat_poly))
            # shiny::validate(need(input$`strVarSel-strat_var`, message = T))
            # ## unique stratum
            # sp_grd_strat_poly$stratum_numeric<-sp_grd_strat_poly[,input$`strVarSel-strat_var`, drop=T]
            # ## points for sample
            # ras_points<-data.table(sp_grd_strat_poly %>% st_set_geometry(NULL))
            # ras_points[,CID:=1:.N]
            # popSize<-sum(ras_points$Pop)
            # rm(Pop)

          }
          ##############################################################
          ##  RANDOM CLUSTER
          ##############################################################
        } else if (isolate(input$sampType=="Random Cluster")){
          if (isolate(input$popUpType==1)) {
            # Points processing

            # inputs
            # Use non simplified version of polygons
            #fullPoly<-new_shp$harare_landuse3_type1
            sp_grd_strat_poly<-harare_landuse3_type1
            sp_grd_strat_poly$CID<-1:nrow(sp_grd_strat_poly)
            pop_points<-popFilePoint()

            sp_grd_strat_poly<-aggregatPointsToBoundsgeos(sp_grd_strat_poly, pop_points)
            if (input$strat=="Yes") {
              shiny::validate(need(input$`strVarSel-strat_var`, message = T))
              #sp_grd_strat_poly$stratum_numeric<-sp_grd_strat_poly[[input$`strVarSel-strat_var`]]
              sp_grd_strat_poly$stratum_numeric<-as.numeric(as.factor(sp_grd_strat_poly$stratum_numeric))
            }
            ras_points<-data.table(sp_grd_strat_poly %>% st_set_geometry(NULL))
            #ras_points[,CID:=1:.N]
            popSize<-sum(ras_points$Pop)

          } else if (isolate(input$popUpType==2)){
            # Raster processing
            if (isolate(input$strat=="No")){
              #################################
              ## i. not stratified
              harare_landuse3_type1$ones<-1
              sp_grd_strat_poly<-harare_landuse3_type1
              sp_grd_strat_poly<-st_transform(sp_grd_strat_poly, (raster::proj4string(pop_raster)))
              ########################################
              ## 1.1 Switch to SF/STARS
              ##  - stars proxy --> MUCH faster
              ########################################
              ## i. write the raster to tmp
              tmpFile<-tempfile("raster_proxy", fileext = ".tif")
              unlink(tmpFile)
              writeRaster(pop_raster, tmpFile); rm(pop_raster)
              ## ii. read Proxy
              pop_raster<-read_stars(tmpFile, proxy = TRUE)

              incProgress(0.15, message = "Preparing Loop ...")

              ##  2. N simu and packages
              simu<-nrow(sp_grd_strat_poly)
              pack_dp_sp<-c("stars", "sf",
                            "plyr", "dplyr")

              #######################################
              ## 3. MC for extraction ## MC MC MC not #future!!
              #future::plan(sequential) ## turn off #future?
              ## 3.1 parallel setup
              cores<-data.table::getDTthreads()
              cl<-parallel::makeCluster(cores)
              doParallel::registerDoParallel(cl)

              ## 2.1. Continous Data
              if (input$catRaster=="No"){

                incProgress(0.3, message = "Aggregation in Progress.")
                Pop<-foreach(i=1:simu, .packages = pack_dp_sp,
                             .combine=c,
                             .verbose = F,
                             .errorhandling="stop") %dopar% {
                               tmp.poly<-sp_grd_strat_poly[i,] %>% st_cast()
                               ## ATTENTION: EMPTY geometries receive pop value 1
                               if (st_is_empty(tmp.poly)) {
                                 pop<-1
                               } else {
                                 tmp.ras<-crop(pop_raster, extent(tmp.poly))
                                 tmp.poly.ras<-(fasterize(tmp.poly, tmp.ras, fun="max", background = NA))
                                 tmp.poly.ras[]<-(tmp.poly.ras[]*tmp.ras[])
                                 pop<-(ceiling((sum(tmp.poly.ras[], na.rm = T))))
                               }
                               return(pop)
                             }
                parallel::stopCluster(cl)
              } else {
                ## 2.2. Categorical Data
                incProgress(0.3, message = "Aggregation in Progress.")
                loop.time<-system.time(
                  Pop<-foreach(i=1:simu, .packages = pack_dp_sp,
                               .combine=c,
                               .multicombine = T,
                               #.export = c("a"),
                               #.verbose = T,
                               .errorhandling="pass") %dopar% {
                                 tmp.poly<-sp_grd_strat_poly[i,] %>% st_cast()
                                 tmp.ras<-crop(pop_raster, extent(tmp.poly))
                                 tmp.poly.ras<-fasterize(tmp.poly, tmp.ras, fun="sum", background = NA)
                                 tmp.poly.ras[]<-tmp.poly.ras[]*tmp.ras[]
                                 pop<-round((modal(tmp.poly.ras[], na.rm = T)))
                                 return(pop)
                               }
                )
              }
              incProgress(0.6)
              ########################
              ##  TO sampling, complete DATA table
              sp_grd_strat_poly$Pop<-Pop
              if (input$strat=="Yes") {
                shiny::validate(need(input$`strVarSel-strat_var`, message = T))
                sp_grd_strat_poly$stratum_numeric<-sp_grd_strat_poly[[input$`strVarSel-strat_var`]]
                sp_grd_strat_poly$stratum_numeric<-as.numeric(sp_grd_strat_poly$stratum_numeric)
              }
              ras_points<-data.table(sp_grd_strat_poly %>% st_set_geometry(NULL))
              ras_points[,CID:=1:.N]
              popSize<-sum(ras_points$Pop)
              rm(Pop)
            } else if (isolate(input$strat=="Yes")) {
              ##############################################################
              ##  iii. Stratified
              ##############################################################
              #   1. Data & transformation
              sp_grd_strat_poly<-harare_landuse3_type1

              ########################################
              ## 1.1 Switch to SF/STARS
              ##  - stars proxy --> MUCH faster
              ########################################
              ## i. write the raster to tmp
              tmpFile<-tempfile("raster_proxy", fileext = ".tif")
              unlink(tmpFile)

              writeRaster(pop_raster, tmpFile); rm(pop_raster)
              ## ii. read Proxy
              pop_raster<-read_stars(tmpFile, proxy = TRUE)
              incProgress(0.15, message = "Preparing Loop ...")

              ##  2. N simu and packages
              simu<-nrow(sp_grd_strat_poly)
              pack_dp_sp<-c("stars", "sf",
                            "plyr", "dplyr")

              #######################################
              ## 3. MC for extraction ## MC MC MC not #future!!
              #future::plan(sequential) ## turn off #future?
              ## 3.1 parallel setup
              cores<-data.table::getDTthreads()
              cl<-parallel::makeCluster(cores)
              doParallel::registerDoParallel(cl)

              ## 3.2. Continous Data
              if (input$catRaster=="No"){
                ## parallel loop
                incProgress(0.15, message = "Aggregation in Progress.")
                Pop<-foreach(i=1:simu, .packages = pack_dp_sp,
                             .combine="c",
                             .verbose = F,
                             .errorhandling="pass") %dopar% {
                               ## 1. Subset Polygon & adjust WKT
                               tmp.poly<-sp_grd_strat_poly[i,] %>% st_cast()
                               tmp.poly<-tmp.poly %>% st_transform(crs = st_crs(pop_raster))
                               ## ATTENTION: EMPTY geometries receive pop value 1
                               if (st_is_empty(tmp.poly)) {
                                 pop<-1
                               } else {
                                 tmp.ras<-st_crop(pop_raster, tmp.poly, crop = T)
                                 tmp.ras<-suppressMessages(
                                   tryCatch(
                                     st_as_stars(tmp.ras),
                                     error = function(e) {return(1)}
                                   )
                                 )

                                 if(class(tmp.ras)=="stars") {
                                   pop<-round(sum(tmp.ras[[1]], na.rm = T))
                                 } else {
                                   pop<-tmp.ras
                                 }

                               }
                               return(pop)
                             }
                parallel::stopCluster(cl)

              } else {
                ## 2.2. Categorical Data
                #registerDoFuture()
                ## parallel loop
                incProgress(0.15, message = "Aggregation in Progress.")
                loop.time<-system.time(
                  Pop<-foreach(i=1:simu, .packages = pack_dp_sp,
                               .combine="c",
                               .verbose = F,
                               .errorhandling="pass") %dopar% {
                                 tmp.poly<-sp_grd_strat_poly[i,] %>% st_cast()
                                 tmp.ras<-crop(pop_raster, extent(tmp.poly))
                                 tmp.poly.ras<-fasterize(tmp.poly, tmp.ras, fun="sum", background = NA)
                                 tmp.poly.ras[]<-tmp.poly.ras[]*tmp.ras[]
                                 pop<-round((modal(tmp.poly.ras[], na.rm = T)))
                                 return(pop)
                               }
                )
              }
              incProgress(0.6, message = "Almost done.")
              ########################
              ##  TO sampling, complete DATA table
              sp_grd_strat_poly$Pop<-Pop
              sp_grd_strat_poly$CID<-c(1:nrow(sp_grd_strat_poly))
              shiny::validate(need(input$`strVarSel-strat_var`, message = T))
              ## unique stratum
              sp_grd_strat_poly$stratum_numeric<-sp_grd_strat_poly[[input$`strVarSel-strat_var`]]
              ## points for sample
              ras_points<-data.table(sp_grd_strat_poly %>% st_set_geometry(NULL))
              ras_points[,CID:=1:.N]
              CHECKraspoints<-ras_points

              popSize<-sum(ras_points$Pop)
              rm(Pop)
            }
          }
        }

        #######################################################
        ##  B. Get INFO and send to table for modified pop grid
        if (isolate(input$popUpType==1 & isolate(input$sampType=="Random Cluster"))) {
          pop.info<-list()
          pop.info$class<-class(pop_points)[1]
          pop.info$p4s<-st_crs(sp_grd_strat_poly)$wkt
          pop.info$totPop<-ceiling(popSize)
          pop.info$nCell<-nrow(ras_points)
          pop.info$avPop<-round(mean(ras_points$Pop, na.rm = T), digits = 2)
          pop.info$extent<-st_bbox(sp_grd_strat_poly)
          pop.info$resolution<-NA
          new_pop$pop.info<-pop.info
          setProgress(1)

        } else if (isolate(input$popUpType==2) | (isolate(input$sampType=="Random Grid") & isolate(input$popUpType==1))) {
          if("stars" %in% class(pop_raster)){
            resRas<-st_dimensions(pop_raster)$x$delta
            p4s<-st_dimensions(pop_raster)$x$refsys$wkt
            bbRas<-round(st_bbox(pop_raster), 6)
            #popSize<-sum(pop_raster[[1]], na.rm = T)
          } else {
            if (isLonLat(pop_raster)){
              resRas<-distHaversine(c(extent(pop_raster)[1],
                                      extent(pop_raster)[3]),
                                    c((extent(pop_raster)[1]+res(pop_raster)[1]),
                                      extent(pop_raster)[3]))
              resRas<-round(resRas, 2)
            } else {
              resRas<-res(pop_raster)
              resRas<-round(resRas, 2)
            }
            p4s<-raster::proj4string(pop_raster)
            bbRas<-round(bbox(pop_raster), 6)
            popSize<-sum(pop_raster[], na.rm = T)
          }
          pop.info<-list()
          pop.info$class<-class(pop_raster)[1]
          pop.info$p4s<-p4s
          pop.info$totPop<-ceiling(popSize)
          pop.info$nCell<-nrow(ras_points)
          pop.info$avPop<-round(mean(ras_points$Pop, na.rm = T), digits = 2)
          pop.info$extent<-bbRas
          pop.info$resolution<-resRas
          new_pop$pop.info<-pop.info
          setProgress(1)
        }

      })
      #################H##################
      ##  FOR PLOT taken in leafletproxy
      ##    ATTENTION: done in map observer!!!!
      # sp_grd_strat_poly<-sp_grd_strat_poly %>% group_by(stratum_numeric) %>%
      #   summarise(Pop=ceiling(sum(Pop)), Cells=n_distinct(CID))
      ############################################
      ##    RETURNS - to basemap proxy, to sample, to sample size table modal
      new_shp$sp_grd_strat_poly<-sp_grd_strat_poly
      ##  FOR SAMPLE (only zonal stats!!)
      new_shp$sp_grd_strat_points<-ras_points
      new_shp$pop_raster<-pop_raster
      ## SAMPLING TABLE RETURN
      if(input$sampType=="Random Grid" & input$strat=="No") ras_points$stratum_numeric<-"No Stratification"
      popSizeStrat<-ras_points %>% group_by(stratum_numeric) %>% summarise(Pop=ceiling(sum(Pop)), Cells=n_distinct(CID))
      new_pop$popSizeStrat<-popSizeStrat
    }


  }, ignoreInit = T)

  #################################################################################
  ##  3.3. GRID/RASTER Plot                                                       #
  ##      --> this is now all done in reactive harare_landuse3 (above)            #
  #################################################################################




  ########################################################################
  ##          4. SAMPLE                                                  #
  ##    1) START with modal                                              #
  ##      1.1. includes a map for subset selection                       #
  ##    2) Transform all shape to points                                 #
  ##    3) Sample and extract shapes                                     #
  ########################################################################
  sample_square<-reactiveValues()
  sample_seed<-reactiveVal(NULL)

  ########################################################################
  ##  4.1. MODAL                                                         #
  ########################################################################
  observeEvent(input$genSamp,{
    DF<-new_shp$harare_landuse3_type1
    pop_raster<-new_shp$pop_raster
    rasCat<-new_pop$rasCat

    rasCat<-ifelse(is.null(rasCat) | input$catRaster=="No", "No Categorical Raster!", as.character(unique(raster::values(pop_raster))))
    shiny::validate(need(DF, message = F))
    varNam<-names(DF %>% st_set_geometry(NULL))

    showModal(modalDialog(title =tags$div(
      HTML("<center><font color='#7f0000'><big>Please provide the required sampling information:</big></font></center>")),
      tagList(
        tags$style(HTML("
                        .help-block {color:#33D2FF; text-align: justify;font-size: 12px;}
                        ")),
        fluidPage(
          ## A. Survey Solutions
          fluidRow(
            column(4,
                   fluidRow(
                     column(6,
                            conditionalPanel("input.sampSizeStrat=='No'",
                                             numericInput("sampSize", "Sample Size",
                                                          value = 100, min=0, step = 1)),br(),

                     ),
                     column(6,
                            numericInput("sampSEED", "Set Seed",
                                         value = floor(runif(1, 1000,9999)), min=0, step = 1)
                     )
                   ),
                   fluidRow(
                     helpText("Provide the number of squares to be sampled, in case of stratification, provide either a
                          single number for equal allocation provide individual sample sizes below")
                   )
            ),

            column(4,
                   conditionalPanel("input.popCreate=='Yes'|input.sampType=='Random Cluster'",
                                    radioButtons("sampPPS", "Apply PPS sampling?", choices = c("Yes", "No"), inline = T,
                                                 selected = "No"),
                                    conditionalPanel("input.sampPPS=='Yes' & input.sampType=='Random Grid'",
                                                     numericInput("minCell", "Minimum Grid Value?", 0, step = 1, width = "100%")),
                                    conditionalPanel("input.catRaster=='Yes'& input.sampPPS=='Yes'",
                                                     selectizeInput("catRaster_domCat", "Select Dominant Category", choices = rasCat,
                                                                    options = list(
                                                                      placeholder = 'Select variable bellow',
                                                                      onInitialize = I('function() { this.setValue(""); }')
                                                                    )
                                                     )
                                    ),
                                    conditionalPanel("input.sampType=='Random Cluster'& input.sampPPS=='Yes' & input.popCreate=='No'",
                                                     selectizeInput("rcluster_MOS", "Select Size Variable", choices = varNam,
                                                                    options = list(
                                                                      placeholder = 'Select variable bellow',
                                                                      onInitialize = I('function() { this.setValue(""); }')
                                                                    )
                                                     )
                                    )
                   )
            ),
            column(4,
                   conditionalPanel("input.popCreate=='Yes'|input.sampType=='Random Cluster'",
                                    radioButtons("stratCatProp", "Apply Raster Stratification within Domains?",
                                                 choices = c("Yes", "No"), inline = T,
                                                 selected = "No"),br(),br()))
          ),
          fluidRow(
            column(4,
                   conditionalPanel("input.strat=='Yes'",
                                    radioButtons("sampSizeStrat", "Unequal allocation?",
                                                 choices = c("Yes", "No"), inline = T,
                                                 selected = "No"),br(),br()),
                   conditionalPanel("input.sampSizeStrat=='Yes'",
                                    radioButtons("maxSampSize", "Allocate Total?",
                                                 choices = c("Yes", "No"), inline = T,
                                                 selected = "No"),br(),br()),
                   conditionalPanel("input.maxSampSize=='Yes'",
                                    numericInput("maxSampSizeNum", "Total Sample Size?",
                                                 value = 100,
                                                 step = 1,
                                                 width = "100%"),
                                    conditionalPanel("input.sampPPS=='Yes'",
                                                     radioButtons("maxSampSizePop", "Allocate using Cell Population?",
                                                                  choices = c("Yes", "No"), inline = T,
                                                                  selected = "No")),
                                    actionButton("maxSampSizeCalc","Recalculate Allocation?",
                                                 icon("calculator"),
                                                 style="color: #FFFFFF;
                                                        background-color: #0d47a1;
                                                        border-color: #0d47a1"))
            ),
            column(6,
                   conditionalPanel("input.sampSizeStrat=='Yes'",
                                    #DT::dataTableOutput("ssStratTable")
                                    rHandsontableOutput("hotout", height = 400, width = 500)
                   )
            ),
            column(2)
          )
        )),
      footer = tagList(actionButton("conf3","Confirm?",
                                    icon("check"),
                                    style="color: #FFFFFF;
                                          background-color: #7f0000;
                                          border-color: #7f0000")),
      easyClose = T, size = "l"
    ))
  })

  ########################################################################
  ## 4.1.1. SampSize table                                               #
  ########################################################################
  stratSamp<-reactiveValues(nStrat=NULL)
  frame_CV<-eventReactive(input$genSamp, {
    tabl<-new_pop$popSizeStrat
    shiny::validate(need(tabl, message = "Nothing selected"))
    if("sf" %in% class(tabl)) tabl<-tabl %>% st_set_geometry(NULL)
    ##############################
    ## Table for unequal allocation
    if(input$sampType=="Random Grid") {
      ###################
      ## i. Random Grid
      names(tabl)<-c("Stratum", "Population", "Cells")
      # take 10% of grid cells as initial value for sample
      tabl$Sample<-ceiling(tabl$Cells*0.1)
    } else if (input$sampType=="Random Cluster") {
      ###################
      ## ii. Random Cluster
      names(tabl)<-c("Stratum", "Population", "Cluster")
      # take 10% of clusters as initial value for sample
      tabl$Sample<-ceiling(tabl$Cluster*0.1)
    }
    return(tabl)
  })



  output$hotout<-renderRHandsontable({
    frame_CV<-frame_CV()
    if(input$sampType=="Random Grid") {
      rhandsontable((frame_CV), rowHeaders = NULL, stretchH = "all") %>%
        hot_col("Stratum", readOnly = TRUE) %>%
        hot_col("Population", readOnly = TRUE) %>%
        hot_col("Cells", readOnly = TRUE) %>%
        hot_col("Sample", readOnly = FALSE, ) %>%
        htmlwidgets::onRender(
          "function(el){var hot = this.hot; setTimeout(function(){hot.render();}, 1000)}"
        )
    } else if (input$sampType=="Random Cluster"){
      rhandsontable((frame_CV), rowHeaders = NULL, stretchH = "all") %>%
        hot_col("Stratum", readOnly = TRUE) %>%
        hot_col("Population", readOnly = TRUE) %>%
        hot_col("Cluster", readOnly = TRUE) %>%
        hot_col("Sample", readOnly = FALSE, ) %>%
        htmlwidgets::onRender(
          "function(el){var hot = this.hot; setTimeout(function(){hot.render();}, 1000)}"
        )

    }
  })

  ## get values with input$hotout
  observeEvent(input$maxSampSizeCalc,{
    tabl<-frame_CV()

    shiny::validate(need(tabl, message = "Nothing selected"))
    if("sf" %in% class(tabl)) tabl<-tabl %>% st_set_geometry(NULL)
    ## recalculate Sample Size
    ## a. by grid cells
    sampTot<-input$maxSampSizeNum
    shiny::validate(need(sampTot, message = F))

    # maximum popsize only for grid
    if(input$sampType=="Random Grid") {
      if (input$maxSampSizePop=="No") {
        totCell<-sum(tabl[,3])
        ## allocate by grid cells
        tabl$Sample<-ceiling(tabl$Cells*((sampTot/totCell)))

      } else if (input$maxSampSizePop=="Yes") {
        # allocate by grid population
        totPop<-sum(tabl[,2])
        tabl$Sample<-ceiling(tabl$Population*((sampTot/totPop)))
      }
    } else if (input$sampType=="Random Cluster"){
      if (input$maxSampSizePop=="No") {
        # allocate by cluster
        totPop<-sum(tabl[,3])
        tabl$Sample<-ceiling(tabl$Cluster*((sampTot/totPop)))
      } else if (input$maxSampSizePop=="Yes") {
        totPop<-sum(tabl[,2])
        tabl$Sample<-ceiling(tabl$Population*((sampTot/totPop)))
      }

    }
    rhandsontable::set_data(id = "hotout",
                            row = 1:nrow(tabl),
                            col = 4,
                            session = session,
                            val = tabl$Sample)


  }, ignoreInit = T)

  # observe({
  #   print(hot_to_r(input$hotout))
  # })

  ########################################################################
  ## 4.1.2. Extract sample size                                          #
  ########################################################################
  observeEvent(input$conf3,{
    #############################
    ##  collect sample sizes
    ##  a) unequal
    tabl<-new_pop$popSizeStrat
    tabl_sampsize<-hot_to_r(input$hotout)
    if (input$strat=="Yes" & input$sampSizeStrat=="Yes") {

      shiny::validate(need(tabl, message = "Nothing selected"),
                      need(input$`strVarSel-strat_var`, message = "Nothing selected"))

      if("sf" %in% class(tabl)) tabl<-tabl %>% st_set_geometry(NULL)

      data.table::setorderv(tabl_sampsize, "Stratum")
      stratNames<-tabl_sampsize$Stratum
      stratNames<-iconv(stratNames, to='ASCII//TRANSLIT')
      sampSize<-setNames(tabl_sampsize$Sample, stratNames)
      ##  b) equal
    } else {
      sampSize<-input$sampSize
    }


    ## Process raster when categorical and PPS
    if (input$catRaster=="Yes" & input$sampPPS=="Yes" & input$sampType=="Random Cluster") {
      withProgress(message = "PPS variable generation in Progress", {
        domCat<-as.numeric(input$catRaster_domCat)
        pop_raster<-new_shp$pop_raster
        shiny::validate(need(domCat, message = F))
        harare_landuse3_type1<-tabl
        harare_landuse3_type1$ones<-1
        sp_grd_strat_poly<-harare_landuse3_type1
        sp_grd_strat_poly<-st_transform(sp_grd_strat_poly, (raster::proj4string(pop_raster)))
        #pop_raster<-crop(pop_raster, extent(sp_grd_strat_poly))

        ##  MULITCORE LOOP
        # registerDoFuture()
        # plan(multicore)

        ##  2. N simu and packages
        simu<-nrow(sp_grd_strat_poly)
        #pack_dp_sp<-c("sp", paste0(package_dependencies("sp")[[1]]))
        pack_dp_sp<-c("raster", "sp", "rgdal", "broom",
                      "data.table", "plyr", "dplyr", "shiny", "sf", "fasterize", "spex")

        incProgress(0.3)

        Pop<-foreach(i=1:simu, .packages = pack_dp_sp,
                     .combine=c,
                     .multicombine = T,
                     #.export = c("a"),
                     #.verbose = T,
                     .errorhandling="pass") %dopar% {
                       tmp.poly<-sp_grd_strat_poly[i,] %>% st_cast()
                       tmp.ras<-crop(pop_raster, extent(tmp.poly))
                       tmp.poly.ras<-fasterize(tmp.poly, tmp.ras, fun="max")
                       tmp.poly.ras[]<-tmp.poly.ras[]*tmp.ras[]
                       tmp.poly.ras<-raster::values(tmp.poly.ras)
                       pop<-sum(tmp.poly.ras==domCat, na.rm = T)/length(tmp.poly.ras)
                       return(pop)
                     }
        Pop<-ifelse(Pop==0|is.na(Pop), min(Pop[Pop>0]), Pop)

        incProgress(0.6)
        ########################
        ##  TO sampling, complete DATA table
        sp_grd_strat_poly$Pop<-Pop
        if (input$strat=="Yes") {
          shiny::validate(need(input$`strVarSel-strat_var`, message = T))
          sp_grd_strat_poly$stratum_numeric<-sp_grd_strat_poly[[input$`strVarSel-strat_var`]]
          #sp_grd_strat_poly$stratum_numeric<-as.numeric(sp_grd_strat_poly$stratum_numeric)
        }
        ras_points<-data.table(sp_grd_strat_poly %>% st_set_geometry(NULL))
        ras_points[,CID:=1:.N]
        popSize<-sum(ras_points$Pop)
        rm(Pop)
        setProgress(1)

      })
      #################H##################
      ##  To Sample
      new_shp$sp_grd_strat_points<-ras_points
    }

    stratSamp$nStrat<-sampSize
  })


  ########################################################################
  ## 4.2. SAMPLING                                                       #
  ########################################################################
  ## 4.2.1 Transform to points DF                                        #
  ########################################################################
  pop_raster_shpDF<-reactive({
    if (input$popCreate== "No") {
      if (isolate(input$sampType=="Random Grid")){
        pop_raster_shp<-new_shp$sp_grd_strat
      } else if (isolate(input$sampType=="Random Cluster")) {
        pop_raster_shp<-new_shp$harare_landuse3_type1
        ## when stratified create stratum_numeric
        if (input$strat=="Yes"){
          shiny::validate(need(input$`strVarSel-strat_var`, message=F))
          pop_raster_shp$stratum_numeric<-pop_raster_shp[[input$`strVarSel-strat_var`]]
        }

        ## when PPS & is column in frame
        if (input$sampPPS=="Yes") {
          shiny::validate(need(input$rcluster_MOS, message=F))
          pop_raster_shp$Pop<-pop_raster_shp[[input$rcluster_MOS]]
        }

      }
      shiny::validate(need(pop_raster_shp, message=F))
      pop_raster_shp$CID<-as.numeric(rownames(pop_raster_shp))
      new_shp$sp_grd_strat<-pop_raster_shp
      DF<-pop_raster_shp %>% st_set_geometry(NULL)%>% as.data.frame()
      DF<-data.table::data.table(DF)
      return(DF)
    } else if (isolate(input$popCreate == "Yes") & isolate(input$sampType=="Random Grid")) {
      pop_raster_shp<- new_shp$sp_grd_strat_points
      shiny::validate(need(pop_raster_shp, message=F))
      return(pop_raster_shp)
    } else if (isolate(input$popCreate == "Yes") & isolate(input$sampType=="Random Cluster")) {
      pop_raster_shp<-new_shp$sp_grd_strat_points
      shiny::validate(need(pop_raster_shp, message=F))
      return(pop_raster_shp)
    }
  })

  ########################################################################
  ## 4.2.2 Sample                                                        #
  ########################################################################

  observeEvent(input$conf3, {
    pop_raster_shp<-pop_raster_shpDF()
    shiny::validate(need(pop_raster_shp, message=F),
                    need(stratSamp$nStrat, message = F))
    sampSize<-stratSamp$nStrat
    ###     SET SEED FOR SAMPLE
    set.seed(input$sampSEED)
    sample_seed(input$sampSEED)
    ########################################################################
    ## NO strat, NO pps                                                    #
    ########################################################################
    if (input$sampPPS=="No" & input$strat=="No") {

      removeModal()
      ##  For minimum POP, Pop must be provided

      pop_raster_shp$stratum_numeric<-1
      st<-sampling::strata(pop_raster_shp,
                           stratanames = c("stratum_numeric"),
                           method = c("systematic"),
                           size = sampSize,
                           pik=rep(1, nrow(pop_raster_shp)))
      pop_raster_shp_samp<-data.table(getdata(pop_raster_shp, st))

      ## calculate weights
      pop_raster_shp_samp[,weight:=((1/Prob))]
      ## export to resource creation
      sample_square$grd_df_samp_points<-pop_raster_shp_samp

    } else if (input$sampPPS == "No" & input$strat == "Yes") {
      ########################################################################
      ## STRAT, NO pps                                                       #
      ########################################################################
      #sampSize<-scan(text = input$sampSize, sep = ",")
      ## Sample size can be provided in 2 ways:
      ##      - as a single integer-->equal allocation
      ##      - as a vector with samelength as unique strata
      if (length(sampSize)!=length(unique(pop_raster_shp$stratum_numeric))){
        sampSize<-rep(sampSize, length(unique(pop_raster_shp$stratum_numeric)))
      }
      shiny::validate(need(input$`strVarSel-strat_var`, message = F))
      # shiny::validate(need(is.numeric(sampSize), message = T),
      #                 need((length(sampSize)==length(unique(pop_raster_shp$stratum_numeric)))|
      #                        length(sampSize)==1,
      #                      message = F))
      removeModal()

      setorderv(pop_raster_shp, "stratum_numeric")

      pop_size<-pop_raster_shp[,.(popsize=as.numeric(.N)), by=.(stratum_numeric)]
      pop_size[,sampSizeP:=sampSize]
      pop_size[,sampSize:=min(sampSizeP, popsize), by=1:nrow(pop_size)]


      st<-sampling::strata(pop_raster_shp,
                           stratanames = c("stratum_numeric"),
                           method = c("systematic"),
                           size = pop_size$sampSize,
                           pik=rep(1, nrow(pop_raster_shp)))
      pop_raster_shp_samp<-data.table(getdata(pop_raster_shp, st))

      ## calculate weights
      pop_raster_shp_samp[,weight:=((1/Prob))]
      ## export to resource creation
      sample_square$grd_df_samp_points<-pop_raster_shp_samp


    } else if (input$sampPPS == "Yes" & input$strat == "No") {
      ########################################################################
      ## NO strat, PPS                                                       #
      ########################################################################
      removeModal()

      shiny::validate(need(input$sampSize, message = F),
                      need(is.numeric(sampSize), message = T))
      ##  For minimum POP, Pop must be provided


      pop_raster_shp$stratum_numeric<-1
      st<-sampling::strata(pop_raster_shp,
                           stratanames = c("stratum_numeric"),
                           method = c("systematic"),
                           size = sampSize,
                           pik = pop_raster_shp$Pop)
      pop_raster_shp_samp<-data.table(getdata(pop_raster_shp, st))

      ## calculate weights
      pop_raster_shp_samp[,weight:=((1/Prob))]
      ## export to resource creation
      sample_square$grd_df_samp_points<-pop_raster_shp_samp



    } else if (input$sampPPS == "Yes" & input$strat == "Yes"){
      ########################################################################
      ## STRAT, PPS                                                          #
      ########################################################################
      if (length(sampSize)!=length(unique(pop_raster_shp$stratum_numeric))){
        sampSize<-rep(sampSize, length(unique(pop_raster_shp$stratum_numeric)))
      }
      shiny::validate(need(input$`strVarSel-strat_var`, message = F))

      shiny::validate(need(is.numeric(sampSize), message = T),
                      need((length(sampSize)==length(unique(pop_raster_shp$stratum_numeric)))|
                             length(sampSize)==1,
                           message = F))
      removeModal()
      ## i Poly data
      ##  SET 0 VALUES to small positive!!!!

      pop_raster_shp[,Pop:=ifelse(Pop==0 | is.na(Pop), 0.0001, Pop)]
      pop_size<-pop_raster_shp[,.(popsize=.N), by=.(stratum_numeric)]
      pop_size[,sampSizeP:=sampSize]
      pop_size[,sampSize:=min(sampSizeP, popsize), by=.(stratum_numeric)]

      st<-sampling::strata(pop_raster_shp,
                           stratanames = c("stratum_numeric"),
                           method = c("systematic"),
                           size = pop_size$sampSize,
                           pik = pop_raster_shp$Pop)
      pop_raster_shp_samp<-data.table(getdata(pop_raster_shp, st))


      ##########################
      ## MINIMUM CELL POPULATION
      if (!is.null(input$minCell) && input$minCell>0) {

        ## remove minimum
        pop_raster_shp_samp[, n_samp:=.N, by=.(stratum_numeric)]
        pop_raster_shp_samp<-pop_raster_shp_samp[Pop>=input$minCell]
        pop_raster_shp_samp[, n_elig:=.N, by=.(stratum_numeric)]
        pop_raster_shp_samp[,weight1:=n_samp/n_elig]

        ## combine weight phase 1 and phase 2
        pop_raster_shp_samp[,weight:=((1/Prob)*weight1)]
      } else {
        ## calculate weights
        pop_raster_shp_samp[,weight:=((1/Prob))]

        ## add columns n_samp and n_elig for consistency
        pop_raster_shp_samp[, n_samp:=.N, by=.(stratum_numeric)]
        pop_raster_shp_samp[, n_elig:=.N, by=.(stratum_numeric)]
      }

      sample_square$grd_df_samp_points<-pop_raster_shp_samp

    }
  })

  ########################################################################
  ## 4.2.3 Extract sample                                                #
  ########################################################################
  observe({
    sampDF<-sample_square$grd_df_samp_points

    if (input$sampType=="Random Cluster" & input$popCreate== "Yes") {
      sp_grd_strat<-new_shp$sp_grd_strat_poly
    } else if (input$sampType=="Random Cluster" & input$popCreate== "No"){
      sp_grd_strat<-new_shp$sp_grd_strat
    } else if (input$sampType=="Random Grid" & input$popCreate== "No"){
      sp_grd_strat<-new_shp$sp_grd_strat
    } else {
      sp_grd_strat<-new_shp$sp_grd_strat_poly
    }
    shiny::validate(need(sampDF, message = F))
    shiny::validate(need(sp_grd_strat, message = F))


    ##################################
    ## 1. CHECK for 0 observations in sample-->ONLY GRID!!
    if(input$sampType=="Random Grid" && input$popCreate== "Yes" && nrow(sampDF)==0) {
      ## --> in case when sample results in 0 observations, show notification and stop
      shiny::showNotification(type = "warning", duration = NULL, ui = "Your sampling design resulted in a sample of 0 units.
                              If you have sepcified a minimum number of population units in your grid cell, try to lower
                              this treshold.")
      ## --> stop until new data is loaded
      req(FALSE)
    } else if(input$sampType=="Random Grid" && input$popCreate== "Yes" &&
              dplyr::n_distinct(sp_grd_strat$stratum_numeric)!=dplyr::n_distinct(sampDF$stratum_numeric)){
      ##################################
      ## 2. CHECK for 0 observations in ANY stratum
      ## --> in case when sample results in 0 observations, show notification and stop
      shiny::showNotification(type = "warning", duration = NULL,
                              ui = "Your sampling design resulted in a sample of 0 units
      for one or some strata. If you have sepcified a minimum number of population units in your grid cell,
      try to lower this treshold.")
      ## --> stop until new data is loaded
      req(FALSE)
    }
    ##################################
    ## EXTRACT SAMPLE
    withProgress(message = "Processing", {
      if ((isolate(input$popCreate) == "Yes" & isolate(input$sampType)=="Random Cluster")) {
        pop_raster_shp_samp<-sp_grd_strat[sampDF$CID, ]
      } else {
        if (isolate(input$popCreate)== "No") {
          #pop_raster_shp<-new_shp$sp_grd_strat
          pop_raster_shp_samp<-sp_grd_strat[sampDF$ID_unit,]
        } else if (isolate(input$popCreate) == "Yes" & isolate(input$strat)=="No") {
          pop_raster<-new_shp$pop_raster
          shiny::validate(need(pop_raster, message = F))
          tmp.samp<-copy(sampDF)
          tmp.samp.ras<-raster(nrows=nrow(pop_raster), ncols=ncol(pop_raster) ,crs=crs(pop_raster))
          extent(tmp.samp.ras)<-extent(pop_raster)
          tmp.samp.ras[]<-NA
          tmp.samp.ras[tmp.samp$CID]<-pop_raster[tmp.samp$CID]
          pop_raster_shp_samp<-qm_rasterToPolygons(tmp.samp.ras)
          #names(pop_raster_shp_samp)<-c("Pop", "geometry")
          pop_raster_shp_samp<-cbind(pop_raster_shp_samp, tmp.samp)
        } else if (isolate(input$popCreate) == "Yes" & isolate(input$strat)=="Yes") {
          ## popCreat is YES, strat is YES
          pop_raster<-new_shp$pop_raster
          shiny::validate(need(pop_raster, message = F))
          #######################################
          ## 3. MC for extraction ## MC MC MC not #future!!
          #future::plan(sequential) ## turn off #future?
          ## 3.1 parallel setup
          cores<-data.table::getDTthreads()
          cl<-parallel::makeCluster(cores)
          doParallel::registerDoParallel(cl)
          ##  2. N simu and packages
          simu<-nrow(sp_grd_strat)
          stratNum<-unique(sampDF$stratum_numeric)
          #pack_dp_sp<-c("sp", paste0(package_dependencies("sp")[[1]]))
          # deps<-unique(c(paste0(package_dependencies("sf")[[1]]),
          #                paste0(package_dependencies("raster")[[1]]),
          #                paste0(package_dependencies("spex")[[1]])),
          #              paste0(package_dependencies("data.table")[[1]]))
          pack_and_deps<-unique(c("sf", "fasterize", "spex", "data.table", "raster", "sp"))

          incProgress(0.3)
          final<-foreach(i=1:simu,
                         .packages = pack_and_deps,
                         #.export = c("sp_grd_strat", "sampDF", "pop_raster"),
                         #.combine=list,
                         .multicombine = F,
                         .errorhandling="pass") %dopar% {
                           tmp.poly<-sp_grd_strat[i,]
                           if (st_is_empty(tmp.poly)) {
                             return(NULL)
                           } else {
                             tmp.poly<-as(tmp.poly, "Spatial")

                             ## i. subset sample to stratum
                             tmp.samp<-sampDF[stratum_numeric%in%tmp.poly$stratum_numeric]
                             ## ii. crop raster
                             tmp.ras<-raster::crop(pop_raster, extent(tmp.poly))
                             tmp.samp.ras<-raster(nrows=nrow(tmp.ras), ncols=ncol(tmp.ras) ,crs=crs(tmp.ras))
                             extent(tmp.samp.ras)<-extent(tmp.ras)
                             tmp.samp.ras[]<-NA
                             tmp.samp.ras[tmp.samp$CID]<-tmp.ras[tmp.samp$CID]
                             return(tmp.samp.ras)
                           }

                         }
          parallel::stopCluster(cl)

          if (class(final)[1]=="list" & length(final)>1) {
            final1<-list()
            incInc<-0.4/length(final)
            for (i in 1:length(final)) {
              tmp.samp.ras<-final[[i]]
              ## CHECK for small raster
              if (ncell(tmp.samp.ras)==0) next()
              if (sum(!is.na(tmp.samp.ras[]))<4) {
                tmp.samp.ras.poly<-rasterToPolygons(tmp.samp.ras)
                ## Next if NO SAMPLE
                if(is.null(tmp.samp.ras.poly)) next()
                tmp.samp.ras.poly<-st_as_sf(tmp.samp.ras.poly)
                tmp.samp.ras.poly$CID<-rownames(tmp.samp.ras.poly)
                ## ADD other Frame data
                tmpStrat<-stratNum[i]
                tmp.samp<-sampDF[stratum_numeric==tmpStrat]
                tmp.samp.ras.poly<-merge(tmp.samp.ras.poly,
                                         tmp.samp[,.(CID, Pop, GRIDID, TOTPOP, stratum_numeric, Stratum, n_samp, n_elig)],
                                         by=c("CID"))

              } else {
                tmp.samp.ras.poly<-qm_rasterToPolygons(tmp.samp.ras, na.rm = T)
                tmp.samp.ras.poly$CID<-rownames(tmp.samp.ras.poly)
                #### check warning with old style crs!!

                ## ADD other Frame data
                tmpStrat<-stratNum[i]
                tmp.samp<-sampDF[stratum_numeric==tmpStrat]
                tmp.samp.ras.poly<-merge(tmp.samp.ras.poly,
                                         tmp.samp[,.(CID, Pop, GRIDID, TOTPOP, stratum_numeric, Stratum, n_samp, n_elig)],
                                         by=c("CID"))

              }
              if (nrow(tmp.samp.ras.poly)>0) tmp.samp.ras.poly$stratum_numeric<-i
              final1[[i]]<-tmp.samp.ras.poly
              #incProgress(incInc)
            }
          } else {
            final1<-qm_rasterToPolygons(final)
            final1$stratum_numeric<-1
          }
          #incProgress(0.3)
          if(class(final1)[1]=="list" & length(final1)>1) {
            final1<-final1[vapply(final1, function(x) ifelse(length(x)>0,nrow(x)>0, FALSE), logical(1))]
            pop_raster_shp_samp<-do.call(rbind, final1) #; names(pop_raster_shp_samp)<-c("Pop", "stratum_numeric", "geometry")
          } else {
            pop_raster_shp_samp<-final1 #; names(pop_raster_shp_samp)<-c("Pop", "geometry", "stratum_numeric")
          }
        }
      }
      ## 2.5. Sample Info Table
      samp.info<-list()
      samp.info$class<-class(pop_raster_shp_samp)[1]
      samp.info$p4s<-st_crs(pop_raster_shp_samp)$proj4string
      samp.info$totPop<-ifelse(input$popCreate=="Yes",ceiling(sum(pop_raster_shp_samp$Pop, na.rm = T)), NA)
      samp.info$nCell<-nrow(pop_raster_shp_samp)
      samp.info$avPop<-ifelse(input$popCreate=="Yes", round(mean(pop_raster_shp_samp$Pop, na.rm = T), digits = 2), NA)
      samp.info$extent<-round(st_bbox(pop_raster_shp_samp), 6)
      sample_square$samp.info<-samp.info

      ## 2.6. Returns
      sample_square$samp_raster_shp<-pop_raster_shp_samp

      incProgress(0.3)
    })

    ## 2.7 update selected panel
    updateTabsetPanel(session, "inTabset",
                      selected = paste0("panel", input$controller)
    )
  })


  ########################################################################
  ## 4.3 Map the sample                                                  #
  ########################################################################
  ##  4.3.1. Initial Map                                                 #
  ########################################################################
  ##  i. Switch Tabs
  observeEvent(input$genSamp, {
    #updateTabItems(session, "maps",selected = "sample")
    updateTabItems(session, "tables",selected = "sample")
  })
  ##  ii. Reactive Data intake
  samp_raster_shp<-reactive({
    DF<-sample_square$samp_raster_shp
    shiny::validate(need(DF, message = F))
    DF<-DF %>% st_transform(4326)
    if(isolate(input$popCreate)=="Yes") DF$Pop<-round(DF$Pop)
    if (input$sampType=="Random Cluster"& input$popCreate=="No" & input$strat=="Yes") {
      shiny::validate(need(input$`strVarSel-strat_var`, message = F))
      DF$stratum_numeric<-DF[[input$`strVarSel-strat_var`]]
    }
    return(DF)
  })

  ###################################################################
  ## UPDATING THE BASE MAP with SAMPLE
  ## ----> go to map section row 600 ff


  ##  2.2. TABLE for sample
  sample_square.samp.info<-reactiveVal( NULL)
  output$sample_table<-DT::renderDataTable({
    shp.info<-sample_square$samp.info
    shiny::validate(need(shp.info, message = F))
    ex<-sprintf("%s Lon, %s Lat, %s Lon, %s Lat",shp.info$extent[1], shp.info$extent[2], shp.info$extent[3], shp.info$extent[4])
    tabl<-data.table(character(6), character(6))
    tabl[,V1:=c("File Type", "Projection", "Est. Sample Population", "Number of non-empty Cells", "Average Population per cell","Extent")]
    tabl[,V2:=c(shp.info$class,
                shp.info$p4s,
                as.character(shp.info$totPop),
                as.character(shp.info$nCell),
                as.character(shp.info$avPop),
                paste(ex))]

    ## enable report dwl
    shinyjs::show("div_samplingreport")

    ## tabl for report
    sample_square.samp.info( as.data.frame(tabl))
    tabl<-DT::datatable(tabl, smTab, selection = "single", rownames = F,
                        colnames = c("",""),
                        style = "bootstrap") %>% infoTable
    return(tabl)
  })


  ####################################  DOWNLOAD OF MAPS####################################
  ##                                        T I L E S                                     ##
  ##########################################################################################
  TPKpath <- modal_createbasemap_server("mapfordwl", fpp = fpp(), fppTPK = fppTPK(), fppTPKerror = fppTPKerror(),
                                        sample_seed = sample_seed,
                                        shape_boundaries = shapefileinput,
                                        sampType = reactive({input$sampType}))
  ##  2.2. Actual DWL
  mapNameForDownload<-reactive({
    SYT<-stringr::str_remove_all(Sys.time(), "([:punct:])|([:space:])")
    SEE<-sample_seed()
    paste0(paste("BaseMapFiles", SYT,"seed" ,SEE, sep="_"), ".zip")
  })
  zipFileDwl_server("basemap",
                    file_name = mapNameForDownload,
                    path_to_zip = TPKpath)


  ####################################
  ##  S H A P E S DOWNLOAD
  ## - Module creates zip file
  ## - send to api or download
  shapefileinput<-reactive({
    sample_square$samp_raster_shp
  })
  zipfilepath <- modal_createshape_server("shapesfordwl", sample_seed = sample_seed,
                                          shape_boundaries = shapefileinput,
                                          sampType = reactive({input$sampType}))

  shapeNameForDownload<-reactive({
    SYT<-stringr::str_remove_all(Sys.time(), "([:punct:])|([:space:])")
    SEE<-sample_seed()
    paste0(paste("Shapefiles", SYT,"seed" ,SEE, sep="_"), ".zip")
  })
  zipFileDwl_server("shapeboundaries",
                    file_name = shapeNameForDownload,
                    path_to_zip = zipfilepath)

  ####################################
  ##  TABLE FRAME (CSV)
  ####################################
  # 1. build data
  frameDataForDownload<-reactive({
    #fram_data<-NULL
    ##################
    if (isolate(input$popCreate == "Yes") & isolate(input$sampType=="Random Grid")) {
      fram_data<-new_shp$sp_grd_strat_points
      fram_data$Pop<-ifelse(fram_data$Pop<1, 0, fram_data$Pop)
    } else if (isolate(input$popCreate == "No") & isolate(input$sampType=="Random Grid")) {
      fram_data<-new_shp$sp_grd_strat %>% st_set_geometry(NULL)
    } else if (isolate(input$sampType=="Random Cluster" & input$popCreate== "Yes")){
      fram_data<-new_shp$sp_grd_strat_poly %>% st_set_geometry(NULL)
    } else if (isolate(input$sampType=="Random Cluster" & input$popCreate== "No")) {
      fram_data<-new_shp$sp_grd_strat %>% st_set_geometry(NULL)
    }
    return(fram_data)
  })
  frameNameForDownload<-reactive({
    SEE<-sample_seed()
    if(is.null(SEE)){
      paste("SpatSamp_Frame",
            stringr::str_remove_all(Sys.time(), "([:punct:])|([:space:])"),
            sep="_")
    } else {
      paste("SpatSamp_Frame",
            stringr::str_remove_all(Sys.time(), "([:punct:])|([:space:])"),"seed" , SEE,
            sep="_")
    }
  })
  # 2. Download data module
  download_csv_server("frame_download",
                      file_name = frameNameForDownload,
                      content = frameDataForDownload)

  ####################################
  ##  TABLE SAMPLE (CSV)
  ####################################
  # 1. build data
  sampleDataForDownload<-reactive({
    ##################
    samp_data<-sample_square$grd_df_samp_points
    temp.dir<-tempdir()
    shiny::validate(need(samp_data, message = F))

    ######################################

    ## get coordinates for SuSo
    ## 1. center
    samp_raster_shp<-sample_square$samp_raster_shp
    # transform CID to numeric if character
    if(is.character(samp_raster_shp$CID)) samp_raster_shp$CID<-as.numeric(samp_raster_shp$CID)

    shiny::validate(need(samp_raster_shp, message = F))

    ## rename stratum numeric (only if stratified a grid/cluster)
    if (!(input$strat=="No" & input$sampType=="Random Cluster")) data.table::setnames(samp_raster_shp, "stratum_numeric", "stratum_ID")

    if(!sf::st_is_longlat(samp_raster_shp))  samp_raster_shp<-st_transform(samp_raster_shp, 4326)
    ## CENTROID
    centr_function<-function(x) {
      st_centroid(x) %>%
        st_coordinates()
    }
    ## Bind to data
    samp_raster_shp = cbind(st_sf(geometry=samp_raster_shp),
                            do.call(rbind,lapply(st_geometry(samp_raster_shp), centr_function)))
    data.table::setnames(samp_raster_shp, c("X", "Y"), c("gps_LONG", "gps_LAT"))

    ## RECTANGLE (get bounding box for Suso, add buffer)
    bblist<-list()
    for(i in seq_along(st_geometry(samp_raster_shp))){
      tmps<-samp_raster_shp[i,]

      tmps<-st_transform(tmps, 3857)
      bblist[[i]]<-tmps %>%
        st_buffer(getOption("bufferForSuSoBounds")) %>%
        st_transform(4326) %>%
        st_bbox() %>%
        as.numeric()
    }

    bblist<-as.data.frame(do.call(rbind, bblist))
    names(bblist)<-names(st_bbox(samp_raster_shp[1,]))
    samp_raster_shp<-cbind(st_sf(geometry=samp_raster_shp),bblist)

    # set names for SuSo bounding box
    data.table::setnames(samp_raster_shp, c("ymax", "xmin", "ymin", "xmax"), c("north", "west", "south", "east"))
    samp_raster_shp<-as.data.table(samp_raster_shp %>% st_set_geometry(NULL))

    # merge weights
    samp_data<-merge(samp_raster_shp, samp_data[,.(CID, Prob, weight)], by = "CID")
    return(samp_data)
  })
  sampleNameForDownload<-reactive({
    SEE<-sample_seed()
    paste("SpatSamp_Sample",
          stringr::str_remove_all(Sys.time(), "([:punct:])|([:space:])"),"seed" , SEE,
          sep="_")
  })
  # 2. Download data module
  download_csv_server("sample_download",
                      file_name = sampleNameForDownload,
                      content = sampleDataForDownload)

  ##################################
  #  SAMPLING REPORT DOWNLOAD (DOCX)
  ##################################
  ## 1. Create Inputs
  report_content<-reactive({
    ############################
    ## 1.1. Main Info
    check1<-shp.info.tabl()
    names(check1)<-c("var", "value")
    check2<-new_shp.tmp.Area()
    if(!is.null(check2)) names(check2)<-c("var", "value")
    check3<-new_pop.pop.info()
    check3$var<-rownames(check3); check3<-check3[,c(2,1)]
    check4<-sample_square.samp.info()

    ## 1.2. Main Graph
    check5<-harare_landuse3_type1()
    check6<-new_pop$pop_raster
    check7<-new_shp$sp_grd_strat
    check8<-samp_raster_shp()
    check9<-check8 %>% st_set_geometry(NULL)
    check9<-data.table(check9)
    ## only continue when sample has arrived
    shiny::validate(need(check8, message = F))
    withProgress(message = 'Report generation in progress',
                 value = 0,
                 {
                   ###############################
                   ## 2. Transformations
                   ## 2.1. Overview Table
                   sampType<-input$sampType
                   strat<-input$strat
                   popCreate<-input$popCreate
                   popUpType<-input$popUpType
                   seed<-sample_seed()
                   basic_char<-as.data.frame(cbind(c("Sampling Type", "Stratification", "Population", "Population Type" ,"Seed"),
                                                   c(sampType, strat, popCreate, popUpType, seed)))

                   names(basic_char)<-c("var", "value")
                   ## 2.2. Sample Population Summary
                   toSelect<-which(sapply(check9,is.numeric))
                   toSelect<-toSelect[names(toSelect)!="CID"]
                   statstab<-c("mean", "sd", "min", "max")

                   statsfun<-function(x, digits = 2){
                     check9st<-check9[, c(lapply(.SD, eval(parse(text = x)), na.rm = T)), .SDcols=toSelect]
                     check9<-round(check9st, digits = digits)
                   }
                   samp_char<-as.data.frame(sapply(statstab, FUN = statsfun))
                   samp_char$var<-rownames(samp_char)
                   samp_char<-samp_char[,c(5,1:4)]

                   incProgress(0.2)

                   ## 2.3. Creation of map elements

                   ## i. Base map
                   if(!is.null(check5)) {
                     if(input$strat=="Yes"){
                       strat_name<-input$`strVarSel-strat_var`
                     } else {
                       strat_name<-NULL
                     }
                     #check5<-ms_simpl_complexShape(check5, level = 500000, keep = 0.05)
                     gr_para1_1<-shpMapOSM(check5, strat_name)
                   } else {
                     gr_para1_1<-NULL
                   }
                   ## ii. Stratum id for rest stratum numeric
                   if(input$strat=="Yes"){
                     strat_name<-"stratum_numeric"
                   } else {
                     strat_name<-NULL
                   }
                   incProgress(0.2)
                   ## iii. Raster
                   if(!is.null(check6)) {
                     gr_para2_1<-rasMapOSM(check6, "MoS")
                   } else {
                     gr_para2_1<-NULL
                   }
                   incProgress(0.2)
                   ## Subset
                   if(!is.null(check7)) {
                     gr_para2_2<-shpMapOSM(check7, strat_name)
                   } else {
                     gr_para2_2<-NULL
                   }
                   gr_para3_1<-shpMapOSM(check8, strat_name)
                   incProgress(0.2)
                   full_content<-list()
                   full_content$doc_title<-"Sampling Report"
                   full_content$sec_title<-list(
                     sec1 = "Boundary File",
                     sec2 = "Population File & Sub-set/Modification",
                     sec3 = "Sample Properties")
                   full_content$sec_para<-list(
                     sec1 = list(para1 = paste0("The following Boundaries have been used for the current sample. Within these boundaris,",
                                                toupper(input$sampType), " sampling has been applied.")),
                     sec2 = list(para1 = "The following population information has been provided for the current sample.",
                                 para2 = "The following subset of regions was used for the sample."),
                     sec3 = list(para1 = "The final design parameters are as follows:",
                                 para2 = "Together with the above mentioned inputs, the following sample has been created.",
                                 para3 = "The sampled areas had the following characteristics:")
                   )
                   full_content$sec_table<-list(
                     sec1 = list(para1 = check1),
                     sec2 = list(para1 = check2,
                                 para2 = check3),
                     sec3 = list(para1 = basic_char,
                                 para2 = check4,
                                 para3 = samp_char)
                   )

                   full_content$sec_graph<-list(
                     sec1 = list(para1 = gr_para1_1),
                     sec2 = list(para1 = gr_para2_1,
                                 para2 = gr_para2_2
                     ),
                     sec3 = list(para1 = NULL,
                                 para2 = gr_para3_1,
                                 para3 = NULL)
                   )
                 })
    return(full_content)
  })

  ## 2. Call Module
  dwl_reportSRV("samplingReport",
                fn = "SamplingReport",
                wordstyles = file.path(system.file("rmdfiles", package = "susoquestionnairemanual"), "FINAL_report_for_download.docx"),
                content = report_content,
                type = "word")


  #########################
  # SECOND STAGE SAMPLE
  gridPointsSpatial2stage <- modal_spsample2stage_server("my_modal", shape_boundaries = reactive({ NULL }))

  observe({
    # Display on main map
    if(input$Settings=="Sample 2nd Stage") {
      req(gridPointsSpatial2stage())
      if(getOption("mapwidget.option")=="mapdeck"){
        mapModuleSvr(
          id = "baseMap",
          updateMapPts=gridPointsSpatial2stage,
          fillcolor = "red",
          layeridpts = "2ndstagesample"
        )
      } else if(getOption("mapwidget.option")=="leaflet"){
        print("NOT YET")
        # mapServer("baseMap_leaf",
        #           updateMap = reactive({shp}),
        #           updateGroup = reactive({"Pop"}))
      }

    }
  })

  #########################
  # Survey Solutions Assignment
  susomapadmin <- mapadminSRV("susomapassign", boundaryfile = zipfilepath, mapfile = TPKpath)

  ## Enablement for admin right panel
  observeEvent(input$Settings, {
    if(input$Settings=="Admin") {
      shinyjs::show("adminrightpan")
    } else if(input$Settings!="Admin") {
      shinyjs::hide("adminrightpan")
    }
  })

  ###########################FIN#####################################FIN######################################################FIN############################################################

}


