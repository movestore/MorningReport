library('move2')
library('shiny')
library('shinyWidgets')
library('lutz')
library('geosphere')
library('DT')
library('dplyr')
library('leaflet')
library('sf')
library("shinyTime")
library("lubridate")
library("shinyBS")
library("shinycssloaders")


shinyModuleUserInterface <- function(id, label){
  ns <- NS(id)
  navbarPage("Morning Report",
             tabPanel("Settings", 
                      fluidRow(
                        h5("1. Define your reference date and time, i.e. the date and time from which you want to look back at your data. If your tags are still functioning, this is typically ‘NOW’ in UTC. If you don’t change the date and time, ‘NOW’ will be used by default."),
                        h5("Check the box 'Always use 'NOW'' to use the 'NOW' date and time of each run of a scheduled workflow, date and time will be updated at each run. Uncheck the box if you want to use a fixed date and time."),
                        verbatimTextOutput(ns("ts_lastPos")),
                        column(3,dateInput(ns("date"),"Date:", value = as_date(now("UTC")))),
                        column(2,shinyTime::timeInput(ns("time"), "Time (24h format):",value= now("UTC"), seconds = TRUE)), 
                        column(2, checkboxInput(ns("alwaysNOW"), "Always use date and time 'NOW'", value=TRUE),style = 'top: 25px;position:relative;',
                        bsTooltip(id=ns("alwaysNOW"), title="Checked: When this app is part of a scheduled workflow, each time the workflow runs, it will use the date and time of moment when the app is executed. Unchecked: the selected date and time will be fixed.", placement = "bottom", trigger = "hover", options = list(container = "body")))
                      ),
                      h5("2. Define the radius (in km) an animal must move past to qualify as migration behaviour. Default 100 km"),
                      numericInput(ns("mig7d_dist"),"Migration buffer in km (last 7 days)", min=0, value=100),
                      h5("3. Define the radius (in m) within which an animal must remain to indicate a likely mortality. Default 100 m"),
                      numericInput(ns("dead7d_dist"),"Mortality buffer in m (last 7 days)", min=0, value=100),
                      
                      h5("4. Select your reference position by clicking on the map, i.e. if you are in the field use your current location to find out if any tagged animals are in the surroundings. The distance to this location will be calculated for all of your data points. If no location is selected, the last position of each track is used."),
                      withSpinner(leafletOutput(ns("testmap"))),
                      fluidRow(
                        column(1,uiOutput(ns("posLat_UI"))),
                        column(1,uiOutput(ns("posLon_UI"))),
                        column(1,actionButton(ns("set2Na"),"Reset to last position of track"),style = 'top: 25px;position:relative;')
                      )
             ),
             
             tabPanel("Visualization",
                      div(id=ns("C"),class='shiny-input-radiogroup',DT::dataTableOutput(ns("foo"))),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = ns("sort_table"),
                                      label = "Select property by which to sort the overview table",
                                      choices = c("animal name" = "ids", "tag ID" = "tags", "timestamp of first deployed location" = "time0", "timestamp of last deployed location"= "timeE", "number of locations during last 24 hours" = "posis24h", "number of locations during last 7 days" = "posis7d", "displacement during last 24 hours (km)" = "displ24h", "displacement during last 7 days (km)" = "displ7d")), #, "local timestamp of last deployed location" = "timeE_local"
                          radioButtons(inputId = ns("sort_direc"),
                                       label = "Choose how to sort the table",
                                       choices = c("ascending","descending")),
                          uiOutput(ns("start_timeUI")),
                          
                          pickerInput(inputId =ns("attr"),
                                      label = "Select which data attribute to plot",
                                      choices = c("net square displacement (km2)"="nsd")),
                          pickerInput(inputId =ns("attr2"),
                                      label = "Select which second data attribute to plot",
                                      choices = c("net square displacement (km2)"="nsd")),
                          selectInput(inputId =ns("prop"),
                                      label = "Select which daily property to plot",
                                      choices = c("number of positions per day" = "n_day", "displacement per day (km)" = "displ_day", "average daily distance to selected position (km)" = "avgdaily_dist2posi")),
                          width=2),
                        mainPanel(
                          fluidRow(
                            column(width=6,
                                   plotOutput(ns("attr_time"),height="220px"),
                                   plotOutput(ns("attr2_time"),height="220px"),
                                   plotOutput(ns("prop_time"),height="220px")
                            ),
                            column(width=6, withSpinner(leafletOutput(ns("leafmap"),height="500px")))
                          )
                        )
                      )
             )
  )
}



shinyModule <- function(input, output, session, data){
  ns <- session$ns
  ## distances, time zones and leaflet need lon/lat (EPSG:4326). Projected data is transformed here, the input data is returned unchanged
  if (is.na(st_crs(data))) stop("The input data has no coordinate reference system (CRS). This App needs to know the projection of the data to calculate distances and draw the maps. Please make sure the data have a valid CRS.")
  data_ll <- if (st_crs(data)==st_crs(4326)) data else st_transform(data, crs=4326)

  ## timestamps as text always with time (as.character() drops the time at midnight). In UTC, so they also sort correctly as text
  fmtTime <- function(x) format(x, format="%Y-%m-%d %H:%M:%S", tz="UTC")

  ## zoom the map to the bounding box, or to a fixed zoom level if all positions are at the same place (fitBounds() would zoom in completely)
  setMapView <- function(map, bounds){
    if (bounds[1]==bounds[3] && bounds[2]==bounds[4]){
      setView(map, lng=bounds[1], lat=bounds[2], zoom=12)
    } else {
      fitBounds(map, lng1=bounds[1], lat1=bounds[2], lng2=bounds[3], lat2=bounds[4])
    }
  }

  output$ts_lastPos <- renderText({paste0("Reference: last timestamp in data set is ", fmtTime(max(mt_time(data_ll))), " UTC")})

  ## map to choose coords from
  output$testmap <- renderLeaflet({
    lasLocs <- data_ll%>%group_by(mt_track_id())%>%dplyr::filter(row_number()==n())

    bounds <- as.vector(st_bbox(lasLocs))
    outl <- leaflet() %>%
      setMapView(bounds=bounds) %>%
      addTiles(group = "StreetMap") %>%
      addProviderTiles("Esri.WorldTopoMap",group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addCircleMarkers(data = lasLocs, fillOpacity = 0.7, opacity = 0.7, color="red", radius=5,label=unique(mt_track_id(lasLocs)), labelOptions = labelOptions(noHide = T)) %>%
      addLayersControl(baseGroups = c("StreetMap", "TopoMap", "Aerial")) %>%
      addLegend(position= "topright", colors=c("red","blue"), 
                labels=c("last position","selected location") ,opacity = 0.7)  
    outl
  })
  click <- reactiveVal()
  observeEvent(input$testmap_click, {
    click(input$testmap_click)
    leafletProxy('testmap')%>% removeMarker("mypos")%>%addMarkers(lng = click()$lng, lat = click()$lat, layerId="mypos",label=paste0("Lat: ", round(click()$lat,4)," Lon: ",round(click()$lng,4)),labelOptions = labelOptions(noHide = T))
  })
  ##
  ## reference time NOW: the moment the App is run, fixed during the session
  startTime <- now(tzone="UTC")
  ## when "always use NOW" is checked (e.g. in a scheduled workflow), the date and time fields show the moment the App is run
  observeEvent(input$alwaysNOW, {
    if (isTRUE(input$alwaysNOW)){
      updateDateInput(session, inputId="date", value=as_date(startTime))
      shinyTime::updateTimeInput(session, inputId="time", value=startTime)
    }
  })
  ## reference time in UTC. format() keeps the time as typed in the field (strftime() would convert it to the time zone of the server)
  time_now <- reactive({
    if (isTRUE(input$alwaysNOW)) return(startTime)
    req(input$date, input$time) # date or time field empty
    as.POSIXct(paste(format(input$date, format="%Y-%m-%d"), format(input$time, format="%H:%M:%S")), tz="UTC")
  })

  output$start_timeUI <- renderUI({
    req(time_now())
    sliderInput(ns("start_time"),
                paste0("Choose start time for map and plots (reference: ",fmtTime(time_now())," UTC)"),
                min = time_now() - base::as.difftime(20,units="weeks"),
                max = time_now(),
                value = time_now() - base::as.difftime(20,units="weeks"),
                timeFormat = "%Y-%m-%d %H:%M:%S", timezone = "+0000", ticks = F, animate = animationOptions(interval = 2000))
  })
  
  ## selected position is saved in bookmarks, and can be deleted if last position wants to be used
  posi_lon <- reactiveVal(value=NA)
  posi_lat <- reactiveVal(value=NA)
  observeEvent(input$testmap_click,{
    posi_lon(click()$lng)
    posi_lat(click()$lat)
  })
  
  output$posLat_UI <- renderUI({
    numericInput(ns("posLat"), label="Lat", value=posi_lat(), min=-90, max=90)
  })
  output$posLon_UI <- renderUI({
    numericInput(ns("posLon"), label="Lon", value=posi_lon(), min=-180, max=180)
  })
  
  observeEvent(input$set2Na,{
    posi_lon(NA)
    posi_lat(NA)
    updateNumericInput(session,"posLat",  value=NA)
    updateNumericInput(session,"posLon",  value=NA)
    leafletProxy('testmap') %>% removeMarker("mypos")
  })
  ###
  
  # table parameters
  overviewObj <- reactive({
    req(time_now())
    ## get tag names or set to NA
    if(!any(names(mt_track_data(data_ll))=="tag_local_identifier")){
      data_ll <- data_ll %>% mutate_track_data(tag_local_identifier=NA)
    }
    
    getoverview <- function(x){
      ## get local timezone for last position. Only one position is used if several share the last timestamp
      timeEi <- max(mt_time(x))
      coo <- st_coordinates(x[which.max(mt_time(x)),])
      ## NA if the last position has no coordinates or no time zone is found
      timeEi_tz <- if (any(is.na(coo))) NA else lutz::tz_lookup_coords(lat=coo[,2], lon=coo[,1], method="accurate", warn=FALSE)
      ## local time with the UTC offset of that moment (incl. daylight saving time) and the time zone abbreviation
      locl_time <- if (is.na(timeEi_tz)) NA else format(with_tz(timeEi, tzone=timeEi_tz), format="%Y-%m-%d %H:%M:%S %Z")
      ## tracks of last 24h and last 7 days
      trk24h <- x %>% dplyr::filter(mt_time(x) <= time_now() & mt_time(x) >= time_now()-(24*60*60))
      trk7d <- x %>% dplyr::filter(mt_time(x) <= time_now() & mt_time(x) >= time_now()-(7*24*60*60))

      df <- data.frame(
        ids=unique(mt_track_id(x)),
        tags=mt_track_data(x)$tag_local_identifier,
        time0=fmtTime(min(mt_time(x))),
        timeE=fmtTime(timeEi),
        timeE_local=locl_time,
        posis24h=nrow(trk24h),
        posis7d=nrow(trk7d),
        ## numeric (km) so that the table sorts correctly
        displ24h=if(nrow(trk24h)==0){NA}else{round(as.numeric(sum(mt_distance(trk24h),na.rm=T))/1000,digits=3)},
        displ7d=if(nrow(trk7d)==0){NA}else{round(as.numeric(sum(mt_distance(trk7d),na.rm=T))/1000,digits=3)}
      )
      return(df)
    }
    
    ## get animal status
    evnt <- function(x){
      in7d <- mt_time(x) <= time_now() & mt_time(x) >= time_now()-(7*24*60*60)
      coo7d <- st_coordinates(x)[in7d,,drop=FALSE] # only positions of last 7 days
      coo7d <- coo7d[stats::complete.cases(coo7d),,drop=FALSE]
      if(nrow(coo7d)>0){
        ## mean position on the sphere (correct across the 180° meridian). geomean() fails for a single position
        meanCoo <- if(nrow(coo7d)==1){coo7d[1,]}else{as.vector(geomean(xy=coo7d))}
        dist2mean <- distVincentyEllipsoid(p1=coo7d, p2=meanCoo)
        ## isTRUE(): an empty buffer field (NA) skips that check instead of stopping the app
        if (isTRUE(any(dist2mean>(input$mig7d_dist*1000)))){
          df <- data.frame(event="migration")
        } else if (nrow(coo7d)>1 && isTRUE(all(dist2mean<input$dead7d_dist))){ # a single position cannot indicate mortality
          df <- data.frame(event="dead")
        } else{df <- data.frame(event="-")}
      } else {df <- data.frame(event="no data")}
      return(df)
    }

    ## one row per track. Looping over tracks instead of group_modify(), as group_modify() drops the move2 class
    overviewTB <- bind_rows(lapply(unique(mt_track_id(data_ll)), function(id){
      x <- dplyr::filter(data_ll, mt_track_id(data_ll)==id)
      cbind(getoverview(x),evnt(x))
    }))

    overviewTB %>% relocate(ids,tags,time0,timeE,timeE_local,posis24h,posis7d,displ24h,displ7d,event)
  })
  
  ## row order of the table. Timestamps are text in the format "YYYY-mm-dd HH:MM:SS" (UTC), which sorts correctly
  o <- reactive({
    order(overviewObj()[,input$sort_table], decreasing=(input$sort_direc=="descending"))
  })
  
  # generate reactive parameters for plots and map
  selID <- reactive({
    if (!is.null(input$C)) return(input$C)
    ## default: first track in the (sorted) table that has locations between start time and reference time
    req(input$start_time, time_now())
    idsSorted <- overviewObj()[o(),"ids"]
    inWindow <- mt_time(data_ll)>=input$start_time & mt_time(data_ll)<=time_now()
    idsData <- unique(as.character(mt_track_id(data_ll)[inWindow]))
    c(idsSorted[idsSorted %in% idsData], idsSorted)[1] # first track of the table if no track has data
  })
  
  data_sel_id <- reactive({
    req(input$start_time, time_now(), selID())
    data_ll %>% dplyr::filter(mt_time(data_ll)>=input$start_time & mt_time(data_ll)<=time_now() & mt_track_id(data_ll)==selID())
  }) #here possible to get no data!
  
  ## attributes that can be plotted: numeric and not all NA. Geometry, time, track id and other id columns are excluded
  ## integer64 columns (e.g. checksums) are excluded, they are numeric but plot as meaningless values
  attrNames <- data_ll %>%
    st_drop_geometry() %>%
    as_tibble() %>% # plain tibble, otherwise move2 keeps the time and track id columns
    select(-any_of(c(mt_time_column(data_ll), mt_track_id_column(data_ll)))) %>%
    select(where(~ is.numeric(.) && !inherits(., "integer64") && !all(is.na(.)))) %>%
    select(-ends_with("_id")) %>%
    names()

  ## the choices are only known here, so the selection stored in the bookmark (stored settings) is set again
  updatePickerInput(
    session,
    inputId="attr",
    choices=c("net square displacement (km2)"="nsd",attrNames),
    selected=restoreInput(id=ns("attr"), default="nsd")
  )

  updatePickerInput(
    session,
    inputId="attr2",
    choices=c("net square displacement (km2)"="nsd",attrNames),
    selected=restoreInput(id=ns("attr2"), default="nsd")
  )
  
  timeObj <- reactive({
    mt_time(data_sel_id())
  })
  
  ## message if a selected attribute (e.g. from stored settings) is not in the data
  noAttrMsg <- function(attr) paste0("The attribute '",attr,"' does not exist in the data. Please select another attribute.")
  attrObj <- reactive({
    validate(need(input$attr %in% attrNames, noAttrMsg(input$attr)))
    data_sel_id() %>% pull(input$attr) # get the vector out of a tbl column
  })
  
  attr2Obj <- reactive({
    validate(need(input$attr2 %in% attrNames, noAttrMsg(input$attr2)))
    data_sel_id() %>% pull(input$attr2)
  })
  
  ## reference position: selected position if lat and lon are given, otherwise last position of the selected track. isTruthy() also covers inputs not yet rendered (NULL)
  refPos <- reactive({
    if (isTruthy(input$posLon) && isTruthy(input$posLat)){
      validate(need(abs(input$posLat)<=90 && abs(input$posLon)<=180, "The reference position (Lat/Lon in the 'Settings' tab) is not valid: latitude must be between -90 and 90, longitude between -180 and 180."))
      c(input$posLon, input$posLat)
    } else {
      st_coordinates(data_sel_id())[nrow(data_sel_id()),]
    }
  })

  nsd <- reactive({
    (distVincentyEllipsoid(p1=st_coordinates(data_sel_id()), p2=refPos())/1000)^2
  })
  
  ## calendar day (UTC) of each position of the selected track
  dayOfPos <- reactive({
    as.Date(mt_time(data_sel_id()), tz="UTC")
  })
  daysObj <- reactive({
    unique(dayOfPos())
  })
  
  n_day <- reactive({
    vapply(seq_along(daysObj()), function(i) sum(dayOfPos()==daysObj()[i]), numeric(1))
  })
  displ_day <- reactive({
    coo <- st_coordinates(data_sel_id())
    vapply(seq_along(daysObj()), function(i){
      ix <- which(dayOfPos()==daysObj()[i])
      if (!(1 %in% ix)) ix <- c(min(ix)-1,ix) #add last position before this day
      if (length(ix)>1) sum(distVincentyEllipsoid(p1=coo[ix,,drop=FALSE]),na.rm=TRUE)/1000 else 0
    }, numeric(1))
  })
  
  avgdaily_dist2posi <- reactive({
    coo <- st_coordinates(data_sel_id())
    vapply(seq_along(daysObj()), function(i){
      ix <- which(dayOfPos()==daysObj()[i])
      mean(distVincentyEllipsoid(p1=coo[ix,,drop=FALSE], p2=refPos())/1000, na.rm=TRUE)
    }, numeric(1))
  })
  
  ##### table output
  output$foo <- DT::renderDataTable({
    tb <- overviewObj()[o(),]
    ## radio buttons to select the track for plots and map. The selected track stays checked when the table is sorted again
    ## IDs are escaped, as they are inserted into HTML
    selected <- isolate(input$C)
    show <- sprintf('<input type="radio" name="%s" value="%s"%s/>',
                    ns("C"), htmltools::htmlEscape(tb$ids, attribute=TRUE), ifelse(as.character(tb$ids) %in% selected, " checked", ""))
    tb <- cbind(show, tb)
    names(tb) <- c("Show Plots and Map","Animal","Tag","First timestamp UTC","Last timestamp UTC","Last timestamp local tz","N positions last 24h","N positions last 7d","Moved distance last 24h (km)","Moved distance last 7d (km)","Event last 7d")
    tb
  }, escape = -1, rownames = FALSE, selection = 'none', server = FALSE, # escape all columns except the radio buttons
  options = list(dom = 't', paging = FALSE, ordering = FALSE, scrollY=250, scrollCollapse = TRUE)
  )
  
  ##### plots
  ## message shown in plots and map if the selected track has no locations in the selected time window
  noDataMsg <- "The selected track has no locations between the selected start time and the reference time. Select another track, move the start time slider or adapt the reference time in the 'Settings' tab."

  output$attr_time <- renderPlot({
    validate(need(nrow(data_sel_id())>0, noDataMsg))
    if (input$attr=="nsd"){
      plot(timeObj(),nsd(),type="b",
           xlim=c(min(timeObj(),na.rm=TRUE),max(timeObj(),na.rm=TRUE)),
           ylim=c(min(nsd(),na.rm=TRUE),max(nsd(),na.rm=TRUE)),
           xlab="time",ylab="nsd (km2)",col=2,lwd=2)
    } else{
      validate(need(any(!is.na(attrObj())), paste0("The attribute '",input$attr,"' has no values for the selected track in the selected time window.")))
      plot(timeObj(),attrObj(),type="b",
           xlim=c(min(timeObj(),na.rm=TRUE),max(timeObj(),na.rm=TRUE)),
           ylim=c(min(attrObj(),na.rm=TRUE),max(attrObj(),na.rm=TRUE)),
           xlab="time",ylab=input$attr,col=2,lwd=2)
    }
  })
  
  output$attr2_time <- renderPlot({
    validate(need(nrow(data_sel_id())>0, noDataMsg))
    if(input$attr2=="nsd"){
      plot(timeObj(),nsd(),type="b",
           xlim=c(min(timeObj(),na.rm=TRUE),max(timeObj(),na.rm=TRUE)),
           ylim=c(min(nsd(),na.rm=TRUE),max(nsd(),na.rm=TRUE)),
           xlab="time",ylab="nsd (km2)",col=2,lwd=2)
    }else{
      validate(need(any(!is.na(attr2Obj())), paste0("The attribute '",input$attr2,"' has no values for the selected track in the selected time window.")))
      plot(timeObj(),attr2Obj(),type="b",
           xlim=c(min(timeObj(),na.rm=TRUE),max(timeObj(),na.rm=TRUE)),
           ylim=c(min(attr2Obj(),na.rm=TRUE),max(attr2Obj(),na.rm=TRUE)),
           xlab="time",ylab=input$attr2,col=2,lwd=2)
    }
  })
  
  output$prop_time <- renderPlot({
    validate(need(nrow(data_sel_id())>0, noDataMsg))
    if (input$prop=="n_day"){
      plot(daysObj(),n_day(),type="h",
           xlim=c(min(daysObj(),na.rm=TRUE),max(daysObj(),na.rm=TRUE)),
           ylim=c(min(n_day(),na.rm=TRUE),max(n_day(),na.rm=TRUE)),
           xlab="day",ylab="number of positions per day",col=2,lwd=5)
    } else if(input$prop=="displ_day"){
      plot(daysObj(),displ_day(),type="h",
           xlim=c(min(daysObj(),na.rm=TRUE),max(daysObj(),na.rm=TRUE)),
           ylim=c(min(displ_day(),na.rm=TRUE),max(displ_day(),na.rm=TRUE)),
           xlab="day",ylab="displacement per day (km)",col=2,lwd=5)
    } else if (input$prop=="avgdaily_dist2posi") {
      plot(daysObj(),avgdaily_dist2posi(),type="h",
           xlim=c(min(daysObj(),na.rm=TRUE),max(daysObj(),na.rm=TRUE)),
           ylim=c(min(avgdaily_dist2posi(),na.rm=TRUE),max(avgdaily_dist2posi(),na.rm=TRUE)),
           xlab="day",ylab="average daily distance to selected position (km)",col=2,lwd=5)
    }
  })
  
  output$leafmap <- renderLeaflet({
    validate(need(nrow(data_sel_id())>0, noDataMsg))
    bounds <- as.vector(st_bbox(data_sel_id()))
    outl <- leaflet() %>%
      setMapView(bounds=bounds) %>%
      addTiles(group = "StreetMap") %>%
      addProviderTiles("Esri.WorldTopoMap",group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addPolylines(data =  st_coordinates(data_sel_id()), color ="cyan", group = "lines") %>%
      addCircleMarkers(data = data_sel_id(), fillOpacity = 0.3, opacity = 0.5, color="blue",radius=1, group = "points") %>%
      addCircleMarkers(data = tail(st_coordinates(data_sel_id()),n=3), fillOpacity = 0.3, opacity = 0.5, color="red", radius=1, group = "last positions") %>%
      addLegend(position= "topright", colors=c("cyan","blue","red"),
                labels=c("lines","points","last positions") ,opacity = 0.7, title = paste("track",unique(mt_track_id(data_sel_id())))) %>%
      addScaleBar(position="topleft",
                  options=scaleBarOptions(maxWidth = 100,
                                          metric = TRUE, imperial = F, updateWhenIdle = TRUE)) %>%
      addLayersControl(
        baseGroups = c("StreetMap", "TopoMap", "Aerial"),
        overlayGroups = c("lines", "points","last positions"),
        options = layersControlOptions(collapsed = FALSE)
      )
    outl
  })
  
  return(reactive({ data })) # input data unchanged
}


