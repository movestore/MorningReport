library('move2')
library('shiny')
library('shinyWidgets')
library('foreach')
library('maps')
library('lutz')
library('geosphere')
library('DT')
library('dplyr')
library('leaflet')
library('sf')
library("shinyTime")
library("lubridate")
library("shinyBS")

#setwd("/root/app/")
Sys.setenv(tz="UTC")


## improve error msg ob tab2
# - if tag has no data "now". work in progress



shinyModuleUserInterface <- function(id, label){
  ns <- NS(id)
  navbarPage("Morning Report",
             tabPanel("Settings", 
                      fluidRow(
                        h5("1. Define your reference date and time, i.e. the date and time from which you want to look back at your data. If your tags are still functioning, this is typically ‘NOW’ in UTC. If you don’t change the date and time, ‘NOW’ will be used by default."),
                        h5("Check the box 'Always use 'NOW'' to use the 'NOW' date and time of each run of a scheduled workflow, date and time will be updated at each run. Unckeck the box if you want to use a fixed date and time."),
                        column(3,dateInput(ns("date"),"Date:", value = as_date(now("UTC")))),
                        column(2,timeInput(ns("time"), "Time (24h format):",value= now("UTC"), seconds = FALSE)), 
                        column(2, checkboxInput(ns("alwaysNOW"), "Always use date and time 'NOW'", value=TRUE),style = 'top: 25px;position:relative;'),
                        bsTooltip(id=ns("alwaysNOW"), title="Checked: When this app is part of a scheduled workflow, each time the workflow runs, it will use the date and time of moment when the app is executed. Unchecked: the selected date and time will be fixed.", placement = "bottom", trigger = "hover", options = list(container = "body")),
                      ),
                      h5("2. Define the radius (in km) an animal must move past to qualify as migration behaviour. Default 100 km"),
                      numericInput(ns("mig7d_dist"),"Migration buffer in km (last 7 days)", min=0, value=100),
                      h5("3. Define the radius (in m) within which an animal must remain to indicate a likely mortality. Default 100 m"),
                      numericInput(ns("dead7d_dist"),"Mortality buffer in m (last 7 days)", min=0, value=100),
                      
                      h5("4. Select your reference position by clicking on the map, i.e. if you are in the field use your current location to find out if any tagged animals are in the surroundings. The distance to this location will be calculated for all of your data points. If no location is selected, the last position of each track is used."),
                      leafletOutput(ns("testmap")),
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
                          #leafletOutput(ns("leafmap"),height="500px"),
                          fluidRow(
                            column(width=6,
                                   plotOutput(ns("attr_time"),height="220px"),
                                   # textOutput(ns("errortext")),
                                   plotOutput(ns("attr2_time"),height="220px"),
                                   plotOutput(ns("prop_time"),height="220px")
                            ),
                            column(width=6, leafletOutput(ns("leafmap"),height="500px"))
                          )
                        )
                      )
             )
  )
}


shinyModule <- function(input, output, session, data){
  ns <- session$ns
  dataObj <- reactive({ data })
  current <- reactiveVal(data)
  
  ## map to choose coords from
  output$testmap <- renderLeaflet({
    lasLocs <- data%>%group_by(mt_track_id())%>%filter(row_number()==n())
    
    bounds <- as.vector(st_bbox(lasLocs))
    outl <- leaflet() %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap",group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addCircleMarkers(data = lasLocs, fillOpacity = 0.7, opacity = 0.7, color="red", radius=5,label=unique(mt_track_id(lasLocs)), labelOptions = labelOptions(noHide = T)) %>%
      addLayersControl(baseGroups = c("StreetMap", "Aerial")) %>%
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
  time_now <- reactiveVal()
  output$start_timeUI <- renderUI({
    ## when workflow is scheduled,it always takes the time of that moment when it runs if alwaysNOW is checked
    updateDateInput(session, "date", value = if(input$alwaysNOW){as_date(now("UTC"))}else{input$date})
    updateTimeInput(session, "time", value = if(input$alwaysNOW==TRUE){now("UTC")}else{input$time})
    
    time_now(as.POSIXct(paste0(input$date," ",strftime(input$time, format="%H:%M:%S")),tz="UTC"))
    
    sliderInput(ns("start_time"),
                paste0("Choose start time for map and plots (reference: ",input$date," ", strftime(input$time, format="%H:%M:%S"),")"),
                min = time_now() - as.difftime(20,units="weeks"),
                max = time_now(),
                value = time_now() - as.difftime(20,units="weeks"),
                timeFormat = "%Y-%m-%d %H:%M:%S", ticks = F, animate = T)
  })
  
  ## selected position is saved in bookmarks, and cab be deleted if last position wants to be used
  posi_lon <- reactiveVal(value=NA)
  posi_lat <- reactiveVal(value=NA)
  observeEvent(input$testmap_click,{
    posi_lon(click()$lng)
    posi_lat(click()$lat)
  })
  
  output$posLat_UI <- renderUI({
    numericInput(ns("posLat"), "Lat", value=posi_lat())
  })
  output$posLon_UI <- renderUI({
    numericInput(ns("posLon"), "Lon", value=posi_lon())
  })
  
  observeEvent(input$set2Na,{
    updateNumericInput(session,"posLat",  value=NA)
    updateNumericInput(session,"posLon",  value=NA)
  })
  ###
  
  # table parameters
  overviewObj <- reactive({
    trkIDcolnm <- mt_track_id_column(data)
    ## get tag names or set to NA
    if(any(names(mt_track_data(data))=="tag_local_identifier")){
    }else{
      data <- data %>% mutate_track_data(tag_local_identifier=NA)
    }
    
    getoverview <- function(x){
      ## get local timezone for last position (probably posible to code more efficiently...)
      timeEi <- max(mt_time(x))
      coo <- st_coordinates(x %>% filter(mt_time()==max(mt_time())))
      timeEi_tz <- lutz::tz_lookup_coords(coo[,2],coo[,1], method="accurate")
      timeEi_offset <- lutz::tz_offset(as.Date(timeEi),timeEi_tz)
      locl_time <- as.character(timeEi+(timeEi_offset$utc_offset_h*3600))
      ## tracks of last 24h and last 7 days 
      trk24h <- x %>% filter(mt_time(x) <= time_now() & mt_time(x) >= time_now()-(24*60*60))
      trk7d <- x %>% filter(mt_time(x) <= time_now() & mt_time(x) >= time_now()-(7*24*60*60))

      df <- data.frame(
        ids=unique(mt_track_id(x)),
        tags=mt_track_data(x)$tag_local_identifier,
        time0=as.character(min(mt_time(x))),
        timeE=as.character(max(mt_time(x))),
        timeE_local=locl_time,
        posis24h=nrow(trk24h),
        posis7d=nrow(trk7d),
        displ24h=if(nrow(trk24h)==0){NA}else{paste(round(sum(mt_distance(trk24h),na.rm=T)/1000,digits=3),"km")},
        displ7d=if(nrow(trk7d)==0){NA}else{paste(round(sum(mt_distance(trk7d),na.rm=T)/1000,digits=3),"km")}
      )
      return(df)
    }
    
    overview_tb <- data%>%group_by(mt_track_id()) %>%
      group_modify(~getoverview(.x))
    overview_tb <- st_drop_geometry(overview_tb)
    overviewTB <- rename(overview_tb, {{trkIDcolnm}}:= 1)
    
    ## get animal status (probably took a coding detour)
    data <- dplyr::mutate(data, lon = st_coordinates(data)[,1], lat = st_coordinates(data)[,2])
    datai7d <-
      data %>%
      group_by(mt_track_id()) %>%
      filter(mt_time() <= time_now() & mt_time() >= time_now()-(7*24*60*60))%>% 
      reframe(
        meanlon = mean(lon),
        meanlat = mean(lat)
      )
    
    datai7d <- rename(datai7d, {{trkIDcolnm}}:= 1)
    data <- left_join(data,datai7d, by=trkIDcolnm)
    
    evnt <- function(x){
      if(!all(is.na(x$meanlat))){
        if (any(distVincentyEllipsoid(st_coordinates(x),c(unique(x$meanlon),unique(x$meanlat)))>(500*1000))){
          df <- data.frame(event="migration")
        } else if (all(distVincentyEllipsoid(st_coordinates(x),c(unique(x$meanlon),unique(x$meanlat)))<100 )){
          df <- data.frame(event="dead")
        } else{df <- data.frame(event="-")}
      } else {df <- data.frame(event="no data")}
      return(df)
    }
    
    eventDF <- data %>%
      group_by(mt_track_id()) %>%
      group_modify(~evnt(.x))
    eventDF <- rename(eventDF, {{trkIDcolnm}}:= 1)
    
    ## join tables
    overviewTB <- left_join(overviewTB,eventDF, by=trkIDcolnm)
    overviewTB <- overviewTB %>% as_tibble() %>% select(-c(1)) # drop trkIDcolnm column used for joining
    overviewTB <- overviewTB %>% relocate(ids,tags,time0,timeE,timeE_local,posis24h,posis7d,displ24h,displ7d,event)
    
    ## "cheating" to achive a quicker fix. converting tibble to data.frame so the code below works. Will update gradually to tidyverse
    overviewDF <- as.data.frame(overviewTB)
    
    IDs <- overviewDF$ids
    
    N <- ncol(overviewDF)
    for (i in seq_len(nrow(overviewDF))){
      overviewDF[i,N+1] <- sprintf(
        '<input type="radio" name="%s" value="%s"/>',
        session$ns("C"),IDs[i]
      )
      names(overviewDF)[N+1] <- "show"
    }
    overviewDF[,c(N+1,1:N)]
  })
  
  
  #### USE ARRANGE
  o <- reactive({
    if (input$sort_table %in% c("time0","timeE")) sort_tab <- as.POSIXct(overviewObj()[,input$sort_table]) else sort_tab <- overviewObj()[,input$sort_table] #,"timeE_local"
    if (input$sort_direc=="descending") order(sort_tab, decreasing = TRUE) else if(input$sort_direc=="ascending") order(sort_tab, decreasing = FALSE) else seq(along=sort_tab)
  })
  
  overviewObj_named <- reactive({
    overview_named <- overviewObj()
    names(overview_named) <- c("Show Plots and Map","Animal","Tag","First timestamp UTC","Last timestamp UTC","Last timestamp local tz","N positions last 24h","N positions last 7d","Moved distance last 24h","Moved distance last 7d","Event last 7d") #,"Last timestamp local tz"
    overview_named
  })
  
  # generate reactive parameters for plots and map
  selID <- reactive({
    if (is.null(input$C)) overviewObj()$ids[1] else input$C
  })
  
  data_sel_id <- reactive({
    dataObj() %>% filter(mt_time(dataObj())>input$start_time & mt_time(dataObj())<time_now() & mt_track_id(dataObj())==selID())
  }) #here possible to get no data!
  
  updatePickerInput(
    session,
    inputId="attr",
    choices=c("net square displacement (km2)"="nsd",names(data %>% select(where(~ !(all(is.na(.))))) %>% select(where(~ (all(is.numeric(.))))))) ## removing attributes that are all NA, and selecting those that are numeric
  )
  
  updatePickerInput(
    session,
    inputId="attr2",
    choices=c("net square displacement (km2)"="nsd",names(data %>% select(where(~ !(all(is.na(.))))) %>% select(where(~ (all(is.numeric(.))))))) ## removing attributes that are all NA, and selecting those that are numeric
  )
  
  timeObj <- reactive({
    mt_time(data_sel_id())
  })
  
  attrObj <- reactive({
    data_sel_id() %>% pull(input$attr) # get the vector out of a tbl column
  })
  
  attr2Obj <- reactive({
    data_sel_id() %>% pull(input$attr2)
  })
  
  nsd <- reactive({
    (distVincentyEllipsoid(st_coordinates(data_sel_id()),st_coordinates(data_sel_id())[1,])/1000)^2
  })
  nsd2 <- reactive({nsd()})
  
  daysObj <- reactive({
    unique(as.Date(mt_time(data_sel_id())))
  })
  
  n_day <- reactive({
    foreach(dayi = daysObj(), .combine=c) %do% {
      length(which(as.Date(mt_time(data_sel_id()))==dayi))
    }})
  displ_day <- reactive({
    foreach(dayi = daysObj(), .combine=c) %do% {
      ix <- which(as.Date(mt_time(data_sel_id()))==dayi)
      if (!(1 %in% ix)) ix <- c(min(ix)-1,ix) #add last position before this day
      if (length(ix)>1) sum(distVincentyEllipsoid(st_coordinates(data_sel_id()[ix,])),na.rm=TRUE)/1000 else 0
    }})
  
  avgdaily_dist2posi <- reactive({
    if (is.na(input$posLon)){lonZ <- st_coordinates(data_sel_id())[nrow(data_sel_id()),1]} else {lonZ <- input$posLon}
    if (is.na(input$posLat)){latZ <- st_coordinates(data_sel_id())[nrow(data_sel_id()),2]} else {latZ <- input$posLat}
    
    foreach(dayi = daysObj(), .combine=c) %do% {
      ix <- which(as.Date(mt_time(data_sel_id()))==dayi)
      mean(distVincentyEllipsoid(st_coordinates(data_sel_id()[ix,]),c(lonZ,latZ))/1000,rm.na=TRUE)
    }})
  
  ##### table output
  output$foo <- DT::renderDataTable(
    overviewObj_named()[o(),],escape = FALSE, selection = 'none', server = FALSE,
    options = list(dom = 't', paging = FALSE, ordering = FALSE, rownames=FALSE, scrollY=250, scrollCollapse = TRUE)
  )
  
  ##### plots
  ## HERE: TRY AND ADAPT ERROR MESSAGES IF THERE ARE NO DATA SELECTED
  output$attr_time <- renderPlot({
    # if(!exists("data_sel_id")){output$errortext <- renderText({"ERROR: Your data set does not contain any locations of the previous 5 months before your selected reference time (default NOW). Please adapt your reference time or load different data"})}
    # if(!exists("data_sel_id")){return(plot_exception("ERROR: Your data set does not contain any locations of the previous 5 months before your selected reference time (default NOW). Please adapt your reference time or load different data"))}
    # validate(need(data_sel_id,"Dataframe not found")) 
    
    if (input$attr=="nsd"){
      plot(timeObj(),nsd(),type="b",
           xlim=c(min(timeObj(),na.rm=TRUE),max(timeObj(),na.rm=TRUE)),
           ylim=c(min(nsd(),na.rm=TRUE),max(nsd(),na.rm=TRUE)),
           xlab="time",ylab="nsd (km2)",col=2,lwd=2)
    } else{
      plot(timeObj(),attrObj(),type="b",
           xlim=c(min(timeObj(),na.rm=TRUE),max(timeObj(),na.rm=TRUE)),
           ylim=c(min(attrObj(),na.rm=TRUE),max(attrObj(),na.rm=TRUE)),
           xlab="time",ylab=input$attr,col=2,lwd=2)
    }
  })
  
  output$attr2_time <- renderPlot({
    if(input$attr2=="nsd"){
      plot(timeObj(),nsd(),type="b",
           xlim=c(min(timeObj(),na.rm=TRUE),max(timeObj(),na.rm=TRUE)),
           ylim=c(min(nsd(),na.rm=TRUE),max(nsd(),na.rm=TRUE)),
           xlab="time",ylab="nsd (km2)",col=2,lwd=2)
    }else{
      plot(timeObj(),attr2Obj(),type="b",
           xlim=c(min(timeObj(),na.rm=TRUE),max(timeObj(),na.rm=TRUE)),
           ylim=c(min(attr2Obj(),na.rm=TRUE),max(attr2Obj(),na.rm=TRUE)),
           xlab="time",ylab=input$attr2,col=2,lwd=2)
    }
  })
  
  output$prop_time <- renderPlot({
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
    bounds <- as.vector(st_bbox(data_sel_id()))
    outl <- leaflet() %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addTiles() %>%
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
        baseGroups = c("StreetMap", "Aerial"),
        overlayGroups = c("lines", "points","last positions"),
        options = layersControlOptions(collapsed = FALSE)
      )
    outl
  })
  
  return(reactive({ current() })) 
}


