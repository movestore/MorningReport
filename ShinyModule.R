library('move')
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

#setwd("/root/app/")
Sys.setenv(tz="UTC")


## improve error msg ob tab2
# - if tag has no data "now". work in progress



shinyModuleUserInterface <- function(id, label){
  ns <- NS(id)
  navbarPage("Morning Report",
             tabPanel("Settings", 
                      fluidRow(
                        h5("1. Define your reference date and time, i.e. the date and time from which you want to look back at your data. If your tags are still functioning, this is typically ‘NOW’ in UTC. If you don’t change the date and time, ‘NOW’ will be used by default"),
                        column(3,dateInput(ns("date"),"Date:", value = as_date(now("UTC")))),
                        column(3,timeInput(ns("time"), "Time (24h format):", value = strftime(now("UTC"), format="%H:%M:%S", tz="UTC"), seconds = FALSE))
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
    lasLocs <- moveStack(lapply(split(data), function(ind){
      ind[n.locs(ind)]
    }),forceTz="UTC")
    
    bounds <- as.vector(bbox(extent((lasLocs))))
    outl <- leaflet() %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap",group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addCircleMarkers(data = lasLocs, fillOpacity = 0.7, opacity = 0.7, color="red", radius=5, label=namesIndiv(lasLocs),labelOptions = labelOptions(noHide = T)) %>%
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
    time_now(as.POSIXct(paste0(input$date," ",strftime(input$time, format="%H:%M:%S"))))
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
    names(data) <- make.names(names(data),allow_=FALSE)
    
    data_spl <- move::split(data)
    ids <- namesIndiv(data)
    if (any(names(data@data)=="tags.local.identifier"))
    {
      tags <- foreach(datai = data_spl, .combine=c) %do% {
        datai@data$tag.local.identifier[1]
      }
    } else tags <- rep(NA,length(ids))
    
    time0 <- foreach(datai = data_spl, .combine=c) %do% {
      as.character(min(timestamps(datai)))
    }
    timeE <- foreach(datai = data_spl, .combine=c) %do% {
      as.character(max(timestamps(datai)))
    }
    timeE_local <- foreach(datai = data_spl, .combine=c) %do% {
      timeEi <- max(timestamps(datai))
      coo <- coordinates(datai[timestamps(datai)==timeEi][1,])
      timeEi_tz <- lutz::tz_lookup_coords(coo[,2],coo[,1], method="accurate")
      timeEi_offset <- lutz::tz_offset(as.Date(timeEi),timeEi_tz)
      as.character(as.POSIXct(timeEi)+(timeEi_offset$utc_offset_h*3600))
    }
    
    event <- foreach(datai = data_spl, .combine=c) %do% {
      ix <- which(timestamps(datai) <= time_now() & timestamps(datai) >= time_now()-(7*24*60*60))
      if (length(ix)>0)
      {
        datai7d <- datai[ix,]
        meanlon <- mean(coordinates(datai7d)[,1])
        meanlat <- mean(coordinates(datai7d)[,2])
        if (any(distVincentyEllipsoid(coordinates(datai7d),c(meanlon,meanlat))>(input$mig7d_dist*1000)))
        {
          "migration"
        } else if (all(distVincentyEllipsoid(coordinates(datai7d),c(meanlon,meanlat))<input$dead7d_dist ))
        {
          "dead"
        } else "-"
      } else "no data"
    }
    
    posis24h <- foreach(datai = data_spl, .combine=c) %do% {
      ix <- which(timestamps(datai) <= time_now() & timestamps(datai) >= time_now()-(24*60*60))
      length(ix)
    }
    posis7d <- foreach(datai = data_spl, .combine=c) %do% {
      ix <- which(timestamps(datai) <= time_now() & timestamps(datai) >= time_now()-(7*24*60*60))
      length(ix)
    }
    displ24h <- foreach(datai = data_spl, .combine=c) %do% {
      ix <- which(timestamps(datai) <= time_now() & timestamps(datai) >= time_now()-(24*60*60))
      if (length(ix)>0) paste(round(sum(distance(datai[ix,]))/1000,digits=3),"km") else NA
    }
    displ7d <- foreach(datai = data_spl, .combine=c) %do% {
      ix <- which(timestamps(datai) <= time_now() & timestamps(datai) >= time_now()-(7*24*60*60))
      if (length(ix)>0) paste(round(sum(distance(datai[ix,]))/1000,digits=3),"km") else NA
    }
    
    overview <- data.frame(ids,tags,time0,timeE,timeE_local,posis24h,posis7d,displ24h,displ7d,event) #timeE_local,
    N <- length(overview[1,])
    for (i in seq_len(nrow(overview)))
    {
      
      overview[i,N+1] <- sprintf(
        '<input type="radio" name="%s" value="%s"/>',
        session$ns("C"),ids[i]
      )
      names(overview)[N+1] <- "show"
    }
    overview[,c(N+1,1:N)]
  })
  
  
  
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
    dataObj()[timestamps(dataObj())>input$start_time & timestamps(dataObj())<time_now() & trackId(dataObj())==selID()]
  }) #here possible to get no data!
  
  updatePickerInput(
    session,
    inputId="attr",
    choices=c("net square displacement (km2)"="nsd",names(data@data[,!sapply(data@data, function(x) all(is.na(x)))] %>% select_if(is.numeric)))
  )
  
  updatePickerInput(
    session,
    inputId="attr2",
    choices=c("net square displacement (km2)"="nsd",names(data@data[,!sapply(data@data, function(x) all(is.na(x)))] %>% select_if(is.numeric)))
  )
  
  timeObj <- reactive({
    timestamps(data_sel_id())
  })
  
  attrObj <- reactive({
    data_sel_id()@data[,input$attr]
  })
  
  attr2Obj <- reactive({
    data_sel_id()@data[,input$attr2]
  })
  
  nsd <- reactive({
    (distVincentyEllipsoid(coordinates(data_sel_id()),coordinates(data_sel_id())[1,])/1000)^2
  })
  nsd2 <- reactive({nsd()})
  
  daysObj <- reactive({
    unique(as.Date(timestamps(data_sel_id())))
  })
  
  n_day <- reactive({
    foreach(dayi = daysObj(), .combine=c) %do% {
      length(which(as.Date(timestamps(data_sel_id()))==dayi))
    }})
  displ_day <- reactive({
    foreach(dayi = daysObj(), .combine=c) %do% {
      ix <- which(as.Date(timestamps(data_sel_id()))==dayi)
      if (!(1 %in% ix)) ix <- c(min(ix)-1,ix) #add last position before this day
      if (length(ix)>1) sum(distVincentyEllipsoid(coordinates(data_sel_id()[ix,])),na.rm=TRUE)/1000 else 0
    }})
  
  avgdaily_dist2posi <- reactive({
    # ## using 1st position
    # if (is.na(input$posLon)){lonZ <- coordinates(data_sel_id())[1,1]} else {lonZ <- input$posLon}
    # if (is.na(input$posLat)){latZ <- coordinates(data_sel_id())[1,2]} else {latZ <- input$posLat}
    ## using last position
    if (is.na(input$posLon)){lonZ <- coordinates(data_sel_id())[n.locs(data_sel_id()),1]} else {lonZ <- input$posLon}
    if (is.na(input$posLat)){latZ <- coordinates(data_sel_id())[n.locs(data_sel_id()),2]} else {latZ <- input$posLat}
    
    foreach(dayi = daysObj(), .combine=c) %do% {
      ix <- which(as.Date(timestamps(data_sel_id()))==dayi)
      mean(distVincentyEllipsoid(coordinates(data_sel_id()[ix,]),c(lonZ,latZ))/1000,rm.na=TRUE)
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
    
    if (input$attr=="nsd") plot(timeObj(),nsd(),type="b",xlim=c(min(timeObj(),na.rm=TRUE),max(timeObj(),na.rm=TRUE)),ylim=c(min(nsd(),na.rm=TRUE),max(nsd(),na.rm=TRUE)),xlab="time",ylab="nsd (km2)",col=2,lwd=2) else plot(timeObj(),attrObj(),type="b",xlim=c(min(timeObj(),na.rm=TRUE),max(timeObj(),na.rm=TRUE)),ylim=c(min(attrObj(),na.rm=TRUE),max(attrObj(),na.rm=TRUE)),xlab="time",ylab=input$attr,col=2,lwd=2)
  })
  
  output$attr2_time <- renderPlot({
    if (input$attr2=="nsd") plot(timeObj(),nsd(),type="b",xlim=c(min(timeObj(),na.rm=TRUE),max(timeObj(),na.rm=TRUE)),ylim=c(min(nsd(),na.rm=TRUE),max(nsd(),na.rm=TRUE)),xlab="time",ylab="nsd (km2)",col=2,lwd=2) else plot(timeObj(),attr2Obj(),type="b",xlim=c(min(timeObj(),na.rm=TRUE),max(timeObj(),na.rm=TRUE)),ylim=c(min(attr2Obj(),na.rm=TRUE),max(attr2Obj(),na.rm=TRUE)),xlab="time",ylab=input$attr2,col=2,lwd=2)
  })
  
  output$prop_time <- renderPlot({
    if (input$prop=="n_day") plot(daysObj(),n_day(),type="h",xlim=c(min(daysObj(),na.rm=TRUE),max(daysObj(),na.rm=TRUE)),ylim=c(min(n_day(),na.rm=TRUE),max(n_day(),na.rm=TRUE)),xlab="day",ylab="number of positions per day",col=2,lwd=5)
    else if (input$prop=="displ_day") plot(daysObj(),displ_day(),type="h",xlim=c(min(daysObj(),na.rm=TRUE),max(daysObj(),na.rm=TRUE)),ylim=c(min(displ_day(),na.rm=TRUE),max(displ_day(),na.rm=TRUE)),xlab="day",ylab="displacement per day (km)",col=2,lwd=5)
    else if (input$prop=="avgdaily_dist2posi") plot(daysObj(),avgdaily_dist2posi(),type="h",xlim=c(min(daysObj(),na.rm=TRUE),max(daysObj(),na.rm=TRUE)),ylim=c(min(avgdaily_dist2posi(),na.rm=TRUE),max(avgdaily_dist2posi(),na.rm=TRUE)),xlab="day",ylab="average daily distance to selected position (km)",col=2,lwd=5)
  })
  
  output$leafmap <- renderLeaflet({
    bounds <- as.vector(bbox(extent(data_sel_id())))
    outl <- leaflet() %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap",group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addPolylines(data =  coordinates(data_sel_id()), color ="cyan", group = "lines") %>%
      addCircleMarkers(data = data_sel_id(), fillOpacity = 0.3, opacity = 0.5, color="blue",radius=1, group = "points") %>%
      addCircleMarkers(data = tail(coordinates(data_sel_id()),n=3), fillOpacity = 0.3, opacity = 0.5, color="red", radius=1, group = "last positions") %>%
      addLegend(position= "topright", colors=c("cyan","blue","red"),
                labels=c("lines","points","last positions") ,opacity = 0.7, title = paste("track",unique(trackId(data_sel_id())))) %>%
      addScaleBar(position="bottomleft",
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


