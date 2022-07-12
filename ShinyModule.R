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

#setwd("/root/app/")
Sys.setenv(tz="UTC")

#names(data@data[,!sapply(data@data,function(x) all(is.na(x)))] %>% select_if(is.numeric))
#names(data@data[,!sapply(data@data,function(x) all(is.na(x)))] %>% select_if(is.numeric))


shinyModuleUserInterface <- function(id, label, time_now=NULL, posi_lon=NULL, posi_lat=NULL, mig7d_dist, dead7d_dist) {
  ns <- NS(id)
  if (is.null(time_now)) time_now <- Sys.time() else time_now <- as.POSIXct(time_now,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
  
  tagList(
    titlePanel("Morning Report"),
    div(id=ns("C"),class='shiny-input-radiogroup',DT::dataTableOutput(ns("foo"))),
    sidebarLayout(
      sidebarPanel(

        selectInput(inputId = ns("sort_table"),
                    label = "Select property by which to sort the overview table",
                    choices = c("animal name" = "ids", "tag ID" = "tags", "timestamp of first deployed location" = "time0", "timestamp of last deployed location"= "timeE", "number of locations during last 24 hours" = "posis24h", "number of locations during last 7 days" = "posis7d", "displacement during last 24 hours (km)" = "displ24h", "displacement during last 7 days (km)" = "displ7d")), #, "local timestamp of last deployed location" = "timeE_local"
        radioButtons(inputId = ns("sort_direc"),
                     label = "Choose how to sort the table",
                     choices = c("ascending","descending")),
        sliderInput(ns("start_time"),
                    paste0("Choose start time for map and plots (reference: ",time_now,")"),
                    min = as.POSIXct(time_now) - as.difftime(20,units="weeks"),
                    max = as.POSIXct(time_now),
                    value = as.POSIXct(time_now) - as.difftime(20,units="weeks"),
                    timeFormat = "%Y-%m-%d %H:%M:%S", ticks = F, animate = T),

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
                 plotOutput(ns("attr2_time"),height="220px"),
                 plotOutput(ns("prop_time"),height="220px")
          ),
          column(width=6, leafletOutput(ns("leafmap"),height="500px"))
        )
      )
    )
  )
}


shinyModuleConfiguration <- function(id, input) {
  ns <- NS(id)
  configuration <- list()

  print(ns('time_now'))
  configuration["time_now"] <- input[[ns('time_now')]]

  print(ns('posi_lon'))
  configuration["posi_lon"] <- input[[ns('posi_lon')]]

  print(ns('posi_lat'))
  configuration["posi_lat"] <- input[[ns('posi_lat')]]

  print(ns('mig7d_dist'))
  configuration["mig7d_dist"] <- input[[ns('mig7d_dist')]]

  print(ns('dead7d_dist'))
  configuration["dead7d_dist"] <- input[[ns('dead7d_dist')]]
  
  configuration
}


shinyModule <- function(input, output, session, data, time_now=NULL, posi_lon=NULL, posi_lat=NULL, mig7d_dist, dead7d_dist) {
  dataObj <- reactive({ data })
  current <- reactiveVal(data)
  if (is.null(time_now)) time_now <- Sys.time() else time_now <- as.POSIXct(time_now)

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
      ix <- which(timestamps(datai) <= time_now & timestamps(datai) >= time_now-(7*24*60*60))
      if (length(ix)>0) 
      {
        datai7d <- datai[ix,]
        meanlon <- mean(coordinates(datai7d)[,1])
        meanlat <- mean(coordinates(datai7d)[,2])
        if (any(distVincentyEllipsoid(coordinates(datai7d),c(meanlon,meanlat))>mig7d_dist))
        {
          "migration"
        } else if (all(distVincentyEllipsoid(coordinates(datai7d),c(meanlon,meanlat))<dead7d_dist))
        {
          "dead"
        } else "-"
      } else "no data"
    }
    
    posis24h <- foreach(datai = data_spl, .combine=c) %do% {
      ix <- which(timestamps(datai) <= time_now & timestamps(datai) >= time_now-(24*60*60))
      length(ix)
    }
    posis7d <- foreach(datai = data_spl, .combine=c) %do% {
      ix <- which(timestamps(datai) <= time_now & timestamps(datai) >= time_now-(7*24*60*60))
      length(ix)
    }
    displ24h <- foreach(datai = data_spl, .combine=c) %do% {
      ix <- which(timestamps(datai) <= time_now & timestamps(datai) >= time_now-(24*60*60))
      if (length(ix)>0) paste(round(sum(distance(datai[ix,]))/1000,digits=3),"km") else NA
    }
    displ7d <- foreach(datai = data_spl, .combine=c) %do% {
      ix <- which(timestamps(datai) <= time_now & timestamps(datai) >= time_now-(7*24*60*60))
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
    dataObj()[timestamps(dataObj())>input$start_time & timestamps(dataObj())<time_now & trackId(dataObj())==selID()]
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

  if (is.null(posi_lon)) lonZ <- reactive({coordinates(data_sel_id())[1,1]}) else lonZ <- reactive({posi_lon})
  if (is.null(posi_lat)) latZ <- reactive({coordinates(data_sel_id())[1,2]}) else latZ <- reactive({posi_lat})

  avgdaily_dist2posi <- reactive({
    foreach(dayi = daysObj(), .combine=c) %do% {
      ix <- which(as.Date(timestamps(data_sel_id()))==dayi)
      mean(distVincentyEllipsoid(coordinates(data_sel_id()[ix,]),c(lonZ(),latZ()))/1000,rm.na=TRUE)
    }})
  
##### table output
  output$foo <- DT::renderDataTable(
    overviewObj_named()[o(),],escape = FALSE, selection = 'none', server = FALSE,
    options = list(dom = 't', paging = FALSE, ordering = FALSE, rownames=FALSE, scrollY=250, scrollCollapse = TRUE)
  )

##### plots
## HERE: TRY AND ADAPT ERROR MESSAGES IF THERE ARE NO DATA SELECTED
output$attr_time <- renderPlot({
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


