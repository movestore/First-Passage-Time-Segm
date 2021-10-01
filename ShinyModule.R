library('move')
library('ggmap')
library('adehabitatLT')
library('shiny')
library('leaflet')
library('DT')
library('foreach')

Sys.setenv(tz="UTC") 

shinyModuleUserInterface <- function(id, label, radius, time_thr) {
  ns <- NS(id)

  tagList(
    titlePanel("First Passage Time Segmentation"),

    sliderInput(inputId = ns("time_thr"), 
        label = "Time threshold for passing out of radius (unit = days) to split movement from resting", 
        value = time_thr, min = 0, max = 50),

    div(id=ns("C"),class='shiny-input-radiogroup',DT::dataTableOutput(ns("foo"))),
    
    fluidRow(
          column(width=5, plotOutput(ns("fpt"),height="500px")),
          column(width=7, leafletOutput(ns("leafmap"),height="500px"))
    )
  )
}


shinyModuleConfiguration <- function(id, input) {
  ns <- NS(id)
  
  configuration <- list()
  
  print(ns('radius'))
  configuration["radius"] <- input[[ns('radius')]]
  
  print(ns('time_thr'))
  configuration["time_thr"] <- input[[ns('time_thr')]]
  
  configuration
}

shinyModule <- function(input, output, session, data, radius, time_thr) {
    current <- reactiveVal(data)

    data.split <- move::split(data)
    
    fpt <- foreach(datai = data.split) %do%
      {
        datait <- spTransform(datai,CRSobj=paste0("+proj=aeqd +lat_0=",round(mean(coordinates(data)[,2]),digits=1)," +lon_0=",round(mean(coordinates(data)[,1]),digits=1)," +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
        track <- as.ltraj("xy"=data.frame("x"=coordinates(datait)[,1],"y"=coordinates(datait)[,2]),"date"=as.POSIXct(timestamps(datait)),"id"=namesIndiv(datait),"typeII"=TRUE,"proj4string"=CRS(paste0("+proj=aeqd +lat_0=",round(mean(coordinates(data)[,2]),digits=1)," +lon_0=",round(mean(coordinates(data)[,1]),digits=1)," +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))

        f <- fpt(track,radii=radius,units=c("days"))
        fpt_val <- f[[1]][,1]
        time <- as.POSIXct(timestamps(datait))
        data.frame(fpt_val,time)
      }
    
    names(fpt) <- namesIndiv(data)
    
    overview <- reactive({
      ids <- namesIndiv(data)
      tags <- foreach(datai = data.split, .combine=c) %do% {
        datai@data$tag_local_identifier[1]
      }
      time0 <- foreach(datai = data.split, .combine=c) %do% {
        as.character(min(timestamps(datai)))
      }
      timeE <- foreach(datai = data.split, .combine=c) %do% {
        as.character(max(timestamps(datai)))
      }
      
      overview <- data.frame(ids,tags,time0,timeE)
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
    
    overview_named <- reactive({
      overview_named <- overview()
      names(overview_named) <- c("Show Plots and Map","Animal","Tag","First timestamp","Last timestamp")
      overview_named
    })
    
    selID <- reactive({
      if (is.null(input$C)) overview()$ids[1] else input$C
    })
    
    fpt_sel <- reactive({
      fpt[[selID()]]
    })
    
    data_sel <- reactive({
      data.split[[selID()]]
    })
        
    beh <- reactive({
      behav <- rep(NA,length(data_sel()))
      behav[fpt_sel()$fpt_val<input$time_thr & !is.na(fpt_sel()$fpt_val)] <- 1 #migration
      behav[fpt_sel()$fpt_val>=input$time_thr & !is.na(fpt_sel()$fpt_val)] <- 2 #resting
      time <- timestamps(data_sel())
      data.frame(behav,time)
    })

    data_sel_mig <- reactive({
      data_sel()[which(beh()$behav==1)]
    })
    
    data_sel_rest <- reactive({
      data_sel()[which(beh()$behav==2)]
    })
    
  
  output$foo <- DT::renderDataTable(
    overview_named(),
    escape = FALSE,
    selection = 'none',
    server = FALSE,
    options = list(dom = 't', paging = FALSE, ordering = FALSE, rownames=FALSE, scrollY=250, scrollCollapse = TRUE)
)
  
output$fpt <- renderPlot({
  plot(fpt_sel()$time,fpt_sel()$fpt_val,type="l",main=selID(),ylab="FPT (days)")
  points(fpt_sel()$time,fpt_sel()$fpt_val)
  abline(h=input$time_thr,col="blue",lwd=2)
})    
    
output$leafmap <- renderLeaflet({
  bounds <- as.vector(bbox(extent(data_sel())))
  outl <- leaflet() %>% 
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
    addTiles() %>%
    addProviderTiles("Esri.WorldTopoMap",group = "TopoMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
    addPolylines(data =  coordinates(data_sel()), color ="cyan", group = "lines") %>%
    addCircles(data = data_sel_mig(), fillOpacity = 0.3, opacity = 0.5, color="red", group = "migration") %>%
    addCircles(data = data_sel_rest(), fillOpacity = 0.3, opacity = 0.5, color="darkgreen", group = "resting") %>%
    addLegend(position= "topright", colors=c("cyan","red","darkgreen"), 
              labels=c("lines","migration","resting") ,opacity = 0.7, title = paste("track",unique(namesIndiv(data_sel())))) %>%
    addScaleBar(position="bottomleft", 
                options=scaleBarOptions(maxWidth = 100, 
                                        metric = TRUE, imperial = F, updateWhenIdle = TRUE)) %>%
    addLayersControl(
      baseGroups = c("StreetMap", "Aerial"),
      overlayGroups = c("lines", "migration","resting"),
      options = layersControlOptions(collapsed = FALSE)
    )
  outl    
})
  
  return(reactive({ current() }))
}

