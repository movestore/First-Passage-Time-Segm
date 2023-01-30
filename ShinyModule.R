library('move')
library('ggmap')
library('adehabitatLT')
library('shiny')
library('leaflet')
library('DT')
library('foreach')
library("shinyBS")

Sys.setenv(tz="UTC") 

## improve layout of input buttons
## when indiv is selected, that it stays selected and automatically updates the plot. Started but did not find solution yet. For now all is commented


shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("First Passage Time Segmentation"),
    fluidRow(
      column(3,numericInput(ns("radius"), label="Radius parameter for First Passage Time", value=30000),
             bsTooltip(id=ns("radius"), title="Define the radius for which you want to calculate the first passage times (when does an animal pass the radius). Unit = m.", placement = "bottom", trigger = "hover", options = list(container = "body")),
             
             numericInput(ns("time_thr"),label="Threshold time for migration/resting", value=10),
             bsTooltip(id=ns("time_thr"), title="Define the time threshold with which you will split your tracks into fast and slow movement locations. This is the minimum time your animals need to pass a given radius (above) if resting. Select your time unit below.", placement = "bottom", trigger = "hover", options = list(container = "body")),
             
             selectInput(ns("thr_uni"), label="Unit of the threshold time", choices=c("seconds", "hours","days"),selected="days"),
             bsTooltip(id=ns("thr_uni"), title="Select the time unit of your selected threshold time.", placement = "bottom", trigger = "hover", options = list(container = "body"))),
      
      column(3,uiOutput(ns("sliderOption"))),
      
      column(3,actionButton(ns("goButton"),"Start calculation / Update")),
    ),
    div(id=ns("C"),class='shiny-input-radiogroup',DT::dataTableOutput(ns("foo"))),
    
    fluidRow(
      column(width=5, plotOutput(ns("fpt"),height="500px")),
      column(width=7, leafletOutput(ns("leafmap"),height="500px"))
    )
  )
}


shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  current <- reactiveVal(data)
  output$sliderOption <-renderUI({
    sliderInput(inputId = ns("time_thr"), 
                label = paste("Time threshold for passing out of",input$radius,"m radius (unit =",input$thr_uni,") to split fast movement from slow movement/resting"), 
                value = input$time_thr, min = 0, max = input$time_thr*5)
  })
  
  ## not sure if this is the most elegant way of doing it, but may work...
  currentANN <- reactiveVal() ## before hitting the go button, this object is empty, the input dataset will be saved as output
  # overview_named_r <- reactiveVal()
  # fpt_sel_r <- reactiveVal()
  # data_sel_r <- reactiveVal()
  # mfpt_r <- reactiveVal()
  # 
  observeEvent(input$goButton, {
    data.split <- move::split(data)
    mfpt <- foreach(datai = data.split) %do%
      {
        datait <- spTransform(datai,center=T)
        track <- as.ltraj("xy"=data.frame("x"=coordinates(datait)[,1],"y"=coordinates(datait)[,2]),"date"=as.POSIXct(timestamps(datait)),"id"=namesIndiv(datait),"typeII"=TRUE,"proj4string"=CRS(projection(datait)))
        
        f <- fpt(track,radii=input$radius,units=input$thr_uni)
        fpt_val <- f[[1]][,1]
        time <- as.POSIXct(timestamps(datait))
        data.frame(fpt_val,time)
      }
    
    names(mfpt) <- namesIndiv(data) 
    
    
    dataiANN <- foreach(datai = data.split) %do%
      {
        naam <- namesIndiv(datai)
        datai@data <- cbind(datai@data,"fpt_value"=mfpt[[which(names(mfpt)==naam)]]$fpt_val)
        
        datai@data$fpt_behaviour <- NA
        datai@data$fpt_behaviour[datai@data$fpt_value>input$time_thr] <- "slow"
        datai@data$fpt_behaviour[datai@data$fpt_value<=input$time_thr] <- "fast"
        datai
      }
    
    currentANN(moveStack(dataiANN,forceTz="UTC"))
    
    
    ids <- namesIndiv(data)
    tags <- foreach(datai = data.split, .combine=c) %do% {
      if(is.null(datai@idData$tag.local.identifier[1])){
        datai$tag.local.identifier[1]
      }else{
        datai@idData$tag.local.identifier[1] ### hope this takes care of the possible errors
      }
    }
    time0 <- foreach(datai = data.split, .combine=c) %do% {
      as.character(min(timestamps(datai)))
    }
    timeE <- foreach(datai = data.split, .combine=c) %do% {
      as.character(max(timestamps(datai)))
    }
    
    overview <- data.frame(ids,tags,time0,timeE)
    N <- length(overview[1,])
    for (i in seq_len(nrow(overview))){
      overview[i,N+1] <- sprintf(
        '<input type="radio" name="%s" value="%s"/>',
        session$ns("C"),ids[i]
      )
      names(overview)[N+1] <- "show"
    }
    overview <- overview[,c(N+1,1:N)]
    
    overview_named <- overview
    names(overview_named) <- c("Show Plots and Map","Animal","Tag","First timestamp","Last timestamp")
    # overview_named_r(overview_named)
    # mfpt_r(mfpt)
  # })
  
  # observeEvent(input$C, {
    selID <- if(is.null(input$C)){overview$ids[1]}else{input$C}
    
    fpt_sel <- mfpt[[selID]]
    
    data_sel <- data.split[[selID]]
    
    behav <- rep(NA,length(data_sel))
    behav[fpt_sel$fpt_val<input$time_thr & !is.na(fpt_sel$fpt_val)] <- 1 #fast movement
    behav[fpt_sel$fpt_val>=input$time_thr & !is.na(fpt_sel$fpt_val)] <- 2 #slow movement/resting
    time <- timestamps(data_sel)
    beh <- data.frame(behav,time)
    
    data_sel_mig <- data_sel[which(beh$behav==1)]
    
    data_sel_rest <- data_sel[which(beh$behav==2)]
    
    # fpt_sel_r(fpt_sel)
    # data_sel_r(data_sel)
  
  # })
  
    output$foo <- DT::renderDataTable(
      # if(input$goButton==0){data.frame(ShowPlotsAndMap=numeric(), Animal=character(), Tag=numeric,FirstTimestamp=numeric, LastTimestamp=numeric, stringsAsFactors=FALSE)}else{overview_named_r()},
      overview_named,
      escape = FALSE,
      selection = 'none',
      server = FALSE,
      options = list(dom = 't', paging = FALSE, ordering = FALSE, rownames=FALSE, scrollY=250, scrollCollapse = TRUE)
    )
    
    output$fpt <- renderPlot({
      # if(input$goButton==0){plot(1,1,type="n",xaxt='n',yaxt='n', ann=FALSE)
        # }else{
          # fpt_sel_p <- fpt_sel_r()
          fpt_sel_p <- fpt_sel
      plot(fpt_sel_p$time,fpt_sel_p$fpt_val,type="l",main=selID,ylab=paste0("FPT (",input$thr_uni,")"),xlab="time")
      points(fpt_sel_p$time[beh==1],fpt_sel_p$fpt_val[beh==1],cex=2,pch=20,col="red")
      points(fpt_sel_p$time[beh==2],fpt_sel_p$fpt_val[beh==2],cex=2,pch=20,col="darkgreen")
      abline(h=input$time_thr,col="blue",lwd=2)
      # }
    })
    
    output$leafmap <- renderLeaflet({
      # if(input$goButton==0){leaflet()
      # }else{
        # data_sel_p <- data_sel_r()
        data_sel_p <- data_sel
      bounds <- as.vector(bbox(extent(data_sel_p)))
      outl <- leaflet() %>%
        fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
        addTiles() %>%
        addProviderTiles("Esri.WorldTopoMap",group = "TopoMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
        addPolylines(data =  coordinates(data_sel_p), color ="blue", group = "lines") %>%
        addCircles(data = data_sel_mig, fillOpacity = 0.3, opacity = 0.5, color="red", group = "fast movement") %>%
        addCircles(data = data_sel_rest, fillOpacity = 0.3, opacity = 0.5, color="darkgreen", group = "slow movement") %>%
        addLegend(position= "topright", colors=c("blue","red","darkgreen"),
                  labels=c("lines","fast movement","slow movement") ,opacity = 0.7, title = paste("track",unique(namesIndiv(data_sel)))) %>%
        addScaleBar(position="bottomleft",
                    options=scaleBarOptions(maxWidth = 100,
                                            metric = TRUE, imperial = F, updateWhenIdle = TRUE)) %>%
        addLayersControl(
          baseGroups = c("StreetMap", "Aerial"),
          overlayGroups = c("lines", "fast movement","slow movement"),
          options = layersControlOptions(collapsed = FALSE)
        )
      outl
      # }
    })
  })
 
  reactive({ if((length(currentANN())>=1)){return(currentANN() )}else{return(current())} })
  
}

