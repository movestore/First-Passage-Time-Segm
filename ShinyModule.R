library("shiny")
library("move2")
library("sf")
<<<<<<< Updated upstream
library('ggmap')
=======
>>>>>>> Stashed changes
library('adehabitatLT')
library('leaflet')
library('DT')
library('foreach')
library("shinyBS")
library("leaflet.extras")
library("htmlwidgets")
library("mapview")
library("pals")

<<<<<<< Updated upstream
=======

>>>>>>> Stashed changes
# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the src/common/logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

shinyModuleUserInterface <- function(id, label) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- NS(id)
<<<<<<< Updated upstream
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
       
       column(3,actionButton(ns("goButton"),"Start/Update calculation")),
     ),
     div(id=ns("C"),class='shiny-input-radiogroup',DT::dataTableOutput(ns("foo"))),
     
     fluidRow(
       column(width=5, plotOutput(ns("fpt"),height="500px")),
       column(width=7, leafletOutput(ns("leafmap"),height="500px"))
     )
   )
=======
  tagList(
    titlePanel("First Passage Time Segmentation"),
    fluidRow(
      column(3,numericInput(ns("radius"), label="Radius parameter for First Passage Time (m)", value=30000),
             bsTooltip(id=ns("radius"), title="Define the radius for which you want to calculate the first passage times (when does an animal pass the radius). Unit = m.", placement = "bottom", trigger = "hover", options = list(container = "body")),
             
             numericInput(ns("time_thr"),label="Threshold time for migration/resting", value=10),
             bsTooltip(id=ns("time_thr"), title="Define the time threshold with which you will split your tracks into fast and slow movement locations. This is the minimum time your animals need to pass a given radius (above) if resting. Select your time unit below.", placement = "bottom", trigger = "hover", options = list(container = "body")),
             
             selectInput(ns("thr_uni"), label="Unit of the threshold time", choices=c("seconds", "hours","days"),selected="days"),
             bsTooltip(id=ns("thr_uni"), title="Select the time unit of your selected threshold time.", placement = "bottom", trigger = "hover", options = list(container = "body"))),

      column(3,actionButton(ns("goButton"),"Start/Update calculation",style = "color: white; background-color: orange;")),
    ),
    div(id=ns("C"),class='shiny-input-radiogroup',DT::dataTableOutput(ns("foo"))),
    
    fluidRow(
      column(width=5, plotOutput(ns("fpt"),height="500px")),
      column(width=7, leafletOutput(ns("leafmap"),height="500px")),
      column(3,downloadButton(ns('saveMap'), 'Save Map as Html'))
    )
  )
>>>>>>> Stashed changes
}

# The parameter "data" is reserved for the data object passed on from the previous app
shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  current <- reactiveVal(data)
<<<<<<< Updated upstream
  
  output$sliderOption <-renderUI({
    sliderInput(inputId = ns("time_thr"), 
                label = paste("Time threshold for passing out of",input$radius,"m radius (unit =",input$thr_uni,") to split fast movement from slow movement/resting"), 
                value = input$time_thr, min = 0, max = input$time_thr*5)
  })
=======
>>>>>>> Stashed changes
  
  currentANN <- reactiveVal() ## before hitting the go button, this object is empty, the input dataset will be saved as output
  
  observeEvent(input$goButton, {
    data.split <- split(data,mt_track_id(data))
    mfpt <- foreach(datai = data.split) %do%
      {
        datait <- st_transform(datai,crs="+proj=aeqd")
        track <- as.ltraj("xy"=data.frame("x"=st_coordinates(datait)[,1],"y"=st_coordinates(datait)[,2]),"date"=as.POSIXct(mt_time(datait)),"id"=mt_track_id(datait),"typeII"=TRUE,"proj4string"=CRS("+proj=aeqd"))
        
        f <- fpt(track,radii=input$radius,units=input$thr_uni)
        fpt_val <- f[[1]][,1]
        time <- as.POSIXct(mt_time(datait))
        data.frame(fpt_val,time)
      }
    
    names(mfpt) <- names(data.split) #careful here order between names(data.split) and mt_track_id(data) differs
    
    
    dataiANN <- foreach(datai = data.split) %do%
      {
        naam <- unique(mt_track_id(datai))
<<<<<<< Updated upstream
        datai <- cbind(datai,"fpt_value"=mfpt[[which(names(mfpt)==naam)]]$fpt_val)
=======
        datai$fpt_value <- mfpt[[which(names(mfpt)==naam)]]$fpt_val
>>>>>>> Stashed changes
        
        datai$fpt_behaviour <- NA
        datai$fpt_behaviour[datai$fpt_value>input$time_thr] <- "slow"
        datai$fpt_behaviour[datai$fpt_value<=input$time_thr] <- "fast"
        datai
      }
    
<<<<<<< Updated upstream
    currentANN(mt_stack(dataiANN,track_combine="rename"))
=======
    currentANN(mt_stack(dataiANN)) #mt_stack() runs into error if "rename"
>>>>>>> Stashed changes
    
    ids <- names(data.split)
    time0 <- foreach(datai = data.split, .combine=c) %do% {
      as.character(min(mt_time(datai)))
    }
    timeE <- foreach(datai = data.split, .combine=c) %do% {
      as.character(max(mt_time(datai)))
    }
    
    overview <- data.frame(ids,time0,timeE)
    N <- length(overview[1,])
    for (i in seq_len(nrow(overview))){
      overview[i,N+1] <- sprintf(
        '<input type="radio" name="%s" value="%s"/>',
        session$ns("C"),ids[i]
      )
      names(overview)[N+1] <- "show"
    }
    overview <- overview[,c(N+1,1:N)]
    
    
    observe({
      overview_named <- overview
      names(overview_named) <- c("Show Plots and Map","Track","First timestamp","Last timestamp")
      
      selID <- if(is.null(input$C)){overview$ids[1]} else{input$C}
      
      fpt_sel <- mfpt[[selID]]
      
      data_sel <- data.split[[selID]]
      
      behav <- rep(NA,length(data_sel))
      behav[fpt_sel$fpt_val<input$time_thr & !is.na(fpt_sel$fpt_val)] <- 1 #fast movement
      behav[fpt_sel$fpt_val>=input$time_thr & !is.na(fpt_sel$fpt_val)] <- 2 #slow movement/resting
      time <- mt_time(data_sel)
      beh <- data.frame(behav,time)
      
      data_sel_mig <- data_sel[which(beh$behav==1),]
      data_sel_rest <- data_sel[which(beh$behav==2),]
      
      
      output$foo <- DT::renderDataTable(
        overview_named,
        escape = FALSE,
        selection = 'none',
        server = FALSE,
        options = list(dom = 't', paging = FALSE, ordering = FALSE, rownames=FALSE, scrollY=250, scrollCollapse = TRUE)
      )
      
      output$fpt <- renderPlot({
        plot(fpt_sel$time,fpt_sel$fpt_val,type="l",main=selID,ylab=paste0("FPT (",input$thr_uni,")"),xlab="time")
        points(fpt_sel$time[beh==1],fpt_sel$fpt_val[beh==1],cex=2,pch=20,col="red")
        points(fpt_sel$time[beh==2],fpt_sel$fpt_val[beh==2],cex=2,pch=20,col="darkgreen")
        abline(h=input$time_thr,col="blue",lwd=2)
      })
      
<<<<<<< Updated upstream
      output$leafmap <- renderLeaflet({
=======
      mmap <- reactive({
>>>>>>> Stashed changes
        bounds <- as.vector(st_bbox(data_sel))
        outl <- leaflet() %>%
          fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
          addTiles() %>%
          addProviderTiles("Esri.WorldTopoMap",group = "TopoMap") %>%
          addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
          addPolylines(data =  st_coordinates(data_sel), color ="blue", group = "lines") %>%
          addCircles(data = data_sel_mig, fillOpacity = 0.3, opacity = 0.5, color="red", group = "fast movement") %>%
          addCircles(data = data_sel_rest, fillOpacity = 0.3, opacity = 0.5, color="darkgreen", group = "slow movement") %>%
          addLegend(position= "topright", colors=c("blue","red","darkgreen"),
                    labels=c("lines","fast movement","slow movement") ,opacity = 0.7, title = paste("track",unique(mt_track_id(data_sel)))) %>%
          addScaleBar(position="bottomleft",
                      options=scaleBarOptions(maxWidth = 100,
                                              metric = TRUE, imperial = F, updateWhenIdle = TRUE)) %>%
          addLayersControl(
            baseGroups = c("StreetMap", "Aerial"),
            overlayGroups = c("lines", "fast movement","slow movement"),
            options = layersControlOptions(collapsed = FALSE)
          )
        outl
      })
      
      output$leafmap <- renderLeaflet({
        mmap()
      })
      
      output$saveMap <- downloadHandler(
        filename = "FPT_LeafletMap.html",
        content = function(file) {
          saveWidget(
            widget = mmap(),
            file=file
          )
        })
    })
    

  })
  
  reactive({ if((length(currentANN())>=1)){return(currentANN() )}else{return(current())} })
}
