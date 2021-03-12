print( R.home())
library(shinyBS)
library(DT)
library(leaflet)
library(mapview)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
library(geosphere)
library(rgeos)
library(distr)
library(sp)
library(rgdal)
library(EnvStats)
library(parsedate)
library(plotly)
library(shinyWidgets)
options(shiny.maxRequestSize=100000000000000000000*1024^2) 

function(input,output,session) {
output$whichfile <- renderUI({
  radioButtons(inputId = "whichfile",label="Would you like to add your
               own data or use CWDAT's built-in data?", choices=c("None","Mine","Built-in"),selected="None"
              )
})

output$whichfileT <- renderUI({
  radioButtons(inputId = "whichfileT",label="Would you like to add your
               own data or use CWDAT's built-in data?", choices=c("None","Mine","Built-in"),selected="None"
  )
})


  ## Process uploaded dataset (ref)
  refdata <- eventReactive(c(input$whichfile,input$rf),{
    
    if (input$whichfile == "None"){
      return(NULL)
    }
    
    else if(input$whichfile == "Mine"){
    inFileR <- input$rf 
      if (is.null(inFileR)){
      return(NULL)
    }
    
    Rdata <- read.csv(inFileR$datapath,stringsAsFactors=F)
    Rdata
    }
    else if (input$whichfile == "Built-in") {
      Rdata <- read.csv("Water_Qual_Eau_Mackenzie_2000_present.csv",stringsAsFactors = F)
      rownames(Rdata) <- NULL
      Rdata
    }
    
    
    
})
  
  observeEvent(
    refdata(), {
    gscv <<- names(refdata())[(gsc(refdata()))]
    gdcv <<- names(refdata())[(gdc(refdata()))]
    gycv <<- names(refdata())[(gyc(refdata()))]
    gxcv <<- names(refdata())[(gxc(refdata()))]
    gpcv <<- names(refdata())[(gpc(refdata()))]
    grcv <<- names(refdata())[(grc(refdata()))]
  })
  
  ## Process uploaded dataset (test)
  
  testdata <- eventReactive(c(input$whichfileT,input$tf),{
    
    if (input$whichfileT == "None"){
      return(NULL)
    }
    
    else if(input$whichfileT == "Mine"){
      inFileR <- input$tf 
      if (is.null(inFileR)){
        return(NULL)
      }
      
      Tdata <- read.csv(inFileR$datapath,stringsAsFactors=F)
      Tdata
    }
    else if (input$whichfileT == "Built-in") {
      Tdata <- read.csv("cbmgrab.csv",stringsAsFactors = F)
      Tdata
    }
    
  })
  
  observeEvent(testdata(),{
    gscvt <<- names(testdata())[(gsc(testdata()))]
    gdcvt <<- names(testdata())[(gdc(testdata()))]
    gycvt <<- names(testdata())[(gyc(testdata()))]
    gxcvt <<- names(testdata())[(gxc(testdata()))]
    gpcvt <<- names(testdata())[(gpc(testdata()))]
    grcvt <<- names(testdata())[(grc(testdata()))]
  })
  
  
  
  
  ## Preview of raw data table, to aid in column identification (ref)
  ## Reference Dataset: Data Upload and Properties
  output$rtable <- DT::renderDataTable({
    DT::datatable(refdata(),editable=FALSE,autoHideNavigation=FALSE,options = list(scrollX = TRUE,pageLength=5))
    
  })
  
  ## Preview of raw data table, to aid in column identification (test)
  ## Test Dataset: Data Upload and Properties
  output$ttable <- DT::renderDataTable({
    DT::datatable(testdata(),editable=FALSE,autoHideNavigation=FALSE,options=list(scrollX=TRUE,pageLength=5))
  })
  
  
  
  ## Select column which contains date (ref)
  ## Reference Datset: Data Upload and Properties
  output$Rdate <- renderUI({
    req(refdata())
    selectInput("rdate",label="Please select the column containing date information",
                choices=colnames(refdata()),selected=gdcv) 
  })
  
  ## Select column which contains date (test)
  ## Test Datset: Data Upload and Properties
  
  output$Tdate <- renderUI({
    req(testdata())
    selectInput("tdate",label="Please select the column containing date information",
                choices=colnames(testdata()),selected=gdcvt)
  })
  
  d <- reactive({
    dc <- which(colnames(refdata())==input$rdate)
    dc
    
  })
  
  ## get column indice value for Date (test)
  
  dt <- reactive({
    dt <- which(colnames(testdata())==input$tdate)
    dt
  })
  
  ## Select column which contains longitude (ref)
  ## Reference Dataset: Data Upload and Properties
  output$Rlong <- renderUI({
    req(refdata())
    selectInput("rlong",label="Please select the column containing longitude values",
                choices=colnames(refdata()),selected=gxcv)
  })
  
  
  ## Select column which contains longitude (test)
  ## Test Dataset: Data Upload and Properties
  output$Tlong <- renderUI({
    req(testdata())
    selectInput("tlong",label="Please select the column containing longitude values",
                choices=colnames(testdata()),selected=gxcvt)
  })
  
  ## Select column which contains latitude (ref)
  ## Reference Dataset: Data Upload and Properties
  output$Rlat <- renderUI({
    req(refdata())
    selectInput("rlat",label="Please select the column containing latitude values",
                choices=colnames(refdata()),selected=gycv) 
  })
  
  ## Select column which contains latitude (test)
  ## Test Datset: Data Upload and Properties
  
  output$Tlat <- renderUI({
    req(testdata())
    selectInput("tlat",label="Please select the column containing latitude values",
                choices=colnames(testdata()),selected=gycvt) 
  })
  
  ## Select column which contains unique site identifier (ref)
  ## Reference Dataset: Data Upload and Properties
  output$Rsid <- renderUI({
    req(refdata())
    choices <- colnames(refdata())
    selectInput("rsid",label="Please select the column containing unique site identifiers",
                choices=choices,selected=gscv) 
  })
  
  ## Select column which contains unique site identifier (test)
  ## Test Dataset: Data Upload and Properties
  output$Tsid <- renderUI({
    req(testdata())
    selectInput("tsid",label="Please select the column containing unique site identifiers",
                choices=colnames(testdata()),selected=gscvt) 
  })
  
  ######### Handling Different Formats #####
  output$Rformat <- renderUI({
    req(refdata())
    if (input$whichfile == "Mine"){
    radioButtons("rformat",label="Please select the format of your data", choices=c("Wide","Long"))
    }
    else if (input$whichfile == "Built-in"){
      radioButtons("rformat",label="Please select the format of your data", choices=c("Wide","Long"), selected = "Long") 
    }
  })
  
  output$Rpars <- renderUI({
    req(refdata())
    choices <- colnames(dplyr::select_if(refdata(),is.character))
    selectInput("rpars",label="Please select the column which contains parameter names",choices=choices,selected=gpcv)
  })
  
  output$RparsW <- renderUI({
    req(refdata())
    choices <- colnames(dplyr::select_if(refdata(),is.numeric))
    if (input$whichfile == "Mine"){
    selectInput("rparsW",label = "Please select the columns which contain observed water quality parameter values",choices=choices,multiple=T)
    }
    else if (input$whichfile== "Built-in"){
      selectInput("rparsW",label = "Please select the columns which contain observed water quality parameter values",choices=choices,multiple=T)
      
    }
    })
  
  output$Rvals <- renderUI({
    req(refdata())
    choices <- colnames(dplyr::select_if(refdata(),is.numeric))
    selectInput("rvals",label="Please select the column which contains result values",choices=choices,selected=grcv)
  })
  output$rconfirm <- renderUI({
    req(refdata())
    actionButton("rconfirm","Submit column selections")
  })
  
  observeEvent(input$rconfirm, {
    req(input$rformat,input$whichfile)
    if (input$whichfile == "Mine"){
    mess2 <- paste("You have provided your own dataset, with a",input$rformat,"data structure.")
    }
    else if (input$whichfile == "Built-in"){
      mess2 <- paste("You are using CWDAT's built-in dataset, which has a",input$rformat,"data structure.")
    }
    shinyalert("Column selections submitted",mess2, type = "success",closeOnEsc= TRUE,closeOnClickOutside=TRUE,animation=TRUE)
  })
  
  
  ## Test ##
  
  ######### Handling Different Formats #####
  output$Tformat <- renderUI({
    req(testdata())
    radioButtons("tformat",label="Please select the format of your data", choices=c("Wide","Long"))
  })
  
  output$Tpars <- renderUI({
    req(testdata())
    selectInput("tpars",label="Please select the column which contains parameter names",choices=colnames(testdata()),selected=gpcvt)
  })
  
  output$TparsW <- renderUI({
    req(testdata())
    choices <- colnames(dplyr::select_if(testdata(),is.numeric))
    if (input$whichfileT == "Mine"){
      selectInput("tparsW",label = "Please select the columns which contain observed water quality parameter values",choices=choices,multiple=T)
    }
    else if (input$whichfileT== "Built-in"){
      selectInput("tparsW",label = "Please select the columns which contain observed water quality parameter values",choices=choices,multiple=T,selected=c("pH","Turbidity","Specific_Conductance","Temperature"))
      
    }
  })
  
  output$Tvals <- renderUI({
    req(testdata())
    selectInput("tvals",label="Please select the column which contains result values",choices=colnames(testdata()),selected=grcvt)
  })
  
  output$tconfirm <- renderUI({
    req(testdata())
    actionButton("tconfirm","Submit column selections")
  })
  
  observeEvent(input$tconfirm, {
    req(input$tformat,input$whichfileT)
    if (input$whichfileT == "Mine"){
      mess2 <- paste("You have provided your own dataset, with a",input$tformat,"data structure.")
    }
    else if (input$whichfileT == "Built-in"){
      mess2 <- paste("You are using CWDAT's built-in dataset, which has a",input$tformat,"data structure.")
    }
    shinyalert("Column selections submitted",mess2, type = "success",closeOnEsc= TRUE,closeOnClickOutside=TRUE,animation=TRUE)
  })
  
  ################### Parse date column of input files once, then use resultant file for all other application########
  
  ##### Reference
  
  refdata2 <- eventReactive(input$rconfirm,{
    refdata2 <- refdata()
    refdata2 <- subset(refdata2,!is.na(refdata2[,d()]))
    refdata2[,d()] <- parse_date(refdata2[,d()])
    refdata2[,d()] <- lubridate::as_datetime(refdata2[,d()])
    refdata2
  })
  
  ###### Test
  
  testdata2 <- eventReactive(input$tconfirm,{
    testdata2 <- testdata()
    testdata2 <- subset(testdata2,!is.na(testdata2[,dt()]))
    testdata2[,dt()] <- parse_date(testdata2[,dt()])
    testdata2[,dt()] <- as_datetime(testdata2[,dt()])
    testdata2 
  })
  
 ###################### SPATIAL VISUALIZATION #############
  
############## Reference Dataset Only######################################


  output$Rmap <- renderLeaflet({
    req(input$rconfirm,input$rlong,input$rlat,input$rsid,input$rdate)
    
    leaflet() %>%
      addTiles()
  })
  
 ## Get column indice value for Longitude
 x <- reactive({
   req(input$rconfirm,input$rlong,input$rlat,input$rsid,input$rdate)
   x <- which(colnames(refdata2())==input$rlong)
   x
 })
 
 ## get column indice value for Latitude
 y <- reactive({
   req(input$rconfirm,input$rlong,input$rlat,input$rsid,input$rdate)
   y <- which(colnames(refdata2())==input$rlat)
   y
 })
 
 ## get column indice value for Site Unique Identifier
 s <- reactive({
   req(input$rconfirm,input$rlong,input$rlat,input$rsid,input$rdate)
   s <- which(colnames(refdata2())==input$rsid)
   s
   
 })
 
 sn <- reactive({
   sn <- as.character(names(refdata2()[s()]))
   sn
 })

## Get column indice value for Parameters column (conditional, long format)
 
 p <- reactive({
   req(input$rconfirm,input$rformat=="Long")
   p <- which(colnames(refdata2())==input$rpars)
   p
 })
 
## Get column indice value for Values column (conditional, long format)
 
 v <- reactive({
   req(input$rconfirm,input$rformat=="Long")
   v <- which(colnames(refdata2())==input$rvals)
   v
 })

 
  
 ## table output of filtered data
 output$table <- DT::renderDataTable({
   req(input$rconfirm)
   
   DT::datatable(refdata2(),editable=FALSE,filter="top",options=list(pageLength = 3))
 })
 
 #Get rows of datatable post-filtering
 rows <- reactive({
   req(input$table_rows_all,input$rconfirm)
 rows <- input$table_rows_all
 rows
 })

#Get new table based on rows that remain, then use
 newtable <- reactive({
   req(input$rconfirm)
   newtable <- refdata2()[rows(),]
   newtable
 })
 
 #Map part
 observe({
   req(input$rconfirm,input$table_rows_all)
   withProgress(message =
                  "Loading map - please wait",
                
                min=0,max=20,detail=NULL,
                
                
                {
                  for (i in 1:20) {
                    incProgress(1/20)
                  }
                  
   
   leafletProxy("Rmap",data=newtable()) %>%
     clearMarkers() %>%
     addMarkers(lng=~as.numeric(unique(newtable()[,x()])),lat=~as.numeric(unique(newtable()[,y()])),popup = ~as.character(unique(newtable()[,s()])),layerId=unique(newtable()[,s()])) %>%
     fitBounds(lng1 = max(newtable()[,x()],na.rm=TRUE),lat1 = max(newtable()[,y()],na.rm=TRUE),
               lng2 = min(newtable()[,x()],na.rm=TRUE),lat2 = min(newtable()[,y()],na.rm=TRUE))
                })
 })
 
 
 ## Reactive Graph
 
 output$pselect <- renderUI({
   req(input$rconfirm,input$rformat=="Wide")
   selectInput("p","Select a Parameter of Interest",choices=colnames(refdata2())[colnames(refdata2()) %in% input$rparsW])  
 })
 
 output$pselectL <- renderUI({
   req(input$rconfirm,input$rformat=="Long")
   selectInput("pL","Select a Parameter of Interest",choices=unique(refdata2()[,p()]))  
 })
 
 #  ## Save column indice value for parameter
 n <- reactive({
   req(input$rconfirm,input$rformat=="Wide")
   n <- which(colnames(refdata2())==input$p)
 })
 
 # Step 1: Copy newdata()
 
 newtable2 <- reactive({
   req(input$rconfirm)
   newtable2 <- newtable()
   newtable2
 })
 
 
 data <- reactiveValues(Clicks=list())
 

# Step 2: Filter newtable2 by clicked on site, keeping filters from newtable()
 
 observeEvent(input$Rmap_marker_click, {
   click <- input$Rmap_marker_click
   data$Clicks <- c(data$Clicks,click$id)
   
   nc <- reactive({
     nc <- as.integer(length(data$Clicks))
     nc
   })
   
   sitename <- reactive({
     sitename <- as.character(data$Clicks[[nc()]])
     sitename
   })
   
   psitename <- reactive({
     if (nc() > 1){
     psitename <- as.character(data$Clicks[[nc()-1]])
     psitename
     }
     else{
       return()
     }
   })
   
   

   output$mplot <- renderPlotly({
     (req(input$rconfirm))
     
     if(input$rformat=="Wide") ## Wide format data
     
       {
     
     df <- newtable()
     
     if(sitename() %in% unique(df[,s()]))
     {
       df <- subset(df,df[,s()]==sitename())
       sn <- sitename()
       plot <- ggplot(data=df,aes(x=df[,d()],y=df[,n()])) + geom_point() +theme_bw() + ggtitle(sn) + xlab("Date") + ylab(as.character(input$p))
       # if (input$addguide=="Yes"){
       #   #if () {
       #     plot <- plot + geom_hline(yintercept=uL(),linetype="dashed")
       #   #}
       #   if (!is.null(lL())){
       #     plot <- plot + geom_hline(yintercept=lL(),linetype="dashed")
       #   }
       # }
       plot <- ggplotly(plot,dynamicTicks=TRUE)
       plot
     }
     else if (nc() > 1) {
       df <- subset(df,df[,s()]==psitename())
       sn <- psitename()
       plot <- ggplot(data=df,aes(x=df[,d()],y=df[,n()])) + geom_point() +theme_light() + ggtitle(sn) + xlab("Date") + ylab(as.character(input$p))
       # if (input$addguide=="Yes"){
       #   if (!is.null(uL())){
       #     plot <- plot + geom_hline(yintercept=uL(),linetype="dashed")
       #   }
       #   if (!is.null(lL())){
       #     plot <- plot + geom_hline(yintercept=lL(),linetype="dashed")
       #   }
       # }
       plot <- ggplotly (plot,dynamicTicks=TRUE)
       plot
     }
     else {
       return()
     }
     
     } ## End of plot render in the case of wide format data
     
     else {
       
       df <- newtable()
       ## Filter paras column to only selected parameter (input$p)
       df <- subset(df,df[,p()]==input$pL)
       
       if(sitename() %in% unique(df[,s()]))
       {
         df <- subset(df,df[,s()]==sitename())
         sn <- sitename()
         plot <- ggplot(data=df,aes(x=df[,d()],y=df[,v()])) + geom_point() +theme_bw() + ggtitle(sn) + xlab("Date") + ylab(as.character(input$pL))
         # if (input$addguide=="Yes"){
         #   if (!is.null(uL())){
         #     plot <- plot + geom_hline(yintercept=uL(),linetype="dashed")
         #   }
         #   if (!is.null(lL())){
         #     plot <- plot + geom_hline(yintercept=lL(),linetype="dashed")
         #   }
         # }
         plot <- ggplotly(plot,dynamicTicks=TRUE)
         plot
       }
       else if (nc() > 1) {
         df <- subset(df,df[,s()]==psitename())
         sn <- psitename()
         plot <- ggplot(data=df,aes(x=df[,d()],y=df[,v()])) + geom_point() +theme_light() + ggtitle(sn) + xlab("Date") + ylab(as.character(input$pL))
         # if (input$addguide=="Yes"){
         #   if (!is.null(uL())){
         #     plot <- plot + geom_hline(yintercept=uL(),type="dashed")
         #   }
         #   if (!is.null(lL())){
         #     plot <- plot + geom_hline(yintercept=lL(),type="dashed")
         #   }
         # }
         plot <- ggplotly (plot,dynamicTicks=TRUE)
         plot
       }
       else {
         return()
       }
       
       
       
     }
     
     
     
   })
   
 })
   
 
 output$SpatVisreport <- downloadHandler(
   
   
   # For PDF output, change this to "report.pdf"
   filename = "SpatialVis.pdf",
   content = function(file) {
     
     iterations <- unique(newtable()[,s()])
     
     for (s in interations){
       path <- paste0(iterations,".pdf")
     }
     
     
     withProgress(message = 'Preparing your report - please wait!',{
       # Copy the report file to a temporary directory before processing it, in
       # case we don't have write permissions to the current working dir (which
       # can happen when deployed).
       tempReport <- file.path(tempdir(), "SpatVistemplate.Rmd")
       file.copy("SpatVistemplate.Rmd", tempReport, overwrite = TRUE)
       
       if (inpur$rformat == "Long"){
         varsCol <- input$rvars
         parsCol <- input$rpars
       }
       else if (input$rformat == "Wide"){
         parsCol <- input$rparsW
         varsCol <- NA
       }
       
       # Set up parameters to pass to Rmd document
       params <- list(data = data.frame(newtable()), siteCol = s(), dateCol = d(),varsCol=varsCol,parsCol = parsCol, dataFormat = input$rformat,Sites=input$rsid,latCol = y(), longCol = x(), rendered_by_shiny=TRUE)
       
       # Knit the document, passing in the `params` list, and eval it in a
       # child of the global environment (this isolates the code in the document
       # from the code in this app).
       rmarkdown::render(tempReport, output_file = file,
                         params = params,
                         envir = new.env(parent = globalenv())
       )
     })
   }
   
 )
   
 

 
#################### Test Dataset Only#####################################################################
## Test Data: Spatial Visualization


output$Tmap <- renderLeaflet({
  req(input$tconfirm,input$tlong,input$tlat,input$tsid,input$tdate)
  leaflet() %>%
    addTiles()
})

## Get column indice value for Longitude
xt <- reactive({
  xt <- which(colnames(testdata())==input$tlong)
  xt
})

## get column indice value for Latitude
yt <- reactive({
  yt <- which(colnames(testdata())==input$tlat)
  yt
})

## get column indice value for Site Unique Identifier
st <- reactive({
  st <- which(colnames(testdata())==input$tsid)
  st
  
})

snt <- reactive({
  snt <- as.character(names(testdata2()[s()]))
  snt
})

## Get column indice value for Parameters column (conditional, long format), test data

pt <- reactive({
  req(input$tconfirm,input$tformat=="Long")
  pt <- which(colnames(testdata2())==input$tpars)
  pt
})

## Get column indice value for Values column (conditional, long format), test data

vt <- reactive({
  req(input$tconfirm,input$tformat=="Long")
  vt <- which(colnames(testdata2())==input$tvals)
  vt
})


## table output of filtered data
output$tablet <- DT::renderDataTable({
  req(input$tconfirm)
  DT::datatable(testdata2(),editable=FALSE,filter="top",options=list(pageLength=3))
})

## get rows of datable post filtering
rowst <- reactive({
  req(input$tablet_rows_all)
  rowst <- input$tablet_rows_all
  rowst
})

## get new table based on rowst that remain, then use
newtablet <- reactive({
  req(input$tconfirm,input$tlong,input$tlat,input$tdate,input$tsid)
  newtablet <- testdata2()[rowst(),]
  newtablet
})


# Map part
observe({
  req(input$tconfirm,input$tablet_rows_all)
    
    withProgress(message =
                   "Loading map - please wait",
                 
                 min=0,max=20,detail=NULL,
                 
                 
                 {
                   for (i in 1:20) {
                     incProgress(1/20)
                   }
  
  leafletProxy("Tmap",data=newtablet()) %>%
    clearMarkers() %>%
    addMarkers(lng=~as.numeric(unique(newtablet()[,xt()])),lat=~as.numeric(unique(newtablet()[,yt()])),popup = ~as.character(unique(newtablet()[,st()])),layerId=unique(newtablet()[,st()])) %>%
    fitBounds(lng1 = max(newtablet()[,xt()],na.rm=TRUE),lat1 = max(newtablet()[,yt()],na.rm=TRUE),
              lng2 = min(newtablet()[,xt()],na.rm=TRUE),lat2 = min(newtablet()[,yt()],na.rm=TRUE))
                 })
  
})


## Reactive Graph


output$pselectTw <- renderUI({
  req(input$tconfirm,input$tformat=="Wide")
  selectInput("pt","Select a Parameter of Interest",choices=colnames(testdata2()),selected=5)  
})

output$pselectTL <- renderUI({
  req(input$tconfirm,input$tformat=="Long")
  selectInput("ptL","Select a parameter of interest", choices=unique(testdata2()[,pt()]))
})

#  ## Save column indice value for parameter
nt <- reactive({
  nt <- which(colnames(testdata2())==input$pt)
})

# Step 1: Copy newtablet
newtablet2 <- reactive({
  req(input$tconfirm,input$tlong,input$tlat,input$tsid,input$tdate)
  newtablet2 <- newtablet()
  newtablet2
})

datat <- reactiveValues(Clicks=list())

# Step 2: Filter newtablet2 by clicked-on site, keeping filters from newtablet()

observeEvent(input$Tmap_marker_click,{
  clickt <- input$Tmap_marker_click
  datat$Clicks <- c(datat$Clicks,clickt$id)
  
  nct <- reactive({
    nct <- as.integer(length(datat$Clicks))
    nct
  })
  
  sitenamet <- reactive({
    sitenamet <- as.character(datat$Clicks[[nct()]])
    sitenamet
  })
  
  
  psitenamet <- reactive({
    if (nct() > 1){
      psitenamet <- as.character(datat$Clicks[[nct()-1]])
      psitenamet
    }
    else{
      return()
    }
  })
  
  output$mplott <- renderPlotly({
    (req(input$tconfirm))
    
    if (input$tformat=="Wide")### Wide Data Format
      
      { 
    
    dft <- newtablet()
    
    if(sitenamet() %in% unique(dft[,st()]))
    {
      dft <- subset(dft,dft[,st()]==sitenamet())
      snt <- sitenamet()
      plott <- ggplot(data=dft,aes(x=dft[,dt()],y=dft[,nt()])) + geom_point() +theme_bw() + ggtitle(snt) + xlab("Date") + ylab(as.character(input$pt))
      plott <- ggplotly(plott,dynamicTicks=TRUE)
      plott
    }
    else if (nct() > 1) {
      dft <- subset(dft,dft[,st()]==psitenamet())
      snt <- psitenamet()
      plott <- ggplot(data=dft,aes(x=dft[,dt()],y=dft[,nt()])) + geom_point() +theme_light() + ggtitle(snt) + xlab("Date") + ylab(as.character(input$pt))
      plott <- ggplotly (plott,dynamicTicks=TRUE)
      plott
    }
    else {
      return()
    }
    } ## End of plot render in the case of wide format data
    
    else {
      dft <- newtablet()
      
      ## Filter paras column to only selected parameter (input$ptL)
      dft <- subset(dft,dft[,pt()]==input$ptL)
      
      if(sitenamet() %in% unique(dft[,st()]))
      {
        dft <- subset(dft,dft[,st()]==sitenamet())
        snt <- sitenamet()
        plott <- ggplot(data=dft,aes(x=dft[,dt()],y=dft[,vt()])) + geom_point() +theme_bw() + ggtitle(snt) + xlab("Date") + ylab(as.character(input$ptL))
        plott <- ggplotly(plott,dynamicTicks=TRUE)
        plott
      }
      else if (nct() > 1) {
        dft <- subset(dft,dft[,st()]==psitenamet())
        snt <- psitenamet()
        plott <- ggplot(data=dft,aes(x=dft[,dt()],y=dft[,vt()])) + geom_point() +theme_light() + ggtitle(snt) + xlab("Date") + ylab(as.character(input$ptL))
        plott <- ggplotly (plott,dynamicTicks=TRUE)
        plott
      }
      else {
        return()
      }
      
    }
    
    
  })
  
})


######################## Reference Datset - Graphic Visualizations #######################################

### recall that x() is Longitude indice, y() is Latitude indice, d() is date indice, and s() is site indice from the uploaded dataset called refdata()

## Select plot type
## Select which lines to display
## Select parameter, select sites, select year and/or month(s)
## keep a map to show all of them? still to be done

## Create a copy of refdata() that is sorted alphabetically by site unique identifier
refdatasalpha <- reactive({
  refdatasalpha <- refdata2()
  refdatasalpha <- subset(refdatasalpha,!is.na(refdatasalpha[,d()]))
  refdatasalpha <- arrange(refdatasalpha,refdatasalpha[,s()])
  refdatasalpha
})

## Selection of sites to visualize
output$selectsites <- renderUI({
  pickerInput("selsite","Select site(s)",choices=unique(refdatasalpha()[,s()]),inline=FALSE,multiple=TRUE,
              selected=unique(refdatasalpha()[,s()]),
              options = list(
    `actions-box` = TRUE,
    `deselect-all-text` = "Deselect all",
    `select-all-text` = "Select All",
    `none-selected-text` = "No sites selected",
    `selected-text-format`= "count",
    `count-selected-text` = "{0} of {1} sites selected"
  ))
})

## Selection of variable one (y) for temporal page: reference data
output$cselect1 <- renderUI({
  req(input$rconfirm)
  if (input$rformat=="Wide"){
    choices=colnames(refdata2()[,(colnames(refdata2()) %in% input$rparsW)])
  }
  else if (input$rformat=="Long"){
    choices=unique(refdata2()[,p()])
  }
  
  selectInput("c1","Select variable to display",choices=choices)  ## Could edit this to remove lat and long
})

## Get y column indice
cy <- reactive({
  req(input$rformat=="Wide")
  co1 <- which(colnames(refdata2())==input$c1)
  co1
  
})


## Selection of variable 2 (x) for temporal page: reference data
output$cselect2 <- renderUI({
  #req(input$rconfirm)
  if (input$rformat=="Wide"){
    choices=colnames(refdata2())
  }
  else if (input$rformat=="Long"){
    choices=c(unique(refdata2()[,p()]),colnames(refdata2())[-v()])
  }
  selectInput("c2","Select variable to display on X axis",choices=choices,selected=1)

})

## Get x column indice
cx <- reactive({
  req(input$rformat=="Wide")
  co2 <- which(colnames(refdata())==input$c2)
  co2
  
})

### Parse Dates from original reference file for use in all graphical outputs.
## Also need to account for selection of date format and parse accordingly
## Arrange by month for month input, arrange by year for year input
refdatadam <- reactive({
  req(input$rconfirm,input$rdate)
  # Make a usable copy of the original refdata
  refdatan <- refdata2()
  refdatan$Month <- month(refdatan[,d()]) 
  refdatan$Monthn <- month.name[refdatan$Month]
  refdatan$Year <- year(refdatan[,d()])
  refdatan <- arrange(refdatan,Month)
  refdatan
})

#####

refdataday <- reactive({
  req(input$rconfirm,input$rdate)
  # Make a usable copy of the original refdata
  refdatan <- refdata2()
  refdatan$Month <- month(refdatan[,d()]) 
  refdatan$Monthn <- month.name[refdatan$Month]
  refdatan$Year <- year(refdatan[,d()])
  refdatan <- arrange(refdatan,Year)
  refdatan
})



## Use refdatad to get date ranges
output$filtermonth <- renderUI({
  req(input$rconfirm,input$rdate)
  pickerInput("filtermonth","Select month(s)",choices=unique(refdatadam()$Monthn),inline=FALSE,multiple=TRUE,
              selected=unique(refdatadam()$Monthn),options = list(
    `actions-box` = TRUE,
    `deselect-all-text` = "Deselect all",
    `select-all-text` = "Select All",
    `none-selected-text` = "No months selected",
    `selected-text-format`= "count",
    `count-selected-text` = "{0} of {1} months selected"
  ))
})

output$filteryear <- renderUI({
  req(input$rconfirm,input$rdate)
  pickerInput("filteryear","Select year(s)",choices=unique(refdataday()$Year),inline=FALSE,multiple=TRUE,
              selected=unique(refdataday()$Year),
              options = list(
    `actions-box` = TRUE,
    `deselect-all-text` = "Deselect all",
    `select-all-text` = "Select All",
    `none-selected-text` = "No years selected",
    `selected-text-format`= "count",
    `count-selected-text` = "{0} of {1} years selected"
  ))
})


### Filter data based on user inputs to generate final dataframe for plotting
refdatadf <- reactive({
  #req(input$rconfirm, input$rdate,input$c1,input$selsite)
  #input$refresh
  refdatadf <- refdataday() 
  refdatadf <- subset(refdatadf,refdatadf$Monthn %in% input$filtermonth & (refdatadf$Year %in% input$filteryear) & (refdatadf[,s()] %in% input$selsite))
  refdatadf
})

#################################################
output$units <- renderUI({
  textInput("units", "Enter units")
})
##############################################

## Additional units input for bivariate instances
output$units2x <- renderUI({
  textInput("units2x","Enter the units for the x axis variable")
})

output$units2y <- renderUI({
  textInput("units2y","Enter the units for the y axis variable")
})

###################################################


### Ability to download the generated graph
output$downloadPlot <- downloadHandler(
  filename = "Shinyplot.png",
  content = function(file) {
    ggsave(file, plot = plotInput(), device = "png")
  })

#### Want to do single and double variable plots, with graph options depending on which one you would use
## just need to do single variable, fix the plot type


### One variable selected: input$vars==1
output$refout <- renderPlot({
  print(plotInput())
})


plotInput <- reactive({
  # input$refresh
  # if (input$refresh == 0)
  #   return()
   if (input$vars==1){
  
  #isolate(
    ##################### Single Variable -- Histogram
    
  if (input$ptype1 == "Histogram") {
    # input$refresh
    # bin <- reactive({
    #   input$refresh
    #   b <- isolate(input$binsize)
    #   b
    # })
    
    title <- reactive({
      #input$refresh
      t <-as.character(input$title)
      t
    })
    
    xl <- reactive({
      #input$refresh
      xl <- paste(as.character(colnames(refdata()[cy()]))," (",as.character(input$units),")") ## This works, y axis label is 'count'
    })
    
    if (input$rformat=="Wide"){
    
    ggplot(refdatadf(),aes(refdatadf()[,cy()])) + geom_histogram() + ggtitle(title()) + theme_bw() + xlab(xl())
    }
    
    else if (input$rformat=="Long"){
      
      refdatadf2 <- subset(refdatadf(),refdatadf()[,p()]==input$c1)
      
      ggplot(refdatadf2,aes(refdatadf2[,v()])) + geom_histogram() + ggtitle(title()) + theme_bw() + xlab(as.character(input$c1)) 
      
    }
    
  }
    
  
   ############## Single Variable -- Density 
  
  
  else if(input$ptype1=="Density") {
    #input$refresh
    title <- reactive({
      #input$refresh
      t <- as.character(input$title)
      t
    })
    
    xl <- reactive({
      #input$refresh
      xl <- paste(as.character(colnames(refdata()[cy()]))," (",as.character(input$units),")") ## this is fine because y label is 'density'
    })
    
    if (input$rformat=="Wide"){
    
    ggplot(refdatadf(),aes(refdatadf()[,cy()])) + geom_density(kernel="gaussian") + ggtitle(title()) + theme_bw() + xlab(xl())
    }
    
    else if (input$rformat=="Long"){
      
      refdatadf2 <- subset(refdatadf(),refdatadf()[,p()]==input$c1)
      ggplot(refdatadf2,aes(refdatadf2[,v()])) + geom_density(kernel="gaussian") + ggtitle(title()) + theme_bw() + xlab(as.character(input$c1))
    }
    
    
    
  }
  
  ###################### Single Variable -- Discrete Bar Plot
    
    
  else if (input$ptype1=="Discrete Bar Plot")  {
    #input$refresh
    
    
    title <- reactive({
      #input$refresh
      t <- as.character(input$title)
      t
    })
    
    xl <- reactive({
      #input$refresh
      xl <- paste(as.character(colnames(refdata()[cy()]))," (",as.character(input$units),")") ## this is fince because y label is 'count'
    })
    
    
    cwi <- reactive({
      #input$refresh
      wid <- input$width
      wid
    })
    
    if (input$rformat=="Wide"){
    
    ggplot(refdatadf(),aes(refdatadf()[,cy()])) + geom_bar(width = cwi(), position = position_dodge(width = cwi())) + ggtitle(title()) + theme_bw() + xlab(xl()) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
    
    }
    
    else if (input$rformat=="Long"){
      refdatadf2 <- subset(refdatadf(),refdatadf()[,p()]==input$c1)
      ggplot(refdatadf2,aes(refdatadf2[,v()])) + geom_bar(width = cwi(), position = position_dodge(width = cwi())) + ggtitle(title()) + theme_bw() + xlab(as.character(input$c1)) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
      
    }
      }
  ########## Single Variable - Boxplot
  
  else {
    
    title <- reactive({
      #input$refresh
      t <- as.character(input$title)
      t
    })
    
    yl <- reactive({
      #input$refresh
      yl <- paste(as.character(colnames(refdata()[cy()]))," (",as.character(input$units),")") ###### y label only for boxplot
    })
    
    if (input$rformat=="Wide"){
    boxplot(refdatadf()[,cy()],main=title(),xlab="",ylab=yl())
    }
    else if (input$rformat=="Long"){
      refdatadf2 <- subset(refdatadf(),refdatadf()[,p()]==input$c1)
      boxplot(refdatadf2[,v()],main=title(),xlab="",ylab=input$c1)
    }
    
  }
    
    #)
  } #### END OF UNIVARIATE CODE, START OF BIVARIATE OPTIONS ######################################
  else {
    #input$refresh
    #isolate(
      
      
      if(input$ptype2 == "Point"){
        
        title <- reactive({
          #input$refresh
          t <- as.character(input$title)
          t
        })
        
        xl <- reactive({
          #input$refresh
          if (cx()==d()) {
            xl <- as.character(colnames(refdata()[cx()]))  
          }
          else {
          xl <- paste(as.character(colnames(refdata()[cx()]))," (",as.character(input$units2x),")")
          }
          xl
        })
        
        yl <- reactive({
          #input$refresh
        yl <- paste(as.character(colnames(refdata()[cy()]))," (",as.character(input$units2y),")")
        })
        
        if (input$rformat=="Wide"){
        
       ggplot(refdatadf(),aes(x=refdatadf()[,cx()],y=refdatadf()[,cy()])) + theme_bw() + ggtitle(title()) +geom_point(aes(colour=refdatadf()[,s()]),size=3) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + labs(colour="Site\n") + xlab(input$c2) + ylab(input$c1)
        }
        
        else if (input$rformat=="Long"){
          
          
          
          if (input$c2 %in% colnames(refdatadf())){
            refdatadf2 <- subset(refdatadf(),refdatadf()[,p()]==input$c1)
            cx <- which(colnames(refdatadf2)==input$c2)
            ggplot(refdatadf2,aes(x=refdatadf2[,cx],y=refdatadf2[,v()])) + theme_bw() + ggtitle(title()) +geom_point(aes(colour=refdatadf2[,s()]),size=3) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + labs(colour="Site\n") + xlab(input$c2) + ylab(input$c1)
          }
          
          else{
            
            refdatadfp1 <- subset(refdatadf(),refdatadf()[,p()]==input$c1)
            refdatadfp1 <- refdatadfp1[,c(d(),s(),v())]
            refdatadfp2 <- subset(refdatadf(),refdatadf()[,p()]==input$c2)
            refdatadfp2 <- refdatadfp2[,c(d(),s(),v())]
            refdatadf3 <- left_join(refdatadfp1,refdatadfp2,by=input$rdate)
            ggplot(refdatadf3,aes(x=refdatadf3[,5],y=refdatadf3[,3])) + theme_bw() + ggtitle(title()) +geom_point(aes(colour=refdatadf3[,2]),size=3) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + labs(colour="Site\n") + xlab(input$c2) + ylab(input$c1)
          
          }
          
        }
    
      }
    
      
      
      else if(input$ptype2=="Line") {
        #input$refresh
        
        
        
        title <- reactive({
          input$refresh
          t <- as.character(input$title)
          t
        })
        
        xl <- reactive({
          #input$refresh
          if (cx()==d()) {
            xl <- as.character(colnames(refdata()[cx()]))  
          }
          else {
            xl <- paste(as.character(colnames(refdata()[cx()]))," (",as.character(input$units2x),")")
          }
          xl
        })
        
        yl <- reactive({
          #input$refresh
          yl <- paste(as.character(colnames(refdata()[cy()]))," (",as.character(input$units2y),")")
        })
        
        if (input$rformat=="Wide"){
        
          ggplot(refdatadf(),aes(x=refdatadf()[,cx()],y=refdatadf()[,cy()])) + theme_bw() + ggtitle(title()) +geom_line(aes(colour=refdatadf()[,s()]),size=1.5) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab(xl()) + ylab(yl()) + labs(colour="Site\n")
        }
        
        else if (input$rformat=="Long"){
          
          if (input$c2 %in% colnames(refdatadf())){
            refdatadf2 <- subset(refdatadf(),refdatadf()[,p()]==input$c1)
            cx <- which(colnames(refdatadf2)==input$c2)
            ggplot(refdatadf2,aes(x=refdatadf2[,cx],y=refdatadf2[,v()])) + theme_bw() + ggtitle(title()) +geom_line(aes(colour=refdatadf2[,s()])) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + labs(colour="Site\n") + xlab(input$c2) + ylab(input$c1)
          }
          
          else{
            
            refdatadfp1 <- subset(refdatadf(),refdatadf()[,p()]==input$c1)
            refdatadfp1 <- refdatadfp1[,c(d(),s(),v())]
            refdatadfp2 <- subset(refdatadf(),refdatadf()[,p()]==input$c2)
            refdatadfp2 <- refdatadfp2[,c(d(),s(),v())]
            refdatadf3 <- left_join(refdatadfp1,refdatadfp2,by=input$rdate)
            ggplot(refdatadf3,aes(x=refdatadf3[,5],y=refdatadf3[,3])) + theme_bw() + ggtitle(title()) +geom_line(aes(colour=refdatadf3[,2])) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + labs(colour="Site\n") + xlab(input$c2) + ylab(input$c1)
        }
        
        }
      }
      
      else if(input$ptype2=="Discrete Box Plot"){ ########### Discrete Box Plot
        #input$refresh
        
        # validate(
        #   need(class(refdatadf()[,cx()])=="character"|class(refdatadf()[,cx()])=="factor","Please select a discrete variable to display on the x axis")
        # )
        
        title <- reactive({
          #input$refresh
          t <- as.character(input$title)
          t
        })
        
        xl <- reactive({
          #input$refresh
          xl <- as.character(colnames(refdata()[cx()]))
          xl
        })
        
        yl <- reactive({
          #input$refresh
          yl <- paste(as.character(colnames(refdata()[cy()]))," (",as.character(input$units2y),")")
        })
        
        if (input$rformat=="Wide"){
        
        ggplot(data=refdatadf(),aes(x=refdatadf()[,cx()],y=refdatadf()[,cy()])) + theme_bw() + ggtitle(title()) +geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab(xl()) + ylab(yl())
        }
        
        else if (input$rformat=="Long"){
          
          if (input$c2 %in% colnames(refdatadf())){
            refdatadf2 <- subset(refdatadf(),refdatadf()[,p()]==input$c1)
            cx <- as.integer(which(colnames(refdatadf2)==input$c2))
            ggplot(data=refdatadf2,aes(x=refdatadf2[,cx],y=refdatadf2[,v()])) + theme_bw() + ggtitle(title()) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab(input$c2) +ylab(input$c1)
            
          }
          
          else{
            
            refdatadfp1 <- subset(refdatadf(),refdatadf()[,p()]==input$c1)
            refdatadfp1 <- refdatadfp1[,c(d(),s(),v())]
            refdatadfp2 <- subset(refdatadf(),refdatadf()[,p()]==input$c2)
            refdatadfp2 <- refdatadfp2[,c(d(),s(),v())]
            refdatadf3 <- left_join(refdatadfp1,refdatadfp2,by=input$rdate)
            ggplot(refdatadf3,aes(x=refdatadf3[,5],y=refdatadf3[,3])) + theme_bw() + ggtitle(title()) +geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab(input$c2) + ylab(input$c1)
            
          }
          
        }
          
        }
      
      
  
  
  #) ## end of isoalte function for bivariate operations
  } ## end of if/else options for bivariate visualization
  

}) ## end of RenderPLot script for reference data main graphic output


#####################################################################################


######################## Test Datset - Graphic Visualizations #######################################

### recall that xt() is Longitude indice, yt() is Latitude indice, dt() is date indice, and st() is site indice from the uploaded dataset called testdata2()

## Select plot type
## Select which lines to display
## Select parameter, select sites, select year and/or month(s) <- would be best to use radio buttons for this
## keep a map to show all of them?

## Selection of variable one (y/univariate)

## create a copy of testdata() that is sorted alphabetically by site unique identifier
testdatasalpha <- reactive({
  testdatasalpha <- testdata2()
  testdatasalpha <- arrange(testdatasalpha,testdatasalpha[,st()])
  testdatasalpha
})

## Selection of sites to visualize
output$selectsitest <- renderUI({
  pickerInput("selsitet","Select site(s)",choices=unique(testdatasalpha()[,st()]),inline=FALSE,multiple=TRUE,
              selected=unique(testdatasalpha()[,st()]),
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "Deselect all",
                `select-all-text` = "Select All",
                `none-selected-text` = "No sites selected",
                `selected-text-format`= "count",
                `count-selected-text` = "{0} of {1} sites selected"
              ))
})

## Selection of variable 1 (y), test data
output$cselect1t <- renderUI({
  req(input$tconfirm)
  if (input$tformat=="Wide"){
    choices=choices=colnames(testdata2()[,(colnames(testdata2()) %in% input$tparsW)])
  }
  else if (input$tformat=="Long"){
    choices=unique(testdata2()[,pt()])
  }
  
  selectInput("c1t","Select variable to display (Y)",choices=choices,selected=5)  ##
})

## Get y column indice
cyt <- reactive({
  #input$refresht
  req(input$tformat=="Wide")
  co1 <- which(colnames(testdata2())==input$c1t)
  co1
  
})


## Selection of variable 2 (x) for temporal page: test data
output$cselect2t <- renderUI({
  req(input$tconfirm)
  if (input$tformat=="Wide"){
    choices=colnames(testdata2())
  }
  else if (input$tformat=="Long"){
    choices=c(unique(testdata2()[,pt()]),colnames(testdata2())[-vt()])
  }
  selectInput("c2t","Select variable to display on X axis",choices=choices,selected=5)
  
})

## Get x column indice
cxt <- reactive({
  req(input$tformat=="Wide")
  #input$refresht
  co2 <- which(colnames(testdata())==input$c2t)
  co2
  
})

### Parse Dates from original file for use in all graphical outputs.
testdatadamt <- reactive({
  req(input$tconfirm,input$tdate)
  testdatan <- testdata2()
  testdatan$Month <- month(testdatan[,dt()])
  testdatan$Monthn <- month.name[testdatan$Month]
  testdatan$Year <- year(testdatan[,dt()])
  testdatan <- arrange(testdatan,Month)
  testdatan
})

#####

testdatadayt <- reactive({
  req(input$tconfirm,input$tdate)
  testdatan <- testdata2()
  testdatan$Month <- month(testdatan[,dt()])
  testdatan$Monthn <- month.name[testdatan$Month]
  testdatan$Year <- year(testdatan[,dt()])
  testdatan <- arrange(testdatan,Year)
  testdatan
})



## Use testdatadt to get date ranges
output$filtermontht <- renderUI({
  req(input$tconfirm,input$tdate)
  pickerInput("filtermontht","Select month(s)",choices=unique(testdatadamt()$Monthn),inline=FALSE,multiple=TRUE,
              selected=unique(testdatadamt()$Monthn),options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "Deselect all",
                `select-all-text` = "Select All",
                `none-selected-text` = "No months selected",
                `selected-text-format`= "count",
                `count-selected-text` = "{0} of {1} months selected"
              ))
  
  #selectInput("filtermontht","Show data from the selected month(s)",choices=unique(testdatadamt()$Monthn),multiple=TRUE)
})

output$filteryeart <- renderUI({
  req(input$tconfirm,input$tdate)
  pickerInput("filteryeart","Select year(s)",choices=unique(testdatadayt()$Year),inline=FALSE,multiple=TRUE,
              selected=unique(testdatadayt()$Year),
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "Deselect all",
                `select-all-text` = "Select All",
                `none-selected-text` = "No years selected",
                `selected-text-format`= "count",
                `count-selected-text` = "{0} of {1} years selected"
              ))
  
  #selectInput("filteryeart","Show data from the selected year(s)",choices=unique(testdatadayt()$Year),multiple=TRUE)
})


### Filter data based on user inputs to generate final dataframe for plotting
testdatadft <- reactive({
  req(input$tconfirm, input$tdate,input$c1t)
  #input$refresht
  testdatadft <- testdatadayt()
  testdatadft <- subset(testdatadft,testdatadft$Monthn %in% input$filtermontht & (testdatadft$Year %in% input$filteryeart) & (testdatadft[,st()] %in% input$selsitet))
  testdatadft
})


output$unitst <- renderUI({
  textInput("unitst", "Enter units")
})

## Additional units input for bivariate instances
output$units2xt <- renderUI({
  textInput("units2xt","Enter the units for the x axis variable")
})

output$units2yt <- renderUI({
  textInput("units2yt","Enter the units for the y axis variable")
})


### Ability to download the generated graph
output$downloadPlott <- downloadHandler(
  filename = "Shinyplot.png",
  content = function(file) {
    ggsave(file, plot = plotInputt(), device = "png")
  })

#### Want to do single and double variable plots, with graph options depending on which one you would use
## just need to do single variable, fix the plot type


### One variable selected: input$vars==1
output$refoutt <- renderPlot({
  print(plotInputt())
})


plotInputt <- function(){
  #input$refresht
  #if (input$refresht == 0)
    #return()
  if (input$varst==1){
    
    #isolate(
      
      if (input$ptype1t == "Histogram") { ####### Single Variable: Histogram
        #input$refresht
        
        titlet <- reactive({
          #input$refresht
          t <- as.character(input$titlet)
          t
        })
        
        xlt <- reactive({
          #input$refresht
          xlt <- paste(as.character(colnames(testdata()[cyt()]))," (",as.character(input$unitst),")")
        })
        
        if (input$tformat=="Wide"){
        
        ggplot(testdatadft(),aes(testdatadft()[,cyt()])) + geom_histogram() + ggtitle(titlet()) + theme_bw() + xlab(xlt())
        }
        else if (input$tformat=="Long"){
          testdatadft2 <- subset(testdatadft(),testdatadft()[,pt()]==input$c1t)
          ggplot(testdatadft2,aes(testdatadft2[,vt()])) + geom_histogram() + ggtitle(titlet()) + theme_bw() + xlab(as.character(input$c1t))
        }
        
      }
      
      
      
      
      
      else if(input$ptype1t=="Density") { ######### Single Variable: Density
        #input$refresht
        
        
        
        titlet <- reactive({
          #input$refresht
          t <- as.character(input$titlet)
          t
        })
        
        xlt <- reactive({
          #input$refresht
          xlt <- paste(as.character(colnames(testdata()[cyt()]))," (",as.character(input$unitst),")")
        })
        
        if (input$tformat=="Wide"){
        
        ggplot(testdatadft(),aes(testdatadft()[,cyt()])) + geom_density(kernel="gaussian") + ggtitle(titlet()) + theme_bw() + xlab(xlt()) 
        }
        
        else if (input$tformat=="Long"){
          testdatadft2 <- subset(testdatadft(),testdatadft()[,pt()]==input$c1t)
          ggplot(testdatadft2,aes(testdatadft2[,vt()])) + geom_density(kernel="gaussian") + ggtitle(titlet()) + theme_bw() + xlab(as.character(input$c1)) 
        }
      }
      
      
      else if (input$ptype1t=="Discrete Bar Plot")  { ######## Single Variable: Discrete Box Plot
        #input$refresht
        
        
        titlet <- reactive({
          #input$refresht
          t <- as.character(input$titlet)
          t
        })
        
        xlt <- reactive({
          #input$refresht
          xlt <- paste(as.character(colnames(testdata()[cyt()]))," (",as.character(input$unitst),")")
        })
        
        
        cwit <- reactive({
          #input$refresht
          wid <- input$widtht
          wid
        })
        
        
        if (input$tformat=="Wide"){
        
        ggplot(testdatadft(),aes(testdatadft()[,cyt()])) + geom_bar(width = cwit(), position = position_dodge(width = cwit())) + ggtitle(titlet()) + theme_bw() + xlab(xlt()) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
        
        }
        
        else if (input$tformat=="Long"){
          testdatadft2 <- subset(testdatadft(),testdatadft()[,pt()]==input$c1t)
          ggplot(testdatadft2,aes(testdatadft2[,vt()])) + geom_bar(width = cwit(), position = position_dodge(width = cwit())) + ggtitle(titlet()) + theme_bw() + xlab(as.character(input$c1t)) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) 
        }
      }
      
      else { ################ Single Variable: Boxplot
        
        titlet <- reactive({
          #input$refresht
          t <- as.character(input$titlet)
          t
        })
        
        ylt <- reactive({
          req(input$tformat=="Wide")
          #input$refresht
          #yl <- isolate(as.character(input$unitst))
          yl <- paste(as.character(colnames(testdata()[cyt()]))," (",as.character(input$unitst),")")
        })
        

        if (input$tformat=="Wide"){
        boxplot(testdatadft()[,cyt()],main=titlet(),ylab=ylt())
        }
        else if (input$tformat=="Long"){
          testdatadft2 <- subset(testdatadft(),testdatadft()[,pt()]==input$c1t)
          boxplot(testdatadft2[,vt()],main=titlet(),ylab=as.character(input$c1t))
        }
      }
      
    #)
  } #### END OF UNIVARIATE CODE, START OF BIVARIATE OPTIONS
  else {
    #input$refresht
    #isolate(
      
      
      if(input$ptype2t == "Point"){  ### Bivariate: Point Graph
        
        titlet <- reactive({
          #input$refresht
          t <- as.character(input$titlet)
          t
        })
        
        xlt <- reactive({
          #input$refresht
          if (cxt()==dt()) {
            xl <- as.character(colnames(testdata()[cxt()]))  
          }
          else {
            xl <- paste(as.character(colnames(testdata()[cxt()]))," (",as.character(input$units2xt),")")
          }
          xl  
          
        })
        
        ylt <- reactive({
          #input$refresht
          yl <- paste(as.character(colnames(testdata()[cyt()]))," (",as.character(input$units2yt),")")
        })
        
        if (input$tformat=="Wide"){
        ggplot(testdatadft(),aes(x=testdatadft()[,cxt()],y=testdatadft()[,cyt()])) + theme_bw() + ggtitle(titlet()) +geom_point(aes(colour=testdatadft()[,st()]),size=3.5) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab(xlt()) + ylab(ylt()) + labs(colour="Site\n")
        }
        
        else if (input$tformat=="Long"){
          
          
          if (input$c2t %in% colnames(testdatadft())){
            testdatadft2 <- subset(testdatadft(),testdatadft()[,pt()]==input$c1t)
            cxt <- which(colnames(testdatadft2)==input$c2t)
            ggplot(testdatadft2,aes(x=testdatadft2[,cxt],y=testdatadft2[,vt()])) + theme_bw() + ggtitle(titlet()) +geom_point(aes(colour=testdatadft2[,st()]),size=3.5) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab(input$c2t) + ylab(input$c1t) + labs(colour="Site\n")
            
          }
          
          else{
            testdatadftp1 <- subset(testdatadft(),testdatadft()[,pt()]==input$c1t)
            testdatadftp1 <- testdatadftp1[,c(dt(),st(),vt())]
            testdatadftp2 <- subset(testdatadft(),testdatadft()[,pt()]==input$c2t)
            testdatadftp2 <- testdatadftp2[,c(dt(),st(),vt())]
            testdatadft3 <- left_join(testdatadftp1,testdatadftp2,by=input$tdate)
            
            ggplot(testdatadft3, aes(x=testdatadft3[,5],y=testdatadft3[,3])) + theme_bw() + ggtitle(titlet()) + geom_point(aes(colour=testdatadft3[,2]),size=3) + theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) + labs(colour="Site\n") + xlab(input$c2t) + ylab(input$c1t)
            
          }
        }
      }
      
      
      
      else if(input$ptype2t=="Line") { ### Bivariate: Line Graph
        #input$refresht
        
        
        
        titlet <- reactive({
          #input$refresht
          t <- as.character(input$titlet)
          t
        })
        
        xlt <- reactive({
          #input$refresht
          if (cxt()==dt()) {
            xl <- as.character(colnames(testdata()[cxt()]))  
          }
          else {
            xl <- paste(as.character(colnames(testdata()[cxt()]))," (",as.character(input$units2xt),")")
          }
          xl  
          
        })
        
        ylt <- reactive({
          #input$refresht
          yl <- paste(as.character(colnames(testdata()[cyt()]))," (",as.character(input$units2yt),")")
        })
        
        
        if (input$tformat=="Wide"){
          ggplot(testdatadft(),aes(x=testdatadft()[,cxt()],y=testdatadft()[,cyt()])) + theme_bw() + ggtitle(titlet()) +geom_line(aes(colour=testdatadft()[,st()]),size=1.5) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab(xlt()) + ylab(ylt()) + labs(colour="Site\n")
        }
        
        else if (input$tformat=="Long"){
          
          
          if (input$c2t %in% colnames(testdatadft())){
            testdatadft2 <- subset(testdatadft(),testdatadft()[,pt()]==input$c1t)
            cxt <- which(colnames(testdatadft2)==input$c2t)
            ggplot(testdatadft2,aes(x=testdatadft2[,cxt],y=testdatadft2[,vt()])) + theme_bw() + ggtitle(titlet()) +geom_line(aes(colour=testdatadft2[,st()]),size=1.5) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab(input$c2t) + ylab(input$c1t) + labs(colour="Site\n")
            
          }
          
          else{
            testdatadftp1 <- subset(testdatadft(),testdatadft()[,pt()]==input$c1t)
            testdatadftp1 <- testdatadftp1[,c(dt(),st(),vt())]
            testdatadftp2 <- subset(testdatadft(),testdatadft()[,pt()]==input$c2t)
            testdatadftp2 <- testdatadftp2[,c(dt(),st(),vt())]
            testdatadft3 <- left_join(testdatadftp1,testdatadftp2,by=input$tdate)
            
            ggplot(testdatadft3, aes(x=testdatadft3[,5],y=testdatadft3[,3])) + theme_bw() + ggtitle(titlet()) + geom_line(aes(colour=testdatadft3[,2]),size=1.5) + theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) + labs(colour="Site\n") + xlab(input$c2t) + ylab(input$c1t)
            
          }
        }
        
        
        
        
      }
      
      else if(input$ptype2t=="Discrete Box Plot"){ ############ Bivariate: Discrete Box Plot
        #input$refresht
        
        #validate(
          #need(class(testdatadft()[,cxt()])=="character"|class(testdatadft()[,cxt()])=="factor","Please select a discrete variable")
        #)
        
        titlet <- reactive({
          #input$refresht
          t <- as.character(input$titlet)
          t
        })
        
        xlt <- reactive({
          #input$refresht
          xl <- paste(as.character(colnames(testdata()[cxt()]))," (",as.character(input$units2xt),")")
          xl
        })
        
        ylt <- reactive({
          #input$refresht
          yl <- paste(as.character(colnames(testdata()[cyt()]))," (",as.character(input$units2yt),")")
        })
        
        
        if (input$tformat=="Wide"){
        
        ggplot(data=testdatadft(),aes(x=testdatadft()[,cxt()],y=testdatadft()[,cyt()])) + theme_bw() + ggtitle(titlet()) +geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab(xlt()) + ylab(ylt())
        
        }
        
        else if (input$tformat=="Long"){
          
          
          if (input$c2t %in% colnames(testdatadft())){
            testdatadft2 <- subset(testdatadft(),testdatadft()[,pt()]==input$c1t)
            cxt <- which(colnames(testdatadft2)==input$c2t)
            ggplot(testdatadft2,aes(x=testdatadft2[,cxt],y=testdatadft2[,vt()])) + theme_bw() + ggtitle(titlet()) +geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab(input$c2t) + ylab(input$c1t) + labs(colour="Site\n")
            
          }
          
          else{
            testdatadftp1 <- subset(testdatadft(),testdatadft()[,pt()]==input$c1t)
            testdatadftp1 <- testdatadftp1[,c(dt(),st(),vt())]
            testdatadftp2 <- subset(testdatadft(),testdatadft()[,pt()]==input$c2t)
            testdatadftp2 <- testdatadftp2[,c(dt(),st(),vt())]
            testdatadft3 <- left_join(testdatadftp1,testdatadftp2,by=input$tdate)
            
            ggplot(testdatadft3, aes(x=testdatadft3[,5],y=testdatadft3[,3])) + theme_bw() + ggtitle(titlet()) + geom_boxplot() + theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) + labs(colour="Site\n") + xlab(input$c2t) + ylab(input$c1t)
            
          }
          
        }
        
      }
      
   #) ## end of isolate function for bivariate operations
  } ## end of if/else options for bivariate visualization
  
  
} ## end of RenderPLot script for test data main graphic output

##################################### Statistics: Reference Data #################################

## Select Parameter of Interest, First Tab

output$sumpar <- renderUI({
  req(input$rconfirm)
  if (input$rformat=="Wide"){
    choices=input$rparsW
  }
  else if (input$rformat=="Long"){
    choices=unique(refdata2()[,p()])
  }
  selectInput("sumpar",label="Select a parameter",choices=choices,selected=1)  
})

## Select Site(s) of Interest, First Tab
output$sumselsite <- renderUI({
  sortr <- refdata2()
  sortr <- arrange(sortr, sortr[,s()])
  selectInput("sumsite",label="Select the site you would like to view",choices=unique(sortr[,s()]),multiple=FALSE)
})


## Get column indice of parameter

spc <- reactive({
  req(input$rconfirm,input$rformat=="Wide")
  sp <- which(colnames(refdata2())==input$sumpar)
  sp
})

# Parse date column of reference dataset, Tab 1
rdd <- reactive({
  req(input$rformat)
  rdd<- refdata2()
  rdd$Month <- as.factor(month(rdd[,d()],label=TRUE))
  rdd$Year <- as.factor(year(rdd[,d()]))
  ## keep only selected sites
  rdd <- subset(rdd, rdd[,s()] %in% input$sumsite)
  if (input$rformat=="Long"){
    rdd <- subset(rdd,rdd[,p()] %in% input$sumpar)
  }
  rdd
  
})


## Boxplot Graph

# Graph Design
boxpl <- function(){
  if (input$rformat=="Wide"){
  validate(
    need(class(refdata()[,spc()])=="numeric", "Please select a numeric variable")
  )
    boxplot(rdd()[,spc()],main=paste("Observations of Parameter",input$sumpar,", ",input$sumsite))
  }
  else if (input$rformat=="Long"){
    boxplot(rdd()[,v()],main=paste("Observations of Parameter",input$sumpar,", ",input$sumsite))
  }
  
}
# Render Boxplot Graphic
output$boxp <- renderPlot({
  print(boxpl())
})


output$sum <- renderPrint({
  req(input$sumpar)
  if (input$rformat=="Wide"){
    validate(
      need(class(refdata2()[,spc()])=="numeric", " ")
    )
    
  summary(rdd()[,spc()])
  }
  else if (input$rformat=="Long"){
    summary(rdd()[,v()])  
  }
})

##Monthly Boxplot

# Graph Design

monthpl <- function(){
  req(input$sumpar)
  
  if (input$rformat=="Wide"){
  validate(
    need(class(refdata2()[,spc()])=="numeric", " ")
  )
  
  ggplot(data=rdd(),aes(x=rdd()$Month,y=rdd()[,spc()])) + theme_bw() + ggtitle(paste("Monthly Observations - ",input$sumpar,", ",input$sumsite)) +geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab("Month") +ylab(input$sumpar)

  }
  
  else if (input$rformat=="Long"){
    ggplot(data=rdd(),aes(x=rdd()$Month,y=rdd()[,v()])) + theme_bw() + ggtitle(paste("Monthly Observations - ",input$sumpar,", ",input$sumsite)) +geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab("Month") +ylab(input$sumpar)
    
  }
}

# Render Graphic
output$mboxp <- renderPlot({
  print(monthpl())
})

# Download Handler
output$downmboxp <- downloadHandler(
  filename = "mplotref.png",
  content = function(file) {
    ggsave(file, plot = monthpl(), device = "png")
  })


## Yearly Boxplot

yearpl <- function(){
  req(input$sumpar)
  
  if (input$rformat=="Wide"){
  
  validate(
    need(class(refdata2()[,spc()])=="numeric", " ")
  )
  
  ggplot(data=rdd(),aes(x=rdd()$Year,y=rdd()[,spc()])) + theme_bw() + ggtitle(paste("Yearly Observations - ",input$sumpar,", ",input$sumsite)) +geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab("Year") +ylab(input$sumpar)
  }
  
  else if (input$rformat=="Long"){
    ggplot(data=rdd(),aes(x=rdd()$Year,y=rdd()[,v()])) + theme_bw() + ggtitle(paste("Yearly Observations - ",input$sumpar,", ",input$sumsite)) +geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab("Month") +ylab(input$sumpar)
    
  }
}

# Render Graphic

output$yboxp <- renderPlot({
  print(yearpl())
})


# Download Handler
output$downyboxp <- downloadHandler(
  filename = "yplotref.png",
  content = function(file) {
    ggsave(file, plot = yearpl(), device = "png")
  })

output$Statsreport <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = function(){
    paste0("output", ".zip")
  },
  content = function(file) {
    src <- normalizePath('StatsTemplate.Rmd')
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'StatsTemplate.Rmd', overwrite = TRUE)
    fs <- c()
    sitesforanalysis <- unique(stringr::str_trim(refdata2()[,s()]))
    length <- length(sitesforanalysis)
    dfMain<- refdata2()
    dfMain[,s()] <- stringr::str_trim(refdata2()[,s()])
    dfMain$Month <- as.factor(month(dfMain[,d()],label=TRUE))
    dfMain$Year <- as.factor(year(dfMain[,d()]))
    
    
    for (i in 1:length){
      withProgress(message = paste("Preparing report",i,"of",length," - please wait!"),{  
        df <- subset(dfMain,dfMain[,s()]== sitesforanalysis[i])
        rm(params)
        path <- paste0(sitesforanalysis[i],".pdf")
        path <- gsub("/","_", path)
        if (input$rformat=="Long"){
          params <- list(data = data.frame(df), siteCol = s(), dateCol = d(),varsCol=input$rpars, dataFormat = input$rformat,Sites=input$rsid,sumpar = input$sumpar,valuecol = input$rvals,rendered_by_shiny=TRUE)
        }
        if (input$rformat == "Wide"){
          params <- list(data = data.frame(df), siteCol = s(), dateCol = d(),varsCol=input$rparsW, dataFormat = input$rformat,Sites=input$rsid,sumpar= input$sumpar, rendered_by_shiny=TRUE)
          
        }
        rmarkdown::render("StatsTemplate.rmd", rmarkdown::pdf_document(), output_file = path,params = params,envir = new.env())
        fs <- c(fs, path)
        
        
        zip(file,fs)
      }) # end of withProgress
    }
    contentType = "application/zip"
  }
)

########################################################### Tab 2

## Select Site(s) of Interest, Tab 2
output$sumselsite2 <- renderUI({
  sortr <- refdata2()
  sortr <- arrange(sortr, sortr[,s()])
  selectInput("sumsite2",label="Select the site you would like to view",choices=unique(sortr[,s()]),multiple=FALSE,selected=input$sumsite)
})
##Select Parameter, Tab 2
output$sumpar2 <- renderUI({
  if (input$rformat=="Wide"){
    choices = colnames(refdata2())
  }
  else if (input$rformat=="Long"){
    choices = unique(refdata2()[,p()])
  }
  selectInput("sumpar2",label="Select a parameter",choices=choices,selected=input$sumpar)
  
})

rdd2 <- reactive({
  rdd<- refdata2()
  ###### 
  rdd$Month <- as.factor(month(rdd[,d()],label=TRUE))
  rdd$Year <- as.factor(year(rdd[,d()]))
  ## keep only selected sites
  rdd <- subset(rdd, rdd[,s()] %in% input$sumsite2)
  if (input$rformat =="Long"){
    rdd <- subset(rdd,rdd[,p()] %in% input$sumpar2)
  }
  rdd
  
})

## Get parameter of interest column indice, Tab 2

spc2 <- reactive({
  req(input$rconfirm)
  sp <- which(colnames(refdata2())==input$sumpar2)
  sp
})

output$sumhist <- renderPlot({
  if (input$rformat=="Wide"){
  validate(
    need(class(refdata2()[,spc2()])=="numeric", "Please select a numeric variable")
  )
  ggplot(data=rdd2(),aes(rdd2()[,spc2()])) + geom_histogram() + ggtitle(paste("Histogram of ",input$sumpar2,", ",input$sumsite2)) + theme_bw() + xlab("")
  }
  else if (input$rformat=="Long"){
    ggplot(data=rdd2(),aes(rdd2()[,v()])) + geom_histogram() + ggtitle(paste("Histogram of ",input$sumpar2,", ",input$sumsite2)) + theme_bw() + xlab("")
    
  }
  
})

output$sw <- renderPrint({
  req(input$sumpar2)
  if (input$rformat=="Wide"){
  validate(
    need(class(refdata2()[,spc2()])=="numeric", "Please select a numeric variable")
  )
  
  shapiro.test(rdd2()[,spc2()])
  }
  else {
    shapiro.test(rdd2()[,v()])
  }
  
})

output$swl <- renderPrint({
  req(input$sumpar2)
  if (input$rformat=="Wide"){
  validate(
    need(class(refdata2()[,spc2()])=="numeric", "Please select a numeric variable")
  )
  
  shapiro.test(log(rdd2()[,spc2()]))
  }
  else {
    shapiro.test(log(rdd2()[,v()]))  
  }
})




##################################### Statistics: Test Data #################################

## Select Parameter of Interest, First Tab

output$sumpart <- renderUI({
  req(input$tconfirm)
  if (input$tformat=="Wide"){
    choices=input$tparsW
  }
  else if (input$tformat=="Long"){
    choices = unique(testdata2()[,pt()])
  }
  selectInput("sumpart",label="Select a parameter",choices=choices,selected=1)
})

## Select Site of Interest, First Tab Test

output$sumselsitet <- renderUI({
  sortr <- testdata2()
  sortr <- arrange(sortr, sortr[,st()])
  selectInput("sumsitet",label="Select the site you would like to view",choices=unique(sortr[,st()]),multiple=FALSE)
})



## Get column indice

spct <- reactive({
  req(input$tconfirm, input$tformat=="Wide")
  sp <- which(colnames(testdata2())==input$sumpart)
  sp
})

# Parse date column of test dataset, Tab 1
rddt <- reactive({
  req(input$tformat)
  rddt<- testdata2()
  rddt$Month <- as.factor(month(rddt[,dt()],label=TRUE))
  rddt$Year <- as.factor(year(rddt[,dt()]))
  ## Keep only the selected sites
  rddt <- subset(rddt,rddt[,st()] %in% input$sumsitet)
  if (input$tformat=="Long"){
  rddt <- subset(rddt,rddt[,pt()] %in% input$sumpart)
  }
  rddt
  
})
  
boxplt <- function(){
  req(input$tconfirm,input$sumpart)
  if (input$tformat=="Wide"){
    validate(
      need(class(testdata2()[,spct()])=="numeric", "Please select a numeric variable")
    )
  
  boxplot(rddt()[,spct()],main=paste("All Observations of Parameter",input$sumpart,", ",input$sumsitet))
  }
  
  else if (input$tformat=="Long"){
    boxplot(rddt()[,vt()],main=paste("All Observations of Parameter",input$sumpart,", ",input$sumsitet))
  }
}

# Render boxplot graphic, test data
output$boxpt <- renderPlot({
  print(boxplt())
})


output$sumt <- renderPrint({
  req(input$tconfirm,input$sumpart)
  if (input$tformat=="Wide"){
  validate(
    need(class(testdata2()[,spct()])=="numeric", "Please select a numeric variable")
  )
  summary(rddt()[,spct()])
  }
  else if (input$tformat=="Long"){
    summary(rddt()[,vt()]) 
  }
})

##### Monthly boxplot

### Graph Design


monthplt<- function(){
  req(input$tconfirm,input$sumpart)
  if (input$tformat=="Wide"){
  validate(
    need(class(testdata2()[,spct()])=="numeric", "Please select a numeric variable")
  )
  
  ggplot(data=rddt(),aes(x=rddt()$Month,y=rddt()[,spct()])) + theme_bw() + ggtitle(paste("Monthly Observations - ",input$sumpart,", ",input$sumsitet)) +geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab("Month") +ylab(input$sumpart)
  }
  else if (input$tformat=="Long"){
  ggplot(data=rddt(),aes(x=rddt()$Month,y=rddt()[,vt()])) + theme_bw() + ggtitle(paste("Monthly Observations - ",input$sumpart,", ",input$sumsitet)) +geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab("Month") +ylab(input$sumpart)
    
  }
}

### Render Graphic
output$mboxpt <- renderPlot({
  print(monthplt())
})

### Download Handler
output$downmboxpt <- downloadHandler(
  filename = "mplottest.png",
  content = function(file) {
    ggsave(file, plot = monthplt(), device = "png")
  })

##### Yearly Boxplot

### Graph Design

yearplt <- function(){
  req(input$sumpart)
  if (input$tformat=="Wide"){
  validate(
    need(class(testdata()[,spct()])=="numeric", "Please select a numeric variable")
  )
  
  ggplot(data=rddt(),aes(x=rddt()$Year,y=rddt()[,spct()])) + theme_bw() + ggtitle(paste("Yearly Observations - ",input$sumpart,", ",input$sumsitet)) +geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab("Year") +ylab(input$sumpart)
  }
  else if (input$tformat=="Long"){
    ggplot(data=rddt(),aes(x=rddt()$Year,y=rddt()[,vt()])) + theme_bw() + ggtitle(paste("Yearly Observations - ",input$sumpart,", ",input$sumsitet)) +geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab("Year") +ylab(input$sumpart)
    
  }
}

### Render Graphic
output$yboxpt <- renderPlot({
  print(yearplt())
})

# Download Handler
output$downyboxp <- downloadHandler(
  filename = "yplotref.png",
  content = function(file) {
    ggsave(file, plot = yearpl(), device = "png")
  })

output$StatsreportT <- downloadHandler(
  filename = function(){
    paste0("output", ".zip")
  },
  content = function(file) {
    src <- normalizePath('StatsTemplate.Rmd')
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'StatsTemplate.Rmd', overwrite = TRUE)
    fs <- c()
    sitesforanalysis <- unique(stringr::str_trim(testdata2()[,st()]))
    length <- length(sitesforanalysis)
    dfMain<- testdata2()
    dfMain[,st()] <- stringr::str_trim(testdata2()[,st()])
    dfMain$Month <- as.factor(month(dfMain[,dt()],label=TRUE))
    dfMain$Year <- as.factor(year(dfMain[,dt()]))
    
    
    for (i in 1:length){
      withProgress(message = paste("Preparing report",i,"of",length," - please wait!"),{  
        df <- subset(dfMain,dfMain[,st()]== sitesforanalysis[i])
        rm(params)
        path <- paste0(sitesforanalysis[i],".pdf")
        path <- gsub("/","_", path)
        if (input$tformat=="Long"){
          params <- list(data = data.frame(df), siteCol = st(), dateCol = dt(),varsCol=input$tpars, dataFormat = input$tformat,Sites=input$tsid,sumpar = input$sumpart,valuecol = input$tvals,rendered_by_shiny=TRUE)
        }
        if (input$tformat == "Wide"){
          params <- list(data = data.frame(df), siteCol = st(), dateCol = dt(),varsCol=input$tparsW, dataFormat = input$tformat,Sites=input$tsid,sumpar= input$sumpart, rendered_by_shiny=TRUE)
          
        }
        rmarkdown::render("StatsTemplate.rmd", rmarkdown::pdf_document(), output_file = path,params = params,envir = new.env())
        fs <- c(fs, path)
        
        
        zip(file,fs)
      }) # end of withProgress
    }
    contentType = "application/zip"
  }
)

################## Statistics Tab 2 ###### Test Dataset #######

## Select Site(s) of Interest, Tab 2
output$sumselsite2t <- renderUI({
  sortr <- testdata2()
  sortr <- arrange(sortr, sortr[,st()])
  selectInput("sumsite2t",label="Select the site you would like to view",choices=unique(sortr[,st()]),multiple=FALSE,selected=input$sumsitet)
})

##Select Parameter, Tab 2
output$sumpar2t <- renderUI({
  if (input$tformat=="Wide"){
    choices=colnames(testdata2())
  }
  if (input$tformat=="Long"){
    choices=unique(testdata2()[,pt()])
  }
  selectInput("sumpar2t",label="Select a parameter",choices=choices,selected=input$sumpart)
  
})

## Get parameter of interest column indice

spc2t <- reactive({
  req(input$tconfirm, input$tformat=="Wide")
  sp <- which(colnames(testdata2())==input$sumpar2t)
  sp
})

# Parse date column of test dataset, Tab 2
rddt2 <- reactive({
  rddt<- testdata2()

  rddt$Month <- as.factor(month(rddt[,dt()],label=TRUE))
  rddt$Year <- as.factor(year(rddt[,dt()]))
  ## Keep only the selected sites
  rddt <- subset(rddt,rddt[,st()] %in% input$sumsite2t)
  if (input$tformat=="Long"){
    rddt <- subset(rddt,rddt[,pt()] %in% input$sumpar2t)
  }
  rddt
  
})

output$sumhistt <- renderPlot({
  
  if (input$tformat=="Wide"){
  validate(
    need(class(testdata2()[,spc2t()])=="numeric", "Please select a numeric variable")
  )
  ggplot(data=rddt2(),aes(rddt2()[,spc2t()])) + geom_histogram() + ggtitle(paste("Histogram of ",input$sumpar2t,input$sumsite2t)) + theme_bw() + xlab("")
  }
  else if (input$tformat=="Long"){
    ggplot(data=rddt2(),aes(rddt2()[,vt()])) + geom_histogram() + ggtitle(paste("Histogram of ",input$sumpar2t,input$sumsite2t)) + theme_bw() + xlab("")
  }
})

output$swt <- renderPrint({
  req(input$sumpar2t)
  if (input$tformat=="Wide"){
  validate(
    need(class(testdata2()[,spc2t()])=="numeric", " ")
  )
  
  shapiro.test(rddt2()[,spc2t()])
  }
  if (input$tformat=="Long"){
    shapiro.test(rddt2()[,vt()])
  }
})

output$swlt <- renderPrint({
  req(input$sumpar2t)
  if (input$tformat=="Wide"){
  validate(
    need(class(testdata2()[,spc2t()])=="numeric", " ")
  )
  shapiro.test(log(rddt2()[,spc2t()]))
  }
  
  if (input$tformat=="Long"){
    shapiro.test(log(rddt2()[,vt()])) 
  }
})



####################################### Temporal Coverage Summary: Reference Data #######################

### Create copy dataframe, parse date column, and create columns for year and month

tcsr <- reactive({
  tcsr <- refdata2()
  ###### 
  tcsr <- subset(tcsr,(!is.na(tcsr[,d()]))&(!is.null(tcsr[,d()])))
  tcsr$Month <- lubridate::month(tcsr[,d()])
  tcsr$Year <- lubridate::year(tcsr[,d()])
  tcsr$Year <- as.factor(tcsr$Year)
  tcsr
})



### Select Between Single, Complete, or Regional (Grouped) Summary
output$singrpR <- renderUI({
  radioButtons("singrpr","Select single-site summary (A), complete dataset summary (B), or summary by group (C) ",choices=list("A","B","C"))
})

output$MonthYear <- renderUI({
  radioButtons("monthyear", "Select summary style", choices = list("By Month","By Year"))
})

## If single is selected
output$sitesR <- renderUI({
  selectInput("sitesr","Please select a monitoring site",choices=unique(refdata2()[,s()]))
})

## If group is selected, prompt for column
output$gcR <- renderUI({
  selectInput("gcr","Select group column",choices=colnames(refdata2()),selected=refdata2()[,s()])
})

## get group column indice
gr<- reactive({
  gr <- which(colnames(refdata2())==input$gcr)  
})

## Select group for viewing
output$gsR <- renderUI({
  selectInput("gsr","Please select a group",choices=unique(refdata2()[,gr()]))
})


### Ability to download the generated graph
output$downloadPlotT <- downloadHandler(
  filename = "temporalcoverage.png",
  content = function(file) {
    ggsave(file, plot = tcPr(), device = "png")
  })

output$tcPr <- renderPlot({
  print(tcPr())
})

## Create main output graph for temporal coverage
tcPr <- function(){
  if (input$singrpr=="A") {
  sub <- subset(tcsr(),tcsr()[,s()]==input$sitesr)
  sub$Month <- factor(month.name[sub$Month], levels=month.name[1:12])
  if (input$monthyear == "By Month"){
    ggplot(data = sub, aes(x = Month)) + geom_bar(aes(fill=Year)) + theme_bw() + xlab("Month") + ylab("Number of Observations") + ggtitle(paste("Observation Counts by Month - ",input$sitesr))
  }
  else if (input$monthyear == "By Year"){
    ggplot(data = sub, aes(x = Year)) + geom_bar(aes(fill=Month)) + theme_bw() + xlab("Year") + ylab("Number of Observations") + ggtitle(paste("Observation Counts by Year - ",input$sitesr)) 
  }
    }
  
  else if (input$singrpr=="B") {
    sub <- tcsr()
    sub$Month <- factor(month.name[sub$Month], levels=month.name[1:12])
    
    if (input$monthyear == "By Month"){
      ggplot(data = sub, aes(x = Month)) + geom_bar(aes(fill=Year)) + theme_bw() + xlab("Month") + ylab("Number of Observations") + ggtitle("Observation Counts by Month - All Observations")
    }
    
    else if (input$monthyear == "By Year"){
      ggplot(data = sub, aes(x = Year)) + geom_bar(aes(fill=Month)) + theme_bw() + xlab("Year") + ylab("Number of Observations") + ggtitle("Observation Counts by Year - All Observations")  
    }
      }
  
  else {
    sub <- tcsr()
    sub$Month <- factor(month.name[sub$Month], levels=month.name[1:12])
    sub <- subset(sub,sub[,gr()]==input$gsr)
    
    if (input$monthyear == "By Month"){
      ggplot(data = sub, aes(x = Month)) + geom_bar(aes(fill=Year)) + theme_bw() + xlab("Month") + ylab("Number of Observations") + ggtitle(paste("Observation Counts by Month - ",input$gsr))
    }
    else if (input$monthyear == "By Year"){
      ggplot(data = sub, aes(x = Year)) + geom_bar(aes(fill=Month)) + theme_bw() + xlab("Year") + ylab("Number of Observations") + ggtitle(paste("Observation Counts by Year - ",input$gsr))
    }
      }
  
  
}


##### Report Generation: Temporal Coverage Summary

output$TCSreport <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = function(){
    paste0("output", ".zip")
  },
  content = function(file) {
    src <- normalizePath('TCStemplate.Rmd')
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'TCStemplate.Rmd', overwrite = TRUE)
      fs <- c()
      sitesforanalysis <- unique(stringr::str_trim(refdata2()[,s()]))
      length <- length(sitesforanalysis)
      
    
      for (i in 1:length){
        withProgress(message = paste("Preparing report",i,"of",length," - please wait!"),{  
        tcsr <- subset(tcsr(),tcsr()[,s()]== sitesforanalysis[i])
        rm(params)
        path <- paste0(sitesforanalysis[i],".pdf")
        path <- gsub("/","_", path)
        if (input$rformat=="Long"){
        params <- list(data = data.frame(tcsr), siteCol = s(), dateCol = d(),varsCol=input$rpars, dataFormat = input$rformat,Sites=input$rsid,rendered_by_shiny=TRUE)
        }
        if (input$rformat == "Wide"){
        params <- list(data = data.frame(tcsr), siteCol = s(), dateCol = d(),varsCol=input$rparsW, dataFormat = input$rformat,Sites=input$rsid,rendered_by_shiny=TRUE)
          
        }
      rmarkdown::render("TCStemplate.rmd", rmarkdown::pdf_document(), output_file = path,params = params,envir = new.env())
      fs <- c(fs, path)
      
    zip(file,fs)
    }) # end of withProgress
      }
    contentType = "application/zip"
  }
)


############################################# Temporal Coverage Summary: Test Data ##################


### Create copy dataframe, parse date column, and create columns for year and month

tcsrt <- reactive({
  tcst <- testdata2()
  tcst <- subset(tcst,(!is.na(tcst[,dt()]))&(!is.null(tcst[,dt()])))
  tcst$Month <- lubridate::month(tcst[,dt()])
  tcst$Year <- lubridate::year(tcst[,dt()])
  tcst$Year <- as.factor(tcst$Year)
  tcst
})



### Select Between Single, Complete, or Regional (Grouped) Summary
output$singrpRt <- renderUI({
  radioButtons("singrprt","Select single-site summary (A), complete dataset summary (B), or summary by group (C) ",choices=list("A","B","C"))
})

output$MonthYearT <- renderUI({
  radioButtons("monthyearT", "Select summary style", choices = list("By Month","By Year"))
})

## If single is selected
output$sitesRt <- renderUI({
  selectInput("sitesrt","Please select a monitoring site",choices=unique(testdata2()[,st()]))
})

## If group is selected, prompt for column
output$gcRt <- renderUI({
  selectInput("gcrt","Select group column",choices=colnames(testdata2()))
})

## get group column indice
grt<- reactive({
  grt <- which(colnames(testdata2())==input$gcrt)  
})

## Select group for viewing
output$gsRt <- renderUI({
  selectInput("gsrt","Please select a group",choices=unique(testdata2()[,grt()]))
})


### Ability to download the generated graph
output$downloadPlotTt <- downloadHandler(
  filename = "temporalcoverage.png",
  content = function(file) {
    ggsave(file, plot = tcPrt(), device = "png")
  })

output$tcPrt <- renderPlot({
  print(tcPrt())
})

## Create main output graph for temporal coverage
tcPrt <- function(){  
    if (input$singrprt=="A") {
      sub <- subset(tcsrt(),tcsrt()[,st()]==input$sitesrt)
      sub$Month <- factor(month.name[sub$Month], levels=month.name[1:12])
      if (input$monthyearT == "By Month"){
      
      ggplot(data = sub, aes(x = Month)) + geom_bar(aes(fill=Year)) + theme_bw() + xlab("Month") + ylab("Number of Observations") + ggtitle(paste("Observation Counts by Month - ",input$sitesrt))
      }
      else if (input$monthyearT == "By Year"){
        ggplot(data = sub, aes(x = Year)) + geom_bar(aes(fill=Month)) + theme_bw() + xlab("Year") + ylab("Number of Observations") + ggtitle(paste("Observation Counts by Year - ",input$sitesrt)) 
      }
        }
    
    else if (input$singrprt=="B") {
      sub <- tcsrt()
      sub$Month <- factor(month.name[sub$Month], levels=month.name[1:12])
      
      if (input$monthyearT == "By Month"){
      ggplot(data = sub, aes(x = Month)) + geom_bar(aes(fill=Year)) + theme_bw() + xlab("Month") + ylab("Number of Observations") + ggtitle("Observation Counts by Month - All Observations")
      }
      else if (input$monthyearT == "By Year"){
        ggplot(data = sub, aes(x = Year)) + geom_bar(aes(fill=Month)) + theme_bw() + xlab("Year") + ylab("Number of Observations") + ggtitle("Observation Counts by Year - All Observations")  
      }
      
      }
    
    else {
      sub <- tcsrt()
      sub$Month <- factor(month.name[sub$Month], levels=month.name[1:12])
      sub <- subset(sub,sub[,grt()]==input$gsrt)
      
      if(input$monthyearT == "By Month"){
      
      ggplot(data = sub, aes(x = Month)) + geom_bar(aes(fill=Year)) + theme_bw() + xlab("Month") + ylab("Number of Observations") + ggtitle(paste("Observation Counts by Month - ",input$gsrt))
      }
      
      else if (input$monthyearT == "By Year"){
        ggplot(data = sub, aes(x = Year)) + geom_bar(aes(fill=Month)) + theme_bw() + xlab("Year") + ylab("Number of Observations") + ggtitle(paste("Observation Counts by Year - ",input$gsrt))
      }
         }
  
  
}

### Report Generation: Temporal Coverage Summary (Test)
output$TCSreportT <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = function(){
    paste0("output", ".zip")
  },
  content = function(file) {
    
    
    #tempReport <- file.path(tempdir(), "TCStemplate.Rmd")
    #file.copy("TCStemplate.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    #params <- list(data = data.frame(tcsr()), siteCol = s(), dateCol = d(),varsCol=input$rpars, dataFormat = input$rformat,Sites=input$rsid,rendered_by_shiny=TRUE)
    fs <- c()
    sitesforanalysis <- unique(stringr::str_trim(testdata2()[,st()]))
    length <- length(sitesforanalysis)
    
    
    for (i in 1:length){
      withProgress(message = paste("Preparing report",i,"of",length," - please wait!"),{  
        tcsrt <- subset(tcsrt(),tcsrt()[,st()]== sitesforanalysis[i])
        rm(params)
        path <- paste0(sitesforanalysis[i],".pdf")
        path <- gsub("/","_", path)
        if (input$rformat=="Long"){
          params <- list(data = data.frame(tcsrt), siteCol = st(), dateCol = dt(),varsCol=input$tpars, dataFormat = input$tformat,Sites=input$tsid,rendered_by_shiny=TRUE)
        }
        if (input$rformat == "Wide"){
          params <- list(data = data.frame(tcsrt), siteCol = st(), dateCol = dt(),varsCol=input$tparsW, dataFormat = input$tformat,Sites=input$tsid,rendered_by_shiny=TRUE)
          
        }
        rmarkdown::render("TCStemplate.rmd", rmarkdown::pdf_document(), output_file = path,params = params,envir = new.env())
        fs <- c(fs, path)
        
        zip(file,fs)
      }) # end of withProgress
    }
    contentType = "application/zip"
  }
)



################################################### Paired Site Comparison Script ###################

## Create text input bar for CRS code
output$proj <- renderUI({
  textInput("proj","Please provide the full Proj4 definition string to project from GCS")
})



## Create spatial points dataframe for each file

sp.r <- reactive({
  req(input$buffer) #add input$proj to return to original state with user input

    sp.ref <- refdata2()
    sp.ref<- subset(sp.ref, !sp.ref[,x()]=="null"&!sp.ref[,y()]=="null"&!is.na(sp.ref[,x()])&!is.na(sp.ref[,y()]))
    sp.ref[,x()] <- as.numeric(sp.ref[,x()])
    sp.ref[,y()] <- as.numeric(sp.ref[,y()])
    sp.ref <- sp.ref[!duplicated(sp.ref[,s()]), ] ## want single entry for each monitoring site
    sp.ref <- sp.ref[,c(s(),x(),y())] ## only need name, x, and y to simplify
    coordinates(sp.ref) <- c(2,3)
    proj4string(sp.ref) <- CRS("+init=epsg:4326") ## initially assign WGS84 as GCS
    p <- "+proj=lcc +lat_1=62 +lat_2=70 +lat_0=0 +lon_0=-112 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
    spTransform(sp.ref, CRS(p))
    sp.ref 
})



sp.t <- reactive({
  req(input$buffer) # add input$proj to return to original state with user input
    sp.test <- testdata2()
    sp.test<- subset(sp.test, !sp.test[,xt()]=="null"&!sp.test[,yt()]=="null"&!is.na(sp.test[,xt()])&!is.na(sp.test[,yt()]))
    sp.test[,xt()] <- as.numeric(sp.test[,xt()])
    sp.test[,yt()] <- as.numeric(sp.test[,yt()])
    sp.test <- sp.test[!duplicated(sp.test[,st()]), ]
    sp.test <- sp.test[,c(st(),xt(),yt())]
    coordinates(sp.test) <- c(2,3)
    proj4string(sp.test) <- CRS("+init=epsg:4326") ## initially assign WGS84 as GCS
    p <- "+proj=lcc +lat_1=62 +lat_2=70 +lat_0=0 +lon_0=-112 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
    spTransform(sp.test, CRS(p))
    sp.test 
})
  

### Generate distance matrix
distmatrix <- reactive ({
  validate (
    need(input$buffer != "","")
  )
  distmatrix <- data.frame(spDists(sp.r(),sp.t()))
  temt <- testdata2()
  temt<- subset(temt, !temt[,xt()]=="null"&!temt[,yt()]=="null"&!is.na(temt[,xt()])&!is.na(temt[,yt()]))
  temt[,xt()] <- as.numeric(temt[,xt()])
  temt[,yt()] <- as.numeric(temt[,yt()])
  temt <- temt[!duplicated(temt[,st()]), ]
  temt <- temt[,c(st(),xt(),yt())]
  
  temr <- refdata2()
  temr<- subset(temr, !temr[,x()]=="null"&!temr[,y()]=="null"&!is.na(temr[,x()])&!is.na(temr[,y()]))
  temr[,x()] <- as.numeric(temr[,x()])
  temr[,y()] <- as.numeric(temr[,y()])
  temr <- temr[!duplicated(temr[,s()]), ]
  temr <- temr[,c(s(),x(),y())]
  
  names(distmatrix) <- temt[,1]
  distmatrix$Reference <- temr[,1]
  l <- as.integer(ncol(distmatrix))
  distmatrix <- distmatrix[,c(l,1:(l-1))]
  distmatrix
})
## Input tool for buffer
output$buffer <- renderUI({
  textInput("buffer","Specify maximum euclidean distance to generate site pairs (km)")
})


## Filter matrix and reshape based on user input



## Generate final list of outputs
pairs <- reactive({
  validate (
    need(input$buffer != "","")
  )
  l <- as.integer(ncol(distmatrix()))
  finalout <- data.frame("Reference"=NA,"Test"=NA,"Distance"=NA,"Pairing Number"=NA)
  n <- 1
  for (c in 2:l) {
    for (r in 1:(nrow(distmatrix()))) {
      if ((distmatrix()[r,c]) <= as.numeric(input$buffer)){
        newrow <- c((distmatrix()[r,1]),(names(distmatrix())[c]),(distmatrix()[r,c]),n)
        finalout <- rbind(finalout,newrow)
        n <- n + 1
        
      }
      else {
        finalout <- finalout
        
      }
    }
  }
  finalout <- finalout[-1,]
  row.names(finalout) <- c()
  finalout
  
})

output$distmatrixt <- renderTable({
  distmatrix()
})

output$pair <- DT::renderDataTable({
  # validate(
  #   need(input$proj != "", "Please provide a complete proj4 definition string for the desired projected coordinate system. To obtain a definition string, see https://epsg.io. Select PROJ.4 under Export, and copy the provided text.")
  # )
  validate (
    need(input$buffer != "","Please specify a buffer distance")
    )
  DT::datatable(pairs(),options=list(
    pageLength=5
  )
  )
})

## Count number of pairs generated
output$paircount <- renderText({
  validate (
    need(input$buffer != "","Pairs will be generated following input of buffer distance")
  )
  
  ps <- as.character(nrow(pairs()))
  text<- paste("Using the Euclidean distance of",input$buffer,", ",ps,"pair(s) of sites have been determined")
  text
  
})

####################################

## Select Pairing you would like to View

output$spair <- renderUI({
  validate (
    need(input$buffer != "","")
  )
  
  selectInput("spair","Select the site pair you would like to view",choices=1:nrow(pairs()))
})


## get Site names based on selection

## Reference Site Name
rsite <- reactive({
  req(input$spair)
    refsite<- pairs()[input$spair,1]
    refsite
  })

## Test Site Name
  tsite<- reactive({
    req(input$spair)
    testsite <- pairs()[input$spair,2]
  })

## Select a parameter from the reference dataset
output$refp <- renderUI({
  validate (
    need(input$buffer != "","")
  )
  if (input$rformat=="Wide"){
    
   choices = colnames(refdata()) 
  }
  else{
    choices=unique(refdata()[,p()])
  }
    selectInput("refp","Select a parameter from the Reference Dataset",choices=choices)
  })

## Select the matching parameter from the test dataset
output$tesp <- renderUI({
  if (input$tformat=="Wide"){
    validate (
      need(input$buffer != "","")
    )
   choices=colnames(testdata()) 
  }
  else{
    choices=unique(testdata()[,pt()])
  }
    selectInput("tesp","Select the matching parameter from the Test Dataset",choices=choices)
  })



####################################### Create main data tables

####### Reference ########
referencedt <- reactive({
  
  req(input$rconfirm,input$spair)
  
  referencedt <- refdata2()
  referencedt <- subset(referencedt, referencedt[,s()]==pairs()[input$spair,1])
  
  if (input$rformat=="Long"){
    referencedt <- subset(referencedt,referencedt[,p()] == input$refp & !is.na(referencedt[,v()]) & !is.null(referencedt[,v()]))
  }
  
  ## Remove nulls and NAs for date column, ref parameter column, lat and long columns
  
  else{
    coln <- which(colnames(referencedt)==input$refp)
    referencedt <- subset(referencedt, !is.na(referencedt[,coln]) & !is.null(referencedt[,coln]))
    
 
  }
  referencedt <- subset(referencedt, !is.na(referencedt[,x()])& !is.null(referencedt[,x()]) & !is.na(referencedt[,y()]) & !is.null(referencedt[,y()]) & !is.na(referencedt[,d()]) & !is.null(referencedt[,d()]))
  referencedt$Month <- month(referencedt[,d()])
  referencedt$Monthname<- month.name[referencedt$Month]
  referencedt$Year <- year(referencedt[,d()])
  referencedt <- arrange(referencedt,Monthname,Year)
  referencedt
  
})

#### Test ###

testdt <- reactive({
  
  req(input$tconfirm,input$tesp)
  
  testdt <- testdata2()
  testdt <- subset(testdt, testdt[,st()]==tsite())
  if (input$tformat=="Long"){
    testdt <- subset(testdt,testdt[,pt()] %in% input$tesp & !is.na(testdt[,vt()]) & !is.null(testdt[,vt()]))
  }
  else {
    colnt <- which(colnames(testdt)==input$tesp)
    testdt <- subset(testdt, !is.na(testdt[,colnt]) & !is.null(testdt[,colnt]))
  }
  
  
  ## Remove nulls and NAs for date column, ref parameter column, lat and long columns
  testdt <- subset(testdt, !is.na(testdt[,xt()])& !is.null(testdt[,xt()]) & !is.na(testdt[,yt()]) & !is.null(testdt[,yt()]) & !is.na(testdt[,dt()]) & !is.null(testdt[,dt()]))
  testdt$Month <- month(testdt[,dt()])
  testdt$Monthname<- month.name[testdt$Month]
  testdt$Year <- year(testdt[,dt()])
  testdt <- arrange(testdt,Month,Year)
  testdt
  
})

### Now there is a single data table for Test and for Reference, to be used for date avaliability and for statistics




## Select Test Month

output$month <- renderUI({
  validate (
    need(input$buffer != "","")
  )
    selectInput("month","Select the month you would like to view",choices=unique(testdt()$Monthname))
  })

## Select Test Year

output$testyear <- renderUI({
    req(input$month)
    tem<- testdt()
    tem <- subset(tem,tem$Monthname==input$month)
    tem <- arrange(tem,tem$Year)
    selectInput("testyear","Select the test year",choices=unique(tem$Year))
  })

## Select Reference Data year Range

output$yearrange <- renderUI({
    tempr <- subset(referencedt(),referencedt()$Monthname==input$month)
    ys <- unique(tempr$Year)
    sliderInput("yearrange","Years included in the slider's range will be used as a long-term baseline",min=min(ys),max=max(ys),value=c(min(ys),max(ys)),step=1,sep="")
  })


## generate table with just names of selected pairs and their respective lat/longs

pairt <- reactive({
  validate (
    need(input$buffer != "","")
  )
  req(input$spair)
  getxyref <- subset(refdata(),refdata()[,s()]==pairs()[input$spair,1])
  getxytest <- subset(testdata(),testdata()[,st()]==pairs()[input$spair,2])
  xref <- getxyref[1,x()]
  yref <- getxyref[1,y()]
  xtest <- as.numeric(getxytest[1,xt()])
  ytest <- as.numeric(getxytest[1,yt()])
  pairt <- data.frame("name"=c(pairs()[input$spair,1],pairs()[input$spair,2]),"x"=c(xref,xtest),"y"=c(yref,ytest))
  row.names(pairt) <- c()
  pairt$x <- as.numeric(pairt$x)
  pairt$y <- as.numeric(pairt$y)
  pairt ## Will use this dataframe in the leaflet output to show only the location of the pair selected
})



output$pairmap <- renderLeaflet({
  validate (
    need(input$spair != "","")
  )
  leaflet() %>%
    addTiles()
})
observe({
leafletProxy("pairmap",data=pairt()) %>%
  clearMarkers() %>%
  addMarkers(lng=~pairt()$x,lat=~pairt()$y,popup = ~as.character(pairt()$name)) %>%
    fitBounds(lng1 = max(pairt()$x),lat1 = max(pairt()$y),
              lng2 = min(pairt()$x),lat2 = min(pairt()$y))

})


### Need to add action button for after test month, test year, and reference year range have been selected. 
### After action button is pressed, referencedt and testdt will be filtered

Rfinal <- reactive({
 
    rfinal <- referencedt()
    rfinal <- subset(rfinal, rfinal$Year >= input$yearrange[1] & rfinal$Year <= input$yearrange[2] & rfinal$Monthname == input$month)
    rfinal
})

Tfinal <- reactive({
  
    tfinal <- testdt()
    tfinal <- subset(tfinal,tfinal$Monthname==input$month) # took out tfinal$Year == input$testyear as a test
    tfinal

})

### Get column indice for reference and test
colr <- reactive ({

  if (input$rformat=="Wide"){
    colr <- which(colnames(refdata2())==input$refp)
  }
  else{
    colr <- v()
  }
  colr
})

colt <- reactive({

  if (input$tformat == "Wide"){
  colt <- which(colnames(testdata2())==input$tesp)
  }
  else {
    colt <- vt()
  }
  colt
})

observeEvent(input$perftest,{
  
pscresult <<- psctest(Rfinal(),colr(),Tfinal(),colt())

output$resulttext <- renderText({
  pscresult
})


})



### Trying to do single graph with data from both files
observeEvent(input$perftest,{
  
output$combplot <- renderPlot({
  isolate(Rfinal <- Rfinal())
  isolate(Rfinal$Site <- Rfinal()[,s()])
  isolate(Tfinal <- Tfinal())
  isolate(Tfinal$Site <- Tfinal()[,st()])
  
  ## Combine both files into a single file with date, parameter value, and column for name of site
  isolate(Rfinal <- Rfinal[,c(d(),colr(),s())])
  isolate(Tfinal <- Tfinal[,c(dt(),colt(),st())])
  names(Tfinal) <- c("Date","Value","Site")
  names(Rfinal) <- c("Date","Value","Site")
  isolate(Final <- bind_rows(Rfinal,Tfinal))
  
  
    #ggplot() + geom_point(data=Rfinal(),aes(Rfinal()[,d()],Rfinal()[,colr()])) + geom_point(data=Tfinal(),aes(Tfinal()[,dt()],Tfinal()[,colt()])) + xlab("Date") + ylab("Parameter Value") + theme_bw() + ggtitle(paste("Reference Data - ",input$month,"vs. ","Test Data - ",input$tesp)) + labs(colour="Site\n")
    plot <- isolate(ggplot(Final,aes(x=Final$Date,y=Final$Value)) + theme_bw() + geom_point(aes(colour=Final$Site),size=4) + labs(colour="Site Name") + xlab("Date") + ylab("Parameter Value"))
    plot <- isolate(plot + ggtitle(paste(input$month,",",input$testyear,"Test Data -",input$tesp,"Against Reference Baseline")))
    plot
})
})


### END OF SERVER CODE
}










