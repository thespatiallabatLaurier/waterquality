library(shinyBS)
library(leaflet)
library(rgeos)
library(geosphere)
library(sp)
library(plotly)

tags$head(tags$style(css))


navbarPage("",
           tabPanel("Water Quality Data Analysis Tool - PRE-ALPHA"),
                      navbarMenu("Reference Dataset",
                                 tabPanel("Data Upload and Properties",
                               sidebarLayout(
                                 sidebarPanel(
                                   
                                   fileInput("rf", "Please Upload Reference Data CSV File",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")
                                 ),
                                 
                                 uiOutput("Rsid"), 
                                 uiOutput("Rdate"),
                                 #uiOutput("Rdfo"),
                                 uiOutput("Rlong"),
                                 uiOutput("Rlat"),
                                 uiOutput("Rformat"),
                                 conditionalPanel(
                                   condition="input.rformat=='Long'",
                                   uiOutput("Rpars"),
                                   uiOutput("Rvals")
                                 ),
                                 actionButton("rconfirm","Submit column selections")
                               ),
                               mainPanel(
                                 DT::dataTableOutput("rtable")
                                 )
                              
                               )),
                      tabPanel("Spatial Visualization",
                               fluidPage(
                                 #tags$head(tags$style(css)),
                                 
                                 fluidRow(
                                   column(3,leafletOutput("Rmap")),
                                   column(2,uiOutput("pselect"),
                                          uiOutput("pselectL")
                                          ),
                                   column(2,uiOutput("addguide"),
                                          conditionalPanel(
                                            condition="input.addguide=='Yes'",
                                            uiOutput("selguide"),
                                            uiOutput("hardW"),
                                            uiOutput("hardL"),
                                            uiOutput("phW"),
                                            uiOutput("phL")
                                          )
                                          ),
                                   column(5,plotlyOutput("mplot"))
                                 
                                  
                                          
                                 ),
                                 br(),
                                 
                                 fluidRow(
                                   column(12,div(id="table_id",style = 'overflow-x: scroll',DT::dataTableOutput("table")) 
                                          )
                                 )
                                 
                                 
                                   
                                 )
                               
                      ),
                               
                      tabPanel("Graphic Visualization",
                               sidebarLayout(
                                 sidebarPanel(
                                   tags$head(tags$style(".checkbox-inline {margin: 0 !important;}")),
                                   selectInput("vars","Select the number of variables you would like to visualize",c(1,2)),## At least one variable will always be displayed
                                   ########### Insert Radio Button Selection#####################
                                   uiOutput("selectsites"),
                                   uiOutput("filtermonth"),
                                   uiOutput("filteryear"),
                                   
                                   uiOutput("cselect1"),
                                   conditionalPanel(
                                     condition="input.vars==2",
                                     uiOutput("cselect2")),
                                   ## Univariate visualization options:
                                   conditionalPanel(
                                     condition="input.vars==1",
                                     radioButtons("ptype1", "Choose plot type:",list("Histogram", "Density", "Discrete Bar Plot", "Boxplot"))
                                   )
                                   ,
                                   conditionalPanel(
                                     condition="input.ptype1=='Discrete Bar Plot'&input.vars==1",
                                     sliderInput("width","Slide to adjust the width of barplot columns",0.01,1.00,value=0.01,step=0.01)
                                   ),
                                   
                                   textInput("title","Enter the name of the graph",value=""),
                                   
                                   conditionalPanel(
                                     condition="input.vars==1",
                                     uiOutput("units")
                                   ),
                                   
                                   
                                   
                                   ## Bivariate visualization options:
                                   conditionalPanel(
                                     condition="input.vars==2",
                                     radioButtons("ptype2", "Choose plot type:",list("Point", "Line","Discrete Box Plot"))
                                   )
                                   ,
                                  
                                   
                                   
                                   
                                   
                                   
                                   
                                   
          
                                     
                                   
                                   ################################## Bivariate Visualization of Reference Data #################
                                   
                                   ## Optional selection of second variable
                                   
                                   
                                  
                                   conditionalPanel(
                                     condition="input.vars==2",
                                     uiOutput("units2x")
                                     ),
                                   conditionalPanel(
                                     condition="input.vars==2",
                                     uiOutput("units2y")
                                   ),
                                   
                                   
                                   
                                   ########################## Overall Controls for Visualization of Reference Data #######################33
                                   
                                  
                                   actionButton("refresh", "Refresh map and update settings"),
                                   downloadButton('downloadPlot','Click to download your plot')
                                   ),
                                 mainPanel(
                                   
                                   plotOutput("refout")
                                   
                                            )
                                 
                                           
                                 
                                 
                               
                               
                               )),
                      tabPanel("Statistics",
                                   tabsetPanel(
                                     tabPanel("Summary Statistics",
                                              mainPanel(
                                                fluidRow(
                                                  column(5,uiOutput("sumselsite")),
                                                  column(6,uiOutput("sumpar"))
                                                  ),
                                              fluidRow(
                                                column(12,verbatimTextOutput("sum"))
                                                ),
                                              br(),
                                              fluidRow(
                                                column(12,plotOutput("boxp"))
                                                ),
                                              fluidRow(
                                                column(12,plotOutput("mboxp"))
                                                ),
                                              fluidRow(
                                                column(3,downloadButton("downmboxp","Click to download this graph"))
                                              ),
                                              br(),
                                              br(),
                                              
                                              fluidRow(
                                                column(12,plotOutput("yboxp"))
                                                ),
                                              fluidRow(
                                                column(3,downloadButton("downyboxp","Click to download this graph"))
                                              )
                                              )
                                     ),
                                              
                                     tabPanel("Normality Testing",
                                              mainPanel(
                                                fluidRow(
                                                  column(6,uiOutput("sumselsite2")),
                                                  column(6,uiOutput("sumpar2"))
                                                  ),
                                                fluidRow(
                                                  column(12,plotOutput("sumhist"))
                                                  ),
                                                fluidRow(
                                                  column(5,offset=1,h6("Shapiro-Wilk Test for Normality - Non-Transformed Data"),verbatimTextOutput("sw")
                                                         ),
                          
                                                  column(5,offset=1,h6("Shapiro-Wilk Test for Normality - Non-Transformed Data"),verbatimTextOutput("swl"))
                                                         
                                                )
                                              )
                                     )
                                   )
                      
                                                
                                                
                                              
                                     
                                     
                               
                               
                               
                               ),
                      tabPanel("Temporal Coverage Summary",
                               sidebarLayout(
                                 sidebarPanel(
                                   uiOutput("singrpR"),
                                   
                                   conditionalPanel(
                          
                                    condition="input.singrpr=='A'",
                                     uiOutput("sitesR")
                                     ),
                                   conditionalPanel(
                                     condition="input.singrpr=='C'",
                                     uiOutput("gcR")
                                     ),
                                   conditionalPanel(
                                     condition="input.singrpr=='C'",
                                     uiOutput("gsR")
                                     ),
                                   
                                   actionButton("refresh2", "Refresh plot"),
                                   downloadButton("downloadPlotT","Click to download your plot")
                                     
                                   
                                    
                                   
                                   
                                   
                                   ),
                                 mainPanel(
                                   plotOutput("tcPr")
                                   )
                                 )
                               ),
                      tabPanel("CCME WQI Calculations",
                               sidebarLayout(
                                 sidebarPanel(
                                   h4("To calculate the CCME WQI, please provide the following information:"),
                                  uiOutput("alk"),
                                  uiOutput("ips")
                                   
                                 ),
                                 mainPanel(
                                   leafletOutput("imap")
                                   
                                 )
                               )
                               )
                      
                      
                      
                      
                      ),
           
######################################################################################################################################
#####################################################################################################################################

           navbarMenu("Test Dataset",
                      tabPanel("Data Upload and Properties",
                               sidebarLayout(
                                 sidebarPanel(
                                   fileInput("tf", "Please Upload Test Data CSV File",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")
                                   ),
                                   
                                   uiOutput("Tsid"),
                                   uiOutput("Tdate"),
                                   uiOutput("Tlong"),
                                   uiOutput("Tlat"),
                                   uiOutput("Tformat"),
                                   conditionalPanel(
                                     condition="input.tformat=='Long'",
                                     uiOutput("Tpars"),
                                     uiOutput("Tvals")
                                   ),
                                   actionButton("tconfirm","Submit column selections")
                                   
                                 ),
                                 mainPanel(
                                   DT::dataTableOutput("ttable")
                                 )
                                 
                               )
                               
                               
                               ),
                      tabPanel("Spatial Visualization",
                               fluidPage(
                                 #tags$head(tags$style(css)),
                                 fluidRow(
                                   column(4,leafletOutput("Tmap")),
                                   column(2,uiOutput("pselectTw"), uiOutput("pselectTL")),
                                   column(6,plotlyOutput("mplott"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(12,div(id="tablet_id",style='overflow-x: scroll',DT::dataTableOutput("tablet")))
                                 )
                                 
                               )
                      ),
                               
                      tabPanel("Graphic Visualization",
                               
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput("varst","Select the number of variables you would like to visualize",c(1,2)),
                                   ## At least one variable will always be displayed
                                   uiOutput("selectsitest"),
                                   uiOutput("filtermontht"),
                                   uiOutput("filteryeart"),
                                   uiOutput("cselect1t"),
                                   conditionalPanel(
                                     condition="input.varst==2",
                                     uiOutput("cselect2t")),
                                   ## Univariate visualization options:
                                   conditionalPanel(
                                     condition="input.varst==1",
                                     radioButtons("ptype1t", "Choose plot type:",list("Histogram", "Density", "Discrete Bar Plot", "Boxplot"))
                                   )
                                   ,
                                   ## Bin size selection if histogram selected for univariate display
                                   # conditionalPanel(
                                   #   condition="input.ptype1t=='Histogram'& input.varst==1",
                                   #   sliderInput("binsizet","Select bin size for histogram",1,500,value=10,step=1)  
                                   #   
                                   # ),
                                   
                                   ## Kernel type selection if density plot is selected for univariate display
                                   # conditionalPanel(
                                   #   condition="input.ptype1t=='Density'& input.varst==1",
                                   #   selectInput("kernt","Select kernel for density visualization",list("gaussian","epanechnikov", "rectangular","triangular", "biweight",
                                   #                                                                      "cosine", "optcosine"))
                                   # ),
                                   
                                   ## Width selection if bar plot is selected for univariate display
                                   conditionalPanel(
                                     condition="input.ptype1t=='Discrete Bar Plot'&input.varst==1",
                                     sliderInput("widtht","Slide to adjust the width of barplot columns",0.01,1.00,value=0.01,step=0.01)
                                   ),
                                   ## Bivariate visualization options:
                                   conditionalPanel(
                                     condition="input.varst==2",
                                     radioButtons("ptype2t", "Choose plot type:",list("Point", "Line","Discrete Box Plot"))
                                   )
                                   ,
                                   
                                   
                                   
                                   textInput("titlet","Enter the name of the graph",value=""),
                                   
                                   
                                   
                                  
                                   conditionalPanel(
                                     condition="input.varst==1",
                                     uiOutput("unitst")
                                   ),
                                   
                                  
                                   
                                   
                                  
                                   conditionalPanel(
                                     condition="input.varst==2",
                                     uiOutput("units2xt")
                                   ),
                                   conditionalPanel(
                                     condition="input.varst==2",
                                     uiOutput("units2yt")
                                   ),
                                   
                                   
                                   
                                   ########################## Overall Controls for Visualization of Test Data #######################33
                                   
                                   
                                   actionButton("refresht", "Refresh map and update settings"),
                                   downloadButton('downloadPlott','Click to download your plot')
                                 ),
                                 mainPanel(
                                   
                                   plotOutput("refoutt")
                                   
                                 )
                                 
                                 
                               )
                               
                               ),
                      tabPanel("Statistics",
                               tabsetPanel(
                                 tabPanel("Summary Statistics",
                                          mainPanel(
                                            fluidRow(
                                              column(5,uiOutput("sumselsitet")),
                                              column(6,uiOutput("sumpart"))
                                              ),
                                            fluidRow(
                                              column(12,verbatimTextOutput("sumt"))
                                            ),
                                            br(),
                                            fluidRow(
                                              column(12,plotOutput("boxpt"))
                                            ),
                                            fluidRow(
                                              column(12,plotOutput("mboxpt"))
                                            ),
                                            fluidRow(
                                              column(3,downloadButton("downmboxpt","Click to download this graph"))
                                            ),
                                            br(),
                                            br(),
                                            fluidRow(
                                              column(12,plotOutput("yboxpt"))
                                            ),
                                            fluidRow(
                                              column(3,downloadButton("downyboxpt","Click to download this graph"))
                                            )
                                          )
                                 ),
                                 
                                 tabPanel("Normality Testing",
                                          mainPanel(
                                            fluidRow(
                                              column(6,uiOutput("sumselsite2t")),
                                              column(6,uiOutput("sumpar2t"))
                                            ),
                                            fluidRow(
                                              column(12,plotOutput("sumhistt"))
                                            ),
                                            fluidRow(
                                              column(5,offset=1,h6("Shapiro-Wilk Test for Normality - Non-Transformed Data"),verbatimTextOutput("swt")
                                              ),
                                              
                                              column(5,offset=1,h6("Shapiro-Wilk Test for Normality - Non-Transformed Data"),verbatimTextOutput("swlt"))
                                              
                                            )
                                          )
                                 )
                                 
                                 
                               )
      
                               
                      ),         
                               
                      tabPanel("Temporal Coverage Summary",
                               sidebarLayout(
                                 sidebarPanel(
                                   uiOutput("singrpRt"),
                                   
                                   conditionalPanel(
                                     
                                     condition="input.singrprt=='A'",
                                     uiOutput("sitesRt")
                                   ),
                                   conditionalPanel(
                                     condition="input.singrprt=='C'",
                                     uiOutput("gcRt")
                                   ),
                                   conditionalPanel(
                                     condition="input.singrprt=='C'",
                                     uiOutput("gsRt")
                                   ),
                                   
                                   actionButton("refresh3", "Refresh plot"),
                                   downloadButton("downloadPlotTt","Click to download your plot")
                                   
                                   
                                   
                                   
                                   
                                   
                                 ),
                                 mainPanel(
                                   plotOutput("tcPrt")
                                 )
                               )
                            
                               
                               
                               )
           ),
           
                      
           navbarMenu("Paired Site Comparisons",  
                      tabPanel("Site Pairings",
                               sidebarLayout(
                                 sidebarPanel(
                                   #uiOutput("proj"),
                                   uiOutput("buffer"),
                                   br(),
                                  
                                   textOutput("paircount"),
                                   br(),
                                   br(),
                                   
                                   leafletOutput("pairmap"),
                                   br(),
                                   br(),
                                   actionButton("perftest", "Click to compare")
                                   ),
                                 mainPanel(
                                   fluidRow(
                                     column(12,h4("List of Site Pairs"),DT::dataTableOutput("pair"))
                                     ),
                                   br(),
                                   br(),
                                  
                                   
                                   
                                   
                                   fluidRow(
                                     column(3,offset=1,uiOutput("spair")),
                                     column(3,offset=1,uiOutput("refp")),
                                     column(3,offset=1,uiOutput("tesp"))
                                     ),
                                   br(),
                                   br(),
                                   
                                   fluidRow(
                                     column(3,offset=1,uiOutput("month")),
                                     #column(3,offset=1,uiOutput("testyear")),
                                     column(3,offset=1,uiOutput("yearrange"))   
                                       ),
                                   br(),
                                   
                                   fluidRow(
                                     column(12,textOutput("resulttext"))
                                   ),
                                   br(),
                                   br(),
                                   fluidRow(
                                     # column(5,plotOutput("testplot")),
                                     # column(5,offset=1,plotOutput("referplot"))
                                     column(12,plotOutput("combplot"))
                                   ),
                                   br(),
                                   br(),
                                   
                                   br()
                                   

                                   )
                                 
                               )
                      )
                    
           
))