library(shinyBS)
library(leaflet)
library(rgeos)
library(geosphere)
library(sp)
library(plotly)
library(shinythemes)
library(htmltools)


tags$head(tags$style(css))


navbarPage("CWDAT - The Community Water Data Analysis Tool",
          selected="About CWDAT",
           windowTitle = "CWDAT"
           ,
           theme= shinytheme("cerulean"),
           useShinyalert(),
           tabPanel(
             "About CWDAT",
             fluidPage(
               h1("Welcome to the Community Water Data Analysis Tool",align="center",style = "font-size:500px;"),
               br(),
               hr(),
               br(),
               h3("What is CWDAT?"),
               br(),
               div("CWDAT is an interative web tool which visualizes,
                  summarizes, and generates reports from user-provided
                   water quality data. It was created using the
                   R/Shiny platform.",style = "color:black"),
              br(),
               h3("Who can use CWDAT?"),
               
               div("Although this tool is intended for
               community-based water quality monitoring initiatives,
               anyone can use CWDAT for free to visualize their 
                  water quality data",style = "color:black"),
               br(),
              h3("Why should I use CWDAT?"),
              div("CWDAT provides a medium for the
                 immediate, independant visualization and
                 investigation of your data.It also
                 supports reporting, to help you and your initiaitve
                 communicate your findings with your community if
                 you so choose.
                 CWDAT encourages transparency through the use of
                 an open-source platform and by providing its source
                 code to the public for free.",style = "color:black"),
              br(),
              h3("How do I use CWDAT?"),
              fluidRow(
              column(6,
                HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/413FuQUftaQ" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>',align="center")
              ),
              column(6,
                div(
                  "CWDAT accepts data in 'long' and 'wide formats. All files must be 
                  comma seperated data files, denoted by the extension '.csv'. Microsoft
                  Office Excel files can be easily converted into '.csv' files for use
                  in CWDAT. See below for some more details and requirements for your
                  data in CWDAT:"
                ),
                div(
                  tags$ul(
                    tags$li("Your .csv file must include a seperate column for each
                            of the following attributes: site unique identifier; 
                            latitude; longitude; and date"), 
                    br(),
                    tags$li("Latitude and longitude values must be in decimal degrees
                            using the WGS84 Gerographic Coordinate System. This ensures that,
                            no matter where in the world your data was collected, CWDAT can map
                            your monitoring locations."),
                    br(),
                    tags$li("If you are using a .csv in the 'long' format, you will
                            also need a specific column for your parameter names and
                            a specific column which contains your data values."),
                    br(),
                    tags$li( "If you wish to explore CWDAT before
                    adding your own data, you may use CWDAT's built-in data 
                    files as shown in the instruction video. The built-in 'reference' and 'test'
                    datasets are attributlable to their respective sources:"),
                    br(),
                    tags$li("Environment and Climate Change Canada (2016). Lower Mackenzie River Basin Long-term Water Quality Monitoring Data - Canada's North (dataset). 
                            Record ID 0177c195-13a8-4078-aa85-80b17e9e2cfe. Accessed from open.canada.ca. This dataset is made available under the Open Government License - Canada: 
                            https://open.canada.ca/en/open-government-licence-canada. See below for data disclaimer."),
                    br(),
                    tags$li("NWT-wide Community-based Water Quality Monitoring, Environment and Natural Resources, Government of the Northwest Territories. (2019).
                            DOI: 10.25976/4der-gd31. See below for data disclaimer."),
                    br(),
                    tags$li("Data Disclaimer (Reference dataset): 
                            Environment and Climate Change Canada employs every reasonable effort whenever feasible, to ensure the currency, accuracy and 
                            precision of the information provided. However, there are some limitations due to the sources of the data and the technology 
                            used in its processing and management. Furthermore, the material or any data derived using the data is subject to interpretation. 
                            Users are responsible for verifying that the supplied material is appropriate for the use or application for which they wish to employ it. 
                            The water quality and aquatic ecosystem data released may include provisional and/or validated data. Provisional data should be 
                            considered with greater caution as it has yet to be validated with quality control and quality assurance activities, 
                            which may result in significant change made to the data. Environment and Climate Change Canada makes no representation 
                            or warranty of any kind, either expressed or implied, as to the information presented, nor as to its fitness for any particular use. 
                            Environment and Climate Change Canada does not assume nor accept any liability arising from any use of the information 
                            and applications used to display or use the information. The data are not intended for any legal, or navigation purposes. 
                            The data may change from time to time as updated information is received. If any information seems to be incorrect, 
                            raises concerns, or for more information on this data, please communicate with the Environment and Climate Change Canada 
                            contact provided within the application."),
                    br(),
                    tags$li("Data Disclaimer (Test dataset):Information on this website is provided as a public service by the Government of the Northwest Territories. 
                    This information is made available without warranty of any kind, either expressed or implied. 
                    By accessing and using the website, users acknowledge and agree that use of the website and the content is entirely at their 
                    own risk. The Government of the Northwest Territories, including their employees, agents, and assignees shall not be liable for 
                    losses or damages of any kind that may arise as a result of use of information provided on the website.
                    The material on this website is covered by the provisions of the Copyright Act, and by Canadian laws, policies, regulations and 
                    international agreements. Material may not be used or reproduced for commercial purposes without the prior written consent 
                    arranged by the publisher of the material. If it is reproduced or redistributed for non-commercial purposes, copyright must be appropriately acknowledged."
                  )
                  )
                )
              )
              )
   
             )
           ),
                      navbarMenu("Reference Dataset",
                                 tabPanel("Data Upload and Properties",
                               sidebarLayout(
                                 sidebarPanel(
                                   uiOutput("whichfile"),
                                   conditionalPanel(
                                     condition="input.whichfile=='Mine'",
                                     fileInput("rf", "Please Upload a CSV File",
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv") 
                                               )
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
                                 conditionalPanel(
                                   condition="input.rformat=='Wide'",
                                   uiOutput("RparsW")
                                 ),
                                 uiOutput("rconfirm"),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br()
                               ),
                               mainPanel(
                                 DT::dataTableOutput("rtable") %>% withSpinner(color="#0dc5c1")
                                 )
                              
                               )),
                      tabPanel("Spatial Visualization",
                               fluidPage(
                                 #tags$head(tags$style(css)),
                                 
                                 fluidRow(
                                   column(3,leafletOutput("Rmap")%>% withSpinner(color="#0dc5c1")),
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
                                   column(12,div(id="table_id",style = 'overflow-x: scroll',DT::dataTableOutput("table")%>% withSpinner(color="#0dc5c1")) 
                                          )
                                 )
                                 
                                 
                                   
                                 )
                               
                      ),
                               
                      tabPanel("Graphic Visualization",
                               sidebarLayout(
                                 sidebarPanel(
                                   tags$head(tags$style(".checkbox-inline {margin: 0 !important;}")),
                                   radioButtons("vars","Select the number of variables you would like to visualize",c(1,2)),## At least one variable will always be displayed
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
                                   
                                  
                                   #actionButton("refresh", "Refresh map and update settings"),
                                   downloadButton('downloadPlot','Click to download your plot')
                                   ),
                                 mainPanel(
                                   
                                   plotOutput("refout") %>% withSpinner(color="#0dc5c1")
                                   
                                            )
                                 
                                           
                                 
                                 
                               
                               
                               )),
                      tabPanel("Statistics",
                                   tabsetPanel(
                                     tabPanel("Summary Statistics",
                                              mainPanel(
                                                fluidRow(
                                                  column(4,uiOutput("sumselsite")%>% withSpinner(color="#0dc5c1")),
                                                  column(4,uiOutput("sumpar")%>% withSpinner(color="#0dc5c1")),
                                                  column(4,downloadButton("Statsreport", "Click to generate a PDF report for your selected variable for all sites")%>% withSpinner(color="#0dc5c1"))
                                                  
                                                  ),
                                                br(),
                                              fluidRow(
                                                column(12,verbatimTextOutput("sum") %>% withSpinner(color="#0dc5c1")) 
                                                ),
                                              br(),
                                              fluidRow(
                                                column(12,plotOutput("boxp")%>% withSpinner(color="#0dc5c1")) 
                                                ),
                                              fluidRow(
                                                column(12,plotOutput("mboxp")%>% withSpinner(color="#0dc5c1")) 
                                                ),
                                              fluidRow(
                                                column(3,downloadButton("downmboxp","Click to download this graph"))
                                              ),
                                              br(),
                                              br(),
                                              
                                              fluidRow(
                                                column(12,plotOutput("yboxp")%>% withSpinner(color="#0dc5c1")) 
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
                                   uiOutput("MonthYear"),
                                   uiOutput("singrpR"),
                                   
                                   conditionalPanel(
                          
                                    condition="input.singrpr=='A'",
                                     uiOutput("sitesR")
                                     ),
                                   conditionalPanel(
                                     condition="input.singrpr=='C'",
                                     uiOutput("gcR"),
                                     actionButton("refresh2", "Submit group column")
                                     ),
                                   conditionalPanel(
                                     condition="input.singrpr=='C'",
                                     uiOutput("gsR") %>% withSpinner()
                                     ),
                                   
                                   
                                   
                                   downloadButton("downloadPlotT","Click to download your plot"),
                                   
                                   
                                   downloadButton("TCSreport", "Click to generate a PDF report")
                                     
                                   
                                    
                                   
                                   
                                   
                                   ),
                                 mainPanel(
                                   plotOutput("tcPr") %>% withSpinner()
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
                                   uiOutput("whichfileT"),
                                   conditionalPanel(
                                     condition="input.whichfileT=='Mine'",
                                   fileInput("tf", "Please Upload Test Data CSV File",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")
                                   )
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
                                   conditionalPanel(
                                     condition="input.tformat=='Wide'",
                                     uiOutput("TparsW")
                                   ),
                                   uiOutput("tconfirm")
                                   
                                 ),
                                 mainPanel(
                                   DT::dataTableOutput("ttable") %>% withSpinner(color="#0dc5c1")
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
                                   column(12,div(id="tablet_id",style='overflow-x: scroll',DT::dataTableOutput("tablet"))) %>% withSpinner(color="#0dc5c1")
                                 )
                                 
                               )
                      ),
                               
                      tabPanel("Graphic Visualization",
                               
                               sidebarLayout(
                                 sidebarPanel(
                                   radioButtons("varst","Select the number of variables you would like to visualize",c(1,2)),
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
                                   
                                   
                                   #actionButton("refresht", "Refresh map and update settings"),
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
                                              column(5,uiOutput("sumselsitet") %>% withSpinner(color="#0dc5c1")),
                                              column(6,uiOutput("sumpart")%>% withSpinner(color="#0dc5c1")),
                                              column(4,downloadButton("StatsreportT", "Click to generate a PDF report for your selected variable for all sites")%>% withSpinner(color="#0dc5c1"))
                                              ),
                                            fluidRow(
                                              column(12,verbatimTextOutput("sumt")%>% withSpinner(color="#0dc5c1"))
                                            ),
                                            br(),
                                            fluidRow(
                                              column(12,plotOutput("boxpt")%>% withSpinner(color="#0dc5c1"))
                                            ),
                                            fluidRow(
                                              column(12,plotOutput("mboxpt")%>% withSpinner(color="#0dc5c1"))
                                            ),
                                            fluidRow(
                                              column(3,downloadButton("downmboxpt","Click to download this graph"))
                                            ),
                                            br(),
                                            br(),
                                            fluidRow(
                                              column(12,plotOutput("yboxpt")%>% withSpinner(color="#0dc5c1"))
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
                                   uiOutput("MonthYearT"),
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
                                   downloadButton("downloadPlotTt","Click to download your plot"),
                                   downloadButton("TCSreportT", "Click to generate a PDF report")
                                   
                                   
                                   
                                   
                                   
                                   
                                 ),
                                 mainPanel(
                                   plotOutput("tcPrt") %>% withSpinner()
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