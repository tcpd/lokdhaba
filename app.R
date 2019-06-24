library(shiny)
library(reshape2)
library(leaflet)
library(plotly)
library(shinyjs)
library(mapview)
library(shinydashboard)
library(DT)
source("AE/aeOptionsInput.R")
source("GE/geOptionsInput.R")
source("DataDownloader/dataDownloadOptions.R")
source("DataDownloader/browseDataOptions.R")
source("utils/utils-classes.R")
source("utils/utils-lokdhaba.R")
source("utils/utils-charts-ui.R")
library(rgdal)
library(dplyr)
library(leaflet.extras)
shiny::addResourcePath("shinyjs", "www/assets/shinyjs")

ui<-function(request){
  navbarPage(
	#      tags$head(
	# 	       tags$link(rel= "stylesheet",type="text/css",href="shared/shiny.css"),
	# 	       tags$link(rel="stylesheet",type="text/css",href="assets/style.css")
	# 			 
	# 	       ),
	     id="Page"
	     ,title=div(img(src="www/assets/logo.png"))
             ,windowTitle = "LokDhaba"
             ,useShinyjs()
             ,selected="EDV"
             ,tabPanel(tagList(br(),h4("Election Data Visualization")),
                       dashboardPage(
                         dashboardHeader(disable = TRUE),
                         dashboardSidebar(
                           tabsetPanel(id="electionType",
                                       selected = "AE",
                                       tabPanel("General Elections",
                                                tagList(
                                                  uiOutput("ge_uitype_selection"),
                                                  uiOutput("ge_filter_selection")
                                                )
                                                ,value="GE"),
                                       tabPanel("Assembly Elections",
                                                tagList(
                                                  uiOutput("state_selection"),
                                                  uiOutput("ae_uitype_selection"),
                                                  uiOutput("ae_filter_selection"))
                                                ,value="AE")
                           )),
                         dashboardBody(
                           plotlyOutput("distPlot")
                           ,leafletOutput("mapPlot")
                           ,bookmarkButton(id="bookmark_edv")
                           
                         )
                         
                       )
                       ,value="EDV"),
             tabPanel(tagList(br(),h4("Data Download")),
                      basicPage(
                        tagList(
                          selectizeInput("dd_electiontype_selector","Election Type",choices = c("Select Election Type"="","General Elections"="GE","Assembly Elections"="AE")),
                          conditionalPanel(
                            condition="input.dd_electiontype_selector!=''",
                            selectizeInput("dd_state_selector","State Name",choices=c("Select State"=""))
                          ),
                          conditionalPanel(
                            condition="input.dd_state_selector!=''",
                            checkboxGroupInput("dd_year_selector","Years",choices=c())
                          ),
                          conditionalPanel(
                            condition="input.dd_year_selector.length!=0",
                            DT::dataTableOutput("dd_variablenames_selector")
                            , downloadButton("id","label")
                            
                          )
                        )
                      )
                      ,value="DLD"),
             tabPanel(tagList(br(),h4("Browse Data")),
                      basicPage(
                        tagList(
                          selectizeInput("bd_electiontype_selector","Election Type",choices = c("Select Election Type"="","General Elections"="GE","Assembly Elections"="AE")),
                          
                          conditionalPanel(
                            condition="input.bd_electiontype_selector!=''",
                            selectizeInput("bd_state_selector","State Name",choices=c("Select State"=""))
                          ),
                          conditionalPanel(
                            condition="input.bd_state_selector!=''",
                            checkboxGroupInput("bd_year_selector","Years",choices=c())
                            ,bookmarkButton(id="bookmark_bd")
                          ),
                          conditionalPanel(
                            condition="input.bd_year_selector.length!=0",
                            DT::dataTableOutput("bd_variablenames_selector")
                            
                          )
                        )
                        )
                      ,value = "BRS"),
             tabPanel(tagList(br(),h4("How to Cite Us")),
                      basicPage(
                        htmlOutput("howtocite")
                      )
                      ,value="CITE")
             
  )
  
}

shinyServer <- function(input, output, session) {
  
  #browser()
  
  ########################Allow server to reconnect to client after network
  session$allowReconnect("force") 
  ###########failure provided that the client browser is still open
  
  # ###########Adds resource path
  # addResourcePath("myassets", "www/assets")
 #useShinyjs(html = TRUE)  
  #######################Connection reset management#######################
  ######## When server process restarts after connection failure of over 15 
  ####### seconds then all inputs are sent again by browser to server.
  ####### We need to capture them here so that server can again start rendering
  ####### from the last saved state of the browser.. This solves the problem of 
  ####### connection failure.
  ##Create an object of connectionmanager class
  cls<-getRefClass("ConnectionRestoreManager")
  conmanager<-cls$new()
  
  isolate({
    lapply(names(input),function(x){
      #print(paste0('value of element ',x,' is ',input[[x]]))
      conmanager$setval(x,input[[x]])
      #shinyjs::reset(x)
      #resetvalues[[x]]<<-input[[x]]
    })
    #print(paste(names(query), query, sep = "=", collapse=", "))
  })

  setBookmarkExclude(c("bookmark_edv", "bookmark_bd"))
  observeEvent(input$bookmark_edv, {
    session$doBookmark()
  })
  
  observeEvent(input$bookmark_bd, {
    session$doBookmark()
  })
  onRestore(function(state) {
      
    updateTabsetPanel(session,"Page",selected = state$input$Page)
    updateTabsetPanel(session,"electionType",selected = state$input$electionType)
    lapply(names(state$input),function(x){
      #print(paste0('value of element ',x,' is ',input[[x]]))
      conmanager$setval(x,state$input[[x]])
      #shinyjs::reset(x)
      #resetvalues[[x]]<<-input[[x]]
    })
    #browser()
    
  })

  #print(conmanager$restoredvals)
  
  #print('----inputs---')

  #isolate({
   # lapply(names(input),function(x){
    #  print(paste0('value of element ',x,' is ',input[[x]]))
      # conmanager$setval(x,input[[x]])
      # shinyjs::reset(x)
      #resetvalues[[x]]<<-input[[x]]
   # })
    #print(paste(names(query), query, sep = "=", collapse=", "))
  #})
  
  
  ###Call aeOptionsInput.R's function
  #pass input, output and session and connection restore manager object. It does the following,
  #1. render AE specific ui components, state_selection, ae_uitype_selection, and ae_filter_selection(it's children will do that)
  #2. During this process it will also callmodule for each UI component.  
  
  aeOptionsInput(input,output,session,"AE/",conmanager)
  #aeOptionsInput(input,output,session,"AE/")
  
  ###Call geoptionsInput.R's function
  #pass input, output and session. That function will
  ##It does the following 
  #1. render GE specific ui components, ge_uitype_selection, and ge_filter_selection(it's children will do that)
  #2. During this process it will also callmodule for each UI component.  
  
  #geOptionsInput(input,output,session,"GE/",conmanager)
  geOptionsInput(input,output,session,"GE/",conmanager)
  
  observe({
    #browser()
    if(session$input$electionType=="GE"){
      #browser()
      #clean AE slate
      updateSelectizeInput(session,"ae_I_chart_map_name",selected="")
      
    }else{
      #clean GE slate
      #browser()
      updateSelectizeInput(session,"ge_I_chart_map_name",selected="")
      
    } 
  })
  
  ####################Handling UI and event handlings for data download tab 
  ####################of index.html
  dataDownloadOptions(input,output,session,"AE/",conmanager)
  browseDataOptions(input,output,session,conmanager)

  #Rendering of home tab 
  output$home<-renderUI({
     includeHTML("www/home.html")
   })
  
  #Rendering of documentation tab 
   output$documentation<-renderUI({
     includeHTML("www/documentation.html")
   })

  #Rendering of about tab 
   output$about<-renderUI({
     includeHTML("www/about.html")
   })

   #Rendering of incumbency tab 
   output$incumbency<-renderUI({
     includeHTML("www/incumbency/GE.html")
   })

   #browser()
  output$visDataDownload <- downloadHandler(
   filename = function() {
	   browser()
     paste("TCPD",conmanager$restoredvals$selectedState,conmanager$restoredvals$vis, paste0(Sys.Date(), '.csv'), sep='_')
   },
   content = function(con) {
     write.csv(conmanager$restoredvals$visData, con, row.names = F)
   }
 )
   

}

ui1 <- function(request){
	htmlTemplate("www/index.html")
}
shinyApp(ui = ui1, server=shinyServer, enableBookmarking = "server")

  

  
