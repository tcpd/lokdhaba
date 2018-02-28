library(shiny)
library(reshape2)
library(leaflet)
library(plotly)
library(shinyjs)
library(mapview)
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

shinyServer(function(input, output, session) {
  
  ########################Allow server to reconnect to client after network
  ###########failure provided that the client browser is still open
  session$allowReconnect("force") 
  
  # ###########Adds resource path
  # addResourcePath("myassets", "www/assets")
useShinyjs(html = TRUE)  
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
    if(input$electionType=="GE"){
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

  ###################Rendering of contact us tab by reading the html from html file
   output$contactus<-renderUI({
     ##Render the summary report of UP election written in rmd file format.
     includeHTML("ContactUs.html")
   })

   ################Rendering of how to cite us tab by reading the html file   
   output$howtocite<-renderUI({
     ##Render the summary report of UP election written in rmd file format.
     includeHTML("HowToCite.html")
   })

   

})
  

  
