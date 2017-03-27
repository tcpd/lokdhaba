library(shiny)
library(reshape2)
library(leaflet)
library(plotly)
library(shinyjs)
library(DT)
source("AE/aeOptionsInput.R")
source("GE/geOptionsInput.R")
source("DataDownloader/dataDownloadOptions.R")
shinyServer(function(input, output, session) {
# enableBookmarking("url")
# testf<-function(){
# cat(file=stderr(),"---restoring to old state----","\n")
# #cat(file=stderr(),state,"\n")
# }
# testff<-function(){
# 	cat(file=stderr(),"---Session ended---","\n")
# }
# session$onRestored(function(state){
# #	message("message onrestored\n")
#   #updateSelectizeInput("dd_state_selector",selected="Madras")
#  testf 
# })   

# session$onSessionEnded({
#   #message("message on session end\n")
#   #updateSelectizeInput("dd_state_selector",selected="Madras")
#   testff  
# })   
#cat(file=stderr(),"Testing of cat","\n")
  addResourcePath("myassets", "www/assets")
###Call aeOptionsInput.R's function
  #pass input, output and session. That function will
  aeOptionsInput(input,output,session,"AE/")
  #1. render AE specific ui components, state_selection, ae_uitype_selection, and ae_filter_selection(it's children will do that)
  #2. During this process it will also callmodule for each UI component.  
  
###Call geoptionsInput.R's function
  #pass input, output and session. That function will
  geOptionsInput(input,output,session,"GE/")
  #1. render GE specific ui components, ge_uitype_selection, and ge_filter_selection(it's children will do that)
  #2. During this process it will also callmodule for each UI component.  
  
  observe({
    if(input$electionType=="GE"){
      #browser()
      #clean AE slate
      updateSelectizeInput(session,"ge_I_chart_map_name",selected="")
      
    }else{
      #clean GE slate
      #browser()
      updateSelectizeInput(session,"ae_I_chart_map_name",selected="")
      
    }
  })
  # output$UPSummary<-renderUI({
  #   ##Render the summary report of UP election written in rmd file format.
  #   includeHTML("www/Reports/Gilles/UP-2017/Report.html")
  # })
  # 
  # output$PBSummary<-renderUI({
  #   ##Render the summary report of UP election written in rmd file format.
  #   includeHTML("www/Reports/Gilles/Punjab-2017/Report.html")
  # })
  
  dataDownloadOptions(input,output,session,"AE/")

   output$contactus<-renderUI({
     ##Render the summary report of UP election written in rmd file format.
     includeHTML("ContactUs.html")
   })
   
   output$howtocite<-renderUI({
     ##Render the summary report of UP election written in rmd file format.
     includeHTML("HowToCite.html")
   })

 
session$allowReconnect("force") 
})
  

  
