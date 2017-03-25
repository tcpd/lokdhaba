#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
options(shiny.sanitize.errors = FALSE)

geOptionsInput <- function(input, output, session,dname) {
  ####Global environement variable#######
  g_env<-new.env()
  last_ui_type<-""
  chartmaptitles<-c()
  
  #######################################
  
  
  

######################Filling in the different charts and maps types available. This information is displayed
  ######################by reading from a file 'chartsMaps.csv' that contains this meta information (title,type,filename)
  ##########Get's triggered on initialization of uityupe selection ui
  output$ge_uitype_selection<-renderUI({
    #read chartmaps csv file
    chartmap<-read.csv(paste0(dname,"chartsMaps.csv"))
    ###Also iterate over all chart types.. call their module functions and store their return types in something...
    
    titles<-list()
    for(i in seq_len(nrow(chartmap))) {
      x <- chartmap[i,]
      # do stuff with row
      filename<-(trimws(as.character(x$filename)))
      dirname<-paste0(dname,"plugins/",trimws(x$type),"s/",trimws(x$modulename),"/")
      filename<-paste0(dirname,filename)
      print(paste("loading source file ",filename))
      source(filename)
      modname<-(trimws(as.character(x$modulename)))
      print(paste("calling callModule",modname))
      r<-callModule(get(modname),modname,session,dirname) 
      r$HideAll()
      title<-trimws(as.character(x$title))
      
      assign(title,r,envir=g_env)
      
      titles<-append(titles,list(title))
      #append(titles,title)
      #print(titles)
    }
    
    #Create a selection input box
    chartmaptitles<<-titles
    selectInput("ge_I_chart_map_name","Visualization Type",c("Chart/Map"="",titles),selectize = TRUE)
    
  })

  
    
  #################Fixed part copied everywhere########################
  observe({
    
   if(!is.null(input$ge_I_chart_map_name) && trimws(input$ge_I_chart_map_name)!=""){
      print(paste("inside",input$ge_I_chart_map_name))
      #get the showall and hideall functions stored
     if(!is.null(last_ui_type) && last_ui_type!=""){
       lastselection<-get(last_ui_type,envir =  g_env)
       #browser()
       #hide the previously selected option
       print('Hiding the ui of previous chart option')
       lastselection$HideAll()
     }
     print(input$ge_I_chart_map_name)
     currselection<-get(input$ge_I_chart_map_name,envir =  g_env)
     currselection$ShowAll()
     #set the current selection as the last_ui_type global variable which will be used to hide
     #this ui when ui type change happens
     last_ui_type<<-input$ge_I_chart_map_name
   }else{
      #call hide on the last selected one. This will be used when we will switch from AE
     #to GE option type and that will require cleaning up of AE charts/maps
     if(!is.null(last_ui_type) && last_ui_type!=""){
       lastselection<-get(last_ui_type,envir =  g_env)
       #browser()
       #hide the previously selected option
       print('Hiding the ui of previous chart option')
       lastselection$HideAll()
     }
    }
  })
  
  #isolate(sname())
  #callModule(voteShareChart,"VoteShareChart",session)  
#}###end of showAll function
# 
# HideAll<-function(){
#   updateSelectInput(session,ns("I_chart_map_name"),"Visualization Type",c("Chart/Map"="",chartmaptitles),selected = "")
# }

##Return these two functions to callers
# ret<-c()
# ret$HideAll<-HideAll
# ret$ShowAll<-ShowAll
# return (ret)
################################################################################################################################

}
