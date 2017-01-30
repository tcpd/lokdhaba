source("utils/utils-charts-ui.R")

#################################Fixed:################################################################
########################################################################################################
gevoterTurnoutChart <- function(input, output, session, parentsession,dname) {
  ##################################### Values is a container to keep reactive values. These values are### 
  ##################use to trigger UI component renderings (like filters and chart area)##################
  values<-reactiveValues(gendernames=c())
  ##Variable to store the values used across functions
  current_filters<-c()
  #get the session id
  ns<-session$ns
  #Store passed directory name (name of the dir where this R file is stored).. It is interesting that using dname directly
  #does not work because as that value changes in server.R it changes here at the point of use as well.
  dirname<-dname
  ns("dummy")
  
  obs_gendernames<-observe({
    if(!is.null(input$filter_pname)){
      print(paste("observer_filter",input$filter_pname))
      values$gendernames<-input$filter_pname
      #current_filters$partynames<<-input$filter_pname
      }
    #print(paste("observer_filter",input$filter_pname))
  },suspended=TRUE)
  
  # obs_sname<-observe({
  #   st<-statename_reactive()
  #   ###get statename from the reactive passed from the parent module
  #   if(!is.null(st) && trimws(st)!=""){
  #     st<-gsub(" ","_",st)
  #     print(dirname)
  #     print(paste('Voter turnout-stchange: statename is ',st))
  #       b<-readVoterTurnoutFile(st)
  #       #pivotdata<-dcast(b,year~party)
  #       #create a base line chart with year as the x-axis
  #       current_filters$base<<-plot_ly(b, x = ~year)
  #       #print(paste('before',values$statename))
  #       current_filters$sname<<-st
  #       values$statename<-st
  #       #print(paste('after',values$statename))
  # 
  #   }
  # },suspended=TRUE)
  
  
  
  #######################################Fixed part: ##########################################################################
  #############Every component must provide two functions. HideAll and showAll. These functions will be called by the main dashobard
  #############to ensure that proper shutdown and startup takes place when a UI type (chart/map visualization) changes
  HideAll<-function(){
    ##disable all observers
    obs_gendernames$suspend()
    # obs_sname$suspend()
    ##hide all components (pname_filter in this case)
    #shinyjs::disable(ns("pname_filter"))
    shinyjs::hide(ns("filter_pname")) #may be this hiding not possible..check it later
    shinyjs::hide("distPlot")
    print('ge Voteshar[]=e: Hidden all')
  }
  ShowAll<-function(){
    ##show all components 
    
    shinyjs::show("distPlot")
    
    ####setting up filter triggered on change in the state name##############################################
    parentsession$output$ge_filter_selection<-renderUI({
      #else from the csv file read in the information regarding this state in another dataframe
      b<-readVoterTurnoutFile(st)
      #pivotdata<-dcast(b,year~party)
      #create a base line chart with year as the x-axis
      current_filters$base<<-plot_ly(b, x = ~year)
      
      gendernames<-c("male","female","total")
      #stale_filters$partynames<<-current_filters$partynames
      #Writing to the following reactive value triggers plotly rendering which vanishes the previously drawn chart
      values$gendernames<-c()
      checkboxGroupInput(ns("filter_pname"), "Select Voter Turnout options for ",
                         gendernames)
      
    })
    
    #################Render plotly chart based on the name of the state and the selected party ###################################
    parentsession$output$distPlot <- renderPlotly({
      selectedgendernames<-values$gendernames
      if(length(selectedgendernames)==0){
        print('ge voter turnout chart: returning')
        return()
      }
      # if( length(stale_filters$partynames)!=0)
      # {
      #   stale_filters$partynames<<-c()
      #   print(paste('stale names','returning'))
      #   return()
      # }
      print(paste('selected',selectedgendernames))
      #read base that was set when state name changed.
      base<-current_filters$base
      # #for each selected party in the input "filter_pname" id (checkbox) add a new trace
      # #corresponding to that party
      lapply(selectedgendernames,function(x) {
        print(paste('adding',x));
        base<<-add_trace(base,y=~get(x),name=x,mode='lines+markers',showlegend=TRUE)
        }
        )
      sname<-current_filters$sname
      sname<-gsub("_"," ",sname)
      thistitle<-paste0('Voter turnout across years in General Elections')
      xtitle<-''
      ytitle<-'Turnout in %'
      yrange<-c(0,100)
      preparechartlayout(base,thistitle,xtitle,ytitle,yrange)      
    })
    ##enable all observers
    obs_gendernames$resume()
    # obs_sname$resume()
    
    print('Voter turnout chart : Enabled all')
  }
  
  ##Return these two functions to callers
  ret<-c()
  ret$HideAll<-HideAll
  ret$ShowAll<-ShowAll
  return (ret)
  ################################################################################################################################

}
