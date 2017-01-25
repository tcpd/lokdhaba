source("utils/utils-lokdhaba.R")
library(rgdal)
library(dplyr)
#################################Fixed:################################################################
########################################################################################################
winnerCasteMap <- function(input, output, session, parentsession,statename_reactive,dirname) {
  ##################################### Values is a container to keep reactive values. These values are### 
  ##################use to trigger UI component renderings (like filters and chart area)##################
  values=reactiveValues(snameset=1,yearselected="",castenames=c())
  ##Variable to store the values used across functions
  current_filters=c()
  #get the session id
  ns=session$ns
  ns("dummy")
  
  ############Why should we have a dummy read of ns and session###
  ##1. If not then all ns were taking the same values as a result when we change the UI type for the same state
  ## the year selection for the last one continues to hold for the newly changed UI as well.i.e.
  ## assam- gender map 2016 -> winner map 2011 -> gender map switch does not preserve the old year value that is 2016 but 
  ## switches to the year value for the last one i.e. winner map that is 2011. The reason being that ns remains same for both modules
  ## and hence both modules refer to the same year selection input ns("I_year")
  #####################Observer for yearname selection UI##########################
  obs_yearname<-observe({
    if(!is.null(input$I_year)){
      values$yearselected<-input$I_year
    }
  },suspended = TRUE)
  
  
  ######################Observer for Gender selection UI (checkbox)###################
  obs_castenames<-observe({
    if(!is.null(input$filter_gname)){
      print(paste("observer_filter",input$filter_gname))
      values$castenames<-input$filter_gname
      }
  },suspended=TRUE)
  
  ######################Observer for statename selection UI (using reactive passed from the server.R)############
  obs_sname<-observe({
    st<-statename_reactive()
    ###get statename from the reactive passed from the parent module
    if(!is.null(st) && trimws(st)!=""){
        st<-gsub(" ","_",st)
      
        print(paste('Winnermap ac type-stchange: statename is ',st))
        #store the statename in the filter setting variable
        
        current_filters$sname<<-st
        #read ae_maps.csv file for this state tcpd_data/AE/Data/ + st + /derived/lokdhaba/ae_maps.csv
        #and store the dataframe in the filter setting variable
        m<-readStateWinnersFile(st)
        
        #store it in the filter setting variable
        current_filters$dframewinners<<-m
        #get the year of elections for this state from current drame set 
        years<-unique(current_filters$dframewinners$year)
        current_filters$yearlist<<-years
        values$castenames<-c()#for removing the rendered map
        isolate({
          if(!is.null(input$I_year)){
                shiny::updateSelectizeInput(parentsession,ns("I_year"),choices = c("Year"="",years),selected="")
              }
          else{
            values$snameset<-(values$snameset+1)%%2
            }
          })
    }
  },suspended=TRUE)
  
  
  #######################################Fixed part: ##########################################################################
  #############Every component must provide two functions. HideAll and showAll. These functions will be called by the main dashobard
  #############to ensure that proper shutdown and startup takes place when a UI type (chart/map visualization) changes
  HideAll<-function(){
    ##hide all components (pname_filter in this case)
    #shinyjs::disable(ns("pname_filter"))
    #shinyjs::hide(ns("filter_gname"))
    #before hiding plot we also want to clear it out.. so use reactive value change
    #values$yearselected<-"" #this will trigger change in filter
    values$castenames<-c() #this will trigger mapPlot render 
    #I wanted to trigger the year selection reset when calling hide so that the next time show is called on this module
    #the year selection comes afresh. However this was not working. Need to be investigagted further. Because the current feature does
    #not look bad hence continuing without this. In the current setting, if the UI was selected earlier and an year was selected then that
    #year preserves its value when that UI is switched back. However if no year was selected for a UI then when selecting that UI first time
    # the year selection box comes afresh.
    
    # isolate({
    #   if(!is.null(input$I_year)){
    #     shiny::updateSelectInput(parentsession,ns("I_year"),selected="")
    #   }
    # })
    ##disable all observers
    obs_castenames$suspend()
    obs_sname$suspend()
    obs_yearname$suspend()
    
    shinyjs::hide("mapPlot")
    
    print('Winner caste map: Hidden all')
  }
  ShowAll<-function(){
    ##show all components 
    
    shinyjs::show("mapPlot")
    ##enable all observers
    obs_castenames$resume()
    obs_yearname$resume()
    obs_sname$resume()
    ####setting up filter triggered on change in the state name##############################################
    parentsession$output$ae_filter_selection<-renderUI({
      #Trigger this rendering when a) values$snameset changes or valeus$yearselected changes
      if(is.null(values$snameset)){
        return()
      }
      years<-current_filters$yearlist
      #create year selection box, also set it to the currently set value
      if(values$yearselected=="" ){
        selectInput(ns("I_year"),"Select Year",c("Year"="",years),selectize = TRUE)
      }else{
        yr<-values$yearselected
        print(paste0('year change detected',yr))
        shape<-readShapeFile(current_filters$sname, yr)
        #get winners name from winners dataframe stored for this state for the given year
        winners<-current_filters$dframewinners %>% filter(year==yr)
        print(nrow(winners))
        #merge shape file with winners on ASSEMBLY and AC_No and set it as the leaflet data file
        #for creating a new leaflet map. Set this leaflet map in the current setting variable
        winners<-merge(shape,winners,by.x=c("ASSEMBLY"),by.y=c("ac_no"))
        assertthat::are_equal(nrow(shape),nrow(winners))
        winners<-addPopupInfo(winners)
        current_filters$leaflet<<-leaflet(winners)
        print('leaflet value is set')
        #set the count of winning seats for each ac type
        tm<-winners
        #browser()
        tm<-subset(tm,select=c("year","ac_type"))
        tm<-WinnerCasteMapLegendCount(tm)
        current_filters$countedframe<<-tm
        
        #create checkbox group for ac types and render it with yearinput (make sure that the year selection
        #remains same). Will yearinput being reactive help here?
        values$partynames<-c()
        tagList(
          selectInput(ns("I_year"),"Select Year",c("Year"="",years), selected=yr,selectize = TRUE),
          checkboxGroupInput(ns("filter_gname"), "Select AC Type ",
                             WinnerCasteMapLegendList())
        )
        
      }
    })
    
    #################Render leaflet map based on the name of the state and the selected party ###################################
    parentsession$output$mapPlot <- renderLeaflet({
      selectedcastenames<-values$castenames
      if(length(selectedcastenames)==0){
        print('winner caste map: returning')
        return()
      }
      # if( length(stale_filters$partynames)!=0)
      # {
      #   stale_filters$partynames<<-c()
      #   print(paste('stale names','returning'))
      #   return()
      # }
      print(paste('selected',selectedcastenames))
      #read base leaflet that was set when year changed.
      base<-current_filters$leaflet
      #create a colour plaette only for the partys selected in selectedcastenames variable
      #pal<-createPal(selectedgendersnames, current_filters$sname, current_filters$year)
      cols<-c()
      lapply(selectedcastenames,function(x){
          cols<<-c(cols,WinnerCasteMapLegendColor(x))
      })
      
      pal<- leaflet::colorFactor(cols,levels=selectedcastenames,na.color = "white")
      
      counted<-current_filters$countedframe
      #addpolygon for coloured display and add legend
      base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.character(trimws((ac_type)))), popup=~(popup)) %>%
        addLegend("topright",pal=pal, opacity= 1, values=as.character(selectedcastenames),title="AC Type",
                  labFormat = labelFormat(transform=function(x) {
                    lapply(x,function(y){
                      counted$legend[(trimws(counted$ac_type))==y]
                    });
                  })
        )
      
      
    })
    
    
    print('Winner ac type map: Enabled all')
  }
  
  ##Return these two functions to callers
  ret<-c()
  ret$HideAll<-HideAll
  ret$ShowAll<-ShowAll
  return (ret)
  ################################################################################################################################

}
