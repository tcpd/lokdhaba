source("utils/utils-lokdhaba.R")
library(rgdal)
library(dplyr)
#################################Fixed:################################################################
########################################################################################################
genumCandidatesMap <- function(input, output, session, parentsession,dirname) {
  ##################################### Values is a container to keep reactive values. These values are### 
  ##################use to trigger UI component renderings (like filters and chart area)##################
  values=reactiveValues(yearselected="",count=c(),filterreset=T)
  ##Variable to store the values used across functions
  current_filters=c()
  #get the session id
  ns=session$ns
  
  #####################Observer for yearname selection UI##########################
  obs_yearname<-observe({
    if(!is.null(input$I_year)){
      values$yearselected<-input$I_year
    }
  },suspended = TRUE)
  
  
  ######################Observer for percentage selection UI (checkbox)###################
  obs_count<-observe({
    if(!is.null(input$filter_pname)){
      print(paste("observer_filter",input$filter_pname))
      values$count<-input$filter_pname
      }
  },suspended=TRUE)
  
  ######################Observer for statename selection UI (using reactive passed from the server.R)############
  # obs_sname<-observe({
  #   st<-statename_reactive()
  #   ###get statename from the reactive passed from the parent module
  #   if(!is.null(st) && trimws(st)!=""){
  #       st<-gsub(" ","_",st)
  #     
  #       print(paste('Winnermap gender-stchange: statename is ',st))
  #       #store the statename in the filter setting variable
  #       
  #       current_filters$sname<<-st
  #       #read ae_maps.csv file for this state tcpd_data/AE/Data/ + st + /derived/lokdhaba/ae_maps.csv
  #       #and store the dataframe in the filter setting variable
  #       m<-readStateWinnersFile(st)
  #       
  #       #store it in the filter setting variable
  #       current_filters$dframewinners<<-m
  #       #get the year of elections for this state from current drame set 
  #       years<-unique(current_filters$dframewinners$year)
  #       #reset the year selection UI by filling it appropriately
  #       current_filters$yearlist<<-years
  #       values$count<-c()#for removing the rendered map
  #       isolate({
  #         if(!is.null(input$I_year)){
  #           shiny::updateSelectizeInput(parentsession,ns("I_year"),choices = c("Year"="",years),selected="")
  #         }
  #         else{
  #           values$snameset<-(values$snameset+1)%%2
  #         }
  #       })
  #       
  #   }
  # },suspended=TRUE)
  
  
  
  #######################################Fixed part: ##########################################################################
  #############Every component must provide two functions. HideAll and showAll. These functions will be called by the main dashobard
  #############to ensure that proper shutdown and startup takes place when a UI type (chart/map visualization) changes
  HideAll<-function(){
    values$count<-c() #this will trigger mapPlot render
    values$filterreset<-T
    ##disable all observers
    obs_count$suspend()
    # obs_sname$suspend()
    obs_yearname$suspend()
    
    ##hide all components (pname_filter in this case)
    #shinyjs::disable(ns("pname_filter"))
    #shinyjs::hide(ns("filter_pname"))
    #before hiding plot we also want to clear it out.. so use reactive value change
    shinyjs::hide("mapPlot")
    print('Num candidates map: Hidden all')
  }
  ShowAll<-function(){
    ##show all components 
    
    shinyjs::show("mapPlot")
    obs_count$resume()
    # obs_sname$resume()
    obs_yearname$resume()
    values$filterreset<-F
    ####setting up filter triggered on change in the state name##############################################
    parentsession$output$ge_filter_selection<-renderUI({
      if(values$filterreset==T){
        return()
      }
            #read ae_maps.csv file for this state tcpd_data/AE/Data/ + st + /derived/lokdhaba/ae_maps.csv
            #and store the dataframe in the filter setting variable
            m<-readStateWinnersFile("ge")

            #store it in the filter setting variable
            current_filters$dframewinners<<-m
            #get the year of elections for this state from current drame set
            years<-unique(current_filters$dframewinners$year)
            years<-sort(years)
            #reset the year selection UI by filling it appropriately
            current_filters$yearlist<<-years

      #create year selection box, also set it to the currently set value
      if(values$yearselected==""){
        selectInput(ns("I_year"),"Select Year",c("Year"="",years),selectize = TRUE)
      }else{
        yr<-values$yearselected
        print(paste0('year change detected',yr))
        shape<-readShapeFile("ge", yr)
        #get winners name from winners dataframe stored for this state for the given year
        winners<-current_filters$dframewinners %>% filter(year==yr)
        print(nrow(winners))
        #merge shape file with winners on ASSEMBLY and AC_No and set it as the leaflet data file
        #for creating a new leaflet map. Set this leaflet map in the current setting variable
        winners<-merge(shape,winners,by.x=c("STATE_UT","PC_NO"),by.y=c("state","pc_no"))
        assertthat::are_equal(nrow(shape),nrow(winners))
        winners<-addPopupInfo(winners,type="ge")
        current_filters$leaflet<<-leaflet(winners)
        print('leaflet value is set')
        #set the count of winning seats for each victory margin
        tm<-winners
        tm<-subset(tm,select=c("year","n_cand"))
        tm<-NumCandidatesMapLegendCount(tm)
        current_filters$countedframe<<-tm
        
        #create checkbox group for numbers and render it with yearinput (make sure that the year selection
        #remains same). Will yearinput being reactive help here?
        values$count<-c()
        tagList(
          selectInput(ns("I_year"),"Select Year",c("Year"="",years), selected=yr,selectize = TRUE),
          checkboxGroupInput(ns("filter_pname"), "Select nunber of candidates ",
                             NumCandidatesMapLegendList(), selected=NumCandidatesMapLegendList())
        )
        
      }
    })
    
    #################Render leaflet map based on the name of the state year and the selected percentage ###################################
    parentsession$output$mapPlot <- renderLeaflet({
      selectedcount<-values$count
      if(length(selectedcount)==0){
        print('ge num candidates map : returning')
        return()
      }
      # if( length(stale_filters$partynames)!=0)
      # {
      #   stale_filters$partynames<<-c()
      #   print(paste('stale names','returning'))
      #   return()
      # }
      print(paste('selected',selectedcount))
      #read base leaflet that was set when year changed.
      base<-current_filters$leaflet
      #create a colour plaette only for the numbers selected in selectedcount variable
      #pal<-createPal(selectedgendersnames, current_filters$sname, current_filters$year)
      cols<-c()
      optionslist<-NumCandidatesMapLegendList()
      lapply(optionslist,function(x){
        if(x %in% selectedcount){
          cols<<-c(cols,NumCandidatesMapLegendColor(x))
        }else{
          cols<<-c(cols,"white")
        }
      })
      print(cols)
      print(NumCandidatesMapBreakupList())
      pal<-leaflet::colorBin(cols,bins=NumCandidatesMapBreakupList(),na.color="white")
      #coords<-current_filters$coords
      #From the colors of legend remove white they are the colors/options not selected in the checkbox 
      legendcolors<-setdiff(cols,c("white"))
      counted<-current_filters$countedframe
      legendvalues<- lapply(selectedcount,function(y){
        counted$legend[(trimws(counted$tmp))==y]
      });
      
      #addpolygon for coloured display and add legend
      base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.numeric(((n_cand)))), popup=~(popup)) %>%
        addLegend("topright",colors=legendcolors, labels=legendvalues,opacity=1,title="Number of contesting candidates"
        )
      
    })
    print('ge num candidates map: Enabled all')
  }
  
  ##Return these two functions to callers
  ret<-c()
  ret$HideAll<-HideAll
  ret$ShowAll<-ShowAll
  return (ret)
  ################################################################################################################################

}
