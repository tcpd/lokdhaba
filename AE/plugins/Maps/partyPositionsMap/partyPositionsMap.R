source("utils/utils-lokdhaba.R")
library(rgdal)
#################################Fixed:################################################################
########################################################################################################
partyPositionsMap <- function(input, output, session, parentsession,statename_reactive,dirname) {
  ##################################### Values is a container to keep reactive values. These values are### 
  ##################use to trigger UI component renderings (like filters and chart area)##################
  values<-reactiveValues(filter_input=c(),snameset=1,yearselected="",partyselected="")
  ##Variable to store the values used across functions
  current_filters<-c()
  #get the session id
  ns<-session$ns
  
  ##################Observer for filter (<10%,10-30% etc. selection UI )checkbox############
  obs_filter<-observe({
    if(!is.null(input$filter_input)){
      print(paste("observer_filter",input$filter_input))
      values$filter_input<-input$filter_input
      }
  },suspended=TRUE)
  
  #####################Observer for yearname selection UI##########################
  obs_yearname<-observe({
    if(!is.null(input$I_year)){
      print('year observer')
      values$yearselected<-input$I_year
      ###Reset the party selected UI
      isolate({
          shiny::updateSelectizeInput(parentsession,ns("I_party"),selected="")
        })
    }
  },suspended = TRUE)
  
  
  ##################Observer for party name change, selectInputBox############################
  obs_partyname<-observe({
    if(!is.null(input$I_party)){
      print('partyname observer')
      values$partyselected<-input$I_party
    }
  },suspended = TRUE)
  
  #################Observer for statename change (reactive passed from server.R)##############
  obs_sname<-observe({
    st<-statename_reactive()
    ###get statename from the reactive passed from the parent module
    if(!is.null(st) && trimws(st)!=""){
      print('statename observer')
        #Send a signal to change/reload year filter
        st<-gsub(" ","_",st)
      
        print(paste('Winner party position map-stchange: statename is ',st))
        #store the statename in the filter setting variable
        
        current_filters$sname<<-st
        #read ae_maps.csv file for this state tcpd_data/AE/Data/ + st + /derived/lokdhaba/ae_maps.csv
        #and store the dataframe in the filter setting variable
	m<-readPartyPositionsFile(st)
        #m<-readStateWinnersFileAll(st)
        
        #store it in the filter setting variable
        current_filters$dframewinners<<-m
        #get the year of elections for this state from current drame set 
        years<-getYearsForMap(current_filters$dframewinners)
 
       #years<-unique(current_filters$dframewinners$year)
        current_filters$yearlist<<-years
        values$filter_input<-c()#for removing the rendered map
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
    values$filter_input<-c() #this will trigger mapPlot render 
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
    obs_filter$suspend()
    obs_sname$suspend()
    obs_yearname$suspend()
    obs_partyname$suspend()
    ##hide all components (pname_filter in this case)
    #shinyjs::disable(ns("pname_filter"))
    #shinyjs::hide(ns("filter_input"))
    #before hiding plot we also want to clear it out.. so use reactive value change
    shinyjs::hide("mapPlot")
    print('party position map: Hidden all')
  }
  ShowAll<-function(){
    ##show all components 
    
    shinyjs::show("mapPlot")
    ##enable all observers
    obs_filter$resume()
    obs_sname$resume()
    obs_yearname$resume()
    obs_partyname$resume()
    
    ####setting up filter triggered on change in the state name##############################################
    parentsession$output$ae_filter_selection<-renderUI({
      #Trigger this rendering when a) values$snameset changes or valeus$yearselected changes
      if(is.null(values$snameset)){
        return()
      }
      #browser()
      years<-current_filters$yearlist
      #create year selection box, also set it to the currently set value
      if(values$yearselected==""){
        ###assert that values$partyselected is also ""
        selectInput(ns("I_year"),"Select Year",c("Year"="",years),selectize = TRUE)
      }else if(values$yearselected!=""){
        yr<-values$yearselected
         current_filters$year<<-yr
        if(values$partyselected==""){
          ###we have to get parties present for this year and create a party selection drop box for them
          #get winners name from winners dataframe stored for this state for the given year
          winners<-current_filters$dframewinners %>% filter(year==yr)
          partys<-unique(winners$party1)
          current_filters$partys<<-partys
         print('setting the party names')
          print(partys)
          tagList(
            selectInput(ns("I_year"),"Select Year",c("Year"="",years),yr,selectize = TRUE),
            selectInput(ns("I_party"),"Select Party",c("Party"="",as.list(partys)),selectize = TRUE)
          )  
        }else{
        party<-values$partyselected
	current_filters$party<<-party
        print('populating further')
        ##IMP: for every row where party1 is different from party (selected) set position as na
          #merge shape file with winners on ASSEMBLY and AC_No and set it as the leaflet data file
          #for creating a new leaflet map. Set this leaflet map in the current setting variable
          winners<-current_filters$dframewinners %>% filter(year==yr) %>% filter(party1==party)
          shape<-readShapeFile(current_filters$sname, yr)
          current_filters$coords<<-coordinates(shape)
  if("ASSEMBLY" %in% names(shape)){
          winners<-merge(shape,winners,by.x=c("ASSEMBLY"),by.y=c("ac_no"))
        }else{

          winners<-merge(shape,winners,by.x=c("ASSEMBLY_N"),by.y=c("ac_no"))

        }
          
          #assertthat::are_equal(nrow(shape),nrow(winners))
          winners<-addPopupInfo(winners)
          #store merged frame in the current setting
          current_filters$mergedframe<<-winners
         #winners<-current_filters$mergedframe %>% filter(party1==party)
        #winners$position[winners$party1!=party]<-NA
        current_filters$leaflet<<-leaflet(winners)
        print('leaflet value is set')
        #set the count of  seats for each option
        tm<-winners
        tm<-subset(tm,select=c("year","position"))
        tm<-PartyPositionsMapLegendCount(tm)
        current_filters$countedframe<<-tm
        
        #create checkbox group for inputs and render it with filterinput (make sure that the year and party
        #selection remains same). Will yearinput being reactive help here?
        values$filter_input<-c()
        
        tagList(
          selectInput(ns("I_year"),"Select Year",c("Year"="",years),selected=yr,selectize = TRUE),
          selectInput(ns("I_party"),"Select Party",c("Party"="",as.list(current_filters$partys)),selected=party,selectize = TRUE),
          checkboxGroupInput(ns("filter_input"), "Select Party Positions ",
                             PartyPositionsMapLegendList(),selected=PartyPositionsMapLegendList())
        )
        }
      }
        
    })
    
    #################Render leaflet map based on the name of the state and the selected party ###################################
    parentsession$output$mapPlot <- renderLeaflet({
      selectedfilters<-values$filter_input
      if(length(selectedfilters)==0){
        print('party positions map: returning')
        return()
      }
#cat(file=stderr(), selectedfilters, "\n")
      print(paste('selected',selectedfilters))
      #read base leaflet that was set when year changed.
      base<-current_filters$leaflet
      #create a colour plaette only for the options selected in selectedfilters variable
      #pal<-createPal(selectedpartynames, current_filters$sname, current_filters$year)
      #pal<- leaflet::colorFactor(topo.colors(length(selectedpartynames)),levels=selectedpartynames,na.color = "white")
      cols<-c()
      optionslist<-PartyPositionsMapLegendList()
      lapply(optionslist,function(x){
        if(x %in% selectedfilters){
#cat(file=stderr(), x, "\n")
          cols<<-c(cols,PartyPositionsMapLegendColor(x))
        }else{
          cols<<-c(cols,"white")
        }
      })
#cat(file=stderr(), cols, "\n")
#cat(file=stderr(),PartyPositionsMapBreakupList(),"\n")
      pal<-leaflet::colorBin(cols,bins=PartyPositionsMapBreakupList(),na.color="white")
      #coords<-current_filters$coords
      #From the colors of legend remove white they are the colors/options not selected in the checkbox 
      legendcolors<-setdiff(cols,c("white"))
      counted<-current_filters$countedframe
      #print(counted)
      legendvalues<- lapply(selectedfilters,function(y){
        counted$legend[(trimws(counted$tmp))==y]
      });
      print(legendvalues)
      #addpolygon for coloured display and add legend
      title<-paste0("Party wise positions for ",current_filters$party," in ",gsub("_"," ",current_filters$sname)," - ",current_filters$year)

      base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.numeric(position)), popup=~(popup)) %>%
        #addLegend("topright",pal=pal, values=(selectedfilters),opacity=1,title="Percentage vote share of winners")
        addLegend("topright",colors=legendcolors, labels=legendvalues,opacity=1,title="Party positions "
                  )%>%
        addTitleLeaflet(title)
    })
    
    print('Party Positions map: Enabled all')
  }
  
  ##Return these two functions to callers
  ret<-c()
  ret$HideAll<-HideAll
  ret$ShowAll<-ShowAll
  return (ret)
  ################################################################################################################################

}
