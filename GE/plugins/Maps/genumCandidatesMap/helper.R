#############################Helper functions for this map visualization################################
getYears<-function(years, envr){
    m<-readStateWinnersFile("ge")
        
    yearlist<-unique(m$Year)
    yearlist <- yearlist[which(yearlist >=2008)]
    assign(years,yearlist,env=envr)
  }

getOptions<-function(year, options,envr){


       yr<-get(year,envr)
       winners<-readStateWinnersFile("ge")%>%filter(Year==yr)
       assign("winners_df",winners,env=envr)
       


   assign(options,NumCandidatesMapLegendList(),env=envr)

       shape<-readShapeFile("ge", yr)
       #merge shape file with winners on ASSEMBLY and AC_No and set it as the leaflet data file
       #for creating a new leaflet map. Set this leaflet map in the current setting variable
        winners<-merge(shape,winners,by.x=c("STATE_UT","PC_NO"),by.y=c("State_Name","Constituency_No"))
        assertthat::are_equal(nrow(shape),nrow(winners))
        winners<-addPopupInfo(winners)
        winners$Lat<-as.vector(coordinates(shape)[,2])
        winners$Long<-as.vector(coordinates(shape)[,1])
        
	base<-leaflet(winners,options = leafletOptions(minZoom=5,maxZoom=11,zoomSnap=0.2,zoomDelta=0.2,scrollWheelZoom=F,touchZoom=F))
        print('leaflet value is set')
        assign("leafletbase",base,env=envr)
     #set the count of winning seats for each victory margin
        tm<-winners
        tm<-subset(tm,select=c("Year","N_Cand"))
        tm<-NumCandidatesMapLegendCount(tm)
       
        assign("countedframe",tm,env=envr)
    


}

plotMap<-function(year, options, plot, envr){
       yr<-get(year,envr)

        selectedcount<-get(options,envr)

        counted<-get("countedframe",envr)
        base<-get("leafletbase",envr)
        
        #setting up variables for visualization data download
        df<- get("winners_df",envr)
        df$N_Legend <- getLegendIntervals(NumCandidatesMapLegendList(),df$N_Cand)
        dat <- subset(df,N_Legend %in% selectedcount,select = c("State_Name","Year","Constituency_No","Constituency_Name","N_Cand"))
        conmanager$setval("visData",dat)
        conmanager$setval("selectedState","Loksabha")
        conmanager$setval("vis",paste("ConstituencyWise","NumberOfCandidates",yr,sep="_"))
        
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
      pal<-leaflet::colorBin(cols,bins=NumCandidatesMapBreakupList(),na.color="white")
      #coords<-current_filters$coords
      #From the colors of legend remove white they are the colors/options not selected in the checkbox 
      legendcolors<-setdiff(cols,c("white"))
      legendvalues<- lapply(selectedcount,function(y){
        counted$legend[(trimws(counted$tmp))==y]
      });
      
      #addpolygon for coloured display and add legend
      title<-paste0("Constituency wise Candidate count for Lok Sabha in ",yr)

      base<-base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.numeric(((N_Cand)))), popup=~(popup)) %>%
        addLegend("topright",colors=legendcolors, labels=legendvalues,opacity=1,title="Number of contesting candidates"
        )%>%
        addTitleLeaflet(title)      

    assign(plot, base,env=envr)
    }
#######################################End of helper function ############################################



