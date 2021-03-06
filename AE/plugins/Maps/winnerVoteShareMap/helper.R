#############################Helper functions for this map visualization################################
getYears<-function(state, years, envr){
    st<-get(state,envr)
    st<-gsub(" ","_",st)
    print(st)
    m<-readStateWinnersFile(st)
        
    yearlist<-unique(m$Year)
    yearlist <- yearlist[which(yearlist >=2008)]
    assign(years,yearlist,env=envr)
  }

getOptions<-function(state,year,options,envr){

        
    
       st<-get(state,envr)
       st<-gsub(" ","_",st)

       yr<-get(year,envr)
       winners <- readStateWinnersFile(st)%>%filter(Year==yr)

       assign("winners_df",winners,env=envr)
       
        assign(options,voteShareMapLegendList(),env=envr)
        shape<-readShapeFile(st, yr)
               #merge shape file with winners on ASSEMBLY and AC_No and set it as the leaflet data file
               #for creating a new leaflet map. Set this leaflet map in the current setting variable
        winners<-merge(shape,winners,by.x=c("ASSEMBLY"),by.y=c("Constituency_No"))
        assertthat::are_equal(nrow(shape),nrow(winners))
        winners<-addPopupInfo(winners)
        winners$Lat<-as.vector(coordinates(shape)[,2])
        winners$Long<-as.vector(coordinates(shape)[,1])
        base<-leaflet(winners,options = leafletOptions(minZoom=6,maxZoom=10,zoomSnap=0.2,zoomDelta=0.2,scrollWheelZoom=F,touchZoom=F))
        print('leaflet value is set')
        assign("leafletbase",base,env=envr)
    
        #set the count of  seats for each option
        tm<-winners
        tm<-subset(tm,select=c("Year","Vote_Share_Percentage"))
        tm<-VoteShareMapLegendCount(tm)
        assign("countedframe",tm,env=envr)
        
    

    }


plotMap<-function(state, year,  options, plot, envr){
       st<-get(state,envr)
       yr<-get(year,envr)
       st<-gsub(" ","_",st)

       selectedfilters<-get(options,envr)
        counted<-get("countedframe",envr)
        base<-get("leafletbase",envr)

        #setting up variables for visualization data download
        df <- get("winners_df",envr)
        df$Vs_Legend <- getLegendIntervals(voteShareMapLegendList(),df$Vote_Share_Percentage)
        dat <- subset(df,Vs_Legend %in% selectedfilters,select = c("State_Name","Year","Constituency_No","Constituency_Name","Candidate","Votes","Sex","Vote_Share_Percentage","Vs_Legend"))
        conmanager$setval("visData",dat)
        conmanager$setval("selectedState",st)
        conmanager$setval("vis",paste("ConstituencyWise","Winners_VoteShare",yr,sep="_"))
        
             #create a colour plaette only for the options selected in selectedfilters variable
      #pal<-createPal(selectedpartynames, current_filters$sname, current_filters$year)
      #pal<- leaflet::colorFactor(topo.colors(length(selectedpartynames)),levels=selectedpartynames,na.color = "white")
      cols<-c()
      optionslist<-voteShareMapLegendList()
      lapply(optionslist,function(x){
        if(x %in% selectedfilters){
          cols<<-c(cols,VoteShareMapLegendColor(x))
        }else{
          cols<<-c(cols,"white")
        }
      })
      pal<-leaflet::colorBin(cols,bins=voteShareMapBreakupList(),na.color="white")
      #coords<-current_filters$coords
      #From the colors of legend remove white they are the colors/options not selected in the checkbox 
      legendcolors<-setdiff(cols,c("white"))
      legendvalues<- lapply(selectedfilters,function(y){
        counted$legend[(trimws(counted$tmp))==y]
      });
      #print(legendvalues)
      #addpolygon for coloured display and add legend
      title<-paste0("Winners' vote share for  in ",gsub("_"," ",st),"-",yr)

      base<-base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.numeric(Vote_Share_Percentage)), popup=~(popup)) %>%
        #addLegend("topright",pal=pal, values=(selectedfilters),opacity=1,title="Percentage vote share of winners")
        addLegend("topright",colors=legendcolors, labels=legendvalues,opacity=1,title="Percentage vote share of winners"
                  )%>%
        addTitleLeaflet(title)

    assign(plot, base,env=envr)
    }
#######################################End of helper function ############################################
