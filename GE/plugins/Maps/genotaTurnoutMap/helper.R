#############################Helper functions for this map visualization################################
getYears<-function(years, envr){
    m<-readStateWinnersFile("ge")
        
    yearlist<-unique(m$Year)
    #removing years in which NOTA was not present
    yl <- yearlist[which(yearlist >=2014)] 
    assign(years,yl,env=envr)
  }

getOptions<-function( year, options,envr){


       yr<-get(year,envr)
       winners<-readStateWinnersFile("ge")%>%filter(Year==yr)
       
       assign("winners_df",winners,env=envr)
       
       
   assign(options,NotaTurnoutMapLegendList(),env=envr)

       shape<-readShapeFile("ge", yr)
       #merge shape file with winners on ASSEMBLY and AC_No and set it as the leaflet data file
       #for creating a new leaflet map. Set this leaflet map in the current setting variable
        winners<-merge(shape,winners,by.x=c("STATE_UT",
                                            "PC_NO"),by.y=c("State_Name","Constituency_No"))
        assertthat::are_equal(nrow(shape),nrow(winners))
        winners<-addPopupInfo(winners)
        winners$Lat<-as.vector(coordinates(shape)[,2])
        winners$Long<-as.vector(coordinates(shape)[,1])
        
	      base<-leaflet(winners,options = leafletOptions(minZoom=5,maxZoom=11,zoomSnap=0.2,zoomDelta=0.2,scrollWheelZoom=F,touchZoom=F))
        print('leaflet value is set')
        assign("leafletbase",base,env=envr)
     #set the count of winning seats for each nota range
        tm<-winners
        tm<-subset(tm,select=c("Year","Nota_Percentage"))
        tm<-NotaTurnoutMapLegendCount(tm)
          
        assign("countedframe",tm,env=envr)
    


}

plotMap<-function(year, options, plot, envr){
       yr<-get(year,envr)

        selectedpercentage<-get(options,envr)

        counted<-get("countedframe",envr)
        base<-get("leafletbase",envr)
        
        #setting up variables for visualization data download
        df<- get("winners_df",envr)
        df$Nv_Legend <- getLegendIntervals(NotaTurnoutMapLegendList(),df$Nota_Percentage)
        dat <- subset(df,Nv_Legend %in% selectedpercentage,select = c("State_Name","Year","Constituency_No","Constituency_Name","Nota_Percentage","Nv_Legend"))
        conmanager$setval("visData",dat)
        conmanager$setval("selectedState","Loksabha")
        conmanager$setval("vis",paste("ConstituencyWise","NotaVoteShare",yr,sep="_"))
        
	 #create a colour plaette only for the marrgins selected in selectedpercentage variable
      #pal<-createPal(selectedgendersnames, current_filters$sname, current_filters$year)
      cols<-c()
      optionslist<-NotaTurnoutMapLegendList()
      lapply(optionslist,function(x){
        if(x %in% selectedpercentage){
          cols<<-c(cols,NotaTurnoutMapLegendColor(x))
        }else{
          cols<<-c(cols,"white")
        }
      })
      pal<-leaflet::colorBin(cols,bins=NotaTurnoutMapBreakupList(),na.color="white")
      #coords<-current_filters$coords
      #From the colors of legend remove white they are the colors/options not selected in the checkbox 
      legendcolors<-setdiff(cols,c("white"))
     
      legendvalues<- lapply(selectedpercentage,function(y){
        counted$legend[(trimws(counted$tmp))==y]
      });
      
      #addpolygon for coloured display and add legend
      title<-paste0("NOTA turnout for Lok Sabha in ",yr)

      base<-base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.numeric(((Nota_Percentage)))), popup=~(popup)) %>%
        addLegend("topright",colors=legendcolors, labels=legendvalues,opacity=1,title="NOTA turnout"
        )%>%
        addTitleLeaflet(title)
     
    assign(plot, base,env=envr)
    }
#######################################End of helper function ############################################



