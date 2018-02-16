#############################Helper functions for this map visualization################################
getYears<-function(years, envr){
    m<-readStateWinnersFile("ge")
        
    yearlist<-unique(m$Year)
    yearlist <- yearlist[which(yearlist >=2008)]
    assign(years,yearlist,env=envr)
  }

getMarginOptions<-function( year, margins,envr){


       yr<-get(year,envr)
       winners<-readStateWinnersFile("ge")%>%filter(Year==yr)


   assign(margins,WinnerMarginMapLegendList(),env=envr)

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
        tm<-subset(tm,select=c("Year","Margin_Percentage"))
        tm<-WinnerMarginMapLegendCount(tm)
        assign("countedframe",tm,env=envr)
    


}

plotMap<-function(year, margins, plot, envr){
       yr<-get(year,envr)                                                

        selectedpercentage<-get(margins,envr)

        counted<-get("countedframe",envr)
        base<-get("leafletbase",envr)
	#create a colour plaette only for the marrgins selected in selectedpercentage variable
      cols<-c()
      optionslist<-WinnerMarginMapLegendList()
      lapply(optionslist,function(x){
        if(x %in% selectedpercentage){
          cols<<-c(cols,WinnerMarginMapLegendColor(x))
        }else{
          cols<<-c(cols,"white")
        }
      })
      pal<-leaflet::colorBin(cols,bins=WinnerMarginMapBreakupList(),na.color="white")
      #coords<-current_filters$coords
      #From the colors of legend remove white they are the colors/options not selected in the checkbox 
      legendcolors<-setdiff(cols,c("white"))
      legendvalues<- lapply(selectedpercentage,function(y){
        counted$legend[(trimws(counted$tmp))==y]
      });
      
      #addpolygon for coloured display and add legend
      title<-paste0("Constituency wise winners' margin for loksabha in",yr)

      base<-base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.numeric(((Margin_Percentage)))), popup=~(popup)) %>%
        addLegend("topright",colors=legendcolors, labels=legendvalues,opacity=1,title="Winner margin"
        )%>%
        addTitleLeaflet(title)
      

    assign(plot, base,env=envr)
    }
#######################################End of helper function ############################################



