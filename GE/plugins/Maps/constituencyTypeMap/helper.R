#############################Helper functions for this map visualization################################
getYears<-function(years, envr){
    m<-readStateWinnersFile("ge")
        
    yearlist<-unique(m$Year)
    yearlist <- yearlist[which(yearlist >= 2008)]
    assign(years,yearlist,env=envr)
  }

getOptions<-function(year, options,envr){


       yr<-get(year,envr)
       winners<-readStateWinnersFile("ge")%>%filter(Year==yr)


   assign(options,WinnerCasteMapLegendList(),env=envr)

       shape<-readShapeFile("ge", yr)
       #merge shape file with winners on ASSEMBLY and AC_No and set it as the leaflet data file
       #for creating a new leaflet map. Set this leaflet map in the current setting variable
        winners<-merge(shape,winners,by.x=c("STATE_UT","PC_NO"),by.y=c("State_Name","Constituency_No"))
        assertthat::are_equal(nrow(shape),nrow(winners))
        winners<-addPopupInfo(winners)
        winners$Lat<-as.vector(coordinates(shape)[,2])
        winners$Long<-as.vector(coordinates(shape)[,1])
        
	base<-leaflet(winners,options = leafletOptions(minZoom=5,maxZoom=11,,zoomSnap=0.2,zoomDelta=0.2,scrollWheelZoom=F,touchZoom=F))
        print('leaflet value is set')
        assign("leafletbase",base,env=envr)
    
  #set the count of winning seats for each ac type
        tm<-winners
        #browser()
        tm<-subset(tm,select=c("Year","Constituency_Type"))
        tm<-WinnerCasteMapLegendCount(tm)
        assign("countedframe",tm,env=envr)
    


}

plotMap<-function(year, options, plot, envr){
       yr <- get(year,envr)
        selectedcastenames<-get(options,envr)

        counted<-get("countedframe",envr)
        base<-get("leafletbase",envr)
 #create a colour plaette only for the partys selected in selectedcastenames variable
      #pal<-createPal(selectedgendersnames, current_filters$sname, current_filters$year)
      cols<-c()
      lapply(selectedcastenames,function(x){
          cols<<-c(cols,WinnerCasteMapLegendColor(x))
      })
      
      pal<- leaflet::colorFactor(cols,levels=selectedcastenames,na.color = "white")
      
      #addpolygon for coloured display and add legend
      title<-paste0("Constituency types for loksabha in ",yr)

      base<-base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.character(trimws((Constituency_Type)))), popup=~(popup)) %>%
        addLegend("topright",pal=pal, opacity= 1,
                  values=as.character(selectedcastenames),title="Constituency Type",
                  labFormat = labelFormat(transform=function(x) {
                    lapply(x,function(y){
                      counted$legend[(trimws(counted$Constituency_Type))==y]
                    });
                  })
        )%>%
        addTitleLeaflet(title)
           

    assign(plot, base,env=envr)
    }
#######################################End of helper function ############################################



