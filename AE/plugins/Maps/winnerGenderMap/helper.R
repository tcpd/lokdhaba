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

getOptions<-function(state, year, options,envr){

       st<-get(state,envr)
       st<-gsub(" ","_",st)

       yr<-get(year,envr)
       winners<-readStateWinnersFile(st)%>%filter(Year==yr)


   assign(options,WinnerGenderMapLegendList(),env=envr)

       shape<-readShapeFile(st, yr)
       #merge shape file with winners on ASSEMBLY and AC_No and set it as the leaflet data file
       #for creating a new leaflet map. Set this leaflet map in the current setting variable
        winners<-merge(shape,winners,by.x=c("ASSEMBLY"),by.y=c("Constituency_No"))
        assertthat::are_equal(nrow(shape),nrow(winners))
        winners<-addPopupInfo(winners)
        winners$Lat<-as.vector(coordinates(shape)[,2])
        winners$Long<-as.vector(coordinates(shape)[,1])
        
	base<-leaflet(winners)
        print('leaflet value is set')
        assign("leafletbase",base,env=envr)
    
   #set the count of winning seats for each gender
        tm<-winners
        #browser()
        tm<-subset(tm,select=c("Year","Sex"))
        tm<-WinnerGenderMapLegendCount(tm)
        assign("countedframe",tm,env=envr)
    


}

plotMap<-function(state, year, options, plot, envr){
       st<-get(state,envr)
       yr<-get(year,envr)
    st<-gsub(" ","_",st)

        selectedgendersnames<-get(options,envr)

        counted<-get("countedframe",envr)
        base<-get("leafletbase",envr)
 	cols<-c()
      lapply(selectedgendersnames,function(x){
        cols<<-c(cols,WinnerGenderMapLegendColor(x))
      })
      
      #create a colour plaette only for the partys selected in selectedgendersnames variable
      #pal<-createPal(selectedgendersnames, current_filters$sname, current_filters$year)
      pal<- leaflet::colorFactor(cols,levels=selectedgendersnames,na.color = "white")
      
      #addpolygon for coloured display and add legend
  title<-paste0("Gender wise winners for ",gsub("_"," ",st)," in ",yr)
      base<-base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.character(trimws((Sex)))), popup=~(popup)) %>%
        addLegend("topright",pal=pal, opacity= 1, values=as.character(selectedgendersnames),title="Gender",
                  labFormat = labelFormat(transform=function(x) {
                    lapply(x,function(y){
                      counted$legend[(trimws(counted$Sex))==y]
                    });
                  })
        )%>%
	addTitleLeaflet(title)
     

    assign(plot, base,env=envr)
    }
#######################################End of helper function ############################################



