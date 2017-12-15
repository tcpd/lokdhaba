#############################Helper functions for this map visualization################################
getYears<-function( years, envr){
    m<-readStateWinnersFile("ge")
        
    yearlist<-unique(m$Year)
    yearlist <- yearlist[which(yearlist >=2008)]
    assign(years,yearlist,env=envr)
  }

getPartyNames<-function( year, parties, envr){


       yr<-get(year,envr)
       winners<-readStateWinnersFile("ge")%>%filter(Year==yr)

       partys<-unique(winners$Party)
       assign(parties,partys,env=envr)

       shape<-readShapeFile("ge", yr)
       #merge shape file with winners on ASSEMBLY and AC_No and set it as the leaflet data file
       #for creating a new leaflet map. Set this leaflet map in the current setting variable
        winners<-merge(shape,winners,by.x=c("STATE_UT","PC_NO"),by.y=c("State_Name","Constituency_No"))
        assertthat::are_equal(nrow(shape),nrow(winners))
        winners<-addPopupInfo(winners)
        winners$Lat<-as.vector(coordinates(shape)[,2])
        winners$Long<-as.vector(coordinates(shape)[,1])
        
	base<-leaflet(winners,options = leafletOptions(minZoom=5,maxZoom=11))
        print('leaflet value is set')
        assign("leafletbase",base,env=envr)
    
        #set the count of winning seats for each party
        tm<-winners
        tm<-subset(tm,select=c("Year","Party"))
        tm$count<-1
        tm<-aggregate(count~Year+Party,tm,function(x) length(x))
        tm$legend<-paste0(tm$Party,"(",tm$count,")")
        tm<-arrange(tm,desc(count))
        tm$count<-NULL
        assign("countedframe",tm,env=envr)
    

    }

plotMap<-function( year, parties, plot, envr){
       yr<-get(year,envr)

        selectedpartynames<-get(parties,envr)

        counted<-get("countedframe",envr)
        base<-get("leafletbase",envr)
        pal<-getColorFactorParty(selectedpartynames)
      #(selectedpartynames)
#      pal<- leaflet::colorFactor(topo.colors(length(selectedpartynames)),levels=selectedpartynames,na.color = "white")
      # pal<-colorFactor(c("#ff6600","#A5F1F9","#0000ff","#228B22","#0000ff",
      #"#808000","#32CD32","#A52A2A","#A2FF33","#FF33E3","#F3FF33","#FF334C"),levels=
      #c("BJP","INC","SAD","SP","BSP","IND","AAP","MAG","RLD","ADS","SBSP","NISHD"),na.color = "#800000")
      sset<-subset(counted,counted$Party %in% selectedpartynames)
      sset$color<-pal(as.character(sset$Party))

      #addpolygon for coloured display and add legend
      title<-paste0("Constituency wise party winners for Loksabha in ",yr)

      base<-base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.character(Party)), popup=~(popup)) %>%
      addLegend("topright",color=sset$color, opacity= 1, labels=sset$legend,title="Party",
                        )%>%
        addTitleLeaflet(title)

    assign(plot, base,env=envr)
    }
#######################################End of helper function ############################################
