#############################Helper functions for this map visualization################################
getYears<-function(state, years, envr){
    st<-get(state,envr)
    st<-gsub(" ","_",st)
    print(st)
    m<-readStateWinnersFile(st)
        
    yearlist<-unique(m$year)

    assign(years,yearlist,env=envr)
  }

getPartyNames<-function(state, year, parties, envr){

       st<-get(state,envr)
    st<-gsub(" ","_",st)

       yr<-get(year,envr)
       winners<-readStateWinnersFile(st)%>%filter(year==yr)

       partys<-unique(winners$party1)
       assign(parties,partys,env=envr)

       shape<-readShapeFile(st, yr)
       #merge shape file with winners on ASSEMBLY and AC_No and set it as the leaflet data file
       #for creating a new leaflet map. Set this leaflet map in the current setting variable
        winners<-merge(shape,winners,by.x=c("ASSEMBLY"),by.y=c("ac_no"))
        assertthat::are_equal(nrow(shape),nrow(winners))
        winners<-addPopupInfo(winners)
        winners$Lat<-as.vector(coordinates(shape)[,2])
        winners$Long<-as.vector(coordinates(shape)[,1])
        
	base<-leaflet(winners)
        print('leaflet value is set')
        assign("leafletbase",base,env=envr)
    
        #set the count of winning seats for each party
        tm<-winners
        tm<-subset(tm,select=c("year","party1"))
        tm$count<-1
        tm<-aggregate(count~year+party1,tm,function(x) length(x))
        tm$legend<-paste0(tm$party1,"(",tm$count,")")
        tm<-arrange(tm,desc(count))
        tm$count<-NULL
        assign("countedframe",tm,env=envr)
    

    }

plotMap<-function(state, year, parties, plot, envr){
       st<-get(state,envr)
       yr<-get(year,envr)
    st<-gsub(" ","_",st)

        selectedpartynames<-get(parties,envr)

        counted<-get("countedframe",envr)
        base<-get("leafletbase",envr)
        pal<-getColorFactorParty(selectedpartynames)
      #(selectedpartynames)
#      pal<- leaflet::colorFactor(topo.colors(length(selectedpartynames)),levels=selectedpartynames,na.color = "white")
      # pal<-colorFactor(c("#ff6600","#A5F1F9","#0000ff","#228B22","#0000ff",
      #"#808000","#32CD32","#A52A2A","#A2FF33","#FF33E3","#F3FF33","#FF334C"),levels=
      #c("BJP","INC","SAD","SP","BSP","IND","AAP","MAG","RLD","ADS","SBSP","NISHD"),na.color = "#800000")
      sset<-subset(counted,counted$party1 %in% selectedpartynames)
      sset$color<-pal(as.character(sset$party1))

      #addpolygon for coloured display and add legend
      title<-paste0("Constituency wise party winners for ",gsub("_"," ",st)," in ",yr)

      base<-base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.character(party1)), popup=~(popup)) %>%
      addLegend("topright",color=sset$color, opacity= 1, labels=sset$legend,title="Party",
                        )%>%
        addTitleLeaflet(title)

    assign(plot, base,env=envr)
    }
#######################################End of helper function ############################################
