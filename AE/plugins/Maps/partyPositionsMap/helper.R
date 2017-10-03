#############################Helper functions for this map visualization################################
getYears<-function(state, years, envr){
    st<-get(state,envr)
    st<-gsub(" ","_",st)
    print(st)
	m<-readPartyPositionsFile(st)
        yearlist<-unique(m$year)


    assign(years,yearlist,env=envr)
  }

getPartyNames<-function(state, year, parties, envr){

       st<-get(state,envr)
       st<-gsub(" ","_",st)

       yr<-get(year,envr)
       cands<-readPartyPositionsFile(st)%>%filter(year==yr)

       partys<-unique(cands$party1)
       assign(parties,partys,env=envr)


    }

getOptions<-function(state,year,party,options,envr){
       st<-get(state,envr)
       st<-gsub(" ","_",st)

       yr<-get(year,envr)
       partyname<-get(party,envr)
       
       party_wise <-   readPartyPositionsFile(st)%>%filter(year==yr & party1 == partyname)
       shape<-readShapeFile(st, yr)
       #merge shape file with winners on ASSEMBLY and AC_No and set it as the leaflet data file
       #for creating a new leaflet map. Set this leaflet map in the current setting variable
       party_wise<-merge(shape,party_wise,by.x=c("ASSEMBLY"),by.y=c("ac_no"))
        assertthat::are_equal(nrow(shape),nrow(party_wise))
        party_wise<-addPopupInfo(party_wise)
        party_wise$Lat<-as.vector(coordinates(shape)[,2])
        party_wise$Long<-as.vector(coordinates(shape)[,1])
        #assign("mergedwinners",winners,env=envr)


    
       #winners<-get("mergedwinners",envr)
       #winners$position[winners$party1!=partyname]<-NA

        base<-leaflet(party_wise)
        print('leaflet value is set')
        assign("leafletbase",base,env=envr)

#set the count of  seats for each option
        tm<-party_wise
        tm<-subset(tm,select=c("year","position"))
        tm<-PartyPositionsMapLegendCount(tm)
        assign("countedframe",tm,env=envr)
        
        assign(options,PartyPositionsMapLegendList(),env=envr)
    

    }


plotMap<-function(state, year, party, options, plot, envr){
       st<-get(state,envr)
       yr<-get(year,envr)
       st<-gsub(" ","_",st)

        partyname<-get(party,envr)
       selectedfilters<-get(options,envr)
        counted<-get("countedframe",envr)
        base<-get("leafletbase",envr)
      #create a colour plaette only for the options selected in selectedfilters variable
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

pal<-leaflet::colorBin(cols,bins=PartyPositionsMapBreakupList(),na.color="white")
      #From the colors of legend remove white they are the colors/options not selected in the checkbox 
      legendcolors<-setdiff(cols,c("white"))
      legendvalues<- lapply(selectedfilters,function(y){
        counted$legend[(trimws(counted$tmp))==y]
      });
      print(legendvalues)
      #addpolygon for coloured display and add legend
      title<-paste0("Party wise positions for ",partyname," in ",gsub("_"," ",st)," - ",yr)

      base<-base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.numeric(position)), popup=~(popup)) %>%
        #addLegend("topright",pal=pal, values=(selectedfilters),opacity=1,title="Positions")
        addLegend("topright",colors=legendcolors, labels=legendvalues,opacity=1,title="Positions "
                  )%>%
        addTitleLeaflet(title)

assign(plot, base,env=envr)
    }
#######################################End of helper function ############################################
