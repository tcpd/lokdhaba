partyPositionsMap<-function(input, output, session, parentsession,statename_reactive,dname,conmanager){
#############################Helper functions for this map visualization################################
getYears<-function(state, years, envr){
    st<-get(state,envr)
    st<-gsub(" ","_",st)
    print(st)
	m<-readPartyPositionsFile(st)
        yearlist<-unique(m$Year)
    yearlist <- yearlist[which(yearlist >=2008)]

    assign(years,yearlist,env=envr)
  }

getPartyNames<-function(state, year, parties, envr){

       st<-get(state,envr)
       st<-gsub(" ","_",st)

       yr<-get(year,envr)
       cands<-readPartyPositionsFile(st)%>%filter(Year==yr)

       partys<-unique(cands$Party)
       assign(parties,partys,env=envr)


    }

getOptions<-function(state,year,party,options,envr){
       st<-get(state,envr)
       st<-gsub(" ","_",st)

       yr<-get(year,envr)
       partyname<-get(party,envr)
       
       party_wise <-   readPartyPositionsFile(st)%>%filter(Year==yr & Party == partyname)
      
       assign("winners_df",party_wise,env=envr)
       
       shape<-readShapeFile(st, yr)
       #merge shape file with winners on ASSEMBLY and AC_No and set it as the leaflet data file
       #for creating a new leaflet map. Set this leaflet map in the current setting variable
       party_wise<-merge(shape,party_wise,by.x=c("ASSEMBLY"),by.y=c("Constituency_No"))
        assertthat::are_equal(nrow(shape),nrow(party_wise))
        party_wise<-addPopupInfo(party_wise)
        party_wise$Lat<-as.vector(coordinates(shape)[,2])
        party_wise$Long<-as.vector(coordinates(shape)[,1])
        #assign("mergedwinners",winners,env=envr)


    
       #winners<-get("mergedwinners",envr)
       #winners$position[winners$party1!=partyname]<-NA

        base<-leaflet(party_wise,options = leafletOptions(minZoom=6,maxZoom=10,zoomSnap=0.2,zoomDelta=0.2,scrollWheelZoom=F,touchZoom=F))
        print('leaflet value is set')
        assign("leafletbase",base,env=envr)

#set the count of  seats for each option
        tm<-party_wise
        tm<-subset(tm,select=c("Year","Position"))
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
      
        #setting up variables for visualization data download
        df<- get("winners_df",envr)
        df$P_Legend <- getLegendIntervals(PartyPositionsMapLegendList(),df$Position)
        dat <- subset(df,P_Legend %in% selectedfilters,select = c("State_Name","Year","Constituency_No","Constituency_Name","Candidate","Party","Position","Votes"))
        conmanager$setval("visData",dat)
        conmanager$setval("selectedState",st)
        conmanager$setval("vis",paste("ConstituencyWise",partyname,"Positions",yr,sep="_"))
        
        
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
                    fillColor = ~pal(as.numeric(Position)), popup=~(popup)) %>%
        #addLegend("topright",pal=pal, values=(selectedfilters),opacity=1,title="Positions")
        addLegend("topright",colors=legendcolors, labels=legendvalues,opacity=1,title="Positions "
                  )%>%
        addTitleLeaflet(title)

assign(plot, base,env=envr)
    }
#######################################End of helper function ############################################

######Auto generated code##############Variable to store the values used across functions

           currentvalues<-new.env()

           ##get the session id

           ns<-session$ns

           ##Store passed directory name (name of the dir where this R file is stored).. It is interesting that using dname directly

           ##does not work because as that value changes in server.R it changes here at the point of use as well.

           dirname<-dname
 values<-reactiveValues(triggerfor_1=-1,triggerfor_2=-1,triggerfor_3=-1,triggerfor_4=-1)


Setup<-function(){
parentsession$output$ae_filter_selection<-renderUI({
 #ShowAll()
 tmp1 <-selectInput(ns("partyposI_year") ,  "Select Year", c() , selectize = TRUE)
 tmp2 <- if( T  & isvalid(currentvalues$selected_year,"string")){
 selectInput(ns("partyposI_party") , "Select Party" , c() , selectize = TRUE)
 } 
 else {
shinyjs::hidden(selectInput(ns("partyposI_party") , "Select Party" , c() , selectize = TRUE)) 
 }
 tmp3 <- if( T  & isvalid(currentvalues$selected_party,"string")){
 checkboxGroupInput(ns("partyposoptions") , "Select positions ", c())
 } 
 else {
shinyjs::hidden(checkboxGroupInput(ns("partyposoptions") , "Select positions ", c())) 
 }
 tagList (
 tmp1,
 tmp2,
 tmp3) 
 })
SetupOutputRendering()
}


ShowAll<-function(){
shinyjs::show("mapPlot")
shinyjs::show("bookmark_edv")
shinyjs::show("visDataDownload")
values$triggerfor_1<<-0
}


HideAll<-function(){
ResetOutputRendering()
values$triggerfor_1<<- -1
shinyjs::hide("mapPlot")
shinyjs::hide("bookmark_edv")
shinyjs::hide("visDataDownload")
}


observe({
currentvalues$selected_stname<<-statename_reactive()
if(T && isvalid(values$triggerfor_1,"numeric") && isvalid(currentvalues$selected_stname,"string"))
{
getYears(state="selected_stname" , years="yearlist" , currentvalues)
updateSelectInput(parentsession,ns("partyposI_year"),choices=currentvalues$yearlist,selected=conmanager$getval(ns("partyposI_year"),""))
shinyjs::show("partyposI_year")
isolate({
 values$triggerfor_2<<-(values$triggerfor_2+1)%%2
})
}else{
updateSelectInput(parentsession,ns("partyposI_year"),choices="",selected="")
shinyjs::hide("partyposI_year")
}
})



observe({
currentvalues$selected_year<<-input$partyposI_year
if(T && isvalid(values$triggerfor_2,"numeric") && isvalid(currentvalues$selected_year,"string"))
{
getPartyNames(state="selected_stname" , year="selected_year", parties="partynames" , currentvalues)
updateSelectInput(parentsession,ns("partyposI_party"),choices=currentvalues$partynames,selected=conmanager$getval(ns("partyposI_party"),""))
shinyjs::show("partyposI_party")
isolate({
 values$triggerfor_3<<-(values$triggerfor_3+1)%%2
})
}else{
updateSelectInput(parentsession,ns("partyposI_party"),choices="",selected="")
shinyjs::hide("partyposI_party")
}
})



observe({
currentvalues$selected_party<<-input$partyposI_party
if(T && isvalid(values$triggerfor_3,"numeric") && isvalid(currentvalues$selected_party,"string"))
{
getOptions(state="selected_stname" , year="selected_year" , party="selected_party" , options="optionlist" , currentvalues)
updateCheckboxGroupInput(parentsession,ns("partyposoptions"),choices=currentvalues$optionlist,selected=conmanager$getval(ns("partyposoptions"),currentvalues$optionlist))
shinyjs::show("partyposoptions")
isolate({
 values$triggerfor_4<<-(values$triggerfor_4+1)%%2
})
}else{
updateCheckboxGroupInput(parentsession,ns("partyposoptions"),choices=c(),selected=c())
shinyjs::hide("partyposoptions")
}
})



SetupOutputRendering<-function(){
parentsession$output$mapPlot<-renderLeaflet({
currentvalues$selected_party<<-input$partyposI_party
currentvalues$selected_options<<-input$partyposoptions
if(T && isvalid(values$triggerfor_4,"numeric") && isvalid(currentvalues$selected_party,"string") && isvalid(currentvalues$selected_options,"list"))
{
plotMap(state="selected_stname" , year="selected_year", party="selected_party" , options="selected_options" , plot="leafletmap" , currentvalues)
currentvalues$leafletmap
}else{
return()
}
})




}

ResetOutputRendering<-function(){
parentsession$output$mapPlot<-renderLeaflet({
return()})


}



ret<-c()
ret$HideAll<-HideAll
ret$ShowAll<-ShowAll
ret$Setup<-Setup
ret$SetupOutputRendering<-SetupOutputRendering
return (ret)


}

