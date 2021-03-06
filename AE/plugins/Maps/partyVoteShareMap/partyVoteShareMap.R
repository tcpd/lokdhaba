partyVoteShareMap<-function(input, output, session, parentsession,statename_reactive,dname,conmanager){
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

       partyname<-get(party,envr)
       yr<-get(year,envr)
       winners <- readPartyPositionsFile(st)%>%filter(Year==yr & Party == partyname)
    
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


plotMap<-function(state, year,party,  options, plot, envr){
       st<-get(state,envr)
       yr<-get(year,envr)
       st<-gsub(" ","_",st)

       partyname<-get(party,envr)
       selectedfilters<-get(options,envr)
        counted<-get("countedframe",envr)
        base<-get("leafletbase",envr)

        #setting up variables for visualization data download
        df<- get("winners_df",envr)
        df$Pv_Legend <- getLegendIntervals(voteShareMapLegendList(),df$Vote_Share_Percentage)
        dat <- subset(df,Pv_Legend %in% selectedfilters,select = c("State_Name","Year","Constituency_No","Constituency_Name","Candidate","Party","Position","Votes","Vote_Share_Percentage"))
        conmanager$setval("visData",dat)
        conmanager$setval("selectedState",st)
        conmanager$setval("vis",paste("ConstituencyWise",partyname,"VoteShares",yr,sep="_"))
        
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
      title<-paste0("Vote share for ",partyname," in ",gsub("_"," ",st),"-",yr)

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
 tmp1 <-selectInput(ns("pvsI_year") ,  "Select Year", c() , selectize = TRUE)
 tmp2 <- if( T  & isvalid(currentvalues$selected_year,"string")){
 selectInput(ns("pvsI_party") , "Select Party" , c() , selectize = TRUE)
 } 
 else {
shinyjs::hidden(selectInput(ns("pvsI_party") , "Select Party" , c() , selectize = TRUE)) 
 }
 tmp3 <- if( T  & isvalid(currentvalues$selected_party,"string")){
 checkboxGroupInput(ns("pvsoptions") , "Select voteshare range ", c())
 } 
 else {
shinyjs::hidden(checkboxGroupInput(ns("pvsoptions") , "Select voteshare range ", c())) 
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
updateSelectInput(parentsession,ns("pvsI_year"),choices=currentvalues$yearlist,selected=conmanager$getval(ns("pvsI_year"),""))
shinyjs::show("pvsI_year")
isolate({
 values$triggerfor_2<<-(values$triggerfor_2+1)%%2
})
}else{
updateSelectInput(parentsession,ns("pvsI_year"),choices="",selected="")
shinyjs::hide("pvsI_year")
}
})



observe({
currentvalues$selected_year<<-input$pvsI_year
if(T && isvalid(values$triggerfor_2,"numeric") && isvalid(currentvalues$selected_year,"string"))
{
getPartyNames(state="selected_stname",year="selected_year" , parties="partynames",currentvalues)	
updateSelectInput(parentsession,ns("pvsI_party"),choices=currentvalues$partynames,selected=conmanager$getval(ns("pvsI_party"),""))
shinyjs::show("pvsI_party")
isolate({
 values$triggerfor_3<<-(values$triggerfor_3+1)%%2
})
}else{
updateSelectInput(parentsession,ns("pvsI_party"),choices="",selected="")
shinyjs::hide("pvsI_party")
}
})



observe({
currentvalues$selected_party<<-input$pvsI_party
if(T && isvalid(values$triggerfor_3,"numeric") && isvalid(currentvalues$selected_party,"string"))
{
getOptions(state="selected_stname" , year="selected_year",party="selected_party" , options="optionlist" , currentvalues)
updateCheckboxGroupInput(parentsession,ns("pvsoptions"),choices=currentvalues$optionlist,selected=conmanager$getval(ns("pvsoptions"),currentvalues$optionlist))
shinyjs::show("pvsoptions")
isolate({
 values$triggerfor_4<<-(values$triggerfor_4+1)%%2
})
}else{
updateCheckboxGroupInput(parentsession,ns("pvsoptions"),choices=c(),selected=c())
shinyjs::hide("pvsoptions")
}
})



SetupOutputRendering<-function(){
parentsession$output$mapPlot<-renderLeaflet({
currentvalues$selected_party<<-input$pvsI_party
currentvalues$selected_options<<-input$pvsoptions
if(T && isvalid(values$triggerfor_4,"numeric") && isvalid(currentvalues$selected_party,"string") && isvalid(currentvalues$selected_options,"list"))
{
plotMap(state="selected_stname" , year="selected_year",party="selected_party" , options="selected_options" , plot="leafletmap" , currentvalues)
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

