winnerMap<-function(input, output, session, parentsession,statename_reactive,dname,conmanager){
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

getPartyNames<-function(state, year, parties, envr){

       st<-get(state,envr)
    st<-gsub(" ","_",st)

       yr<-get(year,envr)
       winners<-readStateWinnersFile(st)%>%filter(Year==yr)

       partys<-unique(winners$Party)
       assign(parties,partys,env=envr)

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
      sset<-subset(counted,counted$Party %in% selectedpartynames)
      sset$color<-pal(as.character(sset$Party))

      #addpolygon for coloured display and add legend
      title<-paste0("Constituency wise party winners for ",gsub("_"," ",st)," in ",yr)

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

######Auto generated code##############Variable to store the values used across functions

           currentvalues<-new.env()

           ##get the session id

           ns<-session$ns

           ##Store passed directory name (name of the dir where this R file is stored).. It is interesting that using dname directly

           ##does not work because as that value changes in server.R it changes here at the point of use as well.

           dirname<-dname
 values<-reactiveValues(triggerfor_1=-1,triggerfor_2=-1,triggerfor_3=-1)


Setup<-function(){
parentsession$output$ae_filter_selection<-renderUI({
 #ShowAll()
 tmp1 <-selectInput(ns("wmI_year"),"Select Year", c() , selectize = TRUE)
 tmp2 <- if( T  & isvalid(currentvalues$selected_year,"string")){
 checkboxGroupInput(ns("wmparty_names") , "Select parties ", c())
 } 
 else {
shinyjs::hidden(checkboxGroupInput(ns("wmparty_names") , "Select parties ", c())) 
 }
 tagList (
 tmp1,
 tmp2) 
 })
SetupOutputRendering()
}


ShowAll<-function(){
shinyjs::show("mapPlot")
values$triggerfor_1<<-0
}


HideAll<-function(){
ResetOutputRendering()
values$triggerfor_1<<- -1
shinyjs::hide("mapPlot")
}


observe({
currentvalues$selected_stname<<-statename_reactive()
if(T && isvalid(values$triggerfor_1,"numeric") && isvalid(currentvalues$selected_stname,"string"))
{
getYears(state="selected_stname" , years="yearlist" , currentvalues)
updateSelectInput(parentsession,ns("wmI_year"),choices=currentvalues$yearlist,selected=conmanager$getval(ns("wmI_year"),""))
shinyjs::show("wmI_year")
isolate({
 values$triggerfor_2<<-(values$triggerfor_2+1)%%2
})
}else{
updateSelectInput(parentsession,ns("wmI_year"),choices="",selected="")
shinyjs::hide("wmI_year")
}
})



observe({
currentvalues$selected_year<<-input$wmI_year
if(T && isvalid(values$triggerfor_2,"numeric") && isvalid(currentvalues$selected_year,"string"))
{
getPartyNames(state="selected_stname" , year="selected_year", parties="partynames" , currentvalues)
updateCheckboxGroupInput(parentsession,ns("wmparty_names"),choices=currentvalues$partynames,selected=conmanager$getval(ns("wmparty_names"),currentvalues$partynames))
shinyjs::show("wmparty_names")
isolate({
 values$triggerfor_3<<-(values$triggerfor_3+1)%%2
})
}else{
updateCheckboxGroupInput(parentsession,ns("wmparty_names"),choices=c(),selected=c())
shinyjs::hide("wmparty_names")
}
})



SetupOutputRendering<-function(){
parentsession$output$mapPlot<-renderLeaflet({
currentvalues$selected_parties<<-input$wmparty_names
if(T && isvalid(values$triggerfor_3,"numeric") && isvalid(currentvalues$selected_parties,"list"))
{
plotMap(state="selected_stname" , year="selected_year", parties="selected_parties" , plot="leafletmap" , currentvalues)
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

