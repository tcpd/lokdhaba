voterTurnoutMap<-function(input, output, session, parentsession,dname,conmanager){
#############################Helper functions for this map visualization################################
getYears<-function(years, envr){
    m<-readStateWinnersFile("ge")
        
    yearlist<-unique(m$Year)
    yearlist <- yearlist[which(yearlist >=2008)]
    assign(years,yearlist,env=envr)
  }

getOptions<-function( year, options,envr){


       yr<-get(year,envr)
       winners<-readStateWinnersFile("ge")%>%filter(Year==yr)

       assign("winners_df",winners,env=envr)

   assign(options,VoterTurnoutMapLegendList(),env=envr)
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
        tm<-subset(tm,select=c("Year","Turnout_Percentage"))
        tm<-VoterTurnoutMapLegendCount(tm)
       
        assign("countedframe",tm,env=envr)
    


}

plotMap<-function(year, options, plot, envr){
       yr<-get(year,envr)

        selectedpercentage<-get(options,envr)

        counted<-get("countedframe",envr)
        base<-get("leafletbase",envr)
        
        
        
 #create a colour plaette only for the marrgins selected in selectedpercentage variable
      #pal<-createPal(selectedgendersnames, current_filters$sname, current_filters$year)
      cols<-c()
      optionslist<-VoterTurnoutMapLegendList()
      #setting up variables for visualization data download
      df <- get("winners_df",envr)
      df$Vt_Legend <- getLegendIntervals(optionslist,df$Turnout_Percentage)
      dat <- subset(df,Vt_Legend %in% selectedpercentage,select = c("State_Name","Year","Constituency_No","Constituency_Name","Turnout_Percentage","Vt_Legend"))
      conmanager$setval("visData",dat)
      conmanager$setval("selectedState","Loksabha")
      conmanager$setval("vis",paste("Voter_Turnout",yr,sep="_"))
      
      
      lapply(optionslist,function(x){
        if(x %in% selectedpercentage){
          cols<<-c(cols,VoterTurnoutMapLegendColor(x))
        }else{
          cols<<-c(cols,"white")
        }
      })
      pal<-leaflet::colorBin(cols,bins=VoterTurnoutMapBreakupList(),na.color="white")
      #coords<-current_filters$coords
      #From the colors of legend remove white they are the colors/options not selected in the checkbox 
      legendcolors<-setdiff(cols,c("white"))
      
      legendvalues<- lapply(selectedpercentage,function(y){
        counted$legend[(trimws(counted$tmp))==y]
      });
      
      #addpolygon for coloured display and add legend
      title<-paste0("Constituency wise voter turnout for Lok Sabha in ",yr)
      base<-base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.numeric(((Turnout_Percentage)))), popup=~(popup)) %>%
        addLegend("topright",colors=legendcolors, labels=legendvalues,opacity=1,title="Turnout"
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
parentsession$output$ge_filter_selection<-renderUI({
 #ShowAll()
 tmp1 <-selectInput(ns("I_year"),"Select Year", c() , selectize = TRUE)
 tmp2 <- if( T  & isvalid(currentvalues$selected_year,"string")){
 checkboxGroupInput(ns("filter_pname") , "Select range ", c())
 } 
 else {
shinyjs::hidden(checkboxGroupInput(ns("filter_pname") , "Select range ", c())) 
 }
 tagList (
 tmp1,
 tmp2) 
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
if(T && isvalid(values$triggerfor_1,"numeric"))
{
getYears(years="yearlist" , currentvalues)
updateSelectInput(parentsession,ns("I_year"),choices=currentvalues$yearlist,selected=conmanager$getval(ns("I_year"),""))
shinyjs::show("I_year")
isolate({
 values$triggerfor_2<<-(values$triggerfor_2+1)%%2
})
}else{
updateSelectInput(parentsession,ns("I_year"),choices="",selected="")
shinyjs::hide("I_year")
}
})



observe({
currentvalues$selected_year<<-input$I_year
if(T && isvalid(values$triggerfor_2,"numeric") && isvalid(currentvalues$selected_year,"string"))
{
getOptions(year="selected_year", options="options" , currentvalues)
updateCheckboxGroupInput(parentsession,ns("filter_pname"),choices=currentvalues$options,selected=conmanager$getval(ns("filter_pname"),currentvalues$options))
shinyjs::show("filter_pname")
isolate({
 values$triggerfor_3<<-(values$triggerfor_3+1)%%2
})
}else{
updateCheckboxGroupInput(parentsession,ns("filter_pname"),choices=c(),selected=c())
shinyjs::hide("filter_pname")
}
})



SetupOutputRendering<-function(){
parentsession$output$mapPlot<-renderLeaflet({
currentvalues$selected_year<<-input$I_year
currentvalues$selected_range<<-input$filter_pname
if(T && isvalid(values$triggerfor_3,"numeric") && isvalid(currentvalues$selected_year,"string") && isvalid(currentvalues$selected_range,"list"))
{
plotMap(year="selected_year", options="selected_range" , plot="leafletmap" , currentvalues)
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

