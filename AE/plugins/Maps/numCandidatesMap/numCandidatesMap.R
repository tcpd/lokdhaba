numCandidatesMap<-function(input, output, session, parentsession,statename_reactive,dname,conmanager){
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


   assign(options,NumCandidatesMapLegendList(),env=envr)

       shape<-readShapeFile(st, yr)
       #merge shape file with winners on ASSEMBLY and AC_No and set it as the leaflet data file
       #for creating a new leaflet map. Set this leaflet map in the current setting variable
        winners<-merge(shape,winners,by.x=c("ASSEMBLY"),by.y=c("Constituency_No"))
        assertthat::are_equal(nrow(shape),nrow(winners))
        winners<-addPopupInfo(winners)
        winners$Lat<-as.vector(coordinates(shape)[,2])
        winners$Long<-as.vector(coordinates(shape)[,1])
        
	base<-leaflet(winners,options = leafletOptions(minZoom=6,maxZoom=10))
        print('leaflet value is set')
        assign("leafletbase",base,env=envr)
     #set the count of winning seats for each victory margin
        tm<-winners
        tm<-subset(tm,select=c("Year","N_Cand"))
        tm<-NumCandidatesMapLegendCount(tm)
       
        assign("countedframe",tm,env=envr)
    


}

plotMap<-function(state, year, options, plot, envr){
       st<-get(state,envr)
       yr<-get(year,envr)
    st<-gsub(" ","_",st)

        selectedcount<-get(options,envr)

        counted<-get("countedframe",envr)
        base<-get("leafletbase",envr)
  #create a colour plaette only for the numbers selected in selectedcount variable
      #pal<-createPal(selectedgendersnames, current_filters$sname, current_filters$year)
      cols<-c()
      optionslist<-NumCandidatesMapLegendList()
      lapply(optionslist,function(x){
        if(x %in% selectedcount){
          cols<<-c(cols,NumCandidatesMapLegendColor(x))
        }else{
          cols<<-c(cols,"white")
        }
      })
      pal<-leaflet::colorBin(cols,bins=NumCandidatesMapBreakupList(),na.color="white")
      #coords<-current_filters$coords
      #From the colors of legend remove white they are the colors/options not selected in the checkbox 
      legendcolors<-setdiff(cols,c("white"))
      legendvalues<- lapply(selectedcount,function(y){
        counted$legend[(trimws(counted$tmp))==y]
      });
      
      #addpolygon for coloured display and add legend
      title<-paste0("Constituency wise Candidate count for ",gsub("_"," ",st)," in ",yr)

      base<-base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.numeric(((N_Cand)))), popup=~(popup)) %>%
        addLegend("topright",colors=legendcolors, labels=legendvalues,opacity=1,title="Number of contesting candidates"
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
 tmp1 <-selectInput(ns("numcandI_year"),"Select Year", c() , selectize = TRUE)
 tmp2 <- if( T  & isvalid(currentvalues$selected_year,"string")){
 checkboxGroupInput(ns("numcand_names") , "Select number of candidates ", c())
 } 
 else {
shinyjs::hidden(checkboxGroupInput(ns("numcand_names") , "Select number of candidates ", c())) 
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
updateSelectInput(parentsession,ns("numcandI_year"),choices=currentvalues$yearlist,selected=conmanager$getval(ns("numcandI_year"),""))
shinyjs::show("numcandI_year")
isolate({
 values$triggerfor_2<<-(values$triggerfor_2+1)%%2
})
}else{
updateSelectInput(parentsession,ns("numcandI_year"),choices="",selected="")
shinyjs::hide("numcandI_year")
}
})



observe({
currentvalues$selected_year<<-input$numcandI_year
if(T && isvalid(values$triggerfor_2,"numeric") && isvalid(currentvalues$selected_year,"string"))
{
getOptions(state="selected_stname" , year="selected_year", options="numoptions" , currentvalues)
updateCheckboxGroupInput(parentsession,ns("numcand_names"),choices=currentvalues$numoptions,selected=conmanager$getval(ns("numcand_names"),currentvalues$numoptions))
shinyjs::show("numcand_names")
isolate({
 values$triggerfor_3<<-(values$triggerfor_3+1)%%2
})
}else{
updateCheckboxGroupInput(parentsession,ns("numcand_names"),choices=c(),selected=c())
shinyjs::hide("numcand_names")
}
})



SetupOutputRendering<-function(){
parentsession$output$mapPlot<-renderLeaflet({
currentvalues$selected_year<<-input$numcandI_year
currentvalues$selected_numbers<<-input$numcand_names
if(T && isvalid(values$triggerfor_3,"numeric") && isvalid(currentvalues$selected_year,"string") && isvalid(currentvalues$selected_numbers,"list"))
{
plotMap(state="selected_stname" , year="selected_year", options="numoptions" , plot="leafletmap" , currentvalues)
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

