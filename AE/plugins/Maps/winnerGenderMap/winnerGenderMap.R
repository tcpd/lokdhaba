winnerGenderMap<-function(input, output, session, parentsession,statename_reactive,dname,conmanager){
#############################Helper functions for this map visualization################################
getYears<-function(state, years, envr){
    st<-get(state,envr)
    st<-gsub(" ","_",st)
    print(st)
    m<-readStateWinnersFile(st)
        
    yearlist<-unique(m$year)

    assign(years,yearlist,env=envr)
  }

getOptions<-function(state, year, options,envr){

       st<-get(state,envr)
       st<-gsub(" ","_",st)

       yr<-get(year,envr)
       winners<-readStateWinnersFile(st)%>%filter(year==yr)


   assign(options,WinnerGenderMapLegendList(),env=envr)

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
    
   #set the count of winning seats for each gender
        tm<-winners
        #browser()
        tm<-subset(tm,select=c("year","sex1"))
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
                    fillColor = ~pal(as.character(trimws((sex1)))), popup=~(popup)) %>%
        addLegend("topright",pal=pal, opacity= 1, values=as.character(selectedgendersnames),title="Gender",
                  labFormat = labelFormat(transform=function(x) {
                    lapply(x,function(y){
                      counted$legend[(trimws(counted$sex1))==y]
                    });
                  })
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
 ShowAll()
 tagList(
selectInput(ns("wgenderI_year"),"Select Year", c() , selectize = TRUE),
shinyjs::hidden(checkboxGroupInput(ns("wgender_names") , "Select gender type ", c()))) })
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
updateSelectInput(parentsession,ns("wgenderI_year"),choices=currentvalues$yearlist,selected=conmanager$getval(ns("wgenderI_year"),""))
shinyjs::show("wgenderI_year")
isolate({
 values$triggerfor_2<<-(values$triggerfor_2+1)%%2
})
}else{
updateSelectInput(parentsession,ns("wgenderI_year"),choices="",selected="")
shinyjs::hide("wgenderI_year")
}
})



observe({
currentvalues$selected_year<<-input$wgenderI_year
if(T && isvalid(values$triggerfor_2,"numeric") && isvalid(currentvalues$selected_year,"string"))
{
getOptions(state="selected_stname" , year="selected_year", options="options" , currentvalues)
updateCheckboxGroupInput(parentsession,ns("wgender_names"),choices=currentvalues$options,selected=conmanager$getval(ns("wgender_names"),currentvalues$options))
shinyjs::show("wgender_names")
isolate({
 values$triggerfor_3<<-(values$triggerfor_3+1)%%2
})
}else{
updateCheckboxGroupInput(parentsession,ns("wgender_names"),choices=c(),selected=c())
shinyjs::hide("wgender_names")
}
})



SetupOutputRendering<-function(){
parentsession$output$mapPlot<-renderLeaflet({
currentvalues$selected_year<<-input$wgenderI_year
currentvalues$selected_options<<-input$wgender_names
if(T && isvalid(values$triggerfor_3,"numeric") && isvalid(currentvalues$selected_year,"string") && isvalid(currentvalues$selected_options,"list"))
{
plotMap(state="selected_stname" , year="selected_year", options="selected_options" , plot="leafletmap" , currentvalues)
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

