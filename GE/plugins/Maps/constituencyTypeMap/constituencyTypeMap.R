constituencyTypeMap<-function(input, output, session, parentsession,dname,conmanager){
#############################Helper functions for this map visualization################################
getYears<-function(years, envr){
    m<-readStateWinnersFile("ge")
        
    yearlist<-unique(m$Year)
    yearlist <- yearlist[which(yearlist >= 2008)]
    assign(years,yearlist,env=envr)
  }

getOptions<-function(year, options,envr){


       yr<-get(year,envr)
       winners<-readStateWinnersFile("ge")%>%filter(Year==yr)
       assign("winners_df",winners,env=envr)
       

   assign(options,WinnerCasteMapLegendList(),env=envr)

       shape<-readShapeFile("ge", yr)
       #merge shape file with winners on ASSEMBLY and AC_No and set it as the leaflet data file
       #for creating a new leaflet map. Set this leaflet map in the current setting variable
        winners<-merge(shape,winners,by.x=c("STATE_UT","PC_NO"),by.y=c("State_Name","Constituency_No"))
        assertthat::are_equal(nrow(shape),nrow(winners))
        winners<-addPopupInfo(winners)
        winners$Lat<-as.vector(coordinates(shape)[,2])
        winners$Long<-as.vector(coordinates(shape)[,1])
        
	base<-leaflet(winners,options = leafletOptions(minZoom=5,maxZoom=11,,zoomSnap=0.2,zoomDelta=0.2,scrollWheelZoom=F,touchZoom=F))
        print('leaflet value is set')
        assign("leafletbase",base,env=envr)
    
  #set the count of winning seats for each ac type
        tm<-winners
        #browser()
        tm<-subset(tm,select=c("Year","Constituency_Type"))
        tm<-WinnerCasteMapLegendCount(tm)
        assign("countedframe",tm,env=envr)
    


}

plotMap<-function(year, options, plot, envr){
       yr <- get(year,envr)
        selectedcastenames<-get(options,envr)

        counted<-get("countedframe",envr)
        base<-get("leafletbase",envr)
        
        #setting up variables for visualization data download
        df<- get("winners_df",envr)
        dat <- subset(df,Constituency_Type %in% selectedcastenames,select = c("State_Name","Year","Constituency_No","Constituency_Name","Constituency_Type"))
        conmanager$setval("visData",dat)
        conmanager$setval("selectedState","Loksabha")
        conmanager$setval("vis",paste("Constituency_Types",yr,sep="_"))
        
 #create a colour plaette only for the partys selected in selectedcastenames variable
      #pal<-createPal(selectedgendersnames, current_filters$sname, current_filters$year)
      cols<-c()
      lapply(selectedcastenames,function(x){
          cols<<-c(cols,WinnerCasteMapLegendColor(x))
      })
      
      pal<- leaflet::colorFactor(cols,levels=selectedcastenames,na.color = "white")
      
      #addpolygon for coloured display and add legend
      title<-paste0("Constituency types for Lok Sabha in ",yr)

      base<-base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.character(trimws((Constituency_Type)))), popup=~(popup)) %>%
        addLegend("topright",pal=pal, opacity= 1,
                  values=as.character(selectedcastenames),title="Constituency Type",
                  labFormat = labelFormat(transform=function(x) {
                    lapply(x,function(y){
                      counted$legend[(trimws(counted$Constituency_Type))==y]
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
parentsession$output$ge_filter_selection<-renderUI({
 #ShowAll()
 tmp1 <-selectInput(ns("I_year"),"Select Year", c() , selectize = TRUE)
 tmp2 <- if( T  & isvalid(currentvalues$selected_year,"string")){
 checkboxGroupInput(ns("filter_pname") , "Select constituency type ", c())
 } 
 else {
shinyjs::hidden(checkboxGroupInput(ns("filter_pname") , "Select constituency type ", c())) 
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
currentvalues$selected_options<<-input$filter_pname
if(T && isvalid(values$triggerfor_3,"numeric") && isvalid(currentvalues$selected_year,"string") && isvalid(currentvalues$selected_options,"list"))
{
plotMap(year="selected_year", options="selected_options" , plot="leafletmap" , currentvalues)
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

