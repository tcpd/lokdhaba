gewinnerMap<-function(input, output, session, parentsession,dname,conmanager){
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
        
	base<-leaflet(winners,options = leafletOptions(minZoom=5,maxZoom=11,zoomSnap=0.02,zoomDelta=0.02,scrollWheelZoom=F,touchZoom=F))
        print('leaflet value is set')
        assign("leafletbase",base,env=envr)
    
        #set the count of winning seats for each party
        tm<-winners
        tm<-subset(tm,select=c("Year","Party"))
        tm$count<-1
        tm<-aggregate(count~Year+Party,tm,function(x) length(x))
        tm$legend<-paste0(tm$Party," (",tm$count,")")
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
      title<-paste0("Constituency wise party winners for Lok Sabha in ",yr)

      base<-base %>% 
        addPolygons(stroke = TRUE, fillOpacity = 1, smoothFactor = 1,
                    color = "#000000", opacity = 1, weight=1,
                    fillColor = ~pal(as.character(Party)), popup=~(popup)) %>%
      addLegend("topright",color=sset$color, opacity= 1, labels=sset$legend,title="Party",
        )%>%
        addTitleLeaflet(title)

    assign(plot, base,env=envr)
}
user.custom.map <- function(year,parties,plot,mfile,envr){
  #browser()
  
  yr<-get(year,envr)
  selectedpartynames<-get(parties,envr)
  counted<-get("countedframe",envr)
  pal<-getColorFactorParty(selectedpartynames)
  sset<-subset(counted,counted$Party %in% selectedpartynames)
  sset$color<-pal(as.character(sset$Party))
  
  
  plot <- get(plot,envr)
  plot <- plot %>% clearControls() %>% addLegend("topright",color=sset$color, opacity= 1, labels=sset$legend,title="Party") %>% addControl(html=paste0("<p class=\"leaflet-tcpd\">Source: Adapted from <a href=&quot;www.eci.nic.in&quot;>ECI Data</a><br>",
                                         "<a href=&quot;www.tcpd.ashoka.edu.in&quot;>Trivedi Centre for Political Data, Ashoka University</a></p>"),position = "bottomleft",className="leaflettitle")
  mapimg <- mapshot( x = plot
           , file = mfile
           , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
           , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
  )
  
  assign("savemap",mapimg,env=envr)
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
 checkboxGroupInput(ns("filter_pname") , "Select parties ", c())
 } 
 else {
shinyjs::hidden(checkboxGroupInput(ns("filter_pname") , "Select parties ", c())) 
 }
 tagList (
 tmp1,
 tmp2) 
 })
SetupOutputRendering()
}


ShowAll<-function(){
shinyjs::show("mapPlot")
shinyjs::show("dl")  
values$triggerfor_1<<-0
}


HideAll<-function(){
ResetOutputRendering()
values$triggerfor_1<<- -1
shinyjs::hide("mapPlot")
shinyjs::hide("dl")
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
getPartyNames(year="selected_year", parties="partynames" , currentvalues)
updateCheckboxGroupInput(parentsession,ns("filter_pname"),choices=currentvalues$partynames,selected=conmanager$getval(ns("filter_pname"),currentvalues$partynames))
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
currentvalues$selected_parties<<-input$filter_pname
if(T && isvalid(values$triggerfor_3,"numeric") && isvalid(currentvalues$selected_year,"string") && isvalid(currentvalues$selected_parties,"list"))
{
plotMap(year="selected_year", parties="selected_parties" , plot="leafletmap" , currentvalues)
currentvalues$leafletmap
}else{
return()
}
})

parentsession$output$dl <- downloadHandler(
  filename = paste0( Sys.Date()
                     , "_GE_winners_by_party_tcpd"
                     , ".png"
  )
  
  , content = function(file) {
    user.custom.map(year="selected_year", parties="selected_parties" ,plot="leafletmap",mfile=file , currentvalues)
    currentvalues$savemap
  } # end of content() function
) # end of downloadHandler()   function



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

