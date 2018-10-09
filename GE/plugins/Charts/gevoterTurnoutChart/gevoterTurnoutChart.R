gevoterTurnoutChart<-function(input, output, session, parentsession,dname,conmanager){
  getGenderNames<-function(genders,envr){
    #browser()
    assign(genders,c("male","female","total"),env=envr)
  }

  plotChart<-function(gendernames , plot,envr){
    selectedgendernames<-get(gendernames,envr)
    b<-readVoterTurnoutFile("ge")

    #setting up variables for visualization data download
    dat <- subset(b,select = c("Year",gsub(" ","_",selectedgendernames)))
    dat$State_Name <- "LokSabha"
    conmanager$setval("visData",dat)
    conmanager$setval("selectedState","LokSabha")
    conmanager$setval("vis","VoterTurnout")
    
    #create a base line chart with year as the x-axis
    base<-plot_ly(b, x = ~Year)
          lapply(selectedgendernames,function(x) {
        print(paste('adding',x));
        base<<-add_trace(base,y=~get(x),name=x,mode='lines+markers',showlegend=TRUE)
        }
        )
      thistitle<-paste0('Voter turnout across years in LokSabha')
      xtitle<-''
      ytitle<-'Turnout in %'
      yrange<-c(0,100)
    assign(plot, preparechartlayout(base,thistitle,xtitle,ytitle,yrange),env=envr)

  }

######Auto generated code##############Variable to store the values used across functions

           currentvalues<-new.env()

           ##get the session id

           ns<-session$ns

           ##Store passed directory name (name of the dir where this R file is stored).. It is interesting that using dname directly

           ##does not work because as that value changes in server.R it changes here at the point of use as well.

           dirname<-dname
 values<-reactiveValues(triggerfor_1=-1,triggerfor_2=-1)


Setup<-function(){
parentsession$output$ge_filter_selection<-renderUI({
 #ShowAll()
 tmp1 <-checkboxGroupInput(ns("filter_pname") , "Select turnout for ", c())
 tagList (
 tmp1) 
 })
SetupOutputRendering()
}


ShowAll<-function(){
shinyjs::show("distPlot")
shinyjs::show("bookmark_edv")
shinyjs::show("visDataDownload")
values$triggerfor_1<<-0
}


HideAll<-function(){
ResetOutputRendering()
values$triggerfor_1<<- -1
shinyjs::hide("distPlot")
shinyjs::hide("bookmark_edv")
shinyjs::hide("visDataDownload")
}


observe({
if(T && isvalid(values$triggerfor_1,"numeric"))
{
getGenderNames(genders="gendernames" , currentvalues)
updateCheckboxGroupInput(parentsession,ns("filter_pname"),choices=currentvalues$gendernames,selected=conmanager$getval(ns("filter_pname"),currentvalues$gendernames))
shinyjs::show("filter_pname")
isolate({
 values$triggerfor_2<<-(values$triggerfor_2+1)%%2
})
}else{
updateCheckboxGroupInput(parentsession,ns("filter_pname"),choices=c(),selected=c())
shinyjs::hide("filter_pname")
}
})



SetupOutputRendering<-function(){
parentsession$output$distPlot<-renderPlotly({
currentvalues$selected_genders<<-input$filter_pname
if(T && isvalid(values$triggerfor_2,"numeric") && isvalid(currentvalues$selected_genders,"list"))
{
plotChart(gendernames="selected_genders" , plot="plotlychart" , currentvalues)
currentvalues$plotlychart
}else{
return()
}
})




}

ResetOutputRendering<-function(){
parentsession$output$distPlot<-renderPlotly({
return()})


}



ret<-c()
ret$HideAll<-HideAll
ret$ShowAll<-ShowAll
ret$Setup<-Setup
ret$SetupOutputRendering<-SetupOutputRendering
return (ret)


}

