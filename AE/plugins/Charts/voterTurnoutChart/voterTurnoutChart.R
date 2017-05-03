voterTurnoutChart<-function(input, output, session, parentsession,statename_reactive,dname,conmanager){
  getGenderNames<-function(genders,envr){
    #browser()
    assign(genders,c("male","female","total"),env=envr)
  }

  plotChart<-function(state,gendernames , plot,envr){
    selectedgendernames<-get(gendernames,envr)
    sname<-gsub(" ","_",get(state,envr))
    b<-readVoterTurnoutFile(sname)

    #create a base line chart with year as the x-axis
    base<-plot_ly(b, x = ~year)
          lapply(selectedgendernames,function(x) {
        print(paste('adding',x));
        base<<-add_trace(base,y=~get(x),name=x,mode='lines+markers',showlegend=TRUE)
        }
        )
      sname<-gsub("_"," ",sname)
      thistitle<-paste0('Voter turnout across years in ',sname)
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
parentsession$output$ae_filter_selection<-renderUI({
 ShowAll()
 tagList(
checkboxGroupInput(ns("gender_names") , "Select turnout for ", c())) })
SetupOutputRendering()
}


ShowAll<-function(){
shinyjs::show("distPlot")
values$triggerfor_1<<-0
}


HideAll<-function(){
ResetOutputRendering()
values$triggerfor_1<<- -1
shinyjs::hide("distPlot")
}


observe({
currentvalues$selected_stname<<-statename_reactive()
if(T && isvalid(values$triggerfor_1,"numeric") && isvalid(currentvalues$selected_stname,"string"))
{
getGenderNames(genders="gendernames" , currentvalues)
updateCheckboxGroupInput(parentsession,ns("gender_names"),choices=currentvalues$gendernames,selected=conmanager$getval(ns("gender_names"),currentvalues$gendernames))
shinyjs::show("gender_names")
isolate({
 values$triggerfor_2<<-(values$triggerfor_2+1)%%2
})
}else{
updateCheckboxGroupInput(parentsession,ns("gender_names"),choices=c(),selected=c())
shinyjs::hide("gender_names")
}
})



SetupOutputRendering<-function(){
parentsession$output$distPlot<-renderPlotly({
currentvalues$selected_stname<<-statename_reactive()
currentvalues$selected_genders<<-input$gender_names
if(T && isvalid(values$triggerfor_2,"numeric") && isvalid(currentvalues$selected_stname,"string") && isvalid(currentvalues$selected_genders,"list"))
{
plotChart(state="selected_stname" , gendernames="selected_genders" , plot="plotlychart" , currentvalues)
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

