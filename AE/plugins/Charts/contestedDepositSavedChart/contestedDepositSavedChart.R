contestedDepositSavedChart<-function(input, output, session, parentsession,statename_reactive,dname,conmanager){
  ###################Specific for voteshare chart visualization##########################################
  getOptions<-function(options,envr){
    #browser()
    assign(options,c("Total candidates","Deposit Saved"),env=envr)
  }

  plotChart<-function(state, options , plot,envr){
    selectedoptionnames<-get(options,envr)
    sname<-gsub(" ","_",get(state,envr))
  
    #create a base line chart with year as the x-axis
     b<-readCandidatesContestedDepositLostFile(sname)
        #pivotdata<-dcast(b,year~party)
        #create a base line chart with year as the x-axis
    base<-plot_ly(b, x = ~Year)
    lapply(selectedoptionnames,function(x) {
        n<-gsub(" ","_",x)
        print(paste('adding',x));
        
        base<<-add_trace(base,y=~get(n),name=x, type ='bar')
        }
        )
      sname<-gsub("_"," ",sname)
      thistitle<-paste0('Contested and deposit saved across years in ',sname)
      xtitle<-''
      ytitle<-'Number of Candidates'
      yrange<-c(0,5000)

    assign(plot,preparechartlayout(base,thistitle,xtitle,ytitle,yrange),env=envr)
  }
  #################################################################################################

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
 #ShowAll()
 tmp1 <-checkboxGroupInput(ns("cd_options") , "Select  ", c())
 tagList (
 tmp1) 
 })
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
getOptions(options="optionnames" , currentvalues)
updateCheckboxGroupInput(parentsession,ns("cd_options"),choices=currentvalues$optionnames,selected=conmanager$getval(ns("cd_options"),currentvalues$optionnames))
shinyjs::show("cd_options")
isolate({
 values$triggerfor_2<<-(values$triggerfor_2+1)%%2
})
}else{
updateCheckboxGroupInput(parentsession,ns("cd_options"),choices=c(),selected=c())
shinyjs::hide("cd_options")
}
})



SetupOutputRendering<-function(){
parentsession$output$distPlot<-renderPlotly({
currentvalues$selected_stname<<-statename_reactive()
currentvalues$selected_options<<-input$cd_options
if(T && isvalid(values$triggerfor_2,"numeric") && isvalid(currentvalues$selected_stname,"string") && isvalid(currentvalues$selected_options,"list"))
{
plotChart(state="selected_stname" , options="selected_options" , plot="plotlychart" , currentvalues)
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

