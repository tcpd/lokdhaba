gecontestedDepositSavedChart<-function(input, output, session, parentsession,dname,conmanager){
  ###################Specific for voteshare chart visualization##########################################
  getOptions<-function(options,envr){
    #browser()
    assign(options,c("Total Candidates","Deposit Lost"),env=envr)
  }

  plotChart<-function( options , plot,envr){
    selectedoptionnames<-get(options,envr)
  
    #create a base line chart with year as the x-axis
     b<-readCandidatesContestedDepositLostFile("ge")
        #pivotdata<-dcast(b,year~party)
        #create a base line chart with year as the x-axis
    base<-plot_ly(b, x = ~Year)
    lapply(selectedoptionnames,function(x) {
        n<-gsub(" ","_",x)
        print(paste('adding',x));
        
        base<<-add_trace(base,y=~get(n),name=x, type ='bar')
        }
        )
      thistitle<-paste0('Contested and deposit lost across years in LokSabha')
      xtitle<-''
      ytitle<-'Number of Candidates'
      yrange <-c(0,15000)
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
parentsession$output$ge_filter_selection<-renderUI({
 #ShowAll()
 tmp1 <-checkboxGroupInput(ns("filter_pname") , "Select  ", c())
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
if(T && isvalid(values$triggerfor_1,"numeric"))
{
getOptions(options="optionnames" , currentvalues)
updateCheckboxGroupInput(parentsession,ns("filter_pname"),choices=currentvalues$optionnames,selected=conmanager$getval(ns("filter_pname"),currentvalues$optionnames))
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
currentvalues$selected_options<<-input$filter_pname
if(T && isvalid(values$triggerfor_2,"numeric") && isvalid(currentvalues$selected_options,"list"))
{
plotChart(options="selected_options" , plot="plotlychart" , currentvalues)
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

