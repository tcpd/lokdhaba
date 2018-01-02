gevoteShareChart_c<-function(input, output, session, parentsession,dname,conmanager){
  ###################Specific for voteshare chart visualization##########################################
  getPartyNames<-function(parties,envr){
    #browser()
    b<-readcVoteShareFile("ge")
    
    assign(parties,as.list(unique(b$Party)),env=envr)
  }

  plotChart<-function( parties , plot,envr){
    selectedpartynames<-get(parties,envr)
    #sname<-gsub(" ","_",get(state,envr))
    b<-readcVoteShareFile("ge")
    #browser()
    pivotdata<-dcast(b,Year~Party,value.var=c('Votes'))

    #create a base line chart with year as the x-axis
    base<-plot_ly(pivotdata, x = ~Year)
    #print(paste('selected',selectedpartynames))
    # #for each selected party in the input "filter_pname" id (checkbox) add a new trace
    # #corresponding to that party
    lapply(selectedpartynames,function(x) {print(paste('adding',x));base<<-add_trace(base,y=~get(x),name=x,type='scatter',mode='lines+markers')})
    #sname<-gsub("_"," ",sname)
    thistitle<-paste0('Party wise voteshare in seats contested across years in GE')
    xtitle<-''
    ytitle<-'Vote share %'
    yrange<-c(0,100)
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
 tmp1 <-checkboxGroupInput(ns("filter_pnames") , "Select voteshare for ", c())
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
getPartyNames( parties="gepartynames" , currentvalues)
updateCheckboxGroupInput(parentsession,ns("filter_pnames"),choices=currentvalues$gepartynames,selected=conmanager$getval(ns("filter_pnames"),c()))
shinyjs::show("filter_pnames")
isolate({
 values$triggerfor_2<<-(values$triggerfor_2+1)%%2
})
}else{
updateCheckboxGroupInput(parentsession,ns("filter_pnames"),choices=c(),selected=c())
shinyjs::hide("filter_pnames")
}
})



SetupOutputRendering<-function(){
parentsession$output$distPlot<-renderPlotly({
currentvalues$selected_parties<<-input$filter_pnames
if(T && isvalid(values$triggerfor_2,"numeric") && isvalid(currentvalues$selected_parties,"list"))
{
plotChart(parties="selected_parties" , plot="plotlychart" , currentvalues)
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

