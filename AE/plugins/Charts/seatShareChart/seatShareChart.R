seatShareChart<-function(input, output, session, parentsession,statename_reactive,dname,conmanager){
  
  ###################Specific for this visualization##########################################
  getPartyNames<-function(state,parties,envr){
    #browser()
    sname<-gsub(" ","_",get(state,envr))
    b<-readSeatShareFile(sname)
    assign(parties,as.vector(unique(b$party)),env=envr)
  }

  plotChart<-function(state, parties , plot,envr){
    selectedpartynames<-get(parties,envr)
    sname<-gsub(" ","_",get(state,envr))
    b<-readSeatShareFile(sname)
    pivotdata<-dcast(b,year~party,value.var=c('seats'))
    #create a base line chart with year as the x-axis
    base<-plot_ly(pivotdata, x = ~year)
    #print(paste('selected',selectedpartynames))
    # #for each selected party in the input "filter_pname" id (checkbox) add a new trace
    # #corresponding to that party
    lapply(selectedpartynames,function(x) {print(paste('adding',x));base<<-add_trace(base,y=~get(x),name=x,type='scatter',mode='lines+markers')})
    sname<-gsub("_"," ",sname)
    thistitle<-paste0('Party wise seatshare across years in ',sname)
    xtitle<-''
    ytitle<-'Seat share %'
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
parentsession$output$ae_filter_selection<-renderUI({
 #ShowAll()
 tmp1 <-checkboxGroupInput(ns("party_names") , "Select seatshare for ", c())
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
getPartyNames(state="selected_stname" , parties="partynames" , currentvalues)
updateCheckboxGroupInput(parentsession,ns("party_names"),choices=currentvalues$partynames,selected=conmanager$getval(ns("party_names"),c()))
shinyjs::show("party_names")
isolate({
 values$triggerfor_2<<-(values$triggerfor_2+1)%%2
})
}else{
updateCheckboxGroupInput(parentsession,ns("party_names"),choices=c(),selected=c())
shinyjs::hide("party_names")
}
})



SetupOutputRendering<-function(){
parentsession$output$distPlot<-renderPlotly({
currentvalues$selected_parties<<-input$party_names
if(T && isvalid(values$triggerfor_2,"numeric") && isvalid(currentvalues$selected_parties,"list"))
{
plotChart(state="selected_stname" , parties="selected_parties" , plot="plotlychart" , currentvalues)
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

