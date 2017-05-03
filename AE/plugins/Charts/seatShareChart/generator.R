source("utils/utils-metaprog.R")
library(data.table)
library(dplyr)


componentname="seatShareChart"
componentarguments="input, output, session, parentsession,statename_reactive,dname,conmanager"
plotarea="distPlot"
filteroutput="parentsession$output$ae_filter_selection"
#plotarea="distPlot"

filters = "filterid,construction,isinit
           1,checkboxGroupInput(ns(\"party_names\") : \"Select seatshare for \": c()),T"
    
       #1, checkboxGroupInput(ns(\"party_names\") : \"Select seatshare for \": c())"
    
inputtable="filterid,name,type,alias
            1,statename_reactive(),string,selected_stname 
            2,input$party_names,list,selected_parties"

##default can be allmultiple/allsingle/emptymultiple/emptysingle/NA.
outputtable="filterid,name,type,alias,default,label 
            1,party_names,updateCheckboxGroupInput,partynames,emptymultiple,\"Select seatshare for \"
            2,parentsession$output$distPlot,renderPlotly,plotlychart,NA,NA"



functiontable="filterid,funname 
              1,getPartyNames(state=\"selected_stname\" : parties=\"partynames\" : currentvalues)
              2,plotChart(state=\"selected_stname\" : parties=\"selected_parties\" : plot=\"plotlychart\" : currentvalues)"
##########################################################################


###############################################################################
l<-(commandArgs())
fname<-(gsub("--file=","",l[4]))
script.dir <-dirname(fname)

overallcode<-constructFunctions(inputtable,outputtable,functiontable,filteroutput,filters,script.dir,componentname,componentarguments,plotarea)
cat(overallcode,file=paste0(script.dir,"/",componentname,".R"))

