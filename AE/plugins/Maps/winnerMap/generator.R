source("utils/utils-metaprog.R")
library(data.table)
library(dplyr)

componentname="winnerMap"
componentarguments="input, output, session, parentsession,statename_reactive,dname,conmanager"
plotarea="mapPlot"
filteroutput="parentsession$output$ae_filter_selection"

filters = "filterid,construction,isinit
           1,selectInput(ns(\"wmI_year\"):\"Select Year\": c() : selectize = TRUE),T
           2,checkboxGroupInput(ns(\"wmparty_names\") : \"Select parties \": c()),F"
    
       #1, checkboxGroupInput(ns(\"party_names\") : \"Select seatshare for \": c())"
    
inputtable="filterid,name,type,alias
            1,statename_reactive(),string,selected_stname 
            2,input$wmI_year,string,selected_year
            3,input$wmparty_names,list,selected_parties"

##default can be allmultiple/allsingle/emptymultiple/emptysingle/NA.
outputtable="filterid,name,type,alias,default,label 
            1,wmI_year, updateSelectInput,yearlist,emptysingle,\"Select Year\"
            2,wmparty_names, updateCheckboxGroupInput,partynames,allmultiple,\"Select parties\"
            3,parentsession$output$mapPlot,renderLeaflet,leafletmap,NA,NA"



functiontable="filterid,funname
              1,getYears(state=\"selected_stname\" : years=\"yearlist\" : currentvalues)
              2,getPartyNames(state=\"selected_stname\" : year=\"selected_year\": parties=\"partynames\" : currentvalues)
              3,plotMap(state=\"selected_stname\" : year=\"selected_year\": parties=\"selected_parties\" : plot=\"leafletmap\" : currentvalues)"
##########################################################################


###############################################################################
l<-(commandArgs())
fname<-(gsub("--file=","",l[4]))
script.dir <-dirname(fname)

overallcode<-constructFunctions(inputtable,outputtable,functiontable,filteroutput,filters,script.dir,componentname,componentarguments,plotarea)
cat(overallcode,file=paste0(script.dir,"/",componentname,".R"))
