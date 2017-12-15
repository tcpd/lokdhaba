source("utils/utils-metaprog.R")
library(data.table)
library(dplyr)

componentname="winnerVoteShareMap"
componentarguments="input, output, session, parentsession,statename_reactive,dname,conmanager"
plotarea="mapPlot"
filteroutput="parentsession$output$ae_filter_selection"
#plotarea="distPlot"

filters = "filterid,construction,isinit
           1,selectInput(ns(\"wvmI_year\") :  \"Select Year\": c() : selectize = TRUE),T
           2,checkboxGroupInput(ns(\"wvmoptions\") : \"Select voteshare range \": c()),F"
    
    
inputtable="filterid,name,type,alias
            1,statename_reactive(),string,selected_stname 
            2,input$wvmI_year,string,selected_year
            3,input$wvmI_year,string,selected_year
            3,input$wvmoptions,list,selected_options"

##default can be allmultiple/allsingle/emptymultiple/emptysingle/NA.
outputtable="filterid,name,type,alias,default,label 
            1,wvmI_year, updateSelectInput,yearlist,emptysingle,\"Select Year\"
            2,wvmoptions, updateCheckboxGroupInput,optionlist,allmultiple,\"Select voteshare range\"
            3,parentsession$output$mapPlot,renderLeaflet,leafletmap,NA,NA"



functiontable="filterid,funname
              1,getYears(state=\"selected_stname\" : years=\"yearlist\" : currentvalues)
              2,getOptions(state=\"selected_stname\" : year=\"selected_year\" : options=\"optionlist\" : currentvalues)
              3,plotMap(state=\"selected_stname\" : year=\"selected_year\" : options=\"selected_options\" : plot=\"leafletmap\" : currentvalues)"
##########################################################################


###############################################################################
l<-(commandArgs())
fname<-(gsub("--file=","",l[4]))
script.dir <-dirname(fname)

overallcode<-constructFunctions(inputtable,outputtable,functiontable,filteroutput,filters,script.dir,componentname,componentarguments,plotarea)
cat(overallcode,file=paste0(script.dir,"/",componentname,".R"))
