source("utils/utils-metaprog.R")
library(data.table)
library(dplyr)

componentname="gecontestedDepositSavedChart"
componentarguments="input, output, session, parentsession,dname,conmanager"
plotarea=c("distPlot","bookmark_edv","visDataDownload")
filteroutput="parentsession$output$ge_filter_selection"
#plotarea="distPlot"

filters = "filterid,construction,isinit
           1,checkboxGroupInput(ns(\"filter_pname\") : \"Select  \": c()),T"
    
       #1, checkboxGroupInput(ns(\"party_names\") : \"Select seatshare for \": c())"
    
inputtable="filterid,name,type,alias
            1,NA,NA,NA 
            2,input$filter_pname,list,selected_options"

##default can be allmultiple/allsingle/emptymultiple/emptysingle/NA.
outputtable="filterid,name,type,alias,default,label 
            1,filter_pname,updateCheckboxGroupInput,optionnames,allmultiple,\"Select \"
            2,parentsession$output$distPlot,renderPlotly,plotlychart,NA,NA"



functiontable="filterid,funname 
              1,getOptions(options=\"optionnames\" : currentvalues)
              2,plotChart(options=\"selected_options\" : plot=\"plotlychart\" : currentvalues)"
##########################################################################


###############################################################################
l<-(commandArgs())
fname<-(gsub("--file=","",l[4]))
script.dir <-dirname(fname)

overallcode<-constructFunctions(inputtable,outputtable,functiontable,filteroutput,filters,script.dir,componentname,componentarguments,plotarea)
cat(overallcode,file=paste0(script.dir,"/",componentname,".R"))
