source("utils/utils-metaprog.R")
library(data.table)
library(dplyr)

componentname="voterTurnoutChart"
componentarguments="input, output, session, parentsession,statename_reactive,dname,conmanager"
plotarea=c("distPlot","bookmark_edv","visDataDownload")
filteroutput="parentsession$output$ae_filter_selection"
#plotarea="distPlot"

filters = "filterid,construction,isinit 
           1,checkboxGroupInput(ns(\"gender_names\") : \"Select turnout for \": c()),T"
    
       #1, checkboxGroupInput(ns(\"party_names\") : \"Select seatshare for \": c())"
    
inputtable="filterid,name,type,alias
            1,statename_reactive(),string,selected_stname
            2,statename_reactive(),string,selected_stname 
            2,input$gender_names,list,selected_genders"

##default can be allmultiple/allsingle/emptymultiple/emptysingle/NA.
outputtable="filterid,name,type,alias,default,label 
            1,gender_names,updateCheckboxGroupInput,gendernames,allmultiple,\"Select turnout for\"
            2,parentsession$output$distPlot,renderPlotly,plotlychart,NA,NA"



functiontable="filterid,funname 
              1,getGenderNames(genders=\"gendernames\" : currentvalues)
              2,plotChart(state=\"selected_stname\" : gendernames=\"selected_genders\" : plot=\"plotlychart\" : currentvalues)"
##########################################################################


###############################################################################
l<-(commandArgs())
fname<-(gsub("--file=","",l[4]))
script.dir <-dirname(fname)

overallcode<-constructFunctions(inputtable,outputtable,functiontable,filteroutput,filters,script.dir,componentname,componentarguments,plotarea)
cat(overallcode,file=paste0(script.dir,"/",componentname,".R"))
