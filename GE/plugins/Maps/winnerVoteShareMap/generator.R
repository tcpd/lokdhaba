source("utils/utils-metaprog.R")
library(data.table)
library(dplyr)

componentname="winnerVoteShareMap"
componentarguments="input, output, session, parentsession,dname,conmanager"
plotarea="mapPlot"
filteroutput="parentsession$output$ge_filter_selection"
#plotarea="distPlot"

filters = "filterid,construction,isinit
           1,selectInput(ns(\"I_year\") :  \"Select Year\": c() : selectize = TRUE),T
           2,checkboxGroupInput(ns(\"options\") : \"Select voteshare range \": c()),F"
    
    
inputtable="filterid,name,type,alias
            1,NA,NA,NA 
            2,input$I_year,string,selected_year
            3,input$I_year,string,selected_year
            3,input$options,list,selected_options"

##default can be allmultiple/allsingle/emptymultiple/emptysingle/NA.
outputtable="filterid,name,type,alias,default,label 
            1,I_year, updateSelectInput,yearlist,emptysingle,\"Select Year\"
            2,options, updateCheckboxGroupInput,optionlist,allmultiple,\"Select voteshare range\"
            3,parentsession$output$mapPlot,renderLeaflet,leafletmap,NA,NA"



functiontable="filterid,funname
              1,getYears(years=\"yearlist\" : currentvalues)
              2,getOptions(year=\"selected_year\" : options=\"optionlist\" : currentvalues)
              3,plotMap(year=\"selected_year\" : options=\"selected_options\" : plot=\"leafletmap\" : currentvalues)"
##########################################################################


###############################################################################
l<-(commandArgs())
fname<-(gsub("--file=","",l[4]))
script.dir <-dirname(fname)

overallcode<-constructFunctions(inputtable,outputtable,functiontable,filteroutput,filters,script.dir,componentname,componentarguments,plotarea)
cat(overallcode,file=paste0(script.dir,"/",componentname,".R"))
