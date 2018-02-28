source("utils/utils-metaprog.R")
library(data.table)
library(dplyr)

componentname="partyVoteShareMap"
componentarguments="input, output, session, parentsession,dname,conmanager"
plotarea="mapPlot"
filteroutput="parentsession$output$ge_filter_selection"
#plotarea="distPlot"

filters = "filterid,construction,isinit
           1,selectInput(ns(\"I_year\") :  \"Select Year\": c() : selectize = TRUE),T
	   2,selectInput(ns(\"I_Party\") : \"Select Party\" : c(): selectize =TRUE),F
           3,checkboxGroupInput(ns(\"options\") : \"Select voteshare range \": c()),F"
    
    
inputtable="filterid,name,type,alias
            1,NA,NA,NA 
            2,input$I_year,string,selected_year
	    3,input$I_Party,string,selected_party
            4,input$I_Party,string,selected_party
	    4,input$options,list,selected_options"

##default can be allmultiple/allsingle/emptymultiple/emptysingle/NA.
outputtable="filterid,name,type,alias,default,label 
            1,I_year, updateSelectInput,yearlist,emptysingle,\"Select Year\"
            2,I_Party,updateSelectInput,partynames,emptysingle,\"Select Party\"
            3,options, updateCheckboxGroupInput,optionlist,allmultiple,\"Select voteshare range\"
            4,parentsession$output$mapPlot,renderLeaflet,leafletmap,NA,NA"



functiontable="filterid,funname
              1,getYears(years=\"yearlist\" : currentvalues)
	      2,getPartyNames(year=\"selected_year\":parties=\"partynames\":currentvalues)
              3,getOptions(year=\"selected_year\":party=\"selected_party\" : options=\"optionlist\" : currentvalues)
              4,plotMap(year=\"selected_year\": party=\"selected_party\" : options=\"selected_options\" : plot=\"leafletmap\" : currentvalues)"
##########################################################################


###############################################################################
l<-(commandArgs())
fname<-(gsub("--file=","",l[4]))
script.dir <-dirname(fname)

overallcode<-constructFunctions(inputtable,outputtable,functiontable,filteroutput,filters,script.dir,componentname,componentarguments,plotarea)
cat(overallcode,file=paste0(script.dir,"/",componentname,".R"))
