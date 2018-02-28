source("utils/utils-metaprog.R")
library(data.table)
library(dplyr)

componentname="partyVoteShareMap"
componentarguments="input, output, session, parentsession,statename_reactive,dname,conmanager"
plotarea="mapPlot"
filteroutput="parentsession$output$ae_filter_selection"
#plotarea="distPlot"

filters = "filterid,construction,isinit
           1,selectInput(ns(\"pvsI_year\") :  \"Select Year\": c() : selectize = TRUE),T
	   2,selectInput(ns(\"pvsI_party\") : \"Select Party\" : c() : selectize = TRUE),F
           3,checkboxGroupInput(ns(\"pvsoptions\") : \"Select voteshare range \": c()),F"
    
    
inputtable="filterid,name,type,alias
            1,statename_reactive(),string,selected_stname 
            2,input$pvsI_year,string,selected_year
            3,input$pvsI_party,string,selected_party
	    4,input$pvsI_party,string,selected_party
            4,input$pvsoptions,list,selected_options"

##default can be allmultiple/allsingle/emptymultiple/emptysingle/NA.
outputtable="filterid,name,type,alias,default,label 
            1,pvsI_year, updateSelectInput,yearlist,emptysingle,\"Select Year\"
            2,pvsI_party,updateSelectInput,partynames,emptysingle,\"Select Party\"
            3,pvsoptions, updateCheckboxGroupInput,optionlist,allmultiple,\"Select voteshare range\"
            4,parentsession$output$mapPlot,renderLeaflet,leafletmap,NA,NA"



functiontable="filterid,funname
              1,getYears(state=\"selected_stname\" : years=\"yearlist\" : currentvalues)
	      2,getPartyNames(state=\"selected_stname\":year=\"selected_year\" : parties=\"partynames\":currentvalues)	 
              3,getOptions(state=\"selected_stname\" : year=\"selected_year\":party=\"selected_party\" : options=\"optionlist\" : currentvalues)
              4,plotMap(state=\"selected_stname\" : year=\"selected_year\":party=\"selected_party\" : options=\"selected_options\" : plot=\"leafletmap\" : currentvalues)"
##########################################################################


###############################################################################
l<-(commandArgs())
fname<-(gsub("--file=","",l[4]))
script.dir <-dirname(fname)

overallcode<-constructFunctions(inputtable,outputtable,functiontable,filteroutput,filters,script.dir,componentname,componentarguments,plotarea)
cat(overallcode,file=paste0(script.dir,"/",componentname,".R"))
