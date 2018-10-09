source("utils/utils-metaprog.R")
library(data.table)
library(dplyr)

componentname="winnerMarginMap"
componentarguments="input, output, session, parentsession,statename_reactive,dname,conmanager"
plotarea=c("mapPlot","bookmark_edv","visDataDownload")
filteroutput="parentsession$output$ae_filter_selection"

filters = "filterid,construction,isinit
           1,selectInput(ns(\"wmarginI_year\"):\"Select Year\": c() : selectize = TRUE),T
           2,checkboxGroupInput(ns(\"wmargin_names\") : \"Select margins \": c()),F"
    
inputtable="filterid,name,type,alias
            1,statename_reactive(),string,selected_stname 
            2,input$wmarginI_year,string,selected_year
            3,input$wmarginI_year,string,selected_year
            3,input$wmargin_names,list,selected_margins"

##default can be allmultiple/allsingle/emptymultiple/emptysingle/NA.
outputtable="filterid,name,type,alias,default,label 
            1,wmarginI_year, updateSelectInput,yearlist,emptysingle,\"Select Year\"
            2,wmargin_names, updateCheckboxGroupInput,marginoptions,allmultiple,\"Select margins\"
            3,parentsession$output$mapPlot,renderLeaflet,leafletmap,NA,NA"



functiontable="filterid,funname
              1,getYears(state=\"selected_stname\" : years=\"yearlist\" : currentvalues)
              2,getMarginOptions(state=\"selected_stname\" : year=\"selected_year\": margins=\"marginoptions\" : currentvalues)
              3,plotMap(state=\"selected_stname\" : year=\"selected_year\": margins=\"selected_margins\" : plot=\"leafletmap\" : currentvalues)"
##########################################################################


###############################################################################
l<-(commandArgs())
fname<-(gsub("--file=","",l[4]))
script.dir <-dirname(fname)

overallcode<-constructFunctions(inputtable,outputtable,functiontable,filteroutput,filters,script.dir,componentname,componentarguments,plotarea)
cat(overallcode,file=paste0(script.dir,"/",componentname,".R"))
