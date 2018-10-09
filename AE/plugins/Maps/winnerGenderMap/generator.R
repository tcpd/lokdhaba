source("utils/utils-metaprog.R")
library(data.table)
library(dplyr)

componentname="winnerGenderMap"
componentarguments="input, output, session, parentsession,statename_reactive,dname,conmanager"
plotarea=c("mapPlot","bookmark_edv","visDataDownload")
filteroutput="parentsession$output$ae_filter_selection"

filters = "filterid,construction,isinit
           1,selectInput(ns(\"wgenderI_year\"):\"Select Year\": c() : selectize = TRUE),T
           2,checkboxGroupInput(ns(\"wgender_names\") : \"Select gender type \": c()),F"
    
inputtable="filterid,name,type,alias
            1,statename_reactive(),string,selected_stname 
            2,input$wgenderI_year,string,selected_year
            3,input$wgenderI_year,string,selected_year
            3,input$wgender_names,list,selected_options"

##default can be allmultiple/allsingle/emptymultiple/emptysingle/NA.
outputtable="filterid,name,type,alias,default,label 
            1,wgenderI_year, updateSelectInput,yearlist,emptysingle,\"Select Year\"
            2,wgender_names, updateCheckboxGroupInput,options,allmultiple,\"Select gender type\"
            3,parentsession$output$mapPlot,renderLeaflet,leafletmap,NA,NA"



functiontable="filterid,funname
              1,getYears(state=\"selected_stname\" : years=\"yearlist\" : currentvalues)
              2,getOptions(state=\"selected_stname\" : year=\"selected_year\": options=\"options\" : currentvalues)
              3,plotMap(state=\"selected_stname\" : year=\"selected_year\": options=\"selected_options\" : plot=\"leafletmap\" : currentvalues)"
##########################################################################


###############################################################################
l<-(commandArgs())
fname<-(gsub("--file=","",l[4]))
script.dir <-dirname(fname)

overallcode<-constructFunctions(inputtable,outputtable,functiontable,filteroutput,filters,script.dir,componentname,componentarguments,plotarea)
cat(overallcode,file=paste0(script.dir,"/",componentname,".R"))
