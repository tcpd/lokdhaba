source("utils/utils-metaprog.R")
library(data.table)
library(dplyr)

componentname="gewinnerMap"
componentarguments="input, output, session, parentsession,dname,conmanager"
plotarea=c("mapPlot","bookmark_edv","visDataDownload")
filteroutput="parentsession$output$ge_filter_selection"

filters = "filterid,construction,isinit
           1,selectInput(ns(\"I_year\"):\"Select Year\": c() : selectize = TRUE),T
           2,checkboxGroupInput(ns(\"filter_pname\") : \"Select parties \": c()),F"
    
       #1, checkboxGroupInput(ns(\"party_names\") : \"Select seatshare for \": c())"
    
inputtable="filterid,name,type,alias
            1,NA,NA,NA 
            2,input$I_year,string,selected_year
            3,input$I_year,string,selected_year
            3,input$filter_pname,list,selected_parties"

##default can be allmultiple/allsingle/emptymultiple/emptysingle/NA.
outputtable="filterid,name,type,alias,default,label 
            1,I_year, updateSelectInput,yearlist,emptysingle,\"Select Year\"
            2,filter_pname, updateCheckboxGroupInput,partynames,allmultiple,\"Select parties\"
            3,parentsession$output$mapPlot,renderLeaflet,leafletmap,NA,NA"



functiontable="filterid,funname
              1,getYears(years=\"yearlist\" : currentvalues)
              2,getPartyNames(year=\"selected_year\": parties=\"partynames\" : currentvalues)
              3,plotMap(year=\"selected_year\": parties=\"selected_parties\" : plot=\"leafletmap\" : currentvalues)"
##########################################################################


###############################################################################
l<-(commandArgs())
fname<-(gsub("--file=","",l[4]))
script.dir <-dirname(fname)

overallcode<-constructFunctions(inputtable,outputtable,functiontable,filteroutput,filters,script.dir,componentname,componentarguments,plotarea)
cat(overallcode,file=paste0(script.dir,"/",componentname,".R"))
