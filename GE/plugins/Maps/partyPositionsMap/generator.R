source("utils/utils-metaprog.R")
library(data.table)
library(dplyr)

componentname="partyPositionsMap"
componentarguments="input, output, session, parentsession,dname,conmanager"
plotarea="mapPlot"
filteroutput="parentsession$output$ge_filter_selection"
#plotarea="distPlot"

filters = "filterid,construction,isinit
           1,selectInput(ns(\"gepartyposI_year\") :  \"Select Year\": c() : selectize = TRUE),T
           2,selectInput(ns(\"gepartyposI_party\") : \"Select Party\" : c() : selectize = TRUE),F
           3,checkboxGroupInput(ns(\"gepartyposoptions\") : \"Select positions \": c()),F"
    
       #1, checkboxGroupInput(ns(\"party_names\") : \"Select seatshare for \": c())"
    
inputtable="filterid,name,type,alias
            1,NA,NA,NA 
            2,input$gepartyposI_year,string,selected_year
            3,input$gepartyposI_party,string,selected_party
            4,input$gepartyposI_party,string,selected_party
            4,input$gepartyposoptions,list,selected_options"

##default can be allmultiple/allsingle/emptymultiple/emptysingle/NA.
outputtable="filterid,name,type,alias,default,label 
            1,gepartyposI_year, updateSelectInput,yearlist,emptysingle,\"Select Year\"
            2,gepartyposI_party,updateSelectInput,partynames,emptysingle,\"Select Party\"
            3,gepartyposoptions, updateCheckboxGroupInput,optionlist,allmultiple,\"Select positions\"
            4,parentsession$output$mapPlot,renderLeaflet,leafletmap,NA,NA"



functiontable="filterid,funname
              1,getYears(years=\"yearlist\" : currentvalues)
              2,getPartyNames( year=\"selected_year\": parties=\"partynames\" : currentvalues)
              3,getOptions( year=\"selected_year\" : party=\"selected_party\" : options=\"optionlist\" : currentvalues)
              4,plotMap(year=\"selected_year\": party=\"selected_party\" : options=\"selected_options\" : plot=\"leafletmap\" : currentvalues)"
##########################################################################


###############################################################################
l<-(commandArgs())
fname<-(gsub("--file=","",l[4]))
script.dir <-dirname(fname)

overallcode<-constructFunctions(inputtable,outputtable,functiontable,filteroutput,filters,script.dir,componentname,componentarguments,plotarea)
cat(overallcode,file=paste0(script.dir,"/",componentname,".R"))
