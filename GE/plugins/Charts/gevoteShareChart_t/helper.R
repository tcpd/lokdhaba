  ###################Specific for voteshare chart visualization##########################################
  getPartyNames<-function(parties,envr){
    #browser()
    b<-readtVoteShareFile("ge")
    
    assign(parties,as.list(unique(b$Party)),env=envr)
  }

  plotChart<-function( parties , plot,envr){
    selectedpartynames<-get(parties,envr)
    #sname<-gsub(" ","_",get(state,envr))
    b<-readtVoteShareFile("ge")
    #browser()
    
    #setting up variables for visualization data download
    dat <- subset(b,Party %in% selectedpartynames)
    dat$State_Name <- "LokSabha"
    conmanager$setval("visData",dat)
    conmanager$setval("selectedState","LokSabha")
    conmanager$setval("vis","PartyVoteShare(all_seats)")
    
    pivotdata<-dcast(b,Year~Party,value.var=c('Votes'))

    pal <- getPartyColor(b$Party)

    #create a base line chart with year as the x-axis
    base<-plot_ly(pivotdata, x = ~Year)
    #print(paste('selected',selectedpartynames))
    # #for each selected party in the input "filter_pname" id (checkbox) add a new trace
    # #corresponding to that party
    lapply(selectedpartynames,function(x) {print(paste('adding',x));base<<-add_trace(base,y=~get(x),type='scatter',mode='lines+markers',color=x,colors=pal)})
    #sname<-gsub("_"," ",sname)
    thistitle<-paste0('Party wise voteshare across years in GE')
    xtitle<-''
    ytitle<-'Vote share %'
    yrange<-c(0,100)
    assign(plot,preparechartlayout(base,thistitle,xtitle,ytitle,yrange),env=envr)
  }
  #################################################################################################
