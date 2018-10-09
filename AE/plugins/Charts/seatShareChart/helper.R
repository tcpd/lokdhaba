  
  ###################Specific for this visualization##########################################
  getPartyNames<-function(state,parties,envr){
    #browser()
    sname<-gsub(" ","_",get(state,envr))
    b<-readSeatShareFile(sname)
    assign(parties,as.vector(unique(b$Party)),env=envr)
  }

  plotChart<-function(state, parties , plot,envr){
    selectedpartynames<-get(parties,envr)
    sname<-gsub(" ","_",get(state,envr))
    b<-readSeatShareFile(sname)
    #setting up variables for visualization data download
    dat <- subset(b,Party %in% selectedpartynames)
    conmanager$setval("visData",dat)
    conmanager$setval("selectedState",sname)
    conmanager$setval("vis","PartySeatShare")
    
    
    pivotdata<-dcast(b,Year~Party,value.var=c('Seats'))
    pal <- getPartyColor(b$Party)
    #create a base line chart with year as the x-axis
    base<-plot_ly(pivotdata, x = ~Year)
    #print(paste('selected',selectedpartynames))
    # #for each selected party in the input "filter_pname" id (checkbox) add a new trace
    # #corresponding to that party
    #lapply(selectedpartynames,function(x) {print(paste('adding',x));base<<-add_trace(base,y=~get(x),name=x,type='scatter',mode='lines+markers')})

    lapply(selectedpartynames,function(x) {print(paste('adding',x));base<<-add_trace(base,y=~get(x),type='scatter',mode='lines+markers',color=x,colors=pal)})
    sname<-gsub("_"," ",sname)
    thistitle<-paste0('Party wise seatshare across years in ',sname)
    xtitle<-''
    ytitle<-'Seat share %'
    yrange<-c(0,100)
    assign(plot,preparechartlayout(base,thistitle,xtitle,ytitle,yrange),env=envr)
  }
  #################################################################################################
