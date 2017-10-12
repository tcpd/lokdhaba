  
  ###################Specific for this visualization##########################################
  getPartyNames<-function(parties,envr){
    #browser()
    b<-readSeatShareFile("ge")
    assign(parties,as.vector(unique(b$Party)),env=envr)
  }

  plotChart<-function( parties , plot,envr){
    selectedpartynames<-get(parties,envr)
    b<-readSeatShareFile("ge")
    pivotdata<-dcast(b,Year~Party,value.var=c('Seats'))
    #create a base line chart with year as the x-axis
    base<-plot_ly(pivotdata, x = ~Year)
    #print(paste('selected',selectedpartynames))
    # #for each selected party in the input "filter_pname" id (checkbox) add a new trace
    # #corresponding to that party
    lapply(selectedpartynames,function(x) {print(paste('adding',x));base<<-add_trace(base,y=~get(x),name=x,type='scatter',mode='lines+markers')})
    thistitle<-paste0('Party wise seatshare across years in LokSabha')
    xtitle<-''
    ytitle<-'Seat share %'
    yrange<-c(0,100)
    assign(plot,preparechartlayout(base,thistitle,xtitle,ytitle,yrange),env=envr)
  }
  #################################################################################################
