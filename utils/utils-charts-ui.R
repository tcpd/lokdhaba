preparechartlayout<-function(base,title,xtitle,ytitle,yrange){
  
  subtitle<-'Source: Adapted from <a href="www.eci.nic.in">ECI Data</a><br><a href="www.tcpd.ashoka.edu.in">Trivedi Centre for Political Data, Ashoka University</a>'                          
 
  xtitle<-'Year (Assembly number)'
  base %>%
    layout(title = title, #xanchor="center",
         font=list(family= 'Droid Serif, serif',size=16,color=rgb(.2,.2,.2)),
         xaxis = list(title=xtitle,
           tickangle=45, type="category",categoryorder="category ascending",titlefont=list(
             family='Droid Serif, serif',
             size=16
           ),zeroline=T,showline=T),
         yaxis = list (title = ytitle, titlefont=list(
           family='Droid Serif, serif',
           size=16
         ),zeroline=T,showline=T,range=yrange),
         annotations=list(text=subtitle,
                          showarrow=FALSE,
                          x=0.45,y=1.05,
                          xref='paper',
                          yref='paper',
                          font=list(
                            family='Droid Serif, serif',
                            size=10
                          )
         ),
         margin=list(
           l=60,
           t=100,
           b=100,
           pad=1
         ))
}


readVoteSharePhaseFile<-function(statename,year){
    filename<-paste0("../tcpd_data/data/AE/Data/",statename,"/",year,"/derived/lokdhaba/ae_voteshares_phase.csv")
    m<-read.csv(filename)
    #m<-subset(m,m$year>=2012,select=c("state","year","party","votes"))
    return(m)

}

#read ae_voteshares.csv file for this state tcpd_data/AE/Data/ + st + /derived/lokdhaba/ae_voteshares.csv

readVoteShareFile<-function(statename){
  if(statename=="ge"){
    filename<-paste0("../tcpd_data/data/GE/Data/derived/lokdhaba/ge_voteshares.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    m$newyear<-paste0(m$year," (#",m$ga_no,")")
    m$year<-NULL
    names(m)[names(m)=="newyear"]<-"year"
    
    return(m)
  }else{
    filename<-paste0("../tcpd_data/data/AE/Data/",statename,"/derived/lokdhaba/ae_voteshares.csv")
    print(paste0('reading from ',filename))
    
    m<-read.csv(filename)
    m<-subset(m,select=c("state","year","sa_no","party","votes"))
    m$newyear<-paste0(m$year," (#",m$sa_no,")")
    m$year<-NULL
    names(m)[names(m)=="newyear"]<-"year"
    return(m)
  }
}

#read ae_seatshares.csv file for this state tcpd_data/AE/Data/ + st + /derived/lokdhaba/ae_seatshares.csv

readSeatShareFile<-function(statename){
  if(statename=="ge"){
    filename<-paste0("../tcpd_data/data/GE/Data/derived/lokdhaba/ge_seatshares.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    m$newyear<-paste0(m$year," (#",m$ga_no,")")
    m$year<-NULL
    names(m)[names(m)=="newyear"]<-"year"
    
    return(m)
  }else{
    filename<-paste0("../tcpd_data/data/AE/Data/",statename,"/derived/lokdhaba/ae_seatshares.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    m<-subset(m,select=c("state","year","sa_no","party","seats"))
    m$newyear<-paste0(m$year," (#",m$sa_no,")")
    m$year<-NULL
    names(m)[names(m)=="newyear"]<-"year"
    
    return(m)
  }
}

#read ae_voter_turnouts.csv
readVoterTurnoutFile<-function(statename){
  if(statename=="ge"){
    filename<-paste0("../tcpd_data/data/GE/ge_voter_turnouts.csv")
    print(paste0('reading from ',filename))
    m$newyear<-paste0(m$year," (#",m$ga_no,")")
    m$year<-NULL
    names(m)[names(m)=="newyear"]<-"year"
    
      m<-read.csv(filename)
    return(m)
  }else{
    filename<-paste0("../tcpd_data/data/AE/ae_voter_turnouts.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename) %>% filter(state==statename)
    m$newyear<-paste0(m$year," (#",m$sa_no,")")
    m$year<-NULL
    names(m)[names(m)=="newyear"]<-"year"
    
    return(m)
  }
}

##read ae_parties_contested.csv file for this state tcpd_data/AE/Data/ + st + /derived/lokdhaba/ae_parties_contested.csv

readPartiesContestedRepresentedFile<-function(statename){
  if(statename=="ge"){
    filename<-paste0("../tcpd_data/data/GE/Data/derived/lokdhaba/ge_parties_contests.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    m$newyear<-paste0(m$year," (#",m$ga_no,")")
    m$year<-NULL
    names(m)[names(m)=="newyear"]<-"year"
    
    return(m)
  }else{
    filename<-paste0("../tcpd_data/data/AE/Data/",statename,"/derived/lokdhaba/ae_parties_contests.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    m$newyear<-paste0(m$year," (#",m$sa_no,")")
    m$year<-NULL
    names(m)[names(m)=="newyear"]<-"year"
    
    return(m)
  }
}


 
##read ae_contested_deposit_losts.csv file for this state tcpd_data/AE/Data/ + st + /derived/lokdhaba/ae_contested_deposit_losts.csv

readCandidatesContestedDepositLostFile<-function(statename){
  if(statename=="ge"){
    filename<-paste0("../tcpd_data/data/GE/Data/derived/lokdhaba/ge_contested_deposit_losts.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    m$newyear<-paste0(m$year," (#",m$ga_no,")")
    m$year<-NULL
    names(m)[names(m)=="newyear"]<-"year"
    
    return(m)
  }else{
    filename<-paste0("../tcpd_data/data/AE/Data/",statename,"/derived/lokdhaba/ae_contested_deposit_losts.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    m$newyear<-paste0(m$year," (#",m$sa_no,")")
    m$year<-NULL
    names(m)[names(m)=="newyear"]<-"year"
    
    return(m)
  }
}
