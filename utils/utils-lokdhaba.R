
readShapeFile<- function(sname, year){
  dname<-paste0("datadir/AE/Maps/Delim4/",sname)
  lname<-paste0(sname,"_Assembly_con")
  print(dname)
  print(lname)
  shape<-readOGR(dsn=dname,layer=lname)
  return(shape)
}


addPopupInfo<- function(winnersframe){
  cand<-paste0("<b>Candidate:</b> ", winnersframe$cand1)
  assembly<-paste0("<b>Constituency :</b> ", winnersframe$ASSEMBLY_1)
  marginp<-paste0("<b>Margin Percentage :</b> ", paste0(winnersframe$margin_percent,"%"))
  winnersframe$popup<-paste(cand,assembly,marginp,sep="<br>")
  return(winnersframe)
}

#read ae_maps.csv file for this state tcpd_data/AE/Data/ + st + /derived/lokdhaba/ae_maps.csv

readStateWinnersFile<- function(statename){
  filename<-paste0("datadir/AE/Data/",statename,"/derived/lokdhaba/ae_maps.csv")
  print(paste0('reading from ',filename))
  m<-read.csv(filename)
  return(m)
}
############################################VoteShareMap######################################################3
voteShareMapLegendList<- function(){
  return(c("<10%","10%-20%","20%-30%","30%-40%",">40%"))
}

voteShareMapBreakupList<- function(){
  return(c(0,10,20,30,40,100))
}

VoteShareMapLegendColor<-function(inp){
  if(inp=="<10%"){
    return("green")
  }else if(inp=="10%-20%"){
    return("pink")
  }else if(inp=="20%-30%"){
    return("blue")
  }else if(inp=="30%-40%"){
    return("yellow")
  }else if(inp==">40%"){
    return("orange")
  }else{
    stop('passed argument should be either <10%, 10%-20%, 20%-30%,30%-40% or >40%')
  }
}

VoteShareMapLegendCount<-function(dframe){
  
  ##set a new column same as legend based on the percentage.
  dframe$tmp[dframe$vote_percent<10]<-"<10%"
  dframe$tmp[dframe$vote_percent>=10 & dframe$vote_percent<20]<-"10%-20%"
  dframe$tmp[dframe$vote_percent>=20 & dframe$vote_percent<30]<-"20%-30%"
  dframe$tmp[dframe$vote_percent>=30 & dframe$vote_percent<40]<-"30%-40%"
  dframe$tmp[dframe$vote_percent>=40]<-">40%"
  dframe$vote_percent<-NULL
  dframe$count<-1
  dframe<-aggregate(count~year+tmp,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$tmp),"(",dframe$count,")")
  dframe<-subset(dframe,select=c("tmp","legend"))
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(voteShareMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$tmp==x))==0){
      dframe<<-rbind(dframe,data.frame(tmp=x,legend=paste0(x,"(0)")))
    }
  }) 
  return(dframe)
  #we can't remove tmp column as it will be used in addLegend function of winnerVoteShareMap.R
}
#########################################Winner Gender Map##################################################
WinnerGenderMapLegendList<- function(){return (c("Male","Female","Others"))}


WinnerGenderMapLegendCount<-function(dframe){
  dframe$count<-1
  dframe<-aggregate(count~year+sex1,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$sex1),"(",dframe$count,")")
  dframe$count<-NULL
  dframe$year<-NULL
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(WinnerGenderMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$sex1==x))==0){
      dframe<<-rbind(dframe,data.frame(sex1=x,legend=paste0(x,"(0)")))
    }
  }) 
  return(dframe)
}

##########################################Winner Margin Map#################################################
WinnerMarginMapLegendList <- function(){return (c("<5%","5%-10%","10%-20%",">20%"))}


WinnerMarginMapBreakupList<- function(){
  return(c(0,5,10,20,100))
}

WinnerMarginMapLegendCount<- function(dframe){
  ##set a new column same as legend based on the percentage.
  dframe$tmp[dframe$margin_percent<5]<-"<5%"
  dframe$tmp[dframe$margin_percent>=5 & dframe$margin_percent<10]<-"5%-10%"
  dframe$tmp[dframe$margin_percent>=10 & dframe$margin_percent<20]<-"10%-20%"
  dframe$tmp[dframe$margin_percent>=20]<-">20%"
  #browser()
  dframe$margin_percent<-NULL
  dframe$count<-1
  #browser()
  dframe<-aggregate(count~tmp,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$tmp),"(",dframe$count,")")
  dframe<-subset(dframe,select=c("tmp","legend"))
  #browser()
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(WinnerMarginMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$tmp==x))==0){
      dframe<<-rbind(dframe,data.frame(tmp=x,legend=paste0(x,"(0)")))
    }
  }) 
  return(dframe)

}

WinnerMarginMapLegendColor<-function(inp){
  if(inp=="<5%"){
    return("green")
  }else if(inp=="5%-10%"){
    return("pink")
  }else if(inp=="10%-20%"){
    return("blue")
  }else if(inp==">20%"){
    return("orange")
  }else{
    stop('passed argument should be either <5%, 5%-10%, 10%-20% or >20%')
  }
}

#############################Constituency Wise total candidates#############################################################
NumCandidatesMapLegendList <- function(){return (c("<5","5-15",">15"))}


NumCandidatesMapBreakupList<- function(){
  return(c(0,5,15,100))
}

NumCandidatesMapLegendCount<- function(dframe){
  ##set a new column same as legend based on the percentage.
  dframe$tmp[dframe$n_cand<5]<-"<5"
  dframe$tmp[dframe$n_cand>=5 & dframe$n_cand<15]<-"5-15"
  dframe$tmp[dframe$n_cand>=15]<-">15"
  dframe$n_cand<-NULL
  dframe$count<-1
  #browser()
  dframe<-aggregate(count~tmp,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$tmp),"(",dframe$count,")")
  dframe<-subset(dframe,select=c("tmp","legend"))
  #browser()
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(NumCandidatesMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$tmp==x))==0){
      dframe<<-rbind(dframe,data.frame(tmp=x,legend=paste0(x,"(0)")))
    }
  }) 
  
  return(dframe)
  
}

NumCandidatesMapLegendColor<-function(inp){
  if(inp=="<5"){
    return("green")
  }else if(inp=="5-15"){
    return("pink")
  }else if(inp==">15"){
    return("blue")
  }else{
    stop('passed argument should be either <5, 5-15, or >15')
  }
}
