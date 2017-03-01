#right now no selection of shape file based on year.. but later we need to do it appropriately
readShapeFile<- function(sname, year){
  if(sname=="ge"){
    dname<-paste0("../tcpd_data/data/GE/Maps/Delim4/")
    lname<-paste0("LOKSABHA_15_Modified")
    shape<-readOGR(dsn=dname,layer=lname)
    return(shape)  
  }else{
    dname<-paste0("../tcpd_data/data/AE/Maps/Delim4/",sname)
    lname<-paste0(sname,"_Assembly_con")
    shape<-readOGR(dsn=dname,layer=lname)
    return(shape)
  }
}



addPopupInfo<- function(winnersframe,type="state"){
  cand<-paste0("<b>Candidate:</b> ", winnersframe$cand1)
  marginp<-paste0("<b>Margin Percentage :</b> ", paste0(winnersframe$margin_percent,"%"))
  numcand<-paste0("<b>Total Candidates :</b> ", paste0(winnersframe$n_cand))
  party<-paste0("<b>Party :</b> ", paste0(winnersframe$party1))
  
  if(type=="state"){
    assembly<-paste0("<b>Constituency :</b> ", winnersframe$ASSEMBLY_1)
  }else{
    assembly<-paste0("<b>Constituency :</b> ", winnersframe$pc_name)
  }
  winnersframe$popup<-paste(cand,assembly,party,numcand,marginp,sep="<br>")
  return(winnersframe)
}

addTitleLeaflet<-function(map,titlemessage){
 addControl(map,html=paste0(titlemessage,"<br>",
                                    "<p class=\"leaflet-tcpd\">Source: Adapted from <a href=&quot;www.eci.nic.in&quot;>ECI Data</a><br>",
                                     "<a href=&quot;www.tcpd.ashoka.edu.in&quot;>Trivedi Centre for Political Data, Ashoka University</a></p>"),className="leaflettitle",position="topleft")

}
#read ae_maps.csv file for this state tcpd_data/AE/Data/ + st + /derived/lokdhaba/ae_maps.csv

readStateWinnersFile<- function(statename){
  if(statename=="ge"){
    filename<-paste0("../tcpd_data/data/GE/Data/derived/lokdhaba/ge_maps.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    return(m) 
  }else{
    filename<-paste0("../tcpd_data/data/AE/Data/",statename,"/derived/lokdhaba/ae_maps.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    return(m)
  }
}


#read ae_partys.csv file for this state tcpd_data/AE/Data/ + st + /derived/lokdhaba/ae_maps.csv

readPartyPositionsFile<- function(statename){
  if(statename=="ge"){
    filename<-paste0("../tcpd_data/data/GE/Data/derived/lokdhaba/ge_partys.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    return(m)
  }else{
    filename<-paste0("../tcpd_data/data/AE/Data/",statename,"/derived/lokdhaba/ae_partys.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    return(m)
  }
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
    return('#ff3739')
  }else if(inp=="10%-20%"){
    return('#d62728')
  }else if(inp=="20%-30%"){
    return('#ad1717')
  }else if(inp=="30%-40%"){
    return('#840706')
  }else if(inp==">40%"){
    return('#5b0000')
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

WinnerGenderMapLegendColor<-function(inp){
  if(inp=="Male"){
    return('#1f77b4')
  }else if(inp=="Female"){
    return('#8c564b')
  }else if(inp=="Others"){
    return('#433e66')
  }else{
    stop('passed argument should be either Male, Female or Others')
  }
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
    return('#a798fc')
  }else if(inp=="5%-10%"){
    return('#756bb1')
  }else if(inp=="10%-20%"){
    return('#433e66')
  }else if(inp==">20%"){
    return('#000000')
  }else{
    stop('passed argument should be either <5%, 5%-10%, 10%-20% or >20%')
  }
}

#############################Constituency Wise total candidates#############################################################
NumCandidatesMapLegendList <- function(){return (c("<5","5-15",">15"))}


NumCandidatesMapBreakupList<- function(){
  return(c(0,5,15,1000))
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
    return('#bc5533')
  }else if(inp=="5-15"){
    return('#843c41')
  }else if(inp==">15"){
    return('#140109')
  }else{
    stop('passed argument should be either <5, 5-15, or >15')
  }
}

##########################################Nota Turnout Map#################################################
NotaTurnoutMapLegendList <- function(){return (c("<1%","1%-3%","3%-5%",">5%"))}


NotaTurnoutMapBreakupList<- function(){
  return(c(0,1,3,5,100))
}

NotaTurnoutMapLegendCount<- function(dframe){
  ##set a new column same as legend based on the percentage.
  dframe$tmp[dframe$nota_percent<1]<-"<1%"
  dframe$tmp[dframe$nota_percent>=1 & dframe$nota_percent<3]<-"1%-3%"
  dframe$tmp[dframe$nota_percent>=3 & dframe$nota_percent<5]<-"3%-5%"
  dframe$tmp[dframe$nota_percent>=5]<-">5%"
  #browser()
  dframe$nota_percent<-NULL
  dframe$count<-1
  #browser()
  dframe<-aggregate(count~tmp,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$tmp),"(",dframe$count,")")
  dframe<-subset(dframe,select=c("tmp","legend"))
  #browser()
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(NotaTurnoutMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$tmp==x))==0){
      dframe<<-rbind(dframe,data.frame(tmp=x,legend=paste0(x,"(0)")))
    }
  }) 
  return(dframe)
  
}

NotaTurnoutMapLegendColor<-function(inp){
  if(inp=="<1%"){
    return('#a798fc')
  }else if(inp=="1%-3%"){
    return('#756bb1')
  }else if(inp=="3%-5%"){
    return('#11111b')
  }else if(inp==">5%"){
    return('#000000')
  }else{
    stop('passed argument should be either <1%, 1%-3%, 3%-5% or >5%')
  }
}


##########################################Voter Turnout Map#################################################
VoterTurnoutMapLegendList <- function(){return (c("<40%","40%-50%","50%-60%","60%-70%",">70%"))}


VoterTurnoutMapBreakupList<- function(){
  return(c(0,40,50,60,70,100))
}

VoterTurnoutMapLegendCount<- function(dframe){
  ##set a new column same as legend based on the percentage.
  dframe$tmp[dframe$turnout<40]<-"<40%"
  dframe$tmp[dframe$turnout>=40 & dframe$turnout<50]<-"40%-50%"
  dframe$tmp[dframe$turnout>=50 & dframe$turnout<60]<-"50%-60%"
  dframe$tmp[dframe$turnout>=60 & dframe$turnout<70]<-"60%-70%"
  dframe$tmp[dframe$turnout>=70]<-">70%"
  #browser()
  dframe$turnout<-NULL
  dframe$count<-1
  #browser()
  dframe<-aggregate(count~tmp,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$tmp),"(",dframe$count,")")
  dframe<-subset(dframe,select=c("tmp","legend"))
  #browser()
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(VoterTurnoutMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$tmp==x))==0){
      dframe<<-rbind(dframe,data.frame(tmp=x,legend=paste0(x,"(0)")))
    }
  }) 
  return(dframe)
  
}

VoterTurnoutMapLegendColor<-function(inp){
  if(inp=="<40%"){
    return('#f1ffff')
  }else if(inp=="40%-50%"){
    return('#8d9bbe')
  }else if(inp=="50%-60%"){
    return('#757fa9')
  }else if(inp=="60%-70%"){
    return('#5c6295')
  }else if(inp==">70%"){
    return('#434681')
  }else{
    stop('passed argument should be either <40%, 40%-50%,50%-60%,60%-70% or >70%')
  }
}

#########################################Winner Caste Map##################################################
WinnerCasteMapLegendList<- function(){return (c("General","SC","ST"))}


WinnerCasteMapLegendCount<-function(dframe){
  dframe$count<-1
  dframe<-aggregate(count~year+ac_type,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$ac_type),"(",dframe$count,")")
  dframe$count<-NULL
  dframe$year<-NULL
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(WinnerCasteMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$ac_type==x))==0){
      dframe<<-rbind(dframe,data.frame(ac_type=x,legend=paste0(x,"(0)")))
    }
  }) 
  return(dframe)
}

WinnerCasteMapLegendColor<- function(inp){
  
  if(inp=="General"){
    return('#1f77b4')
  }else if(inp=="SC"){
    return('#ff7f0e')
  }else if(inp=="ST"){
    return('#2ca02c')
  }else{
    stop('passed argument should be either 1,2,3 or >3')
  }
}

############################################party positions map######################################################3
PartyPositionsMapLegendList<- function(){
  return(c("1","2","3",">3"))
}

PartyPositionsMapBreakupList<- function(){
  return(c(0,1,2,3,4))
}

PartyPositionsMapLegendColor<-function(inp){
  if(inp=="1"){
    return('#696ddf')
  }else if(inp=="2"){
    return('#5154ac')
  }else if(inp=="3"){
    return('#393b79')
  }else if(inp==">3"){
    return('#212246')
  }else{
    stop('passed argument should be either 1,2,3 or >3')
  }
}

PartyPositionsMapLegendCount<-function(dframe){
  
  ##set a new column same as legend based on the percentage.
  dframe$tmp[dframe$position==1]<-"1"
  dframe$tmp[dframe$position==2]<-"2"
  dframe$tmp[dframe$position==3]<-"3"
  dframe$tmp[dframe$position>3]<-">3"
  dframe$position<-NULL
  dframe$count<-1
  dframe<-aggregate(count~year+tmp,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$tmp),"(",dframe$count,")")
  dframe<-subset(dframe,select=c("tmp","legend"))
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(PartyPositionsMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$tmp==x))==0){
      dframe<<-rbind(dframe,data.frame(tmp=x,legend=paste0(x,"(0)")))
    }
  }) 
  return(dframe)
  #we can't remove tmp column as it will be used in addLegend function of winnerVoteShareMap.R
}
