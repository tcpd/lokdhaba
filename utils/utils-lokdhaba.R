#right now no selection of shape file based on year.. but later we need to do it appropriately
readShapeFile<- function(sname, year){
  if(sname=="ge"){
    dname<-paste0("../tcpd_data/data/GE/Maps/Delim4/")
    lname<-paste0("India_PC_Updated")
    shape<-readOGR(dsn=dname,layer=lname)
    return(shape)  
  }else{
    dname<-paste0("../tcpd_data/data/AE/Maps/Delim4/",sname)
    lname<-paste0(sname,"_Assembly_con")
    shape<-readOGR(dsn=dname,layer=lname)
    return(shape)
  }
}

  isvalid<-function(obj,type){
    if(!is.null(obj)){
      if(type=="bool"){
        return(obj)
      }
      if(type=="string"){
        if(trimws(obj)==""){return(F)}else{return(T)}  
      }
      if(type=="list"){
        if(length(obj)==0){return(F)}else{return(T)}
      }
      if(type=="numeric"){
          if(obj==-1){return(F)}else{return(T)}
          }
    }else{
      return(F)
    }
  }


addPopupInfo<- function(winnersframe,type="state"){
  #browser() #Check for Assembly_1 and pc_ name
  cand<-paste0("<b>Candidate:</b> ", winnersframe$Candidate)
  marginp<-paste0("<b>Margin Percentage :</b> ",
                  paste0(winnersframe$Margin_Percentage,"%"))
  vsp <- paste0("<b>Vote Share
                :</b>",paste0(winnersframe$Vote_Share_Percentage,"%"))
  numcand<-paste0("<b>Total Candidates :</b> ", paste0(winnersframe$N_Cand))
  party<-paste0("<b>Party :</b> ", paste0(winnersframe$Party))             
  voter_turnout <- paste0("<b>Turnout :</b> ",paste0(winnersframe$Turnout_Percentage,"%"))
  
  assembly<-paste0("<b>Constituency :</b> ", winnersframe$Constituency_Name)
  
  winnersframe$popup<-paste(cand,assembly,party,numcand,vsp,marginp,voter_turnout,sep="<br>")
  return(winnersframe)
}

addTitleLeaflet<-function(map,titlemessage){
 winners<-getMapData(map)
titlemessage<-gsub("#","Assembly #",titlemessage)
 winners$Lat<-as.vector(coordinates(winners)[,2])
 winners$Long<-as.vector(coordinates(winners)[,1]) 

addControl(map,html=paste0(titlemessage,"<br>",
                                    "<p class=\"leaflet-tcpd\">Source: Adapted from <a href=&quot;www.eci.nic.in&quot;>ECI Data</a><br>",
                                     "<a href=&quot;www.tcpd.ashoka.edu.in&quot;>Trivedi Centre for Political Data, Ashoka University</a></p>"),className="leaflettitle")%>%
 addCircleMarkers(data=winners,lng=~ Long, lat= ~ Lat,fill=F, stroke=F,
                  color='#000000',opacity= 0, label=~ Constituency_Name,group='const')%>%
        addSearchFeatures(targetGroup = 'const',options=searchFeaturesOptions(textPlaceholder='Search Constituency', zoom=10,autoCollapse=T, autoCollapseTime=1600 , hideMarkerOnCollapse=T))


}
#read ae_maps.csv file for this state tcpd_data/AE/Data/ + st + /derived/lokdhaba/ae_maps.csv

readStateWinnersFile<- function(statename){
  if(statename=="ge"){
    filename<-paste0("../tcpd_data/data/GE/Data/derived/lokdhaba/ge_maps.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    m$newyear<-paste0(m$Year," (#",m$Assembly_No,")")
    m$Year<-NULL
    names(m)[names(m)=="newyear"]<-"Year"
     return(m) 
  }else{
    filename<-paste0("../tcpd_data/data/AE/Data/",statename,"/derived/lokdhaba/ae_maps.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    m$newyear<-paste0(m$Year," (#",m$Assembly_No,")")
    m$Year<-NULL
    names(m)[names(m)=="newyear"]<-"Year"
    return(m)
  }
}

#
getYearsForMap<-function(dframe){
        all<-unique(dframe$Year)
	return(Filter(function(x) x>=2009,all))

}

#read ae_partys.csv file for this state tcpd_data/AE/Data/ + st + /derived/lokdhaba/ae_maps.csv

readPartyPositionsFile<- function(statename){
  if(statename=="ge"){
    filename<-paste0("../tcpd_data/data/GE/Data/derived/lokdhaba/ge_partys.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    m$newyear<-paste0(m$Year," (#",m$Assembly_No,")")
    m$Year<-NULL
    names(m)[names(m)=="newyear"]<-"Year"
     return(m)
  }else{
    filename<-paste0("../tcpd_data/data/AE/Data/",statename,"/derived/lokdhaba/ae_partys.csv")
    print(paste0('reading from ',filename))
    m<-read.csv(filename)
    m$newyear<-paste0(m$Year," (#",m$Assembly_No,")")
    m$Year<-NULL
    names(m)[names(m)=="newyear"]<-"Year"
     return(m)
  }
  
}

###########################################Party color charts##############################################
getPartyColor <- function(partynames){
  allcols<-c('#576f2d',
             '#c44eb7',
             '#5dc356',
             '#7862ce',
             '#99b538',
             '#d8407f',
             '#4a9138',
             '#cc8cd8',
             '#c5a539',
             '#6489cc',
             '#df8b30',
             '#3dbabe',
             '#d53f41',
             '#5ab27f',
             '#934e88',
             '#a9ab65',
             '#dd81aa',
             '#92692f',
             '#a64657',
             '#e09a6b',
             '#be562a',
             '#de7c78')
  col <- read.csv("../tcpd_data/data/colours.csv")
  pal <- as.character(col$Color)
  #pal <- setNames(pal,col$Party)
  
  
  rest<-setdiff(partynames,col$Party)
  if(length(rest)==0){
    restcols <- c()
  }else{
    restcols<-allcols[1:length(rest)]
  }
  #partyl<-c(partyl,rest)
  pal<-c(pal,restcols)
  pal <- setNames(pal,c(as.character(col$Party),rest))
  return(pal)
  
} 

###########################################Party color maps##############################################
getColorFactorParty<-function(partynames){
  allcols<-c('#576f2d',
             '#c44eb7',
             '#5dc356',
             '#7862ce',
             '#99b538',
             '#d8407f',
             '#4a9138',
             '#cc8cd8',
             '#c5a539',
             '#6489cc',
             '#df8b30',
             '#3dbabe',
             '#d53f41',
             '#5ab27f',
             '#934e88',
            '#a9ab65',
            '#dd81aa',
            '#92692f',
            '#a64657',
            '#e09a6b',
            '#be562a',
            '#de7c78')
  partyl<-c()
  colors<-c()
  #color_file <- paste0("../tcpd_data/data/colours.csv")
  #c <- read.csv(color_file)
  #allcols <- c$Color
  #c_parties <- c$Party 
  #ind <- which(c_parties %in% partynames)
  #partyl <- c$Party[ind]
  #colors <- c$Color[ind]
  #browser()
  if("BJP"%in% partynames){
    partyl<-c(partyl,"BJP")
    colors<-c(colors,'#ff9933')
  }
  if("INC"%in% partynames){
    partyl<-c(partyl,"INC")
    colors<-c(colors,'#138808')
  }
  if("INC(I)"%in% partynames){
    partyl<-c(partyl,"INC(I)")
    colors<-c(colors,'#138808')
  }
  if("BSP"%in% partynames){
    partyl<-c(partyl,"BSP")
    colors<-c(colors,'#003399')
  }
  if("SP"%in% partynames){
    partyl<-c(partyl,"SP")
    colors<-c(colors,'#990000')
  }
  if("SAD"%in% partynames){
    partyl<-c(partyl,"SAD")
    colors<-c(colors,'#0000ff')
  }
  if("BLD"%in% partynames){
    partyl<-c(partyl,"BLD")
    colors<-c(colors,'#00ff99')
  }
  if("JD"%in% partynames){
    partyl<-c(partyl,"JD")
    colors<-c(colors,'#90f887')
  }
  if("JNP(S)"%in% partynames){
    partyl<-c(partyl,"JNP(S)")
    colors<-c(colors,'#bf4080')
  }
  if("BJS"%in% partynames){
    partyl<-c(partyl,"BJS")
    colors<-c(colors,'#ff9933')
  }
  if("CPM"%in% partynames){
    partyl<-c(partyl,"CPM")
    colors<-c(colors,'#991f00')
  }
  if("SWA"%in% partynames){
    partyl<-c(partyl,"SWA")
    colors<-c(colors,'#0066cc')
  }
  if("JNP"%in% partynames){
    partyl<-c(partyl,"JNP")
    colors<-c(colors,'#602040')
  }
  if("ADMK"%in% partynames){
    partyl<-c(partyl,"ADMK")
    colors<-c(colors,'#37f226')
  }
  if("AITC"%in% partynames){
    partyl<-c(partyl,"AITC")
    colors<-c(colors,'#ffad33')
  }
  if("JS"%in% partynames){
    partyl<-c(partyl,"JS")
    colors<-c(colors,'#339966')
  }
  if("TDP"%in% partynames){
    partyl<-c(partyl,"TDP")
    colors<-c(colors,'#ffff00')
  }
  if("DMK"%in% partynames){
    partyl<-c(partyl,"DMK")
    colors<-c(colors,'#b3b300')
  }
  if("BJD"%in% partynames){
    partyl<-c(partyl,"BJD")
    colors<-c(colors,'#4df43e')
  }
  if("JD(U)"%in% partynames){
    partyl<-c(partyl,"JD(U)")
    colors<-c(colors,'#90f887')
  }
  if("CPI"%in% partynames){
    partyl<-c(partyl,"CPI")
    colors<-c(colors,'#ff3300')
  }
  if("TMC(M)"%in% partynames){
    partyl<-c(partyl,"TMC(M)")
    colors<-c(colors,'#993366')
  }
  if("RJD"%in% partynames){
    partyl<-c(partyl,"RJD")
    colors<-c(colors,'#cc0066')
  }
  if("IND"%in% partynames){
    partyl<-c(partyl,"IND")
    colors<-c(colors,'#808080')
  }
  if("ADK"%in% partynames){
    partyl<-c(partyl,"ADK")
    colors<-c(colors,'#37f226')
  }
  if("NCP"%in% partynames){
    partyl<-c(partyl,"NCP")
    colors<-c(colors,'#99003d')
  }
  if("AIRJP"%in% partynames){
    partyl<-c(partyl,"AIRJP")
    colors<-c(colors,'#009999')
  }
  
  if("AAP"%in% partynames){
    partyl<-c(partyl,"AAP")
    colors<-c(colors,'#b35900')
  }
  if("AAAP"%in% partynames){
    partyl<-c(partyl,"AAAP")
    colors<-c(colors,'#b35900')
  }
  if("GPP"%in% partynames){
    partyl<-c(partyl,"GPP")
    colors<-c(colors,'#ffcc00')
  }
  if("BJNKP"%in% partynames){
    partyl<-c(partyl,"BJNKP")
    colors<-c(colors,'#333300')
  }
  rest<-setdiff(partynames,partyl)
  if(length(rest)==0){
    restcols <- c()
  }else{
    restcols<-allcols[1:length(rest)]
  }
  partyl<-c(partyl,rest)
  colors<-c(colors,restcols)
  
  pal<- leaflet::colorFactor(colors,levels=partyl,na.color = "white")
  return(pal)
  
}

############################################VoteShareMap######################################################3
voteShareMapLegendList<- function(){
  return(c("<20%","20%-30%","30%-40%","40%-50%","50%-60%",">60%"))
}

voteShareMapBreakupList<- function(){
  return(c(0,20,30,40,50,60,100))
}

VoteShareMapLegendColor<-function(inp){
  if(inp=="<20%"){
    return('#eff3ff')
  }else if(inp=="20%-30%"){
    return('#c6dbef')
  }else if(inp=="30%-40%"){
    return('#9ecae1')
  }else if(inp=="40%-50%"){
    return('#6baed6')
  }else if(inp=="50%-60%"){
    return('#3182bd')
  }else if(inp==">60%"){
    return('#08519c')
  }else{
    stop('passed argument should be either <20%, 20%-30%,
         30%-40%,40%-60%,50%-60% or >60%')
  }
}

VoteShareMapLegendCount<-function(dframe){
  
  ##set a new column same as legend based on the percentage.
  dframe$tmp[dframe$Vote_Share_Percentage<20]<-"<20%"
  dframe$tmp[dframe$Vote_Share_Percentage>=20 & dframe$Vote_Share_Percentage<30]<-"20%-30%"
  dframe$tmp[dframe$Vote_Share_Percentage>=30 & dframe$Vote_Share_Percentage<40]<-"30%-40%"
  dframe$tmp[dframe$Vote_Share_Percentage>=40 & dframe$Vote_Share_Percentage<50]<-"40%-50%"
  dframe$tmp[dframe$Vote_Share_Percentage>=50 & dframe$Vote_Share_Percentage<60]<-"50%-60%"
  dframe$tmp[dframe$Vote_Share_Percentage>=60]<-">60%"
  dframe$Vote_Share_Percentage<-NULL
  dframe$count<-1
  dframe<-aggregate(count~Year+tmp,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$tmp)," (",dframe$count,")")
  dframe<-subset(dframe,select=c("tmp","legend"))
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(voteShareMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$tmp==x))==0){
      dframe<<-rbind(dframe,data.frame(tmp=x,legend=paste0(x," (0)")))
    }
  }) 
  return(dframe)
  #we can't remove tmp column as it will be used in addLegend function of winnerVoteShareMap.R
}
#########################################Winner Gender Map##################################################
WinnerGenderMapLegendList<- function(){return (c("Male","Female","Others"))}


WinnerGenderMapLegendCount<-function(dframe){
  dframe$count<-1
  dframe<-aggregate(count~Year+Sex,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$Sex)," (",dframe$count,")")
  dframe$count<-NULL
  dframe$Year<-NULL
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(WinnerGenderMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$Sex==x))==0){
      dframe<<-rbind(dframe,data.frame(Sex=x,legend=paste0(x," (0)")))
    }
  }) 
  return(dframe)
}

WinnerGenderMapLegendColor<-function(inp){
  if(inp=="Male"){
    return('#1f78b4')
  }else if(inp=="Female"){
    return('#b2df8a')
  }else if(inp=="Others"){
    return('#a6cee3')
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
  dframe$tmp[dframe$Margin_Percentage<5]<-"<5%"
  dframe$tmp[dframe$Margin_Percentage>=5 & dframe$Margin_Percentage<10]<-"5%-10%"
  dframe$tmp[dframe$Margin_Percentage>=10 & dframe$Margin_Percentage<20]<-"10%-20%"
  dframe$tmp[dframe$Margin_Percentage>=20]<-">20%"
  #browser()
  dframe$Margin_Percentage<-NULL
  dframe$count<-1
  #browser()
  dframe<-aggregate(count~tmp,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$tmp)," (",dframe$count,")")
  dframe<-subset(dframe,select=c("tmp","legend"))
  #browser()
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(WinnerMarginMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$tmp==x))==0){
      dframe<<-rbind(dframe,data.frame(tmp=x,legend=paste0(x," (0)")))
    }
  }) 
  return(dframe)

}

WinnerMarginMapLegendColor<-function(inp){
  if(inp=="<5%"){
    return('#f1eef6')
  }else if(inp=="5%-10%"){
    return('#bdc9e1')
  }else if(inp=="10%-20%"){
    return('#74a9cf')
  }else if(inp==">20%"){
    return('#0570b0')
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
  dframe$tmp[dframe$N_Cand<5]<-"<5"
  dframe$tmp[dframe$N_Cand>=5 & dframe$N_Cand<15]<-"5-15"
  dframe$tmp[dframe$N_Cand>=15]<-">15"
  dframe$N_Cand<-NULL
  dframe$count<-1
  #browser()
  dframe<-aggregate(count~tmp,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$tmp)," (",dframe$count,")")
  dframe<-subset(dframe,select=c("tmp","legend"))
  #browser()
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(NumCandidatesMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$tmp==x))==0){
      dframe<<-rbind(dframe,data.frame(tmp=x,legend=paste0(x," (0)")))
    }
  }) 
  
  return(dframe)
  
}

NumCandidatesMapLegendColor<-function(inp){
  if(inp=="<5"){
    return('#ece7f2')
  }else if(inp=="5-15"){
    return('#a6bddb')
  }else if(inp==">15"){
    return('#2b8cbe')
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
  dframe$tmp[dframe$Nota_Percentage<1]<-"<1%"
  dframe$tmp[dframe$Nota_Percentage>=1 & dframe$Nota_Percentage<3]<-"1%-3%"
  dframe$tmp[dframe$Nota_Percentage>=3 & dframe$Nota_Percentage<5]<-"3%-5%"
  dframe$tmp[dframe$Nota_Percentage>=5]<-">5%"
  #browser()
  dframe$Nota_Percentage<-NULL
  dframe$count<-1
  #browser()
  dframe<-aggregate(count~tmp,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$tmp)," (",dframe$count,")")
  dframe<-subset(dframe,select=c("tmp","legend"))
  #browser()
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(NotaTurnoutMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$tmp==x))==0){
      dframe<<-rbind(dframe,data.frame(tmp=x,legend=paste0(x," (0)")))
    }
  }) 
  return(dframe)
  
}

NotaTurnoutMapLegendColor<-function(inp){
  if(inp=="<1%"){
    return('#f1eef6')
  }else if(inp=="1%-3%"){
    return('#bdc9e1')
  }else if(inp=="3%-5%"){
    return('#74a9cf')
  }else if(inp==">5%"){
    return('#0570b0')
  }else{
    stop('passed argument should be either <1%, 1%-3%, 3%-5% or >5%')
  }
}


##########################################Voter Turnout Map#################################################
VoterTurnoutMapLegendList <- function(){return
(c("<50%","50%-60%","60%-70%","70%-75%","75%-80%","80%-85%","85%-90%","90%-95%",">95%"))}


VoterTurnoutMapBreakupList<- function(){
  return(c(0,50,60,70,75,80,85,90,95,100))
}

VoterTurnoutMapLegendCount<- function(dframe){
  ##set a new column same as legend based on the percentage.
  dframe$tmp[dframe$Turnout_Percentage<50]<-"<50%"
  dframe$tmp[dframe$Turnout_Percentage>=50 & dframe$Turnout_Percentage<60]<-"50%-60%"
  dframe$tmp[dframe$Turnout_Percentage>=60 & dframe$Turnout_Percentage<70]<-"60%-70%"
  dframe$tmp[dframe$Turnout_Percentage>=70 & dframe$Turnout_Percentage<75]<-"70%-75%"
  dframe$tmp[dframe$Turnout_Percentage>=75 & dframe$Turnout_Percentage<80]<-"75%-80%"
  dframe$tmp[dframe$Turnout_Percentage>=80 & dframe$Turnout_Percentage<85]<-"80%-85%"
  dframe$tmp[dframe$Turnout_Percentage>=85 & dframe$Turnout_Percentage<90]<-"85%-90%"
  dframe$tmp[dframe$Turnout_Percentage>=90 & dframe$Turnout_Percentage<95]<-"90%-95%"
  
  dframe$tmp[dframe$Turnout_Percentage>=95]<-">95%"
  #browser()
  dframe$Turnout_Percentage<-NULL
  dframe$count<-1
  #browser()
  dframe<-aggregate(count~tmp,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$tmp)," (",dframe$count,")")
  dframe<-subset(dframe,select=c("tmp","legend"))
  #browser()
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(VoterTurnoutMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$tmp==x))==0){
      dframe<<-rbind(dframe,data.frame(tmp=x,legend=paste0(x," (0)")))
    }
  }) 
  return(dframe)
  
}

VoterTurnoutMapLegendColor<-function(inp){
  if(inp=="<50%"){
    return('#f7fbff')
  }else if(inp=="50%-60%"){
    return('#deebf7')
  }else if(inp=="60%-70%"){
    return('#c6dbef')
  }else if(inp=="70%-75%"){
    return('#9ecae1')
  }else if(inp=="75%-80%"){
    return('#6baed6')
  }else if(inp=="80%-85%"){
    return('#4292c6')
  }else if(inp=="85%-90%"){
    return('#2171b5')
  }else if(inp=="90%-95%"){
    return('#08519c')
  }else if(inp==">95%"){
    return('#08306b')
  }else{
    stop('passed argument should be either <40%, 40%-50%,50%-60%,60%-70%,
         70%-80% or >80%')
  }
}

#########################################Winner Caste Map##################################################
WinnerCasteMapLegendList<- function(){return (c("General","SC","ST"))}


WinnerCasteMapLegendCount<-function(dframe){
  dframe$count<-1
  dframe<-aggregate(count~Year+Constituency_Type,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$Constituency_Type)," (",dframe$count,")")
  dframe$count<-NULL
  dframe$Year<-NULL
  #browser()
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(WinnerCasteMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$Constituency_Type==x))==0){
      dframe<<-rbind(dframe,data.frame(Constituency_Type=x,legend=paste0(x," (0)")))
    }
  }) 
  return(dframe)
}

WinnerCasteMapLegendColor<- function(inp){
  
  if(inp=="General"){
    return('#1f78b4')
  }else if(inp=="SC"){
    return('#a6cee3')
  }else if(inp=="ST"){
    return('#b2df8a')
  }else{
    stop('passed argument should be either 1,2,3 or >3')
  }
}

############################################party Positions map######################################################3
PartyPositionsMapLegendList<- function(){
  return(c("1","2","3",">3"))
}

PartyPositionsMapBreakupList<- function(){
  return(c(0,2,3,4,100))
}

PartyPositionsMapLegendColor<-function(inp){
  if(inp=="1"){
    return('#0570b0')
  }else if(inp=="2"){
    return('#74a9cf')
  }else if(inp=="3"){
    return('#bdc9e1')
  }else if(inp==">3"){
    return('#f1eef6')
  }else{
    stop('passed argument should be either 1,2,3 or >3')
  }
}

PartyPositionsMapLegendCount<-function(dframe){
  #browser()
  #dframe <- unique(dframe)
  ##set a new column same as legend based on the percentage.
  dframe$tmp[dframe$Position==1]<-"1"
  dframe$tmp[dframe$Position==2]<-"2"
  dframe$tmp[dframe$Position==3]<-"3"
  dframe$tmp[dframe$Position>3]<-">3"
  dframe$Position<-NULL
  dframe$count<-1
  dframe<-aggregate(count~Year+tmp,dframe,function(x) length(x))
  dframe$legend<-paste0(trimws(dframe$tmp)," (",dframe$count,")")
  dframe<-subset(dframe,select=c("tmp","legend"))
  #add missing legends. They might be missing because no value was between the corresponding percentage
  lapply(PartyPositionsMapLegendList(),function(x){
    if(nrow(subset(dframe,dframe$tmp==x))==0){
      dframe<<-rbind(dframe,data.frame(tmp=x,legend=paste0(x," (0)")))
    }
  }) 
  return(dframe)
  #we can't remove tmp column as it will be used in addLegend function of winnerVoteShareMap.R
}
