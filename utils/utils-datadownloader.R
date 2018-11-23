getValidStateNamesForElectionType<-function(electiontype)
{
  if(toupper(trimws(electiontype))=="GE"){
    a<-read.csv("../tcpd_data/data/GE/Data/derived/mastersheet.csv",stringsAsFactors = F)
    names(a)[names(a)=="State_Name"]<-"State Name"
    return(as.vector(unique(subset(a,select=c("State Name")))))
  }else if(toupper(trimws(electiontype))=="AE"){
    return(list.files(path="../tcpd_data/data/AE/Data"))
  }else{
    stop('Some serious issue as only AE and GE as options are expected')
  }
}

getYearList<-function(statename,type="ge"){
  #if type == "ge" return all years when elections happened..
  if(toupper(type)=="GE"){
  a<-read.csv("../tcpd_data/data/GE/Data/derived/mastersheet.csv")
  aa<-getUniqueANoWithYears(a,"Assembly_No")
  aa$Assembly_No<-as.numeric(trimws(aa$Assembly_No))
  aa$ano<-Vectorize(getStringFormatOfNumber)(aa$Assembly_No)
  }else{
    #otherwise return only election year of that state
  statenamemodified<- gsub(" ","_",statename)
    filename<-paste0("../tcpd_data/data/AE/Data/",statenamemodified,"/derived/mastersheet.csv")
    a<-read.csv(filename)
    aa<-getUniqueANoWithYears(a,"Assembly_No")
    aa$Assembly_No<-as.numeric(trimws(aa$Assembly_No))
    aa$ano<-Vectorize(getStringFormatOfNumber)(aa$Assembly_No)
  }
  aa$yearinfo<-Vectorize(getYearAssemblyNoStringFormat)(aa$Year,aa$ano)
  return(aa$yearinfo)
  
}

getStringFormatOfNumber<-function(num){
  if(num==1){
    return("1st")
  }else if (num==2){
    return("2nd")
  }else if (num==3){
    return("3rd")
  }else{
    return(paste0(num,"th"))
  }
}

##Here str is of the form '%s Assembly (%d)'
#return alist with two element Year and Assembly containing year and assembly number information.
getYearAndAssemblyNumberFromStringFormat<-function(str){
  year<-regmatches(str,regexec("([0-9]+)[sntr][tdh][ ]+(Assembly)[ ]+[(][ ]*([0-9]+)[ ]*[)]",str))[[1]][4]
  assembly<-regmatches(str,regexec("([0-9]+)[sntr][tdh][ ]+(Assembly)[ ]+[(][ ]*([0-9]+)[ ]*[)]",str))[[1]][2]
print(paste0('For ',str,' Year is ',year,' ano is ',assembly))
  return(list(Year=year,Assembly=assembly))
  
}


getYearAssemblyNoStringFormat<-function(Year,ano){
  return(paste0(ano," Assembly (",Year,")"))
}

getUniqueANoWithYears<-function(dframe,ano){
  aa<-unique(subset(dframe,select=c("Year",ano)))
  b<-aggregate(Year~get(ano),aa,function(x) head(x)[1])
  names(b)[1]<-ano
  return(b)
}

getMastersheetPathTimestamped<-function(electiontype,statename,electionyears){
  statenamemodified<- gsub(" ","_",statename)
  
  return (paste0(electiontype,".",statenamemodified,".",Sys.Date(),".csv"))  
}

getMastersheetData<-function(electiontype,statename,electionyears){
 #get proper statename by replacing  space with _.
 statenamemodified<- gsub(" ","_",statename)
 if(trimws(toupper(electiontype))=="GE"){
    ano<-"Assembly_No"
 #   print('reading data.. GE')
  #read mastersheet
   ms<-read.csv("../tcpd_data/data/GE/Data/derived/mastersheet.csv",stringsAsFactors=FALSE)
  if(toupper(statenamemodified)!="ALL"){
    ms<-subset(ms,ms$State_Name==statenamemodified)
  }
 }else if(trimws(toupper(electiontype))=="AE"){
   ano<-"Assembly_No"
#   print('reading data AE..')
   ms<-read.csv(paste0("../tcpd_data/data/AE/Data/",statenamemodified,"/derived/mastersheet.csv"),stringsAsFactors=FALSE)
 }else{
   stop('Some serious issue as only possible options are AE(assembly election)/GE(general election)')
 }
 #subset on assembly numbers 
 #get a vector of assembly numbers from election years..
 lst<-lapply(electionyears,function(x){
   y<-getYearAndAssemblyNumberFromStringFormat(x)
   return(as.numeric(y$Assembly))
 })
 l<-unlist(lst,1)
 ##surprising, why did we need double brackets here..
 ms<-subset(ms,ms[[ano]]%in%l)
 print(unique(ms[[ano]]))
 ####Remove all those columns which should not be distributed right now.. like caste/jati..
 ms$Jati<-NULL
 ms$Caste_Rec<-NULL
 ms$Rel<-NULL
 ms$New.Caste<-NULL
 ms$Old_Jati<-NULL
 ms$Postal_votes<-NULL
 ms$MaleTurnout<-NULL
 ms$FemaleTurnout<-NULL

 #return that dataframe
 return(ms)
 }


#####################################################################################################################
getVariableInfo<-function(type){
  #read a file called variable description [going forward this is our code book]
  a<-read.csv("../tcpd_data/data/CodeBook.csv")
  #get all attributes where type is same as type and where type is "" (to denote that those names make sense
  #for both AE and GE)
  #cat(file=stderr(),names(a),"\n")
  #aa<-subset(a,trimws(toupper(a$validfor))==trimws(toupper(type)) | trimws(a$validfor)=="")
  #cat(file=stderr(),names(aa),"\n")
  aa<-subset(a,select=c("variable","description"))
#,"source","citation"))
  names(aa)[names(aa)=="variable"]<-"Variable Name"
  names(aa)[names(aa)=="description"]<-"Variable Description"
 # names(aa)[names(aa)=="source"]<-"Source"
 # names(aa)[names(aa)=="citation"]<-"How to cite"
  stopifnot(nrow(aa)!=0)
  return(aa)
}
getLegendIntervals <- function(vals,x){
  #browser()
  tmp <- vals
  vals <- gsub("<","0-",gsub("%","",vals))
  last <- vals[grep(">",vals)]
  last <- (as.numeric(gsub(">","",last))+0.001) %>% paste0("-100")
  vals[grep(">",vals)] <- last
  splits <- strsplit(vals,"-")
  ints <- sapply(splits, "[",1)
  indxs <- findInterval(x,as.numeric(ints))
  return(tmp[indxs])
}


getPidData<- function(fpid){
  meta_pids <- read.csv("../tcpd_data/data/pid_meta.csv",stringsAsFactors = F,na.strings = "")
  all_data <- NA
  if(fpid %in% unique(meta_pids$pid)){
    d1 <- subset(meta_pids,pid == fpid)
    ae_states <- unique(d1$State_Name[which(d1$Election_Type=="Vidhan_Sabha")])
    ae_data <-NULL
    for(st in ae_states){
      st_data <- read_csv(paste0("../tcpd_data/data/AE/Data/",st,"/derived/mastersheet.csv"))
      st_data <- subset(st_data,pid==fpid)
      ae_data <- rbind(ae_data,st_data)
    }
    ge_data <- NULL
    if("Lok_Sabha" %in% d1$Election_Type){
      ge_data <- read_csv("../tcpd_data/data/GE/Data/derived/mastersheet.csv")
      ge_data <- subset(ge_data,pid == fpid)
    }
    if(!is.null(ge_data)){
      ge_data[names(ae_data[which(!names(ae_data)%in% names(ge_data))])] <- NA
    }
    ae_data$Election_Type <- "Lok_Sabha"
    ge_data$Election_Type <- "Vidhan_Sabha"
    all_data <- rbind(ae_data,ge_data)
    remove(ae_data)
    remove(ge_data)
    remove(st_data)
    
  }else{
    pid_st <- substr(fpid, 3, 4)
    pid_et <- substr(fpid,1,2)
    
    if(pid_et =="GE"){
      all_data <- read_csv("../tcpd_data/data/GE/Data/derived/mastersheet.csv")
      all_data <- subset(all_data,pid == fpid)
    }else{
      st_codes <- read.csv("../tcpd_data/data/state_codes.csv",stringsAsFactors = F,strip.white = T)
      state <- as.character(subset(st_codes,ST_NAME == pid_st,select=c("State_Name")))
      if(length(state)>1)print("morethan one states")
      if(length(state)==1){
        all_data <- read_csv(paste0("../tcpd_data/data/AE/Data/",state,"/derived/mastersheet.csv"))
        all_data <- subset(all_data,pid==fpid)
      }
    }
    
  }
  return(all_data)
}
