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
  aa<-getUniqueANoWithYears(a,"ga_no")
  aa$ga_no<-as.numeric(trimws(aa$ga_no))
  aa$ano<-Vectorize(getStringFormatOfNumber)(aa$ga_no)
  }else{
    #otherwise return only election year of that state
  statenamemodified<- gsub(" ","_",statename)
    filename<-paste0("../tcpd_data/data/AE/Data/",statenamemodified,"/derived/mastersheet.csv")
    a<-read.csv(filename)
    aa<-getUniqueANoWithYears(a,"sa_no")
    aa$sa_no<-as.numeric(trimws(aa$sa_no))
    aa$ano<-Vectorize(getStringFormatOfNumber)(aa$sa_no)
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
    ano<-"ga_no"
 #   print('reading data.. GE')
  #read mastersheet
   ms<-read.csv("../tcpd_data/data/GE/Data/derived/mastersheet.csv",stringsAsFactors=FALSE)
  if(toupper(statenamemodified)!="ALL"){
    ms<-subset(ms,ms$State_Name==statenamemodified)
  }
 }else if(trimws(toupper(electiontype))=="AE"){
   ano<-"sa_no"
#   print('reading data AE..')
   ms<-read.csv(paste0("../tcpd_data/data/AE/Data/",statenamemodified,"/derived/mastersheet.csv"),stringsAsFactors=FALSE)
 }else{
   stop('Some serious issue as only possible options are AE(assembly election)/GE(general election)')
 }
 print(electionyears)
 #subset on assembly numbers 
 #get a vector of assembly numbers from election years..
 lst<-lapply(electionyears,function(x){
   y<-getYearAndAssemblyNumberFromStringFormat(x)
   return(as.numeric(y$Assembly))
 })
 l<-unlist(lst,1)
 ##surprising, why did we need double brackets here..
 ms<-subset(ms,ms[[ano]]%in%l)
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
  aa<-subset(a,trimws(toupper(a$validfor))==trimws(toupper(type)) | trimws(a$validfor)=="")
  #cat(file=stderr(),names(aa),"\n")
  aa<-subset(aa,select=c("variable","description"))
#,"source","citation"))
  names(aa)[names(aa)=="variable"]<-"Variable Name"
  names(aa)[names(aa)=="description"]<-"Variable Description"
 # names(aa)[names(aa)=="source"]<-"Source"
 # names(aa)[names(aa)=="citation"]<-"How to cite"
  stopifnot(nrow(aa)!=0)
  return(aa)
}
