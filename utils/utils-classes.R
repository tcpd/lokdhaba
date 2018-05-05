
################## Connection restore value management ####################
conmanager<-setRefClass("ConnectionRestoreManager",fields=c("restoredvals"="list"))
conmanager$methods(setval=function(name,val){
  restoredvals[[name]]<<-val
})
conmanager$methods(getval=function(name,default=""){
  if( !(name %in% names(restoredvals)) || restoredvals[[name]]==""){
    return(default)  
  }else{
    t<-restoredvals[[name]]
    # # removed for testing
    restoredvals[[name]]<<-NULL ##remove that element
    return(t)
  }
  
})


