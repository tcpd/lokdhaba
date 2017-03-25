getFixedUIHolders<-function(stname,stshortname){
  info<-paste0("Election Analysis for ",stname)
  tagList(tags$h3(info),
          fluidRow(getSummaryBoxes(stshortname)),
          tags$hr(style="border-color: purple;"),
          fluidRow(
  box(title=paste0("Incomplete results for ",stname), solidheader=TRUE, width=6,leafletOutput(paste0(stshortname,"_vis1")), collapsible = TRUE, solidHeader = TRUE,status= "primary"),
  box(title=paste0("Declared results for ",stname), solidheader=TRUE, width=6,leafletOutput(paste0(stshortname,"_vis2")), collapsible = TRUE, solidHeader = TRUE,status= "primary")),
  fluidRow(
  box(title=paste0("Comparison with 2014 General Election for ",stname), solidheader=TRUE, width=6,leafletOutput(paste0(stshortname,"_vis3")), collapsible = TRUE, solidHeader = TRUE,status= "primary"),
  box(title=paste0("Comparison with 2012 Assembly Election for ",stname), solidheader=TRUE, width=6,leafletOutput(paste0(stshortname,"_vis4")), collapsible = TRUE, solidHeader = TRUE,status= "primary")))

}


getFixedUIHoldersSideBar<-function(stname,stshortname){
  info<-paste0("Election Analysis for ",stname)
  uiOutput(paste0(stshortname,"_info"))
}

getSummaryBoxes<-function(stshortname){
if(stshortname=="UP"){
  tagList(
    infoBox("Counting Status",color="red",width=3,fill=TRUE),
    infoBox("BJP+",icon=icon(list(src="myassets/logo.png",width="100px")),color="orange",width=3,fill=TRUE),
    infoBox("SP+INC",color="green",width=3,fill=TRUE),
    infoBox("BSP",color="blue",width=3,fill=TRUE))
  }else if(stshortname=="UK"){
    tagList(infoBox("Counting Status",color="red",width=3,fill=TRUE),
    infoBox("BJP+",color="orange",width=3,fill=TRUE),
    infoBox("INC+",color="green",width=3,fill=TRUE),
    infoBox("UKD",color="teal",width=3,fill=TRUE))
  }else if(stshortname=="PB"){
    tagList(infoBox("Counting Status",color="red",width=4,fill=TRUE),
    infoBox("BJP",color="orange",width=2,fill=TRUE),
    infoBox("SAD",color="blue",width=2,fill=TRUE),
    infoBox("INC",color="green",width=2,fill=TRUE),
    infoBox("AAP",color="maroon",width=2,fill=TRUE))
  }
  
}