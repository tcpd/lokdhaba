source('utils/utils-datadownloader.R')
library(DT)
# library(shinyTree)
KnowYourNetaOptions <- function(input, output, session,conmanager) {
  
# 	ae_data <- read_csv("../tcpd_data/data/AE/Analysis_Data/Consolidated_AE_mastersheet.csv")
# 	ge_data <- read_csv("../tcpd_data/data/GE/Data/derived/mastersheet.csv")
# 	ge_data[names(ae_data)[which(! names(ae_data) %in% names(ge_data))]] <-NA
# 
# 	ge_data$Election_Type <- "Lok_Sabha"
# 	ae_data$Election_Type <- "Vidhan_Sabha"
# 	all_data <- rbind(ge_data,ae_data)
  ##Variable to store the values used across functions
  current_filters<-c()
  observe({
    shiny::updateTextInput(session,"kyn_pid_selector",value = conmanager$getval("kyn_pid_selector",""))
    #print(conmanager$getval("kyn_pid_selector",""))
  })
  #####################Filling in the state names from the state csv file, ideally it should be filled from 
  #####################StateNameNormalized file
  ###Get's triggered on initialization of state_selection UI
  # observe({
  #     #shiny::updateSelectizeInput(session,inputId = "kyn_pid_selector",choices = c("Select =",a),selected = conmanager$getval("kyn_pid_selector",""))
  #     current_filters$pid <- input$kyn_pid_selector
  #     print(current_filters$pid)
  # 
  # })
  
  ##Filling of tree after the selection of statename..
  

  ##Rendering of variable information table
  output$kyn_table<- DT::renderDataTable(
    options= list(pageLength=100,sDom = '<"top">lrt<"bottom">ip'),
    filter="top",selection= "single",rownames=F,{
      #browser()
      print("rerendering know your neta data.")
      print(paste(session$input$kyn_pid_selector))
      # all_data <- as.data.frame(session$input$kyn_pid_selector)
      # return(all_data)
      
    dframe <- getPidData(session$input$kyn_pid_selector)
    if(!is.na(dframe)){
      c_names <- colnames(dframe)
      prior <- c("State_Name","Year","Candidate","Sex","Party","Constituency_Name","Position")
      last <- c("ENOP","DelimID","Assembly_No","Constituency_No","month")
      if("poll_no" %in% c_names){
        last <- c(last,"poll_no")
      }
      set <- c(prior,c_names[which(!c_names %in% c(prior,last))],last)
      sub <- subset(dframe,select=set)
      if("pid" %in% names(sub) )
      {sub$pid <- as.character(sub$pid)}
      n <- names(sub)
      n[which(n=="State_Name")] <- "State"
      n[which(n=="Constituency_Name")] <- "Constituency Name"
      n[which(n=="Candidate_Type")] <- "Cand. Type"
      n[which(n=="Valid_Votes")] <- "Valid Votes"
      n[which(n=="Constituency_Type")] <- "Const. Type"
      n[which(n=="District_Name")] <- "District"
      n[which(n=="Turnout_Percentage")] <- "Turnout %"
      n[which(n=="Margin_Percentage")] <- "Margin %"
      n[which(n=="Vote_Share_Percentage")] <- "Vote Share %"
      n[which(n=="Constituency_No")] <- "Const. No."
      n[which(n=="Deposit_Lost")] <- "Deposit Lost"
      
      names(sub) <- n
      return(sub) 
    }else{
      return(NULL)
    }
     
    #datatable(getVariableInfo(input$bd_electiontype_selector),#current_filters$electiontype),
    #          rownames=F,
    #          style='bootstrap',
    #          escape=T,
    #          options=list(paging=FALSE, 
    #                       lengthChange=FALSE,
    #                       info=FALSE
    #                       ),
    #          selection = 'single')
  })
  
  
  ##What to do when download button is pressed..
  ##collect election type, statename and year list..
  
}
