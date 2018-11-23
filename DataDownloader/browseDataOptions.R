source('utils/utils-datadownloader.R')
library(DT)
# library(shinyTree)
browseDataOptions <- function(input, output, session,conmanager) {

  ##Variable to store the values used across functions
  current_filters<-c()
  observe({
    shiny::updateSelectInput(session,"bd_electiontype_selector",selected = conmanager$getval("bd_electiontype_selector",""))
    
  })
  #####################Filling in the state names from the state csv file, ideally it should be filled from 
  #####################StateNameNormalized file
  ###Get's triggered on initialization of state_selection UI
  observe({
    if(!is.null(input$bd_electiontype_selector) && trimws(input$bd_electiontype_selector)!=""){
    
      a<-getValidStateNamesForElectionType(input$bd_electiontype_selector)
      #print(paste0('Reading state lists for election type ',input$dd_electiontype_selector))
      b<-lapply((a),function(x) gsub("_"," ",x))
      if(input$bd_electiontype_selector=="GE"){
        current_filters$electiontype<<-"GE"
        b<-c("All",b)
        shiny::updateSelectizeInput(session,"bd_state_selector",choices =
                                    c("Select"="",b),selected=conmanager$getval("bd_state_selector",""))
        ##get the statnames present in general election mastersheet file and update
        ##dd_state_selector control with these name.. make sure to add All as well.
      }else if(input$bd_electiontype_selector=="AE"){
        current_filters$electiontype<<-"AE"
        shiny::updateSelectizeInput(session,"bd_state_selector",choices =
                                    c("Select"="",b),selected=conmanager$getval("bd_state_selector",""))
        ##get the statnames present in assembly election mastersheet file and update
        ##dd_state_selector control with these name.. make sure to add All as well.
        ##set current selection as empty one
      }
      #restoreInput("dd_state_selector",default="")
    }
  })
  
  ##Filling of tree after the selection of statename..
  observe({
# query <- parseQueryString(session$clientData$url_search)
# cat(file=stderr(),"state info","\n")
# cat(file=stderr(),input$dd_state_selector,"\n")
# cat(file=stderr(),query,"\n")

    if(!is.null(input$bd_state_selector) && trimws(input$bd_state_selector)!=""){
      current_filters$statename<<-input$bd_state_selector
      f<-getYearList(current_filters$statename,type=current_filters$electiontype)
      ##Now use this list to fill in the tree
      current_filters$all_years<<-f
    
      shiny::updateCheckboxGroupInput(session,"bd_year_selector",choices=f,selected=conmanager$getval("bd_year_selector",""))
      ####MAKE DT visible on the condition that at least one element in the tree is selected..
      }else{

      shiny::updateCheckboxGroupInput(session,"bd_year_selector",choices=c(),selected="")
    
     }
  })
  
  ##Filling of assembly numbers after selecting them on dropdown
  observe({
    if(!is.null(input$bd_year_selector)){
      print("here")
      #print(paste("election year selector",input$dd_year_selector))
      current_filters$electionyears <<- input$bd_year_selector
      print(current_filters$electionyears)
      #current_filters$partynames<<-input$filter_pname
    }
  })

  ##implementation of select all functionality
  observeEvent(input$bd_select_all_assemblies,{
	#cat(file=stderr(),"--inside observer","\n")
 #cat(file=stderr(),input$dd_select_all_assemblies,"\n")
     if(!is.null(input$bd_select_all_assemblies)){
	if(input$bd_select_all_assemblies==T){
	#cat(file=stderr(),"--inside true observer","\n")
		#select all options in input input$dd_year_selector
		updateCheckboxGroupInput(session,"bd_year_selector",selected=current_filters$all_years)
	}else{
	#cat(file=stderr(),"--inside false observer--","\n")

		#uncheck all options in input input$dd_year_selector
		updateCheckboxGroupInput(session,"bd_year_selector",selected="")

	}
    }
   })


  observeEvent(input$bd_variablenames_selector_cell_clicked, {
    validate(need(length(input$bd_variablenames_selector_cell_clicked) > 0, ''))
    print(paste("clicked value : ",input$bd_variablenames_selector_cell_clicked$value))
    print(input$bd_variablenames_selector_row_clicked)
    #browser()
    col_idx<-NA
    if(current_filters$electiontype=="AE"){
      col_idx <- 21
    }else if(current_filters$electiontype=="GE"){
      col_idx <- 19
    }
    
    #print(input$bd_variablenames_selector_cell_clicked$value)
    if(input$bd_variablenames_selector_cell_clicked$col == col_idx & !is.na(col_idx)){
      #print("here")
      conmanager$setval("kyn_pid_selector",input$bd_variablenames_selector_cell_clicked$value)
      shiny::updateTextInput(session,"kyn_pid_selector",value = conmanager$getval("kyn_pid_selector",""))
      shiny::updateNavbarPage(session,inputId = "Page",selected = "KYN")
    }
    
    #KnowYourNetaOptions(input,output,session,conmanager)
    
    #alert("You clicked something!")
  })
  
  ##Rendering of variable information table
  output$bd_variablenames_selector<- DT::renderDataTable(
    options= list(pageLength=100,sDom = '<"top">lrt<"bottom">ip'),
    filter="top",selection= "single",rownames=F,{
      print("rerendring browse data.")
      print(paste(current_filters$electiontype,current_filters$statename,input$bd_year_selector))
      dframe <- getMastersheetData(current_filters$electiontype,current_filters$statename,input$bd_year_selector)
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
