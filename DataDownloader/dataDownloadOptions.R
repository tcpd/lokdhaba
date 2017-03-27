source('utils/utils-datadownloader.R')
library(DT)
# library(shinyTree)
dataDownloadOptions <- function(input, output, session,dname) {

  ##Variable to store the values used across functions
  current_filters<-c()
  
  #####################Filling in the state names from the state csv file, ideally it should be filled from 
  #####################StateNameNormalized file
  ###Get's triggered on initialization of state_selection UI
  observe({
    if(!is.null(input$dd_electiontype_selector) && trimws(input$dd_electiontype_selector)!=""){
    
      a<-getValidStateNamesForElectionType(input$dd_electiontype_selector)
      print(a)
      b<-lapply((a),function(x) gsub("_"," ",x))
      #message(paste0('input value is ',input$dd_electiontype_selector))      
      if(input$dd_electiontype_selector=="GE"){
        current_filters$electiontype<<-"GE"
        b<-c("All",b)
        #Create a selection input box
        #print(b)
        shiny::updateSelectizeInput(session,"dd_state_selector",choices = c("Select"="",b),selected="")
        
        ##get the statnames present in general election mastersheet file and update
        ##dd_state_selector control with these name.. make sure to add All as well.
        ##set current selected as empty one
      }else if(input$dd_electiontype_selector=="AE"){
        current_filters$electiontype<<-"AE"
        shiny::updateSelectizeInput(session,"dd_state_selector",choices = c("Select"="",b),selected="")
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

    if(!is.null(input$dd_state_selector) && trimws(input$dd_state_selector)!=""){
      #print('state selected')
      current_filters$statename<<-input$dd_state_selector
      #print(current_filters$statename)
      f<-getYearList(current_filters$statename,type=current_filters$electiontype)
      ##Now use this list to fill in the tree
      shiny::updateCheckboxGroupInput(session,"dd_year_selector",choices=f,selected=f)
      #structure(list("11","33"),stselected=F))
      ####MAKE DT visible on the condition that at least one element in the tree is selected..
      }else{

      shiny::updateCheckboxGroupInput(session,"dd_year_selector",choices=c(),selected="")
    
     }
  })
  
  ##Filling of assembly numbers after selecting them on dropdown
  observe({
    if(!is.null(input$dd_year_selector)){
      #print(paste("election year selector",input$dd_year_selector))
      current_filters$electionyears<<-input$dd_year_selector
      #current_filters$partynames<<-input$filter_pname
    }
  })
  output$dd_variablenames_selector<-DT::renderDataTable(
    datatable(getVariableInfo(input$dd_electiontype_selector),#current_filters$electiontype),
              rownames=F,
              style='bootstrap',
              escape=T,
              options=list(paging=FALSE, 
                           lengthChange=FALSE,
                           info=FALSE
                           ),
              selection = 'single')
      )
  
  output$dd_terms_and_conditions<-renderUI({
    includeHTML("DataDownloader/TermsConditions.html")
  })
  
  ##What to do when download button is pressed..
  ##collect election type, statename and year list..
  output$dd_download_button <- downloadHandler(
       filename = function() {
         getMastersheetPathTimestamped(current_filters$electiontype,current_filters$statename,current_filters$electionyears)
       },
       content = function(con) {
         data<-getMastersheetData(current_filters$electiontype,current_filters$statename,current_filters$electionyears)
         write.csv(data, con,row.names=F)
       }
     )
  
}
