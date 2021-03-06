  ###################Specific for voteshare chart visualization##########################################
  getOptions<-function(options,envr){
    #browser()
    assign(options,c("Total Candidates","Deposit Lost"),env=envr)
  }

  plotChart<-function( options , plot,envr){
    selectedoptionnames<-get(options,envr)
  
    #create a base line chart with year as the x-axis
     b<-readCandidatesContestedDepositLostFile("ge")
        #pivotdata<-dcast(b,year~party)
        #create a base line chart with year as the x-axis
     #setting up variables for visualization data download
     dat <- subset(b,select = c("Year",gsub(" ","_",selectedoptionnames)))
     dat$State_Name <- "LokSabha"
     conmanager$setval("visData",dat)
     conmanager$setval("selectedState","LokSabha")
     conmanager$setval("vis","CandidatesContested")
    base<-plot_ly(b, x = ~Year)
    lapply(selectedoptionnames,function(x) {
        n<-gsub(" ","_",x)
        print(paste('adding',x));
        
        base<<-add_trace(base,y=~get(n),name=x, type ='bar')
        }
        )
      thistitle<-paste0('Contested and deposit lost across years in LokSabha')
      xtitle<-''
      ytitle<-'Number of Candidates'
      yrange <-c(0,15000)
    assign(plot,preparechartlayout(base,thistitle,xtitle,ytitle,yrange),env=envr)
  }
  #################################################################################################
