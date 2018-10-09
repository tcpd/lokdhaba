  getGenderNames<-function(genders,envr){
    #browser()
    assign(genders,c("male","female","total"),env=envr)
  }

  plotChart<-function(state,gendernames , plot,envr){
    selectedgendernames<-get(gendernames,envr)
    sname<-gsub(" ","_",get(state,envr))
    b<-readVoterTurnoutFile(sname)

    #setting up variables for visualization data download
    dat <- subset(b,select = c("State_Name","Year",gsub(" ","_",selectedgendernames)))
    conmanager$setval("visData",dat)
    conmanager$setval("selectedState",sname)
    conmanager$setval("vis","VoterTurnout")
    
    #create a base line chart with year as the x-axis
    base<-plot_ly(b, x = ~Year)
          lapply(selectedgendernames,function(x) {
        print(paste('adding',x));
        base<<-add_trace(base,y=~get(x),name=x,mode='lines+markers',showlegend=TRUE)
        }
        )
      sname<-gsub("_"," ",sname)
      thistitle<-paste0('Voter turnout across years in ',sname)
      xtitle<-''
      ytitle<-'Turnout in %'
      yrange<-c(0,100)
    assign(plot, preparechartlayout(base,thistitle,xtitle,ytitle,yrange),env=envr)

  }
