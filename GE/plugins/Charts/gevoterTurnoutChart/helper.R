  getGenderNames<-function(genders,envr){
    #browser()
    assign(genders,c("male","female","total"),env=envr)
  }

  plotChart<-function(gendernames , plot,envr){
    selectedgendernames<-get(gendernames,envr)
    b<-readVoterTurnoutFile("ge")

    #create a base line chart with year as the x-axis
    base<-plot_ly(b, x = ~Year)
          lapply(selectedgendernames,function(x) {
        print(paste('adding',x));
        base<<-add_trace(base,y=~get(x),name=x,mode='lines+markers',showlegend=TRUE)
        }
        )
      thistitle<-paste0('Voter turnout across years in LokSabha')
      xtitle<-''
      ytitle<-'Turnout in %'
      yrange<-c(0,100)
    assign(plot, preparechartlayout(base,thistitle,xtitle,ytitle,yrange),env=envr)

  }
