  ###################Specific for voteshare chart visualization##########################################
  getOptions<-function(options,envr){
    #browser()
    assign(options,c("total candidates","deposit saved"),env=envr)
  }

  plotChart<-function(state, options , plot,envr){
    selectedoptionnames<-get(options,envr)
    sname<-gsub(" ","_",get(state,envr))
  
    #create a base line chart with year as the x-axis
     b<-readCandidatesContestedDepositLostFile(sname)
        #pivotdata<-dcast(b,year~party)
        #create a base line chart with year as the x-axis
    base<-plot_ly(b, x = ~year)
    lapply(selectedoptionnames,function(x) {
        n<-gsub(" ","_",x)
        print(paste('adding',x));
        
        base<<-add_trace(base,y=~get(n),name=x, type ='bar')
        }
        )
      sname<-gsub("_"," ",sname)
      thistitle<-paste0('Contested and deposit saved across years in ',sname)
      xtitle<-''
      ytitle<-'Number of Candidates'
      yrange<-c(0,5000)

    assign(plot,preparechartlayout(base,thistitle,xtitle,ytitle,yrange),env=envr)
  }
  #################################################################################################
