  ###################Specific for voteshare chart visualization##########################################
  getOptions<-function(options,envr){
    #browser()
    assign(options,c("Parties Contested","Parties Represented"),env=envr)
  }

  plotChart<-function(options , plot,envr){
    selectedoptionnames<-get(options,envr)
  
    #create a base line chart with year as the x-axis
        b<-readPartiesContestedRepresentedFile("ge")
        #pivotdata<-dcast(b,year~party)
        #create a base line chart with year as the x-axis
        base<-plot_ly(b, x = ~Year)

      lapply(selectedoptionnames,function(x) {
        n<-gsub(" ","_",x)
        print(paste('adding',x));
        
        base<<-add_trace(base,y=~get(n),name=x, type ='bar')
        }
        )
      thistitle<-paste0('Parties contested and represented across years in
                        LokSabha')
      xtitle<-''
      ytitle<-'Number of parties'
      yrange<-c(0,500)

    assign(plot,preparechartlayout(base,thistitle,xtitle,ytitle,yrange),env=envr)
  }
  #################################################################################################
