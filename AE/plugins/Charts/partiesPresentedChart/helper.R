  ###################Specific for voteshare chart visualization##########################################
  getOptions<-function(options,envr){
    #browser()
    assign(options,c("Parties Contested","Parties Represented"),env=envr)
  }

  plotChart<-function(state, options , plot,envr){
    selectedoptionnames<-get(options,envr)
    sname<-gsub(" ","_",get(state,envr))
  
    #create a base line chart with year as the x-axis
        b<-readPartiesContestedRepresentedFile(sname)
        #pivotdata<-dcast(b,year~party)
        #create a base line chart with year as the x-axis
        #setting up variables for visualization data download
        dat <- subset(b,select = c("State_Name","Year",gsub(" ","_",selectedoptionnames)))
        conmanager$setval("visData",dat)
        conmanager$setval("selectedState",sname)
        conmanager$setval("vis","Parties_Contested_Represented")
        
        base<-plot_ly(b, x = ~Year)

      lapply(selectedoptionnames,function(x) {
        n<-gsub(" ","_",x)
        print(paste('adding',x));
        
        base<<-add_trace(base,y=~get(n),name=x, type ='bar')
        }
        )
      sname<-gsub("_"," ",sname)
      thistitle<-paste0('Parties contested and represented across years in ',sname)
      xtitle<-''
      ytitle<-'Number of parties'
      yrange<-c(0,100)

    assign(plot,preparechartlayout(base,thistitle,xtitle,ytitle,yrange),env=envr)
  }
  #################################################################################################
