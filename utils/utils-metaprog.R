suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(dplyr)))


genFunType<-function(outputc){
  l<-unique(outputc$type)
  chk<-c("renderPlotly","renderLeaflet")
#  print(length(intersect(chk,l)))
  #,"Some serious issue as there can not be two rendering output in a filter")
  
  stopifnot(length(intersect(chk,l))<2)
  if(length(intersect(chk,l))==0){
    return("observer")
  }else{
      m<-filter(outputc,type==intersect(chk,l)[1])
    return(paste0(m$name,"<-",m$type))
  }
}

constructVariables<-function(filterc){

    code<-"##Variable to store the values used across functions\n
           currentvalues<-new.env()\n
           ##get the session id\n
           ns<-session$ns\n
           ##Store passed directory name (name of the dir where this R file is stored).. It is interesting that using dname directly\n
           ##does not work because as that value changes in server.R it changes here at the point of use as well.\n
           dirname<-dname\n values<-reactiveValues("
    nextt<-1
    apply(filterc,1,function(row){
           code<<-paste0(code,"triggerfor_",row["filterid"],"=-1,")
           nextt<<-nextt+1

        })
    ##add one extra for output that is already there.. plotarea..
    code<-paste0(code,"triggerfor_",nextt,"=-1")
    ##close the parenthesis for reactiveValues..
    code<- paste0(code,")\n")
    return(code)
    }


constructShowAllFunction<-function(plotarea){

    code<-paste0("ShowAll","<-","function(){\n","shinyjs::show(\"",plotarea,"\")\n","values$triggerfor_1<<-0","\n}\n")
    return(code)

    }

constructHidAllFunction<-function(plotarea){
    code<-paste0("HideAll","<-","function(){\n","ResetOutputRendering()\n","values$triggerfor_1<<- -1\n","shinyjs::hide(\"",plotarea,"\")\n","}\n")
    return(code)
    }

constructSetupFunction<-function(filteroutputarea,filterc,inputtable){

    innercode <<- paste0(filteroutputarea,"<-renderUI({\n #ShowAll()")
    ##add construction code for filter ui components here..
    apply(filterc,1,function(row){
        if(trimws(row["isinit"])==T){
          innercode <<- paste0(innercode, "\n tmp",row["filterid"], " <-", gsub(":",",",row["construction"]))
            #innercode<<-paste0(innercode,"\n",gsub(":",",",row["construction"]),",")
        }else if(trimws(row["isinit"])==F){
          fid <- row["filterid"]
          dep_inp <- filter(inputtable,filterid == fid) 
          innercode <<- paste0(innercode,"\n ","tmp",row["filterid"] ," <- if( T ")
          apply(dep_inp,1,function(inp_row){
            innercode <<- paste0(innercode," & isvalid(currentvalues$",inp_row["alias"],",\"",inp_row["type"],"\")")
          })
          innercode <<- paste0(innercode,"){")
            
          innercode <<- paste0(innercode,"\n ",gsub(":",",",row["construction"]),"\n } \n else {")
          innercode<<-paste0(innercode,"\nshinyjs::hidden(",gsub(":",",",row["construction"]),") \n }")
        }else{
            stop('some serious error, isinit tag should be either T or F')
        }
        })
    innercode <<- paste0(innercode,"\n tagList (")
    apply(filterc,1,function(row){
      innercode <<- paste0(innercode,"\n tmp",row["filterid"],",")
    })
    ##as we have an extra "," at the end (do we have that alwasy?? yes at least one filter must be there-- but keep it in mind)
    innercode <<-  substr(innercode, 1, nchar(innercode)-1)
    innercode <<-paste0(innercode,") \n })\n")
    ##call setupoutputrendering also..
    innercode <<-paste0(innercode,"SetupOutputRendering()","\n")
    code<-paste0("Setup","<-","function(){\n",innercode,"}","\n")
    return(code)
    }

constructReturnCode<-function(){
      code<-paste0("ret","<-","c()","\n","ret$HideAll","<-","HideAll","\n","ret$ShowAll","<-","ShowAll","\n","ret$Setup","<-","Setup","\n",
                   "ret$SetupOutputRendering","<-","SetupOutputRendering","\n","return (ret)","\n")
      return(code)

    }

constructObserversRenderers<-function(inputc,outputc,func){
  
  code<-""
  observercode<-""
  renderingcode<-""
  resetrenderingcode<-""
  #run for reach filter id obtained from input,output,function table..
  tmp<-lapply(unique(inputc$filterid), function(fid){
    #browser()
    isObserver<-T
    inputcc<-filter(inputc,filterid==fid)
    outputcc<-filter(outputc,filterid==fid)
    s<-genFunType(outputcc)
    if(s=="observer"){##Means we need to update this output type in an observer
     isObserver<-T
     code<<-paste0(code,"observe({\n") 
    }else{##Means we need to create a rendered for this output type
      isObserver<-F
      code<<-paste0(code,s,"({\n")
      resetrenderingcode<<-paste0(resetrenderingcode,code,"return()})\n","\n")
    }
    ###Get input values and store that in alias variables..
   
    inputvalidcheckexpression<-paste0("T && isvalid(values$triggerfor_",fid,",\"numeric\")")
    if(!is.na(inputcc[["name"]])){
      apply(inputcc,1,function(row){
        code<<-paste0(code,"currentvalues$",row["alias"],"<<-",row["name"],"\n")
        ###construct validity checking expression variable and type
        inputvalidcheckexpression<<-paste0(inputvalidcheckexpression," && isvalid(","currentvalues$",row["alias"],",\"",row["type"],"\")")
      })
    }
    
    ##Check the validity of input
    code<<-paste0(code,"if(",inputvalidcheckexpression,")\n{","\n")
    #{
    ###If it is valid then get transfer function for this id and invoke that
    fname<-filter(func,filterid==fid)
    stopifnot(length(fname)!=1)
    #,"Some error as there must be only one transformation function per filter")
    code<<-paste0(code,gsub(":",",",fname$funname),"\n")
    ###for each output of this filter id--starting from updaters to render in last{
    ###for most work. now we have only one output... it requires further look at the protocol when we need to support multiple rendereres.
    apply(outputcc,1,function(row){
           conmanagerdefault<-""
         
        if(grepl("all",row["default"])){
           conmanagerdefault<-paste0("currentvalues$",row["alias"])
        }else if(grepl("single",row["default"])){
            conmanagerdefault<-paste0("\"\"")
        }else{
            conmanagerdefault<-paste0("c()")
        }
     
      ##### use alias variables to update the output depending upon whether it is"
      #####updater or renderer..(in case or rendere just put the return value-alias name)
      if(!grepl("Plotly",row["type"]) && !grepl("Leaflet",row["type"]))
      {
        
        conmanagercode=paste0("conmanager$getval(ns(\"",row["name"],"\")",",",conmanagerdefault,")")
        code<<-paste0(code,
                      row["type"],
                      "(parentsession,",
                      "ns(\"",row["name"],"\"),",
                      "choices=currentvalues$",row["alias"],
                      ",selected=",conmanagercode,")","\n")
        ###make this output visible
        code<<-paste0(code,"shinyjs::show(\"",row["name"],"\")","\n")
        ## trigger the next filter..[imp: under isolate otherwise recursive observation loop happens]
        code<<-paste0(code,"isolate({\n values$triggerfor_",fid+1,"<<-(values$triggerfor_",fid+1,"+1)%%2\n})","\n")
      }else{
        ##no need to trigger the next filter as it must be the last filter that is being rendered (but this invariant should be discussed)
        code<<-paste0(code,"currentvalues$",row["alias"],"\n")
      }
      ##endfor    
    })
    #}
    code<<-paste0(code,"}","else{","\n")
    ###else
    #{
    ###for each output of this filter id--starting from updaters to render in last{
    ###make this output hidden by removing its content and setting to empty
    ##endfor
    apply(outputcc,1,function(row){
        clearcode<-""
        if(grepl("single",row["default"])){
           clearcode<-paste0("\"\"")
        }else {
            clearcode<-"c()"
        }
     
      if(!grepl("Plotly",row["type"]) && !grepl("Leaflet",row["type"]))
      {
        code<<-paste0(code,
                      row["type"],
                      "(parentsession,",
                      "ns(\"",row["name"],"\"),",
                      "choices=",clearcode,",selected=",clearcode,")\n")
        ##Make this output invisible.
        code<<-paste0(code,"shinyjs::hide(\"",row["name"],"\")","\n")
        ###why am in not trigger frotrigger values here??
        ##what are the implications?
      }else{
        code<<-paste0(code,"return()","\n")
      }
      
     code<<-paste0(code,"}","\n")
    code<<-paste0(code,"})\n\n")

    })
    if(isObserver){
        observercode<<-paste0(observercode,code,"\n\n")
        code<<-""
    }else{
        renderingcode<<-paste0(renderingcode,code,"\n\n")
        code<<-""

    }
    })
    #}
   
    ##Make an observer or renderer from the code variable and insert in the environment
  code<-paste0(observercode,"SetupOutputRendering<-function(){\n",renderingcode,"\n}","\n\n")
  code<-paste0(code,"ResetOutputRendering<-function(){\n",resetrenderingcode,"\n}","\n\n")
  return(code)
    }

constructFunctions<-function(inputtable,outputtable,funtable,filteroutputarea,filters,script.dir,componentname,componentarguments,plotarea){
  
  inputc<-fread(inputtable)
  outputc<-fread(outputtable)
  func<-fread(funtable)
  filterc<-fread(filters)

    code<-paste0(constructVariables(filterc),"\n\n")
  code<-paste0(code,constructSetupFunction(filteroutputarea,filterc,inputc),"\n\n")

    code<-paste0(code,constructShowAllFunction(plotarea),"\n\n")

    code<-paste0(code,constructHidAllFunction(plotarea),"\n\n")
 
    code<-paste0(code,constructObserversRenderers(inputc,outputc,func),"\n\n")

    code<-paste0(code,constructReturnCode(),"\n\n")


    ##Get the path where this script is stored in order to get the files present in the current directory
    helpercode<-paste(readLines(paste0(script.dir,"/","helper.R")),collapse="\n")
##
    overallcode<-paste0(componentname,"<-function(",componentarguments,"){","\n")
    overallcode<-paste0(overallcode,helpercode,"\n\n")
    overallcode<-paste0(overallcode,"######Auto generated code############",code,"}\n\n")

    
  return(overallcode)

    
}
