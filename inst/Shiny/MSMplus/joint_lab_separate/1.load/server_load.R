###### pageupload has 4 different scenarios, upload in csv or json file and aim of presenting results or comparing approaches########

####the examples follow the aims###########

output$pageupload <- renderUI({
  
  
  fluidRow(  
    
    tags$style(HTML(".shiny-output-error-validation {color: green;}")),
    
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
    tags$head(
      
      tags$style(type = "text/css", ".navbar-nav {display: -webkit-box;display: -ms-flexbox;-webkit-box-orient: horizontal!important;
                    -webkit-box-direction: normal; -ms-flex-direction: column;flex-direction: column;padding-left: 0px;margin-bottom: 0px;
                    list-style: none;}"),
      
      
      tags$style(type = "text/css", ".form-control {display: block;width: 100%;height: calc(1.5em + 0.75rem + 2px);padding: 0.375rem 0.75rem;
                    font-size: 2rem !important;font-weight: 400;line-height: 1.5;color: #495057;background-color: #fff;
                      background-clip: padding-box;border: 1px solid #ced4da;border-radius: 0.25rem;-webkit-transition: border-color 0.15s ease-in-out, -webkit-box-shadow 0.15s ease-in-out;
                    transition: border-color 0.15s ease-in-out, -webkit-box-shadow 0.15s ease-in-out;transition: border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out;
                    transition: border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out, -webkit-box-shadow 0.15s ease-in-out;
                  }"),
      
      tags$style(type = "text/css", ".btn {display: inline-block;
                    font-weight: 400;color: #495057;text-align: center;vertical-align: middle;
                    cursor: pointer; -webkit-user-select: none;-moz-user-select: none;-ms-user-select: none;user-select: none;
                    background-color: transparent; border: 1px solid transparent;padding: 0.375rem 0.75rem;
                    font-size: 2rem !important;line-height: 1.5;border-radius: 0.25rem;
                    -webkit-transition: color 0.15s ease-in-out, background-color 0.15s ease-in-out, border-color 0.15s ease-in-out, -webkit-box-shadow 0.15s ease-in-out;
                    transition: color 0.15s ease-in-out, background-color 0.15s ease-in-out, border-color 0.15s ease-in-out, -webkit-box-shadow 0.15s ease-in-out;
                    transition: color 0.15s ease-in-out, background-color 0.15s ease-in-out, border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out;
                    transition: color 0.15s ease-in-out, background-color 0.15s ease-in-out, border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out, -webkit-box-shadow 0.15s ease-in-out;
                  }"),
      
      tags$style('body {font-size: 20px;}'),
      tags$style(HTML(type='file', "shiny-input-container{font-size: 12pt !important;}")),
      tags$style("input[type=checkbox] {transform: scale(2);}"),
      tags$style("input[type=number] {font-size: 20px;}"),
      tags$style("input[type=file] {font-size: 20px;}"),
      tags$style(type = "text/css", " .download_this{ height:40px; width:57px;color:blue; padding-top: 15px;}"),
      tags$style(type = "text/css", " .upload_this{ height:40px; width:57px;color:blue; padding-top: 15px;}"),
      tags$style('input[type=radio] {border: 1px;width: 80%; height: 1em;}'),
      tags$style(type="text/css", "select { width: 400px; }"),
      tags$style(type="text/css", "textarea { max-height: 400px; }"),
      tags$style(type='text/css', ".well { max-height: 400px; }"),
      tags$style(type='text/css', ".span4 { max-height: 400px; }"),
      tags$style(type="text/css", "select.shiny-bound-input { font-size:20px; height:35px !important;}"),
      tags$style(type="text/css", "input.shiny-bound-input { font-size:20px; height:35px !important;}"),
      tags$style(type="text/css", "shiny-output-error-myClass  { font-size:20px; height:21px;}"),
      tags$style(HTML(".shiny-output-error-validation {color: green;}" )),
      tags$style(type="text/css", "select { max-width: 150px; max-height: 100px;}"),
      tags$style(type="text/css", "textarea { max-width: 150px; max-height: 100px; }"),
      tags$style(type='text/css', ".well { max-width: 200px; max-height: 100px;}"),
      tags$style(type='text/css', ".span4 { max-width: 250px; max-height: 100px;}"),
      tags$style(    ".k-numeric-wrap input {height: 40px;}"),
      tags$style(type = "text/css", ".irs-grid-text {font-family: 'arial'; color: black; font-size: 20px;}"),
      tags$style(type = "text/css", ".custom-file-input::before {content: 'Select file';display: inline-block;background: linear-gradient(top, #f9f9f9, #e3e3e3);
                                                border: 1px solid #999;
                                                border-radius: 3px;
                                                padding: 5px 8px;
                                                outline: none;
                                                white-space: nowrap;
                                                -webkit-user-select: none;
                                                cursor: pointer;
                                                text-shadow: 1px 1px #fff;
                                                font-weight: 700;
                                                font-size: 15pt !important;}"),
      
    ),
    
    
    
    column(6,
           h1("Upload datasets"),
           
           radioButtons(inputId="loadtype", label= "File type (csv or json)",
                        choices=c("json","csv"),selected = "json"),
           div(radioButtons(inputId="aimtype", label= "Aim",
                            choices=c("Single multistate model"="present","Compare 2 multistate models"="compare"),selected = "present", inline=FALSE, width="100%"),
               style = "text-align: left; margin-right: 3px;"),
           
           
           conditionalPanel(condition="input.loadtype =='json' && input.aimtype =='present'",
                            uiOutput("example_present1")
           ) , 
           
           conditionalPanel(condition="input.loadtype =='json' && input.aimtype =='compare'",
                            uiOutput("example_compare1")
           ),
           
           conditionalPanel("input.loadtype=='csv' && input.aimtype=='present'",
                            uiOutput("example_present2")
           ),
           
           conditionalPanel("input.loadtype=='csv' && input.aimtype=='compare'",
                            uiOutput("example_compare2")
           )
           
           
           
           
    ),
    
    column(6,
           uiOutput("message"),
           uiOutput("message2"),
           uiOutput("message3"),
           
           conditionalPanel(condition="input.loadtype =='json' && input.aimtype =='present' && input.example=='No'",
                            uiOutput("jsonupload") 
           ) , 
           
           conditionalPanel(condition="input.loadtype =='json' && input.aimtype =='compare' && input.compare_approach=='No'",
                            uiOutput("twojsonupload") 
           ),
           
           conditionalPanel("input.loadtype=='csv' && input.aimtype=='present' && input.example2=='No'",
                            uiOutput("csvupload1"),   uiOutput("message4"),  uiOutput("csvupload2"),   uiOutput("message6"),uiOutput("message8")#,verbatimTextOutput("message10"),
           ),
           
           conditionalPanel("input.loadtype=='csv' && input.aimtype=='compare' && input.compare_approach2=='No'",
                            uiOutput("twocsvupload1"),  uiOutput("message5"), uiOutput("twocsvupload2"),   uiOutput("message7"),uiOutput("message9")
           ) ,         
           
        
           # uiOutput("message5")
    )
    
    
    
  )
  
  
  
  
  
})

output$example_present1<- renderUI({
  
  
  item_list <- list()
  item_list[[1]] <-  radioButtons(inputId="example", label= "Example (EBMT)",
                                  choices=c("No","Yes"),selected = "No")
  do.call(tagList, item_list)
})

output$example_present2<- renderUI({
  
  
  item_list <- list()
  item_list[[1]] <-  radioButtons(inputId="example2", label= "Example (EBMT)",
                                  choices=c("No","Yes"),selected = "No")
  do.call(tagList, item_list)
})

output$example_compare1<- renderUI({
  
  
  item_list <- list()
  item_list[[1]] <-  radioButtons(inputId="compare_approach",
                                  label= "Example (EBMT)- Compare approaches", choices=c("No","Yes"),selected = "No")
  
  do.call(tagList, item_list)
})

output$example_compare2<- renderUI({
  
  
  item_list <- list()
  item_list[[1]] <-  radioButtons(inputId="compare_approach2",
                                  label= "Example (EBMT)- Compare approaches", choices=c("No","Yes"),selected = "No")
  
  do.call(tagList, item_list)
})






output$jsonupload <- renderUI({
  
  item_list <- list()
  item_list[[1]] <- helpText("To derive the msboxes graph, a json file with the information on the states and transitions should be provided. msboxes command provides the json file")
  item_list[[2]] <- fileInput("json1_pr",  h1("Upload MSM summary information json file"), accept = c(".json"))
  item_list[[3]] <- helpText("To derive the graphs from the predictions, a json file with the information from the estimations should be provided. mspredict_adjusted command provides those estimates")
  item_list[[4]] <- div(fileInput("json2",  h1("Upload MSM analysis results json file"), accept = c(".json")), style="font-size:150%; font-family:Arial;" )
  
  do.call(tagList, item_list)
  
  
})

output$twojsonupload <- renderUI({
  
  item_list <- list()
  item_list[[1]] <- helpText("To derive the msboxes graph, a json file with the information on the states and transitions should be provided. msboxes command provides the json file")
  item_list[[2]] <- fileInput("json1_cp",  h1("Upload MSM summary information json file"), accept = c(".json"))
  item_list[[3]] <- helpText("To derive the graphs from the predictions, a json file with the information from the estimations should be provided. First approach")
  item_list[[4]] <- div(fileInput("json2a",  h1("Upload MSM analysis results 1st approach json file"), accept = c(".json")), style="font-size:150%; font-family:Arial;" )
  item_list[[5]] <- helpText("To derive the graphs from the predictions, a json file with the information from the estimations should be provided. Second approach")
  item_list[[6]] <- div(fileInput("json2b",  h1("Upload MSM analysis results 2nd approach json file"), accept = c(".json")), style="font-size:150%; font-family:Arial;" )
  
  
  do.call(tagList, item_list)
  
  
})




output$csvupload1 <- renderUI({
  
  item_list <- list()
  
  item_list[[1]] <- numericInput(inputId="Nstates_pr", label= "Number of states",value=1)
  item_list[[2]] <- fileInput("csv1_pr",  h1("Upload frequencies csv file (optional) (only comma delimited text files)"), accept = c(".csv"))
  
  do.call(tagList, item_list)
  
})

output$csvupload2 <- renderUI({
  
  matrixnames=vector()
  
  shiny::validate(need(input$Nstates_pr<50 , "Please define a number of states"),
                  need(input$Nstates_pr!=0,  "Please define a number of states different than 0"))
  
  Nstates=input$Nstates_pr
  
  for (i in 1:input$Nstates_pr) {
    matrixnames[i]= paste0("State ",i)
  }
  
  item_list <- list()
  
  item_list[[1]] <-h1("Transition matrix")
  
  
  item_list[[2]] <- matrixInput(inputId="tmat_input_pr",
                                value = matrix(NA, input$Nstates_pr, input$Nstates_pr,dimnames = list(matrixnames, matrixnames)),
                                rows = list(names=TRUE),
                                cols = list(names=TRUE),
                                class = "numeric",
                                paste = FALSE,
                                copy = FALSE)
  
  item_list[[3]]<-  fileInput("csv2",  h1("Upload csv of analysis results following the specific naming instructions"), accept = c(".csv"))
  
  
  do.call(tagList, item_list)
  
})


output$twocsvupload1 <- renderUI({
  
  item_list <- list()
  
  item_list[[1]] <- numericInput(inputId="Nstates_cp", label= "Number of states",value=1)
  item_list[[2]] <- fileInput("csv1_cp",  h1("Upload frequencies csv file (optional) (only comma delimited text files)"), accept = c(".csv"))
  
  do.call(tagList, item_list)
  
})

output$twocsvupload2 <- renderUI({
  
  matrixnames=vector()
  
  shiny::validate(need(input$Nstates_cp<50 , "Please define a number of states"),
                  need(input$Nstates_cp!=0,  "Please define a number of states different than 0"))
  
  Nstates=input$Nstates_cp
  
  for (i in 1:input$Nstates_cp) {
    matrixnames[i]= paste0("State ",i)
  }
  
  item_list <- list()
  
  item_list[[1]] <-h1("Transition matrix")
  
  item_list[[2]] <- matrixInput(inputId="tmat_input_cp",
                                value = matrix(NA, input$Nstates_cp, input$Nstates_cp,dimnames = list(matrixnames, matrixnames)),
                                rows = list(names=TRUE),
                                cols = list(names=TRUE),
                                class = "numeric",
                                paste = FALSE,
                                copy = FALSE)
  item_list[[3]]<-  fileInput("csv2a",  h1("Upload csv of analysis results of 1st approach"), accept = c(".csv"))
  item_list[[4]]<-  fileInput("csv2b",  h1("Upload csv of analysis results of 2nd approach"), accept = c(".csv"))
  
  do.call(tagList, item_list)
  
})





json1manual<-reactive  ({
  
  
  
  #options(scipen = 999)
  
  if (input$aimtype=="present") {
    
    
    Nstates=input$Nstates_pr
    
  }
  else if  (input$aimtype=="compare") {
    
    Nstates=input$Nstates_cp
  }
  
  boxwidth=0.15
  boxheight=0.15
  
  if (input$aimtype=="present") {tmatnamed= as.matrix(input$tmat_input_pr)}
  else if (input$aimtype=="compare") {tmatnamed= as.matrix(input$tmat_input_cp)}
  
  names=vector()
  
  for (i in 1:Nstates) {names[i]=paste0("State",i)}
  
  
  
  x=vector()
  y=vector()
  r=0.5
  slice=360/Nstates
  
  for (i in 1:Nstates) {
    x[i]= r+  0.4*cos(slice*(i-1)*(pi/180))
    y[i]= r+  0.4*sin(slice*(i-1)*(pi/180))
    
  }
  
  # Statenames
  statenames= names 
  
  tmatnamed2=tmatnamed
  tmatnamed2[is.na(tmatnamed2)] <- 0
  
  #Number of transitions
  Ntransitions=max(tmatnamed2)
  
  #### Transitions values
  u=list()
  
  for (i in 1:nrow(tmatnamed2)) {
    u[[i]]=unique(tmatnamed2[i,][which(!is.na(tmatnamed2[i,]))])
  }
  
  u_unlist=sort(unlist(u, recursive = TRUE, use.names = TRUE))
  
  transitions=order(u_unlist[u_unlist!=0])
  
  transnames=vector()
  
  for (j in 1:Ntransitions) {
    transnames[j]=paste0("h",j) 
  }
  
  # here the csv frequencies will be included in the app
  
  
  list_desc=list()
  
  list_desc[[1]]=Nstates
  list_desc[[2]]= Ntransitions
  list_desc[[3]]=x
  list_desc[[4]]=y
  list_desc[[5]]=boxwidth
  list_desc[[6]]=boxheight
  list_desc[[7]]=statenames
  list_desc[[8]]=transnames
  list_desc[[9]]=tmatnamed
  
  
  
  #  L <- readLines(input$csv1$datapath, n = 1)
  
  #  shiny::validate(need(grepl(";", L),"Provide ; delimited csv file")) 
  
  if (is.null(input$csv1_pr) & is.null(input$csv1_cp) ) {
    list_desc_final=list(Nstates= list_desc[[1]], Ntransitions= list_desc[[2]], 
                         xvalues= list_desc[[3]], yvalues=list_desc[[4]] ,
                         boxwidth=list_desc[[5]], boxheight= list_desc[[6]],
                         statenames=list_desc[[7]],transnames=list_desc[[8]],
                         tmat= list_desc[[9]])
  }
  
      
  if (!is.null(input$csv1_pr)) { 
    
    frequencies= as.data.frame(read.table(input$csv1_pr$datapath,header=TRUE, sep=","))
    
    shiny::validate(need(!is.null(frequencies$time_label) , "Please check that the frequency file was uploaded with the correct format and that it has variable time_label"),
                    need(!is.null(frequencies$timevar),   "Please check that the frequency file was uploaded with the correct format and that it has variable timevar"),
                    need(ncol(frequencies)==(Nstates+ Ntransitions+2),   "Please check that you have specified frequency variables for all states and transitions"))
    
    list_desc_final=list(Nstates= list_desc[[1]], Ntransitions= list_desc[[2]], 
                         xvalues= list_desc[[3]], yvalues=list_desc[[4]] ,
                         boxwidth=list_desc[[5]], boxheight= list_desc[[6]],
                         statenames=list_desc[[7]],transnames=list_desc[[8]],
                         tmat= list_desc[[9]], frequencies= frequencies   )
    }
  
  else if (!is.null(input$csv1_cp)) { 
    
    frequencies= as.data.frame(read.table(input$csv1_cp$datapath,header=TRUE, sep=","))
    
    shiny::validate(need(!is.null(frequencies$time_label) , "Please check that the frequency file was uploaded with the correct format and that it has variable time_label"),
                    need(!is.null(frequencies$timevar),   "Please check that the frequency file was uploaded with the correct format and that it has variable timevar"),
                    need(ncol(frequencies)==(Nstates+ Ntransitions+2),   "Please check that you have specified frequency variables for all states and transitions"))
    
    
    list_desc_final=list(Nstates= list_desc[[1]], Ntransitions= list_desc[[2]], 
                         xvalues= list_desc[[3]], yvalues=list_desc[[4]] ,
                         boxwidth=list_desc[[5]], boxheight= list_desc[[6]],
                         statenames=list_desc[[7]],transnames=list_desc[[8]],
                         tmat= list_desc[[9]], frequencies= frequencies   )
    }
  

    

  
  exportJson <- toJSON(list_desc_final, pretty = TRUE,force = TRUE, na='string')
  
  exportJson
  
  
})



json2manual<-reactive  ({
  
  if (input$loadtype=="json") {return()}
  
  else if (input$aimtype=="present" & input$loadtype=="csv") {
    
    if (is.null(input$csv2)) return() 
    
    else {
      
      data<- read.table(input$csv2$datapath,header=TRUE, sep=",")
      
      shiny::validate(need(!is.null(data$timevar) , "Please check that you have specified the timevar variable"),
                      need(!is.null(data$atlist),   "Please check that you have specified the atlist variable"),
                      need(!is.null(data$Nats),     "Please check that you have specified the Nats variable"),
                      need(!is.null(data$Ntransitions),"Please check that you have specified the Ntransitions variable"),
                      
                      need(length(which(  !startsWith(names(data),"timevar") & !startsWith(names(data),"atlist") &
                                          !startsWith(names(data),"Nats") & !startsWith(names(data),"Ntransitions") &
                                          !startsWith(names(data),"P_") & !startsWith(names(data),"Haz_") &
                                          !startsWith(names(data),"Los_") & !startsWith(names(data),"Visit_") &
                                          !startsWith(names(data),"User_") & !startsWith(names(data),"Number_") &
                                          !startsWith(names(data),"Next_") & !startsWith(names(data),"Soj_") &
                                          !startsWith(names(data),"First_")))==0, 
                                          "You may have specified a variable which does not follow the naming rules")
                      ) 
      
      
      tmatnamed= as.matrix(input$tmat_input_pr)
      tmatnamed2=tmatnamed
      tmatnamed2[is.na(tmatnamed2)] <- 0
      Ntransitions_tmat=max(tmatnamed2)
      
                           
      shiny::validate(need(Ntransitions_tmat==data$Ntransitions , "The transition matrix you specified has different number of transitions than the ones specified at the csv results file"))                          
                           
      
      final_list=list()
      
      final_list[[1]]=c(unique(data$timevar))
      final_list[[2]]=c(unique(data$Nats))
      final_list[[3]]=c(unique(data$Ntransitions))
      final_list[[4]]=c(unique(data$atlist))
      names(final_list)[1:4]<-c("timevar", "Nats", "Ntransitions", "atlist")
      
      for (i in 5:ncol(data)) {
        final_list[[i]]=matrix(nrow=unique(data$Nats), ncol=length(unique(data$timevar)),NA)
        names(final_list)[i] <- colnames(data)[i]
        
        for (k in 1:(data$Nats[1])) {
          
          final_list[[i]][k,] =data[which(data$atlist==unique(data$atlist)[k]),i]
        }
        
        if (length(grep("diff", names(final_list)[i])) | length(grep("ratio", names(final_list)[i])==1) ) {final_list[[i]]= final_list[[i]][-1,]}
      }
      
      
      final_list[[ncol(data)+1]]=as.matrix(input$tmat_input_pr)
      names(final_list)[ncol(data)+1]<-c("tmat")
      
      
      final_list$timevar=as.vector(final_list$timevar)
      
      final_list$atlist=as.vector(final_list$atlist)
      
      exportjson <- toJSON(final_list, pretty = TRUE,force = TRUE, na='string')
      exportjson
      
    }
    
  }
  
  
  
  else if (input$aimtype=="compare"  & input$loadtype=="csv") {
    
    if (is.null(input$csv2a) |is.null(input$csv2b) ) {return()}
    
    data1<- read.table(input$csv2a$datapath,header=TRUE, sep=",")
    

    shiny::validate(need(!is.null(data1$timevar) , "Please check that you have specified the timevar variable"),
                    need(!is.null(data1$atlist),   "Please check that you have specified the atlist variable"),
                    need(!is.null(data1$Nats),     "Please check that you have specified the Nats variable"),
                    need(!is.null(data1$Ntransitions),"Please check that you have specified the Ntransitions variable"),
                    
                    need(length(which( !startsWith(names(data1),"timevar") & !startsWith(names(data1),"atlist") &
                                        !startsWith(names(data1),"Nats") & !startsWith(names(data1),"Ntransitions") &
                                        !startsWith(names(data1),"P_") & !startsWith(names(data1),"Haz_") &
                                        !startsWith(names(data1),"Los_") & !startsWith(names(data1),"Visit_") &
                                        !startsWith(names(data1),"User_") & !startsWith(names(data1),"Number_") &
                                        !startsWith(names(data1),"Next_") & !startsWith(names(data1),"Soj_") &
                                        !startsWith(names(data1),"First_")))==0, 
                         "You may have specified a variable which does not follow the naming rules")
    ) 
    
    tmatnamed= as.matrix(input$tmat_input_cp)
    tmatnamed2=tmatnamed
    tmatnamed2[is.na(tmatnamed2)] <- 0
    Ntransitions_tmat=max(tmatnamed2)
    
    
    shiny::validate(need(Ntransitions_tmat==data1$Ntransitions , "The transition matrix you specified has different number of transitions than the ones specified at the 1st csv results file"))                          
    
    
    
    final_list_a=list()
    
    final_list_a[[1]]=c(unique(data1$timevar))
    final_list_a[[2]]=c(unique(data1$Nats))
    final_list_a[[3]]=c(unique(data1$Ntransitions))
    final_list_a[[4]]=c(unique(data1$atlist))
    names(final_list_a)[1:4]<-c("timevar", "Nats", "Ntransitions", "atlist")
    
    for (i in 5:ncol(data1)) {
      final_list_a[[i]]=matrix(nrow=unique(data1$Nats), ncol=length(unique(data1$timevar)),NA)
      names(final_list_a)[i] <- colnames(data1)[i]
      
      for (k in 1:(data1$Nats[1])) {
        
        final_list_a[[i]][k,] =data1[which(data1$atlist==unique(data1$atlist)[k]),i]
      }
      
      if (length(grep("diff", names(final_list_a)[i])) | length(grep("ratio", names(final_list_a)[i])==1) ) {final_list_a[[i]]= final_list_a[[i]][-1,]}
    }
    
    
    final_list_a[[ncol(data1)+1]]=as.matrix(input$tmat_input_cp)
    names(final_list_a)[ncol(data1)+1]<-c("tmat")
    
    
    final_list_a$timevar=as.vector(final_list_a$timevar)
    
    final_list_a$atlist=as.vector(final_list_a$atlist)
    
    list2a <- final_list_a
    
    #toJSON(final_list_a, pretty = TRUE,force = TRUE, na='string')
    
    
    data2<- read.table(input$csv2b$datapath,header=TRUE, sep=",") 
    
    
    shiny::validate(need(!is.null(data2$timevar) , "Please check that you have specified the timevar variable"),
                    need(!is.null(data2$atlist),   "Please check that you have specified the atlist variable"),
                    need(!is.null(data2$Nats),     "Please check that you have specified the Nats variable"),
                    need(!is.null(data2$Ntransitions),"Please check that you have specified the Ntransitions variable"),
                    
                    need(length(which(!startsWith(names(data2),"timevar") & !startsWith(names(data2),"atlist") &
                                        !startsWith(names(data2),"Nats") & !startsWith(names(data2),"Ntransitions") &
                                        !startsWith(names(data2),"P_") & !startsWith(names(data2),"Haz_") &
                                        !startsWith(names(data2),"Los_") & !startsWith(names(data2),"Visit_") &
                                        !startsWith(names(data2),"User_") & !startsWith(names(data2),"Number_") &
                                        !startsWith(names(data2),"Next_") & !startsWith(names(data2),"Soj_") &
                                        !startsWith(names(data2),"First_")))==0, 
                         "You may have specified a variable which does not follow the naming rules")
    ) 
    
    shiny::validate(need(Ntransitions_tmat==data2$Ntransitions , "The transition matrix you specified has different number of transitions than the ones specified at the 2nd csv results file"))                          
    
    
    final_list_b=list()
    
    final_list_b[[1]]=c(unique(data2$timevar))
    final_list_b[[2]]=c(unique(data2$Nats))
    final_list_b[[3]]=c(unique(data2$Ntransitions))
    final_list_b[[4]]=c(unique(data2$atlist))
    names(final_list_b)[1:4]<-c("timevar", "Nats", "Ntransitions", "atlist")
    
    for (i in 5:ncol(data2)) {
      final_list_b[[i]]=matrix(nrow=unique(data2$Nats), ncol=length(unique(data2$timevar)),NA)
      names(final_list_b)[i] <- colnames(data2)[i]
      
      for (k in 1:(data2$Nats[1])) {
        
        final_list_b[[i]][k,] =data2[which(data2$atlist==unique(data2$atlist)[k]),i]
      }
      
      if (length(grep("diff", names(final_list_b)[i])) | length(grep("ratio", names(final_list_b)[i])==1) ) {final_list_b[[i]]= final_list_b[[i]][-1,]}
    }
    
    
    final_list_b[[ncol(data2)+1]]=as.matrix(input$tmat_input_cp)
    names(final_list_b)[ncol(data2)+1]<-c("tmat")
    
    
    final_list_b$timevar=as.vector(final_list_b$timevar)
    
    final_list_b$atlist=as.vector(final_list_b$atlist)
    
    list2b <- final_list_b
    
    Nstates=ncol(list2b$tmat)
    Ntransitions=max(list2b$tmat[which(!is.na(list2b$tmat))])
    
    y=vector()
    
    for (i in 1:Nstates) {
      
      for (k in 1:Nstates) {
        
        # if (i<=k) {
        j=i+Nstates
        g=k+Nstates
        
        y=sub(paste0("_to_",k),paste0("_to_",g),names(list2b) )
        names(list2b)=y
        #  }
      }
    }
    
    list2b=list2b[-which(!startsWith(names(list2b),"User") == FALSE)]   
    names(list2b)
    
    for (i in 1:Ntransitions) {
      k= i+ Ntransitions
      names(list2b)=sub(paste0("h",i),paste0("h",k),names(list2b) )
    }
    
    
    ### Create a hypertmatrix####
    
    list2b$tmat2= list2b$tmat+Ntransitions
    
    l <- list(list2b$tmat,list2b$tmat2)
    
    list2b$hypertmat<- as.matrix(bdiag(l))
    list2b$hypertmat[which(list2b$hypertmat==0)]=NA
    
    list2b$hypertmat
    
    list2a=list2a[names(list2a) %in% "tmat" == FALSE] 
    list2b=list2b[names(list2b) %in% "tmat" == FALSE] 
    list2b=list2b[names(list2b) %in% "tmat2" == FALSE] 
    
    list2b$tmat=list2b$hypertmat
    
    final_list=list()
    final_list=c(list2a,list2b)
    
    
    exportjson <- toJSON(final_list, pretty = TRUE,force = TRUE, na='string')
    
    exportjson
    
  }
  
  
  
  
})







output$message<- renderUI ({
  if (input$loadtype=="json"  & input$aimtype=="present") {
    
    if  (is.null(input$json1) & input$example=="No") {
      return("Provide the json file with the msm box details")
    }
  }
  else if (input$loadtype=="json"  & input$aimtype=="compare") {
    
    if  (is.null(input$json1) & input$compare_approach=="No") {
      return("Provide the json file with the msm box details")
    }
  }
  else if (input$loadtype=="csv" & input$aimtype=="present") { 
    if (length(which(!is.na(input$tmat_input_pr))==TRUE)==0 & input$example2=="No") {
      return("Provide transition matrix")
    } 
  }
  else if (input$loadtype=="csv" & input$aimtype=="compare") {
    if ( length(which(!is.na(input$tmat_input_cp))==TRUE)==0 & input$compare_approach2=="No") {
      return("Provide transition matrix")
    }
  }
  else  {
    return("")
  }
  
})


output$message2<- renderUI ({
  if (input$loadtype=="json" & input$aimtype=="present") {
    if (is.null(input$json2) & input$example=="No") {
      return("Provide the json file with the predictions")
    }
  }
  else if (input$loadtype=="csv" & input$aimtype=="present" ) {
    if (is.null(input$csv2) & input$example2=="No") {
      return("Provide the csv file with the predictions")
    }
  }
  
  else if (input$loadtype=="json" & input$aimtype=="compare" ) { 
    if ((is.null(input$json2a) | is.null(input$json2b))   & input$compare_approach=="No") {
      return("Provide the json files with the predictions from the two approaches")
    }
  }
  else if (input$loadtype=="csv" & input$aimtype=="compare" ) { 
    if ((is.null(input$csv2a)| is.null(input$csv2b))  & input$compare_approach2=="No") {
      return("Provide the csv files with the predictions from the two approaches")
    } 
  }
  else {return("")}
  
})


output$message3<- renderUI ({
  if ( input$loadtype=="json" & input$aimtype=="present" ) {  
    if (input$example=="Yes")  {
      message = p(withMathJax(
        helpText(strong('Example dataset: EBMT')),
        
        helpText("The data originate from the European Blood and Marrow Transplant registry.\nThe dataset consists of 2204 patients who received bone marrow transplantation.\nThe three states a patient can be in is 1) Post- transplant, 2) Platelet recovery 3) Relapse/Death.\nThe covariate patterns used in this example are the 3 age categories, namely <20 y.old, 20-40y.old and >40 y.old .")
      ))
      message
    }
  }
  else if ( input$loadtype=="json" & input$aimtype=="compare") {  
    if (input$compare_approach=="Yes") {
      message = p(withMathJax(
        helpText(strong('Example dataset: EBMT')),
        
        helpText("The data originate from the European Blood and Marrow Transplant registry.\nThe dataset consists of 2204 patients who received bone marrow transplantation.\nThe three states a patient can be in is 1) Post- transplant, 2) Platelet recovery 3) Relapse/Death.\nThe covariate patterns used in this example are the 3 age categories, namely <20 y.old, 20-40y.old and >40 y.old .")
      ))
      message
    }
  }
  else if ( input$loadtype=="csv" & input$aimtype=="present" ) {  
    if (input$example2=="Yes")  {
      message = p(withMathJax(
        helpText(strong('Example dataset: EBMT')),
        
        helpText("The data originate from the European Blood and Marrow Transplant registry.\nThe dataset consists of 2204 patients who received bone marrow transplantation.\nThe three states a patient can be in is 1) Post- transplant, 2) Platelet recovery 3) Relapse/Death.\nThe covariate patterns used in this example are the 3 age categories, namely <20 y.old, 20-40y.old and >40 y.old .")
      ))
      message
    }
  }
  else if ( input$loadtype=="csv" & input$aimtype=="compare" ) {  
    if (input$compare_approach2=="Yes") {
      message = p(withMathJax(
        helpText(strong('Example dataset: EBMT')),
        
        helpText("The data originate from the European Blood and Marrow Transplant registry.\nThe dataset consists of 2204 patients who received bone marrow transplantation.\nThe three states a patient can be in is 1) Post- transplant, 2) Platelet recovery 3) Relapse/Death.\nThe covariate patterns used in this example are the 3 age categories, namely <20 y.old, 20-40y.old and >40 y.old .")
      ))
      message
    }
  }
  else {return("")}
  
})


output$message4<-   renderUI ({ 
  
  json1manual=  fromJSON(json1manual(), flatten=TRUE)

  #options(scipen = 999)
  
  if (input$aimtype=="present") { Nstates=input$Nstates_pr;tmatnamed= as.matrix(input$tmat_input_pr) }
  else if  (input$aimtype=="compare") {Nstates=input$Nstates_cp;tmatnamed= as.matrix(input$tmat_input_cp)}
  
  tmatnamed2=tmatnamed
  tmatnamed2[is.na(tmatnamed2)] <- 0
  
  #Number of transitions
  Ntransitions=max(tmatnamed2)
  
  if (!is.null(json1manual$frequencies)) {
  
       shiny::validate(need(!is.null(json1manual$frequencies$time_label) , "Please check that the frequency file was uploaded with the correct format and that it has variable time_label"),
                       need(!is.null(json1manual$frequencies$timevar),   "Please check that the frequency file was uploaded with the correct format and that it has variable timevar"),
                       need(ncol(json1manual$frequencies)==(Nstates+ Ntransitions+2),   "Please check that you have specified frequency variables for for the correct number of states and transitions"))
  }
})

output$message5<-   renderUI ({ 
  
  
  json1manual=  fromJSON(json1manual(), flatten=TRUE)

  #options(scipen = 999)
  
  if (input$aimtype=="present") { Nstates=input$Nstates_pr;tmatnamed= as.matrix(input$tmat_input_pr) }
  else if  (input$aimtype=="compare") {Nstates=input$Nstates_cp;tmatnamed= as.matrix(input$tmat_input_cp)}
  
  tmatnamed2=tmatnamed
  tmatnamed2[is.na(tmatnamed2)] <- 0
  
  #Number of transitions
  Ntransitions=max(tmatnamed2)
  
  if (!is.null(json1manual$frequencies)) {
    
    shiny::validate(need(!is.null(json1manual$frequencies$time_label) , "Please check that the frequency file was uploaded with the correct format and that it has variable time_label"),
                    need(!is.null(json1manual$frequencies$timevar),   "Please check that the frequency file was uploaded with the correct format and that it has variable timevar"),
                    need(ncol(json1manual$frequencies)==(Nstates+ Ntransitions+2),   "Please check that you have specified frequency variables for for the correct number of states and transitions"))
  }
  
})


output$message6<-   renderUI ({ 
  
 

  #options(scipen = 999)
  
  
  if (!is.null(json2manual() )) {
    
    json2manual=  fromJSON(json2manual(), flatten=TRUE)
    
    shiny::validate(need(!is.null(json2manual$timevar) , "Please check that you have specified the timevar variable"),
                    need(!is.null(json2manual$atlist),   "Please check that you have specified the atlist variable"),
                    need(!is.null(json2manual$Nats),     "Please check that you have specified the Nats variable"),
                    need(!is.null(json2manual$Ntransitions),"Please check that you have specified the Ntransitions variable")
                    )
  }
})

output$message7<-   renderUI ({ 
  

  
  #options(scipen = 999)
  
  
  if (!is.null(json2manual() )) {
    
    json2manual=  fromJSON(json2manual(), flatten=TRUE)
    
    shiny::validate(need(!is.null(json2manual$timevar) , "Please check that you have specified the timevar variable"),
                    need(!is.null(json2manual$atlist),   "Please check that you have specified the atlist variable"),
                    need(!is.null(json2manual$Nats),     "Please check that you have specified the Nats variable"),
                    need(!is.null(json2manual$Ntransitions),"Please check that you have specified the Ntransitions variable"),
                    
                    need(length(which(!startsWith(names(json2manual),"P_") | !startsWith(names(json2manual),"Haz_") |
                                        !startsWith(names(json2manual),"Los_") | !startsWith(names(json2manual),"Visit_") |
                                        !startsWith(names(json2manual),"User_") | !startsWith(names(json2manual),"Number_") |
                                        !startsWith(names(json2manual),"Next_") | !startsWith(names(json2manual),"Soj_") |
                                        !startsWith(names(json2manual),"First_")))==0, 
                         "You may have specified a variable which does not follow the naming rules")
    ) 
    
  }
})

output$message8<-   renderUI ({ 
  
  p_def = withMathJax( helpText('If issues when uploading the results csv file persist, check a) That you have used . 
                                as the decimal pointer and b) that you have put NA if whenever a variable has a missing value')
  )
  p_def
})

output$message9<-   renderUI ({ 
  
  p_def = withMathJax( helpText('If issues when uploading the results csv file persist, check a) That you have used . 
                                as the decimal pointer and b) that you have put NA if whenever a variable has a missing value')
  )
  p_def
})

#output$message10<-   renderPrint ({ 
#  
#  data<- read.table(input$csv2$datapath,header=TRUE, sep=",")
#  data[,which(!startsWith(names(data),"timevar") & !startsWith(names(data),"atlist") &
#                !startsWith(names(data),"Nats") & !startsWith(names(data),"Ntransitions") &
#                !startsWith(names(data),"P_") & !startsWith(names(data),"Haz_") &
#                !startsWith(names(data),"Los_") & !startsWith(names(data),"Visit_") &
#                !startsWith(names(data),"User_") & !startsWith(names(data),"Number_") &
#                !startsWith(names(data),"Next_") & !startsWith(names(data),"Soj_") &
#                !startsWith(names(data),"First_"))]
#})