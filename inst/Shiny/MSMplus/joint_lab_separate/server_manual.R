
output$page_manual <- renderUI({
  
  
  fluidRow(
    
    column(4,
           h1(""),
           
           
           
           
           uiOutput("manual"),
           uiOutput("manual2")
    ),
    
    column(8,

    )

    
  )      
  

})

output$manual <- renderUI({
  
  item_list <- list()
  
  
  item_list[[1]] <- numericInput(inputId="Nstates", label= "Number of states", value=1)
  item_list[[2]] <- fileInput("csv1",  h1("Upload frequencies in each state and transition for various time points"), accept = c(".csv"))

    
    
  do.call(tagList, item_list)
  
})

output$manual2 <- renderUI({
  
  item_list <- list()
  
  item_list[[1]] <- matrixInput(inputId="tmat_input",
                                value = matrix("NA", input$Nstates, input$Nstates),
                                rows = list(),
                                cols = list(),
                                class = "character",
                                paste = FALSE,
                                copy = FALSE)
  item_list[[2]]<-  fileInput("csv2",  h1("Upload csv of analysis results following the specific naming instructions"), accept = c(".csv"))
  
  
  
  do.call(tagList, item_list)
  
})


json1manual<-reactive  ({
  
  #options(scipen = 999)
  Nstates=input$Nstates
  boxwidth=0.15
  boxheight=0.15
  tmatnamed= as.matrix(input$tmat_input)
  
  names=vector()
  
  for (i in 1:Nstates) names[i]=paste0("State ",i)
  
  colnames(tmatnamed)=names
  
  x=vector()
  y=vector()
  r=0.5
  slice=360/Nstates
  
  for (i in 1:Nstates) {
    x[i]= r+  0.4*cos(slice*(i-1)*(pi/180))
    y[i]= r+  0.4*sin(slice*(i-1)*(pi/180))

  }
  
  # Statenames
  statenames= colnames(tmatnamed) 
  
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
  
  for (j in 1:ntransitions) {
    transnames[j]=paste0("h",j) 
  }
  
  # here the csv frequencies will be included in the app
  frequencies= as.dataframe(read.csv(input$csv1$datapath,header=TRUE, sep=","))
  
  Nstates
  Ntransitions
  x
  y
  boxwidth
  boxheight
  statenames
  transnames
  tmatnamed
  frequencies
  
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
  list_desc[[10]]=frequencies
  
  
  list_desc_final=list(Nstates= list_desc[[1]], Ntransitions= list_desc[[2]], 
                       xvalues= list_desc[[3]], yvalues=list_desc[[4]] ,
                       boxwidth=list_desc[[5]], boxheight= list_desc[[6]],
                       statenames=list_desc[[7]],transnames=list_desc[[8]],
                       tmat= list_desc[[9]], frequencies= list_desc[[10]]   
  )
  
  
  exportJson <- toJSON(list_desc_final, pretty = TRUE,force = TRUE, flatten=TRUE, na='string')
  
  exportJson
  

})



json2manual<-reactive  ({

  
  data<- read.csv(input$csv2$datapath,header=TRUE, sep=",")
  
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
  
  
  final_list[[ncol(data)+1]]=tmat
  names(final_list)[ncol(data)+1]<-c("tmat")
  
  
  final_list$timevar=as.vector(final_list$timevar)
  
  final_list$atlist=as.vector(final_list$atlist)
  
  exportJson <- toJSON(final_list, pretty = TRUE,force = TRUE, flatten=TRUE, na='string')
  
  exportJson
})




