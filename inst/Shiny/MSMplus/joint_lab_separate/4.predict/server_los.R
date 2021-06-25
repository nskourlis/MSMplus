###### Show and hide  and tick inputs ####
timerlos <- reactiveVal(1.5)


observeEvent(c(input$showticklos,invalidateLater(1000, session)), { 
  
  if(input$showticklos=="No"){
    
    hide("tickinputlos")
    
  }
  
  if(input$showticklos=="Yes"){
    
    show("tickinputlos")
    
  }
  
  isolate({
    
    timerlos(timerlos()-1)
    if(timerlos()>1 & input$showticklos=="No")
    {
      show("tickinputlos")
    }
    
  })
})



#############################################################

existlos <- reactive({
  if (length(myjson2()$los) != 0) {       
    x= 1
  }
  else if (length(myjson2()$los) == 0) {       
    x= 0
  }
})

existlosratio <- reactive({
  if (length(myjson2()$losr) != 0) {       
    x= 1
  }
  else if (length(myjson2()$losr) == 0) {       
    x= 0
  }
})

existlosdiff <- reactive({
  if (length(myjson2()$losd) != 0) {       
    x= 1
  }
  else if (length(myjson2()$losd) == 0) {       
    x= 0
  }
})


output$pagelos <- renderUI({
  
  if (is.null(myjson2())) return("Provide the json file with the predictions")
  
  if (existlos()==0) {
    
    fluidRow(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      column(12,
             output$loginpagelos <- renderUI({h1("Non applicable")})
      )
    )
  }
  
  else if (existlos()==1) {
    
    if (existlosdiff()==1 & existlosratio()==1) { 
      
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,
               h1("Length of stay"),
               
               conditionalPanel(condition="input.tabslos =='#panel1los'||input.tabslos =='#panel2los'||input.tabslos =='#panel4los'||input.tabslos =='#panel5los'",
                                uiOutput("facetlos")
               ),  
               uiOutput("conflos")
        ),
        
        column(2,
               br(),
               p(""),

               radioButtons("showticklos", "Show axis tick options",
                            choices = list("No" = "No",
                                           "Yes" = "Yes"), selected = "No"),
               uiOutput("tickinputlos")
               

               
        ),
        
        column(8,
               tabsetPanel(id = "tabslos",
                 
                 tabPanel(h2("By state"),    value = "#panel1los",                  plotlyOutput("los_state"  , height="600px", width = "100%"),uiOutput("shouldloadlos1")),
                 tabPanel(h2("By covariate pattern"),     value = "#panel2los",          plotlyOutput("los_cov"  , height="600px", width = "100%"),uiOutput("shouldloadlos2")),
                 tabPanel(h2("By state and covariate pattern"), value = "#panel3los", plotlyOutput("los_both"  , height="600px", width = "100%"),uiOutput("shouldloadlos3")),
                 tabPanel(h2("Differences"),       value = "#panel4los",           plotlyOutput("los_diff"  , height="600px", width = "100%"),uiOutput("shouldloadlos4")),
                 tabPanel(h2("Ratios"),          value = "#panel5los",             plotlyOutput("los_ratio"  , height="600px", width = "100%"),uiOutput("shouldloadlos5"))
               )
        )
      )

      
    }
    
   else if (existlosdiff()==1 & existlosratio()==0) { 
      
      
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,
               h1("Length of stay"),
               
               conditionalPanel(condition="input.tabslos =='#panel1los'||input.tabslos =='#panel2los'||input.tabslos =='#panel4los'||input.tabslos =='#panel5los'",
                                uiOutput("facetlos")
               )  ,  
               uiOutput("conflos")
        ),
        
        column(2,
               br(),
               p(""),
               radioButtons("showticklos", "Show axis tick options",
                            choices = list("No" = "No",
                                           "Yes" = "Yes"), selected = "No"),
               uiOutput("tickinputlos"),

               
        ),
        
        column(8,
               tabsetPanel(id = "tabslos",
                           
                           tabPanel(h2("By state"),    value = "#panel1los",                  plotlyOutput("los_state"  , height="600px", width = "100%"),uiOutput("shouldloadlos1")),
                           tabPanel(h2("By covariate pattern"),     value = "#panel2los",          plotlyOutput("los_cov"  , height="600px", width = "100%"),uiOutput("shouldloadlos2")),
                           tabPanel(h2("By state and covariate pattern"), value = "#panel3los", plotlyOutput("los_both"  , height="600px", width = "100%"),uiOutput("shouldloadlos3")),
                           tabPanel(h2("Differences"),       value = "#panel4los",           plotlyOutput("los_diff"  , height="600px", width = "100%"),uiOutput("shouldloadlos4")),
                           tabPanel(h2("Ratios"),          value = "#panel5los",            print("Not Applicable"))
               )
        )
      )
      
    }
    
    else if ( existlosdiff()==0 & existlosratio()==1) { 
      
      
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,
               h1("Length of stay"),
               
               conditionalPanel(condition="input.tabslos =='#panel1los'||input.tabslos =='#panel2los'||input.tabslos =='#panel4los'||input.tabslos =='#panel5los'",
                                uiOutput("facetlos")
               )  ,  
               uiOutput("conflos")
        ),
        
        column(2,
               br(),
               p(""),
               radioButtons("showticklos", "Show axis tick options",
                            choices = list("No" = "No",
                                           "Yes" = "Yes"), selected = "No"),
               uiOutput("tickinputlos"),

               
        ),
        
        column(8,
               tabsetPanel(id = "tabslos",
                           
                           tabPanel(h2("By state"),    value = "#panel1los",                  plotlyOutput("los_state"  , height="600px", width = "100%"),uiOutput("shouldloadlos1")),
                           tabPanel(h2("By covariate pattern"),     value = "#panel2los",          plotlyOutput("los_cov"  , height="600px", width = "100%"),uiOutput("shouldloadlos2")),
                           tabPanel(h2("By state and covariate pattern"), value = "#panel3los", plotlyOutput("los_both"  , height="600px", width = "100%"),uiOutput("shouldloadlos3")),
                           tabPanel(h2("Differences"),       value = "#panel4los",          print("Not Applicable")),
                           tabPanel(h2("Ratios"),          value = "#panel5los",             plotlyOutput("los_ratio"  , height="600px", width = "100%"),uiOutput("shouldloadlos5"))
               )
        )
      )
      
    }
    
    else if (existlosdiff()==0 & existlosratio()==0) { 
      
      
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,
               h1("Length of stay"),
               
               

               #uiOutput("displaylos"),
               #uiOutput("covarinputlos"),
              # uiOutput("statesinputlos")
              conditionalPanel(condition="input.tabslos =='#panel1los'||input.tabslos =='#panel2los'||input.tabslos =='#panel4los'||input.tabslos =='#panel5los'",
                               uiOutput("facetlos")
              )  ,  
              uiOutput("conflos")
        ),
        
        column(2,
               br(),
               p(""),
               radioButtons("showticklos", "Show axis tick options",
                            choices = list("No" = "No",
                                           "Yes" = "Yes"), selected = "No"),
               uiOutput("tickinputlos"),

               
        ),
        
        column(8,
               tabsetPanel(id = "tabslos",
                           
                           tabPanel(h2("By state"),    value = "#panel1los",                  plotlyOutput("los_state"  , height="600px", width = "100%"),uiOutput("shouldloadlos1")),
                           tabPanel(h2("By covariate pattern"),     value = "#panel2los",          plotlyOutput("los_cov"  , height="600px", width = "100%"),uiOutput("shouldloadlos2")),
                           tabPanel(h2("By state and covariate pattern"), value = "#panel3los", plotlyOutput("los_both"  , height="600px", width = "100%"),uiOutput("shouldloadlos3")),
                           tabPanel(h2("Differences"),       value = "#panel4los",           print("Not Applicable")),
                           tabPanel(h2("Ratios"),          value = "#panel5los",             print("Not Applicable"))
               )
        )
      )
      
      
    }
  }
  
})    

observeEvent(input$json2, {
  if( length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'Los')))==0  ) {
    js$disableTab("mytab_los")
    
  } 
}) 


observeEvent(input$csv2, {
  if( length(which(startsWith(names( read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'Los')))==0 ) {
    js$disableTab("mytab_los")
    
  } 
}) 



#output$displaylos <- renderUI({
#  radioButtons("displaylos", "Change labels of states and covariate patterns",
#               c("same" = "same", "change"= "change"))
#})
#
output$facetlos <- renderUI({
radioButtons(inputId="facetlos", label= "Display graph in grids",
             choices=c("No","Yes"),selected = "No")
}) 

output$conflos <- renderUI({
  
  if  (length(myjson2()$ci_los)!=0) {
  radioButtons("conflos", "Confidence intervals",
               c("No" = "ci_no",
                 "Yes" ="ci_yes"))
  }
  
  else if (length(myjson2()$ci_los)==0) {
    item_list <- list()
    item_list[[1]]<- radioButtons("conflos", "Confidence intervals",c("No" = "ci_no"))
    item_list[[2]]<-print("Confidence interval data were not provided")
    do.call(tagList, item_list)
  }

})

#########################################################################
#########################################################################
#Create the reactive input of covariates
#########################################################################
#########################################################################

#output$covarinputlos <- renderUI({
#  
#  if (is.null(myjson2()))  return()
#  
#  if (input$displaylos=="same") return()
#  
#  else {
#  
#  item_list <- list()
#  item_list[[1]] <- h2("Covariate patterns")
#  
#  v=vector()
#  for (i in 1:length(myjson2()$cov$atlist)) {
#    v[i]=myjson2()$cov$atlist[i]
#  }
#  default_choices_cov=v
#  
#  for (i in seq(length(myjson2()$cov$atlist))) {
#    item_list[[i+1]] <- textInput(paste0('covlos', i),default_choices_cov[i], labels_cov()[i])
#  }
#  do.call(tagList, item_list)
#  }
#})
#
##Transform the reactive input of covariates into easy to use laber reactive dataset
#
#labels_covl<- reactive ({
#  
#  if (input$displaylos=="same") {labels_cov()}
#  
#  else {
#  
#  myList<-vector("list",length(myjson2()$cov$atlist))
#  for (i in 1:length(myjson2()$cov$atlist)) {
#    myList[[i]]= input[[paste0('covlos', i)]][1]
#  }
#  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
#  final_list
#  }
#})
#

#####################################
#Create the reactive input of states#
######################################
#output$statesinputlos <- renderUI({
#  
#  if (input$displaylos=="same") return()
#  
#  else {
#  
#  item_list <- list()
#  item_list[[1]] <- h2("States")
#  default_choices_state=vector()
#  
#  title_choices_state=vector()
#  for (i in 1:length(myjson2()$P)) {
#    title_choices_state[i]=paste0("State"," ",input$select,selectend()[i])
#  }
#  for (i in 1:length(myjson2()$P)) {
#    item_list[[1+i]] <- textInput(paste0('statel',i),title_choices_state[i], labels_state()[i])
#  }
#  do.call(tagList, item_list)
#}
#  })
#
#labels_statel<- reactive ({
#  
#  if (input$displaylos=="same") {labels_state()}
#  
#  else {
#  
#  myList<-vector("list",length(myjson2()$P))
#  for (i in 1:length(myjson2()$P)) {
#    
#    myList[[i]]= input[[paste0('statel', i)]][1]
#    
#  }
#  
#  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
#  final_list
#  }
#})
#
#########################################################################
#########################################################################
#########################################################################


data_L <- reactive ({
  if(is.null(myjson2())) 
    return()
  
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups

  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {
    v[i]=myjson2()$cov$atlist[i]
  }
  
  
  ## Different variable of probabilities for each covariate pattern
  ## Different variable of probabilities for each covariate pattern
  los=list()
  
  if (length(myjson2()$los)==0) {return()}
  
  for(i in 1:length(myjson2()$los)) {
    
    los[[i]]=as.data.frame(t(data.frame(myjson2()$los[i])))
    colnames(los[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$los)) {  
    los[[i]]=as.data.frame(cbind(los[[i]], timevar ,state=rep(i,nrow(los[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_los=list()
  data_los[[1]]=los[[1]]
  
  for (u in 2:(length(myjson2()$los))) {
    data_los[[u]]=rbind(los[[u]],data_los[[(u-1)]])
  }
  
  datal=data_los[[length(myjson2()$los)]]

  
  datal
})

data_L_uci <- reactive ({
  if(is.null(myjson2())) return()
  
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {v[i]=myjson2()$cov$atlist[i]}
  
  ## Different variable of probabilities for each covariate pattern
  los_uci=list()
  
  if (length(myjson2()$los_uci)==0) {return()}
  
  for(i in 1:length(myjson2()$los_uci)) {
    los_uci[[i]]=as.data.frame(t(data.frame(myjson2()$los_uci[i])))
    colnames(los_uci[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$los_uci)) {  
    los_uci[[i]]=as.data.frame(cbind(los_uci[[i]], timevar ,state=rep(i,nrow(los_uci[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_los_uci=list()
  data_los_uci[[1]]=los_uci[[1]]
  
  for (u in 2:(length(myjson2()$los_uci))) {
    data_los_uci[[u]]=rbind(los_uci[[u]],data_los_uci[[(u-1)]])
  }
  datal_uci=data_los_uci[[length(myjson2()$los_uci)]]
  datal_uci 
})

data_L_lci <- reactive ({
  if(is.null(myjson2())) return()
  
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {v[i]=myjson2()$cov$atlist[i]}
  
  ## Different variable of probabilities for each covariate pattern
  los_lci=list()
  
  if (length(myjson2()$los_lci)==0) {return()}
  
  for(i in 1:length(myjson2()$los_lci)) {
    los_lci[[i]]=as.data.frame(t(data.frame(myjson2()$los_lci[i])))
    colnames(los_lci[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$los_lci)) {  
    los_lci[[i]]=as.data.frame(cbind(los_lci[[i]], timevar ,state=rep(i,nrow(los_lci[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_los_lci=list()
  data_los_lci[[1]]=los_lci[[1]]
  
  for (u in 2:(length(myjson2()$los_lci))) {
    data_los_lci[[u]]=rbind(los_lci[[u]],data_los_lci[[(u-1)]])
  }
  datal_lci=data_los_lci[[length(myjson2()$los_lci)]]
  datal_lci 
})

data_L_st<-reactive  ({
  
  datanew=data_L()
  datanew$state_fac=ordered(c(rep("NA",nrow(datanew))), levels = labels_state() )
  
  for (o in 1:(length(myjson2()$P))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$state[g]==o) {datanew$state_fac[g]=labels_state()[o] }  
    }
  }
  datanew
})

data_L_st_uci<-reactive  ({
  datanew=data_L_uci()
  datanew$state_fac=ordered(c(rep("NA",nrow(datanew))), levels = labels_state() )
  
  for (o in 1:(length(myjson2()$los_uci))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$state[g]==o) {datanew$state_fac[g]=labels_state()[o] }  
    }
  }
  datanew
})

data_L_st_lci<-reactive  ({
  datanew=data_L_lci()
  datanew$state_fac=ordered(c(rep("NA",nrow(datanew))), levels = labels_state() )
  
  for (o in 1:(length(myjson2()$los_lci))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$state[g]==o) {datanew$state_fac[g]=labels_state()[o]}  
    }
  }
  datanew
})

data_L_d <- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_L_st()[,d],data_L_st()[,ncol(data_L_st())-2],data_L_st()[,ncol(data_L_st())-1],data_L_st()[,ncol(data_L_st())],rep(d,length(data_L_st()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_L_st())[d],length(data_L_st()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_l <- bind_rows(dlist, .id = "column_label")
  d_all_l
  
  
})


data_L_d_uci <- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_L_st_uci()[,d],data_L_st_uci()[,ncol(data_L_st_uci())-2],
                                data_L_st_uci()[,ncol(data_L_st_uci())-1],
                                data_L_st_uci()[,ncol(data_L_st_uci())],rep(d,length(data_L_st_uci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_L_st_uci())[d],length(data_L_st_uci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  d_all_l_uci <- bind_rows(dlist, .id = "column_label")
  d_all_l_uci
}) 


data_L_d_lci <- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_L_st_lci()[,d],data_L_st_lci()[,ncol(data_L_st_lci())-2],
                                data_L_st_lci()[,ncol(data_L_st_lci())-1],
                                data_L_st_lci()[,ncol(data_L_st_lci())],rep(d,length(data_L_st_lci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_L_st_lci())[d],length(data_L_st_lci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  d_all_l_lci <- bind_rows(dlist, .id = "column_label")
  d_all_l_lci
}) 


output$tickinputlos <- renderUI({
  
  default_choices=c("black","blue1","brown1","chartreuse2","cyan1","darkgray","firebrick3",
                    "gold","darkorange2","lightsteelblue4","rosybrow2","violetred2",
                    "yellow2","yellowgreen","tan1","lightslateblue","khaki4")
  
  if (is.null(myjson2()))  return()
  item_list <- list()
  item_list[[1]] <- h2("Provide x axis range and ticks")
  item_list[[2]] <-numericInput("startlosx","Start x at:",value=min(data_L_d()$timevar),min=0 )
  item_list[[3]] <-numericInput("steplosx","step:",value=max(data_L_d()$timevar/10),min=0,max=max(data_L_d()$timevar))
  item_list[[4]] <-numericInput("endlosx","End x at:",value =max(data_L_d()$timevar),min=0,max=max(data_L_d()$timevar))
  item_list[[5]] <-numericInput("steplosy","step at y axis:",value=max(data_L_d()$V)/10,min=max(data_L_d()$V)/100,max=max(data_L_d()$V)/2)
  item_list[[6]] <-numericInput("endlosy","end of y axis:",value=max(data_L_d()$V),min=max(data_L_d()$V)/10,max=max(data_L_d()$V)*10)
  item_list[[7]] <-numericInput("textsizelos",h2("Legends size"),value=input$textsize,min=5,max=30)
  item_list[[8]] <-numericInput("textfacetlos",h2("Facet title size"),value=input$textsize-3,min=5,max=30 )
  
  
  do.call(tagList, item_list)
})


data_L_ci<- reactive ({
  x=c( data_L_d()[order(data_L_d()$timevar,data_L_d()$state,data_L_d()$cov),]$timevar,
       data_L_d_lci()[order(-data_L_d()$timevar,data_L_d()$state,data_L_d()$cov),]$timevar )
  
  y_central=c( data_L_d()[order(data_L_d()$timevar,data_L_d()$state,data_L_d()$cov),]$V,
               data_L_d()[order(-data_L_d()$timevar,data_L_d()$state,data_L_d()$cov),]$V )
  
  y=c( data_L_d_uci()[order(data_L_d_uci()$timevar,data_L_d_uci()$state,data_L_d_uci()$cov),]$V,
       data_L_d_lci()[order(-data_L_d_uci()$timevar,data_L_d_uci()$state,data_L_d_uci()$cov),]$V )
  
  frameto=c(as.character(data_L_d_uci()[order(-data_L_d_uci()$timevar,data_L_d_uci()$state,data_L_d_uci()$cov),]$state_factor),
            as.character(data_L_d_lci()[order(-data_L_d_lci()$timevar,data_L_d_lci()$state,data_L_d_lci()$cov),]$state_factor) )
  
  covto=c( data_L_d_uci()[order(-data_L_d_uci()$timevar,data_L_d_uci()$state,data_L_d_uci()$cov),]$cov_factor,
           data_L_d_lci()[order(-data_L_d_lci()$timevar,data_L_d_lci()$state,data_L_d_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,frameto,covto,y_central)
  data
})


######################################
#output$shouldloadlos1 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotlos1", label = h2("Download the plot"))
#})

datalos1_re <-  reactive ({



  
  
  ####### Plot 1 frame is state, factor is cov ########################
  
  
  if (input$conflos=="ci_no") {
    
    if (input$facetlos=="No") {
      
      
      los_state= plot_ly(data_L_d(),alpha=0.5) %>%
        add_lines(
          x=data_L_d()$timevar,y=data_L_d()$V,
          frame=factor(as.factor(data_L_d()$state_factor),levels = labels_state()),
          color=factor(as.factor(data_L_d()$cov_factor),levels = labels_cov()),
          colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
          mode="lines",
          line=list(simplify=FALSE,color = labels_colour_cov()) ,
          text = 'Select or deselect lines by clicking on the legend',
          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")   )
      
      
      los_state = los_state %>%
        layout(title=list(text="Length of stay of each covariate pattern among states",y=0.95),
               font= list(family = "times new roman", size = input$textsizelos, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$steplosx, 
                          tick0 = input$startlosx, 
                          range=c(input$startlosx,input$endlosx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Length of stay",rangemode = "nonnegative",  
                           range=c(0,input$endlosy),
                           dtick = input$steplosy, 
                           ticklen = 5,
                           tickwidth = 2,
                           tickcolor = toRGB("black")),
               shapes = list(
                 list(type = "rect",
                      fillcolor = "grey", 
                      line = list(color = "grey"), 
                      opacity = 0.8,
                      x0 = 0, x1 =0, xref = "x", y0 = 0, y1 = 1, yref = "y") )  )%>%
        animation_opts(frame = 1000, transition = 0, redraw = FALSE)%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        )
      
      los_state
      
    }
    
    
    if (input$facetlos=="Yes") {
      
      
      data_plot=data_L_d()
      
      los_state = ggplot(data_plot)
      los_state = ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(cov_factor),levels=labels_cov()), group=1,
                                       text=paste("Select or deselect lines by clicking on the legend",
                                                  "<br>Time: ", timevar,
                                                  "<br>Length of stay: ", V,
                                                  "<br>Covariate pattern: ", factor(as.factor(cov_factor),levels=labels_cov()))))
      
      los_state = los_state+geom_line(aes(x=timevar, y=V, color=factor(as.factor(cov_factor),levels=labels_cov())))+
        scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      if (input$aimtype=="compare") { los_state = los_state+ facet_wrap(~ factor(as.factor(state_factor),levels = labels_state()), nrow=2) }
      
      else if (input$aimtype=="present") {los_state = los_state+ facet_wrap(~factor(as.factor(state_factor),levels = labels_state() ))}
      
      los_state = los_state + scale_x_continuous(breaks=c(seq(input$startlosx,input$endlosx,by=input$steplosx  ))) +
                              scale_y_continuous(breaks=c(seq(0,input$endlosy,by=input$steplosy  )))
      
      los_state = los_state +labs(title="Length of stay of each covariate pattern among states", x="Time since entry", y="Length of stay")
      
      los_state = los_state + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      los_state = los_state +theme(title = element_text(size = input$textsizelos-4),  strip.text = element_text(size=input$textfacetlos),        
                           legend.title = element_text(color="black", size= input$textsizelos-5), 
                           legend.text=element_text(size= input$textsizelos-6),
                           plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                           legend.margin = margin(1.5, 1, 1, 1, "cm"),
                           legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                           axis.title.y = element_text(size= input$textsizelos-5),
                           axis.title.x = element_text(size= input$textsizelos-5), 
                           axis.text.x = element_text( size=input$textsizelos-6),axis.text.y = element_text( size=input$textsizelos-6))  
      
      los_state = ggplotly(los_state, tooltip = "text")%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        ) 
      
      los_state 
    }
    
  }
  
  else if (input$conflos=="ci_yes") {
    
    if (input$facetlos=="No") {
      
      los_state <- plot_ly()
      
      los_state <- add_trace(los_state, line=list(simplify=FALSE,color = labels_colour_cov()),
                           mode="lines", type = "scatter",
                           x=data_L_ci()$x, y=data_L_ci()$y_central,
                           frame=factor(as.factor(data_L_ci()$frameto),levels= labels_state()), 
                           colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
                           color=factor(as.factor(data_L_ci()$covto),levels = labels_cov()),
                           text = 'Select or deselect lines by clicking on the legend',
                           hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                 "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") )
      
      los_state <- add_trace(los_state, fill = "tozerox", 
                           line=list(dash = "solid", color = "transparent", width = 1.8897637),
                           mode = "lines", type = "scatter",
                           x=data_L_ci()$x, y=data_L_ci()$y,
                           frame=factor(as.factor(data_L_ci()$frameto),levels= labels_state()), 
                           colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
                           color=factor(as.factor(data_L_ci()$covto),levels = labels_cov()),
                           showlegend = FALSE,
                           text = 'Select or deselect lines by clicking on the legend',
                           hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                 "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      
      los_state = los_state %>%
        layout(title=list(text="Length of stay of each covariate pattern among states",y=0.95),
               font= list(family = "times new roman", size = input$textsizelos, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$steplosx, 
                          tick0 = input$startlosx, 
                          range=c(input$startlosx,input$endlosx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Length of stay",rangemode = "nonnegative",
                           range=c(0,input$endlosy),
                           dtick = input$steplosy, 
                           ticklen = 5,
                           tickwidth = 2,
                           tickcolor = toRGB("black")),
               shapes = list(
                 list(type = "rect",
                      fillcolor = "grey", 
                      line = list(color = "grey"), 
                      opacity = 0.8,
                      x0 = 0, x1 =0, xref = "x", y0 = 0, y1 = 1, yref = "y") )  )%>%
        animation_opts(frame = 1000, transition = 0, redraw = FALSE)%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        )
      
      los_state
    }
    
    if (input$facetlos=="Yes") {
      
      V_lci=  data_L_d_lci()$V
      V_uci=  data_L_d_uci()$V
      
      data_plot=cbind(data_L_d(),V_lci,V_uci)
      
      
      los_state=ggplot(data_plot)
      los_state=ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(cov_factor),levels=labels_cov()), group=1,
                                     text=paste("Select or deselect lines by clicking on the legend",
                                                "<br>Time: ", timevar,
                                                "<br>Length of stay: ", V,
                                                "<br>Covariate pattern: ", factor(as.factor(cov_factor),levels=labels_cov()))))+
        scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      
      los_state=los_state+geom_line(aes(x=timevar, y=V, fill= factor(as.factor(cov_factor),levels=labels_cov())))
      
      los_state=los_state+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill=factor(as.factor(cov_factor),levels=labels_cov())),alpha=0.4)+ 
        scale_fill_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      
      if (input$aimtype=="compare") { los_state = los_state+ facet_wrap(~ factor( as.factor(state_factor), levels = labels_state() ), nrow=2)}
      
      else if (input$aimtype=="present")   {los_state = los_state+ facet_wrap(~ factor( as.factor(state_factor), levels = labels_state() ) ) }

      los_state = los_state + scale_x_continuous(breaks=c(seq(input$startlosx,input$endlosx,by=input$steplosx  ))) +
                              scale_y_continuous(breaks=c(seq(0,input$endlosy,by=input$steplosy  )))
      
      los_state = los_state +labs(title="Length of stay at each state", x="Time since entry", y="Length of stay")
      
      los_state = los_state + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      los_state = los_state  +theme(title = element_text(size = input$textsizelos-4),   strip.text = element_text(size=input$textfacetlos),       
                            legend.title = element_text(color="black", size= input$textsizelos-5), 
                            legend.text=element_text(size= input$textsizelos-6),
                            plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                            legend.margin = margin(1.5, 1, 1, 1, "cm"),
                            legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                            axis.title.y = element_text(size= input$textsizelos-5),
                            axis.title.x = element_text(size= input$textsizelos-5), 
                            axis.text.x = element_text( size=input$textsizelos-6),axis.text.y = element_text( size=input$textsizelos-6))  
      
      los_state = ggplotly(los_state, tooltip = "text")%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        ) 
      
      los_state 
      
    }
  }
  los_state  
})

output$los_state <- renderPlotly ({datalos1_re() })

output$downplotlos1 <- downloadHandler(
  filename = function(){paste("los1",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datalos1_re(),width = 1400, height = 1100, format = "png", scale = 2,  out_file = file )
  }
)
##########################################################

#output$shouldloadlos2 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotlos2", label = h2("Download the plot"))
#})

datalos2_re <-  reactive({


  
  if (input$conflos=="ci_no") {
    
    if (input$facetlos=="No") {
      
      
      los_cov= plot_ly(data_L_d(),alpha=0.5) %>%
        add_lines(
          x=data_L_d()$timevar,y=data_L_d()$V,
          frame=factor(as.factor(data_L_d()$cov_factor), levels = labels_cov() ),
          color=factor(as.factor(data_L_d()$state_factor),levels = labels_state()),
          colors=labels_colour_state()[1:length(myjson2()$P)],
          mode="lines",
          line=list(simplify=FALSE,color = labels_colour_state()) ,
          text = 'Select or deselect lines by clicking on the legend',
          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")   )
      
      
      los_cov = los_cov %>%
        layout(title=list(text="Length of stay of each state among covariate patterns",y=0.95),
               font= list(family = "times new roman", size = input$textsizelos, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$steplosx, 
                          tick0 = input$startlosx, 
                          range=c(input$startlosx,input$endlosx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Length of stay",rangemode = "nonnegative",    
                           range=c(0,input$endlosy),
                           dtick = input$steplosy, 
                           ticklen = 5,
                           tickwidth = 2,
                           tickcolor = toRGB("black")),
               shapes = list(
                 list(type = "rect",
                      fillcolor = "grey", 
                      line = list(color = "grey"), 
                      opacity = 0.8,
                      x0 = 0, x1 =0, xref = "x", y0 = 0, y1 = 1, yref = "y") )  )%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        )
      
      
      if (input$smooth=="No") {
        los_cov= los_cov %>%
          animation_opts(frame = 1000, transition = 0, redraw = FALSE)
      }
      
      los_cov
      
    }
    
    
    if (input$facetlos=="Yes") {
      
      
      data_plot=data_L_d()
      
      los_cov = ggplot(data_plot)
      los_cov = ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(state_factor),levels=labels_state()), group=1,
                                     text=paste("Select or deselect lines by clicking on the legend",
                                                "<br>Time: ", timevar,
                                                "<br>Length of stay: ", V,
                                                "<br>State: ",  factor(as.factor(state_factor),levels=labels_state()))))
      
      los_cov = los_cov+geom_line(aes(x=timevar, y=V, color= factor(as.factor(state_factor),levels=labels_state())))+
        scale_colour_manual( values =labels_colour_state(),labels = labels_state()  ) 
      
      if (input$aimtype=="compare") {los_cov = los_cov+ facet_wrap(~factor(as.factor(cov_factor), levels = labels_cov() ), nrow=2)}
      
      else if (input$aimtype=="present")   {los_cov = los_cov+ facet_wrap(~factor(as.factor(cov_factor), levels = labels_cov() ))}
      
 
      los_cov = los_cov + scale_x_continuous(breaks=c(seq(input$startlosx,input$endlosx,by=input$steplosx  )))  +
                                             scale_y_continuous(breaks=c(seq(0,input$endlosy,by=input$steplosy  )))
      
      los_cov = los_cov +labs(title="Length of stay of each state among covariate patterns", x="Time since entry", y="Length of stay")
      
      los_cov = los_cov + labs(color = "States")+ labs(fill = "States")
      
      los_cov = los_cov  +theme(title = element_text(size = input$textsizelos-4), strip.text = element_text(size=input$textfacetlos),         
                        legend.title = element_text(color="black", size= input$textsizelos-5), 
                        legend.text=element_text(size= input$textsizelos-6),
                        plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                        legend.margin = margin(1.5, 1, 1, 1, "cm"),
                        legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                        axis.title.y = element_text(size= input$textsizelos-5),
                        axis.title.x = element_text(size= input$textsizelos-5), 
                        axis.text.x = element_text( size=input$textsizelos-6),axis.text.y = element_text( size=input$textsizelos-6))  
      
      los_cov = ggplotly(los_cov, tooltip = "text")%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        ) 
      
      los_cov 
    }
    
  }
  
  else if (input$conflos=="ci_yes") {
    
    if (input$facetlos=="No") {
      
      los_cov <- plot_ly()
      
      los_cov <- add_trace(los_cov, line=list(simplify=FALSE,color = labels_colour_cov()),
                         mode="lines", type = "scatter",
                         x=data_L_ci()$x, y=data_L_ci()$y_central,
                         frame= factor( as.factor(data_L_ci()$covto), levels = labels_cov() ), 
                         colors=labels_colour_state()[1:length(myjson2()$P)],
                         color=factor(as.factor(data_L_ci()$frameto),levels = labels_state()) ,
                         text = 'Select or deselect lines by clicking on the legend',
                         hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                               "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      los_cov <- add_trace(los_cov, fill = "tozerox", 
                         line=list(dash = "solid", color = "transparent", width = 1.8897637),
                         mode = "lines", type = "scatter",
                         x=data_L_ci()$x, y=data_L_ci()$y,
                         frame= factor( as.factor(data_L_ci()$covto), levels = labels_cov() ),  
                         colors=labels_colour_state()[1:length(myjson2()$P)],
                         color=factor(as.factor(data_L_ci()$frameto),levels = labels_state()) ,
                         showlegend = FALSE,
                         text = 'Select or deselect lines by clicking on the legend',
                         hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                               "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      
      los_cov = los_cov %>%
        layout(title=list(text="Length of stay of each state among covariate patterns",y=0.95),
               font= list(family = "times new roman", size = input$textsizelos, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$steplosx, 
                          tick0 = input$startlosx, 
                          range=c(input$startlosx,input$endlosx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Length of stay",rangemode = "nonnegative",                    
                           dtick = input$steplosy, 
                           ticklen = 5,
                           tickwidth = 2,
                           tickcolor = toRGB("black")),
               shapes = list(
                 list(type = "rect",
                      fillcolor = "grey", 
                      line = list(color = "grey"), 
                      opacity = 0.8,
                      x0 = 0, x1 =input$area, xref = "x", y0 = 0, y1 = 1, yref = "y") )  )%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        )
      
      if (input$smooth=="No") {
        los_cov= los_cov %>%
          animation_opts(frame = 1000, transition = 0, redraw = FALSE)
      }
      
      los_cov
    }
    
    if (input$facetlos=="Yes") {
      
      V_lci=  data_L_d_lci()$V
      V_uci=  data_L_d_uci()$V
      
      data_plot=cbind(data_L_d(),V_lci,V_uci)
      
      
      los_cov=ggplot(data_plot)
      los_cov=ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(state_factor),levels=labels_state()), group=1,
                                   text=paste("Select or deselect lines by clicking on the legend",
                                              "<br>Time: ", timevar,
                                              "<br>Length of stay: ", V,
                                              "<br>State: ",   factor(as.factor(state_factor),levels=labels_state()))))+
        scale_colour_manual( values =labels_colour_state(),labels = labels_state()  ) 
      
      
      los_cov=los_cov+geom_line(aes(x=timevar, y=V, fill= factor(as.factor(state_factor),levels=labels_state()))) +
        scale_y_continuous(breaks=c(seq(0,input$endlosy,by=input$steplosy  )))
      
      los_cov=los_cov+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill= factor(as.factor(state_factor),levels=labels_state())),alpha=0.4)+ 
        scale_fill_manual( values =labels_colour_state(),labels = labels_state()  ) 
      
      if (input$aimtype=="compare") {los_cov = los_cov+ facet_wrap(~factor(as.factor(cov_factor), levels = labels_cov() ), nrow=2)}
      
      else if (input$aimtype=="present")   {los_cov = los_cov+ facet_wrap(~~factor(as.factor(cov_factor), levels = labels_cov() ) )}
      
      
      los_cov = los_cov + scale_x_continuous(breaks=c(seq(input$startlosx,input$endlosx,by=input$steplosx  ))) 
      los_cov = los_cov +labs(title="Length of stay of each state among covariate patterns", x="Time since entry", y="Length of stay")
      
      los_cov = los_cov + labs(color = "States")+ labs(fill = "States")
      
      los_cov = los_cov +theme(title = element_text(size = input$textsizelos-4),  strip.text = element_text(size=input$textfacetlos),        
                       legend.title = element_text(color="black", size= input$textsizelos-5), 
                       legend.text=element_text(size= input$textsizelos-6),
                       plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                       legend.margin = margin(1.5, 1, 1, 1, "cm"),
                       legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                       axis.title.y = element_text(size= input$textsizelos-5,hjust=2),
                       axis.title.x = element_text(size= input$textsizelos-5), 
                       axis.text.x = element_text( size=input$textsizelos-6),axis.text.y = element_text( size=input$textsizelos-6))  
      
      los_cov = ggplotly(los_cov, tooltip = "text")%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        ) 
      
      los_cov 
      
    }
  }
  los_cov 
})

output$los_cov <- renderPlotly ({datalos2_re() })
  

output$downplotlos2 <- downloadHandler(
  filename = function(){paste("los2",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datalos2_re(),width = 1400, height = 1100, format = "png", scale = 2,  out_file = file )
  }
)
##################################################

#output$shouldloadlos3 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotlos3", label = h2("Download the plot"))
#})

datalos3_re <-  reactive({ 

  
  ####### Plot 3 f factor is state and cov ########################
  
  
  if (input$conflos=="ci_no") {
    
   los_state_cov= plot_ly(data_L_d(),alpha=0.5) %>%
                       add_lines(
                             x=data_L_d()$timevar,y=data_L_d()$V,
                             color = as.factor(data_L_d()$cov_factor),
                             fill =data_L_d()$state_factor,
                             linetype=data_L_d()$state_factor,
                             mode="lines",
                             line=list(simplify=FALSE),
                             text = 'Select or deselect lines by clicking on the legend',
                             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
                             )
  }
  
  else if (input$conflos=="ci_yes") {
    
    los_state_cov <- plot_ly()
    
    los_state_cov  <- add_trace(los_state_cov, line=list(simplify=FALSE),
                              mode="lines", type = "scatter",
                              x=data_L_ci()$x, y=data_L_ci()$y_central,
                              color=as.factor(data_L_ci()$covto), 
                              fill=as.factor(data_L_ci()$frameto),
                              linetype=as.factor(data_L_ci()$frameto),
                              text = 'Select or deselect lines by clicking on the legend',
                              hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                    "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
    
    los_state_cov  <- add_trace(los_state_cov, fill = "tozerox", 
                              line=list(dash = "solid", color = "transparent", width = 1.8897637),
                              mode = "lines", type = "scatter",
                              x=data_L_ci()$x, y=data_L_ci()$y,
                              color=as.factor(data_L_ci()$covto),
                              fill=as.factor(data_L_ci()$frameto),
                              linetype=as.factor(data_L_ci()$frameto),
                              showlegend = FALSE,
                              text = 'Select or deselect lines by clicking on the legend',
                              hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                    "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
    ) 
  }
    los_state_cov=los_state_cov%>%
      layout(title=list(text="Length of stay in each state for all covariate patterns",y=0.95),
             font= list(family = "times new roman", size = input$textsizelos, color = "black"),
             margin = list(l = 50, r = 50, b = 30, t = 70),
             xaxis=list(title=list(text="Time since entry",y=0.2),
                        dtick = input$steplosx, 
                        tick0 = input$startlosx, 
                        range=c(input$startlosx,input$endlosx),
                        ticklen = 5,
                        tickwidth = 2,
                        tickcolor = toRGB("black"),
                        tickmode = "linear"),
             yaxis =list(title= "Probability of state occupancy",rangemode = "nonnegative",                    
                         dtick = input$steplosy, 
                         ticklen = 5,
                         tickwidth = 2,
                         tickcolor = toRGB("black")),
             shapes = list(
                        list(type = "rect",
                        fillcolor = "grey", 
                        line = list(color = "grey"), 
                        opacity = 0.8,
                        x0 = 0, x1 =0, xref = "x", y0 = 0, y1 = 1, yref = "y") ) )%>%
      config(
        toImageButtonOptions = list(
          format = "png",
          width = 1200,
          height = 900,scale=input$figscale
        ), edits = list(
          annotationPosition = TRUE,
          annotationTail = TRUE,
          annotationText = TRUE,
          axisTitleText=TRUE,
          colorbarTitleText=TRUE,
          legendPosition=TRUE,
          legendText=TRUE,
          shapePosition=TRUE,
          titleText=TRUE
        ) ,queueLength=10
      ) 
    
      
  
  los_state_cov
  

}) 

output$los_both <- renderPlotly ({ datalos3_re() })
  
output$downplotlos3 <- downloadHandler(
  filename = function(){paste("los3",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datalos3_re(),width = 1400, height = 1100, format = "png", scale = 2,  out_file = file )
  }
)
########################################################################################################
##########################################################################################################
data_L_diff1 <- reactive ({
  los_diff=list()
  
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_diff= vector()
  
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$losd)) {
      los_diff[[i]]=as.data.frame(t(data.frame(myjson2()$losd[i])))
      colnames(los_diff[[i]]) <- v_diff
    }
    for(i in 1:length(myjson2()$losd)) {  
      los_diff[[i]]=as.data.frame(cbind(los_diff[[i]], timevar ,state=rep(i,nrow(los_diff[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$losd)) {
      los_diff[[i]]=as.data.frame(myjson2()$losd[[i]][,1])
    }
    for (i in 1:length(myjson2()$losd)) {  
      los_diff[[i]]=as.data.frame(c(los_diff[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$losd[[i]][,1])) )) )
      colnames(los_diff[[i]])[1:(length(myjson2()$atlist)-1)] <- v_diff
    }
  }

  # Append the probabilities datasets of the different states
  data_losd=list()
  data_losd[[1]]=los_diff[[1]]
  
  for (u in 2:(length(myjson2()$losd))) {
    data_losd[[u]]=rbind(los_diff[[u]],data_losd[[(u-1)]])
  }
  
  datald=data_losd[[length(myjson2()$losd)]]
  datald$state_fac=c(rep("NA",nrow(datald)))
  
  for (o in 1:(length(myjson2()$losd))) {
    for (g in 1:nrow(datald))  {
      if  (datald$state[g]==o) {datald$state_fac[g]=labels_state()[o]}  
    }
  }
  datald
}) 

data_L_diff1_uci <- reactive ({
  L_diff_uci=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_diff= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$losd_uci)) {
      L_diff_uci[[i]]=as.data.frame(t(data.frame(myjson2()$losd_uci[i])))
      colnames(L_diff_uci[[i]]) <- v_diff
    }
    for(i in 1:length(myjson2()$losd_uci)) {  
      L_diff_uci[[i]]=as.data.frame(cbind(L_diff_uci[[i]], timevar ,state=rep(i,nrow(L_diff_uci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$losd_uci)) {
      L_diff_uci[[i]]=as.data.frame(myjson2()$losd_uci[[i]][,1])
    }
    for (i in 1:length(myjson2()$losd_uci)) {  
      L_diff_uci[[i]]=as.data.frame(c(L_diff_uci[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$losd_uci[[i]][,1])) )) )
      colnames(L_diff_uci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_diff
    }
  }
  
  # Append the probabilities datasets of the different states
  data_losd_uci=list()
  data_losd_uci[[1]]=L_diff_uci[[1]]
  
  for (u in 2:(length(myjson2()$losd_uci))) {
    data_losd_uci[[u]]=rbind(L_diff_uci[[u]],data_losd_uci[[(u-1)]])
  }
  
  datald_uci=data_losd_uci[[length(myjson2()$losd_uci)]]
  datald_uci$state_fac=c(rep("NA",nrow(datald_uci)))
  
  for (o in 1:(length(myjson2()$losd_uci))) {
    for (g in 1:nrow(datald_uci))  {
      if  (datald_uci$state[g]==o) {datald_uci$state_fac[g]=labels_state()[o]}  
    }
  }
  datald_uci
}) 

data_L_diff1_lci <- reactive ({
  L_diff_lci=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_diff= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$losd_lci)) {
      L_diff_lci[[i]]=as.data.frame(t(data.frame(myjson2()$losd_lci[i])))
      colnames(L_diff_lci[[i]]) <- v_diff
    }
    for(i in 1:length(myjson2()$losd_lci)) {  
      L_diff_lci[[i]]=as.data.frame(cbind(L_diff_lci[[i]], timevar ,state=rep(i,nrow(L_diff_lci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$losd_lci)) {
      L_diff_lci[[i]]=as.data.frame(myjson2()$losd_lci[[i]][,1])
    }
    for (i in 1:length(myjson2()$losd_lci)) {  
      L_diff_lci[[i]]=as.data.frame(c(L_diff_lci[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$losd_lci[[i]][,1])) )) )
      colnames(L_diff_lci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_diff
    }
  }  
  # Append the probabilities datasets of the different states
  data_losd_lci=list()
  data_losd_lci[[1]]=L_diff_lci[[1]]
  
  for (u in 2:(length(myjson2()$losd_lci))) {
    data_losd_lci[[u]]=rbind(L_diff_lci[[u]],data_losd_lci[[(u-1)]])
  }
  
  datald_lci=data_losd_lci[[length(myjson2()$losd_lci)]]
  datald_lci$state_fac=c(rep("NA",nrow(datald_lci)))
  
  for (o in 1:(length(myjson2()$losd_lci))) {
    for (g in 1:nrow(datald_lci))  {
      if  (datald_lci$state[g]==o) {datald_lci$state_fac[g]=labels_state()[o]}  
    }
  }
  datald_lci
}) 

data_L_diff2<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    
    
    dlist[[d]]=cbind.data.frame(data_L_diff1()[,d],data_L_diff1()[,ncol(data_L_diff1())-2],data_L_diff1()[,ncol(data_L_diff1())-1],
                                data_L_diff1()[,ncol(data_L_diff1())],rep(d,length(data_L_diff1()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_L_diff1())[d],length(data_L_diff1()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_ld <- bind_rows(dlist, .id = "column_label")
  d_all_ld
}) 

data_L_diff2_uci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_L_diff1_uci()[,d],data_L_diff1_uci()[,ncol(data_L_diff1_uci())-2],
                                data_L_diff1_uci()[,ncol(data_L_diff1_uci())-1],
                                data_L_diff1_uci()[,ncol(data_L_diff1_uci())],rep(d,length(data_L_diff1_uci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_L_diff1_uci())[d],length(data_L_diff1_uci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_ld_uci <- bind_rows(dlist, .id = "column_label")
  d_all_ld_uci
}) 

data_L_diff2_lci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_L_diff1_lci()[,d],data_L_diff1_lci()[,ncol(data_L_diff1_lci())-2],
                                data_L_diff1_lci()[,ncol(data_L_diff1_lci())-1],
                                data_L_diff1_lci()[,ncol(data_L_diff1_lci())],rep(d,length(data_L_diff1_lci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_L_diff1_lci())[d],length(data_L_diff1_lci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_ld_lci <- bind_rows(dlist, .id = "column_label")
  d_all_ld_lci
}) 

data_L_diff_ci<- reactive ({
  x=c( data_L_diff2()[order(data_L_diff2()$timevar,data_L_diff2()$state,data_L_diff2()$cov),]$timevar,
       data_L_diff2()[order(-data_L_diff2()$timevar,data_L_diff2()$state,data_L_diff2()$cov),]$timevar )
  
  y_central=c( data_L_diff2()[order(data_L_diff2()$timevar,data_L_diff2()$state,data_L_diff2()$cov),]$V,
               data_L_diff2()[order(-data_L_diff2()$timevar,data_L_diff2()$state,data_L_diff2()$cov),]$V )
  
  y=c( data_L_diff2_uci()[order(data_L_diff2_uci()$timevar,data_L_diff2_uci()$state,data_L_diff2_uci()$cov),]$V,
       data_L_diff2_lci()[order(-data_L_diff2_lci()$timevar,data_L_diff2_lci()$state,data_L_diff2_lci()$cov),]$V )
  
  frameto=c(as.character(data_L_diff2_uci()[order(-data_L_diff2_uci()$timevar,data_L_diff2_uci()$state,data_L_diff2_uci()$cov),]$state_factor),
            as.character(data_L_diff2_lci()[order(-data_L_diff2_lci()$timevar,data_L_diff2_lci()$state,data_L_diff2_lci()$cov),]$state_factor) )
  
  covto=c( data_L_diff2_uci()[order(-data_L_diff2_uci()$timevar,data_L_diff2_uci()$state,data_L_diff2_uci()$cov),]$cov_factor,
           data_L_diff2_lci()[order(-data_L_diff2_lci()$timevar,data_L_diff2_lci()$state,data_L_diff2_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,frameto,covto,y_central)
  data
})


#output$shouldloadlos4 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotlos4", label = h2("Download the plot"))
#})

datalos4_re <-  reactive({
  
  ax <- list( title = "",zeroline = FALSE,showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
  
  if (length(myjson2()$losd) == 0 | myjson2()$Nats==1 ) {             
    L_state_d= plot_ly() %>%
      layout(title=list(text="Not applicable- Only one covariate pattern specified",y=0.95),xaxis=ax, yaxis=ax)
    L_state_d
  } 
  
  else {
  
  if (input$conflos=="ci_no") {
    
    if (input$facetlos=="No") {
      
      L_state_d= plot_ly(data_L_diff2(),alpha=0.5) %>%
        add_lines(
          x=data_L_diff2()$timevar,y=data_L_diff2()$V,
          frame=factor(as.factor(data_L_diff2()$state_factor),levels=labels_state()),
          color=as.factor(data_L_diff2()$cov_factor),
          colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
          mode="lines",
          line=list(simplify=FALSE),
          text = 'Select or deselect lines by clicking on the legend',
          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
        )  %>%
        layout(title=list(text="Differences in length of stay among covariate patterns",y=0.95),
               font= list(family = "times new roman", size = input$textsizelos, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$steplosx, 
                          tick0 = input$startlosx, 
                          range=c(input$startlosx,input$endlosx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Ratios in length of stay",                    
                           dtick = input$steplosy, 
                           ticklen = 5,
                           tickwidth = 2,
                           tickcolor = toRGB("black")),
               shapes = list(
                 list(type = "rect",
                      fillcolor = "grey", 
                      line = list(color = "grey"), 
                      opacity = 0.8,
                      x0 = 0, x1 =0, xref = "x", y0 = 0, y1 = 1, yref = "y") )
        ) %>%
        animation_opts(frame = 1000, transition = 0, redraw = FALSE)%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        )
      L_state_d
    }
    
    if (input$facetlos=="Yes") {
      
      
      data_plot=data_L_diff2()
      
      L_state_d = ggplot(data_plot)
      L_state_d = ggplot(data_plot,aes(x=timevar, y=V, color=factor(as.factor(cov_factor) ), group=1,
                                       text=paste("Select or deselect lines by clicking on the legend",
                                                  "<br>Time: ", timevar,
                                                  "<br>Differences in length of stay: ", V,
                                                  "<br>Covariate pattern: ", factor(as.factor(cov_factor) ))))
      
      L_state_d = L_state_d+geom_line(aes(x=timevar, y=V, color= factor(as.factor(cov_factor) )))+
        scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      if (input$aimtype=="compare") {L_state_d = L_state_d+ facet_wrap(~factor(as.factor(state_factor),levels= labels_state() ),nrow=2)}
      
      else if (input$aimtype=="present")   {L_state_d = L_state_d+ facet_wrap(~factor(as.factor(state_factor),levels= labels_state() ) )}
      
      L_state_d = L_state_d + scale_x_continuous(breaks=c(seq(input$startlosx,input$endlosx,by=input$steplosx  ))) 
      L_state_d = L_state_d +labs(title="Differences in length of stay among covariate patterns",
                                  x="Time since entry", y="Differences in length of stay")
      
      L_state_d = L_state_d + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      L_state_d = L_state_d  +theme(title = element_text(size = input$textsizelos-4),  strip.text = element_text(size=input$textfacetlos),        
                            legend.title = element_text(color="black", size= input$textsizelos-5), 
                            legend.text=element_text(size= input$textsizelos-6),
                            plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                            legend.margin = margin(1.5, 1, 1, 1, "cm"),
                            legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                            axis.title.y = element_text(size= input$textsizelos-5),
                            axis.title.x = element_text(size= input$textsizelos-5), 
                            axis.text.x = element_text( size=input$textsizelos-6),axis.text.y = element_text( size=input$textsizelos-6))  
      
      L_state_d = ggplotly(L_state_d, tooltip = "text")%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        ) 
      
      L_state_d 
    }
    
  }
  
  else if (input$conflos=="ci_yes") {
    
    
    if (input$facetlos=="No") {
      
      L_state_d <- plot_ly()
      
      L_state_d <- add_trace(L_state_d, line=list(simplify=FALSE),
                             mode="lines", type = "scatter",
                             x=data_L_diff_ci()$x, y=data_L_diff_ci()$y_central,
                             frame=factor(as.factor(data_L_diff_ci()$frameto),levels= labels_state() ), 
                             colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                             color=as.factor(data_L_diff_ci()$covto),
                             text = 'Select or deselect lines by clicking on the legend',
                             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") )
      
      L_state_d <- add_trace(L_state_d, fill = "tozerox", 
                             line=list(dash = "solid", color = "transparent", width = 1.8897637),
                             mode = "lines", type = "scatter",
                             x=data_L_diff_ci()$x, y=data_L_diff_ci()$y,
                             frame=factor(as.factor(data_L_diff_ci()$frameto),levels= labels_state() ), 
                             colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                             color=as.factor(data_L_diff_ci()$covto),
                             showlegend = FALSE,
                             text = 'Select or deselect lines by clicking on the legend',
                             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      L_state_d= L_state_d   %>%
        layout(title=list(text="Differences in length of stay among covariate patterns",y=0.95),
               font= list(family = "times new roman", size = input$textsizelos, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$steplosx, 
                          tick0 = input$startlosx, 
                          range=c(input$startlosx,input$endlosx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Differences in length of stay",                    
                           dtick = input$steplosy, 
                           ticklen = 5,
                           tickwidth = 2,
                           tickcolor = toRGB("black")),
               shapes = list(
                 list(type = "rect",
                      fillcolor = "grey", 
                      line = list(color = "grey"), 
                      opacity = 0.8,
                      x0 = 0, x1 =0, xref = "x", y0 = 0, y1 = 1, yref = "y") )  )%>%
        animation_opts(frame = 1000, transition = 0, redraw = FALSE)%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        )
      L_state_d
      
    }
    
    if (input$facetlos=="Yes") {
      
      V_lci=  data_L_diff2_lci()$V
      V_uci=  data_L_diff2_uci()$V
      
      data_plot=cbind(data_L_diff2(),V_lci,V_uci)
      
      
      L_state_d=ggplot(data_plot)
      L_state_d=ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(cov_factor) ), group=1,
                                     text=paste("Select or deselect lines by clicking on the legend",
                                                "<br>Time: ", timevar,
                                                "<br>Differences in length of stay: ", V,
                                                "<br>Covariate pattern: ",  factor(as.factor(cov_factor) ))))+
        scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      
      L_state_d=L_state_d+geom_line(aes(x=timevar, y=V, fill= factor(as.factor(cov_factor) )))
      
      L_state_d=L_state_d+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill= factor(as.factor(cov_factor) )),alpha=0.4)+ 
        scale_fill_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
    
      if (input$aimtype=="compare") {L_state_d = L_state_d+ facet_wrap(~factor(as.factor(state_factor),levels= labels_state() ),nrow=2)}
      
      else if (input$aimtype=="present")   {L_state_d = L_state_d+ facet_wrap(~factor(as.factor(state_factor),levels= labels_state() ))}
      
      L_state_d = L_state_d + scale_x_continuous(breaks=c(seq(input$startlosx,input$endlosx,by=input$steplosx  ))) 
      L_state_d = L_state_d +labs(title="Differences in length of stay among covariate patterns",
                                  x="Time since entry", y="Differences in length of stay")
      
      L_state_d = L_state_d + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      L_state_d = L_state_d  +theme(title = element_text(size = input$textsizelos-4),  strip.text = element_text(size=input$textfacetlos),     
                            legend.title = element_text(color="black", size= input$textsizelos-5), 
                            legend.text=element_text(size= input$textsizelos-6),
                            plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                            legend.margin = margin(1.5, 1, 1, 1, "cm"),
                            legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                            axis.title.y = element_text(size= input$textsizelos-5),
                            axis.title.x = element_text(size= input$textsizelos-5), 
                            axis.text.x = element_text( size=input$textsizelos-6),axis.text.y = element_text( size=input$textsizelos-6))  
      
      L_state_d = ggplotly(L_state_d, tooltip = "text")%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        ) 
      
      
    }
    
  }
  
  
  L_state_d
  }
  
})  

output$los_diff <- renderPlotly ({ datalos4_re() })

output$downplotlos4 <- downloadHandler(
  filename = function(){paste("los4",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datalos4_re(),width = 1400, height = 1100, format = "png", scale = 2,  out_file = file )
  }
)
  
######################################################################################
#####################################################################################
data_L_ratio1 <- reactive ({
  los_ratio=list()
  
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_ratio= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_ratio[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$losr)) {
      los_ratio[[i]]=as.data.frame(t(data.frame(myjson2()$losr[i])))
      colnames(los_ratio[[i]]) <- v_ratio
    }
    for(i in 1:length(myjson2()$losr)) {  
      los_ratio[[i]]=as.data.frame(cbind(los_ratio[[i]], timevar ,state=rep(i,nrow(los_ratio[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$losr)) {
      los_ratio[[i]]=as.data.frame(myjson2()$losr[[i]][,1])
    }
    for (i in 1:length(myjson2()$losr)) {  
      los_ratio[[i]]=as.data.frame(c(los_ratio[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$losr[[i]][,1])) )) )
      colnames(los_ratio[[i]])[1:(length(myjson2()$atlist)-1)] <- v_ratio
    }
  }
  
  # Append the probabilities datasets of the different states
  data_losr=list()
  data_losr[[1]]=los_ratio[[1]]
  
  for (u in 2:(length(myjson2()$losr))) {
    data_losr[[u]]=rbind(los_ratio[[u]],data_losr[[(u-1)]])
  }
  
  datalr=data_losr[[length(myjson2()$losr)]]
  datalr$state_fac=c(rep("NA",nrow(datalr)))
  
  for (o in 1:(length(myjson2()$losr))) {
    for (g in 1:nrow(datalr))  {
      if  (datalr$state[g]==o) {datalr$state_fac[g]=labels_state()[o]}  
    }
  }
  datalr
}) 


data_L_ratio1_uci <- reactive ({
  L_ratio_uci=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_ratio= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_ratio[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$losr_uci)) {
      L_ratio_uci[[i]]=as.data.frame(t(data.frame(myjson2()$losr_uci[i])))
      colnames(L_ratio_uci[[i]]) <- v_ratio
    }
    for(i in 1:length(myjson2()$losr_uci)) {  
      L_ratio_uci[[i]]=as.data.frame(cbind(L_ratio_uci[[i]], timevar ,state=rep(i,nrow(L_ratio_uci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$losr_uci)) {
      L_ratio_uci[[i]]=as.data.frame(myjson2()$losr_uci[[i]][,1])
    }
    for (i in 1:length(myjson2()$losr_uci)) {  
      L_ratio_uci[[i]]=as.data.frame(c(L_ratio_uci[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$losr_uci[[i]][,1])) )) )
      colnames(L_ratio_uci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_ratio
    }
  }
  
  # Append the probabilities datasets of the ratioerent states
  data_losr_uci=list()
  data_losr_uci[[1]]=L_ratio_uci[[1]]
  
  for (u in 2:(length(myjson2()$losr_uci))) {
    data_losr_uci[[u]]=rbind(L_ratio_uci[[u]],data_losr_uci[[(u-1)]])
  }
  
  datalr_uci=data_losr_uci[[length(myjson2()$losr_uci)]]
  datalr_uci$state_fac=c(rep("NA",nrow(datalr_uci)))
  
  for (o in 1:(length(myjson2()$losr_uci))) {
    for (g in 1:nrow(datalr_uci))  {
      if  (datalr_uci$state[g]==o) {datalr_uci$state_fac[g]=labels_state()[o]}  
    }
  }
  datalr_uci
}) 

data_L_ratio1_lci <- reactive ({
  L_ratio_lci=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_ratio= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_ratio[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$losr_lci)) {
      L_ratio_lci[[i]]=as.data.frame(t(data.frame(myjson2()$losr_lci[i])))
      colnames(L_ratio_lci[[i]]) <- v_ratio
    }
    for(i in 1:length(myjson2()$losr_lci)) {  
      L_ratio_lci[[i]]=as.data.frame(cbind(L_ratio_lci[[i]], timevar ,state=rep(i,nrow(L_ratio_lci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$losr_lci)) {
      L_ratio_lci[[i]]=as.data.frame(myjson2()$losr_lci[[i]][,1])
    }
    for (i in 1:length(myjson2()$losr_lci)) {  
      L_ratio_lci[[i]]=as.data.frame(c(L_ratio_lci[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$losr_lci[[i]][,1])) )) )
      colnames(L_ratio_lci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_ratio
    }
  }
  # Append the probabilities datasets of the ratioerent states
  data_losr_lci=list()
  data_losr_lci[[1]]=L_ratio_lci[[1]]
  
  for (u in 2:(length(myjson2()$losr_lci))) {
    data_losr_lci[[u]]=rbind(L_ratio_lci[[u]],data_losr_lci[[(u-1)]])
  }
  
  datalr_lci=data_losr_lci[[length(myjson2()$losr_lci)]]
  datalr_lci$state_fac=c(rep("NA",nrow(datalr_lci)))
  
  for (o in 1:(length(myjson2()$losr_lci))) {
    for (g in 1:nrow(datalr_lci))  {
      if  (datalr_lci$state[g]==o) {datalr_lci$state_fac[g]=labels_state()[o]}  
    }
  }
  datalr_lci
}) 


data_L_ratio2<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_L_ratio1()[,d],data_L_ratio1()[,ncol(data_L_ratio1())-2],data_L_ratio1()[,ncol(data_L_ratio1())-1],
                                data_L_ratio1()[,ncol(data_L_ratio1())],rep(d,length(data_L_ratio1()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_L_ratio1())[d],length(data_L_ratio1()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_lr <- bind_rows(dlist, .id = "column_label")
  d_all_lr
}) 


data_L_ratio2_uci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_L_ratio1_uci()[,d],data_L_ratio1_uci()[,ncol(data_L_ratio1_uci())-2],
                                data_L_ratio1_uci()[,ncol(data_L_ratio1_uci())-1],
                                data_L_ratio1_uci()[,ncol(data_L_ratio1_uci())],rep(d,length(data_L_ratio1_uci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_L_ratio1_uci())[d],length(data_L_ratio1_uci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_lr_uci <- bind_rows(dlist, .id = "column_label")
  d_all_lr_uci
}) 


data_L_ratio2_lci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_L_ratio1_lci()[,d],data_L_ratio1_lci()[,ncol(data_L_ratio1_lci())-2],
                                data_L_ratio1_lci()[,ncol(data_L_ratio1_lci())-1],
                                data_L_ratio1_lci()[,ncol(data_L_ratio1_lci())],rep(d,length(data_L_ratio1_lci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_L_ratio1_lci())[d],length(data_L_ratio1_lci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_lr_lci <- bind_rows(dlist, .id = "column_label")
  d_all_lr_lci
}) 


data_L_ratio_ci<- reactive ({
  x=c( data_L_ratio2()[order(data_L_ratio2()$timevar,data_L_ratio2()$state,data_L_ratio2()$cov),]$timevar,
       data_L_ratio2()[order(-data_L_ratio2()$timevar,data_L_ratio2()$state,data_L_ratio2()$cov),]$timevar )
  
  y_central=c( data_L_ratio2()[order(data_L_ratio2()$timevar,data_L_ratio2()$state,data_L_ratio2()$cov),]$V,
               data_L_ratio2()[order(-data_L_ratio2()$timevar,data_L_ratio2()$state,data_L_ratio2()$cov),]$V )
  
  y=c( data_L_ratio2_uci()[order(data_L_ratio2_uci()$timevar,data_L_ratio2_uci()$state,data_L_ratio2_uci()$cov),]$V,
       data_L_ratio2_lci()[order(-data_L_ratio2_lci()$timevar,data_L_ratio2_lci()$state,data_L_ratio2_lci()$cov),]$V )
  
  frameto=c(as.character(data_L_ratio2_uci()[order(-data_L_ratio2_uci()$timevar,data_L_ratio2_uci()$state,data_L_ratio2_uci()$cov),]$state_factor),
            as.character(data_L_ratio2_lci()[order(-data_L_ratio2_lci()$timevar,data_L_ratio2_lci()$state,data_L_ratio2_lci()$cov),]$state_factor) )
  
  covto=c( data_L_ratio2_uci()[order(-data_L_ratio2_uci()$timevar,data_L_ratio2_uci()$state,data_L_ratio2_uci()$cov),]$cov_factor,
           data_L_ratio2_lci()[order(-data_L_ratio2_lci()$timevar,data_L_ratio2_lci()$state,data_L_ratio2_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,frameto,covto,y_central)
  data
})

#output$shouldloadlos5 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotlos5", label = h2("Download the plot"))
#})

datalos5_re <-  reactive({

 
  
  ax <- list( title = "",zeroline = FALSE,showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
  
  if (length(myjson2()$losr) == 0| myjson2()$Nats==1 ) {             
    L_state_r= plot_ly() %>%
      layout(title=list(text="Not applicable- Only one covariate pattern specified",y=0.95),xaxis=ax, yaxis=ax)
    L_state_r
  } 
  
  else {
  
  if (input$conflos=="ci_no") {
    
    if (input$facetlos=="No") {
      
      
      L_state_r= plot_ly(data_L_ratio2(),alpha=0.5) %>%
        add_lines(
          x=data_L_ratio2()$timevar,y=data_L_ratio2()$V,
          frame=factor(as.factor(data_L_ratio2()$state_factor),levels=labels_state()),
          color=as.factor(data_L_ratio2()$cov_factor),
          colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
          mode="lines",
          line=list(simplify=FALSE),
          text = 'Select or deselect lines by clicking on the legend',
          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
        )  %>%
        layout(title=list(text="Ratios in length of stay among covariate patterns",y=0.95),
               font= list(family = "times new roman", size = input$textsizelos, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$steplosx, 
                          tick0 = input$startlosx, 
                          range=c(input$startlosx,input$endlosx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Ratios in length of stay",                    
                           dtick = input$steplosy, 
                           ticklen = 5,
                           tickwidth = 2,
                           tickcolor = toRGB("black")),
               shapes = list(
                 list(type = "rect",
                      fillcolor = "grey", 
                      line = list(color = "grey"), 
                      opacity = 0.8,
                      x0 = 0, x1 =0, xref = "x", y0 = 0, y1 = 1, yref = "y") )
        ) %>%
        animation_opts(frame = 1000, transition = 0, redraw = FALSE)%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        )
      L_state_r
      
    }
    
    if (input$facetlos=="Yes") {
      
      
      data_plot=data_L_ratio2()
      
      L_state_r = ggplot(data_plot)
      L_state_r = ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(cov_factor) ), group=1,
                                       text=paste("Select or deselect lines by clicking on the legend",
                                                  "<br>Time: ", timevar,
                                                  "<br>Ratio of length of stay: ", V,
                                                  "<br>Covariate pattern: ",  factor(as.factor(cov_factor) ))))
      
      L_state_r = L_state_r+geom_line(aes(x=timevar, y=V, color=factor(as.factor(cov_factor) )))+
        scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      

      if (input$aimtype=="compare") {L_state_r = L_state_r+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()),nrow=2)}
      
      else if (input$aimtype=="present")   {L_state_r = L_state_r+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state() ) )}
      
      L_state_r = L_state_r + scale_x_continuous(breaks=c(seq(input$startlosx,input$endlosx,by=input$steplosx  ))) 
      L_state_r = L_state_r +labs(title="Ratios in length of stay among covariate patterns", x="Time since entry", y="Ratios in length of stay")
      
      L_state_r = L_state_r + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      L_state_r = L_state_r+theme(title = element_text(size = input$textsizelos-4),   strip.text = element_text(size=input$textfacetlos)  ,   
                            legend.title = element_text(color="black", size= input$textsizelos-5), 
                            legend.text=element_text(size= input$textsizelos-6),
                            plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                            legend.margin = margin(1.5, 1, 1, 1, "cm"),
                            legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                            axis.title.y = element_text(size= input$textsizelos-5),
                            axis.title.x = element_text(size= input$textsizelos-5), 
                            axis.text.x = element_text( size=input$textsizelos-6),axis.text.y = element_text( size=input$textsizelos-6))  
      
      L_state_r = ggplotly(L_state_r, tooltip = "text")%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        ) 
      
      L_state_r 
    }
    
  }
  
  else if (input$conflos=="ci_yes") {
    
    
    if (input$facetlos=="No") {
      
      L_state_r <- plot_ly()
      
      L_state_r <- add_trace(L_state_r, line=list(simplify=FALSE),
                             mode="lines", type = "scatter",
                             x=data_L_ratio_ci()$x, y=data_L_ratio_ci()$y_central,
                             frame=factor(as.factor(data_L_ratio_ci()$frameto),levels = labels_state() ), 
                             colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                             color=as.factor(data_L_ratio_ci()$covto),
                             text = 'Select or deselect lines by clicking on the legend',
                             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") )
      
      L_state_r <- add_trace(L_state_r, fill = "tozerox", 
                             line=list(dash = "solid", color = "transparent", width = 1.8897637),
                             mode = "lines", type = "scatter",
                             x=data_L_ratio_ci()$x, y=data_L_ratio_ci()$y,
                             frame=factor(as.factor(data_L_ratio_ci()$frameto),levels = labels_state() ),
                             colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                             color=as.factor(data_L_ratio_ci()$covto),
                             showlegend = FALSE,
                             text = 'Select or deselect lines by clicking on the legend',
                             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      L_state_r= L_state_r   %>%
        layout(title=list(text="Ratios in length of stay among covariate patterns",y=0.95),
               font= list(family = "times new roman", size = input$textsizelos, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$steplosx, 
                          tick0 = input$startlosx, 
                          range=c(input$startlosx,input$endlosx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Ratios in length of stay",                    
                           dtick = input$stepy, 
                           ticklen = 5,
                           tickwidth = 2,
                           tickcolor = toRGB("black")),
               shapes = list(
                 list(type = "rect",
                      fillcolor = "grey", 
                      line = list(color = "grey"), 
                      opacity = 0.8,
                      x0 = 0, x1 =input$area, xref = "x", y0 = 0, y1 = 1, yref = "y") )  )%>%
        animation_opts(frame = 1000, transition = 0, redraw = FALSE)%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        )
      
      L_state_r  
    }
    
    if (input$facetlos=="Yes") {
      
      V_lci=  data_L_ratio2_lci()$V
      V_uci=  data_L_ratio2_uci()$V
      
      data_plot=cbind(data_L_ratio2(),V_lci,V_uci)
      
      
      L_state_r=ggplot(data_plot)
      L_state_r=ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(cov_factor) ), group=1,
                                     text=paste("Select or deselect lines by clicking on the legend",
                                                "<br>Time: ", timevar,
                                                "<br>Ratio of length of stay: ", V,
                                                "<br>Covariate pattern: ",  factor(as.factor(cov_factor) ))))+
        scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      
      L_state_r=L_state_r+geom_line(aes(x=timevar, y=V, fill= factor(as.factor(cov_factor) )))
      
      L_state_r=L_state_r+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill=factor(as.factor(cov_factor))),alpha=0.4)+ 
        scale_fill_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      if (input$aimtype=="compare") {L_state_r = L_state_r+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state() ),nrow=2)}
      
      else if (input$aimtype=="present")   {L_state_r = L_state_r+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state() ))}
      
      
      L_state_r = L_state_r + scale_x_continuous(breaks=c(seq(input$startlosx,input$endlosx,by=input$steplosx  ))) 
      L_state_r = L_state_r +labs(title="Ratios in length of stay among covariate patterns", x="Time since entry", y="Ratios in length of stay")
      
      L_state_r = L_state_r + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      L_state_r = L_state_r +theme(title = element_text(size = input$textsizelos-4),  strip.text = element_text(size=input$textfacetlos),  
                                   legend.title = element_text(color="black", size= input$textsizelos-5), 
                                   legend.text=element_text(size= input$textsizelos-6),
                                   plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                   legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                   legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                   axis.title.y = element_text(size= input$textsizelos-5),
                                   axis.title.x = element_text(size= input$textsizelos-5), 
                                   axis.text.x = element_text( size=input$textsizelos-6),axis.text.y = element_text( size=input$textsizelos-6))  
                           
                           
                            
      
      L_state_r = ggplotly(L_state_r, tooltip = "text")%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1200,
            height = 900,scale=input$figscale
          ), edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE,
            axisTitleText=TRUE,
            colorbarTitleText=TRUE,
            legendPosition=TRUE,
            legendText=TRUE,
            shapePosition=TRUE,
            titleText=TRUE
          ) ,queueLength=10
        ) 
      
      
    }
    
  }
  
  
  L_state_r
  }
  
}) 

output$los_ratio <- renderPlotly ({datalos5_re()})

output$downplotlos5 <- downloadHandler(
  filename = function(){paste("los5",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datalos5_re(),width = 1400, height = 1100, format = "png", scale = 2,  out_file = file )
  }
)