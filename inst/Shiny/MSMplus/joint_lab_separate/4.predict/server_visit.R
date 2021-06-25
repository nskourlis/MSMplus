###### Show and hide  and tick inputs ####

timerv <- reactiveVal(1.5)


observeEvent(c(input$showtickvis,invalidateLater(1000, session)), { 
  
  if(input$showtickvis=="No"){
    
    hide("tickinputvisit")
    
  }
  
  if(input$showtickvis=="Yes"){
    
    show("tickinputvisit")
    
  }
  
  isolate({
    
    timerv(timerv()-1)
    if(timerv()>1 & input$showtickvis=="No")
    {
      show("tickinputvisit")
    }
    
  })
})



################################################

existvisit <- reactive({
  if (length(myjson2()$visit) != 0) {       
    x= 1
  }
  else if (length(myjson2()$visit) == 0) {       
    x= 0
  }
})

existvisitratio <- reactive({
  if (length(myjson2()$visitr) != 0) {       
    x= 1
  }
  else if (length(myjson2()$visitr) == 0) {       
    x= 0
  }
})

existvisitdiff <- reactive({
  if (length(myjson2()$visitd) != 0) {       
    x= 1
  }
  else if (length(myjson2()$visitd) == 0) {       
    x= 0
  }
})


output$pagevisit <- renderUI({
  
  if (is.null(myjson2())) return("Provide the json file with the predictions")
  
  if (existvisit()==0) {
    
    fluidRow(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      
      column(12,
             shinyjs::useShinyjs(),
             output$loginpagevisit <- renderUI({h1("Non applicable")})
      )
    )
  }
  
  else if (existvisit()==1) {
    
    if (existvisitdiff()==1 & existvisitratio()==1) { 
      
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,
               shinyjs::useShinyjs(),
               h1("Visit probabilities"),
               
               conditionalPanel(condition="input.tabsvis =='#panel1vis'||input.tabsvis =='#panel2vis'||input.tabsvis=='#panel4vis'||input.tabsvis =='#panel5vis'",
                                uiOutput("facetvis")
               ) ,
               uiOutput("confvis")
               
               #radioButtons("displayvis", "Change labels of states and covariate patterns",
               #              c("same" = "same", "change"= "change")),
               #uiOutput("covarinputvis"),
               #uiOutput("statesinputvis")
        ),
        
        column(2,
               br(),
               p(""),
               radioButtons("showtickvis", "Show axis tick options",
                            choices = list("No" = "No",
                                           "Yes" = "Yes"), selected ="No"),
               uiOutput("tickinputvisit")
        ),
        
        column(7,
               tabsetPanel(id = "tabsvis",
                 tabPanel(h2("By state"),  value = "#panel1vis",                  plotlyOutput("visit_state"  , height="600px", width = "100%"),uiOutput("shouldloadvis1")),
                 tabPanel(h2("By covariate pattern"), value = "#panel2vis",      plotlyOutput("visit_cov"  , height="600px", width = "100%"),uiOutput("shouldloadvis2")),
                 tabPanel(h2("By state and covariate pattern"),    value = "#panel3vis",          plotlyOutput("visit_both"  , height="600px", width = "100%"),uiOutput("shouldloadvis3")),
                 tabPanel(h2("Differences"), value = "#panel4vis",                plotlyOutput("visit_diff"  , height="600px", width = "100%"),uiOutput("shouldloadvis4")),
                 tabPanel(h2("Ratios"),         value = "#panel5vis",             plotlyOutput("visit_ratio"  , height="600px", width = "100%"),uiOutput("shouldloadvis5"))
                 
               )
        )
      )
      
      
    }
    
    else if (existvisitdiff()==1 & existvisitratio()==0) { 
      
      
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,
               shinyjs::useShinyjs(),
               h1("Visit probabilities"),
               conditionalPanel(condition="input.tabsvis =='#panel1vis'||input.tabsvis =='#panel2vis'||input.tabsvis=='#panel4vis'||input.tabsvis =='#panel5vis'",
                                uiOutput("facetvis")
               ) ,
               uiOutput("confvis")
               #radioButtons("displayvis", "Change labels of states and covariate patterns",
               #             c("same" = "same", "change"= "change")),
               #uiOutput("covarinputvis"),
               #uiOutput("statesinputvis")
        ),
        
        column(2,
               br(),
               p(""),
               radioButtons("showtickvis", "Show axis tick options",
                            choices = list("No" = "No",
                                           "Yes" = "Yes"), selected = "No"),
               uiOutput("tickinputvisit")
        ),
        
        column(7,
               tabsetPanel(id = "tabsvis",
                 tabPanel(h2("By state"),      value = "#panel1vis",              plotlyOutput("visit_state"  , height="600px", width = "100%"),uiOutput("shouldloadvis1")),
                 tabPanel(h2("By covariate pattern"),  value = "#panel2vis",      plotlyOutput("visit_cov"  , height="600px", width = "100%"),uiOutput("shouldloadvis2")),
                 tabPanel(h2("By state and covariate pattern"),value = "#panel3vis", plotlyOutput("visit_both"  , height="600px", width = "100%"),uiOutput("shouldloadvis3")),
                 tabPanel(h2("Differences"),      value = "#panel4vis",           plotlyOutput("visit_diff"  , height="600px", width = "100%"),uiOutput("shouldloadvis4")),
                 tabPanel(h2("Ratios"),            value = "#panel5vis",          print("Not applicable"))
                 
               )
        )
      )
      
    }
    
    else if ( existvisitdiff()==0 & existvisitratio()==1) { 
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,
               h1("Visit probabilities"),
               conditionalPanel(condition="input.tabsvis =='#panel1vis'||input.tabsvis =='#panel2vis'||input.tabsvis=='#panel4vis'||input.tabsvis =='#panel5vis'",
                                uiOutput("facetvis")
               ) ,
               uiOutput("confvis")
               #radioButtons("displayvis", "Change labels of states and covariate patterns",
               #             c("same" = "same", "change"= "change")),
               #uiOutput("covarinputvis"),
               #uiOutput("statesinputvis")
        ),
        
        column(2,
               br(),
               p(""),
               radioButtons("showtickvis", "Show axis tick options",
                            choices = list("No" = "No",
                                           "Yes" = "Yes"), selected = "No"),
               uiOutput("tickinputvisit")
        ),
        
        column(7,
               tabsetPanel(id = "tabsvis",
                 tabPanel(h2("By state"),     value = "#panel1vis",                plotlyOutput("visit_state"  , height="600px", width = "100%"),uiOutput("shouldloadvis1")),
                 tabPanel(h2("By covariate pattern"),  value = "#panel2vis",       plotlyOutput("visit_cov"  , height="600px", width = "100%"),uiOutput("shouldloadvis2")),
                 tabPanel(h2("By state and covariate pattern"), value = "#panel3vis", plotlyOutput("visit_both"  , height="600px", width = "100%"),uiOutput("shouldloadvis3")),
                 tabPanel(h2("Differences"),         value = "#panel4vis",         print("Not applicable")),
                 tabPanel(h2("Ratios"),            value = "#panel5vis",           plotlyOutput("visit_ratio"  , height="600px", width = "100%"),uiOutput("shouldloadvis5"))
                 
               )
        )
      )
      
      
    }
    
    else if (existvisitdiff()==0 & existvisitratio()==0) { 
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,
               h1("Visit probabilities"),
               
               conditionalPanel(condition="input.tabsvis =='#panel1vis'||input.tabsvis =='#panel2vis'||input.tabsvis=='#panel4vis'||input.tabsvis =='#panel5vis'",
                                uiOutput("facetvis")
               )  ,
               uiOutput("confvis")
               #radioButtons("displayvis", "Change labels of states and covariate patterns",
               #             c("same" = "same", "change"= "change")),
               #uiOutput("covarinputvis"),
               #uiOutput("statesinputvis")
        ),
        
        column(2,
               br(),
               p(""),
               radioButtons("showtickvis", "Show axis tick options",
                            choices = list("No" = "No",
                                           "Yes" = "Yes"), selected = "No"),
               uiOutput("tickinputvisit")
        ),
        
        column(7,
               tabsetPanel(id = "tabsvis",
                 tabPanel(h2("By state"), value = "#panel1vis",                    plotlyOutput("visit_state"  , height="600px", width = "100%"),uiOutput("shouldloadvis1")),
                 tabPanel(h2("By covariate pattern"), value = "#panel2vis",       plotlyOutput("visit_cov"  , height="600px", width = "100%"),uiOutput("shouldloadvis2")),
                 tabPanel(h2("By state and covariate pattern"),value = "#panel3vis", plotlyOutput("visit_both"  , height="600px", width = "100%"),uiOutput("shouldloadvis3")),
                 tabPanel(h2("Differences"),     value = "#panel4vis",            print("Not applicable")),
                 tabPanel(h2("Ratios"),            value = "#panel5vis",          print("Not applicable"))
                 
               )
        )
      )      
      
    }
  }
  
})    

observeEvent(input$json2, {
  if( length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'Visit')))==0  ) {
    js$disableTab("mytab_vis")
    
  } 
}) 

observeEvent(input$csv2, {
  if( length(which(startsWith(names( read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'Visit')))==0 ) {
    js$disableTab("mytab_vis")
    
  } 
}) 

#output$displayvisit <- renderUI({
#  radioButtons("displayvisit", "Change labels of states and covariate patterns",
#               c("same" = "same", "change"= "change"))
#})

output$facetvis <- renderUI({
  
radioButtons(inputId="facetvis", label= "Display graph in grids",
             choices=c("No","Yes"),selected = "No")
})


output$confvis <- renderUI({
  
    if  (length(myjson2()$ci_visit)!=0) {
      radioButtons("confvis", "Confidence intervals",
                   c("No" = "ci_no",
                     "Yes" ="ci_yes"))
    }
    
    else if (length(myjson2()$ci_visit)==0) {
      item_list <- list()
      item_list[[1]]<- radioButtons("confvis", "Confidence intervals",c("No" = "ci_no"))
      item_list[[2]]<-print("Confidence interval data were not provided")
      do.call(tagList, item_list)
    }
    
  })
##################################################
###### Will appear conditionally##################
###################################################

#Create the reactive input of covariates
#output$covarinputvis <- renderUI({
#  
#  if (is.null(myjson2()))  return()
#  
#  if (input$displayvis=="same") return()
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
#  
#  default_choices_cov=v
#  for (i in seq(length(myjson2()$cov$atlist))) {
#    item_list[[i+1]] <- textInput(paste0('covvis', i),default_choices_cov[i], labels_cov()[i])
#  }
#  
#  do.call(tagList, item_list)
#  }
#})
#
#labels_covvis<- reactive ({
#  
#  if (input$displayvis=="same") {labels_cov()}
#  
#  else {
#  
#  myList<-vector("list",length(myjson2()$cov$atlist))
#  for (i in 1:length(myjson2()$cov$atlist)) {
#    myList[[i]]= input[[paste0('covvis', i)]][1]
#  }
#  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
#  final_list
#  }
#})
#
##Create the reactive input of states
#
#output$statesinputvis <- renderUI({
#  
#  if (input$displayvis=="same") return()
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
#    
#    item_list[[1+i]] <- textInput(paste0('statevis',i),title_choices_state[i], labels_state()[i])
#    
#  }
#  do.call(tagList, item_list)
#  }
#})
#
#labels_statevis<- reactive ({
#  
#  if (input$displayvis=="same") {labels_state()}
#  else {
#  
#  myList<-vector("list",length(myjson2()$P))
#  for (i in 1:length(myjson2()$P)) {
#    
#    myList[[i]]= input[[paste0('statevis', i)]][1]
#    
#  }
#  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
#  final_list
#  }
#})

##################################################################################
###################################################################################



data_V <- reactive ({
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
  vis=list()
  if (length(myjson2()$visit)==0) {return()}
  
  for(i in 1:length(myjson2()$visit)) {
    vis[[i]]=as.data.frame(t(data.frame(myjson2()$visit[i])))
    colnames(vis[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$visit)) {  
    vis[[i]]=as.data.frame(cbind(vis[[i]], timevar ,state=rep(i,nrow(vis[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_vis=list()
  data_vis[[1]]=vis[[1]]
  
  for (u in 2:(length(myjson2()$visit))) {
    data_vis[[u]]=rbind(vis[[u]],data_vis[[(u-1)]])
  }
  
  datav=data_vis[[length(myjson2()$visit)]]
  datav
})


data_V_uci <- reactive ({
  if(is.null(myjson2())) return()
  
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {v[i]=myjson2()$cov$atlist[i]}
  
  ## Different variable of probabilities for each covariate pattern
  vis_uci=list()
  
  if (length(myjson2()$visit_uci)==0) {return()}
  
  for(i in 1:length(myjson2()$visit_uci)) {
    vis_uci[[i]]=as.data.frame(t(data.frame(myjson2()$visit_uci[i])))
    colnames(vis_uci[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$visit_uci)) {  
    vis_uci[[i]]=as.data.frame(cbind(vis_uci[[i]], timevar ,state=rep(i,nrow(vis_uci[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_V_uci=list()
  data_V_uci[[1]]=vis_uci[[1]]
  
  for (u in 2:(length(myjson2()$visit_uci))) {
    data_V_uci[[u]]=rbind(vis_uci[[u]],data_V_uci[[(u-1)]])
  }
  datav_uci=data_V_uci[[length(myjson2()$visit_uci)]]
  datav_uci 
})

data_V_lci <- reactive ({
  if(is.null(myjson2())) return()
  
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {v[i]=myjson2()$cov$atlist[i]}
  
  ## Different variable of probabilities for each covariate pattern
  vis_lci=list()
  
  if (length(myjson2()$visit_lci)==0) {return()}
  
  for(i in 1:length(myjson2()$visit_lci)) {
    vis_lci[[i]]=as.data.frame(t(data.frame(myjson2()$visit_lci[i])))
    colnames(vis_lci[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$visit_lci)) {  
    vis_lci[[i]]=as.data.frame(cbind(vis_lci[[i]], timevar ,state=rep(i,nrow(vis_lci[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_V_lci=list()
  data_V_lci[[1]]=vis_lci[[1]]
  
  for (u in 2:(length(myjson2()$visit_lci))) {
    data_V_lci[[u]]=rbind(vis_lci[[u]],data_V_lci[[(u-1)]])
  }
  datav_lci=data_V_lci[[length(myjson2()$visit_lci)]]
  datav_lci 
})

data_V_st<-reactive  ({
  
  datanew=data_V()
  datanew$state_fac=ordered(c(rep("NA",nrow(datanew))), levels = labels_state() )
  
  for (o in 1:(length(myjson2()$visit))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$state[g]==o) {datanew$state_fac[g]=labels_state()[o] }  
    }
  }
  datanew
})

data_V_st_uci<-reactive  ({
  datanew=data_V_uci()
  datanew$state_fac=ordered(c(rep("NA",nrow(datanew))), levels = labels_state() )
  
  for (o in 1:(length(myjson2()$visit_uci))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$state[g]==o) {datanew$state_fac[g]=labels_state()[o] }  
    }
  }
  datanew
})

data_V_st_lci<-reactive  ({
  datanew=data_V_lci()
  datanew$state_fac=ordered(c(rep("NA",nrow(datanew))), levels = labels_state() )
  
  for (o in 1:(length(myjson2()$visit_lci))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$state[g]==o) {datanew$state_fac[g]=labels_state()[o]  }  
    }
  }
  datanew
})


data_V_d <- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_V_st()[,d],data_V_st()[,ncol(data_V_st())-2],data_V_st()[,ncol(data_V_st())-1],data_V_st()[,ncol(data_V_st())],rep(d,length(data_V_st()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_V_st())[d],length(data_V_st()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_v <- bind_rows(dlist, .id = "column_label")
  d_all_v
  
})

data_V_d_uci <- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_V_st_uci()[,d],data_V_st_uci()[,ncol(data_V_st_uci())-2],
                                data_V_st_uci()[,ncol(data_V_st_uci())-1],
                                data_V_st_uci()[,ncol(data_V_st_uci())],rep(d,length(data_V_st_uci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_V_st_uci())[d],length(data_V_st_uci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  d_all_v_uci <- bind_rows(dlist, .id = "column_label")
  d_all_v_uci
}) 

data_V_d_lci <- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_V_st_lci()[,d],data_V_st_lci()[,ncol(data_V_st_lci())-2],
                                data_V_st_lci()[,ncol(data_V_st_lci())-1],
                                data_V_st_lci()[,ncol(data_V_st_lci())],rep(d,length(data_V_st_lci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_V_st_lci())[d],length(data_V_st_lci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  d_all_v_lci <- bind_rows(dlist, .id = "column_label")
  d_all_v_lci
}) 

output$tickinputvisit <- renderUI({
  
  default_choices=c("black","blue1","brown1","chartreuse2","cyan1","darkgray","firebrick3",
                    "gold","darkorange2","lightsteelblue4","rosybrow2","violetred2",
                    "yellow2","yellowgreen","tan1","lightslateblue","khaki4")
  
  if (is.null(myjson2()))  return()
  item_list <- list()
  item_list[[1]] <- h2("Provide x axis range and ticks")
  item_list[[2]] <-numericInput("startvx","Start x at:",value=min(data_V_d()$timevar),min=0,max=max(data_V_d()$timevar) )
  item_list[[3]] <-numericInput("stepvx","step:",value=max(data_V_d()$timevar/10),min=0,max=max(data_V_d()$timevar))
  item_list[[4]] <-numericInput("endvx","End x at:",value =max(data_V_d()$timevar),min=0,max=max(data_V_d()$timevar))
  item_list[[5]] <-numericInput("stepvy","step at y axis:",value=0.2,min=0.001,max=1)
  item_list[[6]] <-numericInput("endvy","End y at:",value =1,min=0,max=1)
  item_list[[7]] <-numericInput("textsizevis",h2("Legends size"),value=input$textsize,min=5,max=30)
  item_list[[8]] <-numericInput("textfacetvis",h2("Facet title size"),value=input$textsize-3,min=5,max=30 )
  
  do.call(tagList, item_list)
})



data_V_ci<- reactive ({
  x=c( data_V_d()[order(data_V_d()$timevar,data_V_d()$state,data_V_d()$cov),]$timevar,
       data_V_d_lci()[order(-data_V_d()$timevar,data_V_d()$state,data_V_d()$cov),]$timevar )
  
  y_central=c( data_V_d()[order(data_V_d()$timevar,data_V_d()$state,data_V_d()$cov),]$V,
               data_V_d()[order(-data_V_d()$timevar,data_V_d()$state,data_V_d()$cov),]$V )
  
  y=c( data_V_d_uci()[order(data_V_d_uci()$timevar,data_V_d_uci()$state,data_V_d_uci()$cov),]$V,
       data_V_d_lci()[order(-data_V_d_uci()$timevar,data_V_d_uci()$state,data_V_d_uci()$cov),]$V )
  
  frameto=c(as.character(data_V_d_uci()[order(-data_V_d_uci()$timevar,data_V_d_uci()$state,data_V_d_uci()$cov),]$state_factor),
            as.character(data_V_d_lci()[order(-data_V_d_lci()$timevar,data_V_d_lci()$state,data_V_d_lci()$cov),]$state_factor) )
  
  covto=c( data_V_d_uci()[order(-data_V_d_uci()$timevar,data_V_d_uci()$state,data_V_d_uci()$cov),]$cov_factor,
           data_V_d_lci()[order(-data_V_d_lci()$timevar,data_V_d_lci()$state,data_V_d_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,frameto,covto,y_central)
  data
})

######################################

#output$shouldloadvis1 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotvis1", label = h2("Download the plot"))
#})

datavis1_re <-  reactive ({
  
  
  ####### Plot 1 frame is state, factor is cov ########################
  
  
  if (input$confvis=="ci_no") {
    
    if (input$facetvis=="No") {
      
      
      vis_state= plot_ly(data_V_d(),alpha=0.5) %>%
        add_lines(
          x=data_V_d()$timevar,y=data_V_d()$V,
          frame=factor(as.factor(data_V_d()$state_factor),levels = labels_state()),
          color=factor(as.factor(data_V_d()$cov_factor),levels = labels_cov()),
          colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
          mode="lines",
          line=list(simplify=FALSE,color = labels_colour_cov()) ,
          text = 'Select or deselect lines by clicking on the legend',
          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")   )
      
      
      vis_state = vis_state %>%
        layout(title=list(text="Probability of visit for each covariate pattern among states",y=0.95),
               font= list(family = "times new roman", size = input$textsizevis, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepvx, 
                          tick0 = input$startvx, 
                          range=c(input$startvx,input$endvx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Probability of visit",rangemode = "nonnegative",                    
                           dtick = input$stepvy, 
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
      
      
      
      
    }
    
    
    if (input$facetvis=="Yes") {
      
      
      data_plot=data_V_d()
      
      vis_state = ggplot(data_plot)
      vis_state = ggplot(data_plot,aes(x=timevar, y=V, color=factor(as.factor(cov_factor),levels=labels_cov()), group=1,
                                       text=paste("Select or deselect lines by clicking on the legend",
                                                  "<br>Time: ", timevar,
                                                  "<br>Probability of visit: ", V,
                                                  "<br>Covariate pattern: ", factor(as.factor(cov_factor),levels=labels_cov()))))
      
      vis_state = vis_state+geom_line(aes(x=timevar, y=V, color= factor(as.factor(cov_factor),levels=labels_cov())))+
                                      scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      if (input$aimtype=="compare") { vis_state = vis_state+ facet_wrap(~ factor(as.factor(state_factor),levels = labels_state()), nrow=2)}
      
      else if (input$aimtype=="present")   {vis_state = vis_state+ facet_wrap(~factor(as.factor(state_factor),levels = labels_state()))}
      
      
      
      vis_state = vis_state + scale_x_continuous(breaks=c(seq(input$startvx,input$endvx,by=input$stepvx  ))) +
                                                 scale_y_continuous(breaks=c(seq(0,input$endvy,by=input$stepvy  )))
      
      vis_state = vis_state +labs(title="Probability of visit for each covariate pattern among states", x="Time since entry", y="Probability of visit")
      
      vis_state = vis_state + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      vis_state = vis_state +theme(title = element_text(size = input$textsizevis-4),  strip.text = element_text(size=input$textfacetvis),      
                                  legend.title = element_text(color="black", size= input$textsizevis-5), 
                                  legend.text=element_text(size= input$textsizevis-6),
                                  plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                  legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                  legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                  axis.title.y = element_text(size= input$textsizevis-5),
                                  axis.title.x = element_text(size= input$textsizevis-5), 
                                  axis.text.x = element_text( size=input$textsizevis-6),axis.text.y = element_text( size=input$textsizevis-6))  
      
      vis_state = ggplotly(vis_state, tooltip = "text")%>%
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
      
      vis_state 
    }
    
  }
  
  else if (input$confvis=="ci_yes") {
    
    if (input$facetvis=="No") {
      
      vis_state <- plot_ly()
      
      vis_state <- add_trace(vis_state, line=list(simplify=FALSE,color = labels_colour_cov()),
                             mode="lines", type = "scatter",
                             x=data_V_ci()$x, y=data_V_ci()$y_central,
                             frame=factor(as.factor(data_V_ci()$frameto),levels = labels_state()),
                             colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
                             color=factor(as.factor(data_V_ci()$covto) ,levels = labels_cov()),
                             text = 'Select or deselect lines by clicking on the legend',
                             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      vis_state <- add_trace(vis_state, fill = "tozerox", 
                             line=list(dash = "solid", color = "transparent", width = 1.8897637),
                             mode = "lines", type = "scatter",
                             x=data_V_ci()$x, y=data_V_ci()$y,
                             frame=factor(as.factor(data_V_ci()$frameto),levels = labels_state()),
                             colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
                             color=factor(as.factor(data_V_ci()$covto),levels = labels_cov()),
                             showlegend = FALSE,
                             text = 'Select or deselect lines by clicking on the legend',
                             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      
      vis_state = vis_state %>%
        layout(title=list(text="Probability of visit for each covariate pattern among states",y=0.95),
               font= list(family = "times new roman", size = input$textsizevis, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepvx, 
                          tick0 = input$startvx, 
                          range=c(input$startvx,input$endvx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Probability of visit",rangemode = "nonnegative",                    
                           dtick = input$stepvy, 
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
      
      
      
    }
    
    if (input$facetvis=="Yes") {
      
      V_lci=  data_V_d_lci()$V
      V_uci=  data_V_d_uci()$V
      
      data_plot=cbind(data_V_d(),V_lci,V_uci)
      
      
      vis_state=ggplot(data_plot)
      vis_state=ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(cov_factor),levels=labels_cov()), group=1,
                                     text=paste("Select or deselect lines by clicking on the legend",
                                                "<br>Time: ", timevar,
                                                "<br>Probability of visit: ", V,
                                                "<br>Covariate pattern: ", factor(as.factor(cov_factor),levels=labels_cov()))))+
        scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      
      vis_state=vis_state+geom_line(aes(x=timevar, y=V, fill= factor(as.factor(cov_factor),levels=labels_cov())))
      
      vis_state=vis_state+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill=factor(as.factor(cov_factor),levels=labels_cov())),alpha=0.4)+ 
        scale_fill_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      if (input$aimtype=="compare") { vis_state = vis_state+ facet_wrap(~factor(as.factor(state_factor),levels = labels_state()), nrow=2)}
      
      else if (input$aimtype=="present")   {vis_state = vis_state+ facet_wrap(~factor(as.factor(state_factor),levels = labels_state()))}
      
      
      vis_state = vis_state + scale_x_continuous(breaks=c(seq(input$startvx,input$endvx,by=input$stepvx  ))) +
        scale_y_continuous(breaks=c(seq(0,input$endvy,by=input$stepvy  )))
      
      vis_state = vis_state +labs(title="Probability of visit at each state", x="Time since entry", y="Probability of visit")
      
      vis_state = vis_state + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      vis_state = vis_state +theme(title = element_text(size = input$textsizevis-4),   strip.text = element_text(size=input$textfacetvis),       
                                   legend.title = element_text(color="black", size= input$textsizevis-5), 
                                   legend.text=element_text(size= input$textsizevis-6),
                                   plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                   legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                   legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                   axis.title.y = element_text(size= input$textsizevis-5),
                                   axis.title.x = element_text(size= input$textsizevis-5), 
                                   axis.text.x = element_text( size=input$textsizevis-6),axis.text.y = element_text( size=input$textsizevis-6))  
      
      vis_state = ggplotly(vis_state, tooltip = "text")%>%
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
      
      vis_state 
      
    }
  }
  vis_state  
})

output$visit_state <- renderPlotly ({ datavis1_re() })

output$downplotvis1 <- downloadHandler(
  filename = function(){paste("vis1",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datavis1_re(),width = 1400, height = 1100, format = "png", scale = 2,  out_file = file )
  }
)
#####################################

#output$shouldloadvis2 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotvis2", label = h2("Download the plot"))
#})

datavis2_re <-  reactive ({
  
  if (input$confvis=="ci_no") {
    
    if (input$facetvis=="No") {
      
      
      vis_cov= plot_ly(data_V_d(),alpha=0.5) %>%
        add_lines(
          x=data_V_d()$timevar,y=data_V_d()$V,
          frame=factor(as.factor(data_V_d()$cov_factor),levels = labels_cov()),
          color=factor(as.factor(data_V_d()$state_factor),levels = labels_state()),
          colors=labels_colour_state()[1:length(myjson2()$P)],
          mode="lines",
          line=list(simplify=FALSE,color = labels_colour_state())  ,
          text = 'Select or deselect lines by clicking on the legend',
          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")  )
      
      
      vis_cov = vis_cov %>%
        layout(title=list(text="Probability of visit for each state among covariate patterns",y=0.95),
               font= list(family = "times new roman", size = input$textsizevis, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepvx, 
                          tick0 = input$startvx, 
                          range=c(input$startvx,input$endvx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Probability of visit",rangemode = "nonnegative",                    
                           dtick = input$stepvy, 
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
        vis_cov= vis_cov %>%
          animation_opts(frame = 1000, transition = 0, redraw = FALSE)
      }
      
      
      
    }
    
    
    if (input$facetvis=="Yes") {
      
      
      data_plot=data_V_d()
      
      vis_cov = ggplot(data_plot)
      vis_cov = ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(state_factor),levels=labels_state()), group=1,
                                     text=paste("Select or deselect lines by clicking on the legend",
                                                "<br>Time: ", timevar,
                                                "<br>Probability of visit: ", V,
                                                "<br>State: ", factor(as.factor(state_factor),levels=labels_state()))))
      
      vis_cov = vis_cov+geom_line(aes(x=timevar, y=V, color= factor(as.factor(state_factor),levels=labels_state())))+
        scale_colour_manual( values =labels_colour_state(),labels = labels_state()  ) 
      

      if (input$aimtype=="compare") {vis_cov = vis_cov+ facet_wrap(~factor(as.factor(cov_factor),levels = labels_cov()),nrow=2)}
      
      else if (input$aimtype=="present")   {vis_cov = vis_cov+ facet_wrap(~factor(as.factor(cov_factor),levels = labels_cov()))  }
      
      vis_cov = vis_cov + scale_x_continuous(breaks=c(seq(input$startvx,input$endvx,by=input$endvx  )))  +
        scale_y_continuous(breaks=c(seq(0,input$endvy,by=input$stepvy  )))
      
      vis_cov = vis_cov +labs(title="Probability of visit for each state among covariate patterns", x="Time since entry", y="Probability of visit")
      
      vis_cov = vis_cov + labs(color = "States")+ labs(fill = "States")
      
      vis_cov = vis_cov+theme(title = element_text(size = input$textsizevis-4),  strip.text = element_text(size=input$textfacetvis),        
                              legend.title = element_text(color="black", size= input$textsizevis-5), 
                              legend.text=element_text(size= input$textsizevis-6),
                              plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                              legend.margin = margin(1.5, 1, 1, 1, "cm"),
                              legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                              axis.title.y = element_text(size= input$textsizevis-5),
                              axis.title.x = element_text(size= input$textsizevis-5), 
                              axis.text.x = element_text( size=input$textsizevis-6),axis.text.y = element_text( size=input$textsizevis-6))  
      
      vis_cov = ggplotly(vis_cov, tooltip = "text")%>%
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
      
      vis_cov 
    }
    
  }
  
  else if (input$confvis=="ci_yes") {
    
    if (input$facetvis=="No") {
      
      vis_cov <- plot_ly()
      
      vis_cov <- add_trace(vis_cov, line=list(simplify=FALSE,color = labels_colour_cov()),
                           mode="lines", type = "scatter",
                           x=data_V_ci()$x, y=data_V_ci()$y_central,
                           frame=factor(as.factor(data_V_ci()$covto),levels = labels_cov()), 
                           colors=labels_colour_state()[1:length(myjson2()$P)],
                           color=factor(as.factor(data_V_ci()$frameto) ,levels = labels_state()),
                           text = 'Select or deselect lines by clicking on the legend',
                           hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                 "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      vis_cov <- add_trace(vis_cov, fill = "tozerox", 
                           line=list(dash = "solid", color = "transparent", width = 1.8897637),
                           mode = "lines", type = "scatter",
                           x=data_V_ci()$x, y=data_V_ci()$y,
                           frame=factor(as.factor(data_V_ci()$covto),levels = labels_cov()), 
                           colors=labels_colour_state()[1:length(myjson2()$P)],
                           color=factor(as.factor(data_V_ci()$frameto) ,levels = labels_state()),
                           showlegend = FALSE,
                           text = 'Select or deselect lines by clicking on the legend',
                           hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                 "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      
      vis_cov = vis_cov %>%
        layout(title=list(text="Probability of visit for each state among covariate patterns",y=0.95),
               font= list(family = "times new roman", size = input$textsizevis, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepvx, 
                          tick0 = input$startvx, 
                          range=c(input$startvx,input$endvx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Probability of visit",rangemode = "nonnegative",                    
                           dtick = input$stepvy, 
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
        vis_cov= vis_cov %>%
          animation_opts(frame = 1000, transition = 0, redraw = FALSE)
      }
      
      vis_cov
    }
    
    if (input$facetvis=="Yes") {
      
      V_lci=  data_V_d_lci()$V
      V_uci=  data_V_d_uci()$V
      
      data_plot=cbind(data_V_d(),V_lci,V_uci)
      
      
      vis_cov=ggplot(data_plot)
      vis_cov=ggplot(data_plot,aes(x=timevar, y=V, color=factor(as.factor(state_factor),levels=labels_state()), group=1,
                                   text=paste("Select or deselect lines by clicking on the legend",
                                              "<br>Time: ", timevar,
                                              "<br>Probability of visit: ", V,
                                              "<br>State: ",  factor(as.factor(state_factor),levels=labels_state()))))+
        scale_colour_manual( values =labels_colour_state(),labels = labels_state()  ) 
      
      
      vis_cov=vis_cov+geom_line(aes(x=timevar, y=V, fill= factor(as.factor(state_factor),levels=labels_state())))
      
      vis_cov=vis_cov+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill=factor(as.factor(state_factor),levels=labels_state())),alpha=0.4)+ 
        scale_fill_manual( values =labels_colour_state(),labels = labels_state()  ) 
      
      
      
      
      if (input$aimtype=="compare") {vis_cov = vis_cov+ facet_wrap(~factor(as.factor(cov_factor),levels = labels_cov()),nrow=2)}
      
      else if (input$aimtype=="present")   {vis_cov = vis_cov+ facet_wrap(~factor(as.factor(cov_factor),levels = labels_cov()))  }
      
      
      vis_cov = vis_cov + scale_x_continuous(breaks=c(seq(input$startvx,input$endvx,by=input$stepvx  ))) 
      vis_cov = vis_cov +labs(title="Probability of visit for each state among covariate patterns", x="Time since entry", y="Probability of visit")
      
      vis_cov = vis_cov + labs(color = "States")+ labs(fill = "States")
      
      vis_cov = vis_cov+theme(title = element_text(size = input$textsizevis-4),  strip.text = element_text(size=input$textfacetvis),        
                              legend.title = element_text(color="black", size= input$textsizevis-5), 
                              legend.text=element_text(size= input$textsizevis-6),
                              plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                              legend.margin = margin(1.5, 1, 1, 1, "cm"),
                              legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                              axis.title.y = element_text(size= input$textsizevis-5),
                              axis.title.x = element_text(size= input$textsizevis-5), 
                              axis.text.x = element_text( size=input$textsizevis-6),axis.text.y = element_text( size=input$textsizevis-6))  
      
      vis_cov = ggplotly(vis_cov, tooltip = "text")%>%
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
      
      vis_cov 
      
    }
  }
  vis_cov 
}) 

output$visit_cov <- renderPlotly ({ datavis2_re() })

output$downplotvis2 <- downloadHandler(
  filename = function(){paste("vis2",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datavis2_re(),width = 1400, height = 1100, format = "png", scale = 2,  out_file = file )
  }
)
  
##########################################


#output$shouldloadvis3 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotvis3", label = h2("Download the plot"))
#})

datavis3_re <-  reactive ({
  
  
  if (input$confvis=="ci_no") {
    
    
    ####### Plot 3 f factor is state and cov ########################
    
    v_cov_state = plot_ly(data_V_d(),alpha=0.5) %>%
      add_lines(
        x=data_V_d()$timevar,y=data_V_d()$V,
        color = as.factor(data_V_d()$cov_factor),
        fill =as.factor(data_V_d()$state_factor),
        linetype=as.factor(data_V_d()$state_factor),
        mode="lines",
        line=list(simplify=FALSE),
        text = 'Select or deselect lines by clicking on the legend',
        hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                              "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
      )   
    
  }
  
  else if (input$confvis=="ci_yes") {
    
    v_cov_state <- plot_ly()
    
    v_cov_state  <- add_trace(v_cov_state, line=list(simplify=FALSE),
                              mode="lines", type = "scatter",
                              x=data_V_ci()$x, y=data_V_ci()$y_central,
                              color=as.factor(data_V_ci()$covto), 
                              fill=as.factor(data_V_ci()$frameto),
                              linetype=as.factor(data_V_ci()$frameto),
                              text = 'Select or deselect lines by clicking on the legend',
                              hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                    "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
    
    v_cov_state  <- add_trace(v_cov_state, fill = "tozerox", 
                              line=list(dash = "solid", color = "transparent", width = 1.8897637),
                              mode = "lines", type = "scatter",
                              x=data_V_ci()$x, y=data_V_ci()$y,
                              color=as.factor(data_V_ci()$covto),
                              fill=as.factor(data_V_ci()$frameto),
                              linetype=as.factor(data_V_ci()$frameto),
                              showlegend = FALSE,
                              text = 'Select or deselect lines by clicking on the legend',
                              hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                    "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
    )  
  }
  v_cov_state=v_cov_state %>%
      layout(title=list(text="Probability of state visit",y=0.95),
             font= list(family = "times new roman", size = input$textsizevis, color = "black"),
             margin = list(l = 50, r = 50, b = 30, t = 70),
             xaxis=list(title=list(text="Time since entry",y=0.2),
                        dtick = input$stepvx, 
                        tick0 = input$startvx, 
                        range=c(input$startvx,input$endvx),
                        ticklen = 5,
                        tickwidth = 2,
                        tickcolor = toRGB("black"),
                        tickmode = "linear"),
             yaxis =list(title= "Probability of visit",rangemode = "nonnegative",                    
                         dtick = input$stepvy, 
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
  
}) 

output$visit_both <- renderPlotly ({ datavis3_re() })
  
output$downplotvis3 <- downloadHandler(
  filename = function(){paste("vis3",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datavis3_re(),width = 1400, height = 1100, format = "png", scale = 2,  out_file = file )
  }
)
##############################################################################################
###############   Diff #######################################################################

data_V_diff1 <- reactive ({
  visit_diff=list()
  
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_diff= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$visitd)) {
      visit_diff[[i]]=as.data.frame(t(data.frame(myjson2()$visitd[i])))
      colnames(visit_diff[[i]]) <- v_diff
    }
    for(i in 1:length(myjson2()$visitd)) {  
      visit_diff[[i]]=as.data.frame(cbind(visit_diff[[i]], timevar ,state=rep(i,nrow(visit_diff[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$visitd)) {
      visit_diff[[i]]=as.data.frame(myjson2()$visitd[[i]][,1])
    }
    for (i in 1:length(myjson2()$visitd)) {  
      visit_diff[[i]]=as.data.frame(c(visit_diff[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$visitd[[i]][,1])) )) )
      colnames(visit_diff[[i]])[1:(length(myjson2()$atlist)-1)] <- v_diff
    }
  }
  
  # Append the probabilities datasets of the different states
  data_visitd=list()
  data_visitd[[1]]=visit_diff[[1]]
  
  for (u in 2:(length(myjson2()$visitd))) {
    data_visitd[[u]]=rbind(visit_diff[[u]],data_visitd[[(u-1)]])
  }
  
  datavd=data_visitd[[length(myjson2()$visitd)]]
  datavd$state_fac=c(rep("NA",nrow(datavd)))
  
  for (o in 1:(length(myjson2()$visitd))) {
    for (g in 1:nrow(datavd))  {
      if  (datavd$state[g]==o) {datavd$state_fac[g]=labels_state()[o]}  
    }
  }
  datavd
}) 



data_V_diff1_uci <- reactive ({
  V_diff_uci=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_diff= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$visitd_uci)) {
      V_diff_uci[[i]]=as.data.frame(t(data.frame(myjson2()$visitd_uci[i])))
      colnames(V_diff_uci[[i]]) <- v_diff
    }
    for(i in 1:length(myjson2()$visitd_uci)) {  
      V_diff_uci[[i]]=as.data.frame(cbind(V_diff_uci[[i]], timevar ,state=rep(i,nrow(V_diff_uci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$visitd_uci)) {
      V_diff_uci[[i]]=as.data.frame(myjson2()$visitd_uci[[i]][,1])
    }
    for (i in 1:length(myjson2()$visitd_uci)) {  
      V_diff_uci[[i]]=as.data.frame(c(V_diff_uci[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$visitd_uci[[i]][,1])) )) )
      colnames(V_diff_uci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_diff
    }
  }
  
  # Append the probabilities datasets of the different states
  data_visitd_uci=list()
  data_visitd_uci[[1]]=V_diff_uci[[1]]
  
  for (u in 2:(length(myjson2()$visitd_uci))) {
    data_visitd_uci[[u]]=rbind(V_diff_uci[[u]],data_visitd_uci[[(u-1)]])
  }
  
  datavd_uci=data_visitd_uci[[length(myjson2()$visitd_uci)]]
  datavd_uci$state_fac=c(rep("NA",nrow(datavd_uci)))
  
  for (o in 1:(length(myjson2()$visitd_uci))) {
    for (g in 1:nrow(datavd_uci))  {
      if  (datavd_uci$state[g]==o) {datavd_uci$state_fac[g]=labels_state()[o]}  
    }
  }
  datavd_uci
}) 


data_V_diff1_lci <- reactive ({
  V_diff_lci=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_diff= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$visitd_lci)) {
      V_diff_lci[[i]]=as.data.frame(t(data.frame(myjson2()$visitd_lci[i])))
      colnames(V_diff_lci[[i]]) <- v_diff
    }
    for(i in 1:length(myjson2()$visitd_lci)) {  
      V_diff_lci[[i]]=as.data.frame(cbind(V_diff_lci[[i]], timevar ,state=rep(i,nrow(V_diff_lci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$visitd_lci)) {
      V_diff_lci[[i]]=as.data.frame(myjson2()$visitd_lci[[i]][,1])
    }
    for (i in 1:length(myjson2()$visitd_lci)) {  
      V_diff_lci[[i]]=as.data.frame(c(V_diff_lci[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$visitd_lci[[i]][,1])) )) )
      colnames(V_diff_lci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_diff
    }
  }
  
  # Append the probabilities datasets of the different states
  data_visitd_lci=list()
  data_visitd_lci[[1]]=V_diff_lci[[1]]
  
  for (u in 2:(length(myjson2()$visitd_lci))) {
    data_visitd_lci[[u]]=rbind(V_diff_lci[[u]],data_visitd_lci[[(u-1)]])
  }
  
  datavd_lci=data_visitd_lci[[length(myjson2()$visitd_lci)]]
  datavd_lci$state_fac=c(rep("NA",nrow(datavd_lci)))
  
  for (o in 1:(length(myjson2()$visitd_lci))) {
    for (g in 1:nrow(datavd_lci))  {
      if  (datavd_lci$state[g]==o) {datavd_lci$state_fac[g]=labels_state()[o]}  
    }
  }
  datavd_lci
}) 

data_V_diff2<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    
    
    dlist[[d]]=cbind.data.frame(data_V_diff1()[,d],data_V_diff1()[,ncol(data_V_diff1())-2],data_V_diff1()[,ncol(data_V_diff1())-1],
                                data_V_diff1()[,ncol(data_V_diff1())],rep(d,length(data_V_diff1()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_V_diff1())[d],length(data_V_diff1()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_Vd <- bind_rows(dlist, .id = "column_Vabel")
  d_all_Vd
}) 



data_V_diff2_uci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_V_diff1_uci()[,d],data_V_diff1_uci()[,ncol(data_V_diff1_uci())-2],
                                data_V_diff1_uci()[,ncol(data_V_diff1_uci())-1],
                                data_V_diff1_uci()[,ncol(data_V_diff1_uci())],rep(d,length(data_V_diff1_uci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_V_diff1_uci())[d],length(data_V_diff1_uci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_Vd_uci <- bind_rows(dlist, .id = "column_Vabel")
  d_all_Vd_uci
}) 


data_V_diff2_lci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_V_diff1_lci()[,d],data_V_diff1_lci()[,ncol(data_V_diff1_lci())-2],
                                data_V_diff1_lci()[,ncol(data_V_diff1_lci())-1],
                                data_V_diff1_lci()[,ncol(data_V_diff1_lci())],rep(d,length(data_V_diff1_lci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_V_diff1_lci())[d],length(data_V_diff1_lci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_Vd_lci <- bind_rows(dlist, .id = "column_Vabel")
  d_all_Vd_lci
}) 


data_V_diff_ci<- reactive ({
  x=c( data_V_diff2()[order(data_V_diff2()$timevar,data_V_diff2()$state,data_V_diff2()$cov),]$timevar,
       data_V_diff2()[order(-data_V_diff2()$timevar,data_V_diff2()$state,data_V_diff2()$cov),]$timevar )
  
  y_central=c( data_V_diff2()[order(data_V_diff2()$timevar,data_V_diff2()$state,data_V_diff2()$cov),]$V,
               data_V_diff2()[order(-data_V_diff2()$timevar,data_V_diff2()$state,data_V_diff2()$cov),]$V )
  
  y=c( data_V_diff2_uci()[order(data_V_diff2_uci()$timevar,data_V_diff2_uci()$state,data_V_diff2_uci()$cov),]$V,
       data_V_diff2_lci()[order(-data_V_diff2_lci()$timevar,data_V_diff2_lci()$state,data_V_diff2_lci()$cov),]$V )
  
  frameto=c(as.character(data_V_diff2_uci()[order(-data_V_diff2_uci()$timevar,data_V_diff2_uci()$state,data_V_diff2_uci()$cov),]$state_factor),
            as.character(data_V_diff2_lci()[order(-data_V_diff2_lci()$timevar,data_V_diff2_lci()$state,data_V_diff2_lci()$cov),]$state_factor) )
  
  covto=c( data_V_diff2_uci()[order(-data_V_diff2_uci()$timevar,data_V_diff2_uci()$state,data_V_diff2_uci()$cov),]$cov_factor,
           data_V_diff2_lci()[order(-data_V_diff2_lci()$timevar,data_V_diff2_lci()$state,data_V_diff2_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,frameto,covto,y_central)
  data
})



#output$shouldloadvis4 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotvis4", label = h2("Download the plot"))
#})

datavis4_re <-  reactive ({
  
  ax <- list( title = "",zeroline = FALSE,showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
  
  if (length(myjson2()$visitd) == 0| myjson2()$Nats==1 ) {             
    V_state_d= plot_ly() %>%
      layout(title=list(text="Not applicable- Only one covariate pattern specified",y=0.95),xaxis=ax, yaxis=ax)
    V_state_d
  } 
  
  else {
  
  if (input$confvis=="ci_no") {
    
    if (input$facetvis=="No") {
      
      
      V_state_d= plot_ly(data_V_diff2(),alpha=0.5) %>%
        add_lines(
          x=data_V_diff2()$timevar,y=data_V_diff2()$V,
          frame=factor(as.factor(data_V_diff2()$state_factor),levels=labels_state()),
          color=as.factor(data_V_diff2()$cov_factor),
          colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
          mode="lines",
          line=list(simplify=FALSE),
          text = 'Select or deselect lines by clicking on the legend',
          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
        )  %>%
        layout(title=list(text="Difference in visit probabilities among covariate patterns (compared to reference)",y=0.95),
               font= list(family = "times new roman", size = input$textsizevis, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepvx, 
                          tick0 = input$startvx, 
                          range=c(input$startvx,input$endvx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Difference in visit probabilities",                    
                           dtick = input$stepvy, 
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
      
      V_state_d
      
      
    }
    
    if (input$facetvis=="Yes") {
      
      
      data_plot=data_V_diff2()
      
      V_state_d = ggplot(data_plot)
      V_state_d = ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(cov_factor) )))
      
      V_state_d = V_state_d+geom_line(aes(x=timevar, y=V, color= factor(as.factor(cov_factor) ), group=1,
                                          text=paste("Select or deselect lines by clicking on the legend",
                                                     "<br>Time: ", timevar,
                                                     "<br>Differences in probability of visit: ", V,
                                                     "<br>Covariate pattern: ", factor(as.factor(cov_factor) ))))+
        scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      if (input$aimtype=="compare") { V_state_d = V_state_d+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()), nrow=2)}
      
      else if (input$aimtype=="present")   {V_state_d = V_state_d+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()))}
      
      V_state_d = V_state_d + scale_x_continuous(breaks=c(seq(input$startvx,input$endvx,by=input$endvx  ))) 
      
      V_state_d = V_state_d +labs(title="Differences in probability of visit among covariate patterns (compared to reference)",
                                  x="Time since entry", y="Differences in probability of visit")
      
      V_state_d = V_state_d + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      V_state_d = V_state_d +theme(title = element_text(size = input$textsizevis-4),  strip.text = element_text(size=input$textfacetvis),        
                                   legend.title = element_text(color="black", size= input$textsizevis-5), 
                                   legend.text=element_text(size= input$textsizevis-6),
                                   plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                   legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                   legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                   axis.title.y = element_text(size= input$textsizevis-5),
                                   axis.title.x = element_text(size= input$textsizevis-5), 
                                   axis.text.x = element_text( size=input$textsizevis-6),axis.text.y = element_text( size=input$textsizevis-6))  
      
      V_state_d = ggplotly(V_state_d, tooltip = "text")%>%
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
      
      V_state_d 
    }
    
  }
  
  else if (input$confvis=="ci_yes") {
    
    
    if (input$facetvis=="No") {
      
      V_state_d <- plot_ly()
      
      V_state_d <- add_trace(V_state_d, line=list(simplify=FALSE),
                             mode="lines", type = "scatter",
                             x=data_V_diff_ci()$x, y=data_V_diff_ci()$y_central,
                             frame=factor(as.factor(data_V_diff_ci()$frameto),levels=labels_state()), 
                             colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                             color=as.factor(data_V_diff_ci()$covto),
                             text = 'Select or deselect lines by clicking on the legend',
                             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") )
      
      V_state_d <- add_trace(V_state_d, fill = "tozerox", 
                             line=list(dash = "solid", color = "transparent", width = 1.8897637),
                             mode = "lines", type = "scatter",
                             x=data_V_diff_ci()$x, y=data_V_diff_ci()$y,
                             frame=factor(as.factor(data_V_diff_ci()$frameto),levels=labels_state()),
                             colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                             color=as.factor(data_V_diff_ci()$covto),
                             showlegend = FALSE,
                             text = 'Select or deselect lines by clicking on the legend',
                             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      V_state_d= V_state_d   %>%
        layout(title=list(text="Differences in probability of visit among covariate patterns (compared to reference)",y=0.95),
               font= list(family = "times new roman", size = input$textsizevis, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepvx, 
                          tick0 = input$startvx, 
                          range=c(input$startvx,input$endvx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Differences in probability of visit",                    
                           dtick = input$stepvy, 
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
      V_state_d
      
    }
    
    if (input$facetvis=="Yes") {
      
      V_lci=  data_V_diff2_lci()$V
      V_uci=  data_V_diff2_uci()$V
      
      data_plot=cbind(data_V_diff2(),V_lci,V_uci)
      
      
      V_state_d=ggplot(data_plot)
      V_state_d=ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(cov_factor) )))+
        scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      
      V_state_d=V_state_d+geom_line(aes(x=timevar, y=V, fill= factor(as.factor(cov_factor) ), group=1,
                                        text=paste("Select or deselect lines by clicking on the legend",
                                                   "<br>Time: ", timevar,
                                                   "<br>Differences in probability of visit: ", V,
                                                   "<br>Covariate pattern: ",  factor(as.factor(cov_factor) ))))
      
      V_state_d=V_state_d+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill=factor(as.factor(cov_factor) )),alpha=0.4)+ 
        scale_fill_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      if (input$aimtype=="compare") { V_state_d = V_state_d+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()), nrow=2)}
      
      else if (input$aimtype=="present") {V_state_d = V_state_d+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()) )}
      
                              
      V_state_d = V_state_d + scale_x_continuous(breaks=c(seq(input$startvx,input$endvx,by=input$endvx  ))) 
      V_state_d = V_state_d +labs(title="Differences in probability of visit among covariate patterns (compared to reference)",
                                  x="Time since entry", y="Differences in probability of visit")
      
      V_state_d = V_state_d + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      V_state_d = V_state_d +theme(title = element_text(size = input$textsizevis-4),  strip.text = element_text(size=input$textfacetvis),        
                                   legend.title = element_text(color="black", size= input$textsizevis-5), 
                                   legend.text=element_text(size= input$textsizevis-6),
                                   plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                   legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                   legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                   axis.title.y = element_text(size= input$textsizevis-5),
                                   axis.title.x = element_text(size= input$textsizevis-5), 
                                   axis.text.x = element_text( size=input$textsizevis-6),axis.text.y = element_text( size=input$textsizevis-6))   
      
      V_state_d = ggplotly(V_state_d, tooltip = "text")%>%
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
  
  
  V_state_d
  }
  
})  

output$visit_diff <- renderPlotly ({ datavis4_re() })

output$downplotvis4 <- downloadHandler(
  filename = function(){paste("vis4",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datavis4_re(),width = 1400, height = 1100, format = "png", scale = 2,  out_file = file )
  }
)
  
######################################################################################
#####################################################################################
data_V_ratio1 <- reactive ({
  visit_ratio=list()
  
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_ratio= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_ratio[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$visitr)) {
      visit_ratio[[i]]=as.data.frame(t(data.frame(myjson2()$visitr[i])))
      colnames(visit_ratio[[i]]) <- v_ratio
    }
    for(i in 1:length(myjson2()$visitr)) {  
      visit_ratio[[i]]=as.data.frame(cbind(visit_ratio[[i]], timevar ,state=rep(i,nrow(visit_ratio[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$visitr)) {
      visit_ratio[[i]]=as.data.frame(myjson2()$visitr[[i]][,1])
    }
    for (i in 1:length(myjson2()$visitr)) {  
      visit_ratio[[i]]=as.data.frame(c(visit_ratio[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$visitr[[i]][,1])) )) )
      colnames(visit_ratio[[i]])[1:(length(myjson2()$atlist)-1)] <- v_ratio
    }
  }
  
  # Append the probabilities datasets of the different states
  data_Vosr=list()
  data_Vosr[[1]]=visit_ratio[[1]]
  
  for (u in 2:(length(myjson2()$visitr))) {
    data_Vosr[[u]]=rbind(visit_ratio[[u]],data_Vosr[[(u-1)]])
  }
  
  datalr=data_Vosr[[length(myjson2()$visitr)]]
  datalr$state_fac=c(rep("NA",nrow(datalr)))
  
  for (o in 1:(length(myjson2()$visitr))) {
    for (g in 1:nrow(datalr))  {
      if  (datalr$state[g]==o) {datalr$state_fac[g]=labels_state()[o]}  
    }
  }
  datalr
}) 


data_V_ratio1_uci <- reactive ({
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
    for(i in 1:length(myjson2()$visitr_uci)) {
      L_ratio_uci[[i]]=as.data.frame(t(data.frame(myjson2()$visitr_uci[i])))
      colnames(L_ratio_uci[[i]]) <- v_ratio
    }
    for(i in 1:length(myjson2()$visitr_uci)) {  
      L_ratio_uci[[i]]=as.data.frame(cbind(L_ratio_uci[[i]], timevar ,state=rep(i,nrow(L_ratio_uci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$visitr_uci)) {
      L_ratio_uci[[i]]=as.data.frame(myjson2()$visitr_uci[[i]][,1])
    }
    for (i in 1:length(myjson2()$visitr_uci)) {  
      L_ratio_uci[[i]]=as.data.frame(c(L_ratio_uci[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$visitr_uci[[i]][,1])) )) )
      colnames(L_ratio_uci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_ratio
    }
  }
  
  # Append the probabilities datasets of the ratioerent states
  data_Vosr_uci=list()
  data_Vosr_uci[[1]]=L_ratio_uci[[1]]
  
  for (u in 2:(length(myjson2()$visitr_uci))) {
    data_Vosr_uci[[u]]=rbind(L_ratio_uci[[u]],data_Vosr_uci[[(u-1)]])
  }
  
  datalr_uci=data_Vosr_uci[[length(myjson2()$visitr_uci)]]
  datalr_uci$state_fac=c(rep("NA",nrow(datalr_uci)))
  
  for (o in 1:(length(myjson2()$visitr_uci))) {
    for (g in 1:nrow(datalr_uci))  {
      if  (datalr_uci$state[g]==o) {datalr_uci$state_fac[g]=labels_state()[o]}  
    }
  }
  datalr_uci
}) 

data_V_ratio1_lci <- reactive ({
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
    for(i in 1:length(myjson2()$visitr_lci)) {
      L_ratio_lci[[i]]=as.data.frame(t(data.frame(myjson2()$visitr_lci[i])))
      colnames(L_ratio_lci[[i]]) <- v_ratio
    }
    for(i in 1:length(myjson2()$visitr_lci)) {  
      L_ratio_lci[[i]]=as.data.frame(cbind(L_ratio_lci[[i]], timevar ,state=rep(i,nrow(L_ratio_lci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$visitr_lci)) {
      L_ratio_lci[[i]]=as.data.frame(myjson2()$visitr_lci[[i]][,1])
    }
    for (i in 1:length(myjson2()$visitr_lci)) {  
      L_ratio_lci[[i]]=as.data.frame(c(L_ratio_lci[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$visitr_lci[[i]][,1])) )) )
      colnames(L_ratio_lci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_ratio
    }
  }
  # Append the probabilities datasets of the ratioerent states
  data_Vosr_lci=list()
  data_Vosr_lci[[1]]=L_ratio_lci[[1]]
  
  for (u in 2:(length(myjson2()$visitr_lci))) {
    data_Vosr_lci[[u]]=rbind(L_ratio_lci[[u]],data_Vosr_lci[[(u-1)]])
  }
  
  datalr_lci=data_Vosr_lci[[length(myjson2()$visitr_lci)]]
  datalr_lci$state_fac=c(rep("NA",nrow(datalr_lci)))
  
  for (o in 1:(length(myjson2()$visitr_lci))) {
    for (g in 1:nrow(datalr_lci))  {
      if  (datalr_lci$state[g]==o) {datalr_lci$state_fac[g]=labels_state()[o]}  
    }
  }
  datalr_lci
}) 


data_V_ratio2<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_V_ratio1()[,d],data_V_ratio1()[,ncol(data_V_ratio1())-2],data_V_ratio1()[,ncol(data_V_ratio1())-1],
                                data_V_ratio1()[,ncol(data_V_ratio1())],rep(d,length(data_V_ratio1()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_V_ratio1())[d],length(data_V_ratio1()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_Vr <- bind_rows(dlist, .id = "column_Vabel")
  d_all_Vr
}) 


data_V_ratio2_uci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_V_ratio1_uci()[,d],data_V_ratio1_uci()[,ncol(data_V_ratio1_uci())-2],
                                data_V_ratio1_uci()[,ncol(data_V_ratio1_uci())-1],
                                data_V_ratio1_uci()[,ncol(data_V_ratio1_uci())],rep(d,length(data_V_ratio1_uci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_V_ratio1_uci())[d],length(data_V_ratio1_uci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_Vr_uci <- bind_rows(dlist, .id = "column_Vabel")
  d_all_Vr_uci
}) 


data_V_ratio2_lci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_V_ratio1_lci()[,d],data_V_ratio1_lci()[,ncol(data_V_ratio1_lci())-2],
                                data_V_ratio1_lci()[,ncol(data_V_ratio1_lci())-1],
                                data_V_ratio1_lci()[,ncol(data_V_ratio1_lci())],rep(d,length(data_V_ratio1_lci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_V_ratio1_lci())[d],length(data_V_ratio1_lci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_Vr_lci <- bind_rows(dlist, .id = "column_Vabel")
  d_all_Vr_lci
}) 


data_V_ratio_ci<- reactive ({
  x=c( data_V_ratio2()[order(data_V_ratio2()$timevar,data_V_ratio2()$state,data_V_ratio2()$cov),]$timevar,
       data_V_ratio2()[order(-data_V_ratio2()$timevar,data_V_ratio2()$state,data_V_ratio2()$cov),]$timevar )
  
  y_central=c( data_V_ratio2()[order(data_V_ratio2()$timevar,data_V_ratio2()$state,data_V_ratio2()$cov),]$V,
               data_V_ratio2()[order(-data_V_ratio2()$timevar,data_V_ratio2()$state,data_V_ratio2()$cov),]$V )
  
  y=c( data_V_ratio2_uci()[order(data_V_ratio2_uci()$timevar,data_V_ratio2_uci()$state,data_V_ratio2_uci()$cov),]$V,
       data_V_ratio2_lci()[order(-data_V_ratio2_lci()$timevar,data_V_ratio2_lci()$state,data_V_ratio2_lci()$cov),]$V )
  
  frameto=c(as.character(data_V_ratio2_uci()[order(-data_V_ratio2_uci()$timevar,data_V_ratio2_uci()$state,data_V_ratio2_uci()$cov),]$state_factor),
            as.character(data_V_ratio2_lci()[order(-data_V_ratio2_lci()$timevar,data_V_ratio2_lci()$state,data_V_ratio2_lci()$cov),]$state_factor) )
  
  covto=c( data_V_ratio2_uci()[order(-data_V_ratio2_uci()$timevar,data_V_ratio2_uci()$state,data_V_ratio2_uci()$cov),]$cov_factor,
           data_V_ratio2_lci()[order(-data_V_ratio2_lci()$timevar,data_V_ratio2_lci()$state,data_V_ratio2_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,frameto,covto,y_central)
  data
})

#output$shouldloadvis5 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotvis5", label = h2("Download the plot"))
#})

datavis5_re <-  reactive ({
  
  ax <- list( title = "",zeroline = FALSE,showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
  
  if (length(myjson2()$visitr) == 0| myjson2()$Nats==1 ) {             
    V_state_r= plot_ly() %>%
      layout(title=list(text="Not applicable- Only one covariate pattern specified",y=0.95),xaxis=ax, yaxis=ax)
    V_state_r
  } 
  
  else {
  
  
  if (input$confvis=="ci_no") {
    
    if (input$facetvis=="No") {
      
      
      V_state_r= plot_ly(data_V_ratio2(),alpha=0.5) %>%
        add_lines(
          x=data_V_ratio2()$timevar,y=data_V_ratio2()$V,
          frame=factor(as.factor(data_V_ratio2()$state_factor),levels=labels_state()),
          color=as.factor(data_V_ratio2()$cov_factor),
          colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
          mode="lines",
          line=list(simplify=FALSE),
          text = 'Select or deselect lines by clicking on the legend',
          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
        )  %>%
        layout(title=list(text="Ratios in  visit probabilities among covariate patterns (compared to reference)",y=0.95),
               font= list(family = "times new roman", size = input$textsizevis, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepvx, 
                          tick0 = input$startvx, 
                          range=c(input$startvx,input$endvx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Ratios in visit probabilities",                    
                           dtick = input$stepvy, 
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
      
      V_state_r
    }
    
    if (input$facetvis=="Yes") {
      
      
      data_plot=data_V_ratio2()
      
      V_state_r = ggplot(data_plot)
      V_state_r = ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(cov_factor) ), group=1,
                                       text=paste("Select or deselect lines by clicking on the legend",
                                                  "<br>Time: ", timevar,
                                                  "<br>Ratio of length of stay: ", V,
                                                  "<br>Covariate pattern: ", factor(as.factor(cov_factor) ))))
      
      V_state_r = V_state_r+geom_line(aes(x=timevar, y=V, color= factor(as.factor(cov_factor) )))+
        scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      
      if (input$aimtype=="compare") { V_state_r = V_state_r+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()), nrow=2)}
      
      else if (input$aimtype=="present")   {V_state_r = V_state_r+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()))}   
      
      
      V_state_r = V_state_r + scale_x_continuous(breaks=c(seq(input$startvx,input$endvx,by=input$endvx  ))) 
      V_state_r = V_state_r +labs(title="Ratios in visit probabilities among covariate patterns (compared to reference)", x="Time since entry", y="Ratios in visit probabilities")
      
      V_state_r = V_state_r + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      V_state_r = V_state_r +theme(title = element_text(size = input$textsizevis-4),  strip.text = element_text(size=input$textfacetvis),        
                                   legend.title = element_text(color="black", size= input$textsizevis-5), 
                                   legend.text=element_text(size= input$textsizevis-6),
                                   plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                   legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                   legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                   axis.title.y = element_text(size= input$textsizevis-5),
                                   axis.title.x = element_text(size= input$textsizevis-5), 
                                   axis.text.x = element_text( size=input$textsizevis-6),axis.text.y = element_text( size=input$textsizevis-6))   
      
      V_state_r = ggplotly(V_state_r, tooltip = "text")%>%
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
      
      V_state_r 
    }
    
  }
  
  else if (input$confvis=="ci_yes") {
    
    
    if (input$facetvis=="No") {
      
      V_state_r <- plot_ly()
      
      V_state_r <- add_trace(V_state_r, line=list(simplify=FALSE),
                             mode="lines", type = "scatter",
                             x=data_V_ratio_ci()$x, y=data_V_ratio_ci()$y_central,
                             frame=factor(as.factor(data_V_ratio_ci()$frameto),levels=labels_state()), 
                             colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                             color=as.factor(data_V_ratio_ci()$covto) ,
                             text = 'Select or deselect lines by clicking on the legend',
                             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      V_state_r <- add_trace(V_state_r, fill = "tozerox", 
                             line=list(dash = "solid", color = "transparent", width = 1.8897637),
                             mode = "lines", type = "scatter",
                             x=data_V_ratio_ci()$x, y=data_V_ratio_ci()$y,
                             frame=factor(as.factor(data_V_ratio_ci()$frameto),levels=labels_state()),
                             colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                             color=as.factor(data_V_ratio_ci()$covto),
                             showlegend = FALSE,
                             text = 'Select or deselect lines by clicking on the legend',
                             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      V_state_r= V_state_r   %>%
        layout(title=list(text="Ratios in  visit probabilities among covariate patterns (compared to reference)",y=0.95),
               font= list(family = "times new roman", size = input$textsizevis, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepvx, 
                          tick0 = input$startvx, 
                          range=c(input$startvx,input$endvx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Ratios in visit probabilities",                    
                           dtick = input$stepvy, 
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
      
      V_state_r
    }
    
    if (input$facetvis=="Yes") {
      
      V_lci=  data_V_ratio2_lci()$V
      V_uci=  data_V_ratio2_uci()$V
      
      data_plot=cbind(data_V_ratio2(),V_lci,V_uci)
      
      
      V_state_r=ggplot(data_plot)
      V_state_r=ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(cov_factor) ), group=1,
                                     text=paste("Select or deselect lines by clicking on the legend",
                                                "<br>Time: ", timevar,
                                                "<br>Ratio of length of stay: ", V,
                                                "<br>Covariate pattern: ",  factor(as.factor(cov_factor) ))))+
        scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      
      V_state_r=V_state_r+geom_line(aes(x=timevar, y=V, fill= factor(as.factor(cov_factor) )))
      
      V_state_r=V_state_r+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill=factor(as.factor(cov_factor) )),alpha=0.4)+ 
        scale_fill_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      if (input$aimtype=="compare") { V_state_r = V_state_r+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()), nrow=2)}
      
      else if (input$aimtype=="present")   {V_state_r = V_state_r+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()),)}   
      
      V_state_r = V_state_r + scale_x_continuous(breaks=c(seq(input$startvx,input$endvx,by=input$endvx  ))) 
      V_state_r = V_state_r +labs(title="Ratios in visit probabilities among covariate patterns (compared to reference)", x="Time since entry", y="Ratios in visit probabilities")
      
      V_state_r = V_state_r + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      V_state_r = V_state_r +theme(title = element_text(size = input$textsizevis-4),  strip.text = element_text(size=input$textfacetvis),        
                                   legend.title = element_text(color="black", size= input$textsizevis-5), 
                                   legend.text=element_text(size= input$textsizevis-6),
                                   plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                   legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                   legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                   axis.title.y = element_text(size= input$textsizevis-5),
                                   axis.title.x = element_text(size= input$textsizevis-5), 
                                   axis.text.x = element_text( size=input$textsizevis-6),axis.text.y = element_text( size=input$textsizevis-6))   
      
      V_state_r = ggplotly(V_state_r, tooltip = "text")%>%
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
  
  
  V_state_r
  }  
}) 

output$visit_ratio <- renderPlotly ({  datavis5_re() })
  
output$downplotvis5 <- downloadHandler(
  filename = function(){paste("vis5",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datavis5_re(),width = 1400, height = 1100, format = "png", scale = 2,  out_file = file )
  }
)