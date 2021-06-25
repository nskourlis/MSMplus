


###### Show and hide transition names option, and tick inputs ####

timerh <- reactiveVal(1.5)

observeEvent(c(input$showtransname,invalidateLater(1000, session)), { 
  
  if(input$showtransname=="No"){
    
    hide("transinputh")
    
  }
  
  if(input$showtransname=="Yes"){
    
    show("transinputh")
    
  }
  
  isolate({
    
    timerh(timerh()-1)
    if(timerh()>1 & input$showtransname=="No")
    {
      show("transinputh")
    }
    
  })
})






observeEvent(c(input$showtickh,invalidateLater(1000, session)), { 
  
  if(input$showtickh=="No"){
    
    hide("tickinputh")
    hide("tickinputhlog")
    
  }
  
  if(input$showtickh=="Yes"){
    
    show("tickinputh")
    show("tickinputhlog")
    
  }
  
  isolate({
    
    timerh(timerh()-1)
    if(timerh()>1 & input$showtickh=="No")
    {
      show("tickinputh")
      show("tickinputhlog")
    }
    
  })
})

#observeEvent(input$tabsh, { 
#  
#  if( input$tabsh=="#panel1h"|input$tabsh=="#panel2h"|input$tabsh=="#panel3h"){
#    
#    hide("faceth")
#    hide("scaleh")
#    hide("showtransname")
#    hide("transinputh")
#    hide("showtickh")
#    hide("tickinputh")
#    hide("tickinputhlog")
#    show("faceth")
#    show("scaleh")
#    show("showtransname")
#    show("transinputh")
#    show("showtickh")
#    show("tickinputh")
#    show("tickinputhlog")
#    
#  }
#
#  
#  
#  if( input$tabsh=="#panel4h"){
#    
#    hide("faceth")
#    hide("scaleh")
#    hide("showtransname")
#    hide("transinputh")
#    hide("showtickh")
#    hide("tickinputh")
#    hide("tickinputhlog")
#    show("faceth")
#    show("scaleh")
#    show("transinputh")
#    show("showtickh")
#    show("tickinputh")
#    show("tickinputhlog")
#    
#  }
#  
#
#})

############################################################################

existh <- reactive({
  if (length(myjson2()$haz) != 0) {       
    x= 1
  }
  else if (length(myjson2()$haz) == 0) {       
    x= 0
  }
})


output$pageh <- renderUI({
  
if (input$aimtype=="compare") {return("Non applicable for comparison")}
  
else {
  
  if (is.null(myjson2())) return("Provide the json file with the predictions")
  
  if (existh()==0) {
    
    fluidRow(
  #    tags$style(type="text/css",
  #               ".shiny-output-error { visibility: hidden; }",
  #               ".shiny-output-error:before { visibility: hidden; }"
  #    ),
      column(12,
             output$loginpageh <- renderUI({h1("Non applicable")})
      )
    )
  }
  
  else if (existh()==1 & myjson2()$Nats>1) {
      
      
    fluidRow(
   #   tags$style(type="text/css",
   #              ".shiny-output-error { visibility: hidden; }",
   #              ".shiny-output-error:before { visibility: hidden; }"
   #   ),
      
      column(2,
             h1("Predictions hazards"),
            
             uiOutput("faceth"),
             
             conditionalPanel(condition="input.tabsh =='#panel1h'||input.tabsh =='#panel2h'||input.tabsh =='#panel5h'||input.tabsh =='#panel6h'",
                              uiOutput("confhaz"),
                              
             )  ,
          
             conditionalPanel(condition="input.tabsh =='#panel1h'||input.tabsh =='#panel2h'",
                              uiOutput("scaleh"),

             )  ,
             conditionalPanel(condition="input.tabsh =='#panel4h'",
                              uiOutput("scaleh_logdefault")
             )  ,
             

             uiOutput("showtransname"),
             uiOutput("transinputh")
             
             #radioButtons("displayh", "Change labels of states and covariate patterns",
             #             c("same" = "same", "change"= "change")),
             
             #uiOutput("covarinputh"),
      ),
      
      
      column(2,
             br(),
             p(""),
             uiOutput("origin"),
             uiOutput("showtickh"),
             uiOutput("tickinputh"),
             uiOutput("tickinputhlog")
             
      ),
      
      column(8,
             tabsetPanel(id = "tabsh",     
               tabPanel(h2("By transitions"),                               value = "#panel1h",plotlyOutput("hazards_trans", height="600px", width = "100%"),uiOutput("shouldloadh1")),
               tabPanel(h2("By covariate pattern"),                         value = "#panel2h",plotlyOutput("hazards_cov", height="600px", width = "100%"),uiOutput("shouldloadh4")),
               tabPanel(h2("Ratios of transition intensities"),             value = "#panel4h", plotlyOutput("hr_transitions", height="600px", width = "100%"),uiOutput("shouldloadh5")),
               tabPanel(h2("Hazard differences between covariate patterns"),value = "#panel5h", plotlyOutput("H_diff", height="600px", width = "100%"),uiOutput("shouldloadh2")),
               tabPanel(h2("Hazard ratios between covariate patterns"),     value = "#panel6h", plotlyOutput("H_ratio", height="600px", width = "100%"),uiOutput("shouldloadh3"))
               
               
                        
               )
             )  
             
      )
      
    
  }
  
  else if (existh()==1 & myjson2()$Nats==1) {
    
    
    fluidRow(
      column(2,
             h1("Predictions hazards"),
             
             uiOutput("faceth"),
             
             conditionalPanel(condition="input.tabsh =='#panel1h'||input.tabsh =='#panel2h'||input.tabsh =='#panel5h'||input.tabsh =='#panel6h'",
                              uiOutput("confhaz"),
                              
             )  ,
             
             conditionalPanel(condition="input.tabsh =='#panel1h'||input.tabsh =='#panel2h'",
                              uiOutput("scaleh"),

             )  ,
             conditionalPanel(condition="input.tabsh =='#panel4h'",
                              uiOutput("scaleh_logdefault")
             )  ,
             
             uiOutput("showtransname"),
             uiOutput("transinputh")
      ),
      
      column(2,
             br(),
             p(""),
             uiOutput("origin"),
             uiOutput("showtickh"),
             uiOutput("tickinputh"),
             uiOutput("tickinputhlog")
             
      ),
      
      column(8,
             tabsetPanel(id = "tabsh",        
               tabPanel(h2("By transitions"),      value = "#panel1h",                       plotlyOutput("hazards_trans", height="600px", width = "100%"),uiOutput("shouldloadh1")),
               tabPanel(h2("By covariate pattern"),   value = "#panel2h",             plotlyOutput("hazards_cov", height="600px", width = "100%"),uiOutput("shouldloadh4")),
               tabPanel(h2("Ratios of transition intensities"),value = "#panel4h", plotlyOutput("hr_transitions", height="600px", width = "100%"),uiOutput("shouldloadh5")) ,           
               tabPanel(h2("Hazard differences between covariate patterns"),                         value = "#panel5h", plotlyOutput("H_diff", height="600px", width = "100%"),uiOutput("shouldloadh2")),
               tabPanel(h2("Hazard ratios between covariate patterns"),                               value = "#panel6h", plotlyOutput("H_ratio", height="600px", width = "100%"),uiOutput("shouldloadh3"))
               
            )  
             
      )
      
    )
  }

}   
  
})   

output$origin <- renderUI({
  

  selectInput("origin",label="Time origin",
              choices= c("Time since study entry"="study","Time since state entry"="state" ), selected ="Time since study entry" )
  
  
  })


output$confhaz <- renderUI({
  
  if  (length(myjson2()$ci_haz)!=0) {
    radioButtons("confh", "Confidence intervals",
                 c("No" = "ci_no",
                   "Yes" ="ci_yes"))
  }
  
  else if (length(myjson2()$ci_haz)==0) {
    item_list <- list()
    item_list[[1]]<- radioButtons("confh", "Confidence intervals",c("No" = "ci_no"))
    item_list[[2]]<-print("Confidence interval data were not provided")
    do.call(tagList, item_list)
  }
  
})

output$showtransname <- renderUI({
radioButtons("showtransname", "Show transitions name options",
             choices = list("No" = "No",
                            "Yes" = "Yes"), selected = "No")
}) 

output$scaleh <- renderUI({
  radioButtons("scaleh", "Scale",
               c("Normal" = "normal",
                 "Log"    = "log"))
}) 


output$scaleh_logdefault <- renderUI({
  radioButtons("scaleh_ld", "Scale",
               c("Normal" = "normal",
                 "Log"    = "log"),selected="log")
}) 

output$faceth <- renderUI({
  
radioButtons(inputId="faceth", label= "Display graph in grids",
             choices=c("No","Yes"),selected = "No")
})

output$showtickh <- renderUI({
  
radioButtons("showtickh", "Show axis tick options",
             choices = list("No" = "No",
                            "Yes" = "Yes"), selected = "No")
})



observeEvent(input$json2, {
  if( length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'Haz')))==0  ) {
    js$disableTab("mytab_h")
    
  } 
}) 

observeEvent(input$csv2, {
  if( length(which(startsWith(names( read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'Haz')))==0 ) {
    js$disableTab("mytab_h")
    
  } 
}) 



observeEvent(input$aimtype, {
  if( input$aimtype=="compare" ) {
    js$disableTab("mytab_h")
  } 
  else if( input$aimtype=="present" ) {
    js$enableTab("mytab_h")
  } 
  
}) 
##############################################################################
##  Transition input ########################################################
##############################################################################
output$transinputh <- renderUI({
  
  if (is.null((myjson2()$haz)))  return()


  tmat_temp=myjson2()$tmat[as.numeric(input$select),]
  
  tr_start_state=vector()
  
  for (k in 1:length(which(!is.na(tmat_temp)))  ) {
  tr_start_state[k]=as.numeric(input$select)
  }
  
  tr_end_state=vector()
  for (k in 1:length(which(!is.na(tmat_temp)))  ) {
    tr_end_state[k]=which(!is.na(tmat_temp))[k]
  }
  
  default_choices_trans=vector()
  for (i in 1:length(which(!is.na(tmat_temp)))  )  {

      default_choices_trans[i]=paste0('Transition'," ", tr_start_state[i],"->",tr_end_state[i])  }
  
  item_list <- list()
  item_list[[1]] <- h2("Transitions specifics")
  
  for (i in 1:length(which(!is.na(tmat_temp))) ) {
    item_list[[i+1]] <- textInput(paste0('trans', i),default_choices_trans[i], default_choices_trans[i])
  }
  do.call(tagList, item_list)
})
#Transform the reactive input of covariates into easy to use laber reactive dataset

labels_trans<- reactive ({
  
  tmat_temp=myjson2()$tmat[as.numeric(input$select),]
  
  myList<-vector("list",length(which(!is.na(tmat_temp))) )
  for (i in 1:length(which(!is.na(tmat_temp))) ) {
    myList[[i]]= input[[paste0('trans', i)]][1]
  }
  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
  final_list
})
###############################################################################

##############################################################################
##  Covariate input ########################################################
##############################################################################

#output$covarinputh <- renderUI({
#  
#  if (is.null(myjson2()))  return()
#  
#  if (input$displayh=="same") return()
#  
#  else {
#  
#  item_list <- list()
#  item_list[[1]] <- h2("Covariate patterns")
#  
#  default_choices_cov=vector()
#  for (i in 1:length(myjson2()$cov$atlist)) {
#    default_choices_cov[i]=myjson2()$cov$atlist[i]
#  }
#  
#  for (i in seq(length(myjson2()$cov$atlist))) {
#    item_list[[i+1]] <- textInput(paste0('covh', i),default_choices_cov[i], labels_cov()[i])
#  }
#  
#  do.call(tagList, item_list)
#  }
#})
#labels_covh<- reactive ({
#  
#  if (input$displayh=="same") {labels_cov()}
#  
#  else {
#  
#  myList<-vector("list",length(myjson2()$cov$atlist))
#  for (i in 1:length(myjson2()$cov$atlist)) {
#    myList[[i]]= input[[paste0('covh', i)]][1]
#  }
#  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
#  final_list
#  }
#})


data_H <- reactive ({
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
  haz=list()
  
  if (length(myjson2()$haz)==0) {return()}
  
  for(i in 1:length(myjson2()$haz)) {
    
    haz[[i]]=as.data.frame(t(data.frame(myjson2()$haz[i])))
    colnames(haz[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$haz)) {  
    haz[[i]]=as.data.frame(cbind(haz[[i]], timevar ,trans=rep(i,nrow(haz[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_haz=list()
  data_haz[[1]]=haz[[1]]
  
  if (length(myjson2()$haz)>=2 ) {
     for (u in 2:(length(myjson2()$haz))) {
       data_haz[[u]]=rbind(haz[[u]],data_haz[[(u-1)]])
     }
  }
  
  datah=data_haz[[length(myjson2()$haz)]]
  
  
  datah
})


data_H_uci <- reactive ({
  if(is.null(myjson2())) return()
  
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {v[i]=myjson2()$cov$atlist[i]}
  
  ## Different variable of probabilities for each covariate pattern
  haz_uci=list()
  
  if (length(myjson2()$haz_uci)==0) {return()}
  
  for(i in 1:length(myjson2()$haz_uci)) {
    haz_uci[[i]]=as.data.frame(t(data.frame(myjson2()$haz_uci[i])))
    colnames(haz_uci[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$haz_uci)) {  
    haz_uci[[i]]=as.data.frame(cbind(haz_uci[[i]], timevar ,trans=rep(i,nrow(haz_uci[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_haz_uci=list()
  data_haz_uci[[1]]=haz_uci[[1]]
  
  if (length(myjson2()$haz_uci)>=2 ) {  
  for (u in 2:(length(myjson2()$haz_uci))) {
    data_haz_uci[[u]]=rbind(haz_uci[[u]],data_haz_uci[[(u-1)]])
  }
  }
  datah_uci=data_haz_uci[[length(myjson2()$haz_uci)]]
  datah_uci 
})

data_H_lci <- reactive ({
  if(is.null(myjson2())) return()
  
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {v[i]=myjson2()$cov$atlist[i]}
  
  ## Different variable of probabilities for each covariate pattern
  haz_lci=list()
  
  if (length(myjson2()$haz_lci)==0) {return()}
  
  for(i in 1:length(myjson2()$haz_lci)) {
    haz_lci[[i]]=as.data.frame(t(data.frame(myjson2()$haz_lci[i])))
    colnames(haz_lci[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$haz_lci)) {  
    haz_lci[[i]]=as.data.frame(cbind(haz_lci[[i]], timevar ,trans=rep(i,nrow(haz_lci[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_haz_lci=list()
  data_haz_lci[[1]]=haz_lci[[1]]
  
  if (length(myjson2()$haz_lci)>=2 ) {  
    
  for (u in 2:(length(myjson2()$haz_lci))) {
    data_haz_lci[[u]]=rbind(haz_lci[[u]],data_haz_lci[[(u-1)]])
  }
  }
  datah_lci=data_haz_lci[[length(myjson2()$haz_lci)]]
  datah_lci 
})


data_H_st<-reactive  ({
  
  datanew=data_H()
  datanew$trans_factor=ordered(c(rep("NA",nrow(datanew))), levels = labels_trans() )
  
  for (o in 1:(length(myjson2()$haz))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$trans[g]==o) {datanew$trans_factor[g]=labels_trans()[o] }  
    }
  }
  datanew
})

data_H_st_uci<-reactive  ({
  datanew=data_H_uci()
  datanew$trans_factor=ordered(c(rep("NA",nrow(datanew))), levels = labels_trans() )
  
  for (o in 1:(length(myjson2()$haz_uci))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$trans[g]==o) {datanew$trans_factor[g]=labels_trans()[o] }  
    }
  }
  datanew
})

data_H_st_lci<-reactive  ({
  datanew=data_H_lci()
  datanew$trans_factor=ordered(c(rep("NA",nrow(datanew))), levels = labels_trans() )
  
  for (o in 1:(length(myjson2()$haz_lci))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$trans[g]==o) {datanew$trans_factor[g]=labels_trans()[o]}  
    }
  }
  datanew
})

data_H_d <- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_H_st()[,d],data_H_st()[,ncol(data_H_st())-2],data_H_st()[,ncol(data_H_st())-1],data_H_st()[,ncol(data_H_st())],rep(d,length(data_H_st()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_H_st())[d],length(data_H_st()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","trans","trans_factor","cov","cov_factor")
  }
  
  d_all_h <- bind_rows(dlist, .id = "column_label")
  d_all_h
  
  
})

data_H_d_uci <- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_H_st_uci()[,d],data_H_st_uci()[,ncol(data_H_st_uci())-2],
                                data_H_st_uci()[,ncol(data_H_st_uci())-1],
                                data_H_st_uci()[,ncol(data_H_st_uci())],rep(d,length(data_H_st_uci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_H_st_uci())[d],length(data_H_st_uci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","trans","trans_factor","cov","cov_factor")
  }
  d_all_h_uci <- bind_rows(dlist, .id = "column_label")
  d_all_h_uci
}) 

data_H_d_lci <- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_H_st_lci()[,d],data_H_st_lci()[,ncol(data_H_st_lci())-2],
                                data_H_st_lci()[,ncol(data_H_st_lci())-1],
                                data_H_st_lci()[,ncol(data_H_st_lci())],rep(d,length(data_H_st_lci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_H_st_lci())[d],length(data_H_st_lci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","trans","trans_factor","cov","cov_factor")
  }
  d_all_h_lci <- bind_rows(dlist, .id = "column_label")
  d_all_h_lci
}) 

output$tickinputh <- renderUI({
  
  if (is.null(myjson2()))  return()
  
  default_choices=c("black","blue1","brown1","chartreuse2","cyan1","darkgray","firebrick3",
                    "gold","darkorange2","lightsteelblue4","rosybrow2","violetred2",
                    "yellow2","yellowgreen","tan1","lightslateblue","khaki4","gray28",
                    "cyan3","brown4","darkorchid1","goldenrod4","gray63","lightsalmon",
                    "maroon4","palegreen1","royalblue2","red2","sienna4","yellow4","slategray3")
  item_list <- list()
  item_list[[1]] <- h2("Provide x axis range and ticks")
  item_list[[2]] <-numericInput("starthx","Start x at:",value=min(data_H_d()$timevar),min=0 )
  item_list[[3]] <-numericInput("stephx","step:",value=max(data_H_d()$timevar/10),min=0,max=max(data_H_d()$timevar))
  item_list[[4]] <-numericInput("endhx","End x at:",value =max(data_H_d()$timevar),min=0,max=max(data_H_d()$timevar))
  item_list[[5]] <-numericInput("starthy","Start y at:",value=0,min=0 )
  item_list[[6]] <-numericInput("stephy","step at y axis:",value=data_H_d()$V[10],min=min(data_H_d()$V),max=max(data_H_d()$V))
  item_list[[7]] <-numericInput("endhy","end of y axis:",value=max(data_H_d()$V[which(!is.na(data_H_d()$V))]),min=0.0001)
  item_list[[8]] <-numericInput("textsizeh",h2("Legends size"),value=input$textsize,min=5,max=30)
  item_list[[9]] <-numericInput("textfaceth",h2("Facet title size"),value=input$textsize-3,min=5,max=30 )
  
  do.call(tagList, item_list)
})

output$tickinputhlog <- renderUI({
  
  if (is.null(myjson2()))  return()
  item_list <- list()
  item_list[[1]] <- h2("Provide y axis range and ticks for log scale")
  item_list[[2]] <-numericInput("logstarthy","Start log y at:",value=-10)
  item_list[[3]] <-numericInput("logstephy","step at log y axis:",value=1,min=0.01,max=10)
  item_list[[4]] <-numericInput("logendhy","End log y axis at:",value=1,max=20,min=-20)
  
  do.call(tagList, item_list)
})

data_H_ci<- reactive ({
  x=c( data_H_d()[order(data_H_d()$timevar,data_H_d()$trans,data_H_d()$cov),]$timevar,
       data_H_d_lci()[order(-data_H_d()$timevar,data_H_d()$trans,data_H_d()$cov),]$timevar )
  
  y_central=c( data_H_d()[order(data_H_d()$timevar,data_H_d()$trans,data_H_d()$cov),]$V,
               data_H_d()[order(-data_H_d()$timevar,data_H_d()$trans,data_H_d()$cov),]$V )
  
  y=c( data_H_d_uci()[order(data_H_d_uci()$timevar,data_H_d_uci()$trans,data_H_d_uci()$cov),]$V,
       data_H_d_lci()[order(-data_H_d_uci()$timevar,data_H_d_uci()$trans,data_H_d_uci()$cov),]$V )
  
  frameto=c(as.character(data_H_d_uci()[order(-data_H_d_uci()$timevar,data_H_d_uci()$trans,data_H_d_uci()$cov),]$trans_factor),
            as.character(data_H_d_lci()[order(-data_H_d_lci()$timevar,data_H_d_lci()$trans,data_H_d_lci()$cov),]$trans_factor) )
  
  covto=c( data_H_d_uci()[order(-data_H_d_uci()$timevar,data_H_d_uci()$trans,data_H_d_uci()$cov),]$cov_factor,
           data_H_d_lci()[order(-data_H_d_lci()$timevar,data_H_d_lci()$trans,data_H_d_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,frameto,covto,y_central)
  data
})


#########################################

#output$shouldloadh1 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downploth1", label = h2("Download the plot"))
#})

datah1_re <-  reactive ({

  
  if (input$confh=="ci_no") {
  
       if (is.null(myjson2()$is.cumhaz)==FALSE) {
            if (myjson2()$is.cumhaz==1) {hazname= "Cummulative hazards"}
             else if (myjson2()$is.cumhaz!=1)  {hazname= "Transition intensity rate"}
       }
       if (is.null(myjson2()$is.cumhaz)==TRUE) {hazname= "Transition intensity rate"}
    
    
    
      if (input$scaleh=="normal") {
    
            if (input$faceth=="No") {
  
                       h_trans= plot_ly(data_H_d(),alpha=0.5) %>%
                         add_lines(
                           x=data_H_d()$timevar,y=data_H_d()$V,
                           frame=as.factor(data_H_d()$trans_factor),
                           color=factor(as.factor(data_H_d()$cov_factor),levels=labels_cov()),
                           colors=labels_colour_cov(),
                           mode="lines",
                           line=list(simplify=FALSE),color = labels_colour_cov(),
                           text = 'Select or deselect lines by clicking on the legend',
                           hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                 "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") 
                         )  %>%
                         layout(title=list(text=paste0(hazname," for each covariate pattern among states"),y=0.95),
                                font= list(family = "times new roman", size = input$textsizeh, color = "black"),
                                margin = list(l = 50, r = 50, b = 30, t = 70),
                                xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
                                           dtick = input$stephx, 
                                           tick0 = input$starthx, 
                                           range=c(input$starthx,input$endhx),
                                           ticklen = 5,
                                           tickwidth = 2,
                                           tickcolor = toRGB("black"),
                                           tickmode = "linear"),
                                       yaxis =list(title= hazname,rangemode = "nonnegative",                    
                                           dtick = input$stephy, 
                                           tick0 = input$starthy, 
                                           range=c(input$starthy,input$endhy),
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
                       
                       h_trans
            } 
    
            if (input$faceth=="Yes") {
      
                     data_plot=data_H_d()
                     
                     h_trans = ggplot(data_plot)
                     h_trans = ggplot(data_plot,aes(x=timevar, y=V, color=factor(as.factor(cov_factor),levels=labels_cov())))
                     
                     h_trans = h_trans+geom_line(aes(x=timevar, y=V, color=factor(as.factor(cov_factor),levels=labels_cov()), group=1,
                                                     text=paste("Select or deselect lines by clicking on the legend",
                                                                "<br>Time: ", timevar,
                                                                "<br>Hazard: ", V,
                                                                "<br>Covariate pattern: ", factor(as.factor(cov_factor),levels=labels_cov()))))+
                                                 scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
                     
                     
                     h_trans = h_trans + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx  ))) +
                                         scale_y_continuous(breaks=c(seq(input$starthy,input$endhy,by=input$stephy  )))
                     
                     h_trans = h_trans +labs(title=paste0(hazname," for each covariate pattern among states"), x=paste0("Time since ",input$origin," entry"), y=hazname)
                     
                     h_trans = h_trans + labs(color = "Covariate \n patterns")+ labs(fill = "Covariate \n patterns")
                     
                     h_trans = h_trans +theme(title = element_text(size = input$textsizeh-4), strip.text = element_text(size=input$textfaceth),      
                                              legend.title = element_text(color="black", size= input$textsizeh-5), 
                                              legend.text=element_text(size= input$textsizeh-6),
                                              plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                              legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                              legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                              axis.title.y = element_text(size= input$textsizeh-5),
                                              axis.title.x = element_text(size= input$textsizeh-5))  
                     
                     h_trans = h_trans+ facet_wrap(~trans_factor,nrow = NULL,ncol = NULL)
                     
                     
                     h_trans = ggplotly(h_trans, tooltip = "text")%>%
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
                     
                     h_trans 
             
    
            }
    
        }
  
        else if (input$scaleh=="log") {
    
            if (input$faceth=="No") {
      
    
                         h_trans= plot_ly(data_H_d(),alpha=0.5) %>%
                           add_lines(
                             x=data_H_d()$timevar,y=log(data_H_d()$V),
                             frame=as.factor(data_H_d()$trans_factor),
                             color=factor(as.factor(data_H_d()$cov_factor),levels=labels_cov()),
                             colors=labels_colour_cov(),
                             mode="lines",
                             line=list(simplify=FALSE),color=labels_colour_cov(),
                             text = 'Select or deselect lines by clicking on the legend',
                             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") 
                           )  %>%
                         layout(title=list(text=paste0(hazname," (log scale) for each covariate pattern among states"),y=0.95),
                                font= list(family = "times new roman", size = input$textsizeh, color = "black"),
                                margin = list(l = 50, r = 50, b = 30, t = 70),
                                xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
                                           dtick = input$stephx, 
                                           tick0 = input$starthx, 
                                           range=c(input$starthx,input$endhx),
                                           ticklen = 5,
                                           tickwidth = 2,
                                           tickcolor = toRGB("black"),
                                           tickmode = "linear"),
                                yaxis =list(title= paste0(hazname," (log scale)"),                    
                                            dtick = input$logstephy, 
                                            tick0 = input$logstarthy, 
                                            range=c(input$logstarthy,input$logendhy),
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
                         
                         h_trans
                                  
            }
    
            if (input$faceth=="Yes") {
      
                    data_plot=data_H_d()
                    
                    h_trans = ggplot(data_plot)
                    h_trans = ggplot(data_plot,aes(x=timevar, y=log(V), color=factor(as.factor(cov_factor),levels=labels_cov())))
                    
                    h_trans = h_trans+geom_line(aes(x=timevar, y=log(V), color= factor(as.factor(cov_factor),levels=labels_cov()), group=1,
                                                    text=paste("Select or deselect lines by clicking on the legend",
                                                               "<br>Time: ", timevar,
                                                               "<br>log hazard: ", log(V),
                                                               "<br>Covariate pattern: ", factor(as.factor(cov_factor),levels=labels_cov()))))+
                      scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
                    
                    h_trans = h_trans + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx  ))) +
                                                           scale_y_continuous(breaks=c(seq(input$logstarthy,input$logendhy,by=input$logstephy  )))
                    
                    h_trans = h_trans +labs(title=paste0(hazname," for each covariate pattern among states"), x=paste0("Time since ",input$origin," entry"), y=hazname)
                    
                    h_trans = h_trans + labs(color = "Covariate \n patterns")+ labs(fill = "Covariate \n patterns")
                    
                    h_trans = h_trans +theme(title = element_text(size = input$textsizeh-4), strip.text = element_text(size=input$textfaceth),      
                                             legend.title = element_text(color="black", size= input$textsizeh-5), 
                                             legend.text=element_text(size= input$textsizeh-6),
                                             plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                             legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                             legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                             axis.title.y = element_text(size= input$textsizeh-5),
                                             axis.title.x = element_text(size= input$textsizeh-5))  
                    
                    h_trans = h_trans+ facet_wrap(~factor(as.factor(trans_factor),levels=labels_trans()),nrow = NULL,ncol = NULL)
                    
                    h_trans = ggplotly(h_trans, tooltip = "text")%>%
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
                    
             } #facet
       } #scale
    } # ci
  
  
  if (input$confh=="ci_yes") {
    
    
        if (is.null(myjson2()$is.cumhaz)==FALSE) {
          if (myjson2()$is.cumhaz==1) {hazname= "Cummulative hazards"}
          else if (myjson2()$is.cumhaz!=1)  {hazname= "Transition intensity rate"}
        }
        if (is.null(myjson2()$is.cumhaz)==TRUE) {hazname= "Transition intensity rate"}
        
        
        
    if (input$scaleh=="normal") {
          
        if (input$faceth=="No") {
            
                   h_trans= plot_ly(data_H_d(),alpha=0.5) %>%
                     add_lines(
                       x=data_H_d()$timevar,y=data_H_d()$V,
                       frame=as.factor(data_H_d()$trans_factor),
                       color=factor(as.factor(data_H_d()$cov_factor),levels=labels_cov()),
                       colors=labels_colour_cov(),
                       mode="lines",
                       line=list(simplify=FALSE),color=labels_colour_cov(),
                       text = 'Select or deselect lines by clicking on the legend',
                       hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                             "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") 
                     )  
                   
                   h_trans <- add_trace(h_trans, fill = "tozerox", 
                                          line=list(dash = "solid", color = "transparent", width = 1.8897637),
                                          mode = "lines", type = "scatter",
                                          x=data_H_ci()$x, y=data_H_ci()$y,
                                          frame=as.factor(data_H_ci()$frameto),
                                          colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
                                          color=as.factor(data_H_ci()$covto),
                                          showlegend = FALSE,
                                          text = 'Select or deselect lines by clicking on the legend',
                                          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
                   
                   
                   h_trans = h_trans %>%
                     layout(title=list(text=paste0(hazname," for each covariate pattern among states"),y=0.95),
                            font= list(family = "times new roman", size = input$textsizeh, color = "black"),
                            margin = list(l = 50, r = 50, b = 30, t = 70),
                            xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
                                       dtick = input$stephx, 
                                       tick0 = input$starthx, 
                                       range=c(input$starthx,input$endhx),
                                       ticklen = 5,
                                       tickwidth = 2,
                                       tickcolor = toRGB("black"),
                                       tickmode = "linear"),
                            yaxis =list(title= hazname,rangemode = "nonnegative",                    
                                        dtick = input$stephy, 
                                        tick0 = input$starthy, 
                                        range=c(input$starthy,input$endhy),
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
                   
                   h_trans
        } 
          
        if (input$faceth=="Yes") {
          
             V_lci=  data_H_d_lci()$V
             V_uci=  data_H_d_uci()$V
          
            data_plot=cbind(data_H_d(),V_lci,V_uci)
            
            h_trans = ggplot(data_plot)
            h_trans = ggplot(data_plot,aes(x=timevar, y=V, color=factor(as.factor(cov_factor),levels=labels_cov()), group=1,
                                           text=paste("Select or deselect lines by clicking on the legend",
                                                      "<br>Time: ", timevar,
                                                      "<br>Hazard: ", V,
                                                      "<br>Covariate pattern: ", factor(as.factor(cov_factor),levels=labels_cov()))))+
                               scale_colour_manual( values =labels_colour_cov(),labels = labels_cov() ) 
            
            h_trans = h_trans+geom_line(aes(x=timevar, y=V, fill=factor(as.factor(cov_factor),levels=labels_cov())))
             
            
            h_trans=h_trans+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill=as.factor(cov_factor)),alpha=0.4)+ 
              scale_fill_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
            
            
            h_trans = h_trans + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx  ))) +
              scale_y_continuous(breaks=c(seq(input$starthy,input$endhy,by=input$stephy  )))
            
            h_trans = h_trans +labs(title=paste0(hazname," for each covariate pattern among states"), x=paste0("Time since ",input$origin," entry"), y=hazname)
            
            h_trans = h_trans + labs(color = "Covariate \n patterns")+ labs(fill = "Covariate \n patterns")
            
            h_trans = h_trans +theme(title = element_text(size = input$textsizeh-4), strip.text = element_text(size=input$textfaceth),      
                                     legend.title = element_text(color="black", size= input$textsizeh-5), 
                                     legend.text=element_text(size= input$textsizeh-6),
                                     plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                     legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                     legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                     axis.title.y = element_text(size= input$textsizeh-5),
                                     axis.title.x = element_text(size= input$textsizeh-5))  
            
            h_trans = h_trans+ facet_wrap(~trans_factor,nrow = NULL,ncol = NULL)
            
            
            h_trans = ggplotly(h_trans, tooltip = "text")%>%
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
            
            h_trans 
            
            
          }
          
        }
        
    else if (input$scaleh=="log") {
          
          if (input$faceth=="No") {
            
            
                   h_trans= plot_ly(data_H_d(),alpha=0.5) %>%
                     add_lines(
                       x=data_H_d()$timevar,y=log(data_H_d()$V),
                       frame=as.factor(data_H_d()$trans_factor),
                       color=factor(as.factor(data_H_d()$cov_factor),levels=labels_cov()),
                       colors=labels_colour_cov(),
                       mode="lines",
                       line=list(simplify=FALSE), color=labels_colour_cov(),
                       text = 'Select or deselect lines by clicking on the legend',
                       hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                             "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") 
                     )
                   h_trans <- add_trace(h_trans, fill = "tozerox", 
                                        line=list(dash = "solid", color = "transparent", width = 1.8897637),
                                        mode = "lines", type = "scatter",
                                        x=data_H_ci()$x, y=log(data_H_ci()$y),
                                        frame=as.factor(data_H_ci()$frameto),
                                        colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
                                        color=as.factor(data_H_ci()$covto),
                                        showlegend = FALSE,
                                        text = 'Select or deselect lines by clicking on the legend',
                                        hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                              "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
                                        )%>%
                     layout(title=list(text=paste0(hazname," (log scale) for each covariate pattern among states"),y=0.95),
                            font= list(family = "times new roman", size = input$textsizeh, color = "black"),
                            margin = list(l = 50, r = 50, b = 30, t = 70),
                            xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
                                       dtick = input$stephx, 
                                       tick0 = input$starthx, 
                                       range=c(input$starthx,input$endhx),
                                       ticklen = 5,
                                       tickwidth = 2,
                                       tickcolor = toRGB("black"),
                                       tickmode = "linear"),
                            yaxis =list(title= paste0(hazname," (log scale)"),                    
                                        dtick = input$logstephy, 
                                        tick0 = input$logstarthy, 
                                        range=c(input$logstarthy,input$logendhy),
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
                   
                   h_trans
            
          }
          
          if (input$faceth=="Yes") {
            
            V_lci=  data_H_d_lci()$V
            V_uci=  data_H_d_uci()$V
            
            data_plot=cbind(data_H_d(),V_lci,V_uci)
            
            h_trans = ggplot(data_plot)
            h_trans = ggplot(data_plot,aes(x=timevar, y=log(V), color=factor(as.factor(cov_factor),levels=labels_cov()), group=1,
                                           text=paste("Select or deselect lines by clicking on the legend",
                                                      "<br>Time: ", timevar,
                                                      "<br>Hazard: ", V,
                                                      "<br>Covariate pattern: ", factor(as.factor(cov_factor),levels=labels_cov()))))+
              scale_colour_manual( values =labels_colour_cov(),labels = labels_cov() ) 
            
            h_trans = h_trans+geom_line(aes(x=timevar, y=log(V), fill=factor(as.factor(cov_factor),levels=labels_cov())))
            
            
            h_trans=h_trans+ geom_ribbon(aes(ymin = log(V_lci), ymax =log(V_uci),fill=as.factor(cov_factor)),alpha=0.4)+ 
              scale_fill_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
            
            
            h_trans = h_trans + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx  ))) +
              scale_y_continuous(breaks=c(seq(input$logstarthy,input$logendhy,by=input$logstephy  )))
            
            h_trans = h_trans +labs(title=paste0(hazname," for each covariate pattern among states"), x=paste0("Time since ",input$origin," entry"), y=hazname)
            
            h_trans = h_trans + labs(color = "Covariate \n patterns")+ labs(fill = "Covariate \n patterns")
            
            h_trans = h_trans +theme(title = element_text(size = input$textsizeh-4), strip.text = element_text(size=input$textfaceth),      
                                     legend.title = element_text(color="black", size= input$textsizeh-5), 
                                     legend.text=element_text(size= input$textsizeh-6),
                                     plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                     legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                     legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                     axis.title.y = element_text(size= input$textsizeh-5),
                                     axis.title.x = element_text(size= input$textsizeh-5))  
            
            h_trans = h_trans+ facet_wrap(~trans_factor,nrow = NULL,ncol = NULL)
            
            h_trans = ggplotly(h_trans, tooltip = "text")%>%
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
            
          } #facet
        } #scale
        
      }
    
    h_trans
})

output$hazards_trans <- renderPlotly ({ datah1_re() })


output$downploth1 <- downloadHandler(
  filename = function(){paste("h1",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datah1_re(),width = 1200, height = 900, format = "png", scale = 2,  out_file = file )
  }
)
  
#########################################################################
##########################################################################


data_H_diff1 <- reactive ({
  haz_diff=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_diff= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$hazd)) {
      haz_diff[[i]]=as.data.frame(t(data.frame(myjson2()$hazd[i])))
      colnames(haz_diff[[i]]) <- v_diff
    }
    for(i in 1:length(myjson2()$hazd)) {  
      haz_diff[[i]]=as.data.frame(cbind(haz_diff[[i]], timevar ,trans=rep(i,nrow(haz_diff[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$hazd)) {
      haz_diff[[i]]=as.data.frame(myjson2()$hazd[[i]][,1])
      # colnames(P_diff[[i]]) <- v_diff
    }
    for (i in 1:length(myjson2()$hazd)) {  
      haz_diff[[i]]=as.data.frame(c(haz_diff[[i]], timevar ,trans=rep(i,ncol(as.data.frame(myjson2()$hazd[[i]][,1])) )) )
      colnames(haz_diff[[i]])[1:(length(myjson2()$atlist)-1)] <- v_diff
    }
  }
  
  
  # Append the probabilities datasets of the different states
  data_hazd=list()
  data_hazd[[1]]=haz_diff[[1]]
  
  if (length(myjson2()$hazd)>1) {
    for (u in 2:(length(myjson2()$hazd))) {
      data_hazd[[u]]=rbind(haz_diff[[u]],data_hazd[[(u-1)]])
    }
  }
  
  datahazd=data_hazd[[length(myjson2()$hazd)]]
  datahazd$trans_fac=c(rep("NA",nrow(datahazd)))
  
  for (o in 1:(length(myjson2()$hazd))) {
    for (g in 1:nrow(datahazd))  {
      if  (datahazd$trans[g]==o) {datahazd$trans_fac[g]=labels_trans()[o]}  
    }
  }
  datahazd
}) 

data_H_diff1_uci <- reactive ({
  haz_diff_uci=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_diff= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$hazd_uci)) {
      haz_diff_uci[[i]]=as.data.frame(t(data.frame(myjson2()$hazd_uci[i])))
      colnames(haz_diff_uci[[i]]) <- v_diff
    }
    for(i in 1:length(myjson2()$hazd_uci)) {  
      haz_diff_uci[[i]]=as.data.frame(cbind(haz_diff_uci[[i]], timevar ,trans=rep(i,nrow(haz_diff_uci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$hazd_uci)) {
      haz_diff_uci[[i]]=as.data.frame(myjson2()$hazd_uci[[i]][,1])
      # colnames(P_diff[[i]]) <- v_diff
    }
    for (i in 1:length(myjson2()$hazd_uci)) {  
      haz_diff_uci[[i]]=as.data.frame(c(haz_diff_uci[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$hazd_uci[[i]][,1])) )) )
      colnames(haz_diff_uci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_diff
    }
  }
  
  
  # Append the probabilities datasets of the different states
  data_hazd_uci=list()
  data_hazd_uci[[1]]=haz_diff_uci[[1]]
  
  if (length(myjson2()$hazd_uci)>1) {
    for (u in 2:(length(myjson2()$hazd_uci))) {
      data_hazd_uci[[u]]=rbind(haz_diff_uci[[u]],data_hazd_uci[[(u-1)]])
    }
  }
  
  datahazd_uci=data_hazd_uci[[length(myjson2()$hazd_uci)]]
  datahazd_uci$trans_fac=c(rep("NA",nrow(datahazd_uci)))
  
  for (o in 1:(length(myjson2()$hazd_uci))) {
    for (g in 1:nrow(datahazd_uci))  {
      if  (datahazd_uci$trans[g]==o) {datahazd_uci$trans_fac[g]=labels_trans()[o]}  
    }
  }
  datahazd_uci
}) 

data_H_diff1_lci <- reactive ({
  haz_diff_lci=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_diff= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$hazd_lci)) {
      haz_diff_lci[[i]]=as.data.frame(t(data.frame(myjson2()$hazd_lci[i])))
      colnames(haz_diff_lci[[i]]) <- v_diff
    }
    for(i in 1:length(myjson2()$hazd_lci)) {  
      haz_diff_lci[[i]]=as.data.frame(cbind(haz_diff_lci[[i]], timevar ,trans=rep(i,nrow(haz_diff_lci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$hazd_lci)) {
      haz_diff_lci[[i]]=as.data.frame(myjson2()$hazd_lci[[i]][,1])
      # colnames(P_diff[[i]]) <- v_diff
    }
    for (i in 1:length(myjson2()$hazd_lci)) {  
      haz_diff_lci[[i]]=as.data.frame(c(haz_diff_lci[[i]], timevar ,trans=rep(i,ncol(as.data.frame(myjson2()$hazd_lci[[i]][,1])) )) )
      colnames(haz_diff_lci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_diff
    }
  }
  
  
  # Append the probabilities datasets of the different states
  data_hazd_lci=list()
  data_hazd_lci[[1]]=haz_diff_lci[[1]]
  
  if (length(myjson2()$hazd_lci)>1) {
    for (u in 2:(length(myjson2()$hazd_lci))) {
      data_hazd_lci[[u]]=rbind(haz_diff_lci[[u]],data_hazd_lci[[(u-1)]])
    }
  }
  
  datahazd_lci=data_hazd_lci[[length(myjson2()$hazd_lci)]]
  datahazd_lci$trans_fac=c(rep("NA",nrow(datahazd_lci)))
  
  for (o in 1:(length(myjson2()$hazd_lci))) {
    for (g in 1:nrow(datahazd_lci))  {
      if  (datahazd_lci$trans[g]==o) {datahazd_lci$trans_fac[g]=labels_trans()[o]}  
    }
  }
  datahazd_lci
}) 

data_H_diff2<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_H_diff1()[,d],data_H_diff1()[,ncol(data_H_diff1())-2],data_H_diff1()[,ncol(data_H_diff1())-1],
                                data_H_diff1()[,ncol(data_H_diff1())],rep(d,length(data_H_diff1()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_H_diff1())[d],length(data_H_diff1()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","trans","trans_factor","cov","cov_factor")
  }
  
  d_all_Hd <- bind_rows(dlist, .id = "column_label")
  d_all_Hd
}) 

data_H_diff2_uci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_H_diff1_uci()[,d],data_H_diff1_uci()[,ncol(data_H_diff1_uci())-2],data_H_diff1_uci()[,ncol(data_H_diff1_uci())-1],
                                data_H_diff1_uci()[,ncol(data_H_diff1_uci())],rep(d,length(data_H_diff1_uci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_H_diff1_uci())[d],length(data_H_diff1_uci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","trans","trans_factor","cov","cov_factor")
  }
  
  d_all_Hd_uci <- bind_rows(dlist, .id = "column_label")
  d_all_Hd_uci
}) 

data_H_diff2_lci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_H_diff1_lci()[,d],data_H_diff1_lci()[,ncol(data_H_diff1_lci())-2],data_H_diff1_lci()[,ncol(data_H_diff1_lci())-1],
                                data_H_diff1_lci()[,ncol(data_H_diff1_lci())],rep(d,length(data_H_diff1_lci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_H_diff1_lci())[d],length(data_H_diff1_lci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","trans","trans_factor","cov","cov_factor")
  }
  
  d_all_Hd_lci <- bind_rows(dlist, .id = "column_label")
  d_all_Hd_lci
}) 

data_H_diff_ci<- reactive ({
  x=c( data_H_diff2()[order(data_H_diff2()$timevar,data_H_diff2()$trans,data_H_diff2()$cov),]$timevar,
       data_H_diff2()[order(-data_H_diff2()$timevar,data_H_diff2()$trans,data_H_diff2()$cov),]$timevar )
  
  y_central=c( data_H_diff2()[order(data_H_diff2()$timevar,data_H_diff2()$trans,data_H_diff2()$cov),]$V,
               data_H_diff2()[order(-data_H_diff2()$timevar,data_H_diff2()$trans,data_H_diff2()$cov),]$V )
  
  y=c( data_H_diff2_uci()[order(data_H_diff2_uci()$timevar,data_H_diff2_uci()$trans,data_H_diff2_uci()$cov),]$V,
       data_H_diff2_lci()[order(-data_H_diff2_lci()$timevar,data_H_diff2_lci()$trans,data_H_diff2_lci()$cov),]$V )
  
  frameto=c(as.character(data_H_diff2_uci()[order(-data_H_diff2_uci()$timevar,data_H_diff2_uci()$trans,data_H_diff2_uci()$cov),]$trans_factor),
            as.character(data_H_diff2_lci()[order(-data_H_diff2_lci()$timevar,data_H_diff2_lci()$trans,data_H_diff2_lci()$cov),]$trans_factor) )
  
  covto=c( data_H_diff2_uci()[order(-data_H_diff2_uci()$timevar,data_H_diff2_uci()$trans,data_H_diff2_uci()$cov),]$cov_factor,
           data_H_diff2_lci()[order(-data_H_diff2_lci()$timevar,data_H_diff2_lci()$trans,data_H_diff2_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,frameto,covto,y_central)
  data
})


#output$shouldloadh2 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downploth2", label = h2("Download the plot"))
#})

datah2_re <-  reactive ({
  
  ax <- list( title = "",zeroline = FALSE,showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
  
  if (length(myjson2()$hazd) == 0| myjson2()$Nats==1 ) {             
    haz_trans_d= plot_ly() %>%
      layout(title=list(text="Not applicable- Only one covariate pattern specified",y=0.95),xaxis=ax, yaxis=ax)
    haz_trans_d
  } 
  
  else  {
    
    if (input$confh=="ci_no") {
      
      if (input$faceth=="No") {
        
        
        haz_trans_d= plot_ly(data_H_diff2(),alpha=0.5) %>%
          add_lines(
            x=data_H_diff2()$timevar,y=data_H_diff2()$V,
            frame=factor(as.factor(data_H_diff2()$trans_factor),levels=labels_trans() ),
            color=as.factor(data_H_diff2()$cov_factor),
            colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
            mode="lines",
            line=list(simplify=FALSE),
            text = 'Select or deselect lines by clicking on the legend',
            hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                  "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
          )  %>%
          layout(title=list(text="Differences in hazards among covariate patterns (compared to reference)",y=0.95),
                 font= list(family = "times new roman", size = input$textsizeh, color = "black"),
                 margin = list(l = 50, r = 50, b = 30, t = 70),
                 xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
                            dtick = input$stephx, 
                            tick0 = input$starthx, 
                            range=c(input$starthx,input$endhx),
                            ticklen = 5,
                            tickwidth = 2,
                            tickcolor = toRGB("black"),
                            tickmode = "linear"),
                 yaxis =list(title= "Differences of hazards for each transition",                    
                             dtick = input$stephy, 
                             ticklen = 5,
                             tickwidth = 2,
                             tickcolor = toRGB("black")),
                 shapes = list(
                   list(type = "rect",
                        fillcolor = "grey", 
                        line = list(color = "grey"), 
                        opacity = 0.8,
                        x0 = 0, x1 =input$area, xref = "x", y0 = 0, y1 = 1, yref = "y") )
          )%>%
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
        
        haz_trans_d
        
      }
      
      if (input$faceth=="Yes") {
        
        
        data_plot=data_H_diff2()
        
        haz_trans_d = ggplot(data_plot)
        haz_trans_d = ggplot(data_plot,aes(x=timevar, y=V, color= as.factor(cov_factor), group=1,
                                         text=paste("Select or deselect lines by clicking on the legend",
                                                    "<br>Time: ", timevar,
                                                    "<br>Difference of probabilities: ", V,
                                                    "<br>Covariate pattern: ",  as.factor(cov_factor))))
        
        haz_trans_d = haz_trans_d+geom_line(aes(x=timevar, y=V, color= as.factor(cov_factor)))+
          scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
        
        haz_trans_d = haz_trans_d+ facet_wrap(~factor(as.factor(trans_factor),levels=labels_trans() ))
        
        haz_trans_d = haz_trans_d + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx ))) 
        
        haz_trans_d = haz_trans_d + scale_y_continuous(breaks=c(seq(min(data_plot$V[which(!is.na(data_plot$V))]),max(data_plot$V[which(!is.na(data_plot$V))]), by=input$stephy )))
        
  
        
        haz_trans_d = haz_trans_d +labs(title="Differences in hazards among covariate patterns (compared to reference)",
                                    x=paste0("Time since ",input$origin," entry"), y="Differences of hazards")
        
        haz_trans_d = haz_trans_d + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
        
        haz_trans_d = haz_trans_d +theme(title = element_text(size = input$textsizeh-4), strip.text = element_text(size=input$textfaceth),      
                                     legend.title = element_text(color=input$textcolourp, size= input$textsizeh-5), 
                                     legend.text=element_text(size= input$textsizeh-6),
                                     plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                     legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                     legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                     axis.title.y = element_text(size= input$textsizeh-5),
                                     axis.title.x = element_text(size= input$textsizeh-5)) 
        
        haz_trans_d = ggplotly(haz_trans_d, tooltip = "text")%>%
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
    
    else if (input$confh=="ci_yes") {
      
      
      if (input$faceth=="No") {
        
        haz_trans_d <- plot_ly()
        
        haz_trans_d <- add_trace(haz_trans_d, line=list(simplify=FALSE),
                               mode="lines", type = "scatter",
                               x=data_H_diff_ci()$x, y=data_H_diff_ci()$y_central,
                               frame=factor(as.factor(data_H_diff_ci()$frameto),levels=labels_trans()), 
                               colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                               color=as.factor(data_H_diff_ci()$covto),
                               text = 'Select or deselect lines by clicking on the legend',
                               hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                     "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") )
        
        haz_trans_d <- add_trace(haz_trans_d, fill = "tozerox", 
                               line=list(dash = "solid", color = "transparent", width = 1.8897637),
                               mode = "lines", type = "scatter",
                               x=data_H_diff_ci()$x, y=data_H_diff_ci()$y,
                               frame=factor(as.factor(data_H_diff_ci()$frameto),levels=labels_trans()), 
                               colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                               color=as.factor(data_H_diff_ci()$covto),
                               showlegend = FALSE,
                               text = 'Select or deselect lines by clicking on the legend',
                               hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                     "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
        
        haz_trans_d= haz_trans_d   %>%
          layout(title=list(text="Differences in hazards among covariate patterns (compared to reference)",y=0.95),
                 font= list(family = "times new roman", size = input$textsizeh, color = "black"),
                 margin = list(l = 50, r = 50, b = 30, t = 70),
                 xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
                            dtick = input$stephx, 
                            tick0 = input$starthx, 
                            range=c(input$starthx,input$endhx),
                            ticklen = 5,
                            tickwidth = 2,
                            tickcolor = toRGB("black"),
                            tickmode = "linear"),
                 yaxis =list(title= "Differences of hazards for each transition",                    
                             dtick = input$stephy, 
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
        
        
      }
      
      if (input$faceth=="Yes") {
        
        V_lci=  data_H_diff2_lci()$V
        V_uci=  data_H_diff2_uci()$V
        
        data_plot=cbind(data_H_diff2(),V_lci,V_uci)
        
        
        haz_trans_d=ggplot(data_plot)
        haz_trans_d=ggplot(data_plot,aes(x=timevar, y=V, color= as.factor(cov_factor), group=1,
                                       text=paste("Select or deselect lines by clicking on the legend",
                                                  "<br>Time: ", timevar,
                                                  "<br>Difference of hazards for each transition: ", V,
                                                  "<br>Covariate pattern: ",  as.factor(cov_factor))))
        haz_trans_d=haz_trans_d+ scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
        
        
        haz_trans_d=haz_trans_d+geom_line(aes(x=timevar, y=V, fill= as.factor(cov_factor)))
        
        haz_trans_d=haz_trans_d+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill=as.factor(cov_factor)),alpha=0.4)+ 
          scale_fill_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
        
        haz_trans_d = haz_trans_d+ facet_wrap(~factor(as.factor(trans_factor),levels=labels_trans()) )
        
        haz_trans_d = haz_trans_d + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx ))) 
        
        haz_trans_d = haz_trans_d + scale_y_continuous(breaks=c(seq(min(data_plot$V_lci[which(!is.na(data_plot$V_lci))]),max(data_plot$V_uci[which(!is.na(data_plot$V_uci))]),by=input$stephy )))
      
        
        haz_trans_d = haz_trans_d +labs(title="Differences in hazards  among covariate patterns (compared to reference)",
                                    x=paste0("Time since ",input$origin," entry"), y="Differences of hazards for each transition")
        
        haz_trans_d = haz_trans_d + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
        
        haz_trans_d = haz_trans_d +theme(title = element_text(size = input$textsizeh-4), strip.text = element_text(size=input$textfaceth),      
                                     legend.title = element_text(color="black", size= input$textsizeh-5), 
                                     legend.text=element_text(size= input$textsizeh-6),
                                     plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                     legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                     legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                     axis.title.y = element_text(size= input$textsizeh-5),
                                     axis.title.x = element_text(size= input$textsizeh-5), 
                                     axis.text.x = element_text( size=input$textsizeh-6),axis.text.y = element_text( size=input$textsizeh-6)) 
        
        haz_trans_d = ggplotly(haz_trans_d, tooltip = "text")%>%
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
    
    
    haz_trans_d
    
  }
}) 

output$H_diff <- renderPlotly ({datah2_re() }) 


output$downploth2 <- downloadHandler(
  filename = function(){paste("h2",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datah2_re(),width = 1200, height = 900, format = "png", scale = 2,  out_file = file )
  }
)

############################################################################################################
###########################################################################################################

######################################################################################################################################
######################################################################################################################################

data_H_ratio1 <- reactive ({
  haz_ratio=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_ratio= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_ratio[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$hazr)) {
      haz_ratio[[i]]=as.data.frame(t(data.frame(myjson2()$hazr[i])))
      colnames(haz_ratio[[i]]) <- v_ratio
    }
    for(i in 1:length(myjson2()$hazr)) {  
      haz_ratio[[i]]=as.data.frame(cbind(haz_ratio[[i]], timevar ,trans=rep(i,nrow(haz_ratio[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$hazr)) {
      haz_ratio[[i]]=as.data.frame(myjson2()$hazr[[i]][,1])
      # colnames(P_ratio[[i]]) <- v_ratio
    }
    for (i in 1:length(myjson2()$hazr)) {  
      haz_ratio[[i]]=as.data.frame(c(haz_ratio[[i]], timevar ,trans=rep(i,ncol(as.data.frame(myjson2()$hazr[[i]][,1])) )) )
      colnames(haz_ratio[[i]])[1:(length(myjson2()$atlist)-1)] <- v_ratio
    }
  }
  
  
  # Append the probabilities datasets of the ratioerent states
  data_hazr=list()
  data_hazr[[1]]=haz_ratio[[1]]
  
  if (length(myjson2()$hazr)>1) {
    for (u in 2:(length(myjson2()$hazr))) {
      data_hazr[[u]]=rbind(haz_ratio[[u]],data_hazr[[(u-1)]])
    }
  }
  
  datahazr=data_hazr[[length(myjson2()$hazr)]]
  datahazr$trans_fac=c(rep("NA",nrow(datahazr)))
  
  for (o in 1:(length(myjson2()$hazr))) {
    for (g in 1:nrow(datahazr))  {
      if  (datahazr$trans[g]==o) {datahazr$trans_fac[g]=labels_trans()[o]}  
    }
  }
  datahazr
}) 


data_H_ratio1_uci <- reactive ({
  haz_ratio_uci=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_ratio= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_ratio[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$hazr_uci)) {
      haz_ratio_uci[[i]]=as.data.frame(t(data.frame(myjson2()$hazr_uci[i])))
      colnames(haz_ratio_uci[[i]]) <- v_ratio
    }
    for(i in 1:length(myjson2()$hazr_uci)) {  
      haz_ratio_uci[[i]]=as.data.frame(cbind(haz_ratio_uci[[i]], timevar ,trans=rep(i,nrow(haz_ratio_uci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$hazr_uci)) {
      haz_ratio_uci[[i]]=as.data.frame(myjson2()$hazr_uci[[i]][,1])
      # colnames(P_ratio[[i]]) <- v_ratio
    }
    for (i in 1:length(myjson2()$hazr_uci)) {  
      haz_ratio_uci[[i]]=as.data.frame(c(haz_ratio_uci[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$hazr_uci[[i]][,1])) )) )
      colnames(haz_ratio_uci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_ratio
    }
  }
  
  
  # Append the probabilities datasets of the ratioerent states
  data_hazr_uci=list()
  data_hazr_uci[[1]]=haz_ratio_uci[[1]]
  
  if (length(myjson2()$hazr_uci)>1) {
    for (u in 2:(length(myjson2()$hazr_uci))) {
      data_hazr_uci[[u]]=rbind(haz_ratio_uci[[u]],data_hazr_uci[[(u-1)]])
    }
  }
  
  datahazr_uci=data_hazr_uci[[length(myjson2()$hazr_uci)]]
  datahazr_uci$trans_fac=c(rep("NA",nrow(datahazr_uci)))
  
  for (o in 1:(length(myjson2()$hazr_uci))) {
    for (g in 1:nrow(datahazr_uci))  {
      if  (datahazr_uci$trans[g]==o) {datahazr_uci$trans_fac[g]=labels_trans()[o]}  
    }
  }
  datahazr_uci
}) 

data_H_ratio1_lci <- reactive ({
  haz_ratio_lci=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_ratio= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_ratio[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$hazr_lci)) {
      haz_ratio_lci[[i]]=as.data.frame(t(data.frame(myjson2()$hazr_lci[i])))
      colnames(haz_ratio_lci[[i]]) <- v_ratio
    }
    for(i in 1:length(myjson2()$hazr_lci)) {  
      haz_ratio_lci[[i]]=as.data.frame(cbind(haz_ratio_lci[[i]], timevar ,trans=rep(i,nrow(haz_ratio_lci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$hazr_lci)) {
      haz_ratio_lci[[i]]=as.data.frame(myjson2()$hazr_lci[[i]][,1])
      # colnames(P_ratio[[i]]) <- v_ratio
    }
    for (i in 1:length(myjson2()$hazr_lci)) {  
      haz_ratio_lci[[i]]=as.data.frame(c(haz_ratio_lci[[i]], timevar ,trans=rep(i,ncol(as.data.frame(myjson2()$hazr_lci[[i]][,1])) )) )
      colnames(haz_ratio_lci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_ratio
    }
  }
  
  
  # Append the probabilities datasets of the ratioerent states
  data_hazr_lci=list()
  data_hazr_lci[[1]]=haz_ratio_lci[[1]]
  
  if (length(myjson2()$hazr_lci)>1) {
    for (u in 2:(length(myjson2()$hazr_lci))) {
      data_hazr_lci[[u]]=rbind(haz_ratio_lci[[u]],data_hazr_lci[[(u-1)]])
    }
  }
  
  datahazr_lci=data_hazr_lci[[length(myjson2()$hazr_lci)]]
  datahazr_lci$trans_fac=c(rep("NA",nrow(datahazr_lci)))
  
  for (o in 1:(length(myjson2()$hazr_lci))) {
    for (g in 1:nrow(datahazr_lci))  {
      if  (datahazr_lci$trans[g]==o) {datahazr_lci$trans_fac[g]=labels_trans()[o]}  
    }
  }
  datahazr_lci
}) 

data_H_ratio2<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_H_ratio1()[,d],data_H_ratio1()[,ncol(data_H_ratio1())-2],data_H_ratio1()[,ncol(data_H_ratio1())-1],
                                data_H_ratio1()[,ncol(data_H_ratio1())],rep(d,length(data_H_ratio1()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_H_ratio1())[d],length(data_H_ratio1()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","trans","trans_factor","cov","cov_factor")
  }
  
  d_all_Hr <- bind_rows(dlist, .id = "column_label")
  d_all_Hr
}) 

data_H_ratio2_uci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_H_ratio1_uci()[,d],data_H_ratio1_uci()[,ncol(data_H_ratio1_uci())-2],data_H_ratio1_uci()[,ncol(data_H_ratio1_uci())-1],
                                data_H_ratio1_uci()[,ncol(data_H_ratio1_uci())],rep(d,length(data_H_ratio1_uci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_H_ratio1_uci())[d],length(data_H_ratio1_uci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","trans","trans_factor","cov","cov_factor")
  }
  
  d_all_Hr_uci <- bind_rows(dlist, .id = "column_label")
  d_all_Hr_uci
}) 

data_H_ratio2_lci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_H_ratio1_lci()[,d],data_H_ratio1_lci()[,ncol(data_H_ratio1_lci())-2],data_H_ratio1_lci()[,ncol(data_H_ratio1_lci())-1],
                                data_H_ratio1_lci()[,ncol(data_H_ratio1_lci())],rep(d,length(data_H_ratio1_lci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_H_ratio1_lci())[d],length(data_H_ratio1_lci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","trans","trans_factor","cov","cov_factor")
  }
  
  d_all_Hr_lci <- bind_rows(dlist, .id = "column_label")
  d_all_Hr_lci
}) 

data_H_ratio_ci<- reactive ({
  x=c( data_H_ratio2()[order(data_H_ratio2()$timevar,data_H_ratio2()$trans,data_H_ratio2()$cov),]$timevar,
       data_H_ratio2()[order(-data_H_ratio2()$timevar,data_H_ratio2()$trans,data_H_ratio2()$cov),]$timevar )
  
  y_central=c( data_H_ratio2()[order(data_H_ratio2()$timevar,data_H_ratio2()$trans,data_H_ratio2()$cov),]$V,
               data_H_ratio2()[order(-data_H_ratio2()$timevar,data_H_ratio2()$trans,data_H_ratio2()$cov),]$V )
  
  y=c( data_H_ratio2_uci()[order(data_H_ratio2_uci()$timevar,data_H_ratio2_uci()$trans,data_H_ratio2_uci()$cov),]$V,
       data_H_ratio2_lci()[order(-data_H_ratio2_lci()$timevar,data_H_ratio2_lci()$trans,data_H_ratio2_lci()$cov),]$V )
  
  frameto=c(as.character(data_H_ratio2_uci()[order(-data_H_ratio2_uci()$timevar,data_H_ratio2_uci()$trans,data_H_ratio2_uci()$cov),]$trans_factor),
            as.character(data_H_ratio2_lci()[order(-data_H_ratio2_lci()$timevar,data_H_ratio2_lci()$trans,data_H_ratio2_lci()$cov),]$trans_factor) )
  
  covto=c( data_H_ratio2_uci()[order(-data_H_ratio2_uci()$timevar,data_H_ratio2_uci()$trans,data_H_ratio2_uci()$cov),]$cov_factor,
           data_H_ratio2_lci()[order(-data_H_ratio2_lci()$timevar,data_H_ratio2_lci()$trans,data_H_ratio2_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,frameto,covto,y_central)
  data
})



#output$shouldloadh3 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downploth3", label = h2("Download the plot"))
#})

datah3_re <-  reactive ({
  
  ax <- list( title = "",zeroline = FALSE,showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
  
  if (length(myjson2()$hazr) == 0| myjson2()$Nats==1 ) {             
    haz_trans_r= plot_ly() %>%
      layout(title=list(text="Not applicable- Only one covariate pattern specified",y=0.95),xaxis=ax, yaxis=ax)
    haz_trans_r
  } 
  
  else  {
    
    if (input$confh=="ci_no") {
      
      if (input$faceth=="No") {
        
        
        haz_trans_r= plot_ly(data_H_ratio2(),alpha=0.5) %>%
          add_lines(
            x=data_H_ratio2()$timevar,y=data_H_ratio2()$V,
            frame=factor(as.factor(data_H_ratio2()$trans_factor),levels=labels_trans()),
            color=as.factor(data_H_ratio2()$cov_factor),
            colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
            mode="lines",
            line=list(simplify=FALSE),
            text = 'Select or deselect lines by clicking on the legend',
            hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                  "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
          )  %>%
          layout(title=list(text="Ratio of hazards for each transition among covariate patterns (compared to ref. cov pattern)",y=0.95),
                 font= list(family = "times new roman", size = input$textsizeh, color = "black"),
                 margin = list(l = 50, r = 50, b = 30, t = 70),
                 xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
                            dtick = input$stephx, 
                            tick0 = input$starthx, 
                            range=c(input$starthx,input$endhx),
                            ticklen = 5,
                            tickwidth = 2,
                            tickcolor = toRGB("black"),
                            tickmode = "linear"),
                 yaxis =list(title= "Ratio of hazards for each transition",                    
                             dtick = input$stephy, 
                             ticklen = 5,
                             tickwidth = 2,
                             tickcolor = toRGB("black")),
                 shapes = list(
                   list(type = "rect",
                        fillcolor = "grey", 
                        line = list(color = "grey"), 
                        opacity = 0.8,
                        x0 = 0, x1 =input$area, xref = "x", y0 = 0, y1 = 1, yref = "y") )
          )%>%
          animation_opts(frame = 1000, transition = 0, redraw = FALSE) %>%
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
      
      if (input$faceth=="Yes") {
        
        
        data_plot=data_H_ratio2()
        
        haz_trans_r = ggplot(data_plot)
        haz_trans_r = ggplot(data_plot,aes(x=timevar, y=V, color= as.factor(cov_factor), group=1,
                                           text=paste("Select or deselect lines by clicking on the legend",
                                                      "<br>Time: ", timevar,
                                                      "<br>Ratio of probabilities: ", V,
                                                      "<br>Covariate pattern: ",  as.factor(cov_factor))))
        
        haz_trans_r = haz_trans_r+geom_line(aes(x=timevar, y=V, color= as.factor(cov_factor)))+
          scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
        
        haz_trans_r = haz_trans_r+ facet_wrap(~factor(as.factor(trans_factor),levels=labels_trans()))
        
        haz_trans_r = haz_trans_r + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx ))) +
                                  scale_y_continuous(breaks=c(seq(input$starthy,input$endhy,by=input$stephy )))
        haz_trans_r = haz_trans_r +labs(title="Ratio of hazards among covariate patterns (compared to ref. cov pattern)",
                                        x=paste0("Time since ",input$origin," entry"), y="Ratio of hazards")
        
        haz_trans_r = haz_trans_r + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
        
        haz_trans_r = haz_trans_r +theme(title = element_text(size = input$textsizeh-4), strip.text = element_text(size=input$textfaceth),      
                                         legend.title = element_text(color="black", size= input$textsizeh-5), 
                                         legend.text=element_text(size= input$textsizeh-6),
                                         plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                         legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                         legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                         axis.title.y = element_text(size= input$textsizeh-5),
                                         axis.title.x = element_text(size= input$textsizeh-5), 
                                         axis.text.x = element_text( size=input$textsizeh-6),axis.text.y = element_text( size=input$textsizeh-6)) 
        
        haz_trans_r = ggplotly(haz_trans_r, tooltip = "text")%>%
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
    
    else if (input$confh=="ci_yes") {
      
      
      if (input$faceth=="No") {
        
        haz_trans_r <- plot_ly()
        
        haz_trans_r <- add_trace(haz_trans_r, line=list(simplify=FALSE),
                                 mode="lines", type = "scatter",
                                 x=data_H_ratio_ci()$x, y=data_H_ratio_ci()$y_central,
                                 frame=factor(as.factor(data_H_ratio_ci()$frameto),levels=labels_trans()), 
                                 colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                                 color=as.factor(data_H_ratio_ci()$covto),
                                 text = 'Select or deselect lines by clicking on the legend',
                                 hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                       "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") )
        
        haz_trans_r <- add_trace(haz_trans_r, fill = "tozerox", 
                                 line=list(dash = "solid", color = "transparent", width = 1.8897637),
                                 mode = "lines", type = "scatter",
                                 x=data_H_ratio_ci()$x, y=data_H_ratio_ci()$y,
                                 frame=factor(as.factor(data_H_ratio_ci()$frameto),levels=labels_trans()),
                                 colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                                 color=as.factor(data_H_ratio_ci()$covto),
                                 showlegend = FALSE,
                                 text = 'Select or deselect lines by clicking on the legend',
                                 hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                       "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
        
        haz_trans_r= haz_trans_r   %>%
          layout(title=list(text="Ratio of hazards for each transition among covariate patterns (compared to ref. cov pattern)",y=0.95),
                 font= list(family = "times new roman", size = input$textsizeh, color = "black"),
                 margin = list(l = 50, r = 50, b = 30, t = 70),
                 xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
                            dtick = input$stephx, 
                            tick0 = input$starthx, 
                            range=c(input$starthx,input$endhx),
                            ticklen = 5,
                            tickwidth = 2,
                            tickcolor = toRGB("black"),
                            tickmode = "linear"),
                 yaxis =list(title= "Ratio of hazards for each transition",                    
                             dtick = input$stephy, 
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
        
        
      }
      
      if (input$faceth=="Yes") {
        
        V_lci=  data_H_ratio2_lci()$V
        V_uci=  data_H_ratio2_uci()$V
        
        data_plot=cbind(data_H_ratio2(),V_lci,V_uci)
        
        
        haz_trans_r=ggplot(data_plot)
        haz_trans_r=ggplot(data_plot,aes(x=timevar, y=V, color= as.factor(cov_factor), group=1,
                                         text=paste("Select or deselect lines by clicking on the legend",
                                                    "<br>Time: ", timevar,
                                                    "<br>Ratio of hazards for each transition: ", V,
                                                    "<br>Covariate pattern: ",  as.factor(cov_factor))))
        haz_trans_r=haz_trans_r+ scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
        
        
        haz_trans_r=haz_trans_r+geom_line(aes(x=timevar, y=V, fill= as.factor(cov_factor)))
        
        haz_trans_r=haz_trans_r+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill=as.factor(cov_factor)),alpha=0.4)+ 
          scale_fill_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
        
        haz_trans_r = haz_trans_r+ facet_wrap(~factor(as.factor(trans_factor),levels=labels_trans()))
        
        haz_trans_r = haz_trans_r + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx )))+
                                                      scale_y_continuous(breaks=c(seq(input$starthy,input$endhy,by=input$stephy )))
        haz_trans_r = haz_trans_r +labs(title="Ratio of hazards for each transition among covariate patterns (compared to ref. cov pattern)",
                                        x=paste0("Time since ",input$origin," entry"), y="Ratio of hazards for each transition")
        
        haz_trans_r = haz_trans_r + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
        
        haz_trans_r = haz_trans_r +theme(title = element_text(size = input$textsizeh-4),strip.text = element_text(size=input$textfaceth),       
                                         legend.title = element_text(color="black", size= input$textsizeh-5), 
                                         legend.text=element_text(size= input$textsizeh-6),
                                         plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                         legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                         legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                         axis.title.y = element_text(size= input$textsizeh-5),
                                         axis.title.x = element_text(size= input$textsizeh-5), 
                                         axis.text.x = element_text( size=input$textsizeh-6),axis.text.y = element_text( size=input$textsizeh-6)) 
        
        haz_trans_r = ggplotly(haz_trans_r, tooltip = "text")%>%
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
    
    
    haz_trans_r
    
  }
}) 

output$H_ratio <- renderPlotly ({ datah3_re() })

output$downploth3 <- downloadHandler(
  filename = function(){paste("h3",'.png',sep='')},
  content = function(file){
    
  #  svg <- plotly_gadget(datah3_re())
  #  
  #  png_gadget <- tempfile(fileext=".png")
  #  
  #  rsvg_png(charToRaw(svg), png_gadget)
  #  
  #  png_gadget
  plotly_IMAGE( datah3_re(),width = 1200, height = 900, format = "png", scale = 2,  out_file = file )
  }
)
  
##########################################################################################################################
##########################################################################################################################

#output$shouldloadh4 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downploth4", label = h2("Download the plot"))
#})

datah4_re <-  reactive ({


  
  if (input$confh=="ci_no") {  
          
          if (is.null(myjson2()$is.cumhaz)==FALSE) {
    if (myjson2()$is.cumhaz==1) {hazname= "Cummulative hazards"}
    else if (myjson2()$is.cumhaz!=1)  {hazname= "Transition intensity rate"}
  }
          if (is.null(myjson2()$is.cumhaz)==TRUE) {hazname="Transition intensity rate"}
          
          
          if (input$scaleh=="log") {

      
               if (input$faceth=="No") {
        
    
    
         h_cov= plot_ly(data_H_d(),alpha=0.5) %>%
           add_lines(
             x=data_H_d()$timevar,y=log(data_H_d()$V),
             frame=factor(as.factor(data_H_d()$cov_factor),levels=labels_cov()),
             color=as.factor(data_H_d()$trans_factor),
             colors=labels_colour_trans()[1:length(levels(data_H_d()$trans_factor))],
             mode="lines",
             line=list(simplify=FALSE),
             text = 'Select or deselect lines by clicking on the legend',
             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") 
           )  %>%
           layout(title=list(text=paste0(hazname," among covariate patterns"),y=0.95),
                  font= list(family = "times new roman", size = input$textsizeh, color = "black"),
                  margin = list(l = 50, r = 50, b = 30, t = 70),
                  xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
                             dtick = input$stephx, 
                             tick0 = input$starthx, 
                             range=c(input$starthx,input$endhx),
                             ticklen = 5,
                             tickwidth = 2,
                             tickcolor = toRGB("black"),
                             tickmode = "linear"),
                  yaxis =list(title=  paste0(hazname," (log scale)"),                    
                              dtick = input$logstephy, 
                              tick0 = input$logstarthy, 
                              range=c(input$logstarthy,input$logendhy),
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
           h_cov= h_cov %>%
             animation_opts(frame = 1000, transition = 0, redraw = FALSE)
         }
        
          h_cov
      }
    
               if (input$faceth=="Yes") {
      
      
           data_plot=data_H_d()
           
           h_cov = ggplot(data_plot)
           h_cov = ggplot(data_plot,aes(x=timevar, y=log(V), color= as.factor(trans_factor)))
           
           h_cov = h_cov+geom_line(aes(x=timevar, y=log(V), color= as.factor(trans_factor), group=1,
                                       text=paste("Select or deselect lines by clicking on the legend",
                                                  "<br>Time: ", timevar,
                                                  "<br>Log hazards: ", log(V),
                                                  "<br>Transition: ",  as.factor(trans_factor))))
           
           h_cov = h_cov  + scale_colour_manual( values =labels_colour_trans(),labels = labels_trans()  ) 
           
           h_cov = h_cov+ facet_wrap(~factor(as.factor(cov_factor),levels=labels_cov()))
           
           h_cov = h_cov + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx  )))  
                                                     #  scale_y_continuous(breaks=c(seq(input$logstarthy,input$logendhy,by=input$logstephy  )))
           
           
          h_cov = h_cov + scale_y_continuous(breaks=c(seq(input$logstarthy,input$logendhy,by=input$logstephy  )))  
           

           
           h_cov = h_cov +labs(title=paste0(hazname," for each state among covariate patterns"), x=paste0("Time since ",input$origin," entry"), y=paste0(hazname," (log scale)"))
           
           h_cov = h_cov + labs(color = "States")+ labs(fill = "States")
           
           h_cov = h_cov +theme(title = element_text(size = input$textsizeh-4), strip.text = element_text(size=input$textfaceth),      
                                    legend.title = element_text(color="black", size= input$textsizeh-5), 
                                    legend.text=element_text(size= input$textsizeh-6),
                                    plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                    legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                    legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                    axis.title.y = element_text(size= input$textsizeh-5),
                                    axis.title.x = element_text(size= input$textsizeh-5), 
                                axis.text.x = element_text( size=input$textsizeh-6),axis.text.y = element_text( size=input$textsizeh-6))  
           
           h_cov = ggplotly(h_cov, tooltip = "text")%>%
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
           
           h_cov 
           
    }
           
     }
          
          if (input$scaleh=="normal") {
    
               if (input$faceth=="No") {
    
               h_cov= plot_ly(data_H_d(),alpha=0.5) %>%
                 add_lines(
                   x=data_H_d()$timevar,y=data_H_d()$V,
                   frame=factor(as.factor(data_H_d()$cov_factor),levels=labels_cov()),
                   color=as.factor(data_H_d()$trans_factor),
                   colors=labels_colour_trans()[1:length(levels(data_H_d()$trans_factor))],
                   mode="lines",
                   line=list(simplify=FALSE),
                   text = 'Select or deselect lines by clicking on the legend',
                   hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                         "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") 
                 )  %>%
                 layout(title=list(text=paste0(hazname," for each state among covariate patterns"),y=0.95),
                        font= list(family = "times new roman", size = input$textsizeh, color = "black"),
                        margin = list(l = 50, r = 50, b = 30, t = 70),
                        xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
                                   dtick = input$stephx, 
                                   tick0 = input$starthx, 
                                   range=c(input$starthx,input$endhx),
                                   ticklen = 5,
                                   tickwidth = 2,
                                   tickcolor = toRGB("black"),
                                   tickmode = "linear"),
                        yaxis =list(title=  hazname, rangemode = "nonnegative",                    
                                    tick0 = input$starthy,
                                    dtick = input$stephy, 
                                    range=c(input$starthy,input$endhy),
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
                 h_cov= h_cov %>%
                   animation_opts(frame = 1000, transition = 0, redraw = FALSE)
               } 
               
               h_cov
           }
 
               if (input$faceth=="Yes") {
    
    
              data_plot=data_H_d()
              
              h_cov = ggplot(data_plot)
              h_cov = ggplot(data_plot,aes(x=timevar, y=V, color= as.factor(trans_factor)))
              
              h_cov = h_cov+geom_line(aes(x=timevar, y=V, color= as.factor(trans_factor), group=1,
                                          text=paste("Select or deselect lines by clicking on the legend",
                                                     "<br>Time: ", timevar,
                                                     "<br>Hazard: ", V,
                                                     "<br>Transition: ",  as.factor(trans_factor))))+
                scale_colour_manual( values =labels_colour_trans(),labels = labels_trans()  ) 
              
              h_cov = h_cov+ facet_wrap(~factor(as.factor(cov_factor),levels=labels_cov()), ncol=2)
              
              h_cov = h_cov + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx  )))  
                                                 #scale_y_continuous(breaks=c(seq(input$starthy,input$endhy,by=input$stephy  )))
              
             # h_cov = h_cov + scale_y_continuous(breaks=c(seq(min(data_plot$V[which(!is.na(data_plot$V))]),max(data_plot$V[which(!is.na(data_plot$V))]), by=input$stephy )))
              
              h_cov = h_cov + scale_y_continuous(breaks=c(seq(input$starthy,input$endhy,by=input$stephy  )))  
              
              
              h_cov = h_cov +labs(title=paste0(hazname," for each state among covariate patterns"), x=paste0("Time since ",input$origin," entry"), y=hazname)
              
              h_cov = h_cov + labs(color = "States")+ labs(fill = "States")
              
              h_cov = h_cov +theme(title = element_text(size = input$textsizeh-4), strip.text = element_text(size=input$textfaceth),      
                                   legend.title = element_text(color="black", size= input$textsizeh-5), 
                                   legend.text=element_text(size= input$textsizeh-6),
                                   plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                   legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                   legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                   axis.title.y = element_text(size= input$textsizeh-5),
                                   axis.title.x = element_text(size= input$textsizeh-5), 
                                   axis.text.x = element_text( size=input$textsizeh-6),axis.text.y = element_text( size=input$textsizeh-6))  
              
              h_cov = ggplotly(h_cov, tooltip = "text")%>%
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
              
              h_cov 
    
      }
    }
  }
  
  if (input$confh=="ci_yes") {  
     
         if (is.null(myjson2()$is.cumhaz)==FALSE) {
      if (myjson2()$is.cumhaz==1) {hazname= "Cummulative hazards"}
      else if (myjson2()$is.cumhaz!=1)  {hazname= "Transition intensity rate"}
    }
         if (is.null(myjson2()$is.cumhaz)==TRUE) {hazname="Transition intensity rate"}
         
         
         if (input$scaleh=="log") {
      
      
               if (input$faceth=="No") {
                 
                 
                 
                 h_cov= plot_ly(data_H_d(),alpha=0.5) %>%
                   add_lines(
                     x=data_H_d()$timevar,y=log(data_H_d()$V),
                     frame=factor(as.factor(data_H_d()$cov_factor),levels=labels_cov()),
                     color=as.factor(data_H_d()$trans_factor),
                     colors=labels_colour_trans()[1:length(levels(data_H_d()$trans_factor))],
                     mode="lines",
                     line=list(simplify=FALSE),
                     text = 'Select or deselect lines by clicking on the legend',
                     hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                           "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") 
                   )  
                 
                 h_cov <- add_trace(h_cov, fill = "tozerox", 
                                      line=list(dash = "solid", color = "transparent", width = 1.8897637),
                                      mode = "lines", type = "scatter",
                                      x=data_H_ci()$x, y=log(data_H_ci()$y),
                                      frame=factor(as.factor(data_H_ci()$covto),levels=labels_cov()), 
                                      colors=labels_colour_trans(),
                                      color=as.factor(data_H_ci()$frameto),
                                      showlegend = FALSE,
                                      text = 'Select or deselect lines by clicking on the legend',
                                      hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                            "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
                                    ) %>%
                   layout(title=list(text=paste0(hazname," among covariate patterns"),y=0.95),
                          font= list(family = "times new roman", size = input$textsizeh, color = "black"),
                          margin = list(l = 50, r = 50, b = 30, t = 70),
                          xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
                                     dtick = input$stephx, 
                                     tick0 = input$starthx, 
                                     range=c(input$starthx,input$endhx),
                                     ticklen = 5,
                                     tickwidth = 2,
                                     tickcolor = toRGB("black"),
                                     tickmode = "linear"),
                          yaxis =list(title=  paste0(hazname," (log scale)"),                    
                                      dtick = input$logstephy, 
                                      tick0 = input$logstarthy, 
                                      range=c(input$logstarthy,input$logendhy),
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
                   h_cov= h_cov %>%
                     animation_opts(frame = 1000, transition = 0, redraw = FALSE)
                 }
                 
                 h_cov
          }
      
               if (input$faceth=="Yes") {
        
        
                V_lci=  data_H_d_lci()$V
                V_uci=  data_H_d_uci()$V
                
                data_plot=cbind(data_H_d(),V_lci,V_uci)
                
                h_cov = ggplot(data_plot)

                h_cov =ggplot(data_plot,aes(x=timevar, y=log(V), color= as.factor(trans_factor), group=1,
                                            text=paste("Select or deselect lines by clicking on the legend",
                                                       "<br>Time: ", timevar,
                                                       "<br>Log hazards: ", log(V),
                                                       "<br>Transition: ",  as.factor(trans_factor))))+
                  scale_colour_manual( values =labels_colour_trans(),labels = labels_trans()  ) 
                
                h_cov=h_cov+geom_line(aes(x=timevar, y=log(V), fill= as.factor(trans_factor)))
                
                
                h_cov=h_cov+ geom_ribbon(aes(ymin = log(V_lci), ymax =log(V_uci),fill=as.factor(trans_factor)),alpha=0.4)+ 
                  scale_fill_manual( values =labels_colour_trans(),labels = labels_trans()  ) 
                
                h_cov = h_cov+ facet_wrap(~factor(as.factor(cov_factor),levels=labels_cov()))
                
                h_cov = h_cov + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx  )))  
                  #scale_y_continuous(breaks=c(seq(input$logstarthy,input$logendhy,by=input$logstephy  )))
                
            
               # h_cov = h_cov + scale_y_continuous(breaks=c(seq(min(data_plot$V_lci[which(!is.na(data_plot$V_lci))]),max(data_plot$V_uci[which(!is.na(data_plot$V_uci))]),by=input$logstephy )))
                
                h_cov = h_cov + scale_y_continuous(breaks=c(seq(input$logstarthy,input$logendhy,by=input$logstephy  )))  
                
                h_cov = h_cov +labs(title=paste0(hazname," for each state among covariate patterns"), x=paste0("Time since ",input$origin," entry"), y=paste0(hazname," (log scale)"))
                
                h_cov = h_cov + labs(color = "Transitions")+ labs(fill = "Transitions")
                
                h_cov = h_cov +theme(title = element_text(size = input$textsizeh-4),strip.text = element_text(size=input$textfaceth),       
                                     legend.title = element_text(color="black", size= input$textsizeh-5), 
                                     legend.text=element_text(size= input$textsizeh-6),
                                     plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                     legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                     legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                     axis.title.y = element_text(size= input$textsizeh-5),
                                     axis.title.x = element_text(size= input$textsizeh-5), 
                                     axis.text.x = element_text( size=input$textsizeh-6),axis.text.y = element_text( size=input$textsizeh-6))  
                
                
                
                h_cov = ggplotly(h_cov, tooltip = "text")%>%
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
                
                h_cov 
                
       } #facet
      
    } #scale
         
         if (input$scaleh=="normal") {
      
                    if (input$faceth=="No") {
                      
                      h_cov= plot_ly(data_H_d(),alpha=0.5) %>%
                        add_lines(
                          x=data_H_d()$timevar,y=data_H_d()$V,
                          frame=factor(as.factor(data_H_d()$cov_factor),levels=labels_cov()),
                          color=as.factor(data_H_d()$trans_factor),
                          colors=labels_colour_trans()[1:length(levels(data_H_d()$trans_factor))],
                          mode="lines",
                          line=list(simplify=FALSE),
                          text = 'Select or deselect lines by clicking on the legend',
                          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") 
                        )  
                      h_cov <- add_trace(h_cov, fill = "tozerox", 
                                         line=list(dash = "solid", color = "transparent", width = 1.8897637),
                                         mode = "lines", type = "scatter",
                                         x=data_H_ci()$x, y=data_H_ci()$y,
                                         frame=factor(as.factor(data_H_ci()$covto),levels=labels_cov()), 
                                         colors=labels_colour_trans(),
                                         color=as.factor(data_H_ci()$frameto),
                                         showlegend = FALSE,
                                         text = 'Select or deselect lines by clicking on the legend',
                                         hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                               "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
                      ) %>%
                        layout(title=list(text=paste0(hazname," for each state among covariate patterns"),y=0.95),
                               font= list(family = "times new roman", size = input$textsizeh, color = "black"),
                               margin = list(l = 50, r = 50, b = 30, t = 70),
                               xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
                                          dtick = input$stephx, 
                                          tick0 = input$starthx, 
                                          range=c(input$starthx,input$endhx),
                                          ticklen = 5,
                                          tickwidth = 2,
                                          tickcolor = toRGB("black"),
                                          tickmode = "linear"),
                               yaxis =list(title=  hazname, rangemode = "nonnegative",                    
                                           tick0 = input$starthy,
                                           dtick = input$stephy, 
                                           range=c(input$starthy,input$endhy),
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
                        h_cov= h_cov %>%
                          animation_opts(frame = 1000, transition = 0, redraw = FALSE)
                      } 
                      
                      h_cov
                    }
                    
                    
                    
                    if (input$faceth=="Yes") {
                      
                      
                      
                      V_lci=  data_H_d_lci()$V
                      V_uci=  data_H_d_uci()$V
                      
                      data_plot=cbind(data_H_d(),V_lci,V_uci)
                      
                      h_cov = ggplot(data_plot)
                      
                      h_cov =ggplot(data_plot,aes(x=timevar, y=V, color= as.factor(trans_factor), group=1,
                                                  text=paste("Select or deselect lines by clicking on the legend",
                                                             "<br>Time: ", timevar,
                                                             "<br>Log hazards: ", V,
                                                             "<br>Transition: ",  as.factor(trans_factor))))+
                        scale_colour_manual( values =labels_colour_trans(),labels = labels_trans()  ) 
                      
                      h_cov=h_cov+geom_line(aes(x=timevar, y=V, fill= as.factor(trans_factor)))
                      
                      
                      h_cov=h_cov+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill=as.factor(trans_factor)),alpha=0.4)+ 
                        scale_fill_manual( values =labels_colour_trans(),labels = labels_trans()  ) 
                      
                                          
                      h_cov = h_cov+ facet_wrap(~factor(as.factor(cov_factor),levels=labels_cov()), ncol=2)
                      
                      h_cov = h_cov + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx  )))  
                        #scale_y_continuous(breaks=c(seq(input$starthy,input$endhy,by=input$stephy  )))
                      

                     # h_cov = h_cov  + scale_y_continuous(breaks=c(seq(min(data_plot$V_lci[which(!is.na(data_plot$V_lci))]),max(data_plot$V_uci[which(!is.na(data_plot$V_uci))]),by=input$stephy )))
                      
                      h_cov = h_cov + scale_y_continuous(breaks=c(seq(input$starthy,input$endhy,by=input$stephy  )))  
                      
                      h_cov = h_cov +labs(title=paste0(hazname," for each state among covariate patterns"), x=paste0("Time since ",input$origin," entry"), y=hazname)
                      
                      h_cov = h_cov + labs(color = "Transitions")+ labs(fill = "Transitions")
                      
                      h_cov = h_cov +theme(title = element_text(size = input$textsizeh-4), strip.text = element_text(size=input$textfaceth),       
                                           legend.title = element_text(color="black", size= input$textsizeh-5), 
                                           legend.text=element_text(size= input$textsizeh-6),
                                           plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                           legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                           legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                           axis.title.y = element_text(size= input$textsizeh-5),
                                           axis.title.x = element_text(size= input$textsizeh-5), 
                                           axis.text.x = element_text( size=input$textsizeh-6),axis.text.y = element_text( size=input$textsizeh-6))  
                      
                      h_cov = ggplotly(h_cov, tooltip = "text")%>%
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
                      
                      h_cov 
                      
                    }
                  }
                  
         }
    h_cov
  }) 

output$hazards_cov <- renderPlotly ({  datah4_re() }) 
  
output$downploth4 <- downloadHandler(
  filename = function(){paste("h4",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datah4_re(),width = 1200, height = 900, format = "png", scale = 2,  out_file = file )
  }
)


###########################################################################################

############################################################################################
data_H_d_all <- reactive ({
  if(is.null(myjson2())) 
    return()
  
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups
  
  timevar2=as.data.frame(myjson2()$timevar)
  names(timevar2)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {
    v[i]=myjson2()$cov$atlist[i]
  }
  
  
  ## Different variable of probabilities for each covariate pattern
  haz=list()
  
  if (length(myjson2()$haz)==0) {return()}
  
  for(i in 1:length(myjson2()$haz_all)) {
    
    haz[[i]]=as.data.frame(t(data.frame(myjson2()$haz_all[i])))
    colnames( haz[[i]]) <- labels_cov()
  }
  
  for(i in 1:length(myjson2()$haz_all)) {  
    haz[[i]]=as.data.frame(cbind( haz[[i]],timevar2 ,trans=rep(i,nrow( haz[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  datap_H=list()
  datap_H[[1]]=haz[[1]]
  
  if (length(myjson2()$haz_all)>1 )  { 
      for (u in 2:(length(myjson2()$haz_all))) {
          datap_H[[u]]=rbind( haz[[u]],datap_H[[(u-1)]])
      }
    
  }
  
  datah=datap_H[[length(myjson2()$haz_all)]]
  datah 
  
  datanew=datah
  
  datanew$trans_factor=c(rep("NA",nrow(datanew)))
  
  for (o in 1:(length(myjson2()$haz_all))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$trans[g]==o) {datanew$trans_factor[g]=paste0("Trans",o) }  
    }
  }
  
  ### Meke one variable of hazards so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(datanew[,d],datanew[,ncol(datanew)-2],datanew[,ncol(datanew)-1],
                                datanew[,ncol(datanew)],rep(d,length(datanew[,d])) )
    dlist[[d]][,6] <- rep(colnames(datanew)[d],length(datanew[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","trans","trans_factor","cov","cov_factor")
  }
  
  d_all_h <- bind_rows(dlist, .id = "column_label")
  d_all_h
  
  
})
############################################################################################
###########################################################################################
data_H_trans <- reactive ({
  

 
   hr_trans=list()
  p=1
  for (i in levels(as.factor(data_H_d_all()$cov_factor)) ) {
    for (k in 1:myjson2()$Ntransitions) {
      for (j in 1:myjson2()$Ntransitions) {
        
        
        ratio=data_H_d_all()[which(data_H_d_all()$cov_factor==i & data_H_d_all()$trans==k),2]/data_H_d_all()[which(data_H_d_all()$cov_factor==i & data_H_d_all()$trans==j),2]
        timevar=data_H_d_all()$timevar
        nominator_trans=rep(k,length(myjson2()$timevar))
        denominator_trans=rep(j,length(myjson2()$timevar))
        cov_factor=rep(i,length(myjson2()$timevar))
        hr_trans[[p]]=cbind.data.frame(ratio,timevar,nominator_trans,denominator_trans,cov_factor)
        colnames(hr_trans[[p]]) <- c("ratio","timevar","nominator_trans","denominator_trans","cov_factor")
        p=p+1
      }
    }
  }
  hr_trans_d= bind_rows(hr_trans, .id = "column_label")
  
  hr_trans_d=hr_trans_d[which(hr_trans_d$nominator_trans!=hr_trans_d$denominator_trans),]
  
  hr_trans_d$ratiolab=paste0("Trans", hr_trans_d$nominator_trans," vs ","Trans",hr_trans_d$denominator_trans)    
  
  hr_trans_d=hr_trans_d[which(hr_trans_d$nominator_trans<hr_trans_d$denominator_trans),]
  
  hr_trans_d
})
###############################################################################################################



#output$shouldloadh5 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downploth5", label = h2("Download the plot"))
#})

datah5_re <-  reactive ({
  
  if (is.null(myjson2()$is.cumhaz)==FALSE) {
    if (myjson2()$is.cumhaz==1) {hazname= "Cummulative hazard ratios"}
    else if (myjson2()$is.cumhaz!=1)  {hazname= "Intensity rates ratios"}
  }
  if (is.null(myjson2()$is.cumhaz)==TRUE) {hazname="Intensity rates ratios"}

  if (input$scaleh_ld=="normal") {
      
       if (input$faceth=="No") {
      
    
                h_trans= plot_ly(data_H_trans(),alpha=0.5) %>%
                  add_lines(
                    x=data_H_trans()$timevar,y=data_H_trans()$ratio,
                    frame=factor(as.factor(data_H_trans()$cov_factor),levels=labels_cov()),
                    color=as.factor(data_H_trans()$ratiolab),
                    mode="lines",
                    line=list(simplify=FALSE),
                    text = 'Select or deselect lines by clicking on the legend',
                    hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                          "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") 
                  )  %>%
                layout(title=list(text=paste0(hazname," among covariate patterns"),y=0.95),
                       font= list(family = "times new roman", size = input$textsizeh, color = "black"),
                       margin = list(l = 50, r = 50, b = 30, t = 70),
                       xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
                                  dtick = input$stephx, 
                                  tick0 = input$starthx, 
                                  range=c(input$starthx,input$endhx),
                                  ticklen = 5,
                                  tickwidth = 2,
                                  tickcolor = toRGB("black"),
                                  tickmode = "linear"),
                       yaxis =list(title= hazname, rangemode = "nonnegative",
                                   range=c(0,max(data_H_trans()$ratio)),
                                   dtick = input$stephy, 
                                   ticklen = 5,
                                   tickwidth = 2,
                                   tickcolor = toRGB("black")),
                       legend = list(x = 150, y = 1),
                       shapes = list(
                         list(type = "rect",
                              fillcolor = "grey", 
                              line = list(color = "grey"), 
                              opacity = 0.8,
                              x0 = 0, x1 =0, xref = "x", y0 = 0, y1 = 1, yref = "y") )  )  %>%
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
   
        if (input$faceth=="Yes") {
         
          
               data_plot=data_H_trans()
              
               h_trans = ggplot(data_plot)
               h_trans = ggplot(data_plot,aes(x=timevar, y=ratio, color= as.factor(ratiolab), group=1,
                                              text=paste("Select or deselect lines by clicking on the legend",
                                                         "<br>Time: ", timevar,
                                                         "<br>Ratio: ", ratio,
                                                         "<br>Transition ratios: ",  as.factor(ratiolab))))
              
               h_trans = h_trans+geom_line(aes(x=timevar, y=ratio, color= as.factor(ratiolab)))

               h_trans = h_trans+ facet_wrap(~factor(as.factor(cov_factor),levels=labels_cov()))
              
               h_trans = h_trans + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx  )))  
                                                     #scale_y_continuous(breaks=c(seq(0,input$endhy,by=input$stephy  )))
               
              # h_trans = h_trans + scale_y_continuous(breaks=c(seq(min(data_plot$V[which(!is.na(data_plot$V))]),max(data_plot$V[which(!is.na(data_plot$V))]), by=input$stephy )))
               
         
              
               h_trans = h_trans +labs(title=paste0(hazname," among covariate patterns"), x=paste0("Time since ",input$origin," entry"), y=hazname)
              
               h_trans = h_trans + labs(color = "States")+ labs(fill = "States")
              
               h_trans = h_trans +theme(title = element_text(size = input$textsizeh-4), strip.text = element_text(size=input$textfaceth),      
                                    legend.title = element_text(color="black", size= input$textsizeh-5), 
                                    legend.text=element_text(size= input$textsizeh-6),
                                    plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                    legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                    legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                    axis.title.y = element_text(size= input$textsizeh-5),
                                    axis.title.x = element_text(size= input$textsizeh-5), 
                                    axis.text.x = element_text( size=input$textsizeh-6),axis.text.y = element_text( size=input$textsizeh-6))  
              
               h_trans = ggplotly(h_trans, tooltip = "text")%>%
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
  
  if (input$scaleh_ld=="log") {
         
          if (input$faceth=="No") {
           
    
              h_trans= plot_ly(data_H_trans(),alpha=0.5) %>%
                add_lines(
                  x=data_H_trans()$timevar,y=log(data_H_trans()$ratio),
                  frame=factor(as.factor(data_H_trans()$cov_factor),levels=labels_cov()),
                  color=as.factor(data_H_trans()$ratiolab),
                  mode="lines",
                  line=list(simplify=FALSE),
                  text = 'Select or deselect lines by clicking on the legend',
                  hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                        "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") 
                )  %>%
                layout(title=list(text=paste0(hazname," (log scale) among covariate patterns"),y=0.95),
                       font= list(family = "times new roman", size = input$textsizeh, color = "black"),
                       margin = list(l = 50, r = 50, b = 30, t = 70),
                       xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
                                  dtick = input$stephx, 
                                  tick0 = input$starthx, 
                                  range=c(input$starthx,input$endhx),
                                  ticklen = 5,
                                  tickwidth = 2,
                                  tickcolor = toRGB("black"),
                                  tickmode = "linear"),
                       yaxis =list(title= paste0(hazname," (log scale)"),rangemode = "nonnegative",                    
                                   dtick = input$logstephy, 
                                   tick0 = input$logstarthy, 
                                   range=c(input$logstarthy,input$logendhy),
                                   ticklen = 5,
                                   tickwidth = 2,
                                   tickcolor = toRGB("black")),
                       legend = list(x = 150, y = 1),
                       shapes = list(
                         list(type = "rect",
                              fillcolor = "grey", 
                              line = list(color = "grey"), 
                              opacity = 0.8,
                              x0 = 0, x1 =0, xref = "x", y0 = 0, y1 = 1, yref = "y") )  ) %>%
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
        
        
        if (input$faceth=="Yes") {
          
                
                data_plot=data_H_trans()
                
                h_trans = ggplot(data_plot)
                h_trans = ggplot(data_plot,aes(x=timevar, y=log(ratio), color= as.factor(ratiolab), group=1,
                                               text=paste("Select or deselect lines by clicking on the legend",
                                                          "<br>Time: ", timevar,
                                                          "<br>Log Ratio: ", log(ratio),
                                                          "<br>Transition ratios: ",  as.factor(ratiolab))))
                
                h_trans = h_trans+geom_line(aes(x=timevar, y=log(ratio), color= as.factor(ratiolab)))

                h_trans = h_trans+ facet_wrap(~factor(as.factor(cov_factor),levels=labels_cov()))
                
                h_trans = h_trans + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx  )))  
                  #scale_y_continuous(breaks=c(seq(input$logstarthy,input$logendhy,by=input$logstephy  )))
                
          #      h_trans = h_trans +  scale_y_continuous(breaks=c(seq(min(V[which(!is.na(V))]),max(V[which(!is.na(V))]),
                                                                 #    by=max(V[which(!is.na(V))])-min(V[which(!is.na(V))]) )))
                

                
                h_trans = h_trans +labs(title=paste0(hazname," among covariate patterns"), x=paste0("Time since ",input$origin," entry"), y=hazname)
                
                h_trans = h_trans + labs(color = "States")+ labs(fill = "States")
                
                h_trans = h_trans +theme(title = element_text(size = input$textsizeh-4),strip.text = element_text(size=input$textfaceth),       
                                         legend.title = element_text(color="black", size= input$textsizeh-5), 
                                         legend.text=element_text(size= input$textsizeh-6),
                                         plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                         legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                         legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                         axis.title.y = element_text(size= input$textsizeh-5),
                                         axis.title.x = element_text(size= input$textsizeh-5), 
                                         axis.text.x = element_text( size=input$textsizeh-6),axis.text.y = element_text( size=input$textsizeh-6))  

                h_trans = ggplotly(h_trans, tooltip = "text")%>%
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
    
h_trans

}) 

output$hr_transitions <- renderPlotly ({ datah5_re()  })
  

output$downploth5 <- downloadHandler(
  filename = function(){paste("h5",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datah5_re(),width = 1200, height = 900, format = "png", scale = 2,  out_file = file )
  }
)


#######################################################################

  
data_H_cov <- reactive ({
    
hr_cov=list()
#hr_trans=array(dim=c(length(myjson$timevar),5,length(myjson$cov$atlist)*myjsonbox$Ntransitions^2),"NA")
p=1
for (i in  1:myjson2()$Ntransitions ) {
  for (k in levels(as.factor(data_H_d()$cov_factor))) {
    for (j in levels(as.factor(data_H_d()$cov_factor))) {
      
      
      ratio=data_H_d()[which(data_H_d()$trans==i & data_H_d()$cov_factor==k),2]/data_H_d()[which(data_H_d()$trans==i & data_H_d()$cov_factor==j),2]
      timevar=data_H_d()$timevar
      nominator_cov=rep(k,length(myjson2()$timevar))
      denominator_cov=rep(j,length(myjson2()$timevar))
      trans=rep(i,length(myjson2()$timevar))
      trans_factor=rep(input[[paste0('trans',i)]][1],length(myjson2()$timevar)) 
      ratiolab=paste0("HR ",nominator_cov," vs ",denominator_cov)
      hr_cov[[p]]=as.data.frame(cbind.data.frame(ratio,timevar,nominator_cov,denominator_cov,trans,trans_factor,ratiolab))
      colnames(hr_cov[[p]]) <- c("ratio","timevar","nominator_cov","denominator_cov","trans","trans_factor","ratiolab")
      p=p+1
    }
  }
}

hr_cov_d= as.data.frame(bind_rows(hr_cov, .id = "column_label"))

hr_cov_d=as.data.frame(hr_cov_d[which(hr_cov_d$nominator_cov!=hr_cov_d$denominator_cov),])


hr_cov_d

  }) 

#####################################################################

#output$hr_covariate <- renderPlotly ({ 
#  
#
#  if (is.null(myjson2()$is.cumhaz)==FALSE) {
#    if (myjson2()$is.cumhaz==1) {hazname= "Cummulative hazard ratios"}
#    else if (myjson2()$is.cumhaz!=1)  {hazname= "Hazard ratios"}
#  }
#  if (is.null(myjson2()$is.cumhaz)==TRUE) {hazname= "Hazard ratios"}
#  
#  
#  if (input$scaleh_ld=="normal") {
#    
#        if (input$faceth=="No") {
#      
#    
#                    h_cov= plot_ly(data_H_cov(),alpha=0.5) %>%
#                      add_lines(
#                        x=data_H_cov()$timevar,y=data_H_cov()$ratio,
#                        frame=as.factor(data_H_cov()$trans_factor),
#                        color=as.factor(data_H_cov()$ratiolab),
#                        mode="lines",
#                        line=list(simplify=FALSE),
#                        text = 'Select or deselect lines by clicking on the legend',
#                        hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
#                                              "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") 
#                      )  %>%
#                      layout(title=list(text=paste0(hazname," between covariate patterns for each transition"),y=0.95),
#                             font= list(family = "times new roman", size = input$textsizeh, color = "black"),
#                             margin = list(l = 50, r = 50, b = 30, t = 70),
#                             xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
#                                        dtick = input$stephx, 
#                                        tick0 = input$starthx, 
#                                        range=c(input$starthx,input$endhx),
#                                        ticklen = 5,
#                                        tickwidth = 2,
#                                        tickcolor = toRGB("black"),
#                                        tickmode = "linear"),
#                             yaxis =list(title= hazname,rangemode = "nonnegative",
#                                         range=c(0,max(data_H_cov()$ratio[which(data_H_cov()$ratio<100)])),
#                                         dtick = input$stephy, 
#                                         ticklen = 5,
#                                         tickwidth = 2,
#                                         tickcolor = toRGB("black")),
#                             legend = list(x = 150, y = 1),
#                             shapes = list(
#                               list(type = "rect",
#                                    fillcolor = "grey", 
#                                    line = list(color = "grey"), 
#                                    opacity = 0.8,
#                                    x0 = 0, x1 =0, xref = "x", y0 = 0, y1 = 1, yref = "y") )  )  
#                    
#                    
#                    
#                    if (input$smooth=="No") {
#                      h_cov= h_cov %>%
#                        animation_opts(frame = 1000, transition = 0, redraw = FALSE)
#                    }
#       }
#    
#       if (input$faceth=="Yes") {
#      
#  
#                       data_plot=data_H_cov()
#                       
#                       h_cov = ggplot(data_plot)
#                       h_cov = ggplot(data_plot,aes(x=timevar, y=ratio, color= as.factor(ratiolab), group=1,
#                                                    text=paste("Select or deselect lines by clicking on the legend",
#                                                               "<br>Time: ", timevar,
#                                                               "<br>Ratio: ", ratio,
#                                                               "<br>Transition ratios: ",  as.factor(ratiolab))))
#                       
#                       h_cov = h_cov+geom_line(aes(x=timevar, y=ratio, color= as.factor(ratiolab)))
#
#                       h_cov = h_cov+ facet_wrap(~trans_factor)
#                       
#                       h_cov = h_cov + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx  )))  +
#                         scale_y_continuous(breaks=c(seq(0,input$endhy,by=input$stephy  )))
#                       
#                       h_cov = h_cov +labs(title=paste0(hazname," among covariate patterns"), x=paste0("Time since ",origin," entry"), y=hazname)
#                       
#                       h_cov = h_cov + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
#                       
#                       h_cov = h_cov +theme(title = element_text(size = input$textsizeh-4),       
#                                                legend.title = element_text(color="black", size= input$textsizeh-5), 
#                                                legend.text=element_text(size= input$textsizeh-6),
#                                                plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
#                                                legend.margin = margin(1.5, 1, 1, 1, "cm"),
#                                                legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
#                                                axis.title.y = element_text(size= input$textsizeh-5),
#                                                axis.title.x = element_text(size= input$textsizeh-5))   
#                       
#                       h_cov = ggplotly(h_cov, tooltip = "text") 
#  
#          }
#     }
#  
#        if (input$scaleh_ld=="log") {
#             
#              if (input$faceth=="No") {
#            
#                       h_cov= plot_ly(data_H_cov(),alpha=0.5) %>%
#                         add_lines(
#                           x=data_H_cov()$timevar,y=log(data_H_cov()$ratio),
#                           frame=as.factor(data_H_cov()$trans_factor),
#                           color=as.factor(data_H_cov()$ratiolab),
#                           mode="lines",
#                           line=list(simplify=FALSE),
#                           text = 'Select or deselect lines by clicking on the legend',
#                           hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
#                                                 "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") 
#                         )  %>%
#                         layout(title=list(text=paste0(hazname," (log scale) between covariate patterns for each transition"),y=0.95),
#                                font= list(family = "times new roman", size = input$textsizeh, color = "black"),
#                                margin = list(l = 50, r = 50, b = 30, t = 70),
#                                xaxis=list(title=list(text=paste0("Time since ",input$origin," entry"),y=0.2),
#                                           dtick = input$stephx, 
#                                           tick0 = input$starthx, 
#                                           range=c(input$starthx,input$endhx),
#                                           ticklen = 5,
#                                           tickwidth = 2,
#                                           tickcolor = toRGB("black"),
#                                           tickmode = "linear"),
#                                yaxis =list(title= paste0(hazname," (log scale)"),rangemode = "nonnegative",                    
#                                            dtick = input$logstephy, 
#                                            tick0 = input$logstarthy, 
#                                            range=c(input$logstarthy,input$logendhy),
#                                            ticklen = 5,
#                                            tickwidth = 2,
#                                            tickcolor = toRGB("black")),
#                                legend = list(x = 150, y = 1),
#                                shapes = list(
#                                  list(type = "rect",
#                                       fillcolor = "grey", 
#                                       line = list(color = "grey"), 
#                                       opacity = 0.8,
#                                       x0 = 0, x1 =0, xref = "x", y0 = 0, y1 = 1, yref = "y") )  ) 
#                       
#                       
#                       if (input$smooth=="No") {
#                         h_cov= h_cov %>%
#                           animation_opts(frame = 1000, transition = 0, redraw = FALSE)
#                       }
#                       
#                
#              }
#          
#          if (input$faceth=="Yes") {
#            
#                  data_plot=data_H_cov()
#                  
#                  h_cov = ggplot(data_plot)
#                  h_cov = ggplot(data_plot,aes(x=timevar, y=log(ratio), color= as.factor(ratiolab), group=1,
#                                               text=paste("Select or deselect lines by clicking on the legend",
#                                                          "<br>Time: ", timevar,
#                                                          "<br>Log ratio: ", log(ratio),
#                                                          "<br>Transition ratios: ",  as.factor(ratiolab))))
#                  
#                  h_cov = h_cov+geom_line(aes(x=timevar, y=log(ratio), color= as.factor(ratiolab)))
#
#                  h_cov = h_cov+ facet_wrap(~trans_factor)
#                  
#                  h_cov = h_cov + scale_x_continuous(breaks=c(seq(input$starthx,input$endhx,by=input$stephx  )))  +
#                                           scale_y_continuous(breaks=c(seq(0,input$logendhy,by=input$logstephy  )))
#                  
#                  h_cov = h_cov +labs(title=paste0(hazname," (log scale) between covariate patterns for each transition"), x=paste0("Time since ",input$origin," entry"), y=paste0(hazname," (log scale)"))
#                  
#                  h_cov = h_cov + labs(color ="Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
#                  
#                  h_cov = h_cov +theme(title = element_text(size = input$textsizeh-4),       
#                                       legend.title = element_text(color="black", size= input$textsizeh-5), 
#                                       legend.text=element_text(size= input$textsizeh-6),
#                                       plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
#                                       legend.margin = margin(1.5, 1, 1, 1, "cm"),
#                                       legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
#                                       axis.title.y = element_text(size= input$textsizeh-5),
#                                       axis.title.x = element_text(size= input$textsizeh-5))    
#                  
#                  h_cov = ggplotly(h_cov, tooltip = "text") 
#            
#          }
#          
#    }
#        
#  h_cov
#
#
#}) 
 