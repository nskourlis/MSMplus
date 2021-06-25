
###Note: Comparisons are deactivated for the extra tab temporarily





observeEvent(input$json2, {
  if( length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'Number')))==0 &
      length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'Next')))==0 &
      length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'Soj')))==0 &
      length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'First')))==0 ) {
    js$disableTab("mytab_extra")
    
  } 
}) 

observeEvent(input$csv2, {
  if( length(which(startsWith(names( read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'Number')))==0 &
      length(which(startsWith(names( read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'Next')))==0 &
      length(which(startsWith(names( read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'Soj')))==0 &
      length(which(startsWith(names( read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'First')))==0 ) {
    js$disableTab("mytab_extra")
    
  } 
}) 

observeEvent(input$example, {
  if( input$example=="Yes" ) {
    js$disableTab("mytab_extra")
  } 
}) 

observeEvent(input$example2, {
  if( input$example2=="Yes" ) {
    js$disableTab("mytab_extra")
  } 
}) 

observeEvent(input$compare_approach, {
  if( input$example=="Yes" ) {
    js$disableTab("mytab_extra")
  } 
}) 

observeEvent(input$compare_approach2, {
  if( input$example2=="Yes" ) {
    js$disableTab("mytab_extra")
  } 
}) 


observeEvent(input$aimtype, {
  if( input$aimtype=="compare" ) {
    js$disableTab("mytab_extra")
  } 
}) 

#observeEvent(input$json2, {
#  if( length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'Soj')))==0 &
#      length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'Next')))==0 &
#      length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'First')))==0 &
#      length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'Number')))==0) {
#    js$disableTab("mytab_extra")
#    
#  } 
#}) 
#
#observeEvent(input$csv2, {
#  if( length(which(startsWith(names(read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'Soj')))==0 &
#      length(which(startsWith(names(read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'Next')))==0 &
#      length(which(startsWith(names(read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'First')))==0 &
#      length(which(startsWith(names(read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'Number')))==0) {
#    js$disableTab("mytab_extra")
#    
#  } 
#  
#  if( length(which(startsWith(names(read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'Soj')))!=0 |
#      length(which(startsWith(names(read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'Next')))!=0 |
#      length(which(startsWith(names(read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'First')))!=0 |
#      length(which(startsWith(names(read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'Number')))!=0) { 
#    js$enableTab("mytab_extra")
#    
#  } 
#}) 
#

###### Show and hide  and tick inputs ####


timerextra <- reactiveVal(1.5)


observeEvent(c(input$showticknumber,invalidateLater(1000, session)), { 
  
  if(input$showticknumber=="No"){
    
    hide("tickinputnumber")
    
  }
  
  if(input$showticknumber=="Yes"){
    
    show("tickinputnumber")
    
  }
  
  isolate({
    
    timerextra(timerextra()-1)
    if(timerextra()>1 & input$showticknumber=="No")
    {
      show("tickinputnumber")
    }
    
  })
})


############################################

existnumber <- reactive({
  if (length(myjson2()$number) != 0) {       
    x= 1
  }
  else if (length(myjson2()$number) == 0) {       
    x= 0
  }
})


existfirst <- reactive({

  if (length(myjson2()$first ) == 0) {       
    x= 0
  }
  else if (length(myjson2()$first ) != 0  &  length(myjson2()$ci_first)==0) {       
    x= 1
  }
  else if (length(myjson2()$first ) != 0  &  length(myjson2()$ci_first)!=0) {       
    x= 2
  }
})

existnext <- reactive({

  if (length(myjson2()$nextv) == 0 ) {       
    x= 0
  }
  
  else if (length(myjson2()$nextv) != 0  &  length(myjson2()$ci_next)==0) {       
    x= 1
  }
  
  else if (length(myjson2()$nextv) != 0  &  length(myjson2()$ci_next)!=0) {       
    x= 2
  }
})

existsoj <- reactive({

  if (length(myjson2()$soj) == 0) {       
    x= 0
  }
  else if (length(myjson2()$soj) != 0 &  length(myjson2()$ci_soj)==0 )  {       
    x= 1
  }
  else if (length(myjson2()$soj)!= 0 & length(myjson2()$ci_soj)!=0) {       
    x= 2
  }
})

output$page_extra <- renderUI({
  
  if (is.null(myjson2())) return("Provide the json file with the predictions")

      
      
      fluidRow(

        
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,
               h1("Extra estimates"),
               
               conditionalPanel(condition="input.tabsextra =='#panel1extra'||input.tabsextra =='#panel2extra'",
                                uiOutput("facetnum"),
                                
                                uiOutput("confnum")
               ) ,


        ),
        
        column(2,
               br(),
               p(""),
               conditionalPanel(condition="input.tabsextra =='#panel1extra'||input.tabsextra =='#panel2extra'",
                                uiOutput("showticknumber"),
                                
                                
                                uiOutput("tickinputnumber")
               ) 

        ),
        
        column(8,
               tabsetPanel(id = "tabsextra",

                # tabPanel(h2("Expected number of visits by state"), id="#panel1extra", value = "#panel1extra",  plotlyOutput("number_state"  , height="600px", width = "100%"),uiOutput("shouldloadextra1")),
                 
                 
                 tabPanel(h2("Expected number of visits by state"), id="#panel1extra", value = "#panel1extra",
                          fluidRow( 
                            column(12, useShinyjs(),
                                   conditionalPanel(condition="output.test_number=='1'",
                                                    plotlyOutput("number_state"  , height="600px", width = "100%"),uiOutput("shouldloadextra1"),
                                   ),
                                   conditionalPanel(condition="output.test_number=='0'",
                                                    uiOutput("notab_number")
                                   )
                            ) 
                          )
                 ),
                 
                 
                # tabPanel(h2("Expected number of visits by cov pattern"),  value = "#panel2extra",  plotlyOutput("number_cov"  , height="600px", width = "100%"),uiOutput("shouldloadextra2")),
                
                tabPanel(h2("Expected number of visits by cov pattern"), id="#panel2extra", value = "#panel2extra",
                         fluidRow( 
                           column(12, useShinyjs(),
                                  conditionalPanel(condition="output.test_number=='1'",
                                                   plotlyOutput("number_cov"  , height="600px", width = "100%"),uiOutput("shouldloadextra2"),
                                  ),
                                  conditionalPanel(condition="output.test_number=='0'",
                                                   uiOutput("notab_number2")
                                  )
                           ) 
                         )
                ),
                
      #           tabPanel(h2("Next state"),         textOutput("next1"),  value = "#panel3extra",   DT:: dataTableOutput("next_state")),
                
                tabPanel(h2("Next state"), id="#panel3extra", value = "#panel3extra",
                         fluidRow( 
                           column(12, useShinyjs(),
                                  conditionalPanel(condition="output.test_next=='1'",
                                                   DT:: dataTableOutput("next_state"),
                                  ),
                                  conditionalPanel(condition="output.test_next=='0'",
                                                   uiOutput("notab_next")
                                  )
                           ) 
                         )
                ),  
                   
                
                
      #          tabPanel(h2("Mean sojourn times"), textOutput("soj1"),   value = "#panel4extra",   DT:: dataTableOutput("soj_state")),
               
               tabPanel(h2("Mean sojourn times"), id="#panel4extra", value = "#panel4extra",
                       fluidRow( 
                         column(12, useShinyjs(),
                                conditionalPanel(condition="output.test_soj=='1'",
                                                 DT:: dataTableOutput("soj_state"),
                                ),
                                conditionalPanel(condition="output.test_soj=='0'",
                                                 uiOutput("notab_soj")
                                )
                         ) 
                       )
              ), 
               
              #   tabPanel(h2("Expected first passage times"), textOutput("first1"),value = "#panel5extra",  DT:: dataTableOutput("first_state"))
            
               tabPanel(h2("Expected first passage times"), id="#panel5extra", value = "#panel5extra",
                        fluidRow( 
                          column(12, useShinyjs(),
                                 conditionalPanel(condition="output.test_first=='1'",
                                                  DT:: dataTableOutput("first_state"),
                                 ),
                                 conditionalPanel(condition="output.test_first=='0'",
                                                  uiOutput("notab_first")
                                 )
                          ) 
                        )
               ) 
                   
               )
            )
         )
      
  
  
})   

#################################################################
### code helping the conditional panel for Number ##############
################################################################

number_tab <- reactive({
  if (length(myjson2()$number) != 0) {       
    x= 1
  }
  else if (length(myjson2()$number) == 0) {       
    x= 0
  }
  x
})
  
output$test_number <- renderText({
    number_tab()
})


outputOptions(output, "test_number", suspendWhenHidden=FALSE)



output$notab_number<- renderUI({
  
  helpText("No information on expected number of visits was provided- Tab not available") 
  
})

output$notab_number2<- renderUI({
  
  helpText("No information on expected number of visits was provided- Tab not available") 
  
})
#################################################################
#################################################################
#################################################################
#
##################################################################
#### code helping the conditional panel for Next state ##############
#################################################################


next_tab <- reactive({
  if (length(myjson2()$nextv) != 0) {       
    x= 1
  }
  else if (length(myjson2()$nextv) == 0) {       
    x= 0
  }
  x
})



output$test_next <- renderText({
  next_tab()
})


outputOptions(output, "test_next", suspendWhenHidden=FALSE)



output$notab_next<- renderUI({
  
  helpText("No information on expected next state was provided- Tab not available") 
  
})

#########################################################################
#
#
##################################################################
#### code helping the conditional panel for sojourn ##############
#################################################################


soj_tab <- reactive({
  if (length(myjson2()$soj) != 0) {       
    x= 1
  }
  else if (length(myjson2()$soj) == 0) {       
    x= 0
  }
  x
})



output$test_soj <- renderText({
  soj_tab()
})


outputOptions(output, "test_soj", suspendWhenHidden=FALSE)



output$notab_soj<- renderUI({
  
  helpText("No information on sojourn states was provided- Tab not available") 
  
})

########################################################################


###################################################################################
#### code helping the conditional panel for first passage from states ##############
#####################################################################################


first_tab <- reactive({
  if (length(myjson2()$first) != 0) {       
    x= 1
  }
  else if (length(myjson2()$first) == 0) {       
    x= 0
  }
  x
})



output$test_first <- renderText({
  first_tab()
})


outputOptions(output, "test_first", suspendWhenHidden=FALSE)



output$notab_first<- renderUI({
  
  helpText("No information on first passage times was provided- Tab not available") 
  
})

#########################################################################
#






 
#observeEvent(input$json2, {
#    if( length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'Next')))==0  ) {
#      js$disableTab("#panel3extra")
#
#    }  
#})
#observeEvent(input$json2, {
#  if( length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'Soj')))==0  ) {
#    js$disableTab("#panel4extra")
#    
#  }  
#})
#observeEvent(input$json2, {
#  if( length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'First')))==0  ) {
#    js$disableTab("#panel5extra")
#    
#  }  
#})
#
#

#toListenExtra <- reactive({
#  list(input$json2,input$example,input$example2,input$compare_approach,input$compare_approach2)
#})
#
#observeEvent(toListenExtra(), {
#  if( length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'Number')))==0 &
#      length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'Next')))==0 &
#      length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'Soj')))==0 &
#      length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'First')))==0 ) {
#    js$disableTab("mytab_extra")
#    
#  }  
#  
#  if (input$example=="Yes"|input$example2=="Yes"|input$compare_approach=="Yes"|input$compare_approach2=="Yes") {
#    js$disableTab("mytab_extra")  
#  }
#})

  #else {
  #  shinyjs::enable(id = "mytab_visit")
  #}


#observeEvent(input$json2, {
#  if (length(fromJSON(input$json2$datapath, flatten=TRUE)$number)==0) {
#  hideTab(inputId = "tabs_start", target = "mytab_extra")
#  }
#})

#observeEvent(input$json2, {
#  if (length(fromJSON(input$json2$datapath, flatten=TRUE)$number)==0) {
#
#  js$disableTab("mytab_extra")
#  }
#  })




#  else {
#    js$enableTab("mytab_extra")
#    updateTabsetPanel(session,"tabs_start" ,"mytab_extra")  
#  }
# 
#})
#


#observeEvent({
#  toggleClass(selector = "#navbar li a[data-value=mytab_extra]", class = "disabled",
#              condition = length(fromJSON(input$json2$datapath, flatten=TRUE)$number)==0)
#})

#output$displaynumber <- renderUI({
#  radioButtons("displayvisit", "Change labels of states and covariate patterns",
#               c("same" = "same", "change"= "change"))
#})
output$showticknumber <- renderUI({
  
radioButtons("showticknumber", "Show axis tick options",
             choices = list("No" = "No",
                            "Yes" = "Yes"), selected = "No")

})


output$facetnum <- renderUI({
  
radioButtons(inputId="facetnum", label= "Display graph in grids",
             choices=c("No","Yes"),selected = "No")

})


output$confnum <- renderUI({
  
  if  (length(myjson2()$ci_number)!=0) {
    radioButtons("confnum", "Confidence intervals",
                 c("No" = "ci_no",
                   "Yes" ="ci_yes"))
  }
  
  else if (length(myjson2()$ci_number)==0) {
    item_list <- list()
    item_list[[1]]<- radioButtons("confnum", "Confidence intervals",c("No" = "ci_no"))
    item_list[[2]]<-print("Confidence interval data were not provided")
    do.call(tagList, item_list)
  }
  
})

output$numbers1 <- renderText({
  if ( length(myjson2()$number)>0)  {print("Expected number of visits")}
  else if ( length(myjson2()$number)==0)  {print("Non applicable")}
}) 

output$next1 <- renderText({
  if ( length(myjson2()$nextv)>0)  {print("Probability that each state is next")}
  else if ( length(myjson2()$nextv)==0)  {print("Non applicable")}
}) 

output$first1 <- renderText({
  if ( length(myjson2()$first)>0)  {print("Expected first passage times")}
  else if ( length(myjson2()$first)==0)  {print("Non applicable")}
}) 

output$soj1 <- renderText({
  if ( length(myjson2()$soj)>0)  {print("Mean sojourn times")}
  else if ( length(myjson2()$soj)==0)  {print("Non applicable")}
}) 
##################################################
###### Will appear conditionally##################
###################################################

#Create the reactive input of covariates
#output$covarinputnum <- renderUI({
#  
#  if (is.null(myjson2()))  return()
#  
#  if (input$displaynum=="same") return()
#  
#  else {
#    
#    item_list <- list()
#    item_list[[1]] <- h2("Covariate patterns")
#    
#    v=vector()
#    for (i in 1:length(myjson2()$cov$atlist)) {
#      v[i]=myjson2()$cov$atlist[i]
#    }
#    
#    default_choices_cov=v
#    
#    for (i in seq(length(myjson2()$cov$atlist))) {
#      item_list[[i+1]] <- textInput(paste0('covnum', i),default_choices_cov[i], labels_cov()[i])
#    }
#    
#    do.call(tagList, item_list)
#  }
#})
#
#labels_covnum<- reactive ({
#  
#  if (input$displaynum=="same") {labels_cov()}
#  
#  else {
#    
#    myList<-vector("list",length(myjson2()$cov$atlist))
#    for (i in 1:length(myjson2()$cov$atlist)) {
#      myList[[i]]= input[[paste0('covnum', i)]][1]
#    }
#    final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
#    final_list
#  }
#})

#Create the reactive input of states

#output$statesinputnum <- renderUI({
#  
#  if (input$displaynum=="same") return()
#  
#  else {
#    
#    item_list <- list()
#    item_list[[1]] <- h2("States")
#    default_choices_state=vector()
#    
#    title_choices_state=vector()
#    for (i in 1:length(myjson2()$P)) {
#      title_choices_state[i]=paste0("State"," ",input$select,selectend()[i])
#    }
#    for (i in 1:length(myjson2()$P)) {
#      
#      item_list[[1+i]] <- textInput(paste0('statenum',i),title_choices_state[i], labels_state()[i])
#      
#    }
#    do.call(tagList, item_list)
#  }
#})
#
#labels_statenum<- reactive ({
#  
#  if (input$displaynum=="same") {labels_state()}
#  else {
#    
#    myList<-vector("list",length(myjson2()$P))
#    
#    for (i in 1:length(myjson2()$P)) {
#      
#      myList[[i]]= input[[paste0('statenum', i)]][1]
#      
#    }
#    final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
#    final_list
#  }
#})
#
##################################################################################
###################################################################################



data_N <- reactive ({
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
  num=list()
  if (length(myjson2()$number)==0) {return()}
  
  for(i in 1:length(myjson2()$number)) {
    num[[i]]=as.data.frame(t(data.frame(myjson2()$number[i])))
    colnames(num[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$number)) {  
    num[[i]]=as.data.frame(cbind(num[[i]], timevar ,state=rep(i,nrow(num[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_num=list()
  data_num[[1]]=num[[1]]
  
  for (u in 2:(length(myjson2()$number))) {
    data_num[[u]]=rbind(num[[u]],data_num[[(u-1)]])
  }
  
  datan=data_num[[length(myjson2()$number)]]
  datan
})


data_N_uci <- reactive ({
  if(is.null(myjson2())) return()
  
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {v[i]=myjson2()$cov$atlist[i]}
  
  ## Different variable of probabilities for each covariate pattern
  num_uci=list()
  
  if (length(myjson2()$number_uci)==0) {return()}
  
  for(i in 1:length(myjson2()$number_uci)) {
    num_uci[[i]]=as.data.frame(t(data.frame(myjson2()$number_uci[i])))
    colnames(num_uci[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$number_uci)) {  
    num_uci[[i]]=as.data.frame(cbind(num_uci[[i]], timevar ,state=rep(i,nrow(num_uci[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_N_uci=list()
  data_N_uci[[1]]=num_uci[[1]]
  
  for (u in 2:(length(myjson2()$number_uci))) {
    data_N_uci[[u]]=rbind(num_uci[[u]],data_N_uci[[(u-1)]])
  }
  datan_uci=data_N_uci[[length(myjson2()$number_uci)]]
  datan_uci 
})

data_N_lci <- reactive ({
  if(is.null(myjson2())) return()
  
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {v[i]=myjson2()$cov$atlist[i]}
  
  ## Different variable of probabilities for each covariate pattern
  num_lci=list()
  
  if (length(myjson2()$number_lci)==0) {return()}
  
  for(i in 1:length(myjson2()$number_lci)) {
    num_lci[[i]]=as.data.frame(t(data.frame(myjson2()$number_lci[i])))
    colnames(num_lci[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$number_lci)) {  
    num_lci[[i]]=as.data.frame(cbind(num_lci[[i]], timevar ,state=rep(i,nrow(num_lci[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_N_lci=list()
  data_N_lci[[1]]=num_lci[[1]]
  
  for (u in 2:(length(myjson2()$number_lci))) {
    data_N_lci[[u]]=rbind(num_lci[[u]],data_N_lci[[(u-1)]])
  }
  datan_lci=data_N_lci[[length(myjson2()$number_lci)]]
  datan_lci 
})


data_N_st<-reactive  ({
  
  datanew=data_N()
  datanew$state_fac=c(rep("NA",nrow(datanew)))
  
  for (o in 1:(length(myjson2()$number))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$state[g]==o) {datanew$state_fac[g]=labels_state()[o] }  
    }
  }
  datanew
})

data_N_st_uci<-reactive  ({
  datanew=data_N_uci()
  datanew$state_fac=c(rep("NA",nrow(datanew)))
  
  for (o in 1:(length(myjson2()$number_uci))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$state[g]==o) {datanew$state_fac[g]=labels_state()[o] }  
    }
  }
  datanew
})

data_N_st_lci<-reactive  ({
  datanew=data_N_lci()
  datanew$state_fac=c(rep("NA",nrow(datanew)))
  
  for (o in 1:(length(myjson2()$number_lci))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$state[g]==o) {datanew$state_fac[g]=labels_state()[o]  }  
    }
  }
  datanew
})

data_N_d <- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_N_st()[,d],data_N_st()[,ncol(data_N_st())-2],data_N_st()[,ncol(data_N_st())-1],data_N_st()[,ncol(data_N_st())],rep(d,length(data_N_st()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_N_st())[d],length(data_N_st()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_n <- bind_rows(dlist, .id = "column_label")
  d_all_n
  
})

data_N_d_uci <- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_N_st_uci()[,d],data_N_st_uci()[,ncol(data_N_st_uci())-2],
                                data_N_st_uci()[,ncol(data_N_st_uci())-1],
                                data_N_st_uci()[,ncol(data_N_st_uci())],rep(d,length(data_N_st_uci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_N_st_uci())[d],length(data_N_st_uci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  d_all_n_uci <- bind_rows(dlist, .id = "column_label")
  d_all_n_uci
}) 

data_N_d_lci <- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_N_st_lci()[,d],data_N_st_lci()[,ncol(data_N_st_lci())-2],
                                data_N_st_lci()[,ncol(data_N_st_lci())-1],
                                data_N_st_lci()[,ncol(data_N_st_lci())],rep(d,length(data_N_st_lci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_N_st_lci())[d],length(data_N_st_lci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  d_all_n_lci <- bind_rows(dlist, .id = "column_label")
  d_all_n_lci
}) 

output$tickinputnumber <- renderUI({
  
  if (existnumber()==0) {return()}
  
  else if (existnumber()==1) {
  
  default_choices=c("black","blue1","brown1","chartreuse2","cyan1","darkgray","firebrick3",
                    "gold","darkorange2","lightsteelblue4","rosybrow2","violetred2",
                    "yellow2","yellowgreen","tan1","lightslateblue","khaki4")
  
  if (is.null(myjson2()))  return()
  item_list <- list()
  item_list[[1]] <- h4("Provide x axis range and ticks")
  item_list[[2]] <-numericInput("startnx","Start x at:",value=min(data_N_d()$timevar),min=0,max=max(data_N_d()$timevar) )
  item_list[[3]] <-numericInput("stepnx","step:",value=max(data_N_d()$timevar/10),min=0,max=max(data_N_d()$timevar))
  item_list[[4]] <-numericInput("endnx","End x at:",value =max(data_N_d()$timevar),min=0,max=max(data_N_d()$timevar))
  item_list[[5]] <-numericInput("stepny","step at y axis:",value=0.2,min=0.001,max=1)
  item_list[[6]] <-numericInput("endny","End y at:",value =1,min=0,max=1)
  item_list[[7]] <-numericInput("textsizenum",h2("Legends size"),value=input$textsize,min=5,max=30)
  item_list[[8]] <-selectInput("textcolournum",h2("Legends colour"), choices= default_choices, selected =input$textcolour )
  
  do.call(tagList, item_list)
  }
})



data_N_ci<- reactive ({
  x=c( data_N_d()[order(data_N_d()$timevar,data_N_d()$state,data_N_d()$cov),]$timevar,
       data_N_d_lci()[order(-data_N_d()$timevar,data_N_d()$state,data_N_d()$cov),]$timevar )
  
  y_central=c( data_N_d()[order(data_N_d()$timevar,data_N_d()$state,data_N_d()$cov),]$V,
               data_N_d()[order(-data_N_d()$timevar,data_N_d()$state,data_N_d()$cov),]$V )
  
  y=c( data_N_d_uci()[order(data_N_d_uci()$timevar,data_N_d_uci()$state,data_N_d_uci()$cov),]$V,
       data_N_d_lci()[order(-data_N_d_uci()$timevar,data_N_d_uci()$state,data_N_d_uci()$cov),]$V )
  
  frameto=c(as.character(data_N_d_uci()[order(-data_N_d_uci()$timevar,data_N_d_uci()$state,data_N_d_uci()$cov),]$state_factor),
            as.character(data_N_d_lci()[order(-data_N_d_lci()$timevar,data_N_d_lci()$state,data_N_d_lci()$cov),]$state_factor) )
  
  covto=c( data_N_d_uci()[order(-data_N_d_uci()$timevar,data_N_d_uci()$state,data_N_d_uci()$cov),]$cov_factor,
           data_N_d_lci()[order(-data_N_d_lci()$timevar,data_N_d_lci()$state,data_N_d_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,frameto,covto,y_central)
  data
})



#output$shouldloadextra1 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotextra1", label = h2("Download the plot"))
#})

dataextra1_re <-  reactive ({
  
  
  ####### Plot 1 frame is state, factor is cov ########################
  
  
  if (input$confnum=="ci_no") {
    
    if (input$facetnum=="No") {
      
      num_state= plot_ly(data_N_d(),alpha=0.5) %>%
        add_lines(
          x=data_N_d()$timevar,y=data_N_d()$V,
          frame=as.factor(data_N_d()$state_factor),
          color=as.factor(data_N_d()$cov_factor),
          colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
          mode="lines",
          line=list(simplify=FALSE,color = labels_colour_cov())  ,
          text = 'Select or deselect lines by clicking on the legend',
          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")  )
      
      
      num_state = num_state %>%
        layout(title=list(text="Expected number of visits for each covariate pattern among states",y=0.95),
               font= list(family = "times new roman", size = input$textsizenum, color = input$textcolournum),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepnx, 
                          tick0 = input$startnx, 
                          range=c(input$startnx,input$endnx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Expected number of visits",rangemode = "nonnegative",                    
                           dtick = input$stepny, 
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
      
      num_state
      
    }
    
    
    if (input$facetnum=="Yes") {
      
      
      data_plot=data_N_d()
      
      num_state = ggplot(data_plot)
      num_state = ggplot(data_plot,aes(x=timevar, y=V, color= as.factor(cov_factor)))
      
      num_state = num_state+geom_line(aes(x=timevar, y=V, color= as.factor(cov_factor)))+
        scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      num_state = num_state+ facet_wrap(~state_factor)
      
      num_state = num_state + scale_x_continuous(breaks=c(seq(input$startnx,input$endnx,by=input$stepnx  ))) +
        scale_y_continuous(breaks=c(seq(0,input$endny,by=input$stepny  )))
      
      num_state = num_state +labs(title="Expected number of visits for each covariate pattern among states", x="Time since entry", y="Expected number of visits")
      
      num_state = num_state + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      num_state = num_state  +theme(title = element_text(size = input$textsizenum-4),       
                                    legend.title = element_text(color=input$textcolourvis, size= input$textsizenum-5), 
                                    legend.text=element_text(size= input$textsizenum-6),
                                    plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                    legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                    legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                    axis.title.y = element_text(size= input$textsizenum-5),
                                    axis.title.x = element_text(size= input$textsizenum-5), 
                                    axis.text.x = element_text( size=input$textsizenum-6),axis.text.y = element_text( size=input$textsizenum-6))   
      
      num_state = ggplotly(num_state, tooltip = "text")%>%
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
      
      num_state 
    }
    
  }
  
  else if (input$confnum=="ci_yes") {
    
    if (input$facetnum=="No") {
      
      num_state <- plot_ly()
      
      num_state <- add_trace(num_state, line=list(simplify=FALSE,color = labels_colour_cov()),
                             mode="lines", type = "scatter",
                             x=data_N_ci()$x, y=data_N_ci()$y_central,
                             frame=as.factor(data_N_ci()$frameto), 
                             colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
                             color=as.factor(data_N_ci()$covto) ,
                             text = 'Select or deselect lines by clicking on the legend',
                             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      num_state <- add_trace(num_state, fill = "tozerox", 
                             line=list(dash = "solid", color = "transparent", width = 1.8897637),
                             mode = "lines", type = "scatter",
                             x=data_N_ci()$x, y=data_N_ci()$y,
                             frame=as.factor(data_N_ci()$frameto),
                             colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
                             color=as.factor(data_N_ci()$covto),
                             showlegend = FALSE,
                             text = 'Select or deselect lines by clicking on the legend',
                             hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                   "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      
      num_state = num_state %>%
        layout(title=list(text="Expected number of visits for each covariate pattern among states",y=0.95),
               font= list(family = "times new roman", size = input$textsizenum, color = input$textcolournum),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepnx, 
                          tick0 = input$startnx, 
                          range=c(input$startnx,input$endnx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Expected number of visits",rangemode = "nonnegative",                    
                           dtick = input$stepny, 
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
      
      num_state
    }
    
    if (input$facetnum=="Yes") {
      
      N_lci=  data_N_d_lci()$V
      N_uci=  data_N_d_uci()$V
      
      data_plot=cbind(data_N_d(),N_lci,N_uci)
      
      
      num_state=ggplot(data_plot)
      num_state=ggplot(data_plot,aes(x=timevar, y=V, color= as.factor(cov_factor), group=1,
                                     text=paste("Select or deselect lines by clicking on the legend",
                                                "<br>Time: ", timevar,
                                                "<br>Expected number of visits: ", V,
                                                "<br>Covariate pattern: ",  as.factor(cov_factor))))+
        scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      
      num_state=num_state+geom_line(aes(x=timevar, y=V, fill= as.factor(cov_factor)))
      
      num_state=num_state+ geom_ribbon(aes(ymin = N_lci, ymax =N_uci,fill=as.factor(cov_factor)),alpha=0.4)+ 
        scale_fill_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      num_state = num_state+ facet_wrap(~state_factor)
      
      num_state = num_state + scale_x_continuous(breaks=c(seq(input$startnx,input$endnx,by=input$stepnx  ))) +
        scale_y_continuous(breaks=c(seq(0,input$endny,by=input$stepny  )))
      
      num_state = num_state +labs(title="Expected number of visits at each state", x="Time since entry", y="Expected number of visits")
      
      num_state = num_state + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      num_state = num_state +theme(title = element_text(size = input$textsizenum-4),       
                                   legend.title = element_text(color=input$textcolourvis, size= input$textsizenum-5), 
                                   legend.text=element_text(size= input$textsizenum-6),
                                   plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                   legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                   legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                   axis.title.y = element_text(size= input$textsizenum-5),
                                   axis.title.x = element_text(size= input$textsizenum-5), 
                                   axis.text.x = element_text( size=input$textsizenum-6),axis.text.y = element_text( size=input$textsizenum-6))
      
      num_state = ggplotly(num_state, tooltip = "text")%>%
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
      
      num_state 
      
    }
  }
  num_state  
})
output$number_state <- renderPlotly ({ dataextra1_re()})

output$downplotextra1 <- downloadHandler(
  filename = function(){paste("extra1",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( dataextra1_re(),width = 1200, height = 900, format = "png", scale = 2,  out_file = file )
  }
)
  
####################################################

#output$shouldloadextra2 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotextra2", label = h2("Download the plot"))
#})

dataextra2_re <-  reactive ({

  
  if (input$confnum=="ci_no") {
    
    if (input$facetnum=="No") {
      
      
      num_cov= plot_ly(data_N_d(),alpha=0.5) %>%
        add_lines(
          x=data_N_d()$timevar,y=data_N_d()$V,
          frame=as.factor(data_N_d()$cov_factor),
          color=as.factor(data_N_d()$state_factor),
          colors=labels_colour_state()[1:length(myjson2()$P)],
          mode="lines",
          line=list(simplify=FALSE,color = labels_colour_state()) ,
          text = 'Select or deselect lines by clicking on the legend',
          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")   )
      
      
      num_cov = num_cov %>%
        layout(title=list(text="Expected number of visits for each state among covariate patterns",y=0.95),
               font= list(family = "times new roman", size = input$textsizenum, color = input$textcolournum),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepnx, 
                          tick0 = input$startnx, 
                          range=c(input$startnx,input$endnx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Expected number of visits",rangemode = "nonnegative",                    
                           dtick = input$stepny, 
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
        num_cov= num_cov %>%
          animation_opts(frame = 1000, transition = 0, redraw = FALSE)
      }
      
      num_cov
      
    }
    
    
    if (input$facetnum=="Yes") {
      
      
      data_plot=data_N_d()
      
      num_cov = ggplot(data_plot)
      num_cov = ggplot(data_plot,aes(x=timevar, y=V, color= as.factor(state_factor), group=1,
                                     text=paste("Select or deselect lines by clicking on the legend",
                                                "<br>Time: ", timevar,
                                                "<br>Expected number of visits: ", V,
                                                "<br>State: ",  as.factor(state_factor))))
      
      num_cov = num_cov+geom_line(aes(x=timevar, y=V, color= as.factor(state_factor)))+
        scale_colour_manual( values =labels_colour_state(),labels = labels_state()  ) 
      
      num_cov = num_cov+ facet_wrap(~cov_factor)
      
      num_cov = num_cov + scale_x_continuous(breaks=c(seq(input$startnx,input$endnx,by=input$endnx  )))  +
        scale_y_continuous(breaks=c(seq(0,input$endny,by=input$stepny  )))
      
      num_cov = num_cov +labs(title="Expected number of visits for each state among covariate patterns", x="Time since entry", y="Expected number of visits")
      
      num_cov = num_cov + labs(color = "States")+ labs(fill = "States")
      
      num_cov = num_cov+theme(title = element_text(size = input$textsizenum-4),       
                              legend.title = element_text(color=input$textcolourvis, size= input$textsizenum-5), 
                              legend.text=element_text(size= input$textsizenum-6),
                              plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                              legend.margin = margin(1.5, 1, 1, 1, "cm"),
                              legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                              axis.title.y = element_text(size= input$textsizenum-5),
                              axis.title.x = element_text(size= input$textsizenum-5), 
                              axis.text.x = element_text( size=input$textsizenum-6),axis.text.y = element_text( size=input$textsizenum-6)) 
      
      num_cov = ggplotly(num_cov, tooltip = "text")%>%
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
      
      num_cov 
    }
    
  }
  
  else if (input$confnum=="ci_yes") {
    
    if (input$facetnum=="No") {
      
      num_cov <- plot_ly()
      
      num_cov <- add_trace(num_cov, line=list(simplify=FALSE,color = labels_colour_cov()),
                           mode="lines", type = "scatter",
                           x=data_N_ci()$x, y=data_N_ci()$y_central,
                           frame=as.factor(data_N_ci()$covto), 
                           colors=labels_colour_state()[1:length(myjson2()$P)],
                           color=as.factor(data_N_ci()$frameto),
                           text = 'Select or deselect lines by clicking on the legend',
                           hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                 "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") )
      
      num_cov <- add_trace(num_cov, fill = "tozerox", 
                           line=list(dash = "solid", color = "transparent", width = 1.8897637),
                           mode = "lines", type = "scatter",
                           x=data_N_ci()$x, y=data_N_ci()$y,
                           frame=as.factor(data_N_ci()$covto), 
                           colors=labels_colour_state()[1:length(myjson2()$P)],
                           color=as.factor(data_N_ci()$frameto) ,
                           showlegend = FALSE,
                           text = 'Select or deselect lines by clicking on the legend',
                           hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                 "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      
      num_cov = num_cov %>%
        layout(title=list(text="Expected number of visits for each state among covariate patterns",y=0.95),
               font= list(family = "times new roman", size = input$textsizenum, color = input$textcolournum),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepnx, 
                          tick0 = input$startnx, 
                          range=c(input$startnx,input$endnx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Expected number of visits",rangemode = "nonnegative",                    
                           dtick = input$stepny, 
                           ticklen = 5,
                           tickwidth = 2,
                           tickcolor = toRGB("black")),
               shapes = list(
                 list(type = "rect",
                      fillcolor = "grey", 
                      line = list(color = "grey"), 
                      opacity = 0.8,
                      x0 = 0, x1 =input$area, xref = "x", y0 = 0, y1 = 1, yref = "y") ))%>%
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
        num_cov= num_cov %>%
          animation_opts(frame = 1000, transition = 0, redraw = FALSE)
      }
      
      num_cov
    }
    
    if (input$facetnum=="Yes") {
      
      N_lci=  data_N_d_lci()$V
      N_uci=  data_N_d_uci()$V
      
      data_plot=cbind(data_N_d(),N_lci,N_uci)
      
      
      num_cov=ggplot(data_plot)
      num_cov=ggplot(data_plot,aes(x=timevar, y=V, color= as.factor(state_factor), group=1,
                                   text=paste("Select or deselect lines by clicking on the legend",
                                              "<br>Time: ", timevar,
                                              "<br>Expected number of visits: ", V,
                                              "<br>State: ",  as.factor(state_factor))))+
        scale_colour_manual( values =labels_colour_state(),labels = labels_state()  ) 
      
      
      num_cov=num_cov+geom_line(aes(x=timevar, y=V, fill= as.factor(state_factor)))
      
      num_cov=num_cov+ geom_ribbon(aes(ymin = N_lci, ymax =N_uci,fill=as.factor(state_factor)),alpha=0.4)+ 
        scale_fill_manual( values =labels_colour_state(),labels = labels_state()  ) 
      
      num_cov = num_cov+ facet_wrap(~cov_factor)
      
      num_cov = num_cov + scale_x_continuous(breaks=c(seq(input$startnx,input$endnx,by=input$stepnx  ))) 
      num_cov = num_cov +labs(title="Expected number of visits for each state among covariate patterns", x="Time since entry", y="Expected number of visits")
      
      num_cov = num_cov + labs(color = "States")+ labs(fill = "States")
      
      num_cov = num_cov +theme(title = element_text(size = input$textsizenum-4),       
                               legend.title = element_text(color=input$textcolourvis, size= input$textsizenum-5), 
                               legend.text=element_text(size= input$textsizenum-6),
                               plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                               legend.margin = margin(1.5, 1, 1, 1, "cm"),
                               legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                               axis.title.y = element_text(size= input$textsizenum-5),
                               axis.title.x = element_text(size= input$textsizenum-5), 
                               axis.text.x = element_text( size=input$textsizenum-6),axis.text.y = element_text( size=input$textsizenum-6))
      
      num_cov = ggplotly(num_cov, tooltip = "text")%>%
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
      
      num_cov 
      
    }
  }
  num_cov 
}) 

output$number_cov <- renderPlotly ({ dataextra2_re() })

output$downplotextra2 <- downloadHandler(
  filename = function(){paste("extra2",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( dataextra2_re(),width = 1200, height = 900, format = "png", scale =2,  out_file = file )
  }
)


###########################################

df_next <- reactive({
  
 if (existnext()==0) { 
  next_matrix=matrix(nrow=1, ncol=1, "Not applicable") }
 
 else  if (existnext()==1) {
    next_matrix=matrix(nrow=length(myjson2()$cov$atlist), ncol=length(myjson2()$nextv)+1, NA) 
    next_matrix=as.data.frame(next_matrix)
    names(next_matrix)=c("Covariate patterns",labels_state())
    next_matrix[,1]<- labels_cov()
    
    for (i in 1: length(myjson2()$cov$atlist)) {
      
      for (j in 2: (length(myjson2()$nextv)+1)) {
        
        next_matrix[i,j]=myjson2()$nextv[[j-1]][i,1]
        
      }
    }
 }
  
  else if (existnext()==2) {
    
    next_matrix=matrix(nrow=length(myjson2()$cov$atlist), ncol=3*length(myjson2()$nextv)+1, NA)
    
    label_state_next=vector()
    label_state_next[1]="Covariate patterns"
    for (j in 1: length(myjson2()$nextv)) {  
      label_state_next[1+(3*(j-1) )+1] = labels_state()[j]
      label_state_next[1+(3*(j-1) )+2] = paste0(labels_state()[j],"_lci")
      label_state_next[1+(3*(j-1) )+3] =  paste0(labels_state()[j],"_uci")
    }

    for (i in 1: length(myjson2()$cov$atlist)) {
      
      for (j in 1: length(myjson2()$nextv)) {
        next_matrix[,1]<- labels_cov()
        next_matrix[i,1+(3*(j-1) )+1]= myjson2()$nextv[[j]][i,1]
        next_matrix[i,1+(3*(j-1) )+2]=myjson2()$next_lci[[j]][i,1]
        next_matrix[i,1+(3*(j-1) )+3]=myjson2()$next_uci[[j]][i,1]
      }
    }
    next_matrix=as.data.frame(next_matrix)
    names(next_matrix)=label_state_next
  }
  
    next_matrix
})

output$next_state<- DT::renderDataTable({
  
  df_next()
  
})


df_soj <- reactive({
  
  if (existsoj()==0) { 
    soj_matrix=matrix(nrow=1, ncol=1, "Not applicable") }
  
  else  if (existsoj()==1) {
    soj_matrix=matrix(nrow=length(myjson2()$cov$atlist), ncol=length(myjson2()$soj)+1, NA) 
    soj_matrix=as.data.frame(soj_matrix)
    
    labels_state_soj=vector()
    for (k in 1: length(myjson2()$intermediate_states)  ) { labels_state_soj[k]=paste0("Sojourn: State"," ",k) }
    
    names(soj_matrix)=c("Covariate patterns",labels_state_soj)
    
    soj_matrix[,1]<- labels_cov()
    
    for (i in 1: length(myjson2()$cov$atlist)) {
      
        for (j in 1: length(myjson2()$intermediate_states)  ) {
        
           soj_matrix[i,j+1]=myjson2()$soj[[j]][i,1]
      }
    }
  }
  
  else if (existsoj()==2) {
    
    soj_matrix=matrix(nrow=length(myjson2()$cov$atlist), ncol=3*length(myjson2()$intermediate_states)+1, NA)
    
    label_state_soj=vector()
    label_state_soj[1]="Covariate patterns"
    
    label_state=vector()
    
    for (j in 1: length(myjson2()$intermediate_states) ) { label_state[j]=paste0("Sojourn: State"," ",myjson2()$intermediate_states[j]) }
    
       for (j in 1: length(myjson2()$intermediate_states) ) {  
      
         label_state_soj[1+(3*(j-1) )+1] =  label_state[j]
         label_state_soj[1+(3*(j-1) )+2] =  paste0(label_state[j],"_lci")
         label_state_soj[1+(3*(j-1) )+3] =  paste0(label_state[j],"_uci")
    }
    
    for (i in 1: length(myjson2()$cov$atlist)) {
      
      for (j in 1: length(myjson2()$soj)) {
        soj_matrix[,1]<- labels_cov()
        soj_matrix[i,1+(3*(j-1) )+1]= myjson2()$soj[[j]][i,1]
        soj_matrix[i,1+(3*(j-1) )+2]= myjson2()$soj_lci[[j]][i,1]
        soj_matrix[i,1+(3*(j-1) )+3]= myjson2()$soj_uci[[j]][i,1]
      }
    }
    soj_matrix=as.data.frame(soj_matrix)
    names(soj_matrix)=label_state_soj
  }
  
  soj_matrix
  
})

output$soj_state<- DT::renderDataTable({
  
  df_soj()
  
})

df_first <- reactive({
  
  if (existfirst()==0) { 
    first_matrix=matrix(nrow=1, ncol=1, "Not applicable") }
  
  else  if (existfirst()==1) {
    first_matrix=matrix(nrow=length(myjson2()$cov$atlist), ncol=length(myjson2()$first)+1, NA) 
    first_matrix=as.data.frame(first_matrix)
    
    labels_state_first=vector()
    for (k in 1: length(myjson2()$intermediate_states)  ) { labels_state_first[k]=paste0("First: State"," ",k) }
    
    names(first_matrix)=c("Covariate patterns",labels_state_first)
    
    first_matrix[,1]<- labels_cov()
    
    for (i in 1: length(myjson2()$cov$atlist)) {
      
      for (j in 1: length(myjson2()$intermediate_states)  ) {
        
        first_matrix[i,j+1]=myjson2()$first[[j]][i,1]
      }
    }
  }
  
  else if (existfirst()==2) {
    
    first_matrix=matrix(nrow=length(myjson2()$cov$atlist), ncol=3*length(myjson2()$intermediate_states)+1, NA)
    
    label_state_first=vector()
    label_state_first[1]="Covariate patterns"
    
    label_state=vector()
    
    for (j in 1: length(myjson2()$intermediate_states) ) { label_state[j]=paste0("First: State"," ",myjson2()$intermediate_states[j]) }
    
    for (j in 1: length(myjson2()$intermediate_states) ) {  
      
      label_state_first[1+(3*(j-1) )+1] =  label_state[j]
      label_state_first[1+(3*(j-1) )+2] =  paste0(label_state[j],"_lci")
      label_state_first[1+(3*(j-1) )+3] =  paste0(label_state[j],"_uci")
    }
    
    for (i in 1: length(myjson2()$cov$atlist)) {
      
      for (j in 1: length(myjson2()$first)) {
        first_matrix[,1]<- labels_cov()
        first_matrix[i,1+(3*(j-1) )+1]= myjson2()$first[[j]][i,1]
        first_matrix[i,1+(3*(j-1) )+2]= myjson2()$first_lci[[j]][i,1]
        first_matrix[i,1+(3*(j-1) )+3]= myjson2()$first_uci[[j]][i,1]
      }
    }
    first_matrix=as.data.frame(first_matrix)
    names(first_matrix)=label_state_first
  }
  
  first_matrix
  
})

output$first_state<- DT::renderDataTable({
  
  df_first()
  
})