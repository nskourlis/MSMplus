###### Show and hide  and tick inputs ####

timeruser <- reactiveVal(1.5)


observeEvent(c(input$showtickuser,invalidateLater(1000, session)), { 
  
  if(input$showtickuser=="No"){
    
    hide("tickinputuser")
    
  }
  
  if(input$showtickuser=="Yes"){
    
    show("tickinputuser")
    
  }
  
  isolate({
    
    timeruser(timeruser()-1)
    if(timeruser()>1 & input$showtickuser=="No")
    {
      show("tickinputuser")
    }
    
  })
})


################################################



##########################################
#Create the reactive input of covariates#
##########################################

existuser <- reactive({
  if (length(myjson2()$user)!= 0) {       
    x= 1
  }
  else if (length(myjson2()$user) == 0) {       
    x= 0
  }
})

existuserratio <- reactive({
  if (length(myjson2()$userr) != 0) {       
    x= 1
  }
  else if (length(myjson2()$userr) == 0) {       
    x= 0
  }
})

existuserdiff <- reactive({
  if (length(myjson2()$userd) != 0) {       
    x= 1
  }
  else if (length(myjson2()$userd) == 0) {       
    x= 0
  }
})

output$loginpageuser <- renderUI({h1("Non applicable")})

output$pageuser <- renderUI({
  
  if (is.null(myjson2())) return("Provide the json file with the predictions")
  
  if (existuser()==0) {
    
    fluidRow(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      column(12,
             uiOutput("loginpageuser"),

      )
    )
  }
  
  else if (existuser()==1) {
    
    if (existuserdiff()==1 & existuserratio()==1) { 
      
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,

               uiOutput("userinput"),
               uiOutput("confuser")
               
               #uiOutput("displayu"),
               #uiOutput("covarinputuser")
        ),
        column(2,
               br(),
               p(""),
               radioButtons("showtickuser", "Show axis tick options",
                            choices = list("No" = "No",
                                           "Yes" = "Yes"), selected = "No"),
               uiOutput("tickinputuser")

        ),
        
        
        column(8,
               tabsetPanel(
                 tabPanel(h2("User function by covariate pattern"),      plotlyOutput("user", height="700px", width = "100%"),uiOutput("shouldloaduser1")),
                 tabPanel(h2("Differences"),          plotlyOutput("U_diff", height="600px", width = "100%"),uiOutput("shouldloaduser2")),
                 tabPanel(h2("Ratios"),         plotlyOutput("U_ratio", height="600px", width = "100%"),uiOutput("shouldloaduser3"))
               )
               
        )
        
      )
      
    }
    
    else if (existuserdiff()==1 & existuserratio()==0) { 
      
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,

               uiOutput("userinput"),
               uiOutput("confuser")
               #uiOutput("displayu"),
               #uiOutput("covarinputuser")
        ),
        column(2,
               br(),
               p(""),
               radioButtons("showtickuser", "Show axis tick options",
                            choices = list("No" = "No",
                                           "Yes" = "Yes"), selected ="No"),
               uiOutput("tickinputuser")
        ),
        
        
        column(8,
               tabsetPanel(
                 tabPanel(h2("By covariate pattern"), plotlyOutput("user", height="700px", width = "100%"),uiOutput("shouldloaduser1")),
                 tabPanel(h2("Differences"),          plotlyOutput("U_diff", height="600px", width = "100%"),uiOutput("shouldloaduser2")),
                 tabPanel(h2("Ratios"),               print("Not applicable"))
               )
               
        )
        
      )
      
    }
    
    else  if (existuserdiff()==0 & existuserratio()==1) { 
      
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,

               uiOutput("userinput"),
               uiOutput("confuser")
               #uiOutput("displayu"),
               #uiOutput("covarinputuser")
        ),
        column(2,
               br(),
               p(""),
               radioButtons("showtickuser", "Show axis tick options",
                            choices = list("No" = "No",
                                           "Yes" = "Yes"), selected = "No"),
               uiOutput("tickinputuser")
        ),
        
        
        column(8,
               tabsetPanel(
                 tabPanel(h2("By covariate pattern"), plotlyOutput("user", height="700px", width = "100%"),uiOutput("shouldloaduser1")),
                 tabPanel(h2("Differences"),           print("Not applicable")),
                 tabPanel(h2("Ratios"),               plotlyOutput("U_ratio", height="600px", width = "100%"),uiOutput("shouldloaduser3"))
               )
               
        )
        
      )
      
    }
    
    else if (existuser()==1 & existuserdiff()==0 & existuserratio()==0) { 
      
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,

               uiOutput("userinput"),
               uiOutput("confuser")
               #uiOutput("displayu"),
               #uiOutput("covarinputuser")
        ),
        column(2,
               br(),
               p(""),
               radioButtons("showtickuser", "Show axis tick options",
                            choices = list("No" = "No",
                                           "Yes" = "Yes"), selected = "No"),
               uiOutput("tickinputuser")
        ),
        
        
        column(8,
               tabsetPanel(
                 tabPanel(h2("By covariate pattern"), plotlyOutput("user", height="700px", width = "100%")),
                 tabPanel(h2("Differences"),          print("Not applicable")),
                 tabPanel(h2("Ratios"),               print("Not applicable"))
               )
               
        )
        
      )
      
    }
  }
  
})    


observeEvent(input$json2, {
  if( length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'User')))==0  ) {
    js$disableTab("mytab_user")
    
  } 
}) 

observeEvent(input$csv2, {
  if( length(which(startsWith(names( read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'User')))==0 ) {
    js$disableTab("mytab_user")
    
  } 
}) 

#output$displayu <- renderUI({
#  radioButtons("displayuser", "Change labels of states and covariate patterns",
#               c("same" = "same", "change"= "change"))
#})

output$confuser <- renderUI({
  
  if  (length(myjson2()$ci_user)!=0) {
    radioButtons("confu", "Confidence intervals",
                 c("No" = "ci_no",
                   "Yes" ="ci_yes"))
  }
  
  else if (length(myjson2()$ci_user)==0) {
    item_list <- list()
    item_list[[1]]<- radioButtons("confu", "Confidence intervals",c("No" = "ci_no"))
    item_list[[2]]<-print("Confidence interval data were not provided")
    do.call(tagList, item_list)
  }
  
})
####################################################################################################
#####################################################################################################
####################################################################################################

#output$covarinputuser <- renderUI({
#  
#  if (is.null(myjson2()))  return()
#  
#  if (input$displayuser=="same") return()
#  
#  else {
#    
#    item_list <- list()
#    item_list[[1]] <- h2("Covariate patterns")
#    
#    default_choices_cov=vector()
#    for (i in 1:length(myjson2()$cov$atlist)) {
#      default_choices_cov[i]=myjson2()$cov$atlist[i]
#    }
#    
#    for (i in seq(length(myjson2()$cov$atlist))) {
#      item_list[[i+1]] <- textInput(paste0('covuser', i),default_choices_cov[i], labels_cov()[i])
#    }
#    do.call(tagList, item_list)
#  }
#})
#
#labels_cov<- reactive ({
#  
#  if (input$displayuser=="same") {labels_cov()}
#  
#  else {
#    
#    myList<-vector("list",length(myjson2()$cov$atlist))
#    
#    for (i in 1:length(myjson2()$cov$atlist)) {
#      myList[[i]]= input[[paste0('covuser', i)]][1]
#    }
#    final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
#    final_list
#  }
#})

####################################################################################
####################################################################################
####################################################################################

data_U <- reactive ({
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
  user=list()
  
  if (length(myjson2()$user)==0) {return()}
  
  for(i in 1:length(myjson2()$user)) {
    user[[i]]=    as.data.frame(t(data.frame(myjson2()$user[i])))
    colnames(user[[i]]) <- labels_cov()
  }
  
  for(i in 1:length(myjson2()$user)) {  
    user[[i]]=as.data.frame(cbind(user[[i]], timevar ))
  }
  
  # Append the probabilities datasets of the different states
  data_U=list()
  data_U[[1]]=user[[1]]
  
  if (length(myjson2()$user)>1) {
    for (u in 2:(length(myjson2()$user))) {
      data_U[[u]]=rbind(user[[u]],data_U[[(u-1)]])
    }
  }
  datau=data_U[[length(myjson2()$user)]]
  datau
})


data_U_uci <- reactive ({
  
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {v[i]=myjson2()$cov$atlist[i]}
  
  ## Different variable of probabilities for each covariate pattern
  user_uci=list()
  
  if (length(myjson2()$user_uci)==0) {return()}
  
  for(i in 1:length(myjson2()$user_uci)) {
    user_uci[[i]]=as.data.frame(t(data.frame(myjson2()$user_uci[i])))
    colnames(user_uci[[i]]) <- labels_cov()
  }
  for(i in 1:length(myjson2()$user_uci)) {  
    user_uci[[i]]=as.data.frame(cbind(user_uci[[i]], timevar ))
  }
  # Append the userabilities datasets of the different states
  data_U_uci=list()
  data_U_uci[[1]]=user_uci[[1]]
  
  if (length(myjson2()$user_uci)>1) {
    
    for (u in 2:(length(myjson2()$user_uci))) {
      data_U_uci[[u]]=rbind(user_uci[[u]],data_U_uci[[(u-1)]])
    }
  }
  datau_uci=data_U_uci[[length(myjson2()$user_uci)]]
  datau_uci 
})

data_U_lci <- reactive ({
  if(is.null(myjson2())) return()
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {v[i]=myjson2()$cov$atlist[i]}
  
  ## Different variable of probabilities for each covariate pattern
  
  user_lci=list()
  
  if (length(myjson2()$user_lci)==0) {return()}
  
  for(i in 1:length(myjson2()$user_lci)) {
    user_lci[[i]]=as.data.frame(t(data.frame(myjson2()$user_lci[i])))
    colnames(user_lci[[i]]) <- labels_cov()
  }
  
  for(i in 1:length(myjson2()$user_lci)) {  
    user_lci[[i]]=as.data.frame(cbind(user_lci[[i]], timevar  ))
  }
  
  # Append the userabilities datasets of the different states
  data_U_lci=list()
  data_U_lci[[1]]=user_lci[[1]]
  
  if (length(myjson2()$user_lci)>1) {
    
    for (u in 2:(length(myjson2()$user_lci))) {
      data_U_lci[[u]]=rbind(user_lci[[u]],data_U_lci[[(u-1)]])
    }
  }
  datau_lci=data_U_lci[[length(myjson2()$user_lci)]]
  datau_lci 
})

data_U_d <- reactive ({
  
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_U()[,d],data_U()[,ncol(data_U())],rep(d,length(data_U()[,d])) )
    dlist[[d]][,4] <- rep(colnames(data_U())[d],length(data_U()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","cov","cov_factor")
  }
  d_all_u <- bind_rows(dlist, .id = "column_label")
  d_all_u
})

data_U_d_uci <- reactive ({
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_U_uci()[,d],data_U_uci()[,ncol(data_U_uci())],rep(d,length(data_U_uci()[,d])) )
    dlist[[d]][,4] <- rep(colnames(data_U_uci())[d],length(data_U_uci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","cov","cov_factor")
  }
  
  d_all_u_uci <- bind_rows(dlist, .id = "column_label")
  d_all_u_uci
  
})
data_U_d_lci <- reactive ({
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_U_lci()[,d],data_U_lci()[,ncol(data_U_lci())],rep(d,length(data_U_lci()[,d])) )
    dlist[[d]][,4] <- rep(colnames(data_U_lci())[d],length(data_U_lci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","cov","cov_factor")
  }
  
  d_all_u_lci <- bind_rows(dlist, .id = "column_label")
  d_all_u_lci
  
})


#Create the reactive input of covariates
output$userinput <- renderUI({
  
  if (is.null(myjson2()))  return()
  
  default_choice=vector()
  default_choice[1]="User specified measure"
  
  item_list <- list()
  item_list[[1]] <- h1("Name of user function")
  item_list[[2]] <- textInput('usertitle',default_choice,default_choice)
  do.call(tagList, item_list)
})

output$tickinputuser <- renderUI({
  
  default_choices=c("black","blue1","brown1","chartreuse2","cyan1","darkgray","firebrick3",
                    "gold","darkorange2","lightsteelblue4","rosybrow2","violetred2",
                    "yellow2","yellowgreen","tan1","lightslateblue","khaki4")
  
  
  if (is.null(myjson2()$user))  return()
  item_list <- list()
  item_list[[1]] <- h2("Provide x axis range and ticks")
  item_list[[2]] <-numericInput("startuserx","Start x at:",value=min(data_U_d()$timevar) )
  item_list[[3]] <-numericInput("stepuserx","step:",value=max(data_U_d()$timevar)/10,min=0,max=max(data_U_d()$timevar))
  item_list[[4]] <-numericInput("enduserx","End x at:",value =max(data_U_d()$timevar),max=max(data_U_d()$timevar))
  item_list[[5]] <-numericInput("stepusery","step at y axis:",value=max(data_U_d()$V)/10,min=max(data_U_d()$V)/1000,max=max(data_U_d()$V))
  item_list[[6]] <-numericInput("textsizeuser",h2("Legends size"),value=input$textsize,min=5,max=30)
  item_list[[7]] <-selectInput("textcolouruser",h2("Legends colour"), choices= default_choices, selected =input$textcolour )
  
  do.call(tagList, item_list)
})


data_U_ci<- reactive ({
  x=c( data_U_d()[order(data_U_d()$timevar,data_U_d()$cov),]$timevar,
       data_U_d()[order(-data_U_d()$timevar,data_U_d()$cov),]$timevar )
  
  y_central=c( data_U_d()[order(data_U_d()$timevar,data_U_d()$cov),]$V,
               data_U_d()[order(-data_U_d()$timevar,data_U_d()$cov),]$V )
  
  y=c( data_U_d_uci()[order(data_U_d_uci()$timevar,data_U_d_uci()$cov),]$V,
       data_U_d_lci()[order(-data_U_d_lci()$timevar,data_U_d_lci()$cov),]$V )
  
  
  covto=c( data_U_d_uci()[order(-data_U_d_uci()$timevar,data_U_d_uci()$cov),]$cov_factor,
           data_U_d_uci()[order(-data_U_d_uci()$timevar,data_U_d_uci()$cov),]$cov_factor )
  
  data=data.frame(x,y,covto,y_central)
  data
})


#####################################################################

#output$shouldloaduser1 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotuser1", label = h2("Download the plot"))
#})

datauser1_re <-  reactive ({
  
  
  
  #  if (is.null(myjson2()$user)) {
  #    #   # js$disableTab("tab1")
  #    return(h2("You have not provided user specified information in the json file"))
  #  }
  #  
  #  else if (!is.null(myjson2()$user)) {
  
  
  ####### Plot 1 frame is state, factor is cov ########################
  if (input$confu=="ci_no") {
    
    user_cov= plot_ly(data_U_d(),alpha=0.5) %>%
      add_lines(
        x=data_U_d()$timevar,y=data_U_d()$V,
        color=factor(as.factor(data_U_d()$cov_factor) ,levels=labels_cov()),
        colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
        mode="lines",
        line=list(simplify=FALSE,color = labels_colour_cov()) ,
        text = 'Select or deselect lines by clicking on the legend',
        hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                              "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") 
      )%>%
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
  
  else if (input$confu=="ci_yes") {
    
    user_cov <- plot_ly()
    
    user_cov <- add_trace(user_cov, line=list(simplify=FALSE,color = labels_colour_cov()),
                          mode="lines", type = "scatter",
                          x=data_U_ci()$x, y=data_U_ci()$y_central,
                          colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
                          color=factor(as.factor(data_U_ci()$covto) ,levels=labels_cov()),
                          text = 'Select or deselect lines by clicking on the legend',
                          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") )
    
    user_cov <- add_trace(user_cov, fill = "tozerox", 
                          line=list(dash = "solid", color = "transparent", width = 1.8897637),
                          mode = "lines", type = "scatter",
                          x=data_U_ci()$x, y=data_U_ci()$y,
                          colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
                          color=factor(as.factor(data_U_ci()$covto) ,levels=labels_cov()),
                          showlegend = FALSE,
                          text = 'Select or deselect lines by clicking on the legend',
                          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
  }
  
  user_cov=user_cov    %>%
    layout(title=paste0(input$usertitle," ","across covariate patterns"),
           font= list(family = "times new roman", size = input$textsizeuser, color = input$textcolouruser),
           margin = list(l = 50, r = 50, b = 50, t = 70),
           xaxis=list(title=list(text="Time since entry",y=0.25),
                      dtick = input$stepuserx, 
                      tick0 = input$startuserx, 
                      range=c(input$startuserx,input$enduserx),
                      ticklen = 5,
                      tickwidth = 2,
                      tickcolor = toRGB("black"),
                      tickmode = "linear"),
           yaxis =list(title= input$usertitle,rangemode = "nonnegative",                    
                       dtick = input$stepusery,
                       ticklen = 5,
                       tickwidth = 2,
                       tickcolor = toRGB("black")),
           shapes = list(
             list(type = "rect",
                  fillcolor = "grey", 
                  line = list(color = "grey"), 
                  opacity = 0.8,
                  x0 = 0, x1 =0, xref = "x", y0 = 0, y1 = 1, yref = "y") )
    )%>%
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
  
  user_cov
  # }
  
})

output$user <- renderPlotly ({datauser1_re() })

output$downplotuser1 <- downloadHandler(
  filename = function(){paste("user1",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datauser1_re(),width = 1200, height = 900, format = "png", scale = 2,  out_file = file )
  }
)
  
#####################################################

data_U_diff1 <- reactive ({
  user_diff=list()
  
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  u_diff= vector()
  
  if (length(myjson2()$atlist)>1) {
  for (i in 2:length(myjson2()$cov$atlist)) {
    u_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
  }
  }
  
  if (length(myjson2()$userd)==0) {return()}
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$userd)) {
      user_diff[[i]]=as.data.frame(t(data.frame(myjson2()$userd[i])))
      colnames(user_diff[[i]]) <- u_diff
      user_diff[[i]]=as.data.frame(cbind(user_diff[[i]], timevar  ))
    }
  }
  
  else if (length(myjson2()$atlist)==2) { 
    for (i in 1:length(myjson2()$userd)) {
      user_diff[[i]]=as.data.frame(myjson2()$userd[[i]][,1])
      user_diff[[i]]=as.data.frame(c(user_diff[[i]], timevar ) )
      colnames(user_diff[[i]])[1:2] <- u_diff
    }
  }
  
  # Append the probabilities datasets of the different states
  data_userd=list()
  data_userd[[1]]=user_diff[[1]]
  
  
  dataud=data_userd[[length(myjson2()$userd)]]
  
  dataud
}) 

data_U_diff1_uci <- reactive ({
  user_diff_uci=list()
  
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  u_diff= vector()
  
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$cov$atlist)) {
      u_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$userd_uci)==0) {return()}
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$userd_uci)) {
      user_diff_uci[[i]]=as.data.frame(t(data.frame(myjson2()$userd_uci[i])))
      colnames(user_diff_uci[[i]]) <- u_diff
      user_diff_uci[[i]]=as.data.frame(cbind(user_diff_uci[[i]], timevar  ))
    }
  }
  
  else if (length(myjson2()$atlist)==2) { 
    for (i in 1:length(myjson2()$userd_uci)) {
      user_diff_uci[[i]]=as.data.frame(myjson2()$userd_uci[[i]][,1])
      user_diff_uci[[i]]=as.data.frame(c(user_diff_uci[[i]], timevar ) )
      colnames(user_diff_uci[[i]])[1:2] <- u_diff
    }
  }
  
  # Append the probabilities datasets of the different states
  data_userd_uci=list()
  data_userd_uci[[1]]=user_diff_uci[[1]]
  
  
  dataud_uci=data_userd_uci[[length(myjson2()$userd_uci)]]
  
  dataud_uci
}) 


data_U_diff1_lci <- reactive ({
  user_diff_lci=list()
  
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  u_diff= vector()
  
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$cov$atlist)) {
      u_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$userd_lci)==0) {return()}
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$userd_lci)) {
      user_diff_lci[[i]]=as.data.frame(t(data.frame(myjson2()$userd_lci[i])))
      colnames(user_diff_lci[[i]]) <- u_diff
      user_diff_lci[[i]]=as.data.frame(cbind(user_diff_lci[[i]], timevar  ))
    }
  }
  
  else if (length(myjson2()$atlist)==2) { 
    for (i in 1:length(myjson2()$userd_lci)) {
      user_diff_lci[[i]]=as.data.frame(myjson2()$userd_lci[[i]][,1])
      user_diff_lci[[i]]=as.data.frame(c(user_diff_lci[[i]], timevar ) )
      colnames(user_diff_lci[[i]])[1:2] <- u_diff
    }
  }
  
  # Append the probabilities datasets of the different states
  data_userd_lci=list()
  data_userd_lci[[1]]=user_diff_lci[[1]]
  
  
  dataud_lci=data_userd_lci[[length(myjson2()$userd_lci)]]
  
  dataud_lci
}) 


data_U_diff2<- reactive ({
  
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    data=cbind.data.frame(data_U_diff1()[,d],data_U_diff1()[,ncol(data_U_diff1())],rep(d,length(data_U_diff1()[,d])),
                          rep(colnames(data_U_diff1())[d],length(data_U_diff1()[,d])) )  
    dlist[[d]]=data
    
    colnames(dlist[[d]]) <- c("V","timevar","cov","cov_factor")
  }
  
  d_all_ud <- bind_rows(dlist, .id = "column_label")
  d_all_ud
})


data_U_diff2_uci<- reactive ({
  
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    data=cbind.data.frame(data_U_diff1_uci()[,d],data_U_diff1_uci()[,ncol(data_U_diff1_uci())],rep(d,length(data_U_diff1_uci()[,d])),
                          rep(colnames(data_U_diff1_uci())[d],length(data_U_diff1_uci()[,d])) )  
    dlist[[d]]=data
    
    colnames(dlist[[d]]) <- c("V","timevar","cov","cov_factor")
  }
  
  d_all_ud_uci <- bind_rows(dlist, .id = "column_label")
  d_all_ud_uci
})


data_U_diff2_lci<- reactive ({
  
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    data=cbind.data.frame(data_U_diff1_lci()[,d],data_U_diff1_lci()[,ncol(data_U_diff1_lci())],rep(d,length(data_U_diff1_lci()[,d])),
                          rep(colnames(data_U_diff1_lci())[d],length(data_U_diff1_lci()[,d])) )  
    dlist[[d]]=data
    
    colnames(dlist[[d]]) <- c("V","timevar","cov","cov_factor")
  }
  
  d_all_ud_lci <- bind_rows(dlist, .id = "column_label")
  d_all_ud_lci
})

data_U_diff_ci<- reactive ({
  
  x=c( data_U_diff2()[order(data_U_diff2()$timevar,data_U_diff2()$cov),]$timevar,
       data_U_diff2()[order(-data_U_diff2()$timevar,data_U_diff2()$cov),]$timevar )
  
  y_central=c( data_U_diff2()[order(data_U_diff2()$timevar,data_U_diff2()$cov),]$V,
               data_U_diff2()[order(-data_U_diff2()$timevar,data_U_diff2()$cov),]$V )
  
  y=c( data_U_diff2_uci()[order(data_U_diff2_uci()$timevar,data_U_diff2_uci()$cov),]$V,
       data_U_diff2_lci()[order(-data_U_diff2_lci()$timevar,data_U_diff2_lci()$cov),]$V )
  
  covto=c( data_U_diff2_uci()[order(-data_U_diff2_uci()$timevar,data_U_diff2_uci()$cov),]$cov_factor,
           data_U_diff2_lci()[order(-data_U_diff2_lci()$timevar,data_U_diff2_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,covto,y_central)
  
  data
})

output$fileob2 <- renderPrint({
  data_U_diff1()[50,]
})

##############################################
#output$shouldloaduser2 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotuser2", label = h2("Download the plot"))
#})

datauser2_re <-  reactive ({

  
  ax <- list( title = "",zeroline = FALSE,showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
  
  if (length(myjson2()$userd) == 0| myjson2()$Nats==1 ) {             
    u_user_d= plot_ly() %>%
      layout(title=list(text="Not applicable- Only one covariate pattern specified",y=0.95),xaxis=ax, yaxis=ax)
    u_user_d
  } 
  
  else  {
    
    if (input$confu=="ci_no") {
      
      
      u_user_d= plot_ly(data_U_diff2(),alpha=0.5) %>%
        add_lines(
          x=data_U_diff2()$timevar,y=data_U_diff2()$V,
          color=as.factor(data_U_diff2()$cov_factor),
          colors=labels_colour_cov()[2:length(myjson2()$cov$atlist)],
          mode="lines",
          line=list(simplify=FALSE),
          text = 'Select or deselect lines by clicking on the legend',
          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
        )%>%
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
    
    else if (input$confu=="ci_yes") {
      u_user_d <- plot_ly()
      
      u_user_d <- add_trace(u_user_d, line=list(simplify=FALSE),
                            mode="lines", type = "scatter",
                            x=data_U_diff_ci()$x, y=data_U_diff_ci()$y_central,
                            #  colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                            color=as.factor(data_U_diff_ci()$covto),
                            colors=labels_colour_cov()[2:length(myjson2()$cov$atlist)],
                            text = 'Select or deselect lines by clicking on the legend',
                            hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                  "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
                            )
      
      u_user_d <- add_trace(u_user_d, fill = "tozerox", 
                            line=list(dash = "solid", color = "transparent", width = 1.8897637),
                            mode = "lines", type = "scatter",
                            x=data_U_diff_ci()$x, y=data_U_diff_ci()$y,
                            #  colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                            color=as.factor(data_U_diff_ci()$covto),
                            showlegend = FALSE,
                            text = 'Select or deselect lines by clicking on the legend',
                            hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                  "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
    }
    
    u_user_d=u_user_d %>%
      layout(title=list(text=paste0("Difference in ",input$usertitle," between covariate patterns"),y=0.95),
             font= list(family = "times new roman", size = input$textsizeuser, color = input$textcolouruser),
             margin = list(l = 50, r = 50, b = 30, t = 70),
             xaxis=list(title=list(text="Time since entry",y=0.2),
                        dtick = input$stepuserx, 
                        tick0 = input$startuserx, 
                        range=c(input$startuserx,input$enduserx),
                        ticklen = 5,
                        tickwidth = 2,
                        tickcolor = toRGB("black"),
                        tickmode = "linear"),
             yaxis =list(title= paste0("Difference in ",input$usertitle),                    
                         dtick = input$stepusery, 
                         ticklen = 5,
                         tickwidth = 2,
                         tickcolor = toRGB("black")),
             shapes = list(
               list(type = "rect",
                    fillcolor = "grey", 
                    line = list(color = "grey"), 
                    opacity = 0.8,
                    x0 = 0, x1 =0, xref = "x", y0 = 0, y1 = 1, yref = "y") )
      )%>%
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
    
    u_user_d
  }

}) 

output$U_diff <- renderPlotly ({ datauser2_re() })
  
output$downplotuser2 <- downloadHandler(
  filename = function(){paste("user2",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datauser2_re(),width = 1200, height = 900, format = "png", scale = 2,  out_file = file )
  }
)

################################################################################################
######                   RAtio                 ################################################
#####################################################################################


data_U_ratio1 <- reactive ({
  
  user_ratio=list()
  
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  u_ratio= vector()
  
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$cov$atlist)) {
      u_ratio[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$userr)==0) {return()}
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$userr)) {
      user_ratio[[i]]=as.data.frame(t(data.frame(myjson2()$userr[i])))
      colnames(user_ratio[[i]]) <- u_ratio
      user_ratio[[i]]=as.data.frame(cbind(user_ratio[[i]], timevar  ))
    }
  }
  
  else if (length(myjson2()$atlist)==2) { 
    for (i in 1:length(myjson2()$userr)) {
      user_ratio[[i]]=as.data.frame(myjson2()$userr[[i]][,1])
      user_ratio[[i]]=as.data.frame(c(user_ratio[[i]], timevar ) )
      colnames(user_ratio[[i]])[1:2] <- u_ratio
    }
  }
  
  # Append the probabilities datasets of the ratioerent states
  data_userr=list()
  data_userr[[1]]=user_ratio[[1]]
  
  
  dataur=data_userr[[length(myjson2()$userr)]]
  
  dataur
}) 

data_U_ratio1_uci <- reactive ({
  user_ratio_uci=list()
  
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  u_ratio= vector()
  
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$cov$atlist)) {
      u_ratio[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$userr_uci)==0) {return()}
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$userr_uci)) {
      user_ratio_uci[[i]]=as.data.frame(t(data.frame(myjson2()$userr_uci[i])))
      colnames(user_ratio_uci[[i]]) <- u_ratio
      user_ratio_uci[[i]]=as.data.frame(cbind(user_ratio_uci[[i]], timevar  ))
    }
  }
  
  else if (length(myjson2()$atlist)==2) { 
    for (i in 1:length(myjson2()$userr_uci)) {
      user_ratio_uci[[i]]=as.data.frame(myjson2()$userr_uci[[i]][,1])
      user_ratio_uci[[i]]=as.data.frame(c(user_ratio_uci[[i]], timevar ) )
      colnames(user_ratio_uci[[i]])[1:2] <- u_ratio
    }
  }
  
  # Append the probabilities datasets of the ratioerent states
  data_userr_uci=list()
  data_userr_uci[[1]]=user_ratio_uci[[1]]
  
  
  dataur_uci=data_userr_uci[[length(myjson2()$userr_uci)]]
  
  dataur_uci
}) 


data_U_ratio1_lci <- reactive ({
  user_ratio_lci=list()
  
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  u_ratio= vector()
  
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$cov$atlist)) {
      u_ratio[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$userr_lci)==0) {return()}
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$userr_lci)) {
      user_ratio_lci[[i]]=as.data.frame(t(data.frame(myjson2()$userr_lci[i])))
      colnames(user_ratio_lci[[i]]) <- u_ratio
      user_ratio_lci[[i]]=as.data.frame(cbind(user_ratio_lci[[i]], timevar  ))
    }
  }
  
  else if (length(myjson2()$atlist)==2) { 
    for (i in 1:length(myjson2()$userr_lci)) {
      user_ratio_lci[[i]]=as.data.frame(myjson2()$userr_lci[[i]][,1])
      user_ratio_lci[[i]]=as.data.frame(c(user_ratio_lci[[i]], timevar ) )
      colnames(user_ratio_lci[[i]])[1:2] <- u_ratio
    }
  }
  
  # Append the probabilities datasets of the ratioerent states
  data_userr_lci=list()
  data_userr_lci[[1]]=user_ratio_lci[[1]]
  
  
  dataur_lci=data_userr_lci[[length(myjson2()$userr_lci)]]
  
  dataur_lci
}) 


data_U_ratio2<- reactive ({
  
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    data=cbind.data.frame(data_U_ratio1()[,d],data_U_ratio1()[,ncol(data_U_ratio1())],rep(d,length(data_U_ratio1()[,d])),
                          rep(colnames(data_U_ratio1())[d],length(data_U_ratio1()[,d])) )  
    dlist[[d]]=data
    
    colnames(dlist[[d]]) <- c("V","timevar","cov","cov_factor")
  }
  
  d_all_ur <- bind_rows(dlist, .id = "column_label")
  d_all_ur
})


data_U_ratio2_uci<- reactive ({
  
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    data=cbind.data.frame(data_U_ratio1_uci()[,d],data_U_ratio1_uci()[,ncol(data_U_ratio1_uci())],rep(d,length(data_U_ratio1_uci()[,d])),
                          rep(colnames(data_U_ratio1_uci())[d],length(data_U_ratio1_uci()[,d])) )  
    dlist[[d]]=data
    
    colnames(dlist[[d]]) <- c("V","timevar","cov","cov_factor")
  }
  
  d_all_ur_uci <- bind_rows(dlist, .id = "column_label")
  d_all_ur_uci
})


data_U_ratio2_lci<- reactive ({
  
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    data=cbind.data.frame(data_U_ratio1_lci()[,d],data_U_ratio1_lci()[,ncol(data_U_ratio1_lci())],rep(d,length(data_U_ratio1_lci()[,d])),
                          rep(colnames(data_U_ratio1_lci())[d],length(data_U_ratio1_lci()[,d])) )  
    dlist[[d]]=data
    
    colnames(dlist[[d]]) <- c("V","timevar","cov","cov_factor")
  }
  
  d_all_ur_lci <- bind_rows(dlist, .id = "column_label")
  d_all_ur_lci
})

data_U_ratio_ci<- reactive ({
  
  x=c( data_U_ratio2()[order(data_U_ratio2()$timevar,data_U_ratio2()$cov),]$timevar,
       data_U_ratio2()[order(-data_U_ratio2()$timevar,data_U_ratio2()$cov),]$timevar )
  
  y_central=c( data_U_ratio2()[order(data_U_ratio2()$timevar,data_U_ratio2()$cov),]$V,
               data_U_ratio2()[order(-data_U_ratio2()$timevar,data_U_ratio2()$cov),]$V )
  
  y=c( data_U_ratio2_uci()[order(data_U_ratio2_uci()$timevar,data_U_ratio2_uci()$cov),]$V,
       data_U_ratio2_lci()[order(-data_U_ratio2_lci()$timevar,data_U_ratio2_lci()$cov),]$V )
  
  covto=c( data_U_ratio2_uci()[order(-data_U_ratio2_uci()$timevar,data_U_ratio2_uci()$cov),]$cov_factor,
           data_U_ratio2_lci()[order(-data_U_ratio2_lci()$timevar,data_U_ratio2_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,covto,y_central)
  
  data
})

output$fileob2 <- renderPrint({
  data_U_ratio1()[50,]
})

##########################################
#output$shouldloaduser3 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotuser3", label = h2("Download the plot"))
#})

datauser3_re <-  reactive ({

  
  ax <- list( title = "",zeroline = FALSE,showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
  
  if (length(myjson2()$userr) == 0| myjson2()$Nats==1 ) {             
    u_user_d= plot_ly() %>%
      layout(title=list(text="Not applicable- Only one covariate pattern specified",y=0.95),xaxis=ax, yaxis=ax)
    u_user_d
  } 
  
  else  {
  
  if (input$confu=="ci_no") {
    
    
    u_user_d= plot_ly(data_U_ratio2(),alpha=0.5) %>%
      add_lines(
        x=data_U_ratio2()$timevar,y=data_U_ratio2()$V,
        color=as.factor(data_U_ratio2()$cov_factor),
        colors=labels_colour_cov()[2:length(myjson2()$cov$atlist)],
        mode="lines",
        line=list(simplify=FALSE),
        text = 'Select or deselect lines by clicking on the legend',
        hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                              "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
      )%>%
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
  
  else if (input$confu=="ci_yes") {
    u_user_d <- plot_ly()
    
    u_user_d <- add_trace(u_user_d, line=list(simplify=FALSE),
                          mode="lines", type = "scatter",
                          x=data_U_ratio_ci()$x, y=data_U_ratio_ci()$y_central,
                          #  colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                          color=as.factor(data_U_ratio_ci()$covto) ,
                          colors=labels_colour_cov()[2:length(myjson2()$cov$atlist)],
                          text = 'Select or deselect lines by clicking on the legend',
                          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
    
    u_user_d <- add_trace(u_user_d, fill = "tozerox", 
                          line=list(dash = "solid", color = "transparent", width = 1.8897637),
                          mode = "lines", type = "scatter",
                          x=data_U_ratio_ci()$x, y=data_U_ratio_ci()$y,
                          #  colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                          color=as.factor(data_U_ratio_ci()$covto),
                          showlegend = FALSE,
                          text = 'Select or deselect lines by clicking on the legend',
                          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
  }
  
  u_user_d=u_user_d %>%
    layout(title=list(text=paste0("Ratio in ",input$usertitle," between covariate pattern"),y=0.95),
           font= list(family = "times new roman", size = input$textsizeuser, color = input$textcolouruser),
           margin = list(l = 50, r = 50, b = 30, t = 70),
           xaxis=list(title=list(text="Time since entry",y=0.2),
                      dtick = input$stepuserx, 
                      tick0 = input$startuserx, 
                      range=c(input$startuserx,input$enduserx),
                      ticklen = 5,
                      tickwidth = 2,
                      tickcolor = toRGB("black"),
                      tickmode = "linear"),
           yaxis =list(title= paste0("Ratio of ",input$usertitle),                    
                       dtick = input$stepusery, 
                       ticklen = 5,
                       tickwidth = 2,
                       tickcolor = toRGB("black")),
           shapes = list(
             list(type = "rect",
                  fillcolor = "grey", 
                  line = list(color = "grey"), 
                  opacity = 0.8,
                  x0 = 0, x1 =0, xref = "x", y0 = 0, y1 = 1, yref = "y") )
    )%>%
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
  
  u_user_d
  } 
}) 

output$U_ratio <- renderPlotly ({ datauser3_re() })
  
output$downplotuser3 <- downloadHandler(
  filename = function(){paste("user2",'.png',sep='')},
  content = function(file){
    
    plotly_IMAGE( datauser3_re(),width = 1200, height = 900, format = "png", scale = 2,  out_file = file )
  }
)


