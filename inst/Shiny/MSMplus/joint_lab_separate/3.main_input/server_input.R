#### Hide and show colour options ####
timerinput <- reactiveVal(1.5)

observeEvent(c(input$showcolor,invalidateLater(1000, session)), {
  
  if(input$showcolor=="No"){
    
    hide("colourinputcov")
    hide("colourinputstate")
    hide("colourinput_trans")
  }
  
  if(input$showcolor=="Yes"){
    
    show("colourinputcov")
    show("colourinputstate")
    show("colourinput_trans")
  }
  
  isolate({
    
    timerinput(timerinput()-1)
    if(timerinput()>1 & input$showcolor=="No")
    {
      show("colourinputcov")
      show("colourinputstate")
      show("colourinput_trans") 
    }
    
  })
  
})

observeEvent(c(input$showcovar,invalidateLater(1000, session)), {
  
  if(input$showcovar=="No"){
    hide("covarinput")
  }
  
  if(input$showcovar=="Yes"){
    show("covarinput")
  }
  
  isolate({
    
    timerinput(timerinput()-1)
    if(timerinput()>1 & input$showcovar=="No")
    {
      show("covarinput")
    }
    
  })
})

observeEvent(c(input$showstate,invalidateLater(1000, session)), {
  
  if(input$showstate=="No"){
    hide("statesinput")
  }
  
  if(input$showstate=="Yes"){
    show("statesinput")
  }
  
  isolate({
    
    timerinput(timerinput()-1)
    if(timerinput()>1 & input$showstate=="No")
    {
      show("statesinput")
    }
    
  })
})

observeEvent(c(input$istext,invalidateLater(1000, session)), {
  
  if(input$istext=="No"){
    hide("tickinput")
  }
  
  if(input$istext=="Yes"){
    show("tickinput")
  }
  
  isolate({
    
    timerinput(timerinput()-1)
    if(timerinput()>1 & input$istext=="No")
    {
      show("tickinput")
      
    }
    
  })
})


output$pageinput1 <- renderUI({

    
    fluidRow(                  
     
      column(3,

             useShinyjs(),
             uiOutput("select")
      ),
      column(9,
             useShinyjs(),
             uiOutput("includecov"),
             uiOutput("selectcov")
      )
    )
})
 

output$pageinput2 <- renderUI({

  fluidRow(                  
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    column(2,
           useShinyjs(),
           uiOutput("is_smooth"),
           verbatimTextOutput("fileob")
           ),
    column(2,
           useShinyjs(),
           uiOutput("is_textinput"),
           uiOutput("tickinput")
           ),
    column(2,
           useShinyjs(),
           uiOutput("is_showcolor"),
           uiOutput("colourinputcov"),
           uiOutput("colourinputstate"),
           uiOutput("colourinput_trans") 
    ),
    
    column(3,
           useShinyjs(),
           uiOutput("is_showcovar"),
           uiOutput("covarinput")
    ),
    
    column(3,
           useShinyjs(),
           uiOutput("is_showstate"),
           uiOutput("statesinput")
    )
    
    
    
  )

  
})


#output$fileob<- renderPrint({
#  
#  json2manual()
#
#})


output$is_smooth<- renderUI ({
#  if (is.null(myjson2())) return()
  
  radioButtons("smooth", "Smooth plot transition between covariate patterns",
               choices = list("No" = "No", "Yes" = "Yes"), selected = "Yes")
})

output$is_textinput<- renderUI ({
 # if (is.null(myjson2())) return()
  
  radioButtons("istext", "Choose size and color of labels",
               choices = list("No" = "No", "Yes" = "Yes"), selected = "No")
})

output$is_showcolor<- renderUI ({
  #if (is.null(myjson2())) return()
  
  radioButtons("showcolor", "Show colouring options",
               choices = list("No" = "No","Yes" = "Yes"),selected = "No")
})


output$is_showcovar<- renderUI ({
  if (is.null(myjson2())) return()
  
  radioButtons("showcovar", "Show covariate naming options",
               choices = list("No" = "No","Yes" = "Yes"),selected = "No")
})


output$is_showstate<- renderUI ({
  if (is.null(myjson2())) return()
  
  radioButtons("showstate", "Show state naming options",
               choices = list("No" = "No","Yes" = "Yes"),selected = "No")
})


output$tickinput <- renderUI({
  
  default_choices=c("black","blue1","brown1","chartreuse2","cyan1","darkgray","firebrick3",
                    "gold","darkorange2","lightsteelblue4","rosybrow2","violetred2",
                    "yellow2","yellowgreen","tan1","lightslateblue","khaki4","gray28",
                    "cyan3","brown4","darkorchid1","goldenrod4","gray63","lightsalmon",
                    "maroon4","palegreen1","royalblue2","red2","sienna4","yellow4","slategray3")
  
  default_choices_scale=c(1,2,3)
  
  if (is.null(myjson2()))  return()
  item_list <- list()

  item_list[[1]] <-numericInput("textsize" ,"Legends size",value=15,min=5,max=30)
  #item_list[[2]] <-selectInput("textcolour","Legends colour", choices= default_choices, selected =default_choices[1] )
  #item_list[[3]] <-selectInput("textfont"  ,"Font", choices= default_choices, selected =default_choices[1] )
  item_list[[2]] <-selectInput("figscale"  ,"Figures scale (multiple of 1200*900px)",choices= default_choices_scale, selected =default_choices_scale[1] )
  
  
  
  
  do.call(tagList, item_list)
})



######### colour input covariate patterns#######################
output$colourinputcov<- renderUI ({
  
  if (is.null(myjson2())) return()
  
  item_list <- list()
  
  item_list[[1]]<- h2("Select colour for each covariate pattern")
  
  default_choices_colour=vector()
  default_choices_colour=c("blue1","brown1","chartreuse2","cyan1","darkgray","firebrick3",
                           "gold","darkorange2","lightsteelblue4","rosybrow2","violetred2",
                           "yellow2","yellowgreen","tan1","lightslateblue","khaki4","gray28",
                           "cyan3","brown4","darkorchid1","goldenrod4","gray63","lightsalmon",
                           "maroon4","palegreen1","royalblue2","red2","sienna4","yellow4","slategray3")
  
  colour_choices_title=vector("character",length =myjson2()$Nats )  
  
  for (i in 1:myjson2()$Nats ) {
    colour_choices_title[i]=paste0("Colour cov. pattern"," ",i)
  }
  
  
  for (i in 1:myjson2()$Nats) {
    
    item_list[[1+i]] <- selectInput(paste0('colourcov',i),label=colour_choices_title[i],
                                    choices= default_choices_colour, selected =default_choices_colour[i] )
    
  }
  
  do.call(tagList, item_list)
  
})

labels_colour_cov<- reactive ({
  
  if (is.null(myjson2())) return()
  
  
  myList<-vector("list",myjson2()$Nats)
  for (i in 1:myjson2()$Nats ) {
    
    myList[[i]]= input[[paste0('colourcov', i)]][1]
    
  }
  
  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
  final_list
})


######### colour input states  #######################
output$colourinputstate<- renderUI ({
  
  if (is.null(myjson2()))
    return()
  
  item_list <- list()
  
  item_list[[1]]<- h2("Select colour for each state")
  
  default_choices_colour=vector()
  default_choices_colour=c("palegreen1","royalblue2","red2","sienna4","yellow4","slategray3",
                           "blue1","brown1","chartreuse2","cyan1","darkgray","firebrick3",
                           "gold","darkorange2","lightsteelblue4","rosybrow2","violetred2",
                           "yellow2","yellowgreen","tan1","lightslateblue","khaki4","gray28",
                           "cyan3","brown4","darkorchid1","goldenrod4","gray63","lightsalmon",
                           "maroon4")
  
  colour_choices_title=vector("character",length = length(myjson2()$P ) )
  
  for (i in 1:length(myjson2()$P)) {
    colour_choices_title[i]=paste0("Colour state"," ",i)
  }
  
  
  for (i in 1:length(myjson2()$P)) {
    
    item_list[[1+i]] <- selectInput(paste0('colourstate',i),label=colour_choices_title[i],
                                    choices= default_choices_colour, selected =default_choices_colour[i] )
    
  }
  
  do.call(tagList, item_list)
  
})

labels_colour_state<- reactive ({
  
  if (is.null(myjson2())) return()
  
  
  myList<-vector("list",length(myjson2()$P))
  for (i in 1:length(myjson2()$P)) {
    
    myList[[i]]= input[[paste0('colourstate', i)]][1]
    
  }
  
  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
  final_list
})

###################Input covariate patterns##########################


#Create the reactive input of covariates
output$covarinput <- renderUI({
  
  #if (is.null(myjson2())) return()
  
  
  item_list <- list()
  item_list[[1]] <- h2("Covariate patterns")
  
  v=vector()
  for (i in 1:myjson2()$Nats) {
    v[i]=myjson2()$cov$atlist[i]
  }
  
  default_choices_cov=v
  
  for (i in 1:myjson2()$Nats) {
    item_list[[i+1]] <- textInput(paste0('cov', i),default_choices_cov[i], default_choices_cov[i])
  }
  
  do.call(tagList, item_list)
  
})

labels_cov<- reactive ({
  
 # if (is.null(myjson2())) return()
  
  
  myList<-vector("list",myjson2()$Nats)
  for (i in 1:myjson2()$Nats) {
    
    myList[[i]]= input[[paste0('cov', i)]][1]
  }
  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
  final_list 
  
})

#values <- reactiveValues()
#
#observe({
#  labels_experimental=labels_cov()
#  if (!(is.null(labels_experimental))){
#    values$data <- labels_experimental
#  }
#})



###############################################################
######### Input hazard colours ################################
###############################################################

#output$colourinput_trans <- renderUI ({
#  
#  if (is.null(myjson2())) return()
#  
#  item_list <- list()
#  
#  item_list[[1]]<- h2("Select colour for each transition")
#  
#  default_choices_colour=vector()
#  default_choices_colour=c("tan1","lightslateblue","khaki4","gray28",
#                           "cyan3","brown4","darkorchid1","goldenrod4","gray63","lightsalmon",
#                           "maroon4","palegreen1","royalblue2","red2","sienna4","yellow4","slategray3",
#                           "blue1","brown1","chartreuse2","cyan1","darkgray","firebrick3",
#                           "gold","darkorange2","lightsteelblue4","rosybrow2","violetred2",
#                           "yellow2","yellowgreen")
#  
#  colour_choices_title=vector("character",length = length(myjson2()$h ) )
#  
#  for (i in 1:length(myjson2()$h)) {
#    colour_choices_title[i]=paste0("Colour transition"," ",i)
#  }
#  
#  
#  for (i in 1:length(myjson2()$h)) {
#    
#    item_list[[1+i]] <- selectInput(paste0('colourtrans',i),label=colour_choices_title[i],
#                                    choices= default_choices_colour, selected =default_choices_colour[i] )
#    
#  }
#  
#  do.call(tagList, item_list)
#  
#})
#
#labels_colour_trans<- reactive ({
#  
#  if (is.null(myjson2())) return()
#  
#  
#  myList<-vector("list",length(myjson2()$h))
#  
#  for (i in 1:length(myjson2()$h)) {
#    
#    myList[[i]]= input[[paste0('colourtrans', i)]][1]
#    
#  }
#  
#  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
#  final_list
#})
#

output$colourinput_trans <- renderUI ({
  
  if (is.null(myjson2())) return()
  
  item_list <- list()
  
  item_list[[1]]<- h2("Select colour for each transition")
  
  default_choices_colour=vector()
  default_choices_colour=c("tan1","lightslateblue","khaki4","gray28",
                           "cyan3","brown4","darkorchid1","goldenrod4","gray63","lightsalmon",
                           "maroon4","palegreen1","royalblue2","red2","sienna4","yellow4","slategray3",
                           "blue1","brown1","chartreuse2","cyan1","darkgray","firebrick3",
                           "gold","darkorange2","lightsteelblue4","rosybrow2","violetred2",
                           "yellow2","yellowgreen")
  
  tmat_temp=myjson2()$tmat[as.numeric(input$select),]
  
  
  colour_choices_title=vector("character",length = length(which(!is.na(tmat_temp))) )
  
  
  tr_start_state=vector()
  
  for (k in 1:length(which(!is.na(tmat_temp)))  ) {
    tr_start_state[k]=as.numeric(input$select)
  }
  
  tr_end_state=vector()
  for (k in 1:length(which(!is.na(tmat_temp)))  ) {
    tr_end_state[k]=which(!is.na(tmat_temp))[k]
  }
  
  colour_choices_title=vector()
  
  for (i in 1:length(which(!is.na(tmat_temp)))  )  {
    
    colour_choices_title[i]=paste0('Colour for transition'," ", tr_start_state[i],"->",tr_end_state[i])  }
  
  
  for (i in 1:length(which(!is.na(tmat_temp))) ) {
    
    item_list[[1+i]] <- selectInput(paste0('colourtrans',i),label=colour_choices_title[i],
                                    choices= default_choices_colour, selected =default_choices_colour[i] )
    
  }
  
  do.call(tagList, item_list)
  
})

labels_colour_trans<- reactive ({
  
  if (is.null(myjson2()$haz)) return()
  
  tmat_temp=myjson2()$tmat[as.numeric(input$select),]
  
  myList<-vector("list",length(which(!is.na(tmat_temp))) )
  
  for (i in 1:length(which(!is.na(tmat_temp))) ) {
    
    myList[[i]]= input[[paste0('colourtrans', i)]][1]
    
  }
  
  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
  final_list
})


labels_colour_trans<- reactive ({
  
  if (is.null(myjson2()$haz)) return()
  
  tmat_temp=myjson2()$tmat[as.numeric(input$select),]
  
  myList<-vector("list",length(which(!is.na(tmat_temp))) )
  
  for (i in 1:length(which(!is.na(tmat_temp))) ) {
    
    myList[[i]]= input[[paste0('colourtrans', i)]][1]
    
  }
  
  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
  final_list
})

# ############### Input states ###############################




output$statesinput <- renderUI({
  
  if (is.null(myjson2())) return()
  
  item_list <- list()
  
  item_list[[1]] <- h2("To states")
  
  title_choices_state=vector()
  
  for (i in 1:length(myjson2()$P)) {
    title_choices_state[i]=paste0("State"," ",input$select,selectend()[i])
  }
  
  
  
  
  default_choices_state=vector()
  
  if (is.null(myjson1() )) {
    
          if ( (input$loadtype=="json"|input$loadtype=="csv") & input$aimtype=="present" )  {
              
               if (length(myjson2()$statenames)==length(myjson2()$P) ) {
                   for (i in 1:length(myjson2()$P)) {
                       default_choices_state[i]=myjson2()$statenames[i] 
                   }
               }
            
               else if ( (length(myjson2()$statenames)!=length(myjson2()$P) ) | is.null(myjson2()$statenames)) { 
                    for (i in 1:length(myjson2()$P)) {
                      default_choices_state[i]=paste0("State"," ",input$select,selectend()[i])
                    }
              }
            
          }
    
          if ((input$loadtype=="json"|input$loadtype=="csv") & input$aimtype=="compare" )  {
      
              if (length(myjson2()$statenames)==length(myjson2()$P)/2) {
              
                   for (i in 1:(length(myjson2()$P)/2))  {
                     
                         default_choices_state[i]=myjson2()$statenames[i]
                         default_choices_state[(length(myjson2()$P)/2)+i]=myjson2()$statenames[i]
                   }
              }
           
              else if ( (length(myjson2()$statenames)!=length(myjson2()$P/2) ) | is.null(myjson2()$statenames)) { 
             
                  for (i in 1:length(myjson2()$P)) {
                    default_choices_state[i]=paste0("State"," ",input$select,selectend()[i])
                    default_choices_state[(length(myjson2()$P)/2)+i]=paste0("State"," ",input$select,selectend()[i],"2nd approach") 
                     
                  }
              }
      
         }
     
 }
    
 if (!is.null(myjson1() )) {
    

      
      if (input$aimtype=="present") {
      
        for (i in 1:length(labels_states_box())) {
          default_choices_state[i]=labels_states_box()[i]
        }
        
      }
   
   
      if (input$aimtype=="compare") {
        
        
          for (i in 1:(length(myjson2()$P)/2)) {
             default_choices_state[i]=labels_states_box()[i]
             default_choices_state[(length(myjson2()$P)/2)+i]=paste0(labels_states_box()[i]," 2nd approach")
          }
          
       }
        
    }
      
  

  
  for (i in 1:length(myjson2()$P)) {
    item_list[[1+i]] <- textInput(paste0('state',i),title_choices_state[i],default_choices_state[i])
  }
  
  do.call(tagList, item_list)
})




labels_state<- reactive ({
  
  if (is.null(myjson2())) return()
  
  myList<-vector("list",length(myjson2()$P))
  
  for (i in 1:length(myjson2()$P)) {
    myList[[i]]= input[[paste0('state', i)]][1]
  }
  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
  final_list
})


output$select<- renderUI ({
  
  if (is.null(myjson1_5())) return()
  
  radioButtons(inputId="select", label="Select the probabilities
                                  From state:", 
               choices=unique(sub("_to_.*","", sub("P_","",names(myjson1_5()$select) )) ),
               selected = unique(sub("_to_.*","", sub("P_","",names(myjson1_5()$select) ))  )[1]   )
})

### Recombine the isolated input of from state to define the ending of names specifind the end states  
selectend<-reactive ({
  
  if (is.null(myjson1_5() )) return()
  
  cond_select<-which(startsWith(names(myjson1_5()), paste0('P_',input$select))  & 
                       !startsWith(names(myjson1_5()), 'P_diff' ) &  !startsWith(names(myjson1_5()), 'P_ratio' ) &
                       !endsWith(names(myjson1_5()), 'uci' ) & !endsWith(names(myjson1_5()), 'lci') )
  
  names=names(myjson1_5()[cond_select])
  
  v=vector()
  for (i in 1:length(cond_select)) {
    v[i]=sub("P_[[:digit:]]+","",names[i])  
  }
  v
})

#### select which of the defined covariate patterns you want
output$includecov<- renderUI ({
  if (is.null(myjson1_5() )) return()
  
  radioButtons(inputId="includecov", label="Manually select covariate patterns", 
               choices=c("Yes","No") ,
               selected = "No"   )
})





output$selectcov<- renderUI ({
  if (is.null(myjson1_5() )) return("Provide the json file with the predictions")
  
  item_list <- list()
  
  item_list[[1]] <- paste0("Reference covariate pattern","  :  ",myjson1_5()$atlist[1])
  
  if (input$includecov == 'Yes')  {
    
    if (length(myjson1_5()$atlist)>1) {
      item_list[[2]] <-   checkboxGroupInput("selectcov","Select extra covariate patterns 
                                             (at least one)",
                                             choices= myjson1_5()$atlist[-1] ,
                                             selected = myjson1_5()$atlist[-1] )
    }
    
    else return("You have included only one covariate pattern in your analysis")
  }
  do.call(tagList, item_list)
  
})






#### Turn the selected covariate patterns into a vector of numbers corresponding
#### to the order of the covariate patterns element in the atlist
covselect_char<- reactive ({
  
  shiny::validate(
    need(!is.null(input$selectcov) , "Please select at least 1 covariate pattern")
  )
  
  final_list=vector()
  
  
  myList<- input[['selectcov']]
  
  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
  
  final_list
  
})


covselect<- reactive ({
  
  tonumber=vector()
  
  shiny::validate(
    need(!is.null(input$selectcov) , "Please select at least 1 covariate pattern")
  )
  
  # if (is.null(input$selectcov)) {tonumber=0}
  
  # else if (!is.null(input$selectcov)) {
  
  for (i in 1:length(covselect_char())) {
    tonumber[i]= which(myjson1_5()$atlist==covselect_char()[i])
  }
  #}
  tonumber
})

covselectcontr<- reactive ({
  
  
  tonumber=vector()
  
  if (length(covselect_char())!=0) {
    
    
    for (i in 1:length(covselect_char())) {
      tonumber[i]= which(myjson1_5()$atlist[-1]==covselect_char()[i])
    }
    tonumber=as.integer(tonumber)
  }
  
  tonumber
})


