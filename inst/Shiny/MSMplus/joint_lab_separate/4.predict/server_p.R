
#observeEvent(input$aimtype, {
#  if( input$aimtype=="compare"  ) {
#    js$disableTab("#panel7p")
#    
#  } 
#}) 

observeEvent(input$json2, {
  if( length(which(startsWith(names(fromJSON(input$json2$datapath, flatten=TRUE)), 'P')))==0  ) {
    js$disableTab("mytab_p")
    
  } 
}) 

observeEvent(input$csv2, {
  if( length(which(startsWith(names( read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'P')))==0 ) {
    js$disableTab("mytab_p")
    
  } 
  
  if( length(which(startsWith(names( read.table(input$csv2$datapath,header=TRUE, sep=",") ), 'P')))!=0 ) {
    js$enableTab("mytab_p")
    
  } 
}) 


##### Hide or show ticks axis ####
timerp <- reactiveVal(1.5)



##############################################################

observeEvent(c(input$showtickp,invalidateLater(1000, session)), { 
  
  if(input$showtickp=="No"){
    
    hide("tickinputp")
    
  }
  
  if(input$showtickp=="Yes"){
    
    show("tickinputp")
    
  }
  
  isolate({
    
    timerp(timerp()-1)
    if(timerp()>1 & input$showtickp=="No")
    {
      show("tickinputp")
    }
    
  })
})





##### The main page ######################################

existp<- reactive({
  if (length(myjson2()$P) != 0) {       
    x= 1
  }
  else if (length(myjson2()$P) == 0) {       
    x= 0
  }
})

existpratio <- reactive({
  if (length(myjson2()$Pr) != 0) {       
    x= 1
  }
  else if (length(myjson2()$Pr) == 0| myjson2()$Nats==1 ) {       
    x= 0
  }
})

existpdiff <- reactive({
  if (length(myjson2()$Pd) != 0) {       
    x= 1
  }
  else if (length(myjson2()$Pd) == 0 | myjson2()$Nats==1 ) {       
    x= 0
  }
})


output$pagep <- renderUI({
  
  
  if (is.null(myjson2())) return("Provide the json file with the predictions")
  
  if (existp()==0) {
    
    fluidRow(
      column(12,
             output$loginpagep <- renderUI({h1("Non applicable")})
      )
    )
  }
  
  else if (existp()==1) {
    
    if (existpdiff()==1 & existpratio()==1) { 
      
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,
               h1("Probabilities"),
               
               conditionalPanel(condition="input.tabsp =='#panel1p'||input.tabsp =='#panel2p'||input.tabsp =='#panel4p'
                                || input.tabsp =='#panel5p' ||input.tabsp =='#panel8p' ||input.tabsp =='#panel9p'",
                                uiOutput("ui_facetp")
               )  ,         
               
               conditionalPanel(condition="input.tabsp =='#panel1p'||input.tabsp =='#panel2p'||input.tabsp =='#panel3p'
                                || input.tabsp =='#panel8p' ||input.tabsp =='#panel9p'",
                                uiOutput("confp")
               )  ,   
            
        ),
        
        column(2,
               br(),
               p(""),
               
               conditionalPanel(condition="input.tabsp =='#panel1p'||input.tabsp =='#panel2p'||input.tabsp =='#panel3p'
                                || input.tabsp =='#panel4p' ||input.tabsp =='#panel8p' || input.tabsp =='#panel9p'",
                                
                                uiOutput("showtickp"),
                                uiOutput("tickinputp")
               )  ,   
               
               conditionalPanel(condition="input.tabsp =='#panel5p'||input.tabsp =='#panel6p'||input.tabsp =='#panel7p'",
                                
                                uiOutput("framespeed")
               )  ,   
               
               conditionalPanel(condition="input.tabsp =='#panel5p'|| input.tabsp =='#panel7p'",
                                
                                uiOutput("areaperc"),
               )  ,   
               conditionalPanel(condition="input.tabsp =='#panel5p'||input.tabsp =='#panel6p'|| input.tabsp =='#panel7p'",
                                
                                uiOutput("textsizep_msm")
               )  ,   
               verbatimTextOutput("infinite")

        ),
        
        column(7,
               tabsetPanel(id = "tabsp", 
                 tabPanel(h2("By state"),value = "#panel1p",
                     fluidRow( 
                        column(12, useShinyjs(),plotlyOutput("probability_state", height="700px", width = "100%"),uiOutput("shouldloadp1"),verbatimTextOutput("states1"))
                     )
                 ),
                 tabPanel(h2("By covariate pattern"),value = "#panel2p",
                          fluidRow( 
                            column(12, useShinyjs(), plotlyOutput("probability_cov", height="700px", width = "100%"),uiOutput("shouldloadp2"),verbatimTextOutput("cov"))
                          )
                 ),
                 tabPanel(h2("By covariate pattern and state"),value = "#panel3p",
                          fluidRow( 
                            column(12, useShinyjs(),plotlyOutput("probability_state_cov", height="600px", width = "100%"),uiOutput("shouldloadp3"),verbatimTextOutput("statecov"))
                          )
                 ),
                 tabPanel(h2("Stacked"),value = "#panel4p",
                          fluidRow(
                            column(12, useShinyjs(),
                                   conditionalPanel(condition="input.aimtype =='present'",
                                                    useShinyjs(),plotlyOutput("probability_both", height="600px", width = "100%"),uiOutput("shouldloadp4"),uiOutput("afterstacked"), 
                                                    ),
                                   conditionalPanel(condition="input.aimtype =='compare'",
                                                    uiOutput("nograph_stacked")
                                                    )
                            ) 
                            
                          )
                 ),
                 tabPanel(h2("Bar plots by covariate pattern"),value = "#panel5p",
                          fluidRow( 
                            column(12, useShinyjs(),plotlyOutput("probability_bars", height="600px", width = "100%"),uiOutput("shouldloadp5"),uiOutput("afterbp"))
                          )
                 ),
                 tabPanel(h2("Stacked bar plot"),value = "#panel6p",
                          fluidRow( 
                            column(12, useShinyjs(),
                                     conditionalPanel(condition="input.aimtype =='present'",
                                                    plotlyOutput("probability_bars_stacked", height="600px", width = "100%"),uiOutput("shouldloadp6"),uiOutput("afterstackedbp")
                                                    ),
                                     conditionalPanel(condition="input.aimtype =='compare'",
                                                    uiOutput("nograph_stackedbp")
                                     )
                                 ) 
                           )
                         ),
                 tabPanel(h2("Predictions on MSM"),value = "#panel7p",
                          fluidRow( 
                            column(12, useShinyjs(),
                                   conditionalPanel(condition="input.aimtype =='present'",
                                                    plotOutput("probability_msm_box",height="750px",width = "100%"),uiOutput("shouldloadp7"),uiOutput("aftermsm")
                                                   ),
                                   conditionalPanel(condition="input.aimtype =='compare'",
                                                    uiOutput("nograph_MSM")
                                   )
                                   )     
                                                             )
                 ),
                 tabPanel(h2("Differences"),value = "#panel8p",
                          fluidRow( 
                            column(12, useShinyjs(),    plotlyOutput("P_diff"  , height="600px", width = "100%"),uiOutput("shouldloadp8"))
                          )
                 ),
                 tabPanel(h2("Ratios"),value = "#panel9p",
                          fluidRow( 
                            column(12, useShinyjs(),   plotlyOutput("P_ratio"  , height="600px", width = "100%"),uiOutput("shouldloadp9"))
                          )
                 )

                 
               )   
               
        )
        
      )      
      
    }
    
    else if (existpdiff()==1 & existpratio()==0) { 
      
      
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,
               h1("Probabilities"),
               
               conditionalPanel(condition="input.tabsp =='#panel1p'||input.tabsp =='#panel2p'||input.tabsp =='#panel4p'
                                || input.tabsp =='#panel5p' ||input.tabsp =='#panel8p' ||input.tabsp =='#panel9p'",
                                uiOutput("ui_facetp")
               )  ,         
               
               conditionalPanel(condition="input.tabsp =='#panel1p'||input.tabsp =='#panel2p'||input.tabsp =='#panel3p'
                                || input.tabsp =='#panel9p' ||input.tabsp =='#panel9p'",
                                uiOutput("confp")
               )  ,   
               
        ),
        
        column(2,
               br(),
               p(""),
               
               conditionalPanel(condition="input.tabsp =='#panel1p'||input.tabsp =='#panel2p'||input.tabsp =='#panel3p'
                                || input.tabsp =='#panel4p' ||input.tabsp =='#panel8p' || input.tabsp =='#panel9p'",
                                
                                uiOutput("showtickp"),
                                uiOutput("tickinputp")
               )  ,   
               
               conditionalPanel(condition="input.tabsp =='#panel5p'||input.tabsp =='#panel6p'||input.tabsp =='#panel7p'",
                                
                                uiOutput("framespeed")
               )  ,   
               
               conditionalPanel(condition="input.tabsp =='#panel5p'|| input.tabsp =='#panel7p'",
                                
                                uiOutput("areaperc")
               )  ,   
               conditionalPanel(condition="input.tabsp =='#panel5p'||input.tabsp =='#panel6p'|| input.tabsp =='#panel7p'",
                                
                                uiOutput("textsizep_msm")
               )  ,  
               
               
        ),
        
        column(7,
               tabsetPanel(id = "tabsp",
                           tabPanel(h2("By state"),value = "#panel1p",
                                    fluidRow( 
                                      column(12, useShinyjs(),plotlyOutput("probability_state", height="700px", width = "100%"),uiOutput("shouldloadp1"),verbatimTextOutput("states1"))
                                    )
                           ),
                           tabPanel(h2("By covariate pattern"),value = "#panel2p",
                                    fluidRow( 
                                      column(12, useShinyjs(), plotlyOutput("probability_cov", height="700px", width = "100%"),uiOutput("shouldloadp2"),verbatimTextOutput("cov"))
                                    )
                           ),
                           tabPanel(h2("By covariate pattern and state"),value = "#panel3p",
                                    fluidRow( 
                                      column(12, useShinyjs(),plotlyOutput("probability_state_cov", height="600px", width = "100%"),uiOutput("shouldloadp3"),verbatimTextOutput("statecov"))
                                    )
                           ),
                           tabPanel(h2("Stacked"),value = "#panel4p",
                                    fluidRow(
                                      column(12, useShinyjs(),
                                             conditionalPanel(condition="input.aimtype =='present'",
                                                              useShinyjs(),plotlyOutput("probability_both", height="600px", width = "100%"),uiOutput("shouldloadp4"),uiOutput("afterstacked"), 
                                             ),
                                             conditionalPanel(condition="input.aimtype =='compare'",
                                                              uiOutput("nograph_stacked")
                                             )
                                      ) 
                                      
                                    )
                           ),
                           tabPanel(h2("Bar plots by covariate pattern"),value = "#panel5p",
                                    fluidRow( 
                                      column(12, useShinyjs(),plotlyOutput("probability_bars", height="600px", width = "100%"),uiOutput("shouldloadp5"),uiOutput("afterbp"))
                                    )
                           ),
                           tabPanel(h2("Stacked bar plot"),value = "#panel6p",
                                    fluidRow( 
                                      column(12, useShinyjs(),
                                             conditionalPanel(condition="input.aimtype =='present'",
                                                              plotlyOutput("probability_bars_stacked", height="600px", width = "100%"),uiOutput("shouldloadp6"),uiOutput("afterstackedbp")
                                             ),
                                             conditionalPanel(condition="input.aimtype =='compare'",
                                                              uiOutput("nograph_stackedbp")
                                             )
                                      ) 
                                    )
                           ),
                           tabPanel(h2("Predictions on MSM"),value = "#panel7p",
                                    fluidRow( 
                                      column(12, useShinyjs(),
                                             conditionalPanel(condition="input.aimtype =='present'",
                                                              plotOutput("probability_msm_box",height="750px",width = "100%"),uiOutput("shouldloadp7"),uiOutput("aftermsm")
                                             ),
                                             conditionalPanel(condition="input.aimtype =='compare'",
                                                              uiOutput("nograph_MSM")
                                             )
                                      )     
                                    )
                           ),
                           tabPanel(h2("Differences"),value = "#panel8p",
                                    fluidRow( 
                                      column(12, useShinyjs(),    plotlyOutput("P_diff"  , height="600px", width = "100%"),uiOutput("shouldloadp8"))
                                    )
                           ),
                           tabPanel(h2("Ratios"),value = "#panel9p",
                                    fluidRow( 
                            column(12, useShinyjs(),  print("Not Applicable"))
                          )
                 )
                 
                 
               )    
               
        )
        
      )      
    }
    
    else if ( existpdiff()==0 & existpratio()==1) { 
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,
               h1("Probabilities"),
               
               conditionalPanel(condition="input.tabsp =='#panel1p'||input.tabsp =='#panel2p'||input.tabsp =='#panel4p'
                                || input.tabsp =='#panel5p' ||input.tabsp =='#panel8p' ||input.tabsp =='#panel9p'",
                                uiOutput("ui_facetp")
               )  ,         
               
               conditionalPanel(condition="input.tabsp =='#panel1p'||input.tabsp =='#panel2p'||input.tabsp =='#panel3p'
                                || input.tabsp =='#panel9p' ||input.tabsp =='#panel9p'",
                                uiOutput("confp")
               )  ,   
               
               
        ),
        
        column(2,
               br(),
               p(""),
               
               conditionalPanel(condition="input.tabsp =='#panel1p'||input.tabsp =='#panel2p'||input.tabsp =='#panel3p'
                                || input.tabsp =='#panel4p' ||input.tabsp =='#panel8p' || input.tabsp =='#panel9p'",
                                
                                uiOutput("showtickp"),
                                uiOutput("tickinputp")
               )  ,   
               
               conditionalPanel(condition="input.tabsp =='#panel5p'||input.tabsp =='#panel6p'||input.tabsp =='#panel7p'",
                                
                                uiOutput("framespeed")
               )  ,   
               
               conditionalPanel(condition="input.tabsp =='#panel5p'|| input.tabsp =='#panel7p'",
                                
                                uiOutput("areaperc")
               )  ,   
               conditionalPanel(condition="input.tabsp =='#panel5p'||input.tabsp =='#panel6p'|| input.tabsp =='#panel7p'",
                                
                                uiOutput("textsizep_msm")
               )  
               
        ),
        column(7,
               tabsetPanel(id = "tabsp",
                           tabPanel(h2("By state"),value = "#panel1p",
                                    fluidRow( 
                                      column(12, useShinyjs(),plotlyOutput("probability_state", height="700px", width = "100%"),uiOutput("shouldloadp1"),verbatimTextOutput("states1"))
                                    )
                           ),
                           tabPanel(h2("By covariate pattern"),value = "#panel2p",
                                    fluidRow( 
                                      column(12, useShinyjs(), plotlyOutput("probability_cov", height="700px", width = "100%"),uiOutput("shouldloadp2"),verbatimTextOutput("cov"))
                                    )
                           ),
                           tabPanel(h2("By covariate pattern and state"),value = "#panel3p",
                                    fluidRow( 
                                      column(12, useShinyjs(),plotlyOutput("probability_state_cov", height="600px", width = "100%"),uiOutput("shouldloadp3"),verbatimTextOutput("statecov"))
                                    )
                           ),
                           tabPanel(h2("Stacked"),value = "#panel4p",
                                    fluidRow(
                                      column(12, useShinyjs(),
                                             conditionalPanel(condition="input.aimtype =='present'",
                                                              useShinyjs(),plotlyOutput("probability_both", height="600px", width = "100%"),uiOutput("shouldloadp4"),uiOutput("afterstacked"), 
                                             ),
                                             conditionalPanel(condition="input.aimtype =='compare'",
                                                              uiOutput("nograph_stacked")
                                             )
                                      ) 
                                      
                                    )
                           ),
                           tabPanel(h2("Bar plots by covariate pattern"),value = "#panel5p",
                                    fluidRow( 
                                      column(12, useShinyjs(),plotlyOutput("probability_bars", height="600px", width = "100%"),uiOutput("shouldloadp5"),uiOutput("afterbp"))
                                    )
                           ),
                           tabPanel(h2("Stacked bar plot"),value = "#panel6p",
                                    fluidRow( 
                                      column(12, useShinyjs(),
                                             conditionalPanel(condition="input.aimtype =='present'",
                                                              plotlyOutput("probability_bars_stacked", height="600px", width = "100%"),uiOutput("shouldloadp6"),uiOutput("afterstackedbp")
                                             ),
                                             conditionalPanel(condition="input.aimtype =='compare'",
                                                              uiOutput("nograph_stackedbp")
                                             )
                                      ) 
                                    )
                           ),
                           tabPanel(h2("Predictions on MSM"),value = "#panel7p",
                                    fluidRow( 
                                      column(12, useShinyjs(),
                                             conditionalPanel(condition="input.aimtype =='present'",
                                                              plotOutput("probability_msm_box",height="750px",width = "100%"),uiOutput("shouldloadp7"),uiOutput("aftermsm")
                                             ),
                                             conditionalPanel(condition="input.aimtype =='compare'",
                                                              uiOutput("nograph_MSM")
                                             )
                                      )     
                                    )
                           ),
                           tabPanel(h2("Differences"),value = "#panel8p",
                                    fluidRow( 
                                      column(12, useShinyjs(),    print("Not Applicable"))
                                    )
                           ),
                           tabPanel(h2("Ratios"),value = "#panel9p",
                                    fluidRow( 
                                      column(12, useShinyjs(),   plotlyOutput("P_ratio"  , height="600px", width = "100%"),uiOutput("shouldloadp9"))
                                    )
                           )
                           
                           
               )  
               
        )
        
      )      
    }
    
    else if (existpdiff()==0 & existpratio()==0) { 
      
      
      
      fluidRow(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        column(2,
               h1("Probabilities"),
               
               conditionalPanel(condition="input.tabsp =='#panel1p'||input.tabsp =='#panel2p'||input.tabsp =='#panel4p'
                                || input.tabsp =='#panel5p' ||input.tabsp =='#panel8p' ||input.tabsp =='#panel9p'",
                                uiOutput("ui_facetp")
               )  ,         
               
               conditionalPanel(condition="input.tabsp =='#panel1p'||input.tabsp =='#panel2p'||input.tabsp =='#panel3p'
                                || input.tabsp =='#panel9p' ||input.tabsp =='#panel9p'",
                                uiOutput("confp")
               )  ,   
               
               
        ),
        
        column(2,
               br(),
               p(""),
               
               conditionalPanel(condition="input.tabsp =='#panel1p'||input.tabsp =='#panel2p'||input.tabsp =='#panel3p'
                                || input.tabsp =='#panel4p' ||input.tabsp =='#panel8p' || input.tabsp =='#panel9p'",
                                
                                uiOutput("showtickp"),
                                uiOutput("tickinputp")
               )  ,   
               
               conditionalPanel(condition="input.tabsp =='#panel5p'||input.tabsp =='#panel6p'||input.tabsp =='#panel7p'",
                                
                                uiOutput("framespeed")
               )  ,   
               
               conditionalPanel(condition="input.tabsp =='#panel5p'|| input.tabsp =='#panel7p'",
                                
                                uiOutput("areaperc")
               )  ,   
               conditionalPanel(condition="input.tabsp =='#panel5p'||input.tabsp =='#panel6p'|| input.tabsp =='#panel7p'",
                                
                                uiOutput("textsizep_msm")
               )  

        ),
        
        column(7,
               tabsetPanel(id = "tabsp",
                           tabPanel(h2("By state"),value = "#panel1p",
                                    fluidRow( 
                                      column(12, useShinyjs(),plotlyOutput("probability_state", height="700px", width = "100%"),uiOutput("shouldloadp1"),verbatimTextOutput("states1"))
                                    )
                           ),
                           tabPanel(h2("By covariate pattern"),value = "#panel2p",
                                    fluidRow( 
                                      column(12, useShinyjs(), plotlyOutput("probability_cov", height="700px", width = "100%"),uiOutput("shouldloadp2"),verbatimTextOutput("cov"))
                                    )
                           ),
                           tabPanel(h2("By covariate pattern and state"),value = "#panel3p",
                                    fluidRow( 
                                      column(12, useShinyjs(),plotlyOutput("probability_state_cov", height="600px", width = "100%"),uiOutput("shouldloadp3"),verbatimTextOutput("statecov"))
                                    )
                           ),
                           tabPanel(h2("Stacked"),value = "#panel4p",
                                    fluidRow(
                                      column(12, useShinyjs(),
                                             conditionalPanel(condition="input.aimtype =='present'",
                                                              useShinyjs(),plotlyOutput("probability_both", height="600px", width = "100%"),uiOutput("shouldloadp4"),uiOutput("afterstacked"), 
                                             ),
                                             conditionalPanel(condition="input.aimtype =='compare'",
                                                              uiOutput("nograph_stacked")
                                             )
                                      ) 
                                      
                                    )
                           ),
                           tabPanel(h2("Bar plots by covariate pattern"),value = "#panel5p",
                                    fluidRow( 
                                      column(12, useShinyjs(),plotlyOutput("probability_bars", height="600px", width = "100%"),uiOutput("shouldloadp5"),uiOutput("afterbp"))
                                    )
                           ),
                           tabPanel(h2("Stacked bar plot"),value = "#panel6p",
                                    fluidRow( 
                                      column(12, useShinyjs(),
                                             conditionalPanel(condition="input.aimtype =='present'",
                                                              plotlyOutput("probability_bars_stacked", height="600px", width = "100%"),uiOutput("shouldloadp6"),uiOutput("afterstackedbp")
                                             ),
                                             conditionalPanel(condition="input.aimtype =='compare'",
                                                              uiOutput("nograph_stackedbp")
                                             )
                                      ) 
                                    )
                           ),
                           tabPanel(h2("Predictions on MSM"),value = "#panel7p",
                                    fluidRow( 
                                      column(12, useShinyjs(),
                                             conditionalPanel(condition="input.aimtype =='present'",
                                                              plotOutput("probability_msm_box",height="750px",width = "100%"),uiOutput("shouldloadp7"),uiOutput("aftermsm")
                                             ),
                                             conditionalPanel(condition="input.aimtype =='compare'",
                                                              uiOutput("nograph_MSM")
                                             )
                                      )     
                                    )
                           ),
                           tabPanel(h2("Differences"),value = "#panel8p",
                                    fluidRow( 
                                      column(12, useShinyjs(),    print("Not Applicable"))
                                    )
                           ),
                           tabPanel(h2("Ratios"),value = "#panel9p",
                                    fluidRow( 
                                      column(12, useShinyjs(),   print("Not Applicable"))
                                    )
                           )
                           
                           
               ) 
               
        )
        
      )      
      
    }
  }
  
}) 

#output$infinite <- renderPrint({
#min(data_P_ratio2_lci()$V[which(!is.na(data_P_ratio2_lci()$V))])
#})
#output$shouldloadgridp1 <- renderUI({
#  if (input$facet=="No" )  return()
#  downloadButton(outputId = "downgridp1", label = h2("Download the plot"))
#})
#
#output$downgridp1 <- downloadHandler(
#  
#  filename= function() {
#    paste("p1","png", sep=".") 
#  },
#  content= function(file) {
#
#   ggsave(file,plot=input$probability_state,device = "png")
#    
#  }
#) 
#
output$nograph_MSM<- renderUI({

  helpText("This graph is not available when comparing 2 approaches") 
  
})

output$nograph_stacked<- renderUI({
  
  helpText("This graph is not available when comparing 2 approaches") 
  
})

output$nograph_stackedbp<- renderUI({
  
  helpText("This graph is not available when comparing 2 approaches") 
  
})


output$showtickp<- renderUI({
  
  radioButtons("showtickp", "Show axis tick options",
               choices = list("No" = "No",
                              "Yes" = "Yes"), selected = "No")
})

output$ui_facetp<- renderUI({
  radioButtons(inputId="facet", label= "Display graph in grids",
               choices=c("No","Yes"),selected = "No")
})

output$confp <- renderUI({
  
  if  (length(myjson2()$ci_P)!=0) {
    radioButtons("conf", "Confidence intervals",
                 c("No" = "ci_no",
                   "Yes" ="ci_yes"))
  }
  
  else if (length(myjson2()$ci_P)==0) {
    item_list <- list()
    item_list[[1]]<- radioButtons("conf", "Confidence intervals",c("No" = "ci_no"))
    item_list[[2]]<-print("Confidence interval data were not provided")
    do.call(tagList, item_list)
  }
  
})

output$textsizep_msm <- renderUI({
  
  if (is.null(myjson2()))  return()
  item_list <- list()

  item_list[[1]] <-numericInput("textsizep_msm",h2("Legends size"),value=15,min=5,max=30)

  
  do.call(tagList, item_list)
})


##################################################
###### Will appear conditionally##################
###################################################
#observeEvent(input$showSidebar, {
#  shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
#})
#observeEvent(input$hideSidebar, {
#  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
#})
#Create the reactive input of covariates

#output$covarinputp <- renderUI({
#  
#  #if (is.null(myjson2()))  return()
#  
#  if (input$displayp=="same") return()
#  
#  else if (input$displayp=="change") {
#    
#    item_list <- list()
#    item_list[[1]] <- h2("Covariate patterns")
#    
#    v=vector()
#    for (i in 1:length(myjson2()$atlist)) {
#      v[i]=myjson2()$atlist[i]
#    }
#    
#    default_choices_cov=v
#    
#    for (i in seq(length(myjson2()$atlist))) {
#      item_list[[i+1]] <- textInput(paste0('covp', i),default_choices_cov[i], labels_cov()[i])
#    }
#    
#    do.call(tagList, item_list)
#  }
#})
#
#labels_covp<- reactive ({
#  
#  if (input$displayp!="change") {as.vector(labels_cov())}
#  
#  else if (input$displayp=="change") {
#    
#    
#    myList<-vector("list",length(myjson2()$cov$atlist))
#    
#    for (i in 1:length(myjson2()$cov$atlist)) {
#      myList[[i]]= input[[paste0('covp', i)]][1]
#    }
#    final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
#    as.vector(final_list)
#  }
#})

#Create the reactive input of states

#output$statesinputp <- renderUI({
#  
#  if (input$displayp=="same") return()
#  
#  else if (input$displayp=="change") {  
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
#      item_list[[1+i]] <- textInput(paste0('statep',i),title_choices_state[i], labels_state()[i])
#      
#    }
#    do.call(tagList, item_list)
#  }
#})

# labels_statep<- reactive ({
#   
#   if (input$displayp!="change") labels_state()
#   
#   else if (input$displayp=="change") {
#     
#     myList<-vector("list",length(myjson2()$P))
#     for (i in 1:length(myjson2()$P)) {
#       
#       myList[[i]]= input[[paste0('statep', i)]][1]
#     }
#     final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
#     final_list
#   }
# })

##################################################################################
###################################################################################




data_P <- reactive ({
  if(is.null(myjson2())) return()
  
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups
  timevar=as.data.frame(myjson2()$timevar); names(timevar)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {v[i]=myjson2()$cov$atlist[i]}
  
  ## Different variable of probabilities for each covariate pattern
  prob=list()
  
  if (length(myjson2()$P)==0) {return()}
  
  for(i in 1:length(myjson2()$P)) {
    prob[[i]]=as.data.frame(t(data.frame(myjson2()$P[i])))
    colnames(prob[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$P)) {  
    prob[[i]]=as.data.frame(cbind(prob[[i]], timevar ,state=rep(i,nrow(prob[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_P=list()
  data_P[[1]]=prob[[1]]
  
  if (length(myjson2()$P)>1) {
    
    for (u in 2:(length(myjson2()$P))) {
      data_P[[u]]=rbind(prob[[u]],data_P[[(u-1)]])
    }
  }
  datap=data_P[[length(myjson2()$P)]]
  datap 
})



data_P_uci <- reactive ({
  if(is.null(myjson2())) return()
  
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {v[i]=myjson2()$cov$atlist[i]}
  
  ## Different variable of probabilities for each covariate pattern
  prob_uci=list()
  
  if (length(myjson2()$P_uci)==0) {return()}
  
  for(i in 1:length(myjson2()$P_uci)) {
    prob_uci[[i]]=as.data.frame(t(data.frame(myjson2()$P_uci[i])))
    colnames(prob_uci[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$P_uci)) {  
    prob_uci[[i]]=as.data.frame(cbind(prob_uci[[i]], timevar ,state=rep(i,nrow(prob_uci[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_P_uci=list()
  data_P_uci[[1]]=prob_uci[[1]]
  
  if (length(myjson2()$P_lci)>1) {
    
    for (u in 2:(length(myjson2()$P_uci))) {
      data_P_uci[[u]]=rbind(prob_uci[[u]],data_P_uci[[(u-1)]])
    }
  }
  datap_uci=data_P_uci[[length(myjson2()$P_uci)]]
  datap_uci 
})

data_P_lci <- reactive ({
  if(is.null(myjson2())) return()
  
  #Will give a certain shape to the probability data so that we have the 
  #covariate patterns as variables and the states as groups
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v=vector()
  for (i in 1:length(myjson2()$cov$atlist)) {v[i]=myjson2()$cov$atlist[i]}
  
  ## Different variable of probabilities for each covariate pattern
  prob_lci=list()
  
  if (length(myjson2()$P_lci)==0) {return()}
  
  for(i in 1:length(myjson2()$P_lci)) {
    prob_lci[[i]]=as.data.frame(t(data.frame(myjson2()$P_lci[i])))
    colnames(prob_lci[[i]]) <-labels_cov()
  }
  
  for(i in 1:length(myjson2()$P_lci)) {  
    prob_lci[[i]]=as.data.frame(cbind(prob_lci[[i]], timevar ,state=rep(i,nrow(prob_lci[[i]] )) ))
  }
  
  # Append the probabilities datasets of the different states
  data_P_lci=list()
  data_P_lci[[1]]=prob_lci[[1]]
  
  if (length(myjson2()$P_lci)>1) {
    for (u in 2:(length(myjson2()$P_lci))) {
      data_P_lci[[u]]=rbind(prob_lci[[u]],data_P_lci[[(u-1)]])
    }
  }
  
  
  
  datap_lci=data_P_lci[[length(myjson2()$P_lci)]]
  datap_lci 
})



output$areaperc <- renderUI({
  
  if (is.null(myjson2()))  return()
  item_list <- list()
  
  
  
  item_list[[1]] <- sliderInput("perc",h2("Time points (Applicable to the non stacked bar graphs)"),
                                min=min(myjson2()$timevar),
                                max=max(myjson2()$timevar),
                                step=myjson2()$timevar[2]-myjson2()$timevar[1], 
                                value=1, width='100%', 
                                animate=animationOptions(interval = (1000/input$speed)))
  
  do.call(tagList, item_list)
  
})





data_P_st<-reactive  ({
  
  datanew=data_P()
  
 # if (input$aimtype=="compare") { datanew$state_fac=c(rep("NA",nrow(datanew)))}
 # else if (input$aimtype=="present") {  datanew$state_fac=ordered(c(rep("NA",nrow(datanew))), levels = labels_state() )}
  
  datanew$state_fac=ordered(c(rep("NA",nrow(datanew))), levels = labels_state() )
  
  for (o in 1:(length(myjson2()$P))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$state[g]==o) {datanew$state_fac[g]=labels_state()[o] }  
    }
  }
  datanew
})


data_P_st_uci<-reactive  ({
  datanew=data_P_uci()
  datanew$state_fac=ordered(c(rep("NA",nrow(datanew))), levels = labels_state() )
  
  for (o in 1:(length(myjson2()$P_uci))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$state[g]==o) {datanew$state_fac[g]=labels_state()[o] }  
    }
  }
  datanew
})

data_P_st_lci<-reactive  ({
  datanew=data_P_lci()
  datanew$state_fac=ordered(c(rep("NA",nrow(datanew))), levels = labels_state() )
  
  for (o in 1:(length(myjson2()$P_lci))) {
    for (g in 1:nrow(datanew))  {
      if  (datanew$state[g]==o) {datanew$state_fac[g]=labels_state()[o] }  
    }
  }
  datanew
})



data_P_d <- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_P_st()[,d],data_P_st()[,ncol(data_P_st())-2],data_P_st()[,ncol(data_P_st())-1],data_P_st()[,ncol(data_P_st())],rep(d,length(data_P_st()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_P_st())[d],length(data_P_st()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  d_all_p <- bind_rows(dlist, .id = "column_label")
  
  d_all_p
})



data_P_d_uci <- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_P_st_uci()[,d],data_P_st_uci()[,ncol(data_P_st_uci())-2],
                                data_P_st_uci()[,ncol(data_P_st_uci())-1],
                                data_P_st_uci()[,ncol(data_P_st_uci())],rep(d,length(data_P_st_uci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_P_st_uci())[d],length(data_P_st_uci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  d_all_p_uci <- bind_rows(dlist, .id = "column_label")
  d_all_p_uci
}) 

data_P_d_lci<- reactive ({
  
  ### Meke one variable of probabilities so now, states and covariate patterns 
  ### define subgroups of the dataset
  dlist=list()
  for (d in 1:length(myjson2()$cov$atlist)) {
    
    dlist[[d]]=cbind.data.frame(data_P_st_lci()[,d],data_P_st_lci()[,ncol(data_P_st_lci())-2],
                                data_P_st_lci()[,ncol(data_P_st_lci())-1],
                                data_P_st_lci()[,ncol(data_P_st_lci())],rep(d,length(data_P_st_lci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_P_st_lci())[d],length(data_P_st_lci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  d_all_p_lci <- bind_rows(dlist, .id = "column_label")
  d_all_p_lci
})


output$tickinputp <- renderUI({
  
  default_choices=c("black","blue1","brown1","chartreuse2","cyan1","darkgray","firebrick3",
                    "gold","darkorange2","lightsteelblue4","rosybrow2","violetred2",
                    "yellow2","yellowgreen","tan1","lightslateblue","khaki4","gray28",
                    "cyan3","brown4","darkorchid1","goldenrod4","gray63","lightsalmon",
                    "maroon4","palegreen1","royalblue2","red2","sienna4","yellow4","slategray3")
  
  if (is.null(myjson2()))  return()
  item_list <- list()
  item_list[[1]] <- h2("Provide x axis range and ticks")
  item_list[[2]] <-numericInput("startx","Start x axis at:",value=min(data_P_d()$timevar),min=0 )
  item_list[[3]] <-numericInput("stepx","Step at x axis:",value=max(data_P_d()$timevar/10),min=0,max=max(data_P_d()$timevar))
  item_list[[4]] <-numericInput("endx","End x axis at:",value =max(data_P_d()$timevar),min=0,max=max(data_P_d()$timevar))
  item_list[[5]] <-numericInput("stepy","Step at y axis:",value=max(data_P_d()$V)/10,min=max(data_P_d()$V)/1000,max=max(data_P_d()$V)/10)
  item_list[[6]] <-numericInput("textsizep",h2("Legends size"),value=input$textsize,min=5,max=30)
  item_list[[7]] <-numericInput("textfacetp",h2("Facet title size"),value=input$textsize-3,min=5,max=30 )
  
  
  
  do.call(tagList, item_list)
})

output$framespeed <- renderUI({
  
  if (is.null(myjson2()))  return()
  item_list <- list()
  item_list[[1]] <-numericInput("speed",h2("Time speed"),value=3,min=1, max=20 )
  
  
  do.call(tagList, item_list)
})


data_P_ci<- reactive ({
  x=c( data_P_d()[order(data_P_d()$timevar,data_P_d()$state,data_P_d()$cov),]$timevar,
       data_P_d_lci()[order(-data_P_d()$timevar,data_P_d()$state,data_P_d()$cov),]$timevar )
  
  y_central=c( data_P_d()[order(data_P_d()$timevar,data_P_d()$state,data_P_d()$cov),]$V,
               data_P_d()[order(-data_P_d()$timevar,data_P_d()$state,data_P_d()$cov),]$V )
  
  y=c( data_P_d_uci()[order(data_P_d_uci()$timevar,data_P_d_uci()$state,data_P_d_uci()$cov),]$V,
       data_P_d_lci()[order(-data_P_d_uci()$timevar,data_P_d_uci()$state,data_P_d_uci()$cov),]$V )
  
  frameto=c(as.character(data_P_d_uci()[order(-data_P_d_uci()$timevar,data_P_d_uci()$state,data_P_d_uci()$cov),]$state_factor),
            as.character(data_P_d_lci()[order(-data_P_d_lci()$timevar,data_P_d_lci()$state,data_P_d_lci()$cov),]$state_factor) )
  
  covto=c( data_P_d_uci()[order(-data_P_d_uci()$timevar,data_P_d_uci()$state,data_P_d_uci()$cov),]$cov_factor,
           data_P_d_lci()[order(-data_P_d_lci()$timevar,data_P_d_lci()$state,data_P_d_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,frameto,covto,y_central)
  data
})

#########################################################
#output$shouldloadp1 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotp1", label = h2("Download the plot"))
#})

datap1_re <-  reactive ({

if(is.null(data_P_d())) return()
  
  else
  
  if (input$conf=="ci_no") {
    
    if (input$facet=="No") {
      
      
      
      p_state= plot_ly(data_P_d(),alpha=0.5) %>%
        add_lines(
          x=data_P_d()$timevar,y=data_P_d()$V,
          frame=factor(as.factor(data_P_d()$state_factor),levels=labels_state()),
          color=factor(as.factor(data_P_d()$cov_factor)  ,levels=labels_cov()),
          colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
          mode="lines",
          line=list(simplify=FALSE,color = labels_colour_cov()[1:length(myjson2()$cov$atlist)] ),
          text = 'Select or deselect lines by clicking on the legend',
          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
          )
      
      
      p_state = p_state %>%
        layout(title=list(text="Probability of state for each covariate pattern over states",y=0.95),
               font= list(family = "times new roman", size = input$textsizep, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepx, 
                          tick0 = input$startx, 
                          range=c(input$startx,input$endx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Probability of state occupancy",rangemode = "nonnegative",                    
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
          ),queueLength=10 
        )
      
      p_state
      
    }
    
    
    if (input$facet=="Yes") {
      
      
      data_plot=data_P_d()
      
      p_state = ggplot(data_plot)
      p_state = ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(cov_factor),levels=labels_cov()), group=1,
                                     text=paste("Select or deselect lines by clicking on the legend",
                                           "<br>Time: ", timevar,
                                           "<br>Probability: ", V,
                                           "<br>Covariate pattern: ", factor(as.factor(cov_factor),levels=labels_cov()) ) )
                       )

      
      p_state = p_state+geom_line(aes(x=timevar, y=V, color= factor(as.factor(cov_factor),levels=labels_cov()) ))+
        scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      if (input$aimtype=="compare") { p_state = p_state+ facet_wrap(~ factor(as.factor(state_factor), levels=labels_state()  ), nrow=2)}
      
      else if (input$aimtype=="present")   {p_state = p_state+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()))}
      
      
      p_state = p_state + scale_x_continuous(breaks=c(seq(input$startx,input$endx,by=input$stepx)))
      
      p_state = p_state + scale_y_continuous(breaks=c(seq(min(data_plot$V[which(!is.na(data_plot$V))]),max(data_plot$V[which(!is.na(data_plot$V))]), by=input$stepy )))
      

      
      p_state = p_state +labs(title="Probability of state for each covariate pattern over states", x="Time since entry", y="Probability of state")
      
      p_state = p_state + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      p_state = p_state +theme(title = element_text(size = input$textsizep-4),  strip.text = element_text(size=input$textfacetp),       
                               legend.title = element_text(color="black", size= input$textsizep-5), 
                               legend.text=element_text(size= input$textsizep-6),
                               plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                               legend.margin = margin(1.5, 1, 1, 1, "cm"),
                               legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                               axis.title.y = element_text(size= input$textsizep-5),
                               axis.title.x = element_text(size= input$textsizep-5), 
                               axis.text.x = element_text( size=input$textsizep-6),axis.text.y = element_text( size=input$textsizep-6)
                               )   
      
      
      p_state = ggplotly(p_state, tooltip = "text")%>%
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
      
      p_state 
    }
    
  }
  
  else if (input$conf=="ci_yes") {
    
    if (input$facet=="No") {
      
      p_state <- plot_ly()
      
      p_state <- add_trace(p_state, line=list(simplify=FALSE,color = labels_colour_cov()),
                           mode="lines", type = "scatter",
                           x=data_P_ci()$x, y=data_P_ci()$y_central,
                           frame=factor(as.factor(data_P_ci()$frameto),levels=labels_state()), 
                           colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
                           color=as.factor(data_P_ci()$covto),
                           text = 'Select or deselect lines by clicking on the legend',
                           hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                 "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") )
      
      p_state <- add_trace(p_state, fill = "tozerox", 
                           line=list(dash = "solid", color = "transparent", width = 1.8897637),
                           mode = "lines", type = "scatter",
                           x=data_P_ci()$x, y=data_P_ci()$y,
                           frame=factor(as.factor(data_P_ci()$frameto),levels=labels_state()),
                           colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
                           color=as.factor(data_P_ci()$covto),
                           text = 'Select or deselect lines by clicking on the legend',
                           hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                 "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"),
                           showlegend = FALSE) #### May need to have an input value here if we eant to deselect CIs
      
      
      p_state = p_state %>%
        layout(title=list(text="Probability of state for each covariate pattern over states",y=0.95),
               font= list(family = "times new roman", size = input$textsizep, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepx, 
                          tick0 = input$startx, 
                          range=c(input$startx,input$endx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Probability of state occupancy",rangemode = "nonnegative",                    
                           dtick = input$stepy, 
                           ticklen = 5,
                           tickwidth = 2,
                           tickcolor = toRGB("black")),
               shapes = list(
                 list(type = "rect",
                      fillcolor = "grey", 
                      line = list(color = "grey"), 
                      opacity = 0.8,
                      x0 = 0, x1 =input$area, xref = "x", y0 = 0, y1 = 1, yref = "y") )) %>%
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
      
      p_state
    }
    
    if (input$facet=="Yes") {
      
      V_lci=  data_P_d_lci()$V
      V_uci=  data_P_d_uci()$V
      
      data_plot=cbind(data_P_d(),V_lci,V_uci)
      
      
      p_state=ggplot(data_plot)
      p_state=ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(cov_factor),levels=labels_cov()), group=1,
                                   text=paste("Select or deselect lines by clicking on the legend",
                                              "<br>Time: ", timevar,
                                              "<br>Probability: ", V,
                                              "<br>Covariate pattern: ", factor(as.factor(cov_factor),levels=labels_cov()) )))
      
      p_state=p_state +scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      
      p_state=p_state+geom_line(aes(x=timevar, y=V, fill=factor(as.factor(cov_factor),levels=labels_cov()) ))
      
      p_state=p_state+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill= factor(as.factor(cov_factor),levels=labels_cov()) ),alpha=0.4)+ 
        scale_fill_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
      
      if (input$aimtype=="compare") { p_state = p_state+ facet_wrap(~ factor(as.factor(state_factor), levels=labels_state()), nrow=2)}
      
      else if (input$aimtype=="present")   {p_state = p_state+ facet_wrap(~ factor(as.factor(state_factor), levels=labels_state()))}
      
      p_state = p_state + scale_x_continuous(breaks=c(seq(input$startx,input$endx,by=input$stepx  ))) 
      p_state = p_state + scale_y_continuous(breaks=c(seq(min(data_plot$V_lci[which(!is.na(data_plot$V_lci))]),max(data_plot$V_uci[which(!is.na(data_plot$V_uci))]),by=input$stepy )))
      

      
      
      p_state = p_state +labs(title="Probability of state for each covariate pattern over states", x="Time since entry", y="Probability of state")
      
      
      p_state = p_state + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
      
      p_state = p_state +theme(title = element_text(size = input$textsizep-4),   strip.text = element_text(size=input$textfacetp),             
                               legend.title = element_text(color="black", size= input$textsizep-5), 
                               legend.text=element_text(size= input$textsizep-6),
                               plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                               legend.margin = margin(1.5, 1, 1, 1, "cm"),
                               legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                               axis.title.y = element_text(size= input$textsizep-5),
                               axis.title.x = element_text(size= input$textsizep-5), 
                               axis.text.x = element_text( size=input$textsizep-6),axis.text.y = element_text( size=input$textsizep-6))  
      
      p_state = ggplotly(p_state, tooltip = "text")%>%
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
      
      p_state 
      
    }
  }

  p_state
})


output$probability_state <- renderPlotly ({
  
 datap1_re()
  
  })

output$downplotp1 <- downloadHandler(
  filename = function(){paste("p1",'.png',sep='')},
  content = function(file){
    
   # if (input$facet=="No") { 
      plotly_IMAGE( datap1_re(),width = 3000, height = 3000, format = "svg", scale = 1,  out_file = file )
  #  }

  #  if (input$facet=="Yes") {
  #    png(filename=file, width=1000, height=1000,units = "px", res=200)
  #    datap1_re()
  #    dev.off()
   #   }
  }
)
################################################################

####Statements after plots #######################

output$covs <- renderPrint({
  
  if ( length(myjson2()$cov$atlist)>5 ) {
    
    df= data.frame(Covariate_patterns=labels_cov())
    df
  }
  else return(print("If more than 5 covariate patterns specified, they will be listed here as well"))  
})
output$states1 <- renderPrint({
  
  if ( length(myjson2()$P)>5)  {
    
    df= data.frame(States=labels_state())
    df
  }
  
  else return(print("If more than 5 states specified, they will be listed here as well"))
  
})
output$states2 <- renderPrint({
  
  if ( length(myjson2()$P)>5 ) {
    
    df= data.frame(States=labels_state())
    df
  }
  else return()
  
})
output$statecov <- renderPrint({
  
  if ( length(myjson2()$P)>5 | length(myjson2()$cov$atlist)>5)    {
    
    df= data.frame(States=labels_state(), Covariate_patterns=labels_cov())
    df
  }
  else return(print("If more than 5 states or covariate patterns specified, they will be listed here as well"))  
  
  
})


output$afterstacked <- renderPrint({
  
  helpText("The transition probabilities for each timepoint are given stacked. They will always sum to 1 ") 
  
})

output$afterstackedbp <- renderPrint({
  
  helpText("The transition probabilities for each covariate pattern are given stacked over states. They will always sum to 1. 
           The estimates across time are given through a slide bar underneath the graph.") 
  
})

output$aftermsm <- renderPrint({
  
  helpText("The transition probabilities for each covariate pattern over states, depicted in boxes accordingly 
           to the multi-state graph. The estimates across time are given through a slide bar underneath the graph.") 
  
  
})

output$afterbp <- renderPrint({
  
  helpText("The transition probabilities for each covariate pattern depicted as bar plots. The estimates for diffent
           states are presented either in frames or grids. The estimates across time are given through a slide bar beside the graph.") 
  
})

##############################################################
#output$shouldloadp2 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotp2", label = h2("Download the plot"))
#})


datap2_re <-  reactive ({
  
  if (input$conf=="ci_no") {
    
    if (input$facet=="No") {
      
      
      p_cov= plot_ly(data_P_d(),alpha=0.5) %>%
        add_lines(
          x=data_P_d()$timevar,y=data_P_d()$V,
          frame=factor(as.factor(data_P_d()$cov_factor),levels=labels_cov()),
          color=factor(as.factor(data_P_d()$state_factor),levels=labels_state()),
          colors=labels_colour_state(),
          mode="lines",
          line=list(simplify=FALSE,color = labels_colour_state()),
          text = 'Select or deselect lines by clicking on the legend',
          hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")    )
      
      
      p_cov = p_cov %>%
        layout(title=list(text="Probability of each state over covariate patterns",y=0.95),
               font= list(family = "times new roman", size = input$textsizep, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepx, 
                          tick0 = input$startx, 
                          range=c(input$startx,input$endx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Probability of state occupancy",rangemode = "nonnegative",                    
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
        p_cov= p_cov %>%
          animation_opts(frame = 1000, transition = 0, redraw = FALSE)
      }
      
      p_cov
      
    }
    
    
    if (input$facet=="Yes") {
      
      
      data_plot=data_P_d()
     
      p_cov = ggplot(data_plot)
      p_cov = ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(state_factor),levels=labels_state()), group=1,
                                   text=paste("Select or deselect lines by clicking on the legend",
                                              "<br>Time: ", timevar,
                                              "<br>Probability: ", V,
                                              "<br>State: ",   factor(as.factor(state_factor),levels=labels_state()) )))
      
      p_cov = p_cov+geom_line(aes(x=timevar, y=V, color= factor(as.factor(state_factor),levels=labels_state()) ))+
        scale_colour_manual( values =labels_colour_state(),labels = labels_state()  ) 
      

      if (input$aimtype=="compare") { p_cov = p_cov+ facet_wrap(~ factor(as.factor(cov_factor), levels=labels_cov()), nrow=2)}
      
      else if (input$aimtype=="present")   {p_cov = p_cov+ facet_wrap(~ factor(as.factor(cov_factor), levels=labels_cov() ) )}
      
      
      p_cov = p_cov + scale_x_continuous(breaks=c(seq(input$startx,input$endx,by=input$stepx ))) 
      p_cov = p_cov + scale_y_continuous(breaks=c(seq(min(data_plot$V[which(!is.na(data_plot$V))]),max(data_plot$V[which(!is.na(data_plot$V))]), by=input$stepy )))
      


      p_cov = p_cov +labs(title="Probability of each state over covariate patterns", x="Time since entry", y="Probability of state")
      
      p_cov = p_cov + labs(color = "States")+ labs(fill = "States")
      
      p_cov = p_cov +theme(title = element_text(size = input$textsizep-4),   strip.text = element_text(size=input$textfacetp),      
                           legend.title = element_text(color="black", size= input$textsizep-5), 
                           legend.text=element_text(size= input$textsizep-6),
                           plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                           legend.margin = margin(1.5, 1, 1, 1, "cm"),
                           legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                           axis.title.y = element_text(size= input$textsizep-5),
                           axis.title.x = element_text(size= input$textsizep-5), 
                           axis.text.x = element_text( size=input$textsizep-6),axis.text.y = element_text( size=input$textsizep-6))   
      
      p_cov = ggplotly(p_cov, tooltip = "text")%>%
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
      
      p_cov 
    }
    
  }
  
  else if (input$conf=="ci_yes") {
    
    if (input$facet=="No") {
      
      p_cov <- plot_ly()
      
      p_cov <- add_trace(p_cov, line=list(simplify=FALSE,color = labels_colour_cov()),
                         mode="lines", type = "scatter",
                         x=data_P_ci()$x, y=data_P_ci()$y_central,
                         frame=factor(as.factor(data_P_ci()$covto),levels=labels_cov()), 
                         colors=labels_colour_state()[1:length(myjson2()$P)],
                         color=as.factor(data_P_ci()$frameto),
                         text = 'Select or deselect lines by clicking on the legend',
                         hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                               "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") )
      
      p_cov <- add_trace(p_cov, fill = "tozerox", 
                         line=list(dash = "solid", color = "transparent", width = 1.8897637),
                         mode = "lines", type = "scatter",
                         x=data_P_ci()$x, y=data_P_ci()$y,
                         frame=factor(as.factor(data_P_ci()$covto),levels=labels_cov()), 
                         colors=labels_colour_state()[1:length(myjson2()$P)],
                         color=as.factor(data_P_ci()$frameto) ,
                         showlegend = FALSE,
                         text = 'Select or deselect lines by clicking on the legend',
                         hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                               "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
      
      
      p_cov = p_cov %>%
        layout(title=list(text="Probability of each state for each covariate pattern",y=0.95),
               font= list(family = "times new roman", size = input$textsizep, color = "black"),
               margin = list(l = 50, r = 50, b = 30, t = 70),
               xaxis=list(title=list(text="Time since entry",y=0.2),
                          dtick = input$stepx, 
                          tick0 = input$startx, 
                          range=c(input$startx,input$endx),
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"),
                          tickmode = "linear"),
               yaxis =list(title= "Probability of each state over covariate patterns",rangemode = "nonnegative",                    
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
        p_cov= p_cov %>%
          animation_opts(frame = 1000, transition = 0, redraw = FALSE)
      }
      
      p_cov
    }
    
    if (input$facet=="Yes") {
      
      V_lci=  data_P_d_lci()$V
      V_uci=  data_P_d_uci()$V
      
      data_plot=cbind(data_P_d(),V_lci,V_uci)
      
      
      p_cov=ggplot(data_plot)
      p_cov=ggplot(data_plot,aes(x=timevar, y=V, color= factor(as.factor(state_factor), levels=labels_state()), group=1,
                                 text=paste("Select or deselect lines by clicking on the legend",
                                            "<br>Time: ", timevar,
                                            "<br>Probability: ", V,
                                            "<br>State: ", factor(as.factor(state_factor), levels=labels_state()) )))+
        scale_colour_manual( values =labels_colour_state(),labels = labels_state()  ) 
      
      
      p_cov=p_cov+geom_line(aes(x=timevar, y=V, fill=factor(as.factor(state_factor), levels=labels_state()) ))
      
      p_cov=p_cov+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill=factor(as.factor(state_factor), levels=labels_state()) ),alpha=0.4)+ 
        scale_fill_manual( values =labels_colour_state(),labels = labels_state()  ) 
      
      
      
      if (input$aimtype=="compare") { p_cov = p_cov+ facet_wrap(~ factor(as.factor(cov_factor), levels=labels_cov()), nrow=2) }
      
      else if (input$aimtype=="present")   {p_cov = p_cov+ facet_wrap(~factor(as.factor(cov_factor), levels=labels_cov() ) ) }
      
      
      
      p_cov = p_cov + scale_x_continuous(breaks=c(seq(input$startx,input$endx,by=input$stepx  ))) 
      
      p_cov = p_cov + scale_y_continuous(breaks=c(seq(min(data_plot$V_lci[which(!is.na(data_plot$V_lci))]),max(data_plot$V_uci[which(!is.na(data_plot$V_uci))]),by=input$stepy )))
      
      p_cov = p_cov +labs(title="Probability of each state over covariate patterns", x="Time since entry", y="Probability of state")
      
      p_cov = p_cov + labs(color = "States")+ labs(fill = "States")
      
      p_cov = p_cov +theme(title = element_text(size = input$textsizep-4),  strip.text = element_text(size=input$textfacetp),       
                           legend.title = element_text(color="black", size= input$textsizep-5), 
                           legend.text=element_text(size= input$textsizep-6),
                           plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                           legend.margin = margin(1.5, 1, 1, 1, "cm"),
                           legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                           axis.title.y = element_text(size= input$textsizep-5),
                           axis.title.x = element_text(size= input$textsizep-5), 
                           axis.text.x = element_text( size=input$textsizep-6),axis.text.y = element_text( size=input$textsizep-6))  
      
      p_cov = ggplotly(p_cov, tooltip = "text")%>%
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
      
      p_cov 
      
    }
  }
  
  p_cov 
}) 

output$probability_cov <- renderPlotly ({datap2_re() })

output$downplotp2 <- downloadHandler(
  filename = function(){paste("p2",'.png',sep='')},
  content = function(file){
    plotly_IMAGE( datap2_re(),width = 3000, height = 3000, format = "png", scale = 1,  out_file = file )
  }
)
#################################################################


#output$shouldloadp3 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotp3", label = h2("Download the plot"))
#})

datap3_re <-  reactive ({
  
  
  if (input$conf=="ci_no") {
    
    p_state_cov= plot_ly(data_P_d(),alpha=0.5) %>%
      add_lines(
        x=data_P_d()$timevar,y=data_P_d()$V,
        color = factor(as.factor(data_P_d()$cov_factor),levels=labels_cov()),
        colors=labels_colour_cov(),
        fill = factor(as.factor(data_P_d()$cov_factor),levels=labels_cov()),
        linetype= factor(as.factor(data_P_d()$state_factor),levels=labels_state()),
        mode="lines",
        line=list(simplify=FALSE),
        text = 'Select or deselect lines by clicking on the legend',
        hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                              "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
      )  %>%
      layout(title=list(text="Probability of state occupancy for all covariate patterns",y=0.95),
             font= list(family = "times new roman", size = input$textsizep, color = "black"),
             margin = list(l = 50, r = 50, b = 30, t = 70),
             xaxis=list(title=list(text="Time since entry",y=0.2),
                        dtick = input$stepx, 
                        tick0 = input$startx, 
                        range=c(input$startx,input$endx),
                        ticklen = 5,
                        tickwidth = 2,
                        tickcolor = toRGB("black"),
                        tickmode = "linear"),
             yaxis =list(title= "Probability of state occupancy",rangemode = "nonnegative",                    
                         dtick = input$stepy, 
                         ticklen = 5,
                         tickwidth = 2,
                         tickcolor = toRGB("black")),
             shapes = list(
               list(type = "rect",
                    fillcolor = "grey", 
                    line = list(color = "grey"), 
                    opacity = 0.8,
                    x0 = 0, x1 =input$area, xref = "x", y0 = 0, y1 = 1, yref = "y") ) )%>%
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
    p_state_cov
  }
  
  else if (input$conf=="ci_yes") {
    p_state_cov <- plot_ly()
    
    p_state_cov  <- add_trace(p_state_cov, line=list(simplify=FALSE),
                              mode="lines", type = "scatter",
                              x=data_P_ci()$x, y=data_P_ci()$y_central,
                              color=as.factor(data_P_ci()$covto), 
                              colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
                              fill=as.factor(data_P_ci()$frameto),
                              linetype=as.factor(data_P_ci()$frameto),
                              text = 'Select or deselect lines by clicking on the legend',
                              hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                    "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
    
    p_state_cov  <- add_trace(p_state_cov, fill = "tozerox", 
                              line=list(dash = "solid", color = "transparent", width = 1.8897637),
                              mode = "lines", type = "scatter",
                              x=data_P_ci()$x, y=data_P_ci()$y,
                              color=as.factor(data_P_ci()$covto),
                              colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)],
                              fill=as.factor(data_P_ci()$frameto),
                              linetype=as.factor(data_P_ci()$frameto),
                                                 showlegend = FALSE,
                              text = 'Select or deselect lines by clicking on the legend',
                              hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                    "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
    )  %>%
      layout(title=list(text="Probability of state occupancy for all covariate patterns",y=0.95),
             font= list(family = "times new roman", size = input$textsizep, color = "black"),
             margin = list(l = 50, r = 50, b = 30, t = 70),
             xaxis=list(title=list(text="Time since entry",y=0.2),
                        dtick = input$stepx, 
                        tick0 = input$startx, 
                        range=c(input$startx,input$endx),
                        ticklen = 5,
                        tickwidth = 2,
                        tickcolor = toRGB("black"),
                        tickmode = "linear"),
             yaxis =list(title= "Probability of state occupancy",rangemode = "nonnegative",                    
                         dtick = input$stepy, 
                         ticklen = 5,
                         tickwidth = 2,
                         tickcolor = toRGB("black")),
             shapes = list(
               list(type = "rect",
                    fillcolor = "grey", 
                    line = list(color = "grey"), 
                    opacity = 0.8,
                    x0 = 0, x1 =input$area, xref = "x", y0 = 0, y1 = 1, yref = "y") ) )%>%
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
    p_state_cov
  }
  
  
}) 

output$probability_state_cov <- renderPlotly ({datap3_re() }) 

output$downplotp3 <- downloadHandler(
  filename = function(){paste("p3",'.png',sep='')},
  content = function(file){
    plotly_IMAGE( datap3_re(),width = 3000, height = 3000, format = "png", scale = 1,  out_file = file )
  }
)

#################################################################################

data_stacked_bars <- reactive ({
  
  if(is.null(myjson2())) return()
  
  stackedl=list()
  
  

    stackedl=stacked_function_bars(json=myjson2(), data=data_P(), labels_cov=labels_cov(), labels_state=labels_state() )
  
  
  stackedp=bind_rows(stackedl, .id = "column_label2")
  
  stackedp=stackedp[order(stackedp$state,stackedp$cov_factor,stackedp$timevar),]
  stackedp
})  


#output$shouldloadp4 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotp4", label = h2("Download the plot"))
#})

datap4_re <-  reactive ({
  
  if (input$aimtype=="compare") {return("Not available when comparing approaches")}
  
  else {
  
  dfk=list()
  for(k in 1:length(myjson2()$P)) {
    dfk[[k]] <- data.frame(prob_st=data_stacked_bars()[c((length(myjson2()$P)*(k-1)*length(myjson2()$timevar)+1):
                                                           ((length(myjson2()$P)*(k-1)*length(myjson2()$timevar)+1)+length(myjson2()$P)*length(myjson2()$timevar)-1)),3],
                           Time=data_stacked_bars()$timevar[(length(myjson2()$P)*(k-1)*length(myjson2()$timevar)+1):
                                                              ((length(myjson2()$P)*(k-1)*length(myjson2()$timevar)+1)+length(myjson2()$P)*length(myjson2()$timevar)-1)],
                           Covariate_pattern=factor(as.factor(data_stacked_bars()$cov_factor[(length(myjson2()$P)*(k-1)*length(myjson2()$timevar)+1):
                                                                                        ((length(myjson2()$P)*(k-1)*length(myjson2()$timevar)+1)+length(myjson2()$P)*length(myjson2()$timevar)-1)] ), levels=labels_cov()),
                           State=as.factor(data_stacked_bars()$state_factor[(length(myjson2()$P)*(k-1)*length(myjson2()$timevar)+1):
                                                                              ((length(myjson2()$P)*(k-1)*length(myjson2()$timevar)+1)+length(myjson2()$P)*length(myjson2()$timevar)-1)])
    )
  } 
  
  if (input$facet=="No") {
    
    

  
         P <- plot_ly(data = data_stacked_bars(), colors=labels_colour_state(), alpha=0.5,
                      line=list(simplify=FALSE, mode = 'lines', stackgroup = 'one',hoverinfo='x+y'),
                      text = 'Select or deselect lines by clicking on the legend',
                      hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                            "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")) %>%
           layout(title=list(text="Stacked probabilities of states among covariate patterns",y=0.95),
                  font= list(family = "times new roman", size = input$textsizep, color = "black"),
                  margin = list(l = 50, r = 50, b = 30, t = 70),
                  xaxis=list(title=list(text="Time since entry",y=0.2),
                             dtick = input$stepx, 
                             tick0 = input$startx, 
                             range=c(input$startx,input$endx),
                             ticklen = 5,
                             tickwidth = 2,
                             tickcolor = toRGB("black"),
                             tickmode = "linear"),
                  yaxis =list(title= "Stacked probability of state occupancies",rangemode = "nonnegative",                    
                              dtick = input$stepy, 
                              ticklen = 5,
                              tickwidth = 2,
                              tickcolor = toRGB("black")),
                  shapes = list(
                    list(type = "rect",
                         fillcolor = "grey", 
                         line = list(color = "grey"), 
                         opacity = 0.8,
                         x0 = 0, x1 =input$area, xref = "x", y0 = 0, y1 = 1, yref = "y") ) )%>%
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
           P=  P %>%
             animation_opts(frame = 1000, transition = 0, redraw = FALSE)
         }
         
         
   
         for(k in 1:length(myjson2()$P)) {
           P <- add_trace(  P, y=~prob_st, x=~Time, frame =~Covariate_pattern, data=dfk[[k]], 
                            mode = 'lines', stackgroup = 'one',hoverinfo='x+y',fill="tonexty", color=~State)
         }
         P
    }
      
  else if (input$facet=="Yes") {   
    
    df_final=list()
    
    df_final[[1]]=as.data.frame(dfk[[1]])
    
    for(k in 2:length(myjson2()$P)) {
         df_final[[k]]=rbind(as.data.frame(dfk[[k]]),df_final[[k-1]])
    }
    
    df_final[[k]]$Probability=df_final[[k]]$prob_st
    
    P <- ggplot(as.data.frame(df_final[[k]]), aes(x = Time, y =Probability,fill= State, alpha=0.5))+
        geom_area()+
        facet_wrap(~Covariate_pattern)
    
    
    P =  P + scale_x_continuous(breaks=c(seq(input$startx,input$endx,by=input$stepx ))) + 
             scale_y_continuous(breaks=c(seq(0,1,input$stepy)))
    
    P =  P +labs(title="Probability of each state over covariate patterns", x="Time since entry", y="Probability of state")
    
    P =  P + labs(fill = "States")
    
    P =  P + scale_fill_manual(values=labels_colour_state())
    
    P =  P +theme(title = element_text(size = input$textsizep-4),  strip.text = element_text(size=input$textfacetp),       
                         legend.title = element_text(color="black", size= input$textsizep-5), 
                         legend.text=element_text(size= input$textsizep-6),
                         plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                         legend.margin = margin(1.5, 1, 1, 1, "cm"),
                         legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                         axis.title.y = element_text(size= input$textsizep-5),
                         axis.title.x = element_text(size= input$textsizep-5), 
                  axis.text.x = element_text( size=input$textsizep-6),axis.text.y = element_text( size=input$textsizep-6))   
    
   
    P = ggplotly(P)%>% 
      onRender("function(el,x){el.on('plotly_legendclick', function(){ return false; })}")
    
    P =P%>%
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
  
})  

output$probability_both <- renderPlotly ({datap4_re() })


output$downplotp4 <- downloadHandler(
  filename = function(){paste("p4",'.png',sep='')},
  content = function(file){
    plotly_IMAGE( datap4_re(),width = 1200, height = 900, format = "png", scale =2,  out_file = file )
  }
)

#####################################################################################

data_t <- reactive ({
  if(is.null(data_P_d())) return()
  datat=data_P_d()[which(data_P_d()$timevar==input$perc),]
  datat
})  


#output$shouldloadp5 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotp5", label = h2("Download the plot"))
#})

datap5_re <-  reactive ({
  
  if (input$facet=="No") { 
    
    data_plot=data_t()
    
    map=as.vector(levels(as.factor(data_plot$cov_factor)))
    
    color_cov_factor=vector()
    
    for (k in 1:myjson2()$Nats) {
       for (n in 1:nrow(data_plot)) {
      if (data_plot$cov_factor[n]==map[k]) {color_cov_factor[n]=labels_colour_cov()[k]}   
       }
    }
    data_plot$color= color_cov_factor
    
      
    
  p_state= plot_ly(data_plot,alpha=0.5)%>%
    add_bars(type="bar",
             x=factor(as.factor(data_plot$cov_factor),levels=labels_cov()),y=data_plot$V,
             frame=factor(as.factor(data_plot$state_factor),levels=labels_state()),
             color=factor(as.factor(data_plot$cov_factor),levels=labels_cov()),
             colors=data_plot$color,
             mode="bar")  %>%
    layout(title=list(text="Probability of state occupancy across covariate patterns",y=0.95),
           font= list(family = "times new roman", size = input$textsizep_msm, color = "black"),
           margin = list(l = 50, r = 50, b = 30, t = 70),
           yaxis =list(title= "Probability of state", rangemode = "nonnegative",range=c(0,1),dtick = 0.1 ,ticklen = 5,tickwidth = 2,tickcolor = toRGB("black"))

           
    ) 
  p_state= p_state %>%
    animation_opts(1000/input$speed, easing = "quad")%>%
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
  
  p_state
  
  }
  else if (input$facet=="Yes") {   
    
    data_plot=data_t()
    data_plot$Probability=data_t()$V
    data_plot$State=data_t()$state_factor
    data_plot$Covariate_pattern=data_t()$cov_factor

    
    p_state <- ggplot(data_plot, aes(y =Probability,x= factor(as.factor(data_plot$Covariate_pattern),levels=labels_cov() ),fill=factor(as.factor(data_plot$Covariate_pattern),levels=labels_cov() ) ))+
      geom_bar(position="dodge", stat="identity")+
      facet_wrap(~State)
    
    p_state =  p_state+ scale_y_continuous(0,1,0.1) 
    p_state =  p_state +labs(title="Probability of each state over covariate patterns", x="Time since entry", y="Probability of state")
    
    p_state =  p_state + labs(fill = "States")
    
    p_state =  p_state +scale_fill_manual(values=labels_colour_cov())
    
    p_state =  p_state +theme(title = element_text(size = input$textsizep_msm),  strip.text = element_text(size=input$textsizep_msm-5),        
                  legend.title = element_text(color="black", size= input$textsizep_msm-1), 
                  legend.text=element_text(size= input$textsizep_msm-2),
                  plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                  legend.margin = margin(1.5, 1, 1, 1, "cm"),
                  legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                  axis.title.y = element_text(size= input$textsizep_msm-1),
                  axis.title.x = element_text(size= input$textsizep_msm-1))   
    
    p_state = ggplotly(p_state)  
    
    p_state= p_state %>%
      animation_opts(1000/input$speed, easing = "quad")%>%
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
    
    p_state
    
    #p_state = ggplotly(p_state)       
    
      #onRender("function(el,x){el.on('plotly_legendclick', function(){ return false; })}")     
  }

  })

output$probability_bars <- renderPlotly ({  datap5_re()  })

output$downplotp5 <- downloadHandler(
  filename = function(){paste("p5",'.png',sep='')},
  content = function(file){
    plotly_IMAGE( datap5_re(),width = 3000, height = 3000, format = "png", scale = 1,  out_file = file )
  }
)
#############################################################################################

#output$shouldloadp6 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotp6", label = h2("Download the plot"))
#})

datap6_re <-  reactive ({
  
  if (input$aimtype=="compare") {return("Not available when comparing approaches")}
  
  else 
  
  dfk=list()
  
  for(k in 1:length(myjson2()$P)) {
    dfk[[k]] <- data.frame(prob_st=data_stacked_bars()[c((myjson2()$Nats*(k-1)*length(myjson2()$timevar)+1):
                                                           ((myjson2()$Nats*(k-1)*length(myjson2()$timevar)+1)+myjson2()$Nats*length(myjson2()$timevar)-1)),3],
                           Time=data_stacked_bars()$timevar[(myjson2()$Nats*(k-1)*length(myjson2()$timevar)+1):
                                                              ((myjson2()$Nats*(k-1)*length(myjson2()$timevar)+1)+myjson2()$Nats*length(myjson2()$timevar)-1)],
                           Covariate_pattern=as.factor(data_stacked_bars()$cov_factor[(myjson2()$Nats*(k-1)*length(myjson2()$timevar)+1):
                                                                                        ((myjson2()$Nats*(k-1)*length(myjson2()$timevar)+1)+myjson2()$Nats*length(myjson2()$timevar)-1)] )  ,
                           State=as.factor(data_stacked_bars()$state_factor[(myjson2()$Nats*(k-1)*length(myjson2()$timevar)+1):
                                                                              ((myjson2()$Nats*(k-1)*length(myjson2()$timevar)+1)+myjson2()$Nats*length(myjson2()$timevar)-1)])
    )}
  
  
  P <- plot_ly(data_stacked_bars(), type = 'bar', colors=labels_colour_state(),
               text = 'Select or deselect bars by clicking on the legend',
               hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                     "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")) %>%
    layout(title=list(text="Stacked probability of states across covariate patterns",y=0.95),
           font= list(family = "times new roman", size = input$textsizep_msm, color = "black"),
           yaxis = list(title = 'Stacked probabilities',rangemode = "nonnegative",dtick = 0.1 ,ticklen = 5,tickwidth = 2,tickcolor = toRGB("black")),
           xaxis = list(rangemode = "nonnegative"),
           margin = list(l = 50, r = 50, b = 30, t = 70),
           barmode = 'stack', bargap = 0.1)
  
  for(k in 1:length(myjson2()$P)) {
    P <- add_trace(  P, y=dfk[[k]]$prob_st,  x=factor(as.factor(dfk[[k]]$Covariate_pattern),levels=labels_cov()),
                     color=factor(as.factor(dfk[[k]]$State),levels=labels_state() ),
                     frame =dfk[[k]]$Time, data=dfk[[k]],
                     name=factor(as.factor(dfk[[k]]$State),levels=labels_state() ),
                     width = 0.1
                     )
  }
  P <- P %>%
   animation_opts(1000/input$speed, easing ="linear-in") %>%
    animation_button(enumerated="animate")%>%
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
  
  P
  
})  

output$probability_bars_stacked <- renderPlotly ({datap6_re() })

output$downplotp6 <- downloadHandler(
  filename = function(){paste("p6",'.png',sep='')},
  content = function(file){
    plotly_IMAGE( datap6_re(),width = 3000, height = 3000, format = "png", scale = 1,  out_file = file )
  }
)

#######################################################################################################
#########  Image MSM probabilities ####################################################################
######################################################################################################

output$shouldloadp7 <- renderUI({
  if (is.null((myjson2())))  return()
  downloadButton(outputId = "downplotp7", label = h2("Download the plot"))
})

datap7_re <-  reactive ({
  
if (input$aimtype=="compare") {return("Not available when comparing approaches")}
  
else {
  
  if(is.null(myjson1())) {return()}
  
  else
    
  ntransitions=myjson1()$Ntransitions
  nstates= myjson1()$Nstates
  nats=length(myjson2()$atlist)
  xvaluesb=labels_x()  #+boxwidth/2
  yvaluesb=labels_y()  #-boxheight/2
  
  boxes=msboxes_R_nofreq(yb=yvaluesb, xb=xvaluesb, boxwidth=input$boxwidth , boxheight=input$boxheight, tmat.= myjson1()$tmat)
  #Read through json from msboxes or through a new function 
  x1=boxes$arrows$x1
  y1=boxes$arrows$y1
  x2=boxes$arrows$x2
  y2=boxes$arrows$y2
  
  
  if (nstates!=length(myjson2()$P)) {
    return(h2("The number of states are not corresponding between the 2 uploaded files"))
  }
  if (nstates==length(myjson2()$P) & nats==length(myjson2()$atlist)) {
    
    w1=input$boxwidth/((5*length(myjson2()$atlist))+1)
    w2=4*input$boxwidth/((5*length(myjson2()$atlist))+1)
    
    
    prob=matrix(nrow=nstates, ncol= nats, NA)
    xleft=matrix(nrow=nstates, ncol= nats, NA)
    xright=matrix(nrow=nstates, ncol= nats, NA)
    ybottom=matrix(nrow=nstates, ncol= nats, NA)
    ytop=matrix(nrow=nstates, ncol= nats, NA)
    
    
    for (i in 1:nstates ) {
      for (j in 1:nats ) {
        prob[i,j]=data_P_d()$V[which(data_P_d()$timevar==input$perc & data_P_d()$state==i & data_P_d()$cov_factor==labels_cov()[j])]
        xleft[i,j]=(xvaluesb[i]-input$boxwidth/2)+ j*w1 + (j-1)*w2
        xright[i,j]=(xvaluesb[i]-input$boxwidth/2)+ j*w1 + j*w2
        ybottom[i,j]=(yvaluesb[i]-input$boxheight/2)
        ytop[i,j]=   (yvaluesb[i]-input$boxheight/2)+ prob[i,j]*input$boxheight
        
      }
    }
    
    xticks=matrix(nrow=length(myjson2()$P), ncol=length(seq(0,1,by=0.1)),NA )
    yticks=matrix(nrow=length(myjson2()$P), ncol=length(seq(0,1,by=0.1)),NA )
    
    
    for (i in 1:length(myjson2()$P) ) {
      for (k in 1: length(seq(0,1,by=0.1)) )  {
        xticks[i,k]=   xvaluesb[i]-(input$boxwidth/2)-(input$boxwidth/6)
        yticks[i,k]=   yvaluesb[i]-(input$boxheight/2)+ k*(input$boxheight/10)- input$boxheight/10
      }
    }
    
    plotit_prob<-function(){
      
      plot(c(0, 1),c(0, 1),
           type = "n" , ylab = "",
           xlab='', xaxt='n',
           yaxt='n', pch=30)
      
      
      text(0.05, 1, paste0("At time"," ",input$perc),cex = input$textsizep_msm/10)
      legend(0.85, 1.0, labels_cov(), fill =labels_colour_cov(), cex = input$textsizep_msm/10)
      
      ### Call the box function
      recttext(xcenter=xvaluesb, ycenter=yvaluesb, 
               boxwidth=input$boxwidth, boxheight=input$boxheight,statename=c(rep("",nstates)),
               freq_box=c(rep("",nstates)),
               rectArgs = list(col = 'white', lty = 'solid'),
               textArgs_state = list(col = input$boxcolornames, cex = input$textsizep_msm/10,pos=3),
               textArgs_freq  = list(col = input$boxcolorfreqs, cex = input$textsizep_msm/10,pos=1))
      
      ### Call the arrows function
      arrows_msm(xstart=x1, ystart=y1,xend=x2, yend=y2,xtext=c(rep("",ntransitions)), ytext=c(rep("",ntransitions)),tname=c(rep("",ntransitions)),
                 tfreq=c(rep("",ntransitions)),
                 textArgs_transname =list(col =input$arrowcolornames, cex = input$textsizep_msm/10, pos=3), 
                 textArgs_transfreq =list(col = input$arrowcolorfreqs, cex = input$textsizep_msm/10, pos=1), arrowcol=input$arrowcolour,lty = 1)
      
       for (i in 1:length(myjson2()$P) ) {
         for (j in 1:length(myjson2()$atlist) ) {
           
           rect(xleft = xleft[i,j], ybottom = ybottom[i,j],
                xright = xright[i,j], ytop = ytop[i,j], 
                col= labels_colour_cov()[j])
         }
       }
      for (i in 1:length(myjson2()$P) ) {
        for (k in  1: length(seq(0,1,by=0.1)) )  {
          text(x = xticks[i,k], y=yticks[i,k],label=as.character( (k-1)*(input$boxheight/10)*(1/input$boxheight) ),cex =input$textsizep_msm*0.07)
          #  do.call('text', c(list(x = xticks[i,k], y = yticks[i,k], labels = as.character(k)), "black"))
          
        }
      }
    }
    
    z.plot2<-function(){plotit_prob()}
    par(mar=c(0, 0, 0, 0)) 
    p=z.plot2()
    
  }  
  
}
  
})

output$probability_msm_box <- renderPlot ({datap7_re() })

output$downplotp7 <- downloadHandler(
  filename = function(){paste("p7",'.tiff',sep='')},
  content = function(file){
      tiff(file, width = 10, height = 10, units = "cm",res=600)
    
    ntransitions=myjson1()$Ntransitions
    nstates= myjson1()$Nstates
    nats=length(myjson2()$atlist)
    xvaluesb=labels_x()  #+boxwidth/2
    yvaluesb=labels_y()  #-boxheight/2
    
    boxes=msboxes_R_nofreq(yb=yvaluesb, xb=xvaluesb, boxwidth=input$boxwidth , boxheight=input$boxheight, tmat.= myjson1()$tmat)
    #Read through json from msboxes or through a new function 
    x1=boxes$arrows$x1
    y1=boxes$arrows$y1
    x2=boxes$arrows$x2
    y2=boxes$arrows$y2
    
    
    if (nstates!=length(myjson2()$P)) {
      return(h2("The number of states are not corresponding between the 2 uploaded files"))
    }
    if (nstates==length(myjson2()$P) & nats==length(myjson2()$atlist)) {
      
      w1=input$boxwidth/((5*length(myjson2()$atlist))+1)
      w2=4*input$boxwidth/((5*length(myjson2()$atlist))+1)
      
      
      prob=matrix(nrow=nstates, ncol= nats, NA)
      xleft=matrix(nrow=nstates, ncol= nats, NA)
      xright=matrix(nrow=nstates, ncol= nats, NA)
      ybottom=matrix(nrow=nstates, ncol= nats, NA)
      ytop=matrix(nrow=nstates, ncol= nats, NA)
      
      
      for (i in 1:nstates ) {
        for (j in 1:nats ) {
          prob[i,j]=data_P_d()$V[which(data_P_d()$timevar==input$perc & data_P_d()$state==i & data_P_d()$cov_factor==labels_cov()[j])]
          xleft[i,j]=(xvaluesb[i]-input$boxwidth/2)+ j*w1 + (j-1)*w2
          xright[i,j]=(xvaluesb[i]-input$boxwidth/2)+ j*w1 + j*w2
          ybottom[i,j]=(yvaluesb[i]-input$boxheight/2)
          ytop[i,j]=   (yvaluesb[i]-input$boxheight/2)+ prob[i,j]*input$boxheight
          
        }
      }
      
      xticks=matrix(nrow=length(myjson2()$P), ncol=length(seq(0,1,by=0.1)),NA )
      yticks=matrix(nrow=length(myjson2()$P), ncol=length(seq(0,1,by=0.1)),NA )
      
      
      for (i in 1:length(myjson2()$P) ) {
        for (k in 1: length(seq(0,1,by=0.1)) )  {
          xticks[i,k]=   xvaluesb[i]-(input$boxwidth/2)-(input$boxwidth/6)
          yticks[i,k]=   yvaluesb[i]-(input$boxheight/2)+ k*(input$boxheight/10)- input$boxheight/10
        }
      }
      
      plotit_prob<-function(){
        
        plot(c(0, 1),c(0, 1),
             type = "n" , ylab = "",
             xlab='', xaxt='n',
             yaxt='n', pch=30)
        
        
        text(0.05, 1, paste0("At time"," ",input$perc),cex = input$textsizep_msm/10)
        legend(0.85, 1.0, labels_cov(), fill =labels_colour_cov(), cex = input$textsizep_msm/10)
        
        ### Call the box function
        recttext(xcenter=xvaluesb, ycenter=yvaluesb, 
                 boxwidth=input$boxwidth, boxheight=input$boxheight,statename=c(rep("",nstates)),
                 freq_box=c(rep("",nstates)),
                 rectArgs = list(col = 'white', lty = 'solid'),
                 textArgs_state = list(col = input$boxcolornames, cex = input$textsizep_msm/10,pos=3),
                 textArgs_freq  = list(col = input$boxcolorfreqs, cex = input$textsizep_msm/10,pos=1))
        
        ### Call the arrows function
        arrows_msm(xstart=x1, ystart=y1,xend=x2, yend=y2,xtext=c(rep("",ntransitions)), ytext=c(rep("",ntransitions)),tname=c(rep("",ntransitions)),
                   tfreq=c(rep("",ntransitions)),
                   textArgs_transname =list(col =input$arrowcolornames, cex = input$textsizep_msm/10, pos=3), 
                   textArgs_transfreq =list(col = input$arrowcolorfreqs, cex = input$textsizep_msm/10, pos=1), arrowcol=input$arrowcolour,lty = 1)
        
        for (i in 1:length(myjson2()$P) ) {
          for (j in 1:length(myjson2()$atlist) ) {
            
            rect(xleft = xleft[i,j], ybottom = ybottom[i,j],
                 xright = xright[i,j], ytop = ytop[i,j], 
                 col= labels_colour_cov()[j])
          }
        }
        for (i in 1:length(myjson2()$P) ) {
          for (k in  1: length(seq(0,1,by=0.1)) )  {
            text(x = xticks[i,k], y=yticks[i,k],label=as.character( (k-1)*(input$boxheight/10)*(1/input$boxheight) ),cex =input$textsizep_msm*0.07)
            #  do.call('text', c(list(x = xticks[i,k], y = yticks[i,k], labels = as.character(k)), "black"))
            
          }
        }
      }
      
      z.plot2<-function(){plotit_prob()}
      par(mar=c(0, 0, 0, 0)) 
      p=z.plot2()
      dev.off()
    
    }
  }
)

######################################################################################################################################
######################################################################################################################################

data_P_diff1 <- reactive ({
  P_diff=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_diff= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$Pd)) {
      P_diff[[i]]=as.data.frame(t(data.frame(myjson2()$Pd[i])))
      colnames(P_diff[[i]]) <- v_diff
    }
    for(i in 1:length(myjson2()$Pd)) {  
      P_diff[[i]]=as.data.frame(cbind(P_diff[[i]], timevar ,state=rep(i,nrow(P_diff[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$Pd)) {
      P_diff[[i]]=as.data.frame(myjson2()$Pd[[i]][,1])
      # colnames(P_diff[[i]]) <- v_diff
    }
    for (i in 1:length(myjson2()$Pd)) {  
      P_diff[[i]]=as.data.frame(c(P_diff[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$Pd[[i]][,1])) )) )
      colnames(P_diff[[i]])[1:(length(myjson2()$atlist)-1)] <- v_diff
    }
  }
  
  
  # Append the probabilities datasets of the different states
  data_Pd=list()
  data_Pd[[1]]=P_diff[[1]]
  
  if (length(myjson2()$Pd)>1) {
    for (u in 2:(length(myjson2()$Pd))) {
      data_Pd[[u]]=rbind(P_diff[[u]],data_Pd[[(u-1)]])
    }
  }
  
  dataPd=data_Pd[[length(myjson2()$Pd)]]
  dataPd$state_fac=c(rep("NA",nrow(dataPd)))
  
  for (o in 1:(length(myjson2()$Pd))) {
    for (g in 1:nrow(dataPd))  {
      if  (dataPd$state[g]==o) {dataPd$state_fac[g]=labels_state()[o]}  
    }
  }
  dataPd
}) 


data_P_diff1_uci <- reactive ({
  P_diff_uci=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_diff= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$Pd_uci)) {
      P_diff_uci[[i]]=as.data.frame(t(data.frame(myjson2()$Pd_uci[i])))
      colnames(P_diff_uci[[i]]) <- v_diff
    }
    for(i in 1:length(myjson2()$Pd_uci)) {  
      P_diff_uci[[i]]=as.data.frame(cbind(P_diff_uci[[i]], timevar ,state=rep(i,nrow(P_diff_uci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$Pd_uci)) {
      P_diff_uci[[i]]=as.data.frame(myjson2()$Pd_uci[[i]][,1])
      # colnames(P_diff[[i]]) <- v_diff
    }
    for (i in 1:length(myjson2()$Pd_uci)) {  
      P_diff_uci[[i]]=as.data.frame(c(P_diff_uci[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$Pd_uci[[i]][,1])) )) )
      colnames(P_diff_uci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_diff
    }
  }
  
  
  # Append the probabilities datasets of the different states
  data_Pd_uci=list()
  data_Pd_uci[[1]]=P_diff_uci[[1]]
  
  if (length(myjson2()$Pd_uci)>1) {
    for (u in 2:(length(myjson2()$Pd_uci))) {
      data_Pd_uci[[u]]=rbind(P_diff_uci[[u]],data_Pd_uci[[(u-1)]])
    }
  }
  
  dataPd_uci=data_Pd_uci[[length(myjson2()$Pd_uci)]]
  dataPd_uci$state_fac=c(rep("NA",nrow(dataPd_uci)))
  
  for (o in 1:(length(myjson2()$Pd_uci))) {
    for (g in 1:nrow(dataPd_uci))  {
      if  (dataPd_uci$state[g]==o) {dataPd_uci$state_fac[g]=labels_state()[o]}  
    }
  }
  dataPd_uci
}) 


data_P_diff1_lci <- reactive ({
  P_diff_lci=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  
  v_diff= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_diff[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$Pd_lci)) {
      P_diff_lci[[i]]=as.data.frame(t(data.frame(myjson2()$Pd_lci[i])))
      colnames(P_diff_lci[[i]]) <- v_diff
    }
    for(i in 1:length(myjson2()$Pd_lci)) {  
      P_diff_lci[[i]]=as.data.frame(cbind(P_diff_lci[[i]], timevar ,state=rep(i,nrow(P_diff_lci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$Pd_lci)) {
      P_diff_lci[[i]]=as.data.frame(myjson2()$Pd_lci[[i]][,1])
      # colnames(P_diff[[i]]) <- v_diff
    }
    for (i in 1:length(myjson2()$Pd_lci)) {  
      P_diff_lci[[i]]=as.data.frame(c(P_diff_lci[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$Pd_lci[[i]][,1])) )) )
      colnames(P_diff_lci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_diff
    }
  }
  
  # Append the probabilities datasets of the different states
  data_Pd_lci=list()
  data_Pd_lci[[1]]=P_diff_lci[[1]]
  
  if (length(myjson2()$Pd_lci)>1) {
    for (u in 2:(length(myjson2()$Pd_lci))) {
      data_Pd_lci[[u]]=rbind(P_diff_lci[[u]],data_Pd_lci[[(u-1)]])
    }
  }
  
  dataPd_lci=data_Pd_lci[[length(myjson2()$Pd_lci)]]
  dataPd_lci$state_fac=c(rep("NA",nrow(dataPd_lci)))
  
  for (o in 1:(length(myjson2()$Pd_lci))) {
    for (g in 1:nrow(dataPd_lci))  {
      if  (dataPd_lci$state[g]==o) {dataPd_lci$state_fac[g]=labels_state()[o]}  
    }
  }
  dataPd_lci
}) 


data_P_diff2<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_P_diff1()[,d],data_P_diff1()[,ncol(data_P_diff1())-2],data_P_diff1()[,ncol(data_P_diff1())-1],
                                data_P_diff1()[,ncol(data_P_diff1())],rep(d,length(data_P_diff1()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_P_diff1())[d],length(data_P_diff1()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_Pd <- bind_rows(dlist, .id = "column_label")
  d_all_Pd
}) 


data_P_diff2_uci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_P_diff1_uci()[,d],data_P_diff1_uci()[,ncol(data_P_diff1_uci())-2],data_P_diff1_uci()[,ncol(data_P_diff1_uci())-1],
                                data_P_diff1_uci()[,ncol(data_P_diff1_uci())],rep(d,length(data_P_diff1_uci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_P_diff1_uci())[d],length(data_P_diff1_uci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_Pd_uci <- bind_rows(dlist, .id = "column_label")
  d_all_Pd_uci
}) 

data_P_diff2_lci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_P_diff1_lci()[,d],data_P_diff1_lci()[,ncol(data_P_diff1_lci())-2],data_P_diff1_lci()[,ncol(data_P_diff1_lci())-1],
                                data_P_diff1_lci()[,ncol(data_P_diff1_lci())],rep(d,length(data_P_diff1_lci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_P_diff1_lci())[d],length(data_P_diff1_lci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_Pd_lci <- bind_rows(dlist, .id = "column_label")
  d_all_Pd_lci
}) 

data_P_diff_ci<- reactive ({
  x=c( data_P_diff2()[order(data_P_diff2()$timevar,data_P_diff2()$state,data_P_diff2()$cov),]$timevar,
       data_P_diff2()[order(-data_P_diff2()$timevar,data_P_diff2()$state,data_P_diff2()$cov),]$timevar )
  
  y_central=c( data_P_diff2()[order(data_P_diff2()$timevar,data_P_diff2()$state,data_P_diff2()$cov),]$V,
               data_P_diff2()[order(-data_P_diff2()$timevar,data_P_diff2()$state,data_P_diff2()$cov),]$V )
  
  y=c( data_P_diff2_uci()[order(data_P_diff2_uci()$timevar,data_P_diff2_uci()$state,data_P_diff2_uci()$cov),]$V,
       data_P_diff2_lci()[order(-data_P_diff2_lci()$timevar,data_P_diff2_lci()$state,data_P_diff2_lci()$cov),]$V )
  
  frameto=c(as.character(data_P_diff2_uci()[order(-data_P_diff2_uci()$timevar,data_P_diff2_uci()$state,data_P_diff2_uci()$cov),]$state_factor),
            as.character(data_P_diff2_lci()[order(-data_P_diff2_lci()$timevar,data_P_diff2_lci()$state,data_P_diff2_lci()$cov),]$state_factor) )
  
  covto=c( data_P_diff2_uci()[order(-data_P_diff2_uci()$timevar,data_P_diff2_uci()$state,data_P_diff2_uci()$cov),]$cov_factor,
           data_P_diff2_lci()[order(-data_P_diff2_lci()$timevar,data_P_diff2_lci()$state,data_P_diff2_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,frameto,covto,y_central)
  data
})

output$table <- renderTable ({ 
  
  data_P_diff1()
}) 


#######################################

#output$shouldloadp8 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotp8", label = h2("Download the plot"))
#})

datap8_re <-  reactive ({

  
  ax <- list( title = "",zeroline = FALSE,showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
  
  if (length(myjson2()$Pd) == 0| myjson2()$Nats==1 ) {             
    P_state_d= plot_ly() %>%
      layout(title=list(text="Not applicable- Only one covariate pattern specified",y=0.95),xaxis=ax, yaxis=ax)
    P_state_d
  } 
  
  else  {
    
    if (input$conf=="ci_no") {
      
      if (input$facet=="No") {
        
        
        P_state_d= plot_ly(data_P_diff2(),alpha=0.5) %>%
          add_lines(
            x=data_P_diff2()$timevar,y=data_P_diff2()$V,
            frame=factor(as.factor(data_P_diff2()$state_factor),levels=labels_state()),
            color=as.factor(data_P_diff2()$cov_factor),
            colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
            mode="lines",
            line=list(simplify=FALSE),
            text = 'Select or deselect lines by clicking on the legend',
            hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                  "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
          )  %>%
          layout(title=list(text="Differences of probabilities among covariate patterns (compared to ref. cov pattern)",y=0.95),
                 font= list(family = "times new roman", size = input$textsizep, color = "black"),
                 margin = list(l = 50, r = 50, b = 30, t = 70),
                 xaxis=list(title=list(text="Time since entry",y=0.2),
                            dtick = input$stepx, 
                            tick0 = input$startx, 
                            range=c(input$startx,input$endx),
                            ticklen = 5,
                            tickwidth = 2,
                            tickcolor = toRGB("black"),
                            tickmode = "linear"),
                 yaxis =list(title= "Differences of probabilities",                    
                             dtick = input$stepy, 
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
        
        
      }
      
      if (input$facet=="Yes") {
        
        
        data_plot=data_P_diff2()
        
        P_state_d = ggplot(data_plot)
        P_state_d = ggplot(data_plot,aes(x=timevar, y=V, color= as.factor(cov_factor), group=1,
                                         text=paste("Select or deselect lines by clicking on the legend",
                                                    "<br>Time: ", timevar,
                                                    "<br>Difference of probabilities: ", V,
                                                    "<br>Covariate pattern: ",  as.factor(cov_factor))))
        
        P_state_d = P_state_d+geom_line(aes(x=timevar, y=V, color= as.factor(cov_factor)))+
          scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
        
        if (input$aimtype=="compare") { P_state_d = P_state_d+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()), nrow=2)}
        
        else if (input$aimtype=="present")   {P_state_d = P_state_d+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()))}  
        
        P_state_d = P_state_d + scale_x_continuous(breaks=c(seq(input$startx,input$endx,by=input$stepx ))) 
        P_state_d = P_state_d + scale_y_continuous(breaks=c(seq(min(data_plot$V[which(!is.na(data_plot$V))]),max(data_plot$V[which(!is.na(data_plot$V))]), by=input$stepy )))
        

        P_state_d = P_state_d +labs(title="Differences of probabilities among covariate patterns (compared to ref. cov pattern)",
                                    x="Time since entry", y="Differences of probabilities")
        
        P_state_d = P_state_d + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
        
        P_state_d = P_state_d +theme(title = element_text(size = input$textsizep-4),    strip.text = element_text(size=input$textfacetp)  ,    
                                     legend.title = element_text(color="black", size= input$textsizep-5), 
                                     legend.text=element_text(size= input$textsizep-6),
                                     plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                     legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                     legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                     axis.title.y = element_text(size= input$textsizep-5),
                                     axis.title.x = element_text(size= input$textsizep-5), 
                                     axis.text.x = element_text( size=input$textsizep-6),axis.text.y = element_text( size=input$textsizep-6)) 
        
        P_state_d = ggplotly(P_state_d, tooltip = "text")%>%
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
        
        P_state_d 
      }
      
    }
    
    else if (input$conf=="ci_yes") {
      
      
      if (input$facet=="No") {
        
        P_state_d <- plot_ly()
        
        P_state_d <- add_trace(P_state_d, line=list(simplify=FALSE),
                               mode="lines", type = "scatter",
                               x=data_P_diff_ci()$x, y=data_P_diff_ci()$y_central,
                               frame=factor(as.factor(data_P_diff_ci()$frameto),levels=labels_state()), 
                               colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                               color=as.factor(data_P_diff_ci()$covto),
                               text = 'Select or deselect lines by clicking on the legend',
                               hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                     "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") )
        
        P_state_d <- add_trace(P_state_d, fill = "tozerox", 
                               line=list(dash = "solid", color = "transparent", width = 1.8897637),
                               mode = "lines", type = "scatter",
                               x=data_P_diff_ci()$x, y=data_P_diff_ci()$y,
                               frame=factor(as.factor(data_P_diff_ci()$frameto),levels=labels_state()), 
                               colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                               color=as.factor(data_P_diff_ci()$covto),
                               showlegend = FALSE,
                               text = 'Select or deselect lines by clicking on the legend',
                               hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                     "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
        
        P_state_d= P_state_d   %>%
          layout(title=list(text="Differences of probabilities among covariate patterns (compared to ref. cov pattern)",y=0.95),
                 font= list(family = "times new roman", size = input$textsizep, color = "black"),
                 margin = list(l = 50, r = 50, b = 30, t = 70),
                 xaxis=list(title=list(text="Time since entry",y=0.2),
                            dtick = input$stepx, 
                            tick0 = input$startx, 
                            range=c(input$startx,input$endx),
                            ticklen = 5,
                            tickwidth = 2,
                            tickcolor = toRGB("black"),
                            tickmode = "linear"),
                 yaxis =list(title= "Differences of probabilities ",                    
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
        
        
      }
      
      if (input$facet=="Yes") {
        
        V_lci=  data_P_diff2_lci()$V
        V_uci=  data_P_diff2_uci()$V
        
        data_plot=cbind(data_P_diff2(),V_lci,V_uci)
        
        
        P_state_d=ggplot(data_plot)
        P_state_d=ggplot(data_plot,aes(x=timevar, y=V, color= as.factor(cov_factor), group=1,
                                       text=paste("Select or deselect lines by clicking on the legend",
                                                  "<br>Time: ", timevar,
                                                  "<br>Difference of probabilities: ", V,
                                                  "<br>Covariate pattern: ",  as.factor(cov_factor))))
          P_state_d=P_state_d+ scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
        
        
        P_state_d=P_state_d+geom_line(aes(x=timevar, y=V, fill= as.factor(cov_factor)))
        
        P_state_d=P_state_d+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill=as.factor(cov_factor)),alpha=0.4)+ 
          scale_fill_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
        
        if (input$aimtype=="compare") { P_state_d = P_state_d+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()), nrow=2)}
        
        else if (input$aimtype=="present")   {P_state_d = P_state_d+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()))}  
        
        
        P_state_d = P_state_d + scale_x_continuous(breaks=c(seq(input$startx,input$endx,by=input$stepx ))) 
        P_state_d = P_state_d + scale_y_continuous(breaks=c(seq(min(data_plot$V_lci[which(!is.na(data_plot$V_lci))]),max(data_plot$V_uci[which(!is.na(data_plot$V_uci))]),by=input$stepy )))
        

        
        P_state_d = P_state_d +labs(title="Differences of probabilities among covariate patterns (compared to ref. cov pattern)",
                                    x="Time since entry", y="Differences of probabilities")
        
        P_state_d = P_state_d + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
        
        P_state_d = P_state_d +theme(title = element_text(size = input$textsizep-4),  strip.text = element_text(size=input$textfacetp),     
                                     legend.title = element_text(color="black", size= input$textsizep-5), 
                                     legend.text=element_text(size= input$textsizep-6),
                                     plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                     legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                     legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                     axis.title.y = element_text(size= input$textsizep-5),
                                     axis.title.x = element_text(size= input$textsizep-5), 
                                     axis.text.x = element_text( size=input$textsizep-6),axis.text.y = element_text( size=input$textsizep-6)) 
        
        P_state_d = ggplotly(P_state_d, tooltip = "text")%>%
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
    
    
    P_state_d
    
  }
}) 

output$P_diff <- renderPlotly ({ datap8_re() })

output$downplotp8 <- downloadHandler(
  filename = function(){paste("p8",'.png',sep='')},
  content = function(file){
    plotly_IMAGE( datap8_re(),width = 3000, height = 3000, format = "png", scale = 1,  out_file = file )
  }
)

##################################################
data_P_ratio1 <- reactive ({
  P_ratio=list()
  
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_ratio= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_ratio[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$Pr)) {
      P_ratio[[i]]=as.data.frame(t(data.frame(myjson2()$Pr[i])))
      colnames(P_ratio[[i]]) <- v_ratio
    }
    for(i in 1:length(myjson2()$Pr)) {  
      P_ratio[[i]]=as.data.frame(cbind(P_ratio[[i]], timevar ,state=rep(i,nrow(P_ratio[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$Pr)) {
      P_ratio[[i]]=as.data.frame(myjson2()$Pr[[i]][,1])
      # colnames(P_ratio[[i]]) <- v_ratio
    }
    for (i in 1:length(myjson2()$Pr)) {  
      P_ratio[[i]]=as.data.frame(c(P_ratio[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$Pr[[i]][,1])) )) )
      colnames(P_ratio[[i]])[1:(length(myjson2()$atlist)-1)] <- v_ratio
    }
  }
  
  # Append the probabilities datasets of the different states
  data_Pr=list()
  data_Pr[[1]]=P_ratio[[1]]
  
  if (length(myjson2()$Pr)>1) {
    for (u in 2:(length(myjson2()$Pr))) {
      data_Pr[[u]]=rbind(P_ratio[[u]],data_Pr[[(u-1)]])
    }
  }
  
  dataPr=data_Pr[[length(myjson2()$Pr)]]
  dataPr$state_fac=c(rep("NA",nrow(dataPr)))
  
  for (o in 1:(length(myjson2()$Pr))) {
    for (g in 1:nrow(dataPr))  {
      if  (dataPr$state[g]==o) {dataPr$state_fac[g]=labels_state()[o]}  
    }
  }
  dataPr
}) 

data_P_ratio1_uci <- reactive ({
  P_ratio_uci=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_ratio= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_ratio[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$Pr_uci)) {
      P_ratio_uci[[i]]=as.data.frame(t(data.frame(myjson2()$Pr_uci[i])))
      colnames(P_ratio_uci[[i]]) <- v_ratio
    }
    for(i in 1:length(myjson2()$Pr_uci)) {  
      P_ratio_uci[[i]]=as.data.frame(cbind(P_ratio_uci[[i]], timevar ,state=rep(i,nrow(P_ratio_uci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$Pr_uci)) {
      P_ratio_uci[[i]]=as.data.frame(myjson2()$Pr_uci[[i]][,1])
      # colnames(P_ratio[[i]]) <- v_ratio
    }
    for (i in 1:length(myjson2()$Pr_uci)) {  
      P_ratio_uci[[i]]=as.data.frame(c(P_ratio_uci[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$Pr_uci[[i]][,1])) )) )
      colnames(P_ratio_uci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_ratio
    }
  }
  
  # Append the probabilities datasets of the ratioerent states
  data_Pr_uci=list()
  data_Pr_uci[[1]]=P_ratio_uci[[1]]
  
  if (length(myjson2()$Pr_uci)>1) {
    for (u in 2:(length(myjson2()$Pr_uci))) {
      data_Pr_uci[[u]]=rbind(P_ratio_uci[[u]],data_Pr_uci[[(u-1)]])
    }
  }
  
  dataPr_uci=data_Pr_uci[[length(myjson2()$Pr_uci)]]
  dataPr_uci$state_fac=c(rep("NA",nrow(dataPr_uci)))
  
  for (o in 1:(length(myjson2()$Pr_uci))) {
    for (g in 1:nrow(dataPr_uci))  {
      if  (dataPr_uci$state[g]==o) {dataPr_uci$state_fac[g]=labels_state()[o]}  
    }
  }
  dataPr_uci
}) 

data_P_ratio1_lci <- reactive ({
  P_ratio_lci=list()
  timevar=as.data.frame(myjson2()$timevar)
  names(timevar)[1]<- "timevar"
  
  v_ratio= vector()
  if (length(myjson2()$atlist)>1) {
    for (i in 2:length(myjson2()$atlist)) {
      v_ratio[i-1]= paste0(labels_cov()[i]," vs ",labels_cov()[1])
    }
  }
  
  if (length(myjson2()$atlist)>2) {
    for(i in 1:length(myjson2()$Pr_lci)) {
      P_ratio_lci[[i]]=as.data.frame(t(data.frame(myjson2()$Pr_lci[i])))
      colnames(P_ratio_lci[[i]]) <- v_ratio
    }
    for(i in 1:length(myjson2()$Pr_lci)) {  
      P_ratio_lci[[i]]=as.data.frame(cbind(P_ratio_lci[[i]], timevar ,state=rep(i,nrow(P_ratio_lci[[i]] )) ))
    }
  }
  
  else  {
    for (i in 1:length(myjson2()$Pr_lci)) {
      P_ratio_lci[[i]]=as.data.frame(myjson2()$Pr_lci[[i]][,1])
      # colnames(P_ratio[[i]]) <- v_ratio
    }
    for (i in 1:length(myjson2()$Pr_lci)) {  
      P_ratio_lci[[i]]=as.data.frame(c(P_ratio_lci[[i]], timevar ,state=rep(i,ncol(as.data.frame(myjson2()$Pr_lci[[i]][,1])) )) )
      colnames(P_ratio_lci[[i]])[1:(length(myjson2()$atlist)-1)] <- v_ratio
    }
  }
  
  # Append the probabilities datasets of the ratioerent states
  data_Pr_lci=list()
  data_Pr_lci[[1]]=P_ratio_lci[[1]]
  
  
  if (length(myjson2()$Pr_lci)>1) {
    for (u in 2:(length(myjson2()$Pr_lci))) {
      data_Pr_lci[[u]]=rbind(P_ratio_lci[[u]],data_Pr_lci[[(u-1)]])
    }
  }
  dataPr_lci=data_Pr_lci[[length(myjson2()$Pr_lci)]]
  dataPr_lci$state_fac=c(rep("NA",nrow(dataPr_lci)))
  
  for (o in 1:(length(myjson2()$Pr_lci))) {
    for (g in 1:nrow(dataPr_lci))  {
      if  (dataPr_lci$state[g]==o) {dataPr_lci$state_fac[g]=labels_state()[o]}  
    }
  }
  dataPr_lci
})

data_P_ratio2<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_P_ratio1()[,d],data_P_ratio1()[,ncol(data_P_ratio1())-2],data_P_ratio1()[,ncol(data_P_ratio1())-1],
                                data_P_ratio1()[,ncol(data_P_ratio1())],rep(d,length(data_P_ratio1()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_P_ratio1())[d],length(data_P_ratio1()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_Pr <- bind_rows(dlist, .id = "column_label")
  d_all_Pr
}) 

data_P_ratio2_uci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_P_ratio1_uci()[,d],data_P_ratio1_uci()[,ncol(data_P_ratio1_uci())-2],
                                data_P_ratio1_uci()[,ncol(data_P_ratio1_uci())-1],
                                data_P_ratio1_uci()[,ncol(data_P_ratio1_uci())],rep(d,length(data_P_ratio1_uci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_P_ratio1_uci())[d],length(data_P_ratio1_uci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_Pd_uci <- bind_rows(dlist, .id = "column_label")
  d_all_Pd_uci
}) 

data_P_ratio2_lci<- reactive ({
  dlist=list()
  for (d in 1:(length(myjson2()$cov$atlist)-1)) {
    
    dlist[[d]]=cbind.data.frame(data_P_ratio1_lci()[,d],data_P_ratio1_lci()[,ncol(data_P_ratio1_lci())-2],
                                data_P_ratio1_lci()[,ncol(data_P_ratio1_lci())-1],
                                data_P_ratio1_lci()[,ncol(data_P_ratio1_lci())],rep(d,length(data_P_ratio1_lci()[,d])) )
    dlist[[d]][,6] <- rep(colnames(data_P_ratio1_lci())[d],length(data_P_ratio1_lci()[,d]))
    colnames(dlist[[d]]) <- c("V","timevar","state","state_factor","cov","cov_factor")
  }
  
  d_all_Pd_lci <- bind_rows(dlist, .id = "column_label")
  d_all_Pd_lci
}) 

data_P_ratio_ci<- reactive ({
  x=c( data_P_ratio2()[order(data_P_ratio2()$timevar,data_P_ratio2()$state,data_P_ratio2()$cov),]$timevar,
       data_P_ratio2()[order(-data_P_ratio2()$timevar,data_P_ratio2()$state,data_P_ratio2()$cov),]$timevar )
  
  y_central=c( data_P_ratio2()[order(data_P_ratio2()$timevar,data_P_ratio2()$state,data_P_ratio2()$cov),]$V,
               data_P_ratio2()[order(-data_P_ratio2()$timevar,data_P_ratio2()$state,data_P_ratio2()$cov),]$V )
  
  y=c( data_P_ratio2_uci()[order(data_P_ratio2_uci()$timevar,data_P_ratio2_uci()$state,data_P_ratio2_uci()$cov),]$V,
       data_P_ratio2_lci()[order(-data_P_ratio2_lci()$timevar,data_P_ratio2_lci()$state,data_P_ratio2_lci()$cov),]$V )
  
  frameto=c(as.character(data_P_ratio2_uci()[order(-data_P_ratio2_uci()$timevar,data_P_ratio2_uci()$state,data_P_ratio2_uci()$cov),]$state_factor),
            as.character(data_P_ratio2_lci()[order(-data_P_ratio2_lci()$timevar,data_P_ratio2_lci()$state,data_P_ratio2_lci()$cov),]$state_factor) )
  
  covto=c( data_P_ratio2_uci()[order(-data_P_ratio2_uci()$timevar,data_P_ratio2_uci()$state,data_P_ratio2_uci()$cov),]$cov_factor,
           data_P_ratio2_lci()[order(-data_P_ratio2_lci()$timevar,data_P_ratio2_lci()$state,data_P_ratio2_lci()$cov),]$cov_factor )
  
  data=data.frame(x,y,frameto,covto,y_central)
  data
})


#output$shouldloadp9 <- renderUI({
#  if (is.null((myjson2())))  return()
#  downloadButton(outputId = "downplotp9", label = h2("Download the plot"))
#})

datap9_re <-  reactive ({
  
  ax <- list( title = "",zeroline = FALSE,showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
  
  if (length(myjson2()$Pr) == 0| myjson2()$Nats==1 ) {             
    P_state_r= plot_ly() %>%
      layout(title=list(text="Not applicable- Only one covariate pattern specified",y=0.95),xaxis=ax, yaxis=ax)
    P_state_r
  } 
  
  else  {
    
    if (input$conf=="ci_no") {
      
      if (input$facet=="No") {
        
        
        P_state_r= plot_ly(data_P_ratio2(),alpha=0.5) %>%
          add_lines(
            x=data_P_ratio2()$timevar,y=data_P_ratio2()$V,
            frame=factor(as.factor(data_P_ratio2()$state_factor),levels=labels_state()),
            color=as.factor(data_P_ratio2()$cov_factor),
            colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
            mode="lines",
            line=list(simplify=FALSE),
            text = 'Select or deselect lines by clicking on the legend',
            hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                  "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>")
          )  %>%
          layout(title=list(text="Ratios of probabilities among covariate patterns (compared to ref. cov pattern)",y=0.95),
                 font= list(family = "times new roman", size = input$textsizep, color = "black"),
                 margin = list(l = 50, r = 50, b = 30, t = 70),
                 xaxis=list(title=list(text="Time since entry",y=0.2),
                            dtick = input$stepx, 
                            tick0 = input$startx, 
                            range=c(input$startx,input$endx),
                            ticklen = 5,
                            tickwidth = 2,
                            tickcolor = toRGB("black"),
                            tickmode = "linear"),
                 yaxis =list(title= "Ratios in probabilities",                    
                             dtick = input$stepy, 
                             ticklen = 5,
                             tickwidth = 2,
                             tickcolor = toRGB("black")),
                 shapes = list(
                   list(type = "rect",
                        fillcolor = "grey", 
                        line = list(color = "grey"), 
                        opacity = 0.8,
                        x0 = 0, x1 =input$area, xref = "x", y0 = 0, y1 = 1, yref = "y") )
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
        
        
      }
      
      if (input$facet=="Yes") {
        
        
        data_plot=data_P_ratio2()
        
        P_state_r = ggplot(data_plot)
        P_state_r = ggplot(data_plot,aes(x=timevar, y=V, color= as.factor(cov_factor), group=1,
                                         text=paste("Select or deselect lines by clicking on the legend",
                                                    "<br>Time: ", timevar,
                                                    "<br>Ratio of probability: ", V,
                                                    "<br>Covariate pattern: ",  as.factor(cov_factor))))
        
        P_state_r = P_state_r+geom_line(aes(x=timevar, y=V, color= as.factor(cov_factor)))+
          scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
        
        if (input$aimtype=="compare") { P_state_r = P_state_r+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()), nrow=2)}
        
        else if (input$aimtype=="present")   {P_state_r = P_state_r+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()))}  
        
        P_state_r = P_state_r + scale_x_continuous(breaks=c(seq(input$startx,input$endx,by=input$stepx  )))

        P_state_r = P_state_r + scale_y_continuous(breaks=c(seq(min(data_plot$V[which(!is.na(data_plot$V))]),max(data_plot$V[which(!is.na(data_plot$V))]), by=input$stepy )))
       
        P_state_r = P_state_r +labs(title="Ratios of probabilities among covariate patterns (compared to ref. cov pattern)", x="Time since entry", y="Ratios of probabilities")
        
        P_state_r = P_state_r + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
        
        P_state_r = P_state_r +theme(title = element_text(size = input$textsizep-4),  strip.text = element_text(size=input$textfacetp),        
                                     legend.title = element_text(color="black", size= input$textsizep-5), 
                                     legend.text=element_text(size= input$textsizep-6),
                                     plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                     legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                     legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                     axis.title.y = element_text(size= input$textsizep-5),
                                     axis.title.x = element_text(size= input$textsizep-5), 
                                     axis.text.x = element_text( size=input$textsizep-6),axis.text.y = element_text( size=input$textsizep-6)) 
        
        P_state_r = ggplotly(P_state_r, tooltip = "text")%>%
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
        
        P_state_r 
      }
      
    }
    
    else if (input$conf=="ci_yes") {
      
      
      if (input$facet=="No") {
        
        P_state_r <- plot_ly()
        
        P_state_r <- add_trace(P_state_r, line=list(simplify=FALSE),
                               mode="lines", type = "scatter",
                               x=data_P_ratio_ci()$x, y=data_P_ratio_ci()$y_central,
                               frame=factor(as.factor(data_P_ratio_ci()$frameto), levels=labels_state()),
                               colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                               color=as.factor(data_P_ratio_ci()$covto),
                               text = 'Select or deselect lines by clicking on the legend',
                               hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                     "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>") )
        
        P_state_r <- add_trace(P_state_r, fill = "tozerox", 
                               line=list(dash = "solid", color = "transparent", width = 1.8897637),
                               mode = "lines", type = "scatter",
                               x=data_P_ratio_ci()$x, y=data_P_ratio_ci()$y,
                               frame=factor(as.factor(data_P_ratio_ci()$frameto), levels=labels_state()),
                               colors=labels_colour_cov()[1:length(myjson2()$cov$atlist)-1],
                               color=as.factor(data_P_ratio_ci()$covto),
                               showlegend = FALSE,
                               text = 'Select or deselect lines by clicking on the legend',
                               hovertemplate = paste("<b>%{text}</b><br><br>", "%{yaxis.title.text}: %{y:,}<br>",
                                                     "%{xaxis.title.text}: %{x:,}<br>","<extra></extra>"))
        
        P_state_r= P_state_r   %>%
          layout(title=list(text="Ratios of probabilities among covariate patterns (compared to ref. cov pattern)",y=0.95),
                 font= list(family = "times new roman", size = input$textsizep, color = "black"),
                 margin = list(l = 50, r = 50, b = 30, t = 70),
                 xaxis=list(title=list(text="Time since entry",y=0.2),
                            dtick = input$stepx, 
                            tick0 = input$startx, 
                            range=c(input$startx,input$endx),
                            ticklen = 5,
                            tickwidth = 2,
                            tickcolor = toRGB("black"),
                            tickmode = "linear"),
                 yaxis =list(title= "Ratios of probabilities",                    
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
        
        
      }
      
      if (input$facet=="Yes") {
        
        V_lci=  data_P_ratio2_lci()$V
        V_uci=  data_P_ratio2_uci()$V
        
        data_plot=cbind(data_P_ratio2(),V_lci,V_uci)
        
        
        P_state_r=ggplot(data_plot)
        P_state_r=ggplot(data_plot,aes(x=timevar, y=V, color= as.factor(cov_factor), group=1,
                                       text=paste("Select or deselect lines by clicking on the legend",
                                                  "<br>Time: ", timevar,
                                                  "<br>Ratio of probability: ", V,
                                                  "<br>Covariate pattern: ",  as.factor(cov_factor))))+
          scale_colour_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
        
        
        P_state_r=P_state_r+geom_line(aes(x=timevar, y=V, fill= as.factor(cov_factor)))
        
        P_state_r=P_state_r+ geom_ribbon(aes(ymin = V_lci, ymax =V_uci,fill=as.factor(cov_factor)),alpha=0.4)+ 
          scale_fill_manual( values =labels_colour_cov(),labels = labels_cov()  ) 
        
        if (input$aimtype=="compare") { P_state_r = P_state_r+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()), nrow=2)}
        
        else if (input$aimtype=="present")   {P_state_r = P_state_r+ facet_wrap(~factor(as.factor(state_factor),levels=labels_state()))}  
        
        
        P_state_r = P_state_r + scale_x_continuous(breaks=c(seq(input$startx,input$endx,by=input$stepx  ))) 
        
         P_state_r = P_state_r + scale_y_continuous(breaks=c(seq(min(data_plot$V_lci[which(!is.na(data_plot$V_lci))]),max(data_plot$V_uci[which(!is.na(data_plot$V_uci))]),by=input$stepy )))
        
        P_state_r = P_state_r +labs(title="Ratios of probabilities among covariate patterns (compared to ref. cov pattern)", x="Time since entry", y="Ratios of probabilities")
        
        P_state_r = P_state_r + labs(color = "Covariate\npatterns")+ labs(fill = "Covariate\npatterns")
        
        P_state_r = P_state_r +theme(title = element_text(size = input$textsizep-4),   strip.text = element_text(size=input$textfacetp), 
                                     legend.title = element_text(color="black", size= input$textsizep-5), 
                                     legend.text=element_text(size= input$textsizep-6),
                                     plot.margin = unit(x=c(1.5,1.5,1.5,1.5),units="cm"),
                                     legend.margin = margin(1.5, 1, 1, 1, "cm"),
                                     legend.justification = "center",legend.box.spacing = unit(0.2, "cm"),
                                     axis.title.y = element_text(size= input$textsizep-5),
                                     axis.title.x = element_text(size= input$textsizep-5), 
                                     axis.text.x = element_text( size=input$textsizep-6),axis.text.y = element_text( size=input$textsizep-6)) 
        
        P_state_r = ggplotly(P_state_r, tooltip = "text")%>%
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
    
    
    P_state_r
  }
  
}) 

output$P_ratio <- renderPlotly ({datap9_re() })
                                
output$downplotp9 <- downloadHandler(
  filename = function(){paste("p9",'.png',sep='')},
  content = function(file){
    plotly_IMAGE( datap9_re(),width = 3000, height = 3000, format = "png", scale = 1,  out_file = file )
  }
)




