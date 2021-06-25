

tabPanel( h1("Interpretation"),
          
          fluidRow(                  
            
           tags$style(HTML(".radio-inline { margin-left: 25px !important;}")),

          

        
            column(2,
                   h1("Interepretations"),
                   "What are the interpretations of measures? ",
                   
                   checkboxGroupInput(inputId = "measures", label = "Measures",
                                      choices =  c("Probability"="prob", "Transition intensity"="trans",
                                                   "Length of stay"="los", 
                                                    "Differences and ratios"="comp","Extra measures"="extram",
                                                    "Robustness"="robust"),
                                      selected= c("prob","trans","los","vis","comp","extram","robust" ))
                   
                  # "Probability of visiting a state"="vis",
                                      
                   #radioButtons("int_prob", label= "Transition probabilities",
                   #             choices=c("Yes","No"),  selected = "No"),
                   #radioButtons("int_trans", label= "Transition intensities",
                   #             choices=c("Yes","No"),selected = "No"),
                   #radioButtons("int_los", label= "Length of stay",
                   #             choices=c("Yes","No"), selected = "No"),
                   #radioButtons("int_vis", label= "Probability of ever visiting a state",
                   #             choices=c("Yes","No"),  selected = "No"),
                   #
                   #h1("Robustness of measures"),
                   #"Measures robustness to the different approaches (clock forward, clock reset)?",
                   #radioButtons("int_rob", label= "Show",
                   #             choices=c("Yes","No"), selected = "No"),
            ),
            
            column(10,
                   uiOutput("interpret"),
                   uiOutput("message_prob"),
                   uiOutput("message_trans"),
                   uiOutput("message_los"),
                   #uiOutput("message_vis"),
                   uiOutput("message_comp"),
                   uiOutput("message_extram"),
                   uiOutput("message_robust")
                   
            )
            
            
            
          )
          
)
