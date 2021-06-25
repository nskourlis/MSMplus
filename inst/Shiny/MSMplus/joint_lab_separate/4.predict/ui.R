tabPanel("Predictions",

           
         sidebarLayout(
           
           sidebarPanel(
             
             h4("Predictions"),
             
             HTML("To derive the graphs from the predictions, a json file with the information from the estimations should be provided. mspredict command in state with option interactive json provides those estimates"),
             
             br(),
             
             fileInput("json2", "Choose Json File", multiple = TRUE, accept = c(".json")),
             uiOutput("covarinput"),
             uiOutput("stateinput")
           ),
             
           mainPanel( verbatimTextOutput('a_out') ,
               tabsetPanel(
                 #tabPanel("ili1",          DT::dataTableOutput("table", height="400px", width="400px")),
                 tabPanel("Probabilities by state",        plotlyOutput("probability_state", height="400px", width="400px")),
                 tabPanel("Probabilities by cov",          plotlyOutput("probability_cov", height="400px", width="400px")),
                 tabPanel("Probabilities by cov and state",plotlyOutput("probability_state_cov", height="400px", width="400px")),
                 tabPanel("Probabilities stacked",         plotlyOutput("probability_both", height="400px", width="400px")),
                 tabPanel("Probabilities bars",            plotlyOutput("probability_bars", height="400px", width="400px")),
                 tabPanel("Probabilities bars stacked",    plotlyOutput("probability_bars_stacked", height="400px", width="400px")),
                 tabPanel("Probabilities bars msm",        plotOutput("probability_msm_box", height="400px", width="400px")),
                 tabPanel("Hazards by state",              plotlyOutput("hazards_state", height="400px", width="400px")),
                 tabPanel("Hazards by cov",                plotlyOutput("hazards_cov", height="400px", width="400px")),
                 tabPanel("Length of stay",       plotOutput("los", height="400px", width="400px")),
                 tabPanel("Probability of visit", plotOutput("visit", height="400px", width="400px"))
               )
             )
           )
         )
   

