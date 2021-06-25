tabPanel("Predictions for probabilities",
         
         
         sidebarLayout(
           
           sidebarPanel(
             
             h4("Predictions probabilities"),
             
             uiOutput("covarinputratio"),
             uiOutput("stateinputratio")
           ),
           
           mainPanel( 
                      tabsetPanel(
                        tabPanel("Ratios of transition intensities",    plotlyOutput("hazard_tran_ratio", height="500px", width="800px")),
                        tabPanel("Hazard ratio between the covariate patterns",        plotOutput("hazard_cov_ratio", height="400px", width="400px"))
                      )
           )
         )
)
