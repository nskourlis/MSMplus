tabPanel( includeHTML("joint_lab_separate/0.example_data/scheme.html"),
   

         uiOutput("pageboxbefore"),
         
         uiOutput("pagebox"),
         
         
         fluidRow(
           tags$head(
             
             tags$style("#frequency {font-size:20px;}"),
             tags$style(HTML(type='number',".irs-grid-text { font-size: 12pt !important; }")),
           ),
           column(6, ),
           column(6,
                  tags$head(  tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
                  uiOutput("frequency"),
                  uiOutput("framebox")
          
           )

       )
)
