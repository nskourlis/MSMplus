

tabPanel( h1("Load"),
          fluidRow(                  
            
            tags$head(
            tags$style('body {font-size: 20px;}'),
            tags$style("input[type=checkbox] {transform: scale(2);}"),
            tags$style('input[type=radio] {border: 1px;width: 80%; height: 1em;}'),
            tags$style(type="text/css", "select { width: 400px; }"),
            tags$style(type="text/css", "textarea { max-height: 400px; }"),
            tags$style(type='text/css', ".well { max-height: 400px; }"),
            tags$style(type='text/css', ".span4 { max-height: 400px; }"),
            tags$style(type="text/css", "select.shiny-bound-input { font-size:20px; height:21px;}"),
            tags$style(type="text/css", "input.shiny-bound-input { font-size:20px; height:21px;}"),
            tags$style(type="text/css", "shiny-output-error-myClass  { font-size:20px; height:21px;}"),
            tags$style(HTML(".shiny-output-error-validation {color: green;}" )),
            tags$style(type="text/css", "select { max-width: 150px; max-height: 100px;}"),
            tags$style(type="text/css", "textarea { max-width: 150px; max-height: 100px; }"),
            tags$style(type="text/css", ".jslider { max-width: 200px; max-height: 100px;}"),
            tags$style(type='text/css', ".well { max-width: 200px; max-height: 100px;}"),
            tags$style(type='text/css', ".span4 { max-width: 250px; max-height: 100px;}"),
            tags$style(    ".k-numeric-wrap input {height: 40px;}"),
            tags$style('body {font-size: 20px;}'),
            tags$style(type = "text/css", ".irs-grid-text {font-family: 'arial'; color: black; font-size: 12px;}")
          ),



            column(5,
                   h1("Upload datasets"),
                   "To derive the msboxes graph, a json file with the information on the states and transitions should be provided. msboxes_adjusted command provides the json file",
                   fileInput("json1",  h1("Choose Json File"), accept = c(".json")),
                   "To derive the graphs from the predictions, a json file with the information from the estimations should be provided. mspredict_adjusted command provides those estimates",
                   fileInput("json2",  h1("Choose Json File"), accept = c(".json")),
            ),
            

            column(4,
                   uiOutput("message"),
                   uiOutput("select"),
                   uiOutput("includecov"),
                   uiOutput("selectcov"),
               #    uiOutput("fileob"),
                  uiOutput("fileob2")
                   

                   
            ),
          column(3,
                 uiOutput("colourinput") 
               # verbatimTextOutput("fileob")
          )
            
          )
          
)

