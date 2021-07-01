tabPanel( h1("Csv"),
          
          fluidRow(                  
          
            
            column(12,uiOutput("to_url_backcsv"),
                   tabsetPanel(
                     tabPanel(h2("Csv file"),
                              uiOutput("message_csv")
                     ),
                     tabPanel(h2("Frequency csv file"),
                              uiOutput("message_csv1"),
                              verbatimTextOutput("csv_file1")
                     ),
                     tabPanel(h2("Results csv file-Naming rules"),  
                              uiOutput("message_csv2"),
                              verbatimTextOutput("csv_file2")
                     ), 
                     tabPanel(h2("Results csv file-Formatting rules"),
                              uiOutput("message_csv3"),
                              verbatimTextOutput("csv_file3")
                     )
#                    tabPanel(h2("Code"),           
#                             uiOutput("message_csv4")
#                    )
                   )
            )
            
            
          )
)