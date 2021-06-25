

tabPanel(  h1("User"),
           id = "mytab_user", value = "mytab_user",
           useShinyjs(),
           extendShinyjs(text = jscode,functions =c("disableTab","enableTab")),
           tags$style(type="text/css",".nav li a.disabled { background-color: #aaa !important;color: #333 !important;cursor: not-allowed !important;border-color: #aaa !important;}"),


                     uiOutput("pageuser")

            )
       




#fluidRow(
#  
#  column(2,
#         #                    conditionalPanel(
#         #                      condition = "!is.null(myjson2()$user)",
#         uiOutput("userinput"),
#         uiOutput("displayu"),
#         
#         uiOutput("covarinputuser")
#         #                                    )
#  ),
#  
#  column(2,
#         #                    conditionalPanel(
#         #                      condition = "!is.null(myjson2()$user)",
#         uiOutput("tickinputuser"),
#         uiOutput("confuser"),
#         
#         #                   )
#         
#  ),
#  
#  column(8,
#         tabsetPanel("A",
#                     #                  conditionalPanel(
#                     #                   condition = "!is.null(myjson2()$user)",
#                     tabPanel (id="user1",h1("User function by cov"),      plotlyOutput("user", height="700px", width = "100%")),
#                     tabPanel(id="user2",h1("User diff by cov"),          plotlyOutput("U_diff", height="600px", width = "100%")),
#                     tabPanel(id="user3",h1("User ratio by cov"),         plotlyOutput("U_ratio", height="600px", width = "100%"))
#                     #                                     )
#         )
#         
#  )
#  
#)