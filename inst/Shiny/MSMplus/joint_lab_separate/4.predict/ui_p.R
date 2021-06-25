


tabPanel(   id = "mytab_p", value = "mytab_p",
            includeHTML("joint_lab_separate/0.example_data/tprob.html"),
            useShinyjs(),
            extendShinyjs(text = jscode,functions =c("disableTab","enableTab")),
            tags$style(type="text/css",".nav li a.disabled { background-color: #aaa !important;color: #333 !important;cursor: not-allowed !important;border-color: #aaa !important;}"),
          
           uiOutput("pagep")
           
          
)

