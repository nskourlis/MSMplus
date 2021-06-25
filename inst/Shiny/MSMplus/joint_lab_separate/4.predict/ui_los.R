tabPanel(   includeHTML("joint_lab_separate/0.example_data/length_of_stay.html"),
            id = "mytab_los", value = "mytab_los",
            useShinyjs(),
            extendShinyjs(text = jscode,functions =c("disableTab","enableTab")),
            tags$style(type="text/css",".nav li a.disabled { background-color: #aaa !important;color: #333 !important;cursor: not-allowed !important;border-color: #aaa !important;}"),
            useShinyjs(),
            extendShinyjs(text = jscode,functions =c("disableTab","enableTab")),
          uiOutput("pagelos")
)
   


