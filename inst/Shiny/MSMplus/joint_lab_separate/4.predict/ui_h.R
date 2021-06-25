



tabPanel(id = "mytab_h", value = "mytab_h",
         h1("Hazards"),
          
         useShinyjs(),
         extendShinyjs(text = jscode,functions =c("disableTab","enableTab")),
         tags$style(type="text/css",".nav li a.disabled { background-color: #aaa !important;color: #333 !important;cursor: not-allowed !important;border-color: #aaa !important;}"),
         
          uiOutput("pageh")
              
)
