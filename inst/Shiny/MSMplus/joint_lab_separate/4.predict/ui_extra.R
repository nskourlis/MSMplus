tabPanel(id = "mytab_extra", value = "mytab_extra", h1("Extra"),

         useShinyjs(),
         extendShinyjs(text = jscode,functions =c("disableTab","enableTab")),
         tags$style(type="text/css",".nav li a.disabled { background-color: #aaa !important;color: #333 !important;cursor: not-allowed !important;border-color: #aaa !important;}"),

          uiOutput("page_extra")        
          
)

