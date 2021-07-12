
#if (!require(visNetwork)) install.packages("visNetwork", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require(shiny)) install.packages("shinyjs", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require(shiny)) install.packages("shiny", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
##if (!require(tidyverse)) install.packages("tidyverse")
#
#if (!require(mstate)) install.packages("mstate", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require(tidyr)) install.packages("tidyr", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require(ggplot2)) install.packages("ggplot2", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require(DiagrammeR)) install.packages("DiagrammeR", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require(stringr)) install.packages("stringr", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require(dplyr)) install.packages("dplyr", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require(RJSONIO)) install.packages("RJSONIO", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require(gapminder)) install.packages("gapminder", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require(plyr)) install.packages("plyr", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require(viridis)) install.packages("viridis", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require(cowplot)) install.packages("cowplot", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require(magick)) install.packages("magick", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require(StatMeasures)) install.packages("StatMeasures", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require("processx")) install.packages("processx", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require("webshot")) install.packages("webshot", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require("htmlwidgets")) install.packages("htmlwidgets", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require("raster")) install.packages("raster", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require("jsonlite")) install.packages("jsonlite", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require("devtools")) install.packages("devtools", INSTALL_opts = '--no-lock',dependencies = TRUE)
#if (!require("usethis")) install.packages("usethis", INSTALL_opts = '--no-lock',dependencies = TRUE)
#if (!require("githubinstall")) install.packages("githubinstall", INSTALL_opts = '--no-lock',dependencies = TRUE)
#if (!require("shinyMatrix"))  install.packages("shinyMatrix", INSTALL_opts = '--no-lock',dependencies = TRUE)
#if (!require("dlm"))  install.packages("dlm", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require("rsvg"))  install.packages("rsvg", INSTALL_opts = '--no-lock',dependencies = TRUE)
#if (!require("miniUI"))  install.packages("miniUI", INSTALL_opts = '--no-lock',dependencies = TRUE)
#if (!require("htmltools"))  install.packages("htmltools", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require("webshot"))  install.packages("webshot", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#if (!require("formattable"))  install.packages("formattable", INSTALL_opts = '--no-lock',dependencies = TRUE)
#
#
#if (!require("plotly")) install.packages("plotly")
#
#if (!require("gridExtra")) install.packages("gridExtra")


library(rsvg)
library(miniUI)
library(htmltools)
library("webshot")
library(visNetwork)
library(plotly)
#library(tidyverse)
library(jsonlite)
library(webshot)
library(htmlwidgets)
library(raster)
library(plyr)
library(viridis)
library('RJSONIO')
library(ggplot2)
library(plotly)
library(dplyr)
library(formattable)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(DT)
#library(shiny.semantic)
library(magrittr)
library(cowplot)
library(imager)
library(StatMeasures)
library(shinyMatrix)
library(dlm)
library(gapminder)
library(gridExtra)
library(shinyjs)
library(shiny)



ui <- navbarPage( h1("Supplementary"), fluid = TRUE, inverse=TRUE,theme = "bootstrap2.css",
                  
                  
                  
                 
                  
                  source( "json_structure/ui_json.R", local=T)$value,
                  
                  source( "csv_structure/ui_csv.R", local=T)$value,
                  
                  source( "analysis_examples/ui_stataR.R", local=T)$value
                  
                  
)



server <- function(input, output, session) {
    
    source( "json_structure/server_json.R", local=T)$value
    
    source( "csv_structure/server_csv.R", local=T)$value
    
    source( "analysis_examples/server_stataR.R", local=T)$value
    
}

shinyApp(ui = ui, server = server)

