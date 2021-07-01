

if (!require(shiny)) install.packages("shinyjs")

if (!require(shiny)) install.packages("shiny")

if (!require(mstate)) install.packages("mstate")

if (!require(tidyverse)) install.packages("tidyverse")

if (!require(mstate)) install.packages("mstate")

if (!require(tidyr)) install.packages("tidyr")

if (!require(ggplot2)) install.packages("ggplot2")

if (!require(stringr)) install.packages("stringr")

if (!require(dplyr)) install.packages("dplyr")

if (!require(RJSONIO)) install.packages("RJSONIO")

if (!require(gapminder)) install.packages("gapminder")

if (!require(plyr)) install.packages("plyr")

if (!require(viridis)) install.packages("viridis")

if (!require(cowplot)) install.packages("cowplot")

if (!require(magick)) install.packages("magick")

if (!require(StatMeasures)) install.packages("StatMeasures")

if (!require("processx")) install.packages("processx")

if (!require("webshot")) install.packages("webshot")

if (!require("htmlwidgets")) install.packages("htmlwidgets")

if (!require("raster")) install.packages("raster")

if (!require("jsonlite")) install.packages("jsonlite")

if (!require("devtools")) install.packages("devtools")
if (!require("usethis")) install.packages("usethis")
if (!require("githubinstall")) install.packages("githubinstall")
if (!require("shinyMatrix"))  install.packages("shinyMatrix")
if (!require("dlm"))  install.packages("dlm")

#library("usethis")
#library("devtools")
#library("githubinstall")

if (!require("plotly")) install.packages("plotly")

library(visNetwork)

library(plotly)
library(shinyjs)
library(shiny)

library(tidyverse)

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


ui <- navbarPage( h1("Supplementary"), fluid = TRUE, inverse=TRUE,theme = "bootstrap2.css",
                  
                  
                  
                  source( "csv_structure/ui_csv.R", local=T)$value,
                  
                  source( "json_structure/ui_json.R", local=T)$value,
                  
                  source( "analysis_examples/ui_stataR.R", local=T)$value
                  
                  
)



server <- function(input, output, session) {
    
    source( "json_structure/server_json.R", local=T)$value
    
    source( "csv_structure/server_csv.R", local=T)$value
    
    source( "analysis_examples/server_stataR.R", local=T)$value
    
}

shinyApp(ui = ui, server = server)

