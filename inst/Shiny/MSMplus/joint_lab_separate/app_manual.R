if (!require(visNetwork)) install.packages("visNetwork")

if (!require(shiny)) install.packages("shinyjs")

if (!require(shiny)) install.packages("shiny")

if (!require(mstate)) install.packages("mstate")

if (!require(tidyverse)) install.packages("tidyverse")

if (!require(mstate)) install.packages("mstate")

if (!require(tidyr)) install.packages("tidyr")

if (!require(ggplot2)) install.packages("ggplot2")

if (!require(DiagrammeR)) install.packages("DiagrammeR")

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
library(shiny.semantic)
library(magrittr)
library(cowplot)
library(imager)
library(StatMeasures)

library(shinyMatrix)

ui <- navbarPage( h1("Multi-State"), fluid = TRUE, inverse=TRUE,theme = "bootstrap2.css",
                  
                  
                  
                  source( "C:/Users/niksko/Desktop/mstate2/msm_shiny/joint_lab_separate/ui_manual.R", local=T)$value

                  
                  
)



server <- function(input, output, session) {
  
  source("C:/Users/niksko/Desktop/mstate2/msm_shiny/joint_lab_separate/server_manual.R", local=T)$value
  
  

  
  
}

shinyApp(ui = ui, server = server)
