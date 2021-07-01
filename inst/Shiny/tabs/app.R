

if (!require(shiny)) install.packages("shinyjs")

if (!require(shiny)) install.packages("shiny")

if (!require(tidyverse)) install.packages("tidyverse")

if (!require(tidyr)) install.packages("tidyr")

if (!require(ggplot2)) install.packages("ggplot2")

if (!require(stringr)) install.packages("stringr")

if (!require(dplyr)) install.packages("dplyr")

if (!require(RJSONIO)) install.packages("RJSONIO")

if (!require(plyr)) install.packages("plyr")

if (!require(magick)) install.packages("magick")

if (!require("processx")) install.packages("processx")

if (!require("htmlwidgets")) install.packages("htmlwidgets")

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


library(htmlwidgets)

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
library(imager)
library(shinyMatrix)
library(dlm)


ui <- navbarPage( h1("Tutorial"), fluid = TRUE, inverse=TRUE,theme = "bootstrap2.css",
                  
                  source( "use_tutorial/ui_use.R", local=T)$value
                  


                  
                  
)



server <- function(input, output, session) {
    
                source( "use_tutorial/server_use.R", local=T)$value
    

    
}

shinyApp(ui = ui, server = server)

