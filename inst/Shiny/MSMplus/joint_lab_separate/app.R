

if (!require(shiny)) install.packages("shiny")

if (!require(mstate)) install.packages("mstate")

if (!require(tidyr)) install.packages("tidyr")

if (!require(ggplot2)) install.packages("ggplot2")

if (!require(DiagrammeR)) install.packages("DiagrammeR")

if (!require(flexsurv)) install.packages("flexsurv")

if (!require(stringr)) install.packages("stringr")

if (!require(dplyr)) install.packages("dplyr")

if (!require(RJSONIO)) install.packages("RJSONIO")

if (!require(gapminder)) install.packages("gapminder")

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


library(webshot)
library(htmlwidgets)
library(raster)
library(plyr)
library(viridis)
library('RJSONIO')
library(ggplot2)
library(jsonlite)
library(dplyr)
library(plotly)
library(reshape2)
library(shiny)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(DT)
library(shiny.semantic)
library(magrittr)
library(cowplot)
library(imager)
library(StatMeasures)


ui <- navbarPage("Multi-State Modelling", fluid = TRUE, inverse=TRUE,
                 
                 source( "joint_lab_separate/dataset/ui.R", local=T)$value,
                 
                 source( "joint_lab_separate/box_inter/ui_inter_alter.R", local=T)$value,
                 
                 source( "joint_lab_separate/predict/ui_p.R", local=T)$value,
                 
                 source( "joint_lab_separate/predict/ui_h.R", local=T)$value,
                 
                 source( "joint_lab_separate/predict/ui_los.R", local=T)$value,
                 
                 source( "joint_lab_separate/predict/ui_visit.R", local=T)$value,
                 
                source( "joint_lab_separate/predict/ui_user.R", local=T)$value
                 
)



server <- function(input, output, session) {
  
  #Functions
  ### Function deriving the frequencies over time
  source("joint_lab_separate/function/freq_total.R",local=T)$value
  
  ### Function that will take the input of x and y's and produce the arrows
  source("joint_lab_separate/function/msboxes_R_nofreq.R",local=T)$value
  
  require(grDevices)
  ### Function to give the boxes and the text inside the boxes
  recttext <- function(xcenter, ycenter, boxwidth, boxheight, statename,freq_box, 
                       rectArgs = "white", textArgs_state = "black", textArgs_freq="black") {
    center=list()
    center[[1]] <- xcenter
    center[[2]] <- ycenter
    do.call('rect', c(list(xleft = xcenter-(boxwidth/2), ybottom = ycenter-(boxheight/2),
                           xright = xcenter+(boxwidth/2), ytop =  ycenter+(boxheight/2)), rectArgs))
    do.call('text', c(list(x = center[[1]], y = center[[2]], labels = statename), textArgs_state))
    do.call('text', c(list(x = center[[1]], y = center[[2]], labels = freq_box), textArgs_freq))
  }
  ### Function to give the arrows and the text on top of the arrows
  arrows_msm<-function(xstart, ystart, xend, yend, xtext, ytext, tname, tfreq,
                       textArgs_transname = "black",textArgs_transfreq = "black", arrowcol="red",lty = 1) {
    
    do.call('arrows', c(list(x0=xstart,y0=ystart, x1=xend,y1=yend,col=arrowcol,lty=lty)))
    do.call('text',   c(list(x = xtext, y = ytext, labels = tname), textArgs_transname))
    do.call('text',   c(list(x = xtext, y = ytext, labels = tfreq), textArgs_transfreq))
  }
  
  #Create reactive msset dataset
  myjson1 <- reactive ({
    if (is.null(input$json1))
      return()
    
    data= fromJSON(input$json1$datapath, flatten=TRUE)
    data
  })
  
  
  #Create reactive json dataset
  myjson2 <- reactive ({
    if (is.null(input$json2))
      return()
    
    data= fromJSON(input$json2$datapath, flatten=TRUE)
    
    
    
    ##Save all probabilities as a separate list
    cond_P<-which(startsWith(names(data), 'P') & !startsWith(names(data), 'P_diff') & !startsWith(names(data),'P_ratio') &
                    !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_P_uci<-which(startsWith(names(data), 'P') & !startsWith(names(data), 'P_diff') & !startsWith(names(data),'P_ratio') &
                        endsWith(names(data), 'uci' ))
    
    cond_P_lci<-which(startsWith(names(data), 'P') & !startsWith(names(data), 'P_diff') & !startsWith(names(data),'P_ratio') &
                        endsWith(names(data), 'lci' ))
    
    cond_P_diff<- which(startsWith(names(data), 'P_diff') & !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  )
    cond_P_diff_uci<- which(startsWith(names(data), 'P_diff') & endsWith(names(data), 'uci' )  )
    cond_P_diff_lci<- which(startsWith(names(data), 'P_diff') & endsWith(names(data), 'lci' )  )
    
    cond_P_ratio<- which(startsWith(names(data), 'P_ratio') & !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  )
    cond_P_ratio_uci<- which(startsWith(names(data), 'P_ratio') & endsWith(names(data), 'uci' )  )
    cond_P_ratio_lci<- which(startsWith(names(data), 'P_ratio') & endsWith(names(data), 'lci' )  )
    
    
    data$P=data[cond_P]
    data$P_uci=data[cond_P_uci]
    data$P_lci=data[cond_P_lci] 
    
    data$Pd=data[cond_P_diff]
    data$Pd_uci=data[cond_P_diff_uci]
    data$Pd_lci=data[cond_P_diff_lci]
    
    data$Pr=data[cond_P_ratio]
    data$Pr_uci=data[cond_P_ratio_uci]
    data$Pr_lci=data[cond_P_ratio_lci]
    
    
    ##Save all transistion intensities as a separate list
    cond_h<-which(startsWith(names(data), 'h'))
    data$h=data[cond_h]
    
    ##Save all los as a separate list
    cond_los<-which(startsWith(names(data), 'Los') & !startsWith(names(data), 'Los_diff') & !startsWith(names(data),'Los_ratio')  &
                      !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_los_uci<-which(startsWith(names(data), 'Los') & !startsWith(names(data), 'Los_diff') & !startsWith(names(data),'Los_ratio')  &
                          endsWith(names(data), 'uci' ) )
    
    cond_los_lci<-which(startsWith(names(data), 'Los') & !startsWith(names(data), 'Los_diff') & !startsWith(names(data),'Los_ratio')  &
                          endsWith(names(data), 'lci' ) )
    
    cond_los_diff<- which(startsWith(names(data), 'Los_diff') & !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci' ) )
    cond_los_diff_uci<- which(startsWith(names(data), 'Los_diff') & endsWith(names(data), 'uci' ) )
    cond_los_diff_lci<- which(startsWith(names(data), 'Los_diff') & endsWith(names(data), 'lci' ) )
    
    
    cond_los_ratio<- which(startsWith(names(data), 'Los_ratio') & !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci' ) )
    cond_los_ratio_uci<- which(startsWith(names(data), 'Los_ratio') & endsWith(names(data), 'uci' ) )
    cond_los_ratio_lci<- which(startsWith(names(data), 'Los_ratio') & endsWith(names(data), 'lci' ) )    
    
    
    data$los=data[cond_los]
    data$los_uci=data[cond_los_uci]
    data$los_lci=data[cond_los_lci]
    
    data$losd=data[cond_los_diff]
    data$losd_uci=data[cond_los_diff_uci]
    data$losd_lci=data[cond_los_diff_lci]
    
    data$losr=data[cond_los_ratio]
    data$losr_uci=data[cond_los_ratio_uci]
    data$losr_lci=data[cond_los_ratio_lci]
    
    
    ##Save all visit as a separate list
    cond_visit<-which(startsWith(names(data), 'Visit') & !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    cond_visit_uci<-which(startsWith(names(data), 'Visit') & endsWith(names(data), 'uci' )  )
    cond_visit_lci<-which(startsWith(names(data), 'Visit') & endsWith(names(data), 'lci' )  )
    
    data$visit=data[cond_visit]
    data$visit_uci=data[cond_visit_uci]
    data$visit_lci=data[cond_visit_lci]
    
    
    ##Save all user as a separate list
    cond_user<-which(startsWith(names(data), 'User') & !startsWith(names(data), 'User_diff') & !startsWith(names(data),'User_ratio')  &
                       !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_user_uci<-which(startsWith(names(data), 'User') & !startsWith(names(data), 'User_diff') & !startsWith(names(data),'User_ratio')  &
                           endsWith(names(data), 'uci' ) )
    
    cond_user_lci<-which(startsWith(names(data), 'User') & !startsWith(names(data), 'User_diff') & !startsWith(names(data),'User_ratio')  &
                           endsWith(names(data), 'lci' ) )
    
    cond_user_diff<- which(startsWith(names(data), 'User_diff') )
    cond_user_diff_uci<- which(startsWith(names(data), 'User_diff') & endsWith(names(data), 'uci' ) )
    cond_user_diff_lci<- which(startsWith(names(data), 'User_diff') & endsWith(names(data), 'lci' ) )
    
    
    cond_user_ratio<- which(startsWith(names(data), 'User_ratio') )
    cond_user_ratio_uci<- which(startsWith(names(data), 'User_ratio') & endsWith(names(data), 'uci' ) )
    cond_user_ratio_lci<- which(startsWith(names(data), 'User_ratio') & endsWith(names(data), 'lci' ) )   
    
    
    data$user=data[cond_user]; data$user_uci=data[cond_los_uci]; data$user_lci=data[cond_user_lci];
    data$userd=data[cond_user_diff]; data$userd_uci=data[cond_user_diff_uci]; data$userd_lci=data[cond_user_diff_lci];
    data$userr=data[cond_user_ratio];data$userr_uci=data[cond_user_ratio_uci];data$userr_lci=data[cond_user_ratio_lci];
    
    ##The time variable
    data$timevar
    
    ### Number of different covariate patterns specified for prediction
    data$Nats
    
    #### List of covariate patterns
    data$atlist
    
    cond_cov<-which(startsWith(names(data), 'atlis'))
    data$cov=data[cond_cov]
    
    data
  })
  
  
  # Include the logic (server) for each tab
  
  source("joint_lab_separate/function/stacked.R",local=T)$value
  
  source("joint_lab_separate/function/stacked_bars.R",local=T)$value
  
  source("joint_lab_separate/dataset/server.R", local=T)$value
  
  source("joint_lab_separate/box_inter/server_inter.R", local=T)$value
  
  source("joint_lab_separate/predict/server_p.R", local=T)$value
  
  source("joint_lab_separate/predict/server_h.R", local=T)$value
  
  source("joint_lab_separate/predict/server_los.R", local=T)$value
  
  source("joint_lab_separate/predict/server_visit.R", local=T)$value
  
  source("joint_lab_separate/predict/server_user.R", local=T)$value
  
}

shinyApp(ui = ui, server = server)

