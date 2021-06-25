
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
if (!require("dlm"))  install.packages("dlm")

if (!require("rsvg"))  install.packages("rsvg")
if (!require("miniUI"))  install.packages("miniUI")
if (!require("htmltools"))  install.packages("htmltools")

if (!require("webshot"))  install.packages("webshot")


#if ( !require("orca") ) {install.packages("orca")}

#library("usethis")
#library("devtools")
#library("githubinstall")

#if (!require(devtools)) install.packages("devtools")
#devtools::install_github("cpsievert/plotcon17")

if (!require("plotly")) install.packages("plotly")

if (!require("gridExtra")) install.packages("gridExtra")
library("orca")
library(rsvg)
library(miniUI)
library(htmltools)
library("webshot")

library(visNetwork)

library(plotly)


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
library(dlm)
library(gapminder)
library(gridExtra)

library(shinyjs)
library(shiny)

#readRenviron("~/.Renviron")
Sys.setenv("plotly_username" = "niksko")
Sys.setenv("plotly_api_key" = "GCDmoehfftRPyu6TCw80")


jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

css <- '
  .disabled {
  background: #eee !important;
  cursor: default !important;
  color: black !important;
  }'


appCSS <- "

#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;

}

"


ui <- navbarPage(id="tabs_start", h1("MSMplus"),  fluid = TRUE, inverse=TRUE,theme = "bootstrap2.css", 
                  
                  
               
               #tags$style(css),
               #inlineCSS(css),
                  
                  source( "joint_lab_separate/0b.load_options/ui_load_options.R", local=T)$value,
               
                  source( "joint_lab_separate/5.interpretation/ui_interpret.R", local=T)$value,
               
                  
                  source( "joint_lab_separate/1.load/ui_load.R", local=T)$value,
                  
                  source( "joint_lab_separate/2.box/ui_box.R", local=T)$value,
                  
                  source( "joint_lab_separate/3.main_input/ui_input.R", local=T)$value,
                  
                  
                  source( "joint_lab_separate/4.predict/ui_p.R", local=T)$value,
                  
                  source( "joint_lab_separate/4.predict/ui_h.R", local=T)$value,
                  
                  source( "joint_lab_separate/4.predict/ui_los.R", local=T)$value,
                  
                  source( "joint_lab_separate/4.predict/ui_visit.R", local=T)$value,
                  
                  source( "joint_lab_separate/4.predict/ui_user.R", local=T)$value,
                  
                  source( "joint_lab_separate/4.predict/ui_extra.R", local=T)$value
                  
                  
)



server <- function(input, output, session) {
  

  
  
  source( "joint_lab_separate/0b.load_options/server_load_options.R", local=T)$value
  
  source( "joint_lab_separate/1.load/server_load.R", local=T)$value
    
  
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
  
  
  
  source("joint_lab_separate/2.box/server_box.R", local=T)$value
  

  
  
  
  
  
  
  source("joint_lab_separate/3.main_input/server_input.R", local=T)$value
  
  
  
  
  myjson1 <- reactive ({
    
      if (input$loadtype=="json") {
        
         if (input$aimtype=="present") {
      
            if (is.null(input$json1_pr) & input$example=="No")  return()
            
            else if (!is.null(input$json1_pr) & input$example=="No" ) {
              
              data= fromJSON(input$json1_pr$datapath, flatten=TRUE)
              
            }
            
            else if (input$example=="Yes" & is.null(input$json1_pr) )  {  
              data= fromJSON("joint_lab_separate/0.example_data/msboxes.json", flatten=TRUE)
            }
            
            
            
         #   if (is.null(data$msm)==FALSE) {
         #     if  (data$msm!=1) {data$msm_box=0}
         #     else if (data$msm==1)  {data$msm_box=1}
         #   }
         #  if (is.null(data$msm)==TRUE) {data$msm_box=0}
            
            data
         }
        
        
      else if (input$aimtype=="compare") {
          
          if (is.null(input$json1_cp) & input$compare_approach=="No")  return()
          
          else if (!is.null(input$json1_cp) & input$compare_approach=="No" ) {
            
            data= fromJSON(input$json1_cp$datapath, flatten=TRUE)
            
          }
          
          else if (input$compare_approach=="Yes" & is.null(input$json1_cp) )  {  
            data= fromJSON("joint_lab_separate/0.example_data/msboxes.json", flatten=TRUE)
          }
          
          
          
    #      if (is.null(data$msm)==FALSE) {
    #        if  (data$msm!=1) {data$msm_box=0}
    #        else if (data$msm==1)  {data$msm_box=1}
    #       }
    #      if (is.null(data$msm)==TRUE) {data$msm_box=0}
          
          data
      }
    }
    
    
    
    else if (input$loadtype=="csv") {
      
      if (input$aimtype=="present") {
     
        if (length(which(!is.na(input$tmat_input_pr))==TRUE)==0  & input$example2=="No")  return()
     
        else if (input$example2=="No" & length(which(!is.na(input$tmat_input_pr))==TRUE)!=0 ) {
            data=  fromJSON(json1manual(), flatten=TRUE)
        }
     
        else if (input$example2=="Yes"  )  { #& is.null(json1manual())
          data= fromJSON("joint_lab_separate/0.example_data/msboxes.json", flatten=TRUE)
        }
      
        else if (input$example2=="Yes" & length(which(!is.na(input$tmat_input_pr))==TRUE)!=0 ) return("Not both") #& !is.null(json1manual())

     # if (is.null(data$msm)==FALSE) {
     #   if  (data$msm!=1) {data$msm_box=0}
     #   else if (data$msm==1)  {data$msm_box=1}
     # }
    # if (is.null(data$msm)==TRUE) {data$msm_box=0}
      
      data
      }
      
      
      else if (input$aimtype=="compare") {
        
        if (is.null(json1manual()) & input$compare_approach2=="No")  return()
        
        else if (input$compare_approach2=="No" & !is.null(json1manual()) ) {
          data= fromJSON(json1manual(), flatten=TRUE)
        }
        
        else if (input$compare_approach2=="Yes"  )  { #& is.null(json1manual())
          data= fromJSON("joint_lab_separate/0.example_data/msboxes.json", flatten=TRUE)
        }
        
        else if (input$compare_approach2=="Yes" &  length(which(!is.na(input$tmat_input_cp))==TRUE)!=0  ) return("Not both") #& !is.null(json1manual())
        
      #  if (is.null(data$msm)==FALSE) {
      #    if  (data$msm!=1) {data$msm_box=0}
      #    else if (data$msm==1)  {data$msm_box=1}
      #  }
      #  if (is.null(data$msm)==TRUE) {data$msm_box=0}
        
        data
      }  
      
      
    }
    
    
  })
  
  
  
  
  ####Read in the predictions file and identify the elements of the probabilities
  myjson1_5 <- reactive ({
    
    if (input$loadtype=="json") {
      
      if (input$aimtype=="present") {
      
      
        if (is.null(input$json2) & input$example=="No" ) return("Provide the json file with the predictions")
      
        else if (!is.null(input$json2) ) {
        
          data= fromJSON(input$json2$datapath, flatten=TRUE)
        
         }
        
        else if ( input$example=="Yes"   & is.null(input$json2)) {  #is.null(input$json2) &
        
          data= fromJSON("joint_lab_separate/0.example_data/predictions_stata_merlin.json", flatten=TRUE)
        
        }
      

      }
      
      else if (input$aimtype=="compare") {
        
         if (  (is.null(input$json2a) | is.null(input$json2b)) & input$compare_approach=="No" )  {return("Provide two json files with the predictions approaches")}
        
         else if (!is.null(input$json2a) & !is.null(input$json2b)  ) {
          
           list2a=fromJSON(input$json2a$datapath, flatten=TRUE)
           list2b=fromJSON(input$json2b$datapath, flatten=TRUE)
           
           
           Nstates=ncol(list2b$tmat)
           Ntransitions=max(list2b$tmat[which(!is.na(list2b$tmat))])
           
           y=vector()
           
           for (i in 1:Nstates) {
             
             for (k in 1:Nstates) {
               
              # if (i<=k) {
                 j=i+Nstates
                 g=k+Nstates
                 
                 y=sub(paste0("_to_",k),paste0("_to_",g),names(list2b) )
                 names(list2b)=y
             #  }
             }
           }
           
           is.integer0 <- function(x)
           {
             is.integer(x) && length(x) == 0L
           }
           
           
           if (  !is.integer0(which(startsWith(names(list2b),"User")) ) == TRUE )   {
             
             list2b=list2b[-which(startsWith(names(list2b),"User") == TRUE)]   
             
             names(list2b)
             
           }
           
         #  list2b=list2b[-which(!startsWith(names(list2b),"User") == FALSE)]   
         #  names(list2b)
           
           for (i in 1:Ntransitions) {
             k= i+ Ntransitions
             names(list2b)=sub(paste0("h",i),paste0("h",k),names(list2b) )
           }
           
           
           ### Create a hypertmatrix####
           
           list2b$tmat2= list2b$tmat+Ntransitions
           
           l <- list(list2b$tmat,list2b$tmat2)
           
           list2b$hypertmat<- as.matrix(bdiag(l))
           list2b$hypertmat[which(list2b$hypertmat==0)]=NA
           
           list2b$hypertmat
           
           list2a=list2a[names(list2a) %in% "tmat" == FALSE] 
           list2b=list2b[names(list2b) %in% "tmat" == FALSE] 
           list2b=list2b[names(list2b) %in% "tmat2" == FALSE] 
           
           list2b$tmat=list2b$hypertmat
           
           data=c(list2a,list2b)
         }
        
        
         else if ( input$compare_approach=="Yes" ) { #(is.null(input$json2a) | is.null(input$json2b)) &
          
           data= fromJSON("joint_lab_separate/0.example_data/predictions_stata_both_approaches.json", flatten=TRUE)
          
         }
      
        else if (!is.null(input$json2a) & !is.null(input$json2b) & input$compare_approach=="Yes" ) return("Not both")
      }
    }
    
    
    else if (input$loadtype=="csv") {
      
       if (input$aimtype=="present") {
      
      
          if (is.null(json2manual()) & input$example2=="No") return("Provide the csv file with the predictions")
      
          else if (!is.null(json2manual())  & input$example2=="No") { #& input$example=="No"
        
           data= fromJSON(json2manual(), flatten=TRUE)
        
         }
      
          else if ( input$example2=="Yes"  & is.null(json2manual())) {  #& is.null(json2manual())
        
            data= fromJSON("joint_lab_separate/0.example_data/predictions_stata_merlin.json", flatten=TRUE)

            data
         }
      
         else if ( input$example2=="Yes" & !is.null(json2manual())) return("Not both")
      }
      
      
      else if (input$aimtype=="compare") {
        
        
        if (is.null(json2manual()) & input$compare_approach2=="No" ) return("Provide the csv file with the predictions")
        
        else if (!is.null(json2manual()) & input$compare_approach2=="No" ) {
          
          data= fromJSON(json2manual(), flatten=TRUE)
          data
        }
      
        
        else if (  input$compare_approach2=="Yes" & is.null(json2manual()) ) { 
          
          data= fromJSON("joint_lab_separate/0.example_data/predictions_stata_both_approaches.json", flatten=TRUE)
          data 
        }
        
        else if (  input$compare_approach2=="Yes" & !is.null(json2manual()) ) return("Not both")
      }
    } 
    
    
    
    ##Save all probabilities as a separate list
    cond_P_select<-which(startsWith(names(data), 'P') & !startsWith(names(data), 'P_diff') & !startsWith(names(data),'P_ratio') &
                           !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    data$select=data[cond_P_select]
    
    data
    
  })
  
  
  
  #  #### output that shows that the previous reactive works
  #output$fileob <- renderPrint({
  #p=covselect()
  #
  #y= as.vector(input[['selectcov']])
  #  
  #y
  # })
  
  #### Isolate the from state probability fragment so that you can choose the cluster of probabilities you are going to use
  output$select<- renderUI ({
    
    if (is.null(myjson1_5())) return()
    
    radioButtons(inputId="select", label="Select conditional starting state", 
                 choices=unique(sub("_to_.*","", sub("P_","",names(myjson1_5()$select) )) ),
                 selected = unique(sub("_to_.*","", sub("P_","",names(myjson1_5()$select) ))  )[1]   )
  })
  
  ### Reconbine the isolated input of from state to define the ending of names specifind the end states  
  selectend<-reactive ({
    
    if (is.null(myjson1_5() )) return()
    
    cond_select<-which(startsWith(names(myjson1_5()), paste0('P_',input$select))  & 
                         !startsWith(names(myjson1_5()), 'P_diff' ) &  !startsWith(names(myjson1_5()), 'P_ratio' ) &
                         !endsWith(names(myjson1_5()), 'uci' ) & !endsWith(names(myjson1_5()), 'lci') )
    
    names=names(myjson1_5()[cond_select])
    
    v=vector()
    for (i in 1:length(cond_select)) {
      v[i]=sub("P_[[:digit:]]+","",names[i])  
    }
    v
  })
  
  
  selectend_h<-reactive ({
    
    if (is.null(myjson1_5() )) return()
    
    cond_select<-which(startsWith(names(myjson1_5()), paste0('Haz_',input$select))  & 
                         !startsWith(names(myjson1_5()), 'Haz_diff' ) &  !startsWith(names(myjson1_5()), 'Haz_ratio' ) &
                         !endsWith(names(myjson1_5()), 'uci' ) & !endsWith(names(myjson1_5()), 'lci') )
    
    names=names(myjson1_5()[cond_select])
    
    v=vector()
    for (i in 1:length(cond_select)) {
      v[i]=sub("Haz_[[:digit:]]+","",names[i])  
    }
    v
  })
  
  #### select which of the defined covariate patterns you want
  output$includecov<- renderUI ({
    if (is.null(myjson1_5() )) return()
    
    radioButtons(inputId="includecov", label="Manually select covariate patterns", 
                 choices=c("Yes","No") ,
                 selected = "No"   )
  })
  
  
  
  
  
  output$selectcov<- renderUI ({
    if (is.null(myjson1_5() )) return("Provide the json file with the predictions")
    
    item_list <- list()
    
    item_list[[1]] <- paste0("Reference covariate pattern","  :  ",myjson1_5()$atlist[1])
    
    if (input$includecov == 'Yes')  {
      
      
      if (length(myjson1_5()$atlist)>1) {
        item_list[[2]] <-   checkboxGroupInput("selectcov","Select extra covariate patterns 
                                             (at least one)",
                                               choices= myjson1_5()$atlist[-1] ,
                                               selected = myjson1_5()$atlist[-1] )
      }
      
      else return("You have included only one covariate pattern in your analysis")
    }
    do.call(tagList, item_list)
    
  })
  
  
  
  
  
  
  #### Turn the selected covariate patterns into a vector of numbers corresponding
  #### to the order of the covariate patterns element in the atlist
  covselect_char<- reactive ({
    
    shiny::validate(
      need(!is.null(input$selectcov) , "Please select at least 1 covariate pattern")
    )
    
    final_list=vector()
    
    
    myList<- input[['selectcov']]
    
    final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
    
    final_list
    
  })
  
  
  covselect<- reactive ({
    
    tonumber=vector()
    
    shiny::validate(
      need(!is.null(input$selectcov) , "Please select at least 1 covariate pattern")
    )
    
    # if (is.null(input$selectcov)) {tonumber=0}
    
    # else if (!is.null(input$selectcov)) {
    
    for (i in 1:length(covselect_char())) {
      tonumber[i]= which(myjson1_5()$atlist==covselect_char()[i])
    }
    #}
    tonumber
  })
  
  covselectcontr<- reactive ({
    
    
    tonumber=vector()
    
    if (length(covselect_char())!=0) {
      
      
      for (i in 1:length(covselect_char())) {
        tonumber[i]= which(myjson1_5()$atlist[-1]==covselect_char()[i])
      }
      tonumber=as.integer(tonumber)
    }
    
    tonumber
  })
  
  
  
  
  myjson2 <- reactive ({
    
    
    data= myjson1_5()
    
    
    cond_select_P<-which(startsWith(names(data), paste0('P_',input$select))  &
                           !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_select_P_diff<-which(startsWith(names(data), paste0('P_diff_',input$select))  &
                                !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_select_P_ratio<-which(startsWith(names(data), paste0('P_ratio_',input$select))  &
                                 !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_exists_P_ci<- which(startsWith(names(data), paste0('P_',input$select))  &
                               (endsWith(names(data), 'uci' ) | endsWith(names(data), 'lci')) )
    
    data$ci_P=cond_exists_P_ci
    
    
    if (length(cond_select_P)==0) {
      cond_P=vector();cond_P_uci=vector();cond_P_lci=vector()
      data$P=NULL;data$P_uci=NULL;data$P_lci=NULL
    }
    
    else if (length(cond_select_P)!=0) { 
      
      if (length(cond_exists_P_ci)!=0) { 
        
        ##Save all probabilities as a separate list
        cond_P=vector()
        for (i in 1:length(cond_select_P)) { cond_P[i]<-which(startsWith(names(data), 
                                                                         paste0('P_',input$select,selectend()[i]) ) & !startsWith(names(data), 'P_diff') & 
                                                                !startsWith(names(data),'P_ratio') & !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
        
        cond_P_uci=vector()
        for (i in 1:length(cond_select_P)) { cond_P_uci[i]<-which(startsWith(names(data), paste0('P_',input$select,selectend()[i])) & 
                                                                    !startsWith(names(data), 'P_diff') &  !startsWith(names(data),'P_ratio') &  endsWith(names(data), 'uci' ))}
        
        cond_P_lci=vector()
        for (i in 1:length(cond_select_P)) { cond_P_lci[i]<-which(startsWith(names(data), paste0('P_',input$select,selectend()[i])) & !startsWith(names(data), 'P_diff') & 
                                                                    !startsWith(names(data),'P_ratio') &  endsWith(names(data), 'lci' ))   }
      }
      
      else if (length(cond_exists_P_ci)==0) { 
        
        cond_P=vector();cond_P_uci=vector();cond_P_lci=vector()
        
        for (i in 1:length(cond_select_P)) { cond_P[i]<-which(startsWith(names(data), paste0('P_',input$select,selectend()[i]) ) & !startsWith(names(data), 'P_diff') & 
                                                                !startsWith(names(data),'P_ratio') & !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
      }
    }
    
    
    if (length(cond_select_P_diff)==0) {
      cond_P_diff=vector();cond_P_diff_uci=vector();cond_P_diff_lci=vector()
      data$Pd=NULL;data$Pd_uci=NULL;data$Pd_lci=NULL
    }
    
    else if (length(cond_select_P_diff)!=0) { 
      
      if (length(cond_exists_P_ci)!=0) { 
        
        cond_P_diff=vector()
        for (i in 1:length(cond_select_P_diff)) { cond_P_diff[i]<- which(startsWith(names(data), paste0('P_diff_',input$select,selectend()[i])) &
                                                                           !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
        cond_P_diff_uci=vector()
        for (i in 1:length(cond_select_P_diff)) { cond_P_diff_uci[i]<- which(startsWith(names(data), 
                                                                                        paste0('P_diff_',input$select,selectend()[i])) & endsWith(names(data), 'uci' ) ) }
        
        cond_P_diff_lci=vector()
        for (i in 1:length(cond_select_P_diff)) { cond_P_diff_lci[i]<- which(startsWith(names(data), 
                                                                                        paste0('P_diff_',input$select,selectend()[i])) & endsWith(names(data), 'lci' ) ) }
      }
      
      else if (length(cond_exists_P_ci)==0) { 
        
        cond_P_diff_uci=vector();cond_P_diff_lci=vector()
        cond_P_diff=vector()
        for (i in 1:length(cond_select_P_diff)) { cond_P_diff[i]<- which(startsWith(names(data), paste0('P_diff_',input$select,selectend()[i])) &
                                                                           !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
      }
    }
    
    if (length(cond_select_P_ratio)==0) {
      cond_P_ratio=vector();cond_P_ratio_uci=vector();cond_P_ratio_lci=vector()
      data$Pr=NULL;data$Pr_uci=NULL;data$Pr_lci=NULL
    }
    
    else if (length(cond_select_P_ratio)!=0) { 
      
      if (length(cond_exists_P_ci)!=0) { 
        
        cond_P_ratio=vector()
        for (i in 1:length(cond_select_P_ratio)) { cond_P_ratio[i]<- which(startsWith(names(data), paste0('P_ratio_',input$select,selectend()[i])) & 
                                                                             !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
        cond_P_ratio_uci=vector()
        for (i in 1:length(cond_select_P_ratio)) { cond_P_ratio_uci[i]<- which(startsWith(names(data),
                                                                                          paste0('P_ratio_',input$select,selectend()[i])) & endsWith(names(data), 'uci' ) ) }
        
        cond_P_ratio_lci=vector()
        for (i in 1:length(cond_select_P_ratio)) { cond_P_ratio_lci[i]<- which(startsWith(names(data), 
                                                                                          paste0('P_ratio_',input$select,selectend()[i])) & endsWith(names(data), 'lci' ) ) }
      }
      
      else if (length(cond_exists_P_ci)==0) { 
        
        cond_P_ratio_uci=vector();cond_P_ratio_lci=vector()
        cond_P_ratio=vector()
        for (i in 1:length(cond_select_P_ratio)) { cond_P_ratio[i]<- which(startsWith(names(data), paste0('P_ratio_',input$select,selectend()[i])) &
                                                                             !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
      }
    }
    
    
    
    data$P=data[cond_P]
    data$P_uci=data[cond_P_uci]
    data$P_lci=data[cond_P_lci] 
    
    data$Pd=data[cond_P_diff]
    data$Pd_uci=data[cond_P_diff_uci]
    data$Pd_lci=data[cond_P_diff_lci]
    
    data$Pr=data[cond_P_ratio]
    data$Pr_uci=data[cond_P_ratio_uci]
    data$Pr_lci=data[cond_P_ratio_lci]
    
    if (input$includecov == 'Yes')  {
      
      for (i in 1:length(cond_select_P)) {
        data$P[[i]]     =as.data.frame(as.matrix(data$P[[i]])[c(1,covselect()),])
        data$P_uci[[i]] =as.data.frame(as.matrix(data$P_uci[[i]])[c(1,covselect()),])
        data$P_lci[[i]] =as.data.frame(as.matrix(data$P_lci[[i]])[c(1,covselect()),])
      }
      for (i in 1:length(cond_select_P_diff)) {
        data$Pd[[i]]    =as.data.frame(as.matrix(data$Pd[[i]])[covselectcontr(),])
        data$Pd_uci[[i]]=as.data.frame(as.matrix(data$Pd_uci[[i]])[covselectcontr(),])
        data$Pd_lci[[i]]=as.data.frame(as.matrix(data$Pd_lci[[i]])[covselectcontr(),])
      }
      for (i in 1:length(cond_select_P_ratio)) {
        data$Pr[[i]]    =as.data.frame(as.matrix(data$Pr[[i]])[covselectcontr(),])
        data$Pr_uci[[i]]=as.data.frame(as.matrix(data$Pr_uci[[i]])[covselectcontr(),])
        data$Pr_lci[[i]]=as.data.frame(as.matrix(data$Pr_lci[[i]])[covselectcontr(),])
        
      }
      
    }  
    
    
    #### Old way of reading hazards into the app
    
    ##Save all transistion intensities as a separate list
   # cond_h<-which(startsWith(names(data), 'h') & endsWith(names(data), paste0('from_',input$select)) &
   #                !endsWith(names(data), 'lci') & !endsWith(names(data), 'uci'))
    
#    cond_h<-which(startsWith(names(data), 'h') &
#                    !endsWith(names(data), 'lci') & !endsWith(names(data), 'uci'))
#    
#    if (length(cond_h)==0) {
#      data$h=NULL
#    }
#    
#    else {
#      
#      if (input$includecov == 'No')  {
#        data$h=data[cond_h]
#      }
#      
#      if (input$includecov == 'Yes')  {
#        data$h=data[cond_h]
#        for (i in 1:data$Ntransitions) {
#          data$h[[i]]     =as.data.frame(as.matrix(data$h[[i]])[c(1,covselect()),])
#        }
#      }
#    }
    
    
    ##Save all los as a separate list
    cond_haz_all<-which(startsWith(names(data), 'Haz_')  
                      & !startsWith(names(data), 'Haz_diff') & !startsWith(names(data),'Haz_ratio') &
                        !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
        if (length(cond_haz_all)==0) {
            data$haz_all=NULL
        }
        else {  
          
                if (input$includecov == 'No')  {
                  data$haz_all=data[cond_haz_all]
                }
                
                if (input$includecov == 'Yes')  {
                  data$haz_all=data[cond_haz_all]
                  for (i in 1:data$Ntransitions) {
                    data$haz_all[[i]]     =as.data.frame(as.matrix(data$haz_all[[i]])[c(1,covselect()),])
                  }
                }
      }
          
          #############Experimental#######################
    
#    cond_h<-which(startsWith(names(data), 'Haz_')  
#                        & !startsWith(names(data), 'Haz_diff') & !startsWith(names(data),'Haz_ratio') &
#                          !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
#    
#    if (length(cond_h)==0) data$h=NULL
#    
#    else {  
#      if (input$includecov == 'No')  {
#        data$h=data[cond_h]
#      }
#      if (input$includecov == 'Yes')  {
#        data$h=data[cond_h]
#        for (i in 1:data$Ntransitions) {
#          data$h[[i]]     =as.data.frame(as.matrix(data$h[[i]])[c(1,covselect()),])
#        }
#      }
#    } 
#    
#    cond_h_uci<-which(startsWith(names(data), 'Haz_')  
#                    & !startsWith(names(data), 'Haz_diff') & !startsWith(names(data),'Haz_ratio') &
#                      endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
#    
#    if (length(cond_h_uci)==0) data$h_uci=NULL
#    
#    else {  
#      if (input$includecov == 'No')  {
#        data$h_uci=data[cond_h_uci]
#      }
#      if (input$includecov == 'Yes')  {
#        data$h_uci=data[cond_h_uci]
#        for (i in 1:data$Ntransitions) {
#          data$h_uci[[i]]     =as.data.frame(as.matrix(data$h_uci[[i]])[c(1,covselect()),])
#        }
#      }
#    }    
#    
#    cond_h_lci<-which(startsWith(names(data), 'Haz_')  
#                      & !startsWith(names(data), 'Haz_diff') & !startsWith(names(data),'Haz_ratio') &
#                        !endsWith(names(data), 'uci' ) & endsWith(names(data), 'lci') )
#    
#    if (length(cond_h_lci)==0) data$h_lci=NULL
#    
#    else {  
#      if (input$includecov == 'No')  {
#        data$h_lci=data[cond_h_lci]
#      }
#      if (input$includecov == 'Yes')  {
#        data$h_lci=data[cond_h_lci]
#        for (i in 1:data$Ntransitions) {
#          data$h_lci[[i]]     =as.data.frame(as.matrix(data$h_lci[[i]])[c(1,covselect()),])
#        }
#      }
#    }  
#    
#    cond_hd<-which(startsWith(names(data), 'Haz_')  
#                  & startsWith(names(data), 'Haz_diff') & !startsWith(names(data),'Haz_ratio') &
#                    !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
#    
#    if (length(cond_hd)==0) data$h=NULL
#    
#    else {  
#      if (input$includecov == 'No')  {
#        data$hd=data[cond_hd]
#      }
#      if (input$includecov == 'Yes')  {
#        data$hd=data[cond_hd]
#        for (i in 1:data$Ntransitions) {
#          data$hd[[i]]     =as.data.frame(as.matrix(data$hd[[i]])[c(1,covselect()),])
#        }
#      }
#    } 
#    
#    cond_hd_uci<-which(startsWith(names(data), 'Haz_')  
#                      & startsWith(names(data), 'Haz_diff') & !startsWith(names(data),'Haz_ratio') &
#                        endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
#    
#    if (length(cond_hd_uci)==0) data$hd_uci=NULL
#    
#    else {  
#      if (input$includecov == 'No')  {
#        data$hd_uci=data[cond_hd_uci]
#      }
#      if (input$includecov == 'Yes')  {
#        data$hd_uci=data[cond_hd_uci]
#        for (i in 1:data$Ntransitions) {
#          data$hd_uci[[i]]     =as.data.frame(as.matrix(data$hd_uci[[i]])[c(1,covselect()),])
#        }
#      }
#    }    
#    
#    cond_hd_lci<-which(startsWith(names(data), 'Haz_')  
#                      & startsWith(names(data), 'Haz_diff') & !startsWith(names(data),'Haz_ratio') &
#                        !endsWith(names(data), 'uci' ) & endsWith(names(data), 'lci') )
#    
#    if (length(cond_hd_lci)==0) data$hd_lci=NULL
#    
#    else {  
#      if (input$includecov == 'No')  {
#        data$hd_lci=data[cond_hd_lci]
#      }
#      if (input$includecov == 'Yes')  {
#        data$hd_lci=data[cond_hd_lci]
#        for (i in 1:data$Ntransitions) {
#          data$hd_lci[[i]]     =as.data.frame(as.matrix(data$hd_lci[[i]])[c(1,covselect()),])
#        }
#      }
#    }  
#    
#    cond_hr<-which(startsWith(names(data), 'Haz_')  
#                   & !startsWith(names(data), 'Haz_diff') & startsWith(names(data),'Haz_ratio') &
#                     !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
#    
#    if (length(cond_hr)==0) data$h=NULL
#    
#    else {  
#      if (input$includecov == 'No')  {
#        data$hr=data[cond_hr]
#      }
#      if (input$includecov == 'Yes')  {
#        data$hr=data[cond_hr]
#        for (i in 1:data$Ntransitions) {
#          data$hr[[i]]     =as.data.frame(as.matrix(data$hr[[i]])[c(1,covselect()),])
#        }
#      }
#    } 
#    
#    cond_hr_uci<-which(startsWith(names(data), 'Haz_')  
#                       & !startsWith(names(data), 'Haz_diff') & startsWith(names(data),'Haz_ratio') &
#                         endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
#    
#    if (length(cond_hr_uci)==0) data$hr_uci=NULL
#    
#    else {  
#      if (input$includecov == 'No')  {
#        data$hr_uci=data[cond_hr_uci]
#      }
#      if (input$includecov == 'Yes')  {
#        data$hr_uci=data[cond_hr_uci]
#        for (i in 1:data$Ntransitions) {
#          data$hr_uci[[i]]     =as.data.frame(as.matrix(data$hr_uci[[i]])[c(1,covselect()),])
#        }
#      }
#    }    
#    
#    cond_hr_lci<-which(startsWith(names(data), 'Haz_')  
#                       & !startsWith(names(data), 'Haz_diff') & startsWith(names(data),'Haz_ratio') &
#                         !endsWith(names(data), 'uci' ) & endsWith(names(data), 'lci') )
#    
#    if (length(cond_hr_lci)==0) data$hr_lci=NULL
#    
#    else {  
#      if (input$includecov == 'No')  {
#        data$hr_lci=data[cond_hr_lci]
#      }
#      if (input$includecov == 'Yes')  {
#        data$hr_lci=data[cond_hr_lci]
#        for (i in 1:data$Ntransitions) {
#          data$hr_lci[[i]]     =as.data.frame(as.matrix(data$hr_lci[[i]])[c(1,covselect()),])
#        }
#      }
#    }  
#    
#    if (length(cond_h_lci)!=0) {
#        data$ci_h=1 
#    }
    
    
   cond_select_haz<-which(startsWith(names(data), paste0('Haz_',input$select))  &
                            !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
   
   cond_select_haz_diff<-which(startsWith(names(data), paste0('Haz_diff_',input$select))  &
                                 !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
   
   cond_select_haz_ratio<-which(startsWith(names(data), paste0('Haz_ratio_',input$select))  &
                                  !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
   
   cond_exists_haz_ci<- which(startsWith(names(data), paste0('Haz_',input$select))  &
                                (endsWith(names(data), 'uci' ) | endsWith(names(data), 'lci')) )
   
   data$ci_haz=cond_exists_haz_ci
   
   if (length(cond_select_haz)==0) {
     cond_haz=vector();cond_haz_uci=vector();cond_haz_lci=vector()
     data$haz=NULL;data$haz_uci=NULL;data$haz_lci=NULL
   }
   
   else if (length(cond_select_haz)!=0) { 
     
     if (length(cond_exists_haz_ci)!=0) { 
       
       ##Save all probabilities as a separate list
       cond_haz=vector()
       for (i in 1:length(cond_select_haz)) { cond_haz[i]<-which(startsWith(names(data), paste0('Haz_',input$select,selectend_h()[i]) ) 
                                                                 & !startsWith(names(data), 'Haz_diff') & !startsWith(names(data),'Haz_ratio') &
                                                                   !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
       
       cond_haz_uci=vector()
       for (i in 1:length(cond_select_haz)) { cond_haz_uci[i]<-which(startsWith(names(data), paste0('Haz_',input$select,selectend_h()[i])) & 
                                                                       !startsWith(names(data), 'Haz_diff') &  !startsWith(names(data),'Haz_ratio') &  endsWith(names(data), 'uci' ))}
       
       cond_haz_lci=vector()
       for (i in 1:length(cond_select_haz)) { cond_haz_lci[i]<-which(startsWith(names(data), paste0('Haz_',input$select,selectend_h()[i])) & 
                                                                       !startsWith(names(data), 'Haz_diff') & !startsWith(names(data),'Haz_ratio') &  endsWith(names(data),'lci'))   }
     }
     
     else if (length(cond_exists_haz_ci)==0) { 
       
       cond_haz=vector();cond_haz_uci=vector();cond_haz_lci=vector()
       
       for (i in 1:length(cond_select_haz)) { cond_haz[i]<-which(startsWith(names(data), paste0('Haz_',input$select,selectend_h()[i]) ) &
                                                                   !startsWith(names(data), 'Haz_diff') & !startsWith(names(data),'Haz_ratio') & !endsWith(names(data), 'uci' ) &
                                                                   !endsWith(names(data), 'lci') ) }
     }
   }
   
   
   if (length(cond_select_haz_diff)==0) {
     cond_haz_diff=vector();cond_haz_diff_uci=vector();cond_haz_diff_lci=vector()
     data$hazd=NULL;data$hazd_uci=NULL;data$hazd_lci=NULL
   }
   
   else if (length(cond_select_haz_diff)!=0) { 
     
     if (length(cond_exists_haz_ci)!=0) { 
       
       cond_haz_diff=vector()
       for (i in 1:length(cond_select_haz_diff)) { cond_haz_diff[i]<- which(startsWith(names(data), paste0('Haz_diff_',input$select,selectend_h()[i])) &
                                                                              !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
       cond_haz_diff_uci=vector()
       for (i in 1:length(cond_select_haz_diff)) { cond_haz_diff_uci[i]<- which(startsWith(names(data), 
                                                                                           paste0('Haz_diff_',input$select,selectend_h()[i])) & endsWith(names(data), 'uci' ) ) }
       
       cond_haz_diff_lci=vector()
       for (i in 1:length(cond_select_haz_diff)) { cond_haz_diff_lci[i]<- which(startsWith(names(data), 
                                                                                           paste0('Haz_diff_',input$select,selectend_h()[i])) & endsWith(names(data), 'lci' ) ) }
     }
     
     else if (length(cond_exists_haz_ci)==0) { 
       
       cond_haz_diff_uci=vector();cond_haz_diff_lci=vector()
       cond_haz_diff=vector()
       for (i in 1:length(cond_select_haz_diff)) { cond_haz_diff[i]<- which(startsWith(names(data), paste0('Haz_diff_',input$select,selectend_h()[i])) &
                                                                              !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
     }
   }
   
   if (length(cond_select_haz_ratio)==0) {
     cond_haz_ratio=vector();cond_haz_ratio_uci=vector();cond_haz_ratio_lci=vector()
     data$hazr=NULL;data$hazr_uci=NULL;data$hazr_lci=NULL
   }
   
   else if (length(cond_select_haz_ratio)!=0) { 
     
     if (length(cond_exists_haz_ci)!=0) { 
       
       cond_haz_ratio=vector()
       for (i in 1:length(cond_select_haz_ratio)) { cond_haz_ratio[i]<- which(startsWith(names(data), paste0('Haz_ratio_',input$select,selectend_h()[i])) & 
                                                                                !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
       cond_haz_ratio_uci=vector()
       for (i in 1:length(cond_select_haz_ratio)) { cond_haz_ratio_uci[i]<- which(startsWith(names(data),
                                                                                             paste0('Haz_ratio_',input$select,selectend_h()[i])) & endsWith(names(data), 'uci' ) ) }
       
       cond_haz_ratio_lci=vector()
       for (i in 1:length(cond_select_haz_ratio)) { cond_haz_ratio_lci[i]<- which(startsWith(names(data), 
                                                                                             paste0('Haz_ratio_',input$select,selectend_h()[i])) & endsWith(names(data), 'lci' ) ) }
     }
     
     else if (length(cond_exists_haz_ci)==0) { 
       
       cond_haz_ratio_uci=vector();cond_haz_ratio_lci=vector()
       cond_haz_ratio=vector()
       for (i in 1:length(cond_select_haz_ratio)) { cond_haz_ratio[i]<- which(startsWith(names(data), paste0('Haz_ratio_',input$select,selectend_h()[i])) &
                                                                                !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
     }
   }
   
   
   data$haz=data[cond_haz]
   data$haz_uci=data[cond_haz_uci]
   data$haz_lci=data[cond_haz_lci] 
   
   data$hazd=data[cond_haz_diff]
   data$hazd_uci=data[cond_haz_diff_uci]
   data$hazd_lci=data[cond_haz_diff_lci]
   
   data$hazr=data[cond_haz_ratio]
   data$hazr_uci=data[cond_haz_ratio_uci]
   data$hazr_lci=data[cond_haz_ratio_lci]
   
   if (input$includecov == 'Yes')  {
     
     for (i in 1:length(cond_select_haz)) {
       data$haz[[i]]     =as.data.frame(as.matrix(data$haz[[i]])[c(1,covselect()),])
       data$haz_uci[[i]] =as.data.frame(as.matrix(data$haz_uci[[i]])[c(1,covselect()),])
       data$haz_lci[[i]] =as.data.frame(as.matrix(data$haz_lci[[i]])[c(1,covselect()),])
     }
     for (i in 1:length(cond_select_haz_diff)) {
       data$hazd[[i]]    =as.data.frame(as.matrix(data$hazd[[i]])[covselectcontr(),])
       data$hazd_uci[[i]]=as.data.frame(as.matrix(data$hazd_uci[[i]])[covselectcontr(),])
       data$hazd_lci[[i]]=as.data.frame(as.matrix(data$hazd_lci[[i]])[covselectcontr(),])
     }
     for (i in 1:length(cond_select_haz_ratio)) {
       data$hazr[[i]]    =as.data.frame(as.matrix(data$hazr[[i]])[covselectcontr(),])
       data$hazr_uci[[i]]=as.data.frame(as.matrix(data$hazr_uci[[i]])[covselectcontr(),])
       data$hazr_lci[[i]]=as.data.frame(as.matrix(data$hazr_lci[[i]])[covselectcontr(),])
     }
   }
  
  
  
  
  
    
#    
    ##Save all los as a separate list
    
    
    cond_select_los<-which(startsWith(names(data), paste0('Los_',input$select))  &
                             !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_select_los_diff<-which(startsWith(names(data), paste0('Los_diff_',input$select))  &
                                  !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_select_los_ratio<-which(startsWith(names(data), paste0('Los_ratio_',input$select))  &
                                   !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_exists_los_ci<- which(startsWith(names(data), paste0('Los_',input$select))  &
                                 (endsWith(names(data), 'uci' ) | endsWith(names(data), 'lci')) )
    
    data$ci_los=cond_exists_los_ci
    
    if (length(cond_select_los)==0) {
      cond_los=vector();cond_los_uci=vector();cond_los_lci=vector()
      data$los=NULL;data$los_uci=NULL;data$los_lci=NULL
    }
    
    else if (length(cond_select_los)!=0) { 
      
      if (length(cond_exists_los_ci)!=0) { 
        
        ##Save all probabilities as a separate list
        cond_los=vector()
        for (i in 1:length(cond_select_los)) { cond_los[i]<-which(startsWith(names(data), paste0('Los_',input$select,selectend()[i]) ) 
                                                                  & !startsWith(names(data), 'Los_diff') & !startsWith(names(data),'Los_ratio') &
                                                                    !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
        
        cond_los_uci=vector()
        for (i in 1:length(cond_select_los)) { cond_los_uci[i]<-which(startsWith(names(data), paste0('Los_',input$select,selectend()[i])) & 
                                                                        !startsWith(names(data), 'Los_diff') &  !startsWith(names(data),'Los_ratio') &  endsWith(names(data), 'uci' ))}
        
        cond_los_lci=vector()
        for (i in 1:length(cond_select_los)) { cond_los_lci[i]<-which(startsWith(names(data), paste0('Los_',input$select,selectend()[i])) & 
                                                                        !startsWith(names(data), 'Los_diff') & !startsWith(names(data),'Los_ratio') &  endsWith(names(data),'lci'))   }
      }
      
      else if (length(cond_exists_los_ci)==0) { 
        
        cond_los=vector();cond_los_uci=vector();cond_los_lci=vector()
        
        for (i in 1:length(cond_select_los)) { cond_los[i]<-which(startsWith(names(data), paste0('Los_',input$select,selectend()[i]) ) &
                                                                    !startsWith(names(data), 'Los_diff') & !startsWith(names(data),'Los_ratio') & !endsWith(names(data), 'uci' ) &
                                                                    !endsWith(names(data), 'lci') ) }
      }
    }
    
    
    if (length(cond_select_los_diff)==0) {
      cond_los_diff=vector();cond_los_diff_uci=vector();cond_los_diff_lci=vector()
      data$losd=NULL;data$losd_uci=NULL;data$losd_lci=NULL
    }
    
    else if (length(cond_select_los_diff)!=0) { 
      
      if (length(cond_exists_los_ci)!=0) { 
        
        cond_los_diff=vector()
        for (i in 1:length(cond_select_los_diff)) { cond_los_diff[i]<- which(startsWith(names(data), paste0('Los_diff_',input$select,selectend()[i])) &
                                                                               !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
        cond_los_diff_uci=vector()
        for (i in 1:length(cond_select_los_diff)) { cond_los_diff_uci[i]<- which(startsWith(names(data), 
                                                                                            paste0('Los_diff_',input$select,selectend()[i])) & endsWith(names(data), 'uci' ) ) }
        
        cond_los_diff_lci=vector()
        for (i in 1:length(cond_select_los_diff)) { cond_los_diff_lci[i]<- which(startsWith(names(data), 
                                                                                            paste0('Los_diff_',input$select,selectend()[i])) & endsWith(names(data), 'lci' ) ) }
      }
      
      else if (length(cond_exists_los_ci)==0) { 
        
        cond_los_diff_uci=vector();cond_los_diff_lci=vector()
        cond_los_diff=vector()
        for (i in 1:length(cond_select_los_diff)) { cond_los_diff[i]<- which(startsWith(names(data), paste0('Los_diff_',input$select,selectend()[i])) &
                                                                               !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
      }
    }
    
    if (length(cond_select_los_ratio)==0) {
      cond_los_ratio=vector();cond_los_ratio_uci=vector();cond_los_ratio_lci=vector()
      data$losr=NULL;data$losr_uci=NULL;data$losr_lci=NULL
    }
    
    else if (length(cond_select_los_ratio)!=0) { 
      
      if (length(cond_exists_los_ci)!=0) { 
        
        cond_los_ratio=vector()
        for (i in 1:length(cond_select_los_ratio)) { cond_los_ratio[i]<- which(startsWith(names(data), paste0('Los_ratio_',input$select,selectend()[i])) & 
                                                                                 !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
        cond_los_ratio_uci=vector()
        for (i in 1:length(cond_select_los_ratio)) { cond_los_ratio_uci[i]<- which(startsWith(names(data),
                                                                                              paste0('Los_ratio_',input$select,selectend()[i])) & endsWith(names(data), 'uci' ) ) }
        
        cond_los_ratio_lci=vector()
        for (i in 1:length(cond_select_los_ratio)) { cond_los_ratio_lci[i]<- which(startsWith(names(data), 
                                                                                              paste0('Los_ratio_',input$select,selectend()[i])) & endsWith(names(data), 'lci' ) ) }
      }
      
      else if (length(cond_exists_los_ci)==0) { 
        
        cond_los_ratio_uci=vector();cond_los_ratio_lci=vector()
        cond_los_ratio=vector()
        for (i in 1:length(cond_select_los_ratio)) { cond_los_ratio[i]<- which(startsWith(names(data), paste0('Los_ratio_',input$select,selectend()[i])) &
                                                                                 !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
      }
    }
    
    
    data$los=data[cond_los]
    data$los_uci=data[cond_los_uci]
    data$los_lci=data[cond_los_lci] 
    
    data$losd=data[cond_los_diff]
    data$losd_uci=data[cond_los_diff_uci]
    data$losd_lci=data[cond_los_diff_lci]
    
    data$losr=data[cond_los_ratio]
    data$losr_uci=data[cond_los_ratio_uci]
    data$losr_lci=data[cond_los_ratio_lci]
    
    if (input$includecov == 'Yes')  {
      
      for (i in 1:length(cond_select_los)) {
        data$los[[i]]     =as.data.frame(as.matrix(data$los[[i]])[c(1,covselect()),])
        data$los_uci[[i]] =as.data.frame(as.matrix(data$los_uci[[i]])[c(1,covselect()),])
        data$los_lci[[i]] =as.data.frame(as.matrix(data$los_lci[[i]])[c(1,covselect()),])
      }
      for (i in 1:length(cond_select_los_diff)) {
        data$losd[[i]]    =as.data.frame(as.matrix(data$losd[[i]])[covselectcontr(),])
        data$losd_uci[[i]]=as.data.frame(as.matrix(data$losd_uci[[i]])[covselectcontr(),])
        data$losd_lci[[i]]=as.data.frame(as.matrix(data$losd_lci[[i]])[covselectcontr(),])
      }
      for (i in 1:length(cond_select_los_ratio)) {
        data$losr[[i]]    =as.data.frame(as.matrix(data$losr[[i]])[covselectcontr(),])
        data$losr_uci[[i]]=as.data.frame(as.matrix(data$losr_uci[[i]])[covselectcontr(),])
        data$losr_lci[[i]]=as.data.frame(as.matrix(data$losr_lci[[i]])[covselectcontr(),])
      }
    }
    
    
    ##Save all visit as a separate list
    
    
    cond_select_visit<-which(startsWith(names(data), paste0('Visit_',input$select))  &
                               !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_select_visit_diff<-which(startsWith(names(data), paste0('Visit_diff_',input$select))  &
                                    !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_select_visit_ratio<-which(startsWith(names(data), paste0('Visit_ratio_',input$select))  &
                                     !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_exists_visit_ci<- which(startsWith(names(data), paste0('Visit_',input$select))  &
                                   (endsWith(names(data), 'uci' ) | endsWith(names(data), 'lci')) )
    
    data$ci_visit=cond_exists_visit_ci
    
    if (length(cond_select_visit)==0) {
      cond_visit=vector();cond_visit_uci=vector();cond_visit_lci=vector()
      data$visit=NULL;data$visit_uci=NULL;data$visit_lci=NULL
    }
    
    else if (length(cond_select_visit)!=0) { 
      
      if (length(cond_exists_visit_ci)!=0) { 
        
        ##Save all probabilities as a separate list
        cond_visit=vector()
        for (i in 1:length(cond_select_visit)) { cond_visit[i]<-which(startsWith(names(data), paste0('Visit_',input$select,selectend()[i]) ) 
                                                                      & !startsWith(names(data), 'Visit_diff') & !startsWith(names(data),'Visit_ratio') &
                                                                        !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
        
        cond_visit_uci=vector()
        for (i in 1:length(cond_select_visit)) { cond_visit_uci[i]<-which(startsWith(names(data), paste0('Visit_',input$select,selectend()[i])) & 
                                                                            !startsWith(names(data), 'Visit_diff') &  !startsWith(names(data),'Visit_ratio') &  endsWith(names(data), 'uci' ))}
        
        cond_visit_lci=vector()
        for (i in 1:length(cond_select_visit)) { cond_visit_lci[i]<-which(startsWith(names(data), paste0('Visit_',input$select,selectend()[i])) & 
                                                                            !startsWith(names(data), 'Visit_diff') & !startsWith(names(data),'Visit_ratio') &  endsWith(names(data),'lci'))   }
      }
      
      else if (length(cond_exists_visit_ci)==0) { 
        
        cond_visit=vector();cond_visit_uci=vector();cond_visit_lci=vector()
        
        for (i in 1:length(cond_select_visit)) { cond_visit[i]<-which(startsWith(names(data), paste0('Visit_',input$select,selectend()[i]) ) &
                                                                        !startsWith(names(data), 'Visit_diff') & !startsWith(names(data),'Visit_ratio') & !endsWith(names(data), 'uci' ) &
                                                                        !endsWith(names(data), 'lci') ) }
      }
    }
    
    
    if (length(cond_select_visit_diff)==0) {
      cond_visit_diff=vector();cond_visit_diff_uci=vector();cond_visit_diff_lci=vector()
      data$visitd=NULL;data$visitd_uci=NULL;data$visitd_lci=NULL
    }
    
    else if (length(cond_select_visit_diff)!=0) { 
      
      if (length(cond_exists_visit_ci)!=0) { 
        
        cond_visit_diff=vector()
        for (i in 1:length(cond_select_visit_diff)) { cond_visit_diff[i]<- which(startsWith(names(data), paste0('Visit_diff_',input$select,selectend()[i])) &
                                                                                   !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
        cond_visit_diff_uci=vector()
        for (i in 1:length(cond_select_visit_diff)) { cond_visit_diff_uci[i]<- which(startsWith(names(data), 
                                                                                                paste0('Visit_diff_',input$select,selectend()[i])) & endsWith(names(data), 'uci' ) ) }
        
        cond_visit_diff_lci=vector()
        for (i in 1:length(cond_select_visit_diff)) { cond_visit_diff_lci[i]<- which(startsWith(names(data), 
                                                                                                paste0('Visit_diff_',input$select,selectend()[i])) & endsWith(names(data), 'lci' ) ) }
      }
      
      else if (length(cond_exists_visit_ci)==0) { 
        
        cond_visit_diff_uci=vector();cond_visit_diff_lci=vector()
        cond_visit_diff=vector()
        for (i in 1:length(cond_select_visit_diff)) { cond_visit_diff[i]<- which(startsWith(names(data), paste0('Visit_diff_',input$select,selectend()[i])) &
                                                                                   !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
      }
    }
    
    if (length(cond_select_visit_ratio)==0) {
      cond_visit_ratio=vector();cond_visit_ratio_uci=vector();cond_visit_ratio_lci=vector()
      data$visitr=NULL;data$visitr_uci=NULL;data$visitr_lci=NULL
    }
    
    else if (length(cond_select_visit_ratio)!=0) { 
      
      if (length(cond_exists_visit_ci)!=0) { 
        
        cond_visit_ratio=vector()
        for (i in 1:length(cond_select_visit_ratio)) { cond_visit_ratio[i]<- which(startsWith(names(data), paste0('Visit_ratio_',input$select,selectend()[i])) & 
                                                                                     !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
        cond_visit_ratio_uci=vector()
        for (i in 1:length(cond_select_visit_ratio)) { cond_visit_ratio_uci[i]<- which(startsWith(names(data),
                                                                                                  paste0('Visit_ratio_',input$select,selectend()[i])) & endsWith(names(data), 'uci' ) ) }
        
        cond_visit_ratio_lci=vector()
        for (i in 1:length(cond_select_visit_ratio)) { cond_visit_ratio_lci[i]<- which(startsWith(names(data), 
                                                                                                  paste0('Visit_ratio_',input$select,selectend()[i])) & endsWith(names(data), 'lci' ) ) }
      }
      
      else if (length(cond_exists_visit_ci)==0) { 
        
        cond_visit_ratio_uci=vector();cond_visit_ratio_lci=vector()
        cond_visit_ratio=vector()
        for (i in 1:length(cond_select_visit_ratio)) { cond_visit_ratio[i]<- which(startsWith(names(data), paste0('Visit_ratio_',input$select,selectend()[i])) &
                                                                                     !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci')  ) }
      }
    }
    
    
    
    data$visit=data[cond_visit]
    data$visit_uci=data[cond_visit_uci]
    data$visit_lci=data[cond_visit_lci]
    
    data$visitd=data[cond_visit_diff]
    data$visitd_uci=data[cond_visit_diff_uci]
    data$visitd_lci=data[cond_visit_diff_lci]
    
    data$visitr=data[cond_visit_ratio]
    data$visitr_uci=data[cond_visit_ratio_uci]
    data$visitr_lci=data[cond_visit_ratio_lci]
    
    if (input$includecov == 'Yes')  {
      
      for (i in 1:length(cond_select_visit)) {
        data$visit[[i]]     =as.data.frame(as.matrix(data$visit[[i]])[c(1,covselect()),])
        data$visit_uci[[i]] =as.data.frame(as.matrix(data$visit_uci[[i]])[c(1,covselect()),])
        data$visit_lci[[i]] =as.data.frame(as.matrix(data$visit_lci[[i]])[c(1,covselect()),])
      }
      for (i in 1:length(cond_select_visit_diff)) {
        data$visitd[[i]]    =as.data.frame(as.matrix(data$visitd[[i]])[covselectcontr(),])
        data$visitd_uci[[i]]=as.data.frame(as.matrix(data$visitd_uci[[i]])[covselectcontr(),])
        data$visitd_lci[[i]]=as.data.frame(as.matrix(data$visitd_lci[[i]])[covselectcontr(),])
      }
      for (i in 1:length(cond_select_visit_ratio)) {
        data$visitr[[i]]    =as.data.frame(as.matrix(data$visitr[[i]])[covselectcontr(),])
        data$visitr_uci[[i]]=as.data.frame(as.matrix(data$visitr_uci[[i]])[covselectcontr(),])
        data$visitr_lci[[i]]=as.data.frame(as.matrix(data$visitr_lci[[i]])[covselectcontr(),])
      }
    }
    
    
    
    ##Save all user as a separate list
    
    
    cond_U<-which(startsWith(names(data), paste0('User_',input$select))  &
                    !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_U_diff<-which(startsWith(names(data), paste0('User_diff_',input$select))  &
                         !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_U_ratio<-which(startsWith(names(data), paste0('User_ratio_',input$select))  &
                          !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) 
    
    cond_exists_user_ci<- which(startsWith(names(data), paste0('User_',input$select))  &
                                  (endsWith(names(data), 'uci' ) | endsWith(names(data), 'lci')) )
    data$ci_user=cond_exists_user_ci
    
    
    
    
    if (length(cond_U)==0) {
      cond_user=vector();cond_user_uci=vector();cond_user_lci=vector()
      data$user=NULL;data$user_uci=NULL;data$user_lci=NULL
    }
    
    else if (length(cond_U)!=0) { 
      
      if (length(cond_exists_user_ci)!=0) { 
        
        cond_user=vector()
        for (i in 1:length(cond_U)) { cond_user[i]<-which(startsWith(names(data), paste0('User_',input$select)) & !startsWith(names(data), 'User_diff') & 
                                                            !startsWith(names(data),'User_ratio') & !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
        
        cond_user_uci=vector()
        for (i in 1:length(cond_U)) { cond_user_uci[i]<-which(startsWith(names(data), paste0('User_',input$select)) & !startsWith(names(data), 'User_diff') & 
                                                                !startsWith(names(data),'User_ratio') &  endsWith(names(data), 'uci' ))   }
        
        cond_user_lci=vector()
        for (i in 1:length(cond_U)) { cond_user_lci[i]<-which(startsWith(names(data), paste0('User_',input$select)) & !startsWith(names(data), 'User_diff') & 
                                                                !startsWith(names(data),'User_ratio') &  endsWith(names(data), 'lci' ))   }
        
      }
      
      else if (length(cond_exists_user_ci)==0) { 
        
        cond_user_uci=vector();cond_user_lci=vector()
        
        cond_user=vector()
        for (i in 1:length(cond_U)) { cond_user[i]<-which(startsWith(names(data), paste0('User_',input$select)) & !startsWith(names(data), 'User_diff') & 
                                                            !startsWith(names(data),'User_ratio') & !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
        
      }
    }
    
    
    if (length(cond_U_diff)==0) {
      cond_user_diff=vector();cond_user_diff_uci=vector();cond_user_diff_lci=vector()
      data$userd=NULL;data$userd_uci=NULL;data$userd_lci=NULL
    }
    
    else  if (length(cond_U_diff)!=0) {
      
      if (length(cond_exists_user_ci)!=0) { 
        
        cond_user_diff=vector()
        for (i in 1:length(cond_U_diff)) { cond_user_diff[i]<- which(startsWith(names(data), paste0('User_diff_',input$select)) & 
                                                                       !endsWith(names(data), 'uci' )  & !endsWith(names(data), 'lci')  ) }
        cond_user_diff_uci=vector()
        for (i in 1:length(cond_U_diff)) { cond_user_diff_uci[i]<- which(startsWith(names(data), paste0('User_diff_',input$select)) & 
                                                                           endsWith(names(data), 'uci' ) ) }
        cond_user_diff_lci=vector()
        for (i in 1:length(cond_U_diff)) { cond_user_diff_lci[i]<- which(startsWith(names(data), paste0('User_diff_',input$select)) 
                                                                         & endsWith(names(data), 'lci' ) ) }
      }
      
      else if (length(cond_exists_user_ci)==0) { 
        cond_user_diff_uci=vector();  cond_user_diff_lci=vector()
        
        cond_user_diff=vector()
        
        for (i in 1:length(cond_U_diff)) { cond_user_diff[i]<- which(startsWith(names(data), paste0('User_diff_',input$select)) & 
                                                                       !endsWith(names(data), 'uci' )  & !endsWith(names(data), 'lci')  ) }
      }
      
    }
    

    
    if (length(cond_U_ratio)==0) {
      cond_user_ratio=vector();cond_user_ratio_uci=vector();cond_user_ratio_lci=vector()
      data$userr=NULL;data$userr_uci=NULL;data$userr_lci=NULL
    } 
    
    else if (length(cond_U_ratio)!=0) {
      
      if (length(cond_exists_user_ci)!=0) { 
        
        cond_user_ratio=vector()
        for (i in 1:length(cond_U_ratio)) { cond_user_ratio[i]<- which(startsWith(names(data), paste0('User_ratio_',input$select)) &
                                                                         !endsWith(names(data), 'uci' ) & !endsWith(names(data),'lci') ) }
        cond_user_ratio_uci=vector()
        for (i in 1:length(cond_U_ratio)) { cond_user_ratio_uci[i]<- which(startsWith(names(data), paste0('User_ratio_',input$select)) &
                                                                             endsWith(names(data), 'uci' ) ) }
        cond_user_ratio_lci=vector()
        for (i in 1:length(cond_U_ratio)) { cond_user_ratio_lci[i]<- which(startsWith(names(data), paste0('User_ratio_',input$select)) &
                                                                             endsWith(names(data), 'lci' ) ) }
        
      }
      
      else if (length(cond_exists_user_ci)==0) { 
        cond_user_ratio_uci=vector();  cond_user_ratio_lci=vector()
        
        cond_user_ratio=vector()     
        
        for (i in 1:length(cond_U_ratio)) { cond_user_ratio[i]<- which(startsWith(names(data), paste0('User_ratio_',input$select)) &
                                                                         !endsWith(names(data), 'uci' ) & !endsWith(names(data),'lci') ) }
        
      }
    }
    
    
    data$user=data[cond_user]
    data$user_uci=data[cond_user_uci]
    data$user_lci=data[cond_user_lci] 
    
    data$userd=data[cond_user_diff]
    data$userd_uci=data[cond_user_diff_uci]
    data$userd_lci=data[cond_user_diff_lci]
    
    data$userr=data[cond_user_ratio]
    data$userr_uci=data[cond_user_ratio_uci]
    data$userr_lci=data[cond_user_ratio_lci]
    
    if (input$includecov == 'Yes')  {
      for (i in 1:length(cond_U)) {
        data$user[[i]]     =as.data.frame(as.matrix(data$user[[i]])[c(1,covselect()),])
        data$user_uci[[i]] =as.data.frame(as.matrix(data$user_uci[[i]])[c(1,covselect()),])
        data$user_lci[[i]] =as.data.frame(as.matrix(data$user_lci[[i]])[c(1,covselect()),])
      }
      for (i in 1:length(cond_U_diff)) {
        data$userd[[i]]    =as.data.frame(as.matrix(data$userd[[i]])[covselectcontr(),])
        data$userd_uci[[i]]=as.data.frame(as.matrix(data$userd_uci[[i]])[covselectcontr(),])
        data$userd_lci[[i]]=as.data.frame(as.matrix(data$userd_lci[[i]])[covselectcontr(),])
      }
      for (i in 1:length(cond_U_ratio)) {
        data$userr[[i]]    =as.data.frame(as.matrix(data$userr[[i]])[covselectcontr(),])
        data$userr_uci[[i]]=as.data.frame(as.matrix(data$userr_uci[[i]])[covselectcontr(),])
        data$userr_lci[[i]]=as.data.frame(as.matrix(data$userr_lci[[i]])[covselectcontr(),])
      }
    }
    
    #### Number ####
    
    ##Save all Number as a separate list
    
    cond_N <- which(startsWith(names(data), paste0('Number_',input$select))  &
                      !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_exists_number_ci<- which(startsWith(names(data), paste0('Number_',input$select))  &
                                    (endsWith(names(data), 'uci' ) | endsWith(names(data), 'lci')) )
    data$ci_number=cond_exists_number_ci
    
    if (length(cond_N)==0) {
      cond_number=vector();cond_number_uci=vector();cond_number_lci=vector()
      data$number=NULL;data$number_uci=NULL;data$number_lci=NULL
    }
    
    else if (length(cond_N)!=0) { 
      
      if (length(cond_exists_number_ci)!=0) { 
        
        cond_number=vector()
        for (i in 1:length(cond_N)) { cond_number[i]<-which(startsWith(names(data), paste0('Number_',input$select,selectend()[i]) ) &
                                                              !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
        
        cond_number_uci=vector()
        for (i in 1:length(cond_N)) { cond_number_uci[i]<-which(startsWith(names(data), paste0('Number_',input$select,selectend()[i])) 
                                                                &  endsWith(names(data), 'uci' ))   }
        
        cond_number_lci=vector()
        for (i in 1:length(cond_N)) { cond_number_lci[i]<-which(startsWith(names(data), paste0('Number_',input$select,selectend()[i])) & 
                                                                  endsWith(names(data), 'lci' ))   }
        
      }
      
      else if (length(cond_exists_number_ci)==0) { 
        
        cond_number_uci=vector();cond_number_lci=vector()
        
        cond_number=vector()
        for (i in 1:length(cond_N)) { cond_number[i]<-which(startsWith(names(data), paste0('Number_',input$select,selectend()[i])) & 
                                                              !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
        
      }
    }
    
    data$number=data[cond_number]
    data$number_uci=data[cond_number_uci]
    data$number_lci=data[cond_number_lci] 
    
    if (input$includecov == 'Yes')  {
      if (length(data$number)>0) {
        for (i in 1:length(data$number)) {
          data$number[[i]]     =as.data.frame(as.matrix(data$number[[i]])[c(1,covselect()),])
          data$number_uci[[i]] =as.data.frame(as.matrix(data$number_uci[[i]])[c(1,covselect()),])
          data$number_lci[[i]] =as.data.frame(as.matrix(data$number_lci[[i]])[c(1,covselect()),])
        } 
      }
    }
    
    ### Save Next ###
    
    cond_Ne <- which(startsWith(names(data), paste0('Next_',input$select))  &
                       !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    data$cond_Ne=cond_Ne
    
    cond_exists_next_ci<- which(startsWith(names(data), paste0('Next_',input$select))  &
                                  (endsWith(names(data), 'uci' ) | endsWith(names(data), 'lci')) )
    data$ci_next=cond_exists_next_ci
    
    if (length(cond_Ne)==0) {
      cond_next=vector();cond_next_uci=vector();cond_next_lci=vector()
      data$nextv=NULL;data$next_uci=NULL;data$next_lci=NULL
    }
    
    else if (length(cond_Ne)!=0) { 
      
      if (length(cond_exists_next_ci)!=0) { 
        
        cond_next=vector()
        for (i in 1:length(cond_Ne)) { cond_next[i]<-which(startsWith(names(data), paste0('Next_',input$select,selectend()[i]) ) &
                                                             !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
        
        
        
        cond_next_uci=vector()
        for (i in 1:length(cond_Ne)) { cond_next_uci[i]<-which(startsWith(names(data), paste0('Next_',input$select,selectend()[i])) 
                                                               &  endsWith(names(data), 'uci' ))   }
        
        cond_next_lci=vector()
        for (i in 1:length(cond_Ne)) { cond_next_lci[i]<-which(startsWith(names(data), paste0('Next_',input$select,selectend()[i])) & 
                                                                 endsWith(names(data), 'lci' ))   }
        
      }
      
      else if (length(cond_exists_next_ci)==0) { 
        
        cond_next_uci=vector();cond_next_lci=vector()
        
        cond_next=vector()
        for (i in 1:length(cond_Ne)) { cond_next[i]<-which(startsWith(names(data), paste0('Next_',input$select,selectend()[i])) & 
                                                             !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
        data$cond_next=cond_next
      }
    }
    
    data$nextv=data[cond_next]
    data$next_uci=data[cond_next_uci]
    data$next_lci=data[cond_next_lci] 
    
    if (input$includecov == 'Yes')  {
      if (length(data$nextv)>0) {
        for (i in 1:length(data$nextv)) {
          data$nextv[[i]]     =as.data.frame(as.matrix(data$nextv[[i]])[c(1,covselect()),])
          data$next_uci[[i]] =as.data.frame(as.matrix(data$next_uci[[i]])[c(1,covselect()),])
          data$next_lci[[i]] =as.data.frame(as.matrix(data$next_lci[[i]])[c(1,covselect()),])
        } 
      }
    }
    
    ### Sojourn ###
    
    cond_S <- which(startsWith(names(data), 'Soj_')  &
                      !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_exists_soj_ci<- which(startsWith(names(data), "Soj_" )  &
                                 (endsWith(names(data), 'uci' ) | endsWith(names(data), 'lci')) )
    data$ci_soj=cond_exists_soj_ci
    
    if (length(cond_S)==0) {
      cond_soj=vector();cond_soj_uci=vector();cond_soj_lci=vector()
      data$soj=NULL;data$soj_uci=NULL;data$soj_lci=NULL
    }
    
    else if (length(cond_S)!=0) { 
      
      if (length(cond_exists_soj_ci)!=0) { 
        
        cond_soj=vector()
        for (i in 1:length(cond_S)) { cond_soj[i]<-which(startsWith(names(data), paste0('Soj_',sub("_to_","", selectend()[i] ) ) ) &
                                                           !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
        
        cond_soj_uci=vector()
        for (i in 1:length(cond_S)) { cond_soj_uci[i]<-which(startsWith(names(data), paste0('Soj_',sub("_to_","", selectend()[i] ) ) )
                                                             &  endsWith(names(data), 'uci' ))   }
        
        cond_soj_lci=vector()
        for (i in 1:length(cond_S)) { cond_soj_lci[i]<-which(startsWith(names(data), paste0('Soj_',sub("_to_","", selectend()[i] ) ) ) 
                                                             &  endsWith(names(data), 'lci' ))   }
        
      }
      
      else if (length(cond_exists_soj_ci)==0) { 
        
        cond_soj_uci=vector();cond_soj_lci=vector()
        
        cond_soj=vector()
        for (i in 1:length(cond_S)) { cond_soj[i]<-which(startsWith(names(data), paste0('Soj_',sub("_to_","", selectend()[i] ) ) )  & 
                                                           !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
        
      }
    }
    
    data$soj=data[cond_soj]
    data$soj_uci=data[cond_soj_uci]
    data$soj_lci=data[cond_soj_lci] 
    
    if (input$includecov == 'Yes')  {
      if (length(data$soj)>0) {
        for (i in 1:length(data$soj)) {
          data$soj[[i]]     =as.data.frame(as.matrix(data$soj[[i]])[c(1,covselect()),])
          data$soj_uci[[i]] =as.data.frame(as.matrix(data$soj_uci[[i]])[c(1,covselect()),])
          data$soj_lci[[i]] =as.data.frame(as.matrix(data$soj_lci[[i]])[c(1,covselect()),])
        } 
      }
    }
    
    ### First ###
    ### Save First ###
    
    cond_F <- which(startsWith(names(data), 'First_')  &
                      !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') )
    
    cond_exists_first_ci<- which(startsWith(names(data), "First_" )  &
                                   (endsWith(names(data), 'uci' ) | endsWith(names(data), 'lci')) )
    data$ci_first=cond_exists_first_ci
    
    if (length(cond_F)==0) {
      cond_first=vector();cond_first_uci=vector();cond_first_lci=vector()
      data$first=NULL;data$first_uci=NULL;data$first_lci=NULL
    }
    
    else if (length(cond_F)!=0) { 
      
      if (length(cond_exists_first_ci)!=0) { 
        
        cond_first=vector()
        for (i in 1:length(cond_F)) { cond_first[i]<-which(startsWith(names(data), paste0('First_',sub("_to_","", selectend()[i] ) ) ) &
                                                             !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
        
        cond_first_uci=vector()
        for (i in 1:length(cond_F)) { cond_first_uci[i]<-which(startsWith(names(data), paste0('First_',sub("_to_","", selectend()[i] ) ) )
                                                               &  endsWith(names(data), 'uci' ))   }
        
        cond_first_lci=vector()
        for (i in 1:length(cond_F)) { cond_first_lci[i]<-which(startsWith(names(data), paste0('First_',sub("_to_","", selectend()[i] ) ) ) 
                                                               &  endsWith(names(data), 'lci' ))   }
        
      }
      
      else if (length(cond_exists_first_ci)==0) { 
        
        cond_first_uci=vector();cond_first_lci=vector()
        
        cond_first=vector()
        for (i in 1:length(cond_F)) { cond_first[i]<-which(startsWith(names(data), paste0('First_',sub("_to_","", selectend()[i] ) ) )  & 
                                                             !endsWith(names(data), 'uci' ) & !endsWith(names(data), 'lci') ) }
        
      }
    }
    
    data$first=data[cond_first]
    data$first_uci=data[cond_first_uci]
    data$first_lci=data[cond_first_lci] 
    
    if (input$includecov == 'Yes')  {
      if (length(data$first)>0) {
        for (i in 1:length(data$first)) {
          data$first[[i]]     =as.data.frame(as.matrix(data$first[[i]])[c(1,covselect()),])
          data$first_uci[[i]] =as.data.frame(as.matrix(data$first_uci[[i]])[c(1,covselect()),])
          data$first_lci[[i]] =as.data.frame(as.matrix(data$first_lci[[i]])[c(1,covselect()),])
        } 
      }
    }
    ##The time variable
    data$timevar
    
    ### Number of different covariate patterns specified for prediction
    
    
    #### List of covariate patterns
    
    
    
    if (input$includecov == 'Yes')  {
      data$cov$atlist= c(myjson1_5()$atlist[1],covselect_char())
      data$atlist=     c(myjson1_5()$atlist[1],covselect_char())
      data$Nats=length(   c(myjson1_5()$atlist[1],covselect_char()))
    }
    else if (input$includecov == 'No') {
      data$cov$atlist= c(myjson1_5()$atlist)
      data$atlist=     c(myjson1_5()$atlist)
      data$Nats=length( c(myjson1_5()$atlist))}
    data
    
    
  })
  
  
  #### output that shows that the previous reactive works
  
  
  
  # Include the logic (server) for each tab
  
  #source("joint_lab_separate/function/stacked.R",local=T)$value
  
  source("joint_lab_separate/function/stacked_bars.R",local=T)$value
  
  

  
  source( "joint_lab_separate/5.interpretation/server_interpret.R", local=T)$value
  
  source("joint_lab_separate/4.predict/server_p.R", local=T)$value
  
  source("joint_lab_separate/4.predict/server_h.R", local=T)$value
  
  source("joint_lab_separate/4.predict/server_los.R", local=T)$value
  
  source("joint_lab_separate/4.predict/server_visit.R", local=T)$value
  
  source("joint_lab_separate/4.predict/server_user.R", local=T)$value
  
  source("joint_lab_separate/4.predict/server_extra.R", local=T)$value
  

 #outputOptions(output, "covarinputp", suspendWhenHidden = FALSE)

#output$fileob<- renderPrint({
#
#  json2manual()
#})
#
  
  }

shinyApp(ui = ui, server = server)

