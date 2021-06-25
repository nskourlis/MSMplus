#### If action buttin is hit, hide all this stuff ####
#observeEvent(input$hide, {
#
#
#  hide("statesinputbox")
#  hide("transitioninput")
#  hide("fillcolor")
#  hide("boxinput")
#  hide("colourinput")
#  hide("boxinput2")
#  hide("boxsave")
#  hide("framebox")
#  
#
## toggle("plot") if you want to alternate between hiding and showing
#})
#
#observeEvent(input$show, {
#
#  show("statesinputbox")
#  show("transitioninput")
#  show("fillcolor")
#  show("boxinput")
#  show("colourinput")
#  show("boxinput2")
#  show("framebox")
#  show("boxsave")
#  })


## If we want to do it with radio buttons

timer <- reactiveVal(1.02)

observeEvent(c(input$showbox,invalidateLater(1000, session)), {
  
  if(input$showbox=="No"){
    
    hide("boxinput")
    hide("colourinput")
    hide("boxinput2")
    hide("network1")  
  }
  
  if(input$showbox=="Yes"){
    
    show("boxinput")
    show("colourinput")
    show("boxinput2")
    show("network1")  
  }
  
  isolate({
    
    timer(timer()-1)
    if(timer()>1 & input$showbox=="No")
    {
      show("boxinput")
      show("colourinput")
      show("boxinput2")
      show("network1")  
    }
    
  })
})



observeEvent(c(input$showbox2,invalidateLater(1000, session)), {
  
  if(input$showbox2=="No"){
    
    hide("statesinputbox")
    hide("transitioninput")
  
  }
  
  if(input$showbox2=="Yes"){
    
    show("statesinputbox")
    show("transitioninput")
 
  }
  
  isolate({
    
    timer(timer()-1)
    if(timer()>1 & input$showbox2=="No")
    {
      show("statesinputbox")
      show("transitioninput")
    }
    
  })
})


#observeEvent(c(input$showbox3,invalidateLater(1000, session)), {
#  
#  if(input$showbox3=="No"){
#    
#    hide("tickinputgraph")
#
#    
#  }
#  
#  if(input$showbox3=="Yes"){
#    
#    show("tickinputgraph")
#    
#  }
#  
#  isolate({
#    
#    timer(timer()-1)
#    if(timer()>1 & input$showbox3=="No")
#    {
#      show("tickinputgraph")
#    }
#    
#  })
#})


output$is_before <- renderUI({
  
  radioButtons("interactive", "Type of graph",
               choices = list("Static" = "No",
                              "Interactive" = "Yes"), selected = "No")
})

output$pageboxbefore <- renderUI({
  
  
  fluidRow(
    column(12,
           uiOutput("is_before")
    )
  )
})


output$pagebox <- renderUI({
  
  if (input$interactive=="No") {
    #.irs-bar-edge {background: black; border: 1px solid black; height: 25px; border-radius: 0px; width: 20px !important;;}   
    fluidRow(   
      tags$head(
        tags$style(HTML(type="text/css", ".jslider { max-width: 200px; max-height: 100px;}")),
        tags$style(HTML(type='text/css', ".irs-grid-text { font-size: 15pt; }")),
        tags$style(HTML(type="text/css", "input.shiny-bound-input { font-size:20px; height:35px !important;}")),
        tags$style(HTML(type="text/css","shiny-html-output{ font-size:20px; height:25px;}")),
        tags$style("#frequency {font-size:20px;}"),
        tags$style(HTML(type='number',".irs-grid-text { font-size: 12pt !important; }")),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        #    tags$style(HTML(type='text/css', " .irs-grid {display: none !important;}")),
        
        
      ),
      column(2,
             
             useShinyjs(),
             uiOutput("boxtitle"),
             uiOutput("is_showbox"),
             uiOutput("statesinputbox"),
             uiOutput("transitioninput"),
        #     uiOutput("tickinputgraph")
      ),
      column(2,
             uiOutput("boxinput"),
             uiOutput("colourinput"),
             #uiOutput("framebox")
             
             
      ),
      column(2,
             uiOutput("boxinput2")
             
      ),
      column(6,
             plotOutput("msm_scheme_interactive", width = "100%", height = "700px"),
             uiOutput("note"),
             uiOutput("shouldload")
      ) 
    )
  }
  
  else if (input$interactive=="Yes") {
    
    fluidRow(   
      tags$head(
        tags$style(HTML(type="text/css", ".jslider { max-width: 200px; max-height: 100px;}")),
        tags$style(HTML(type='text/css', ".irs-grid-text { font-size: 15pt; }")),
        tags$style(HTML(type="text/css", "input.shiny-bound-input { font-size:20px; height:35px !important;}")),
        tags$style(HTML(type="text/css","shiny-html-output{ font-size:20px; height:25px;}")),
        #    tags$style(HTML(type='text/css', " .irs-grid {display: none !important;}")),
        
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
      ),
      column(2,
             
             useShinyjs(),
             uiOutput("is_showbox"),
             uiOutput("boxtitle"),
             uiOutput("statesinputbox"),
             uiOutput("transitioninput")
             
      ),
      column(2,
             uiOutput("network1"),
             #uiOutput("framebox")
      ),
      
      column(8,
             
             visNetworkOutput(outputId="msm_scheme_network", width = "100%", height = "700px"),
             uiOutput("shouldloadnetwork"),
      ) 
    )
  }
  
  
  
  
})


output$fileob3<- renderPrint({
  
  myjson2()$P
  #json1manual()
  #json2manual()
  
  #read.csv(input$csv2$datapath,header=TRUE, sep=";")
})


output$note <- renderUI({
  
  if (is.null(myjson1()$frequencies)) return()
  
  else
    
  item_list <- list()  
  
  item_list[[1]] <- withMathJax(helpText(strong("Frequency of individuals in each state")),
                               helpText('The numbers inside the boxes depict the number of individuals in each state
                                        for the time point specified.'))
  
  item_list[[2]] <- withMathJax(helpText(strong("Cummulative number of transitions")), 
                              helpText('The numbers near the arrows depict the cumulative number of each type of 
                              transition up to the time point specified.'))
  
  do.call(tagList, item_list)
  
})



#####################################################

output$is_showbox <- renderUI({
  if (is.null(myjson1())) return()
  
  else
  
  item_list <- list()  
  
  item_list[[1]] <-radioButtons("showbox2", "Show state and transition names MSM",
                 choices = list("No" = "No","Yes" = "Yes"), selected = "No")
  
  item_list[[2]] <-radioButtons("showbox", "Show customisation adjustments for MSM",
               choices = list("No" = "No","Yes" = "Yes"), selected = "No")
  
 # item_list[[3]] <- radioButtons("showbox3", "Choose scale and resolution of graph",
 #              choices = list("No" = "No", "Yes" = "Yes"), selected = "No")
 # 
  do.call(tagList, item_list)
  
})


#output$tickinputgraph <- renderUI({
#  
# # default_choices_scale=c(1,2,3)
#  
#  if (is.null(myjson2()))  return()
#  item_list <- list()
#  
#  item_list[[1]] <-numericInput("resbox" ,"Graph resolution",value=300,min=100,max=610)
#  item_list[[2]] <-numericInput("figscalebox"  ,"Graph scale (multiple of 800*800px)",value=1,min=1,max=4)
#
#  do.call(tagList, item_list)
#})


output$network1 <- renderUI({
  
  if (is.null(myjson1()))  return()
  item_list <- list()
  
  default_choices_shape=c("box","square", "triangle", "box", "circle", "dot", "star","ellipse")
  
  default_choices_colour=c("orange","white","red", "grey","lightblue")
  
  default_choices_colourtext=c("#17202a","#e74c3c","#7d3c98","#f39c12","#f0f3f4","#2874a6")
  
  # default_choices_shadow=c(FALSE,TRUE)
  
  default_choices_smooth=c("No","discrete","continuous","cubicBezier")
  
  default_choices_arrow=c("to", "from", "middle", "middle;to")
  
  #default_choices_physics=c(FALSE,TRUE)
  
  default_choices_visI=c("Fluid","layout_nicely","layout_in_circle","layout_as_tree","layout_with_sugiyama")
  
  
  ### Defining the default values for boxes
  if (is.null(myjson1()))  return()
  item_list <- list()
  
  item_list[[1]] <- selectInput("shape",      "Shape of nodes", default_choices_shape, selected = default_choices_shape[1])
  item_list[[2]] <- selectInput("colournet",  "Colour of nodes and arrows", default_choices_colour, selected = default_choices_colour[1])
  item_list[[3]] <- selectInput("colourtext", "Colour of text" ,    default_choices_colourtext, selected = default_choices_colourtext[1])
  #item_list[[4]] <- selectInput("putshadow",     "Shadow", default_choices_shadow, selected = default_choices_shadow[1])
  item_list[[4]] <- selectInput("putsmooth",     "Arrows smooth connection", default_choices_smooth, selected = default_choices_smooth[1])
  item_list[[5]] <- selectInput("arrowposition",   "Arrow position", default_choices_arrow, selected = default_choices_arrow[1])
  # item_list[[6]] <- selectInput("physics",   "Enable physics attribute", default_choices_physics, selected = default_choices_physics[1])
  item_list[[6]] <- selectInput("visI",       "Layout", default_choices_visI, selected = default_choices_visI[1])
  item_list[[7]] <- sliderInput("cexnet",     "Size of text",min=0.2,max=2,step=0.1,  value=1)
  
  do.call(tagList, item_list)
  
  
})






output$boxtitle <- renderUI({
  
  if (is.null((myjson1())))  return("Provide msboxes json file")
  
  # create some text inputs
  item_list <- list()
  item_list[[1]] <- textInput("title","Multi-state Graph title","Multi-state Graph")
  
  do.call(tagList, item_list)
  
})

#output$boxfill <- renderUI({
#  
#  if (is.null((myjson1())))  return()
#  # create some text inputs
#  item_list <- list()
#  item_list[[1]] <- radioButtons("fill", "Box fill", choices = list("No","Yes"))
#  
#  do.call(tagList, item_list)
#  
#})

#output$boxsave <- renderUI({
#  
#  if (is.null((myjson1())))  return()
#  # create some text inputs
#  item_list <- list()
#  item_list[[1]] <- radioButtons("save", "Save scheme as pdf", choices = list("png"))
#  
#  do.call(tagList, item_list)
#  
#})



output$shouldload <- renderUI({
  if (is.null((myjson1())))  return()
  downloadButton(outputId = "down", label = h2("Download the MSM plot"))
})


output$shouldloadnetwork <- renderUI({
  if (is.null((myjson1())))  return()
  downloadButton(outputId = "downnetwork", label = h2("Download the MSM plot as html"))
})

output$down <- downloadHandler(
  
  filename= function() {
    paste(paste0("MSM","_up_to",input$uptime),"png", sep=".") 
  },
  content= function(file) {
    #if (input$save=="png")
    heightbox= 800#*input$figscalebox
    widthbox=  800#*input$figscalebox
    png(file, width = heightbox, height = heightbox, units = "px")#, res=input$resbox)
    #else pdf(file, width = 700, height = 700, units = "px")
    
    ntransitions=myjson1()$Ntransitions
    nstates= myjson1()$Nstates
    xvaluesb=labels_x()  #+boxwidth/2
    yvaluesb=labels_y()  #-boxheight/2
    boxes=msboxes_R_nofreq(yb=yvaluesb, xb=xvaluesb, boxwidth=input$boxwidth , boxheight=input$boxheight, tmat.= myjson1()$tmat)
    #Read through json from msboxes or through a new function 
    x1=boxes$arrows$x1
    y1=boxes$arrows$y1
    x2=boxes$arrows$x2
    y2=boxes$arrows$y2
    arrowstextx=boxes$arrowstext$x
    arrowstexty=boxes$arrowstext$y
    #######################################################
    tname=vector()
    tname=labels_trans_box()
    statename=vector()
    statename=labels_states_box()
    
    
    plotit<-function(){
      plot(c(0, 1), c(0, 1), type = "n", ylab='',xlab='', xaxt='n', yaxt='n', pch=30)
      title(main = input$title,  line = -1, cex.main =input$cex)
      text(0.05, 1, paste0("At time"," ",input$uptime),cex = input$cex)
      ### Call the box function
      recttext(xcenter=xvaluesb, ycenter=yvaluesb, 
               boxwidth=input$boxwidth, boxheight=input$boxheight,statename=statename,
               freq_box=myjson1()$frequencies[which(myjson1()$frequencies$timevar==input$uptime),2:(myjson1()$Nstates+1)],
               rectArgs = list(col = 'white', lty = 'solid'),
               textArgs_state = list(col = input$boxcolornames, cex = input$cex,pos=3),
               textArgs_freq  = list(col = input$boxcolorfreqs, cex = input$cex,pos=1))
      
      ### Call the arrows function
      if ( is.null(myjson1()$frequencies) ) {
        ### Call the arrows function
        arrows_msm(xstart=x1, ystart=y1,xend=x2, yend=y2,xtext=arrowstextx, ytext=arrowstexty,tname=tname,
                   tfreq=matrix(nrow=1, ncol=ntransitions,""),
                   textArgs_transname =list(col =input$arrowcolornames, cex = input$cex, pos=3), 
                   textArgs_transfreq =list(col = input$arrowcolorfreqs, cex = input$cex, pos=1), arrowcol=input$arrowcolour,lty = 1)
        
      }
      
      if ( !is.null(myjson1()$frequencies) ) {
        ### Call the arrows function
        arrows_msm(xstart=x1, ystart=y1,xend=x2, yend=y2,xtext=arrowstextx, ytext=arrowstexty,tname=tname,
                   tfreq=myjson1()$frequencies[which(myjson1()$frequencies$timevar==input$uptime),(myjson1()$Nstates+2):(myjson1()$Nstates+1+myjson1()$Ntransitions)],
                   textArgs_transname =list(col =input$arrowcolornames, cex = input$cex, pos=3), 
                   textArgs_transfreq =list(col = input$arrowcolorfreqs, cex = input$cex, pos=1), arrowcol=input$arrowcolour,lty = 1)
      }
      
#      if (input$fill=="Yes") {
#        tfreq_n=myjson1()$frequencies[which(myjson1()$frequencies$timevar==input$uptime),2:(myjson1()$Nstates+1)]
#        total=myjson1()$frequencies[1,2]
#        rect(xleft = xvaluesb-(input$boxwidth/2), ybottom = yvaluesb-(input$boxheight/2),
#             xright = xvaluesb+(input$boxwidth/2), ytop =  yvaluesb-(input$boxheight/2) +(input$boxheight*(tfreq_n/total)), 
#             col= input$fillcolor)
#      }
      
      
      
      
    } 
    z.plot1<-function(){plotit()}
    par(mar=c(0, 0, 0, 0)) 
    p=z.plot1()
    dev.off()
  }
)   


output$downnetwork <- downloadHandler(
  
  filename= function() {
    paste(paste0("MSM","_up_to",input$uptime,"_network"),"html", sep=".") 
  },
  content= function(file) {
    #if (input$save=="png")
    #png(file, width = 700, height = 700, units = "px")
    #else pdf(file, width = 700, height = 700, units = "px")
    
    #### Start, end, Ntransitions, Nstates ####
    
    tmat=myjson1()$tmat
    
    start=which( !is.na(tmat)  ,arr.ind = TRUE)[,1]
    
    end=which( !is.na(tmat)  ,arr.ind = TRUE)[,2]
    
    Ntransitions=myjson1()$Ntransitions
    Nstates=myjson1()$Nstates
    
    ####State names#####
    
    states=vector()
    states=labels_states_box()
    
    ###########################
    
    ###transition names####
    
    trans=vector()
    trans=labels_trans_box()
    
    label_nodes= paste0(states,"\n",myjson1()$frequencies[which(myjson1()$frequencies$timevar==input$uptime) ,2:(2+(Nstates-1))])
    
    
    if (!is.null(myjson1()$frequencies)) {
      label_edge=paste0(trans,"\n", myjson1()$frequencies[which(myjson1()$frequencies$timevar==input$uptime) ,(2+Nstates):((2+Nstates)+ Ntransitions-1)])
    }
    
    if (is.null(myjson1()$frequencies)) {
      label_edge=paste0(trans,"\n", rep("",Ntransitions))
    }
    
    ###############################
    
    # customization adding more variables (see visNodes and visEdges)
    nodes <- data.frame(id = 1:Nstates, 
                        label =label_nodes,                                                   # labels
                        value = rep(3,Nstates),                                     # size 
                        shape = rep(input$shape ,Nstates),                                  # shape
                        color = rep(input$colournet,Nstates),                               # color
                        shadow =rep(TRUE,Nstates),
                        title = paste0("<p>","<br>Frequency of individuals up to the specified time point for state ", 1:Nstates,"<p>")
                        
                        
                        
    )                      
    
    if (input$putsmooth!="No") {  
      edges <- data.frame(from = start, to = end ,
                          label = label_edge ,                                        # labels
                          length =  rep(300,Ntransitions),                            # length
                          arrows = rep(input$arrowposition,Ntransitions),                            # arrows
                          dashes = rep(FALSE,Ntransitions),                           # dashes
                          smooth = list(enabled = TRUE,  type = input$putsmooth), 
                          shadow = rep(TRUE,Ntransitions),
                          font = list(size=15*input$cexnet, color=input$colourtext),
                          title = paste0("<p>","<br>Cummulative events up to the specified time point for transition ", 1:Ntransitions,"<p>")
                    
      )     
    }
    
    else if (input$putsmooth=="No") {  
      edges <- data.frame(from = start, to = end ,
                          label = label_edge ,                                        # labels
                          length =  rep(300,Ntransitions),                            # length
                          arrows = rep(input$arrowposition,Ntransitions),                            # arrows
                          dashes = rep(FALSE,Ntransitions),                           # dashes
                          smooth = rep(FALSE,Ntransitions), 
                          shadow = rep(TRUE,Ntransitions),
                          title = paste0("<p>","<br>Cummulative events up to the specified time point for transition ", 1:Ntransitions,"<p>")
                          
      )
      
    }  
    
    if (input$visI=="Fluid") {
      
      set.seed(124)
      p=visNetwork(nodes, edges,main=input$title,submain= paste0("At time ",input$uptime) )        %>%
        visEvents(startStabilizing = "function() {this.moveTo({scale:1.2})}") %>%
        visPhysics(stabilization = TRUE)  %>%
        visLayout( randomSeed = 145) %>%
        visNodes(shadow = TRUE, x=labels_x()*(-100),y=labels_y()*(100), fixed = FALSE,font = list(size=15*input$cexnet, color=input$colourtext))  %>%
        visEdges(shadow = TRUE, font = list(size=15*input$cexnet, color=input$colourtext),smooth=TRUE)
      p
    }  
    
    
    else if (input$visI!="None") {
      
      p=visNetwork(nodes, edges,height = "500px", width = "100%", main =input$title,submain= paste0("At time ",input$uptime) ) %>% 
        visIgraphLayout(layout = input$visI, physics = FALSE, smooth = FALSE, type ="full") %>% 
        visLayout( randomSeed = 145) %>%
        visInteraction(navigationButtons = TRUE, dragNodes = TRUE, dragView = TRUE, zoomView = FALSE,keyboard = TRUE,selectConnectedEdges = TRUE) %>%
        visEdges(shadow = TRUE,font = list(size=15*input$cexnet, color=input$colourtext)) %>% 
        visNodes(shadow =TRUE,font = list(size=15*input$cexnet, color=input$colourtext),smooth=TRUE)
      p
    }
    
    p   %>%
      visSave(file = file)
    
    
  }     
)   


output$statesinputbox <- renderUI({
  
  if (is.null((myjson1())))  return()
  # create some text inputs
  item_list <- list()
  
  default_choices_state=vector()
  
  for (i in 1:myjson1()$Nstates) {
    if  (is.null(myjson1()$statenames)==FALSE & length(myjson1()$statenames)==myjson1()$Nstates) { 
      default_choices_state[i]= myjson1()$statenames[i]
    }
    
    else { 
      default_choices_state[i]=paste0('State',i) }
    
    item_list[[i]] <- textInput(paste0('statebox',i),default_choices_state[i], default_choices_state[i])
    
  }
  do.call(tagList, item_list)
})

output$transitioninput <- renderUI({
  
  if (is.null((myjson1())))  return()
  # create some text inputs
  item_list <- list()
  
  default_choices_tran=vector()
  
  for (i in 1:myjson1()$Ntransitions) {
    if  (length(which(!is.na(myjson1()$transnames==TRUE)))==myjson1()$Ntransitions) { 
      default_choices_tran[i]= myjson1()$transnames[i]
    }
    
    else { 
      default_choices_tran[i]=paste0('h',i) }
    
    item_list[[i]] <- textInput(paste0('h',i),default_choices_tran[i], default_choices_tran[i])
    
  }
  do.call(tagList, item_list)
  
})

output$frequency <- renderUI({
  
  if (is.null(myjson1()$frequencies))  return()
  
  else
    
    item_list <- list()
  
  item_list[[1]] <- sliderInput("uptime","Frequencies up to time:",
                                min=min(myjson1()$frequencies$timevar),
                                max=max(myjson1()$frequencies$timevar),
                                step=(max(myjson1()$frequencies$timevar)-min(myjson1()$frequencies$timevar))/
                                  (length(myjson1()$frequencies$timevar)-1), 
                                value=myjson1()$frequencies$timevar[1], width='100%', 
                                animate=animationOptions(interval = (1000/input$speedbox)))
  
  do.call(tagList, item_list)
})

output$framebox <- renderUI({
  
  if (is.null(myjson1())|is.null(myjson1()$frequencies))  return()
  item_list <- list()
  item_list[[1]] <-numericInput("speedbox",h2("Frame speed frequency"),value=2,min=1, max=30 )
  
  
  do.call(tagList, item_list)
})

#Create the reactive input of covariates
output$boxinput <- renderUI({
  
  ### Defining the default values for boxes
  if (is.null(myjson1()))  return()
  item_list <- list()
  
  
  default_boxwidth=myjson1()$boxwidth
  default_boxheight=myjson1()$boxheight
  
  
  item_list[[1]] <- sliderInput("boxwidth","Boxwidth",
                                min=0.05,
                                max=0.5,
                                step=0.02, 
                                value=default_boxwidth)
  
  item_list[[2]] <- sliderInput("boxheight","Boxheight",
                                min=0.05,
                                max=0.5,
                                step=0.02, 
                                value=default_boxheight)
  
  item_list[[3]] <- sliderInput("cex","Size of text",                                                               
                                min=0.2,
                                max=3,
                                step=0.2, 
                                value=1)
  do.call(tagList, item_list)
})


#Create the reactive input of covariates
output$colourinput <- renderUI({
  
  default_choices=c("black","blue1","brown1","chartreuse2","cyan1","darkgray","firebrick3",
                    "gold","darkorange2","lightsteelblue4","rosybrow2","violetred2",
                    "yellow2","yellowgreen","tan1","lightslateblue","khaki4","gray28",
                    "cyan3","brown4","darkorchid1","goldenrod4","gray63","lightsalmon",
                    "maroon4","palegreen1","royalblue2","red2","sienna4","yellow4","slategray3")
  
  ### Defining the default values for boxes
  if (is.null(myjson1()))  return()
  item_list <- list()
  
  item_list[[1]] <- selectInput("boxcolornames",  "Colour of box text", default_choices, selected = default_choices[1])
  item_list[[2]] <- selectInput("boxcolorfreqs",  "Colour of box frequencies", default_choices, selected = default_choices[1])
  item_list[[3]] <- selectInput("arrowcolornames","Colour of arrows text", default_choices, selected = default_choices[1])
  item_list[[4]] <- selectInput("arrowcolorfreqs","Colour of arrows freq", default_choices, selected = default_choices[1])
  item_list[[5]] <- selectInput("arrowcolour",    "Colour of arrows colour",c("red",default_choices))
  
  
  do.call(tagList, item_list)
})



#Create the reactive input of covariates
#output$fillcolor <- renderUI({
#  
#  if (is.null((myjson1())))  {return()}
#  
#  else if (!is.null((myjson1())))  {
#    
#    if (input$fill=="No") { return()}
#    
#    else if (input$fill=="Yes") {
#      
#      default_choices=vector()
#      
#      default_choices[1]=rgb(0,0,1.0,alpha=0.1)
#      default_choices[2]=rgb(0,1.0,0,alpha=0.1)
#      default_choices[3]=rgb(1.0,0,0,alpha=0.1)
#      default_choices[4]=rgb(0.5,0.5,0.5,alpha=0.1)
#      
#      item_list <- list()
#      
#      item_list[[1]] <- selectInput("fillcolor",  "Colour of box fill", choices=default_choices, selected =default_choices)
#      
#      do.call(tagList, item_list)
#    }
#  }
#})

output$boxinput2 <- renderUI({
  
  ### Defining the default values for boxes
  if (is.null(myjson1()))  return()
  item_list <- list()
  
  v_x=vector()
  for (i in 1:length(myjson1()$xvalues)) {
    v_x[i]=myjson1()$xvalues[i]+myjson1()$boxwidth/2
  }
  default_choices_x=v_x
  
  v_y=vector()
  for (i in 1:length(myjson1()$yvalues)) {
    v_y[i]=myjson1()$yvalues[i]-myjson1()$boxheight/2
  }
  default_choices_y=v_y
  
  p=0
  for (i in 1:myjson1()$Nstates) {
    item_list[[i+p]] <- sliderInput(paste0('x',i),paste0("Select x centre for box ",i),
                                    min=input$boxwidth/2,
                                    max=1-input$boxwidth/2,
                                    step=1/100, 
                                    value=default_choices_x[i])
    item_list[[(i+1)+p]] <- sliderInput(paste0('y',i),paste0("Select y centre for box ",i),
                                        min=input$boxheight/2,
                                        max=1-input$boxheight/2,
                                        step=1/100, 
                                        value=default_choices_y[i])  
    p=p+1
  }
  
  
  
  
  
  do.call(tagList, item_list)
})


labels_states_box<- reactive ({
  myList<-vector("list",(myjson1()$Nstates))
  for (i in 1:(myjson1()$Nstates)) {
    myList[[i]]= input[[paste0('statebox',i)]][1]
  }
  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
  final_list
})

labels_trans_box<- reactive ({
  myList<-vector("list",myjson1()$Ntransitions)
  for (i in 1:myjson1()$Ntransitions) {
    myList[[i]]= input[[paste0('h',i)]][1]
  }
  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
  final_list
})

labels_x<- reactive ({
  myList<-vector("list",myjson1()$Nstates)
  for (i in 1:myjson1()$Nstates) {
    myList[[i]]= input[[paste0('x',i)]][1]
  }
  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
  final_list
})

labels_y<- reactive ({
  myList<-vector("list",myjson1()$Nstates)
  for (i in 1:myjson1()$Nstates) {
    myList[[i]]= input[[paste0('y',i)]][1]
  }
  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
  final_list
})

output$message_1 <- renderText({labels_y})



output$msm_scheme_interactive <- renderPlot ({ 
  
  if(is.null(myjson1())) {return()}
  else
    
  ntransitions=myjson1()$Ntransitions
  nstates= myjson1()$Nstates
  
  xvaluesb=labels_x()  #+boxwidth/2
  yvaluesb=labels_y()  #-boxheight/2
  
  boxes=msboxes_R_nofreq(yb=yvaluesb, xb=xvaluesb, boxwidth=input$boxwidth , boxheight=input$boxheight, tmat.= myjson1()$tmat)
  
  
  #Read through json from msboxes or through a new function 
  x1=boxes$arrows$x1
  y1=boxes$arrows$y1
  x2=boxes$arrows$x2
  y2=boxes$arrows$y2
  arrowstextx=boxes$arrowstext$x
  arrowstexty=boxes$arrowstext$y
  #######################################################
  
  
  
  tname=vector()
  tname=labels_trans_box()
  
  statename=vector()
  statename=labels_states_box()
  
  
  
  if (is.null(myjson1()$frequencies)) {
    
    
    plotit<-function(){
      
      plot(c(0, 1), c(0, 1), type = "n", ylab='',xlab='', xaxt='n', yaxt='n', pch=30)
      title(main = input$title,  line = -1, cex.main =input$cex)
      #text(0.05, 1, paste0("At time"," ",input$uptime),cex = input$cex)
      ### Call the box function
      recttext(xcenter=xvaluesb, ycenter=yvaluesb, 
               boxwidth=input$boxwidth, boxheight=input$boxheight,statename=statename,
               freq_box=c(rep("",nstates)),
               rectArgs = list(col = 'white', lty = 'solid'),
               textArgs_state = list(col = input$boxcolornames, cex = input$cex,pos=3),
               textArgs_freq  = list(col = input$boxcolorfreqs, cex = input$cex,pos=1))
      
      ### Call the arrows function
      arrows_msm(xstart=x1, ystart=y1,xend=x2, yend=y2,xtext=arrowstextx, ytext=arrowstexty,tname=tname,
                 tfreq=c(rep("",ntransitions)),
                 textArgs_transname =list(col =input$arrowcolornames, cex = input$cex, pos=3), 
                 textArgs_transfreq =list(col = input$arrowcolorfreqs, cex = input$cex, pos=1), arrowcol=input$arrowcolour,lty = 1)
      
    }
    
  }
  
  
  if (!is.null(myjson1()$frequencies)) {
    
    ###################################################################################
    plotit<-function(){
      plot(c(0, 1), c(0, 1), type = "n", ylab='',xlab='', xaxt='n', yaxt='n', pch=30)
      title(main = input$title,  line = -1, cex.main =input$cex)
      text(0.05, 1, paste0("At time"," ",input$uptime),cex = input$cex)
      ### Call the box function
      recttext(xcenter=xvaluesb, ycenter=yvaluesb, 
               boxwidth=input$boxwidth, boxheight=input$boxheight,statename=statename,
               freq_box=myjson1()$frequencies[which(myjson1()$frequencies$timevar==input$uptime),2:(myjson1()$Nstates+1)],
               rectArgs = list(col = 'white', lty = 'solid'),
               textArgs_state = list(col = input$boxcolornames, cex = input$cex,pos=3),
               textArgs_freq  = list(col = input$boxcolorfreqs, cex = input$cex,pos=1))
      
   #   if ( is.null(myjson1()$frequencies)) {
   #     ### Call the arrows function
   #     arrows_msm(xstart=x1, ystart=y1,xend=x2, yend=y2,xtext=arrowstextx, ytext=arrowstexty,tname=tname,
   #                tfreq=matrix(nrow=1, ncol=ntransitions,""),
   #                textArgs_transname =list(col =input$arrowcolornames, cex = input$cex, pos=3), 
   #                textArgs_transfreq =list(col = input$arrowcolorfreqs, cex = input$cex, pos=1), arrowcol=input$arrowcolour,lty = 1)
   #     
   #   }
      
      #if ( !is.null(myjson1()$frequencies)) {
        ### Call the arrows function
        arrows_msm(xstart=x1, ystart=y1,xend=x2, yend=y2,xtext=arrowstextx, ytext=arrowstexty,tname=tname,
                   tfreq=myjson1()$frequencies[which(myjson1()$frequencies$timevar==input$uptime),(myjson1()$Nstates+2):(myjson1()$Nstates+1+myjson1()$Ntransitions)],
                   textArgs_transname =list(col =input$arrowcolornames, cex = input$cex, pos=3), 
                   textArgs_transfreq =list(col = input$arrowcolorfreqs, cex = input$cex, pos=1), arrowcol=input$arrowcolour,lty = 1)
      
      
      
   #   if (input$fill=="Yes") {
   #     tfreq_n=myjson1()$frequencies[which(myjson1()$frequencies$timevar==input$uptime),2:(myjson1()$Nstates+1)]
   #     total=myjson1()$frequencies[1,2]
   #     rect(xleft = xvaluesb-(input$boxwidth/2), ybottom = yvaluesb-(input$boxheight/2),
   #          xright = xvaluesb+(input$boxwidth/2), ytop =  yvaluesb-(input$boxheight/2) +(input$boxheight*(tfreq_n/total)), 
   #          col= input$fillcolor)
   #   }
    }  
    
  }
  
  z.plot1<-function(){plotit()}
  par(mar=c(2, 2, 2, 2)) 
  p=z.plot1()
  
  # z.plot_prob<-function(){plotit_prob()}
  # png("MSM.png", width = 600, height = 600, units = "px")
  # par(mar=c(0, 0, 0, 0)) 
  # p=z.plot_prob()
  # dev.off()
  
  
  p
  
})


output$msm_scheme_network <- renderVisNetwork ({ 
  
  #### Start, end, Ntransitions, Nstates ####
  
  tmat=myjson1()$tmat
  
  start=which( !is.na(tmat)  ,arr.ind = TRUE)[,1]
  
  end=which( !is.na(tmat)  ,arr.ind = TRUE)[,2]
  
  Ntransitions=myjson1()$Ntransitions
  Nstates=myjson1()$Nstates
  
  ####State names#####
  
  states=vector()
  states=labels_states_box()
  
  ###########################
  
  ###transition names####
  
  trans=vector()
  trans=labels_trans_box()
  
  
  
  label_nodes= paste0(states,"\n",myjson1()$frequencies[which(myjson1()$frequencies$timevar==input$uptime) ,2:(2+(Nstates-1))])
  
  if ( !is.null(myjson1()$frequencies)) {
    label_edge=paste0(trans,"\n", myjson1()$frequencies[which(myjson1()$frequencies$timevar==input$uptime) ,(2+Nstates):((2+Nstates)+ Ntransitions-1)])
  }
  
  if ( is.null(myjson1()$frequencies)) {
    label_edge=paste0(trans,"\n", rep("",Ntransitions))
  }
  
  ###############################
  
  # customization adding more variables (see visNodes and visEdges)
  nodes <- data.frame(id = 1:Nstates, 
                      label =label_nodes,                                                   # labels
                      value = rep(3,Nstates),                                     # size 
                      shape = rep(input$shape ,Nstates),                                  # shape
                      color = rep(input$colournet,Nstates),                               # color
                      shadow =rep(TRUE,Nstates),
                      title = paste0("<p>","<br>Frequency of individuals up to the specified time point for state ", 1:Nstates,"<p>")
                      
  )                      
  
  if (input$putsmooth!="No") {  
    edges <- data.frame(from = start, to = end ,
                        label = label_edge ,                                        # labels
                        length =  rep(300,Ntransitions),                            # length
                        arrows = rep(input$arrowposition,Ntransitions),                            # arrows
                        dashes = rep(FALSE,Ntransitions),                           # dashes
                        smooth = list(enabled = TRUE,  type = input$putsmooth), 
                        shadow = rep(TRUE,Ntransitions),
                        font = list(size=15*input$cexnet, color=input$colourtext),
                        title = paste0("<p>","<br>Cummulative events up to the scecified time point for transition ", 1:Ntransitions,"<p>")
                        
    )     
  }
  
  else if (input$putsmooth=="No") {  
    edges <- data.frame(from = start, to = end ,
                        label = label_edge ,                                        # labels
                        length =  rep(300,Ntransitions),                            # length
                        arrows = rep(input$arrowposition,Ntransitions),                            # arrows
                        dashes = rep(FALSE,Ntransitions),                           # dashes
                        smooth = rep(FALSE,Ntransitions), 
                        shadow = rep(TRUE,Ntransitions),
                        title = paste0("<p>","<br>Cummulative events up to the scecified time point for transition ", 1:Ntransitions,"<p>")
                        
    )
    
  }  
  
  
  if (input$visI=="Fluid") {
    
    
    set.seed(124)
    p= visNetwork(nodes, edges,main=input$title,submain= paste0("At time ",input$uptime) )        %>%
      visEvents(startStabilizing = "function() {this.moveTo({scale:1.2})}") %>%
      visPhysics(stabilization = TRUE)  %>%
      visLayout( randomSeed = 145) %>%
      visNodes(shadow = TRUE, x=labels_x()*(-100),y=labels_y()*(100), fixed = FALSE,font = list(size=15*input$cexnet, color=input$colourtext))  %>%
      visEdges(shadow = TRUE, font = list(size=15*input$cexnet, color=input$colourtext),smooth=TRUE)
    
    p
    
  }  
  
  
  else if (input$visI!="None") {
    
    if (input$putsmooth=="No") {
    
         p= visNetwork(nodes, edges,height = "500px", width = "100%", main =input$title,submain= paste0("At time ",input$uptime) ) %>% 
           visIgraphLayout(layout = input$visI, physics = FALSE, smooth = FALSE, type ="full") %>% 
           visLayout( randomSeed = 145) %>%
           visInteraction(navigationButtons = TRUE, dragNodes = TRUE, dragView = TRUE, zoomView = FALSE,keyboard = TRUE,selectConnectedEdges = TRUE) %>%
           visEdges(shadow = TRUE,font = list(size=15*input$cexnet, color=input$colourtext)) %>% 
           visNodes(shadow =TRUE,font = list(size=15*input$cexnet, color=input$colourtext))
    
        p
    }
    
    else if (input$putsmooth!="No") {
      
      p= visNetwork(nodes, edges,height = "500px", width = "100%", main =input$title,submain= paste0("At time ",input$uptime) ) %>% 
        visIgraphLayout(layout = input$visI, physics = FALSE, smooth = FALSE, type ="full") %>% 
        visLayout( randomSeed = 145) %>%
        visInteraction(navigationButtons = TRUE, dragNodes = TRUE, dragView = TRUE, zoomView = FALSE,keyboard = TRUE,selectConnectedEdges = TRUE) %>%
        visEdges(shadow = TRUE,font = list(size=15*input$cexnet, color=input$colourtext),smooth=TRUE) %>% 
        visNodes(shadow =TRUE,font = list(size=15*input$cexnet, color=input$colourtext))
    
    p 
    }
    
  }
})




