
output$colourinput<- renderUI ({
  
  if (is.null(input$json2))
    return()
  


  item_list <- list()
  
  item_list[[1]]<- "Select colour for each probability"
  
  default_choices_colour=vector()
  default_choices_colour=c("blue1","brown1","chartreuse2","cyan1","darkgray","firebrick3",
                           "gold","darkorange2","lightsteelblue4","rosybrow2","violetred2",
                           "yellow2","yellowgreen","tan1","lightslateblue","khaki4","gray28",
                           "cyan3","brown4","darkorchid1","goldenrod4","gray63","lightsalmon",
                           "maroon4","palegreen1","royalblue2","red2","sienna4","yellow4","slategray3")
  
  colour_choices_title=vector("character",length = length(myjson2()$P ) )
  
  for (i in 1:length(myjson2()$P)) {
    colour_choices_title[i]=paste0("Colour"," ",i)
  }
    
    
  for (i in 1:length(myjson2()$P)) {
    
    item_list[[1+i]] <- selectInput(paste0('colour',i),label=colour_choices_title[i],
                                    choices= default_choices_colour, selected =default_choices_colour[i] )
    
  }
  
  do.call(tagList, item_list)
  
})

labels_colour<- reactive ({
  
  myList<-vector("list",length(myjson2()$P))
  for (i in 1:length(myjson2()$P)) {
    
    myList[[i]]= input[[paste0('colour', i)]][1]
    
  }
  
  final_list=unlist(myList, recursive = TRUE, use.names = TRUE)
  final_list
})



#output$Welcome <- renderPlot({
#
#  msm_box=load.image("msm_box.png")%>%
#    plot(rescale=TRUE)
#  
#  
#  p=ggdraw(plot = NULL, xlim = c(0, 1), ylim = c(0, 1))
#  
#  p=p+ draw_image(msm_box, x = 0, hjust = 0, width = 1, height = 1)
#  
#  p
#    
#  })
