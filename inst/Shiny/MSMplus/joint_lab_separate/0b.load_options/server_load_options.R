


#output$plop <- renderImage({
# list(src ="MSM_up_to0_int.png")
#})
#
#output$plop <- renderImage({
#  # A temp file to save the output. It will be deleted after renderImage
#  # sends it, because deleteFile=TRUE.
#  outfile <- tempfile(fileext='.png')
#  
#  # Generate a png
#  png(outfile, width=400, height=400)
#  hist(rnorm(input$n))
#  dev.off()
#  
#  # Return a list
#  list(src = outfile,
#       alt = "This is alternate text")
#}, deleteFile = TRUE)
#


output$message_rules1<- renderUI ({
  
  message = p(withMathJax(
    helpText(strong('Introduction')),
    
    helpText("A regular survival analysis setting consists of two states, a starting state (e.g bone marrow transplantation) and an absorbing state (e.g relapse or death).
             Multi-state models are complex survival analysis settings with one or more intermediate states between the starting state(s) and the absorbing state(s) that allow us
             to study a real-world issue  with sufficient complexity and provide useful and meaningful predictions."),
  
    helpText("\n"),
    helpText("Examples"),
   helpText(HTML("<ul><li> An intermediate state of Platelet recovery between the starting state of the bone marrow transplantation and the absorbing state of Relapse/Death </li></ul>")),   
   helpText(HTML("<ul><li> Recurrent intermediate states of hospitalizations between the starting and the absorbing state </li></ul>")), 
   helpText(HTML("<ul><li> Intermediate stages of renal disease (Stages II-IV) between the starting state (early stage of renal dysfunction) and the absorbing state (death) </li></ul>")) 
  )
  )
   message
   
   return(list(message ))
   
})



output$message_rules2<- renderUI ({
  
  message = p(withMathJax(
    helpText(strong('Aim of MSMplus')),
    
    
    helpText("The presentation of multi-state analyses results can be challenging due to the variety of measures that can describe the multi-state process (e.g transition probabilities, 
    transition intensities, length of stay in each state, probability of visiting each state etc). In addition, many of these measures tend to be 
    functions of time, adding to the challenge of the presentation of results." ),
    
   helpText(HTML("<ul><li>MSMplus aims to help the user to present multi-state analysis results in an easy, comprehensible and meaningful way as well as compare results from two different multi-state
                 approaches. </li></ul>") )
  )
)
  message
  
  return(list(message ))
  
})


output$message_rules3<- renderUI ({
  
  message = p(withMathJax(
    helpText(strong('Example dataset: EBMT')),
    
    
    helpText("The data originate from the European Blood and Marrow Transplant registry.\nThe dataset consists of 2204 patients transplanted at the EBMT between 1995 and 1998.\n
             The three states a patient can be in is 1) Post- transplant, 2) Platelet recovery 3) Relapse/Death.\n There are 3 possible transitions: 
             From Transplant to Platelet recovery (Transition 1), from Transplant directly to Relapse/Death (Transition 2) and from a state of Platelet Recovery to Relapse/Death (Transition 3).\n
             The covariate patterns used in this example are the 3 age categories, namely <20 y.old, 20-40 y.old and >40 y.old."),
    
    helpText('If the user clicks "Yes" on the Example option of the "Load" tab, the graph of this multi-state setting example will be automatically displayed on the "Model Structure" tab.
                    Moreover, the results of a multi-state analysis using flexible parametric models to model each transition will automatically be uploaded to the app so that the user can view
                    the interactive graphs of all the estimated measures of interest.')

  )
  )
  message
  
  return(list(message ))
  
})

to_url1 <- a("Click here for help", href="https://nskbiostatistics.shinyapps.io/tabs/")

output$to_url1 <- renderUI({
  
  tagList(helpText(strong("First time using MSMplus?")), to_url1)
})

output$intro_tutorial <- renderUI({
    
    message = p(withMathJax(
      helpText(strong('How to upload your results')),
      helpText("The multi-state graph summary information and the analysis results of the multi-state analysis can be supplied to MSMplus either as
             Json files automatically derived via specific packages in Stata and R, or as manually provided csv/excel files with specific naming and formatting rules.")
    )
    )
    return(list(message ))
  })  
  
  

to_url2 <- a("Tutorial on preparing the input files", href="https://nskbiostatistics.shinyapps.io/supplementary/")

output$to_url2 <- renderUI({
  
  tagList("URL link:", to_url2)
})



output$message_rules_grey<- renderUI({
  
  message = p(withMathJax(
    #helpText("The tabs related to the multi-state measures remain grey/disabled until the 'Settings' tab is selected. If no estimation has been provided for a measure, the respective tab will remain disabled.")),
    helpText(p("Attention: After loading the results input file, the user has to visit the Settings tab before proceeding to the tabs with the measures, otherwise the tabs will stay empty.", style = "color:red"))
    )
  
  
  )
  
  message
  
  return(list(message))
  
})



#output$json_example1<- renderPrint({
#  if (input$loadtype=="json" ) {
#     data= fromJSON("joint_lab_separate/0.example_data/msboxes.json", flatten=FALSE)
#     data$frequencies=data$frequencies[1:5,]
#     export= toJSON(data, pretty = TRUE,force = TRUE, na='string')
#     export
#     
#  }
#
#  else if (input$loadtype=="csv" ) {
#    
#    data= read.csv2("joint_lab_separate/0.example_data/frequencies_toy.csv",header=TRUE, sep=";")
#    
#    data=data[1:2,]
#    data
#  }
#
#})
#
#
#output$json_example2<- renderPrint({
#  
#   if (input$loadtype=="json" ) {
#     
#     data= fromJSON("joint_lab_separate/0.example_data/predictions_shortened.json", flatten=FALSE)
#     data$frequencies=data$frequencies[1:5,]
#     export= toJSON(data, pretty = TRUE,force = FALSE, na='string')
#     export
#   }
#
#   else if (input$loadtype=="csv" ){
#     
#     data= read.csv2("joint_lab_separate/0.example_data/results_toy.csv",header=TRUE, sep=";")
#     data=data[1:2,]
#     names(data)
#   }
#
#})
#
#