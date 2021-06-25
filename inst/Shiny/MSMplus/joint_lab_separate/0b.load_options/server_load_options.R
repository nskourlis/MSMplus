


#output$loadtypeUI <- renderUI({
#  
#  radioButtons(inputId="loadtype", label= "File type (csv or json)",
#               choices=c("json","csv"),selected = "json")
#  
#})
#
#output$aimtypeUI <- renderUI({
#  
#  div(radioButtons(inputId="aimtype", label= "Aim",
#               choices=c("Single multistate model"="present","Compare two multistate models"="compare"),selected = "present", inline=FALSE, width="100%"),
#      style = "text-align: left; margin-right: 3px;")
#  
#})
#
#


output$message_rules1<- renderUI ({
  
  message = p(withMathJax(
    helpText(strong('Introduction')),
    
    helpText("Multistate models (MSM) are used in a variety of epidemiological settings, enabling the study of individuals 
              through different disease states. Studying acute or chronic disease progression,
              recurrent events such as repeated hospitalisations are typical examples of MSM use. When studying such processes,
              MSM are used in order to portray accurately,
              with sufficient complexity the real- world issue under study and provide useful and meaningful predictions."),
    
    helpText(strong('Aim of MSMplus')),
    
    
    helpText("MSMplus is a usefull tool for presentation of results from a multi-state analysis in an easy, comprehensible and meaningful way.
              aiding the user to present the research findings to the targeted audience. A secondary use of the application is that it can 
              contrast the results of two different modelling approaches for the same multi-state setting (e.g clock-reset versus clock forward approach.)")
   
     
  )
)
  message
  
  return(list(message ))
  
})

to_url1 <- a("Click here for guidance", href="https://nskbiostatistics.shinyapps.io/tabs/")

output$to_url1 <- renderUI({
  
  tagList(helpText(strong("First time using MSMplus?")), to_url1)
})

output$intro_tutorial <- renderUI({
    
    message = p(withMathJax(
      helpText(strong('How to upload your results')),
      helpText("The descriptive and analysis results of the multistate analysis can be supplied either as json files that
             automatically derived for specific packages in Stata and R, or as manually provided csv files of specific structure.
             A tutorial for the successful generation of the input files required for the application, including examples
             in Stata and R, plus the R functions code for R, is provided in the following link:")
    )
    )
    return(list(message ))
  })  
  
  

to_url2 <- a("Tutorial on preparing the input files", href="https://nskbiostatistics.shinyapps.io/supplementary/")

output$to_url2 <- renderUI({
  
  tagList("URL link:", to_url2)
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