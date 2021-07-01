to_url_backjson <- a("Back to MSMplus app", href="https://nskbiostatistics.shinyapps.io/MSMplus/")

output$to_url_backjson <- renderUI({
  
  tagList( to_url_backjson)
})




output$message_json <- renderUI({
  
  
  message = withMathJax(
    helpText(strong("What is JSON?")),
    helpText("JSON stands for JavaScript Object Notation. It is a lightweight format for storing and transporting data and is often used when data is sent from a server to a web page."),
    helpText(strong("JSON files to be uploaded at MSMplus")),
    helpText("The results from a multistate analysis can be packed up into two Json files that will serve as input to the MSMplus app."),
    helpText(HTML("<ul><li>The first Json file is the 'Descriptive Json file' and contains the basic information for the multistate setting of interest
             (names of states and transitions, transition matrix, attributes of the boxes of states etc.). More detail can be found in the 'Descriptive Json file' tab.</li></ul>")),
    helpText(HTML("<ul><li>The second Json file is the 'Results Json file' and contains the results of the multistate analysis for the covariate patterns 
             of interest (transition probabilities, transition intensities etc.). More detail about its content is found in the
              'Results Json file' tab. </li></ul>")),
    helpText(strong("How to derive the Json files needed for the app")),
    helpText(HTML('<ul><li>For Stata, command specific options have been developed for commands msboxes and predictms that automatically derive the required json files.
                 The msboxes and predictms commands are available in Stata by installing the multistate package (net install multistate, from("https://www.mjcrowther.co.uk/code/multistate/") 
                  and then by typing interactive jsonpath("filepath") the json input files are automatically produced. </li></ul>')),
    helpText(HTML('<ul><li>For R, we developed an R package in Github (nskourlis/MSMplus), which contains R wrapper functions that can be used to derive the json input files. Function msboxes_R
              creates the "Descriptives Json file" needed for creating the multi-state graph. If the user wish to use the flexsurv package for his analysis, flexsurvjon() calls the functions of the flexsurv
              package internally and produces the analysis results as a json file. The same can be done if the user wishes to use the mstate package or the msm package by calling functions 
              mstatejson() and msmjson(). However, we strongly support the manual preparation of a csv file for the use of the MSMplus app, as it does not impose any limitations 
              regarding the statistical analysis, in comparison with the automatically derived functions which impose on the user specific R packages </li></ul>')),
    helpText("Examples of how to conduct the analysis and derive the Json files are given the 'Analysis example' tabs.")
    
    
    )
  
  message
  
  return(list(message ))
  
})

output$message_json1 <- renderUI({
  
  
  message = withMathJax(
    helpText("The 'Descriptive Json file' and contains the basic information for the multistate setting of interest") ,
    helpText(strong('Contents the "Descriptive Json file"')),
    helpText(HTML('<ul><li> Number of states ("Nstates") </li></ul>')),
    helpText(HTML('<ul><li> Number of transitions ("Ntransitions")</li></ul>')),
    helpText(HTML('<ul><li> Initial x and y coordinates for the boxes of the multistate graph ("xvalues", "yvalues")</li></ul>')),
    helpText(HTML('<ul><li> Initial width and height of the multistate graph boxes ("boxwidth", "boxheight")</li></ul>')),
    helpText(HTML('<ul><li> Names of the states ("statenames")</li></ul>')),
    helpText(HTML('<ul><li> Names of the transitions ("transnames")</li></ul>')),
    helpText(HTML('<ul><li> the transition matrix ("tmat")</li></ul>')),
    helpText(HTML('<ul><li> A matrix containing the frequency of individuals across time and the cummulative number of individuals
                  that have experienced each transition across time ("frequencies"). This matrix can easily be derived
                  using command msboxes in Stata (specifying the options interactive and json) and in R by using the function msboxes_R.
                  This matrix is optional and is not required to be specified.</li></ul>')),
    helpText(strong("Example of 'Descriptive Json file'"))
    
  )
  message
  
  return(list(message ))
  
})


output$message_json2 <- renderUI({
  
  
  message = withMathJax(
    helpText(strong("The 'Results Json file'  contains the results of the multistate analysis ")),
    helpText(" The analysis results json file provides the statistical results of the MSM analysis to the application. 
               It consists of some mandatory elements while the rest are optional, and depend on the analysis. 
               The time points and a list of the covariate patterns (and number) for which the predictions were made is mandatory.
               The rest of the estimates are optional, meaning that the application will be able to run, but only visualizing the 
               information that it is being given. Differences and ratios of the estimated measures as well as 
               confidence intervals are also supported by MSMplus. "),
    helpText(strong('Contents the "Results Json file"')),
    helpText(HTML('<ul><li> The time points of prediction ("timevar") </li></ul>')),
    helpText(HTML('<ul><li> Number of covariate patterns ("Nats")</li></ul>')),
    helpText(HTML('<ul><li> Names of covariate patterns("atlist")</li></ul>')),
    helpText(HTML('<ul><li> Number of transitions ("Ntransitions")</li></ul>')),
    helpText(HTML('<ul><li> The transition matrix ("tmat")</li></ul>')),
    helpText(HTML('<ul><li> The predicted measures as lists of vectors. Vectors of each measure across the prediction time points,
                            contained in a list for the different covariate patterns of interest.
                  <ul><li>P(_diff/_ratio)_*_to_*(_uci/_lci)</li></ul>
                  <ul><li>h*_from_* </li></ul>
                  <ul><li>Los(_diff/_ratio)_*_to_*(_uci/_lci)</li></ul>
                  <ul><li>Visit(_diff/_ratio)_*_to_*(_uci/_lci)</li></ul>
                  <ul><li>User(_diff/_ratio)_*_to_*(_uci/_lci)</li></ul>
                  <ul><li>Next_*_to_*(_uci/_lci)</li></ul>
                  <ul><li>Number_*_to_*(_uci/_lci)</li></ul>
                  <ul><li>First_*(_uci/_lci)</li></ul>
                  <ul><li>Soj_*(_uci/_lci)</li></ul>
                  </li></ul>')),
    helpText("For more details regarding the naming rules of the estimated measures refer to the publication paper of MSMplus. ")
    
)
    
  
  message
  
  return(list(message ))
  
})




output$json_file1<- renderPrint({
  

    data= fromJSON("material/msboxes.json", flatten=FALSE)
    data$frequencies=data$frequencies[1:5,]
    export= toJSON(data, pretty = TRUE,force = TRUE, na='string')
    export
    
    return(list( export ))
})


output$json_file2<- renderPrint({
  

    
    data= fromJSON("material/predictions_shortened.json", flatten=FALSE)
    data$frequencies=data$frequencies[1:5,]
    export= toJSON(data, pretty = TRUE,force = FALSE, na='string')
    export

  
})


