to_url_backtab <- a("Back to MSMplus app", href="https://nskbiostatistics.shinyapps.io/MSMplus/")

output$to_url_backtab <- renderUI({
  
  tagList(to_url_backtab)
})


output$message_use_general <- renderUI({
  
  
  message = withMathJax(
    helpText(strong("General")),
    helpText("While using MSMplus, it is important to keep in mind some tips/limitations:"),
    
    helpText(HTML('<ul><li> If MSMplus is open in an Internet Explorer browser and the user does not perform an action on the application within a certain time window (e.g 5 minutes), 
                            there is a possibility that the
                            app will stop working and the refresh button will have to be used.</li></ul>')),
    
    helpText(HTML('<ul><li> When selecting a tab, it is important to give the tab a few seconds to fully load all the features (especially the "Settings" tab).
                            If not, there is a possibility that the app will not have the time to read crucial inputs, resulting in graphs not appearing.</li></ul>'))
    
    
  )
  message
  
  return(list(message ))
  
})

output$message_use_load_1 <- renderUI({
  
  
  message = withMathJax(
    helpText(strong("Tab load")),
    helpText("In this tab, the user specifies the type of input files to be uploaded (either json or csv) as well as the aim of the application use
             (either presenting the results of a multi-state analysis or comparing the results from two different multi-state approaches)."),
    
    
    
    helpText('The json files can be automatically derived via commands msboxes and predictms in Stata, by specifying the options interactive and jsonpath.
            The json files can also be derived in R by using functions of the MSMplus package (Github: nskourlis/MSMplus) developed to be compatible with packages msm, mstate and flexsurv. '),
    
    
    helpText('Alternatively, the user can supply csv input files with specific structure and naming rules. We consider this choice more laborious but also very flexible,
            as it poses no limitations to the models or statstical softwares used to perform the multi-state analysis. More info on the structure and derivation of the json and csv input files can be found in the link below:')
  )
  message
  
  return(list(message ))
  
})

to_url3 <- a("Tutorial on preparing the input files", href="https://nskbiostatistics.shinyapps.io/supplementary/")

output$to_url3 <- renderUI({
  
  tagList( to_url3)
})


output$message_use_load_2 <- renderUI({
  
  
  message = withMathJax(
    
    
    helpText("The aim of using MSMplus has to be specified. If the aim is to present the results of a multi-state analysis, the user will be called to 
             upload one json (or csv) file for the descriptive part of the app (optional) and one json (or csv) file for the predictions of the analysis.
             If the aim is to compare the predictions from two different approaches, then the user will be called to upload 2 json (or csv) files of predicted measures,
             one containing the results of the first approach and one containing the results of the second approach."),
    
    helpText("Advise: Take your time to ensure that the uploaded files are correct, especially if you have selected the csv file input option. 
              We strongly encourage the user run the Examples provided by the MSMplus package (Github: nskourlis/MSMplus), in order
              to get accustommed with the procedure of creating the json files and gain confidence while using them.")
    
  )
  message
  
  return(list(message ))
  
})

output$message_use_Inter <- renderUI({
  
  
  message = withMathJax(
    helpText(strong("Tab Interpretation")),
    helpText("In this tab, definitions and interpretations of measures predicted in multi-state analyses are given. We encourage the user to visit this tab if 
              there is a need for better understanding of the meaning of each measure."),
    helpText(HTML('<ul><li> Transition probabilities </li></ul>')),
    helpText(HTML('<ul><li> Transition intensity rates </li></ul>')),
    helpText(HTML('<ul><li> Length of stayin each state </li></ul>')),
    helpText(HTML('<ul><li> Probability of visiting a state </li></ul>')),
    helpText(HTML('<ul><li> Extra measures </li></ul>'))
   # helpText(HTML('<ul><li> Robustness </li></ul>'))
    
  )
  message
  
  return(list(message ))
  
})




output$message_use_schema <- renderUI({
  
  
  message = withMathJax(
    
    helpText(strong('Multi-state graph')),
    helpText("The user can choose between two different ways of generating the multi-state graph.
             The first way produces a static graph while the second way an interactive one. In the static graph the user
             can change the dimensions and positions of the states-boxes via slider inputs that appear in the customization adjustments.
             The interactive graph can be changed by the user by clicking on a state box and directly altering the positions of the boxes.
             A set of customization adjustments allow for different shapes for the states (box, circle, oval) and different shapes
             for the arrows (straight, spline)."),
    
    helpText(strong('Customization adjustments')),
    helpText("The customization adjustments are optional. However they can help the user in giving the multi-state graph the desired appearance.
             Regarding the static graph, the customization adjustments include mostly sliders for defining the boxwidth, boxheight and coordinates 
             of the states-boxes as well colouring of the arrows and text. Similarly, the customization adjustments for the interactive graph 
             are related to the shape and colour of the boxes, the color and trajectory of the arrows, and whether the layout is more static
             of floating when the user tries to change the position of a certain state-box."),
    
    
    helpText(strong('While using the interactive graph')),
    helpText("If the user is interested in seeing the number of people in each state across time and the number people who have experienced each transition,
              it is better to select the fluid layout. If the user is interested in setting the states shapes in exact positions,
             then it is better to first select the time point of interest (if state frequency information is available) and then fix the 
             graph the way that is prefered using any layout other than fluid.")
    
    
  )
  message
  
  return(list(message ))
  
})


output$message_use_settings <- renderUI({
  
  
  message = withMathJax(
    
    helpText(strong('Settings for predictions tab')),
    helpText("This tab includes settings for the tabs with the prediction measures. A range of settings can be defined in this step such as:"),
    
    helpText(HTML('<ul><li> Select the starting state. There are cases whre an individual may start drom an intermediate state. </li></ul>')),
    helpText(HTML('<ul><li> Select the covariate patterns of interest. If there are predictions for many covariate patterns,
                   the user can select a subset of them for which the predicted measures will be visualized.</li></ul>')),
    
    helpText("After the choices above, extra choices can be specified such as: "),
    
    helpText(HTML('<ul><li> Smooth transition between covariate pattenrns. </li></ul>')),
    helpText(HTML('<ul><li> Colouring options for the lines that will represent different states, covariate patterns and transitions </li></ul>')),
    helpText(HTML('<ul><li> Naming options in case the user wants to give specific names for the different states and covariate patterns. </li></ul>')),
    helpText(HTML('<ul><li> Figures scaling </li></ul>')),
    
    helpText(strong('Note')),
    helpText("Make sure that you give the app enough time to fully present the Settings tab before moving to the next tabs.
             This is crucial, as the app reads in all the inputs of the currrent app. If the user neglects this tab or quickly moves to 
             the next one, crucial information may not be read by MSMplus, resulting in figures not appearing in the following tabs.")
    
    
  )
  message
  
  return(list(message ))
  
})

output$message_use_predictions <- renderUI({
  
  
  message = withMathJax(
    
    helpText(strong('Prediction tabs')),
    helpText("The tabs portrayed after the Settings for predictions tab are the tabs where the interactive graphs of the predicted measures will appear.
             Most of the tabs have 2 columns on the left of the panel where plot specific options can be selected such as:"),
    
    helpText(HTML('<ul><li> Display the graph in grids or in frames. </li></ul>')),
    helpText(HTML('<ul><li> Show or not show confidence intervals (if they have been provided by the user).</li></ul>')),
    helpText(HTML('<ul><li> Show or not show tick options related to the range of the axes and the step in each axis</li></ul>')),
    helpText(HTML('<ul><li> Differences of measures between the covariate patterns.</li></ul>')),
    helpText(HTML('<ul><li> Ratios of measures between the covariate patterns.</li></ul>')),
    helpText(HTML('<ul><li> Log scale (in some of the hazard graphs)</li></ul>')),
    helpText(HTML('<ul><li> Slider inputs of different time points that are relevant for some of the illustrations.</li></ul>')),
    
    
    helpText("For most of the predicted measures, predictions are given by state, by covariate pattern or both.
             For complicated illustrations, an explanatory sentence is given below the illustration.")
    
    
  )
  message
  
  return(list(message ))
  
})