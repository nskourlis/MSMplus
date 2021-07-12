to_url_backcsv <- a("Back to MSMplus app", href="https://nskbiostatistics.shinyapps.io/MSMplus/")

output$to_url_backcsv <- renderUI({
  
  tagList(to_url_backcsv)
})





output$message_csv <- renderUI({
  
  
  message = withMathJax(
    helpText(strong("Csv input files")),
    helpText("A Comma Separated Values (CSV) file is a plain text file that contains a list of data. 
             We generally recommend using the tools to automatically generate the JSON file. However, in case the user has no knowledge of R and Stata,
             or the multi-state analysis is conducted with packages other than msm, mstate, flexsurv in R (or multi-state in Stata), then the results 
             can be supplied through a csv file."),
  helpText(strong("Csv files to be uploaded at MSMplus- Manual approach")),
  helpText("The results from a multi-state analysis can be packed up into one csv file that will serve as input to the MSMplus app."),
  helpText(HTML("<ul><li>The advisable approach of preparing the MSMplus input is the manual creation of an excel/csv file containing the analysis results 
             according to certain formatting and naming rules. More details about its contents can be found in the 'results csv file' tab. </li></ul>")),
  helpText(HTML("<ul><li>Regarding the creation of the multi-state graph, the user can specify the number of states and the transition matrix on the app platform. 
             Optionally, one can choose to also upload a csv semicolon delimited file containing information about the number of individuals across time 
             and the cumulative number of individuals that have experienced each transition across time, easily derived through msboxes command in Stata or msboxes_R function in R (Github: nskourlis/MSMplus).
             More details can be found in the 'Frequency csv file' tab. </li></ul>")),
  helpText(strong("How to derive the csv files needed for the app")),
  helpText("Unlike the json files which can be automatically derived using specific commands in Stata or specific functions in R (Github: nskourlis/MSMplus), 
           the csv file with the analysis results needs to be manually created, following certain formatting and 
           naming rules for the variables (rules described in detail in the following tabs). Even though this option may seem to be laborious, it offers great flexibility
           as it allows the user to fit any type of multi-state model, in any of the available statistical software and then upload the results as a csv file on MSMplus..")
  )
  message
  
  return(list(message ))
  
})

output$message_csv1 <- renderUI({
  
  
  message = withMathJax(
    helpText("The optional 'Frequency file' contains the information about the number of individuals in each state across time and the cumulative number of individuals
                that have experienced each transition across time.") ,
    helpText(strong('Variables of the "Frequency csv file"')),
    helpText(HTML('<ul><li> Time_label: The row number of the dataset </li></ul>')),
    helpText(HTML('<ul><li> The variables representing the number of individuals in each state at each time point. 
                  The user should name them as "State\\(i\\)" where \\(i\\)  is the index of the state</li></ul>')),
    helpText(HTML('<ul><li> The variables representing the number of individuals that have experienced each transition at each time point. 
                  The user should name them as "h\\(i\\)" where \\(i\\) is the index of the transition.</li></ul>')),
    helpText(HTML('<ul><li> timevar: The time points at which we assess the number of individuals in each state and number of
                  individuals having experienced each transition.</li></ul>')),
    helpText(strong("Example of 'Frequency csv file'"))
    
  )
  message
  
  return(list(message ))
  
})


output$message_csv2 <- renderUI({
  
  message = withMathJax(
    helpText(strong("The 'Results csv file'  contains the results of the multi-state analysis ")),
    helpText(" The analysis results csv file provides the statistical results of the MSM analysis to the application. 
               It should contain  some mandatory elements while the rest are optional, and depend on the analysis. 
               The time points and a list of the covariate patterns (and number) for which the predictions were made is mandatory.
               The rest of the estimates are optional, meaning that the application will be able to run, creating graphs for the measures for which 
               estimations have biin provided. Differences and ratios of the estimated measures as well as 
               confidence intervals are also supported by MSMplus. "),
    helpText(strong('Naming rules for the "Results csv file"')),
    helpText(HTML('<ul><li> Time points of prediction ("timevar") </li></ul>')),
    helpText(HTML('<ul><li> Number of covariate patterns ("Nats")</li></ul>')),
    helpText(HTML('<ul><li> Names of covariate patterns("atlist")</li></ul>')),
    helpText(HTML('<ul><li> Number of transitions ("Ntransitions")</li></ul>')),
    helpText(HTML('<ul><li> The transition matrix ("tmat")</li></ul>')),
    helpText(HTML('<ul><li> The estimated measure variables. 
                  <ul><li>P(_diff/_ratio)_*_to_*(_uci/_lci) : "P" stands for the "Probability" of being in each state.</li></ul>
                  <ul><li>Haz(_diff/_ratio)_*_to_*(_uci/_lci): "Haz" stands for the hazard or transition rate for each transition. </li></ul>
                  <ul><li>Los(_diff/_ratio)_*_to_*(_uci/_lci):  "Los" stands for Length of stay"</li></ul>
                  <ul><li>Visit(_diff/_ratio)_*_to_*(_uci/_lci):  "Visit" stands for the "Probability of ever being" in each state.</li></ul>
                  <ul><li>User(_diff/_ratio)_*_to_*(_uci/_lci):"User" stands for the "User specified function"</li></ul>
                  <ul><li>Next_*_to_*(_uci/_lci): "Next" stands for the "Probability that a particular state \\(r\\)  is the next state after the current state \\(s\\) " </li></ul>
                  <ul><li>Number_*_to_*(_uci/_lci): "Number" stands for the expected number of visits to a state </li></ul>
                  <ul><li>First_*(_uci/_lci) : "First" stands for the expected "First passage time" into a particular state</li></ul>
                  <ul><li>Soj_*: "Soj" stands for the "mean sojourn time" into a particular state(_uci/_lci)</li></ul>
                  </li></ul>')),
    helpText(HTML('<ul><li> Explaining the rest of the abbreviations in the names of the variables
                  <ul><li>uci stands for "Upper confidence interval". The upper confidence interval of the specified measure is stored in variables with this ending.</li></ul>
                  <ul><li>lci stands for "Lower confidence interval". The lower confidence interval of the specified measure is stored in variables with this ending.</li></ul>
                  <ul><li> diff stands for "Difference". The difference in the specified measure between the present covariate pattern and the covariate pattern of reference is stored in this variable.</li></ul>
                  <ul><li> ratio stands for "Ratio". The ratio of the specified measure between the present covariate pattern and the covariate pattern of reference is stored in this variable.</li></ul>
                  <ul><li>*_to_* : The first asterisk specifies the starting state at time s and the second asterisk specifies the landing state at time t. If a measure has only one asterisk then it is a measure that refers to a specific state that does not entail a sense of transition between states.</li></ul>
                  </li></ul>')),    helpText(strong("Below all possible names are given for a multi-state setting illness-death model of 3 states (EBMT example). "))
    
  )
  
  
  message
  
  return(list(message ))
  
})


output$message_csv3 <- renderUI({
  
  
  message = withMathJax(
    helpText(strong("The 'Results csv file'  contains the results of the multi-state analysis ")),
    helpText(" The analysis results csv file provides the statistical results of the MSM analysis to the application. 
               It should contain  some mandatory elements while the rest are optional, and depend on the analysis. 
               The time points and a list of the covariate patterns (and number) for which the predictions were made is mandatory.
               The rest of the estimates are optional, meaning that the application will be able to run, but only visualizing the 
               information that it is being given. Differences and ratios of the estimated measures as well as 
               confidence intervals are also supported by MSMplus. "),
    helpText(strong('Formatting rules for the "Results csv file"')),
    helpText('If \\(j\\) is the number of the covariate patterns and \\(t\\) the number of time points where predictions where made then the 
             excel file (which will be saved as csv) should havs \\(j*t\\) number of row entries.'),
    helpText(HTML('<ul><li> Nats: This variable will have the same number for each row, that is, the number of covariate patterns (e.g 3)  </li></ul>')),
    helpText(HTML('<ul><li> Ntransitions: This variable will have the same number for each row, that is, the number of transitions (e.g 3)</li></ul>')),
    helpText(HTML('<ul><li> atlist: The value of this variable specify the name of the covariate pattern for which the prediciton is made in the specific row.</li></ul>')),
    helpText(HTML('<ul><li> timevar: It is the time variable. The value of this variable specify the time point for which the prediciton is made for a certain covariate pattern. </li></ul>')),
    helpText(HTML('<ul><li> The estimated measure variables. Each row entry gives the estimation for the specifiv variable for the specific 
                            covariate pattern for a certain time point of prediction. 
                  <ul><li>P(_diff/_ratio)_*_to_*(_uci/_lci)</li></ul>
                  <ul><li>Haz(_diff/_ratio)_*_to_*(_uci/_lci)</li></ul>
                  <ul><li>Los(_diff/_ratio)_*_to_*(_uci/_lci)</li></ul>
                  <ul><li>Visit(_diff/_ratio)_*_to_*(_uci/_lci)</li></ul>
                  <ul><li>User(_diff/_ratio)_*_to_*(_uci/_lci)</li></ul>
                  <ul><li>Next_*_to_*(_uci/_lci)</li></ul>
                  <ul><li>Number_*_to_*(_uci/_lci)</li></ul>
                  <ul><li>First_*(_uci/_lci)</li></ul>
                  <ul><li>Soj_*(_uci/_lci)</li></ul>
                  </li></ul>')),
    helpText(strong('Important note: Note that the predictions are sorted by covariate pattern and within each covariate pattern they are 
                    sorted by ascending timevar value.')),
    helpText(strong("Below an example for the structure of the 'results csv'is given for the multi-state setting illness-death
                    model of 3 states (EBMT example) for the transition probabilities  "))
    
  )
  
  
  message
  
  return(list(message ))
  
})



#output$message_csv4<-renderUI({ 
#  
#  intro0=withMathJax(
#    helpText(strong("Stata code- msboxes and predictms")))
#  intro =p(
#    withMathJax(
#      helpText("//Regenerate the covariates as binary"),
#      helpText("    tab age, gen(ag)"),
#      helpText(''),
#      helpText("//Define the transition matrix"),
#      helpText("    matrix tmat = (,1,2/.,.,3/.,.,)."),
#      helpText(''),
#      helpText("//Msset the data"),
#      helpText("    msset, id(id) transmat(tmat) times(prtime rfstime) ///"),
#      helpText("    states(prstat rfsstat) covariates(ag2 ag3)"),
#      helpText(''),
#      helpText("//Specify time variable for the predictions"),
#      helpText("    range tt 0 5 201"),
#      helpText(''),
#      helpText("//Run the msboxes command with options interactive and jsonpath"),
#      helpText("    msboxes, transmatrix(tmat) id(id) xvalues(0.2 0.7 0.45) yvalues(0.7 0.7 0.2) ///"),
#      helpText('    statenames("(1)Transplant" "(2)Platelet recovery" "(3)Relapse or death") ///'),
#      helpText('    transnames("h1" "h2" "h3") freqat(tt) scale(365.25)  ///'),
#      helpText(strong('    interactive jsonpath("filepath")')),
#      helpText(''),
#      helpText('//stset the data for the MSM model'),
#      helpText('    stset _stop, enter(_start) failure(_status=1) scale(365.25)'),
#      helpText(''),
#      helpText('//Define the transition specific models'),
#      helpText('    stpm2 ag2 ag3 if _trans==1, scale(hazard) df(4)'),
#      helpText('    stpm2 ag2 ag3 if _trans==2, scale(hazard) df(4)'),
#      helpText('    stpm2 ag2 ag3 if _trans==3, scale(hazard) df(4)'),
#      helpText(''),
#      helpText('//Specify user function (optional)'),
#      helpText('    mata:'),
#      helpText('    real matrix ufunc(M) { '),
#      helpText('       los1 = ms_user_los(M,1)'),
#      helpText('       los2 = ms_user_los(M,2)'),
#      helpText('       return(los1:+los2) '),
#      helpText('     }'),
#      helpText('end'),
#      helpText(''),
#      helpText('//Run the predictms command with options interactive and jsonpath'),
#      helpText('     predictms, transmat(tmat) models(trans1 trans2 trans3) timevar(tt) at1(ag2 0 ag3 0) at2(ag2 1 ag3 0) at3(ag2 0 ag3 1) ///'),
#      helpText('     los visit difference ci ratio userfunction(ufunc) n(50000) m(100) from(1 2) ///'),
#      helpText(strong('interactive jsonpath("filepath")')),
#      helpText(''),
#      helpText('')
#    ), style = "font-family: 'courier'; font-si16pt")
#  intro
#  
#  return(list(intro0,intro ))
#  
#})
#

output$csv_file1<- renderPrint({
  
  
  data= read.csv2("material/frequencies_toy.csv",header=TRUE, sep=";")
  
  data=data[1:5,]
  data
  
  
  
})


output$csv_file2<- renderPrint({
  
  
  
  
  data= read.csv2("material/results_toy.csv",header=TRUE, sep=";")
  data=data[1:2,]
  names(data)
  
  
})

output$csv_file3<- renderPrint({
  
  
  data= read.csv2("material/results_toy.csv",header=TRUE, sep=";")
  data=data[c(seq(1,51,10),seq(102,152,10),seq(203,253,10)),1:10]
  #names(data)
  
  data
})