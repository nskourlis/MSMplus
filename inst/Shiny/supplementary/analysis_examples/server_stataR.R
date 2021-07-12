to_url_backstataR <- a("Back to MSMplus app", href="https://nskbiostatistics.shinyapps.io/MSMplus/")

output$to_url_backstataR <- renderUI({
  
  tagList(to_url_backstataR)
})




output$message_stata <- renderUI({
  
  
  message = withMathJax(
    helpText('In this example, we conduct a multi-state analysis and derive the json input files that are needed for the MSMplus app effortlessly via commands msboxes and predictms in Stata.
             These commands are available in Stata by installing the multistate package (net install multistate, from("https://www.mjcrowther.co.uk/code/multistate/") and then by typing',
             strong('interactive jsonpath("filepath")','the json input files are automatically produced.'))
    
  )
  message
  
  return(list(message ))
  
})


output$message_R <- renderUI({
  
  
  message = withMathJax(
    helpText('In this example, we conduct a multi-state analysis using the flexsurv package in order to derive the json input files that are needed for the MSMplus app. This is done
              effortlessly via the functions msboxes_R and flexsurvjson in R.
              These functions are available in R by downloading the MSMplus package from github remotes::install_github("nskourlis/MSMplus", force=TRUE). If the user wish to use the msm or mstate 
              package functions, two extra wrapper functions are available from MSMplus, mstatejson and msmjson.')
    
)
    
  
  message
  
  return(list(message ))
  
})




output$message_analysis_stata<-renderUI({ 
  
  intro0=withMathJax(
    helpText(strong("Stata code- msboxes and predictms")))
  intro =p(
    withMathJax(
    helpText("//Regenerate the covariates of interest as binary"),
    helpText(HTML('&emsp;') ,"tab age, gen(ag)"),
    helpText('\n'),
    helpText("//Define the transition matrix"),
    helpText(HTML('&emsp;') ,"    matrix tmat = (.,1,2/.,.,3/.,.,)."),
    helpText('\n'),
    helpText("//Data preparation- From one row per participant to multiple rows per participant, one for each allowed transition."),
    helpText(HTML('&emsp;') ,"    msset, id(id) transmat(tmat) times(prtime rfstime) ///"),
    helpText(HTML('&emsp;') ,HTML('&emsp;') ,"    states(prstat rfsstat) covariates(ag2 ag3)"),
    helpText('\n'),
    helpText("//Specify time variable for the predictions"),
    helpText(HTML('&emsp;') ,"    range tt 0 5 201"),
    helpText('\n'),
    helpText('\n'),
    helpText("//Run the msboxes command with options interactive and jsonpath"),
    helpText(HTML('&emsp;') ,"    msboxes, transmatrix(tmat) id(id) xvalues(0.2 0.7 0.45) yvalues(0.7 0.7 0.2) ///"),
    helpText(HTML('&emsp;') ,HTML('&emsp;') ,'    statenames("(1)Transplant" "(2)Platelet recovery" "(3)Relapse or death") ///'),
    helpText(HTML('&emsp;') ,HTML('&emsp;') ,'    transnames("h1" "h2" "h3") freqat(tt) scale(365.25)  ///'),
    helpText(HTML('&emsp;') ,HTML('&emsp;') ,strong('    interactive jsonpath("filepath")')),
    helpText('\n'),
    helpText('\n'),
    helpText('//stset the data for the MSM model'),
    helpText(HTML('&emsp;') ,'    stset _stop, enter(_start) failure(_status=1) scale(365.25)'),
    helpText('\n'),
    helpText('//Define the transition specific models'),
    helpText(HTML('&emsp;') ,'    stmerlin ag2 ag3 if _trans==1, distribution(rp) df(4)'),
    helpText(HTML('&emsp;') ,'    stmerlin ag2 ag3 if _trans==2, distribution(rp) df(4)'),
    helpText(HTML('&emsp;') ,'    stmerlin ag2 ag3 if _trans==3, distribution(rp) df(4)'),
    helpText('\n'),
    helpText('\n'),
    helpText('//Specify user function (optional)'),
    helpText(HTML('&emsp;') ,'    mata:'),
    helpText(HTML('&emsp;') ,'    real matrix ufunc(M) { '),
    helpText(HTML('&emsp;') ,'       los1 = ms_user_los(M,1)'),
    helpText(HTML('&emsp;') ,'       los2 = ms_user_los(M,2)'),
    helpText(HTML('&emsp;') ,'       return(los1:+los2) '),
    helpText(HTML('&emsp;') ,HTML('&emsp;') ,'     }'),
    helpText(HTML('&emsp;') ,'end'),
    helpText('\n'),
    helpText('\n'),
    helpText('//Run the predictms command with options interactive and jsonpath. In our example we are interested in making predictions for 3 covariate patterns- 3 age groups'),
    helpText(HTML('&emsp;') ,'     predictms, transmat(tmat) models(trans1 trans2 trans3) timevar(tt) at1(ag2 0 ag3 0) at2(ag2 1 ag3 0) at3(ag2 0 ag3 1) ///'),
    helpText(HTML('&emsp;') ,HTML('&emsp;') ,'     los visit difference ci ratio userfunction(ufunc) n(100000) m(200) from(1 2) simulate latent ///'),
    helpText(HTML('&emsp;') ,HTML('&emsp;') ,strong('interactive jsonpath("filepath")')),
    helpText('\n'),
    helpText('')
    ), style = "font-family: 'courier'; font-si16pt")
  intro
  
  return(list(intro0,intro ))
  
})



output$message_analysis_R<-renderUI({ 
  
  intro0=withMathJax(
    helpText(strong("R code- msboxes_R and flexsurv")))
  
  intro =p(
    withMathJax(
      helpText('# Call the libraries'),
      helpText(''),
      helpText(HTML('&emsp;') ,'library("MSMplus")'),
      helpText(HTML('&emsp;') ,'library("survival")'),
      helpText(HTML('&emsp;') ,'library("mstate")'),
      helpText(HTML('&emsp;') ,'library("tidyverse")'),
      helpText('\n'),
      helpText(HTML('&emsp;') ,'ebmt<- read.csv("ebmt.csv",header=TRUE,sep=",")'),
      helpText('\n'),
      helpText(''),
      helpText('#transition matrix definition'),
      helpText(HTML('&emsp;') ,'tmat <- transMat(x =list(c(2,3),c(3),c()),names=c("Transplant","Platelet Recovery","Relapse/Death"))'),
      helpText('\n'),
      helpText('#Recoding the covariates into binary'),
      helpText(HTML('&emsp;') ,'ebmt$age2=  recode(ebmt$age,">40" =0,"20-40"=1,"<=20"=0)'),
      helpText(HTML('&emsp;') ,'ebmt$age3=  recode(ebmt$age,">40" =1,"20-40"=0,"<=20"=0)'),
      helpText('\n'),
      helpText('#Data preparation- From one row per participant to multiple rows per participant, one for each allowed transition.'),
      helpText(HTML('&emsp;') ,'msebmt <- msprep(data = ebmt, trans = tmat,time = c(NA,"prtime","rfstime"),'),
      helpText(HTML('&emsp;') ,HTML('&emsp;') ,'                     status=c(NA,"prstat","rfsstat"),keep=c("age2","age3"))'),
      helpText('\n'),
      helpText('# msboxes_R- creating an multi-state graph with updated frequencies in each state'),
      helpText(''),
      helpText(HTML('&emsp;') ,'MSMplus::msboxes_R(data=msebmt,id= msebmt$id, yb=c(0.3,0.5,0.75),xb=c(0.5,0.2,0.7),boxwidth=0.1,boxheight=0.1,'),
      helpText(HTML('&emsp;') ,HTML('&emsp;') ,'tmat.= tmat, tstop=msebmt$Tstop,vartime=c(seq(0,10,by=0.1)),scale=365.25,'),
      helpText(HTML('&emsp;') ,HTML('&emsp;') ,'jsonpath="~",name="msboxes_EBMT.json" ) '),
      helpText('\n'),
      helpText('### Moving to the predictions'),
      helpText(''),
      helpText('#Provide time vector'),
      helpText(HTML('&emsp;') ,'tgrid <- seq(1, 10, by = 1)* 365.25'),
      helpText(''),
      helpText('#Provide transition matrix'),
      helpText(HTML('&emsp;') ,'tmat <- rbind(c(NA, 1, 2), c(NA, NA, 3), c(NA, NA, NA))'),
      helpText('\n'),
      helpText('#Run transition specific hazard models -parametric markov (clock forward)'),
      helpText(''),
      helpText(HTML('&emsp;') ,'cffpm.list<-vector(3,mode="list")'),
      helpText(HTML('&emsp;') ,'cffpm.list[[1]] <- flexsurvspline(Surv(Tstart,Tstop,status) ~ age2+age3, subset=(trans==1),'),
      helpText(HTML('&emsp;') ,HTML('&emsp;') ,'                                  data = msebmt, k =3,scale = "hazard"'),
      helpText(HTML('&emsp;') ,HTML('&emsp;') ,'                                  control = list(maxit=100000, REPORT=1,trace=1,reltol=0.0000000001,fnscale=-1))'),
      helpText('\n'),
      helpText(HTML('&emsp;') ,'cffpm.list[[2]] <- flexsurvspline(Surv(Tstart,Tstop,status) ~ age2+age3, subset=(trans==2),'),
      helpText(HTML('&emsp;') ,HTML('&emsp;') ,                                   'data = msebmt, k =3, scale = "hazard" ,'),
      helpText(HTML('&emsp;') ,HTML('&emsp;') ,'                                   control = list(maxit=100000, REPORT=1,trace=1,reltol=0.0000000001,fnscale=1))'),
      helpText('\n'),
      helpText(HTML('&emsp;') ,'cffpm.list[[3]] <- flexsurvspline(Surv(Tstart,Tstop,status) ~ age2+age3, subset=(trans==3), '),
      helpText(HTML('&emsp;') ,HTML('&emsp;') ,'                                  data = msebmt, k =6, scale = "hazard", '),
      helpText(HTML('&emsp;') ,HTML('&emsp;') ,'                                  control = list(maxit=100000, REPORT=1, trace=1,reltol=0.0000000001,fnscale=1))'),
      helpText('\n'),
      helpText('\n'),
      helpText('#In our example we are interested in making predictions for 3 covariate patterns- 3 age groups'),
      helpText(HTML('&emsp;') ,'wh1 <- which(msebmt$age2 == 0 & msebmt$age3 == 0)'),
      helpText(HTML('&emsp;') ,'pat1 <- msebmt[rep(wh1[1], 3), 9:10]'),
      helpText(HTML('&emsp;') ,'attr(pat1, "trans") <- tmat'),
      helpText('\n'),
      helpText(''),
      helpText(HTML('&emsp;') ,'wh2 <- which(msebmt$age2 == 1 & msebmt$age3 == 0)'),
      helpText(HTML('&emsp;') ,'pat2 <- msebmt[rep(wh2[1], 3), 9:10]'),
      helpText(HTML('&emsp;') ,'attr(pat2, "trans") <- tmat'),
      helpText('\n'),
      helpText(''),
      helpText(HTML('&emsp;') ,'wh3 <- which(msebmt$age2 == 0 & msebmt$age3 == 1)'),
      helpText(HTML('&emsp;') ,'pat3 <- msebmt[rep(wh3[1], 3), 9:10]'),
      helpText(HTML('&emsp;') ,'attr(pat3, "trans") <- tmat'),
      helpText('\n'),
      helpText('\n'),
      helpText('#Run the flexsurvjson function to derive the predictions using the function of the flexsurv package.'),
      helpText('#In our example we are interested in making predictions for 3 covariate patterns- 3 age groups'),
      helpText(HTML('&emsp;') ,'MSMplus::flexsurvjson(model=cffpm.list, vartime= tgrid, qmat=tmat,process="Markov",'),
      helpText(HTML('&emsp;') ,HTML('&emsp;') ,'              totlos=TRUE,ci.json=TRUE, cl.json=0.95, B.json=100, tcovs=NULL,'),
      helpText(HTML('&emsp;') ,HTML('&emsp;') ,'              Mjson=100, variance=FALSE, covariates_list=list(pat1,pat2,pat3),'),
      helpText(HTML('&emsp;') ,HTML('&emsp;') ,'              jsonpath="~.", name="predictions_EBMT.json" ) '),
      helpText('')
       ), style = "font-family: 'courier'; font-si16pt")
  intro
  
  return(list(intro0,intro ))
  
})

