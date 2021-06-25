output$interpret<-renderUI({ 
  
 intro = withMathJax(
   helpText(strong("Description of a Multi-state process and definitions.")),
   helpText("Consider a stochastic process \\(Y(t)\\) \\( (t\\in T) \\) with a finite state space \\(L = {1,...,p}\\)
            and a history of the process defined as \\( H_{s}= {Y(u); 0 \\leq u \\leq s} \\)")
   )
 intro

 return(list(intro ))
 
})

output$message_prob<- renderUI ({
  
  #intro = print("Description of a Multi-state process and definitions\nConsider a stochastic process Y(t) (tET) with a finite state space L = {1,...,p} and a history of the process defined as Hs= Y (u); 0<= u <= sg.\n")
  

  if (is.null(input$measures) ){return()}
  
  prob     <- "prob"      %in% input$measures
  trans    <- "trans"     %in% input$measures
  los      <- "los"       %in% input$measures
 #vis      <- "vis"       %in% input$measures
  comp     <- "comp"      %in% input$measures
  extram   <- "extram"    %in% input$measures
  robust   <- "robust"    %in% input$measures
  
  if (prob){
    p_def = withMathJax(
      helpText(strong("Transition probabilities")),
      helpText('The transition probabilities can be defined as $$P(Y(t)=b| Y(s)=a, H_{s-})$$ with $$a,b \\in L, s; t \\in T ; s <=t$$'),
      helpText("The equation above depicts the probability of being in state b at time t given that you were in state a at time s and conditional on the history of the process up to time s.")
    )
    p_ex  = helpText("\nEBMT example: For time= 2 years since transplantation an individual <20 years old at transplantation has 39% probability of having experienced
            platelet recovery (transition from state 1 to state 2). On the other hand, an individual >40 years old at transplantation has 32% probability 
            of platelet recovery\n")
  }
  else if (!prob) {p_def=NULL ; p_ex=NULL}
  
  return(list(p_def,p_ex))
  
})


output$message_trans<- renderUI ({
  
  
  prob     <- "prob"      %in% input$measures
  trans    <- "trans"     %in% input$measures
  los      <- "los"       %in% input$measures
#  vis      <- "vis"       %in% input$measures
  comp     <- "comp"      %in% input$measures
  extram    <- "extram"     %in% input$measures
  robust   <- "robust"    %in% input$measures
  
  if (trans){
    
    trans_def = withMathJax(
      helpText(strong("Transition intensity")),
      helpText('For the stochastic process \\(Y(t)\\), there are \\(K\\) potential transitions \\( (k=1,.,K)\\).
      If the k-th transition is the transition between state \\(a\\) and \\(b\\) \\(a\\rightarrow b)\\) the \\(k-th\\) transition intensity
      is defined as the derivative of the (\\(a\\rightarrow b)\\) transition probability):'),
      helpText("$$q_{k}(t)=\\lim_{\\delta t\\to0}\\frac{P(Y(t+\\delta t)=b_{k}| Y(t)=a_{k}, H_{s-})}{\\delta t}$$"),
      helpText("Which is the instantaneous rate of moving from \\(a\\) to \\(b\\), at time \\(t\\) given that you were in state \\(a\\) at time \\(s\\) and conditional on the history of the process up to time \\(s\\).")
      )
    
    trans_ex  =  helpText("EBMT example: For time= 2 years since transplantation, the transition rate from post transplant (state 1) to relapse/death (state 3)
        is 0.07 for an individual <20 years old at transplantation and 0.09 for an individual >40 years old at transplantation Therefore the hazard ratio
        for relapse/death among those who had not recovered previously (state 1) between the older and the younger age group is 1.28\n")
  }
  else if (!trans) {trans_def=NULL ; trans_ex=NULL}
  
  return(list( trans_def, trans_ex))
  
})



output$message_los<- renderUI ({
  
  
  prob     <- "prob"      %in% input$measures
  trans    <- "trans"     %in% input$measures
  los      <- "los"       %in% input$measures
 # vis      <- "vis"       %in% input$measures
  comp     <- "comp"      %in% input$measures
  extram    <- "extram"     %in% input$measures
  robust   <- "robust"    %in% input$measures
  
  if (los){
    
    los_def = withMathJax(
      helpText(strong("Length of stay")),
      helpText('The residual restricted expected length of stay (or length of stay for simplicity) in state \\(a\\) during the time
       period from \\(s\\) to \\( \\tau  \\) , conditional on the patient being in state \\(a\\) (non-absorbing) at time \\(s\\),  is defined as'),
      helpText("$$e_{ab}(s)=\\int_{s}^{\\tau} P(Y(u)=b| Y(s)=a, H_{s-}) du$$"),
      helpText("which defines the amount of time spent in state \\(b\\), starting in state \\(a\\) at time \\(s\\), up until time \\(\\tau\\) . If \\(\\tau=\\infty\\) , and state \\(a=b\\) is a healthy state and all possible next states are deaths, then this equation represents life expectancy.")
    )
    
    los_ex  =  helpText("EBMT example: For time= 5 years since transplantation, individuals <20 years old at transplantation are expected to have stayed 1.9 years in the recovery state (state 2) while individuals >40 years old at transplantation are expected to have stayed 1.6 years in the recovery state.")
    
  } 
  else if (!los) {los_def=NULL ; los_ex=NULL}
   return(list(los_def,los_ex  ))
  
})


#output$message_vis<- renderUI ({
#  
#  
#  prob     <- "prob"      %in% input$measures
#  trans    <- "trans"     %in% input$measures
#  los      <- "los"       %in% input$measures
#  vis      <- "vis"       %in% input$measures
#  comp     <- "comp"      %in% input$measures
#  extram    <- "extram"   %in% input$measures
#  robust   <- "robust"    %in% input$measures
#  
#  if (vis){
#    
#    vis_def = withMathJax(
#      helpText(strong("Probability of ever visiting a state")),
#      helpText('Now we define $$ f_{ab}:= P(\\tau_{b}<\\infty|X_{0}=a)= \\sum_{k=1}^{\\infty} F_{k}(a,b) $$ for all \\(a,b\\in L \\) 
#                which represents the probability of ever visiting state \\(b\\) after given initial state \\(X_{0}=a\\).
#                \\( F_{k}\\) is the conditional distribution of the first visit to the state \\(b\\in L\\), given the initial state \\(X_{0}=a\\),
#                and \\(k\\) is index so that \\(X_{k}=b\\). More on the theoritical derivation of the estimation can be found at "Stochastic Processes" by Lothar Breuer.')
#      )
#    
#    vis_ex  =  helpText("EBMT example: For time= 5 years since transplantation, the probability that an individual <20 years old at transplantation
#                        will have a relapse/death (state 3) by that time is 39% while the probability that an individual >40 years old at transplantation will have a relapse/death by that time is 52%.")
#  } 
#  
#  else if (!vis) {vis_def=NULL ; vis_ex=NULL}
#  
#   return(list( vis_def, vis_ex  ))
#  
#  
#})
#

output$message_comp<- renderUI ({
  
  
  prob     <- "prob"      %in% input$measures
  trans    <- "trans"     %in% input$measures
  los      <- "los"       %in% input$measures
#  vis      <- "vis"       %in% input$measures
  comp     <- "comp"      %in% input$measures
  extram    <- "extram"     %in% input$measures
  robust   <- "robust"    %in% input$measures
  
  if (comp){
    
    comp_def = withMathJax(
      helpText(strong("Differences and ratios of estimated measures")),
      helpText("In order to illustrate the impact of different covariate levels on the measures of interest, we can derive differences and ratios between
      the covariate patterns of interest. Given covariate patterns \\(X_{1}\\) and \\(X_{2}\\) "),
      helpText('Example for differences in probabilities: $$ P(Y(t)=b| Y(s)=a, H_{s-},X_{1}) - P(Y(t)=b| Y(s)=a, H_{s-},X_{2})$$'),
      helpText('Example for ratios of length of stay     : $$ \\frac{e_{ab}(s,(X_{1})}{e_{ab}(s,(X_{2})}= \\frac{\\int_{s}^{\\tau} P(Y(u)=b| Y(s)=a, H_{s-},X_{1}) du}{\\int_{s}^{\\tau} P(Y(u)=b| Y(s)=a, H_{s-},X_{2}) du} $$')
      
    )
    
    comp_ex  =  helpText("EBMT example: For time= 5 years since transplantation, the probability that an individual <20 years old at transplantation
                        will have a relapse/death (state 3) by that time is 39% while the probability that an individual >40 years old at transplantation will have a relapse/death by that time is 52%.")
  } 
  
  else if (!comp) {comp_def=NULL ; comp_ex=NULL}
  
  return(list( comp_def, comp_ex  ))
  
  
})



 output$message_extram<- renderUI ({
   
   
   prob     <- "prob"      %in% input$measures
   trans    <- "trans"     %in% input$measures
   los      <- "los"       %in% input$measures
 #  vis      <- "vis"       %in% input$measures
   comp     <- "comp"      %in% input$measures
   extram    <- "extram"     %in% input$measures
   robust   <- "robust"    %in% input$measures
   
   if (extram){
     
     extram_def = withMathJax(
       helpText(strong("Extra estimated measures under time homogeneous Markov assumption")),


       helpText('Under the time homogeneous continuous time Markov approach, the expected single period of occupancy of a state \\(a\\)
                is given by \\(-1/q_{aa}\\). The probability that an individuals next move from state \\(a\\) to state \\(b\\) is given by \\( -q_{ab}/q_{aa}\\). 
                The expected total number of visits and expected first hitting times to a specific state can also be derived under
                this assumption via msm package and theoretical background can be found at Stochastic Processes by Lothar Breuer.')
             
     )
     
     extram_ex  = withMathJax( helpText("If the mean sojourn time of state a = 1 year, that means that each time an individual visits state a, they spend an average of 1 year in that state."),
                  helpText("If the expected number of visits for state a = 2, that means that an average individual is expected to visit state 2 times."),
                  helpText("If the probability that an individual next move from state a to state b is 0.5 means that the probability that an individual who is presently at state a will next move at state b is 50%."),
                  helpText("If the first passage time to state b is 2 years, that means, that for an individual with a specific covariate pattern, the estimated first time that he/she will reach state b is 2 years.")
     ) 
     
     } 
   
   else if (!extram) {extram_def=NULL ; extram_ex=NULL}
   
   return(list( extram_def, extram_ex  ))
   

 })

 
 output$message_robust<- renderUI ({
   
   
   prob     <- "prob"      %in% input$measures
   trans    <- "trans"     %in% input$measures
   los      <- "los"       %in% input$measures
#   vis      <- "vis"       %in% input$measures
   comp     <- "comp"      %in% input$measures
   extram   <- "extram"    %in% input$measures
   robust   <- "robust"    %in% input$measures
   
   if (robust){
     
     robust_def = withMathJax(
       helpText(strong("Robustness of measures under different types of models (different timescales)")),
       
       
       helpText('The types of models used for disease processes may depend on the main objectives of the analysis.
                If we aim to understand individual process dynamics and related factors, then models whose intensities reflect
                such dynamics are desirable, using a global "age" of the process (Markov models-clock forward approach).
                However, if the main objective is to assess a treatment or intervention, then models
                which consider marginal process characteristics such as time of entry to a particular state are preferred
                (semi-Markov models clock reset approach) [1]. That being said, the main estimates are said to be quite robust to 
                different modelling approaches. However, exploratory comparison of the results of different methods is commendable.'),
       helpText("Aalen et al. [2] and Datta and Satten [3] showed that with right-censored multistate
                data the usual Aalen-Johansen estimator of the state occupancy probabilities,
                motivated by Markov assumptions, are robust and valid for non-Markov models when
                censoring is completely independent."),
       helpText(em('1.Richard J Cook and Jerald F Lawless. "Statistical issues in modeling chronic disease in
                  cohort studies". In:Statistics in Biosciences 6.1 (2014), pp. 127-161.')),
       helpText(em("2.Aalen OO, Borgan O, Fekjaer H (2001) Covariate adjustment of event histories estimated with Markov
                 chains: the additive approach. Biometrics 57:993-1001")),
       helpText(em("3.Datta S, Satten GA (2001) Validity of the Aalen-Johansen estimators of stage occupation probabilities
                and Nelson-Aalen estimators of integrated transition hazards for non-Markov models. Stat Probab Lett
                55:403-411"))
       
     )
     

     
   } 
   
   else if (!robust) {robust_def=NULL}
   
   return(list( robust_def ))
  
  })
 

output$message_rob1<- renderUI ({
  
 # if (input$int_rob=="No") return()
  
#  if (input$int_rob=="Yes") {
    print("There are different approaches when performing a multistate analysis. When modelling transition intensities, an initial consideration is whether to emphasize
           age of a process (clock forward) or the duration of time spent in each state visited (clock-reset). Markov models are basic for the former situation and semi-Markov
           models for the latter (CookRJ).")
#  }
  
})

output$message_rob2<- renderUI ({
  
  # if (input$int_rob=="No") return()
  
  #  if (input$int_rob=="Yes") {
  print("Despite the approach used, Markov (clock forward) or semi Markov (clock reset), we argue that the resulting estimates will be very similar
        if not identical. With proper manipulation, the user can create a json input file that includes results from both approaches and compare them 
        directly with the app. To check the robustness of these 2 different approaches on the EBMT example click Yes on the robust example.")
  #  }
  
})

output$message_rob3<- renderUI ({
  
  # if (input$int_rob=="No") return()
  
  #  if (input$int_rob=="Yes") {
  print("For more details as to how to develop a json file to assess robustness of different MSM approaches, check the supplementary material of the
        MSMplus publication.")
  #  }
  
})