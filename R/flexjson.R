
#' @title flexsurvjson
#' @description Function flexsurvjson can receive models from flexsurv package.
#' It then uses functions from flexsurv package internally to estimate multi-state model 
#' measures such as transition probabilities, length of stay, and confidence intervals of the
#' estimations. Function flexsurvjson then take these results and reshapes them so that they can
#' be fed to MSMplus properly as a json file.
#' 
#' @param model The hazard model (or list of hazard models), fit with flexsurv
#' @param vartime A vector of time points for which predictions will be made.
#'  Default: 0. Functions from flexsurv package used: pmatrix.fs, pmatrix.simfs 
#' @param qmat the user has to supply the transition matrix
#' @param process "Markov" for clock forward approach, "semiMarkov" 
#' for clock reset approach, Default: 'Markov'
#' @param totlos Estimate total length of stay spent in each state "TRUE", "FALSE",
#'  Default: "FALSE"
#' @param ci.json Estimate confidence intervals, "TRUE", "FALSE", Default:  "FALSE"
#' @param cl.json Specify confidence level, Default: 0.95
#' @param B.json Number of simulations from the normal asymptotic distribution used to calculate variances. 
#' Decrease for greater speed at the expense of accuracy, Default: 50
#' @param tcovs Predictable time-dependent covariates , Default: NULL
#' @param Mjson Number of individuals to simulate in order to approximate the transition probabilities. 
#' Users should adjust this to obtain the required precision. Default: 50
#' @param variance Calculate the variances and covariances of the transition cumulative hazards (TRUE or FALSE).
#'  This is based on simulation from the normal asymptotic distribution of the estimates, which is computationally-expensive., Default: FALSE
#' @param covariates_list The user can specify different covariate patterns
#' for which predictions will be made, Default: list()
#' @param jsonpath specify the path of the folder that the json file should be saved, Default: ""
#' @param name Specify the name of the output json file, Default: 'predictions.json'
#' @return returns a list of objects: the time variable
#' the number of covariate patterns, the names of covariate patterns, the transition matrix,
#' the number of transitions, the transition probabilities, transition intensities
#' length of stay, their confidence intervals 
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  
#' libary(MSMjson)
#' library(flexsurv)
#' head(ebmt)
#' 
#' 
#' ### Let's first define the transition matrix
#' 
#' tmat <- transMat(x = list(c(2, 3),c(3), c() ), names = c("Transplant", "Platelet Recovery", "Relapse/Death" ) )
#' 
#' 
#' ### We will now create dummy variables for the age categories
#' 
#' ebmt$age2=  recode(ebmt$age, ">40" =0, "20-40"=1,"<=20" =0 )
#' ebmt$age3=  recode(ebmt$age, ">40" =1, "20-40"=0,"<=20" =0 )
#' 
#' 
#' #Data preparation- From one row per participant to multiple rows per participant, one for each allowed transition.
#' 
#' msebmt <- msprep(data = ebmt, trans = tmat, 
#'                  time = c(NA, "prtime", "rfstime"), status = c(NA, "prstat", "rfsstat"), keep=c("age2","age3"))
#' 
#' head(msebmt)
#' 

#' ## Multi-state model analysis: Using flexsurvjson function together with flexsurv package
#' 
#' ### Provide time vector
#' 
#' tgrid <- seq(1, 10, by = 1)   
#' 
#' ### Provide transition matrix
#' 
#' tmat <- rbind(c(NA, 1, 2), c(NA, NA, 3), c(NA, NA, NA)) 
#' 
#' 
#' ### Run transition specific hazard models: Clock forward approach
#' 
#' 
#' cfwei.list<-vector(3,mode="list")
#' 
#' for (i in 1:3) {
#'   
#'   cfwei.list[[i]]<-flexsurvreg(Surv(Tstart,Tstop,status)~age2+age3,subset=(trans==i),
#'                                dist="weibull",data=msebmt)
#' }
#' 
#' 
#' 
#' ### Prediction for different covariate patterns (the 3 age categories)
#' 
#' wh1 <- which(msebmt$age2 == 0 & msebmt$age3 == 0)
#' pat1 <- msebmt[rep(wh1[1], 3), 9:10]
#' attr(pat1, "trans") <- tmat
#' 
#' 
#' wh2 <- which(msebmt$age2 == 1 & msebmt$age3 == 0)
#' pat2 <- msebmt[rep(wh2[1], 3), 9:10]
#' attr(pat2, "trans") <- tmat
#' 
#' wh3 <- which(msebmt$age2 == 0 & msebmt$age3 == 1)
#' pat3 <- msebmt[rep(wh3[1], 3), 9:10]
#' attr(pat3, "trans") <- tmat
#' 
#' 
#' #We now run the flexsurvjson function to perform the multi-state model analysis using the function 
#' #from package flexsurv and the pack the predictions in a json file.
#' 
#' results_cf <- MSMplus::flexsurvjson( model=cfwei.list, vartime=seq(365.25,365.25,by=365.25), qmat=tmat, process="Markov",
#'                                    totlos=TRUE, ci.json=FALSE, cl.json=0.95, B.json=10, tcovs=NULL,
#'                                    Mjson=100, variance=FALSE,
#'                                    covariates_list=list(pat1,pat2,pat3), 
#'                                    jsonpath="~",
#'                                    name="predictions_EBMT_flex_fw.json" ) 
#' 
#' 
#' 
#' 
#'  
#'  }
#' }
#' @seealso 
#'  \code{\link[stringi]{stri_sort}}
#' @rdname flexsurv_json
#' @export 
#' @importFrom stringi stri_sort
flexsurvjson <- function( model, vartime=seq(1,1,by=1), qmat, process="Markov",
                           totlos=FALSE, ci.json=FALSE, cl.json=0.95, B.json=50, tcovs=NULL,
                           Mjson=50, variance=FALSE,
                           covariates_list=list(), 
                           jsonpath="~",
                           name="predictions.json" )  {
  
  options(scipen = 999,"digits"=14)
  

  
 # library("msm")
 # library("stringi")
 # library("RJSONIO")
 # library("flexsurv")

  
  #  vartime=time/scale
  
  ################################################################  
  mat=qmat
  mat[is.na(mat)] <- 0
  ntransitions=length(mat[which(mat>0)])
  nstates=ncol(mat)
  states=c(seq(1:nstates))
  p=1
  trmat=matrix(nrow=nstates, ncol=nstates, NA)
  
  for (i in 1:nstates) {
    for (k in 1:nstates) {
      
      if  (mat[i,k]>0)  {  
        
        trmat[i,k]=as.integer(p)
        p=p+1
      } 
      else if (mat[i,k]>0)  {  
        trmat[i,k]="NA"
      }
      
    }
  }
  
  
  tmatlist=list()
  tmatlist[[1]]=trmat
  
  tr_start_state=vector()
  for (k in 1:ntransitions) {
    tr_start_state[k]=which(trmat == k, arr.ind = TRUE)[1,1]
  }
  
  tr_end_state=vector()
  for (k in 1:ntransitions) {
    tr_end_state[k]=which(trmat == k, arr.ind = TRUE)[1,2]
  }

  

  
  pred=list() 
  
  
  
  ### First case, overall predictions
  if (length(covariates_list) == 0) {
    
    
    try( if (process!="Markov" & process!="semiMarkov") stop("Process must be either Markov or semiMarkov"))
    
    ### Calculate nstates, ntransitions #### 
    nstates=ncol(qmat)
    states=c(seq(1:nstates))
    
    tmat2=qmat
    tmat2[is.na(tmat2)] <- 0
    Ntransitions=length(tmat2[which(tmat2>0)])
    
    #################### Probabilities #######################################
    
    pm_array= array(dim=c(length(vartime),nstates+1,nstates),NA)
    pm_array_lci= array(dim=c(length(vartime),nstates+1,nstates),NA)
    pm_array_uci= array(dim=c(length(vartime),nstates+1,nstates),NA)
    
    #g=1
    if (process=="Markov") {
      pm_ci=list()
      for (k in 1:length(vartime)) {
        pm_ci[[k]]=pmatrix.fs(x=model, trans=qmat, t =vartime[k], 
                              sing.inf = 1e+10, B = B.json, ci = ci.json, cl = cl.json)
        
      }
    }
    
    if (process=="semiMarkov") {
      pm_ci=list()
      #p=1
      for (k in 1:length(vartime)) {
        
        pm_ci[[k]]= pmatrix.simfs(x=model, trans=qmat, t = vartime[k], ci = ci.json,
                                  tcovs = tcovs, B = B.json, cl = cl.json, M = Mjson)
        #   p=p+1  
      }
      
    }
    
    
    if (ci.json==FALSE) {
      for (i in 1: length(vartime)  ) {
        pm_array[i,,]=t(cbind(as.matrix(pm_ci[[i]]),vartime[i] ))
      }
    }
    
    if (ci.json==TRUE) {
      
      for (i in 1: length(vartime)  ) {
        pm_array[i,,]=    t(cbind(as.matrix(pm_ci[[i]]),vartime[i] ))
        pm_array_lci[i,,]=t(cbind(attributes(pm_ci[[i]])$lower,vartime[i]  ))
        pm_array_uci[i,,]=t(cbind(attributes(pm_ci[[i]])$upper,vartime[i]  ))
        
      }
    }
    
    
    pmlist=list()
    
    for (k in states) {
      pmlist[[k]]=pm_array[,,k]
      
      if (length(vartime)==1) {pmlist[[k]]=as.data.frame(t(as.matrix(pmlist[[k]])))}
      else if (length(vartime)>1) {pmlist[[k]]=as.data.frame(as.matrix(pmlist[[k]]))}
      
      # pmlist[[k]]=as.data.frame(as.matrix(pmlist[[k]]))
      #pmlist[[k]]=as.data.frame(t(as.matrix(pmlist[[k]])))
      #pmlist[[k]]=as.data.frame(pmlist[[k]])
    }
    
    for (k in states) {
      for (j in states) {
        
        colnames(pmlist[[k]])[j]=paste0("P_",k,"_to_",j)
      }
      colnames(pmlist[[k]])[nstates+1]  ="timevar"  
    } 
    pmlist
    
    pmlist_lci=list()
    pmlist_uci=list()
    
    
    if (ci.json==TRUE) {
      
      ##################### Probabilities lci #####################################################################
      
      
      for (k in states) {
        pmlist_lci[[k]]=pm_array_lci[,,k]
        
        if (length(vartime)==1) {pmlist_lci[[k]]=as.data.frame(t(as.matrix(pmlist_lci[[k]])))}
        else if (length(vartime)>1) {pmlist_lci[[k]]=as.data.frame(as.matrix(pmlist_lci[[k]]))}
        
        #pmlist_lci[[k]]=as.data.frame(as.matrix(pmlist_lci[[k]]))
        
      }
      
      for (k in states) {
        for (j in states) {
          colnames(pmlist_lci[[k]])[j]=paste0("P_",k,"_to_",j,"_lci")
        }
        colnames(pmlist_lci[[k]])[nstates+1]  ="timevar"  
      } 
      
      pmlist_lci
      
      ##################### Probabilities uci #####################################################################
      
      #pmlist_uci=list()
      for (k in states) {
        pmlist_uci[[k]]=pm_array_uci[,,k]
        
        if (length(vartime)==1) {pmlist_uci[[k]]=as.data.frame(t(as.matrix(pmlist_uci[[k]])))}
        else if (length(vartime)>1) {pmlist_uci[[k]]=as.data.frame(as.matrix(pmlist_uci[[k]]))}
        
        #  pmlist_uci[[k]]=as.data.frame(as.matrix(pmlist_uci[[k]]))
      }
      
      for (k in states) {
        for (j in states) {
          colnames(pmlist_uci[[k]])[j]=paste0("P_",k,"_to_",j,"_uci")
        }
        colnames(pmlist_uci[[k]])[nstates+1]  ="timevar"  
      } 
      
      pmlist_uci
      
      
    }
    
    
    
    
    #################### Cummulative transition intensities #######################################
    
    transm=msfit.flexsurvreg(object=model, t=vartime, variance = variance,
                             tvar = "trans", trans=qmat, B = B.json)$Haz
    
    
    
    trans= reshape(transm, idvar = "time", timevar = "trans", direction = "wide")
    trans$timevar=trans$time
    trans=trans[,-1]
    namesh=vector()
    for (i in 1:Ntransitions) {namesh[i]=paste0("Haz_",tr_start_state[i],"_to_",tr_end_state[i])}
    names(trans)[1:Ntransitions]=namesh
    
    
    is.cumhaz=1
    
    #################################################################################
    #################### Total length of stay #######################################
    #################################################################################
    
    #### Markov models###
    
    
    if (process=="Markov") {
      
      los_array= array(dim=c(length(vartime),nstates+1,nstates),NA)
      los_array_lci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      los_array_uci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      
      los_ci=list()
      
      for (k in 1:length(vartime)) {
        
        
        los_ci[[k]]= totlos.fs(x=model, trans=qmat, t = vartime[k], ci = ci.json, tvar = "trans",
                               sing.inf = 1e+5, B = B.json, cl = cl.json)
        
      }
      
      if (ci.json==FALSE) {
        for (i in 1: length(vartime)  ) {
          los_array[i,,]=t(cbind(as.matrix(los_ci[[i]]),vartime[i] ))
        }
      }
      if (ci.json==TRUE) {
        
        for (i in 1: length(vartime)  ) {
          los_array[i,,]=    t(cbind(as.matrix(los_ci[[i]]),vartime[i] ))
          los_array_lci[i,,]=t(cbind(attributes(los_ci[[i]])$lower,vartime[i]  ))
          los_array_uci[i,,]=t(cbind(attributes(los_ci[[i]])$upper,vartime[i]  ))
          
        }
      }
      
      loslist=list()
      
      for (k in states) {
        loslist[[k]]=los_array[,,k]
        
        if (length(vartime)==1) {loslist[[k]]=as.data.frame(t(as.matrix(loslist[[k]])))}
        else if (length(vartime)>1) {loslist[[k]]=as.data.frame(as.matrix(loslist[[k]]))}
        
      }
      
      for (k in states) {
        for (j in states) {
          colnames(loslist[[k]])[j]=paste0("Los_",k,"_to_",j)
        }
        colnames(loslist[[k]])[nstates+1]  ="timevar"  
      } 
      loslist
      
      loslist_lci=list()
      loslist_uci=list()
      
      if (ci.json==TRUE) {
        
        ##################################################################################################
        ##################### Total length of stay lci ##################################################
        #################################################################################################
        
        for (k in states) {
          loslist_lci[[k]]=los_array_lci[,,k]
          
          if (length(vartime)==1) {loslist_lci[[k]]=as.data.frame(t(as.matrix(loslist_lci[[k]])))}
          else if (length(vartime)>1) {loslist_lci[[k]]=as.data.frame(as.matrix(loslist_lci[[k]]))}
          
          # loslist_lci[[k]]=as.data.frame(as.matrix(loslist_lci[[k]]))
        }
        
        for (k in states) {
          for (j in states) {
            colnames(loslist_lci[[k]])[j]=paste0("Los_",k,"_to_",j,"_lci")
          }
          colnames(loslist_lci[[k]])[nstates+1]  ="timevar"  
        } 
        
        loslist_lci
        
        ################################################################################
        ##################### Total length of stay uci #################################
        ################################################################################# 
        #loslist_uci=list()
        for (k in states) {
          loslist_uci[[k]]=los_array_uci[,,k]
          
          if (length(vartime)==1) {loslist_uci[[k]]=as.data.frame(t(as.matrix(loslist_uci[[k]])))}
          else if (length(vartime)>1) {loslist_uci[[k]]=as.data.frame(as.matrix(loslist_uci[[k]]))}
          
          # loslist_uci[[k]]=as.data.frame(as.matrix(loslist_uci[[k]]))
          
        }
        
        for (k in states) {
          for (j in states) {
            colnames(loslist_uci[[k]])[j]=paste0("Los_",k,"_to_",j,"_uci")
          }
          colnames(loslist_uci[[k]])[nstates+1]  ="timevar"  
        } 
        
        loslist_uci
      }
    }
    
    
    ###################################################################################################################
    
    ###########################################################################
    ############## Total length of stay #######################################
    
    ###### semi markov models ##############
    
    if (process=="semiMarkov") {
      
      
      los_array= array(dim=c(length(vartime),nstates+1,nstates),NA)
      los_array_lci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      los_array_uci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      
      
      
      los=list()
      los_ci=list()
      p=1
      for (j in states) {
        
        
        for (k in 1:length(vartime)) {
          
          los_ci[[k]]= totlos.simfs(x=model, trans=qmat, t = vartime[k], start = j, 
                                   ci = TRUE,
                                    tvar = "trans", tcovs = tcovs, group = NULL,
                                    M = Mjson, B = B.json, cl = cl.json)
          p=p+1  
          
        }
        los[[j]]=los_ci
      }
      
      
      
      if (ci.json==TRUE) {
        
        for (i in 1: length(vartime)  ) {
          for (j in 1: length(states)  ) {
            for (k in 1: length(states)  ) {
              los_array[i,k,j]=t(cbind(as.matrix(los[[j]][[i]][,1][k])))
              los_array_lci[i,k,j]=t(cbind(as.matrix(los[[j]][[i]][,2][k])))
              los_array_uci[i,k,j]=t(cbind(as.matrix(los[[j]][[i]][,3][k])))
            }
            los_array[i,nstates+1,j]=vartime[i]
            los_array_lci[i,nstates+1,j]=vartime[i]
            los_array_uci[i,nstates+1,j]=vartime[i]
            
          }
        }
        
      }    
      
      
      if (ci.json==FALSE) {
        for (i in 1: length(vartime)  ) {
          for (j in 1: length(states)  ) {
            for (k in 1: length(states)  ) {
              los_array[i,k,j]=t(cbind(as.matrix(los[[j]][[i]][k])))
            }
            los_array[i,nstates+1,j]=vartime[i]
          }
        }
        
      } 
      
      
      loslist=list()
      
      for (k in states) {
        loslist[[k]]=los_array[,,k]
        
        if (length(vartime)==1) {loslist[[k]]=as.data.frame(t(as.matrix(loslist[[k]])))}
        else if (length(vartime)>1) {loslist[[k]]=as.data.frame(as.matrix(loslist[[k]]))}
        
        
        # loslist[[k]]=as.data.frame(as.matrix(loslist[[k]]))
      }
      
      for (k in states) {
        for (j in states) {
          colnames(loslist[[k]])[j]=paste0("Los_",k,"_to_",j)
        }
        colnames(loslist[[k]])[nstates+1]  ="timevar"  
      } 
      loslist
      
      
      
      loslist_lci=list()
      loslist_uci=list()
      
      if (ci.json==TRUE) {
        
        ##############################################################################
        ##################### Total length of stay lci ###############################
        ##############################################################################
        
        for (k in states) {
          loslist_lci[[k]]=los_array_lci[,,k]
          
          
          if (length(vartime)==1) {loslist_lci[[k]]=as.data.frame(t(as.matrix(loslist_lci[[k]])))}
          else if (length(vartime)>1) {loslist_lci[[k]]=as.data.frame(as.matrix(loslist_lci[[k]]))}
          
          
          # loslist_lci[[k]]=as.data.frame(as.matrix(loslist_lci[[k]]))
        }
        
        for (k in states) {
          for (j in states) {
            colnames(loslist_lci[[k]])[j]=paste0("Los_",k,"_to_",j,"_lci")
          }
          colnames(loslist_lci[[k]])[nstates+1]  ="timevar"  
        } 
        
        loslist_lci
        
        ##################################################################################
        ##################### Total length of stay uci ##################################
        ################################################################################
        
        loslist_uci=list()
        for (k in states) {
          loslist_uci[[k]]=los_array_uci[,,k]
          
          if (length(vartime)==1) {loslist_uci[[k]]=as.data.frame(t(as.matrix(loslist_uci[[k]])))}
          else if (length(vartime)>1) {loslist_uci[[k]]=as.data.frame(as.matrix(loslist_uci[[k]]))}
          
          
          # loslist_uci[[k]]=as.data.frame(as.matrix(loslist_uci[[k]]))
        }
        
        for (k in states) {
          for (j in states) {
            colnames(loslist_uci[[k]])[j]=paste0("Los_",k,"_to_",j,"_uci")
          }
          colnames(loslist_uci[[k]])[nstates+1]  ="timevar"  
        } 
        
        loslist_uci
      }
      
      
      
    }
    pred[[1]]=  list(probs=pmlist,probs_lci=pmlist_lci,probs_uci=pmlist_uci,
                     trans=trans, is.cumhaz=is.cumhaz,
                     los=loslist,los_lci=loslist_lci,los_uci=loslist_uci)
    
  }
  
  
if (length(covariates_list) != 0) {

  for (g in 1:length(covariates_list)) {
  #  g=1
    
    try( if (process!="Markov" & process!="semiMarkov") stop("Process must be either Markov or semiMarkov"))
    
    ### Calculate nstates, ntransitions #### 
     nstates=ncol(qmat)
      states=c(seq(1:nstates))
    
      tmat2=qmat
      tmat2[is.na(tmat2)] <- 0
     Ntransitions=length(tmat2[which(tmat2>0)])
    
    #################### Probabilities #######################################
    
    pm_array= array(dim=c(length(vartime),nstates+1,nstates),NA)
    pm_array_lci= array(dim=c(length(vartime),nstates+1,nstates),NA)
    pm_array_uci= array(dim=c(length(vartime),nstates+1,nstates),NA)
    
    #g=1
    if (process=="Markov") {
      pm_ci=list()
      for (k in 1:length(vartime)) {
            pm_ci[[k]]=pmatrix.fs(x=model, trans=qmat, t =vartime[k], newdata=covariates_list[[g]],
                            sing.inf = 1e+10, B = B.json, ci = ci.json, cl = cl.json)
  
      }
    }
    
    if (process=="semiMarkov") {
      pm_ci=list()
     #p=1
      for (k in 1:length(vartime)) {
        
        pm_ci[[k]]= pmatrix.simfs(x=model, trans=qmat, t = vartime[k],  newdata =covariates_list[[g]], ci = ci.json,
                                  tcovs = tcovs, B = B.json, cl = cl.json, M = Mjson)
     #   p=p+1  
      }
      
    }
    
    
    if (ci.json==FALSE) {
      for (i in 1: length(vartime)  ) {
        pm_array[i,,]=t(cbind(as.matrix(pm_ci[[i]]),vartime[i] ))
      }
    }
    
    if (ci.json==TRUE) {
      
      for (i in 1: length(vartime)  ) {
        pm_array[i,,]=    t(cbind(as.matrix(pm_ci[[i]]),vartime[i] ))
        pm_array_lci[i,,]=t(cbind(attributes(pm_ci[[i]])$lower,vartime[i]  ))
        pm_array_uci[i,,]=t(cbind(attributes(pm_ci[[i]])$upper,vartime[i]  ))
        
      }
    }
    
    
    pmlist=list()
    
    for (k in states) {
      pmlist[[k]]=pm_array[,,k]
      
      if (length(vartime)==1) {pmlist[[k]]=as.data.frame(t(as.matrix(pmlist[[k]])))}
      else if (length(vartime)>1) {pmlist[[k]]=as.data.frame(as.matrix(pmlist[[k]]))}
      
      # pmlist[[k]]=as.data.frame(as.matrix(pmlist[[k]]))
      #pmlist[[k]]=as.data.frame(t(as.matrix(pmlist[[k]])))
      #pmlist[[k]]=as.data.frame(pmlist[[k]])
    }
    
    for (k in states) {
      for (j in states) {
       
        colnames(pmlist[[k]])[j]=paste0("P_",k,"_to_",j)
      }
      colnames(pmlist[[k]])[nstates+1]  ="timevar"  
    } 
    pmlist
    
    pmlist_lci=list()
    pmlist_uci=list()
    
    
    if (ci.json==TRUE) {
      
      ##################### Probabilities lci #####################################################################
      
      
      for (k in states) {
        pmlist_lci[[k]]=pm_array_lci[,,k]
        
        if (length(vartime)==1) {pmlist_lci[[k]]=as.data.frame(t(as.matrix(pmlist_lci[[k]])))}
        else if (length(vartime)>1) {pmlist_lci[[k]]=as.data.frame(as.matrix(pmlist_lci[[k]]))}
        
        #pmlist_lci[[k]]=as.data.frame(as.matrix(pmlist_lci[[k]]))
        
      }
      
      for (k in states) {
        for (j in states) {
          colnames(pmlist_lci[[k]])[j]=paste0("P_",k,"_to_",j,"_lci")
        }
        colnames(pmlist_lci[[k]])[nstates+1]  ="timevar"  
      } 
      
      pmlist_lci
      
      ##################### Probabilities uci #####################################################################
      
      #pmlist_uci=list()
      for (k in states) {
        pmlist_uci[[k]]=pm_array_uci[,,k]
        
        if (length(vartime)==1) {pmlist_uci[[k]]=as.data.frame(t(as.matrix(pmlist_uci[[k]])))}
        else if (length(vartime)>1) {pmlist_uci[[k]]=as.data.frame(as.matrix(pmlist_uci[[k]]))}
        
      #  pmlist_uci[[k]]=as.data.frame(as.matrix(pmlist_uci[[k]]))
      }
      
      for (k in states) {
        for (j in states) {
          colnames(pmlist_uci[[k]])[j]=paste0("P_",k,"_to_",j,"_uci")
        }
        colnames(pmlist_uci[[k]])[nstates+1]  ="timevar"  
      } 
      
      pmlist_uci
      
      
    }
    
    
    
    
    #################### Cummulative transition intensities #######################################
    
    transm=msfit.flexsurvreg(object=model, t=vartime, newdata = covariates_list[[g]], variance = variance,
                             tvar = "trans", trans=qmat, B = B.json)$Haz
    
    
    
    trans= reshape(transm, idvar = "time", timevar = "trans", direction = "wide")
    trans$timevar=trans$time
    trans=trans[,-1]
    namesh=vector()
    for (i in 1:Ntransitions) {namesh[i]=paste0("Haz_",tr_start_state[i],"_to_",tr_end_state[i])}
    names(trans)[1:Ntransitions]=namesh
    
    
    is.cumhaz=1
    
    #################################################################################
    #################### Total length of stay #######################################
    #################################################################################
    
    #### Markov models###
    
    
    if (process=="Markov") {
      
      los_array= array(dim=c(length(vartime),nstates+1,nstates),NA)
      los_array_lci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      los_array_uci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      
      los_ci=list()
      
      for (k in 1:length(vartime)) {
      
      
          los_ci[[k]]= totlos.fs(x=model, trans=qmat, t = vartime[k], newdata =covariates_list[[g]] , ci = ci.json, tvar = "trans",
                        sing.inf = 1e+5, B = B.json, cl = cl.json)
      
      }
      
      if (ci.json==FALSE) {
        for (i in 1: length(vartime)  ) {
          los_array[i,,]=t(cbind(as.matrix(los_ci[[i]]),vartime[i] ))
        }
      }
      if (ci.json==TRUE) {
        
        for (i in 1: length(vartime)  ) {
          los_array[i,,]=    t(cbind(as.matrix(los_ci[[i]]),vartime[i] ))
          los_array_lci[i,,]=t(cbind(attributes(los_ci[[i]])$lower,vartime[i]  ))
          los_array_uci[i,,]=t(cbind(attributes(los_ci[[i]])$upper,vartime[i]  ))
          
        }
      }
      
      loslist=list()
      
      for (k in states) {
        loslist[[k]]=los_array[,,k]
        
        if (length(vartime)==1) {loslist[[k]]=as.data.frame(t(as.matrix(loslist[[k]])))}
        else if (length(vartime)>1) {loslist[[k]]=as.data.frame(as.matrix(loslist[[k]]))}
        
      }
      
      for (k in states) {
        for (j in states) {
          colnames(loslist[[k]])[j]=paste0("Los_",k,"_to_",j)
        }
        colnames(loslist[[k]])[nstates+1]  ="timevar"  
      } 
      loslist
      
      loslist_lci=list()
      loslist_uci=list()
      
      if (ci.json==TRUE) {
        
        ##################################################################################################
        ##################### Total length of stay lci ##################################################
        #################################################################################################
        
        for (k in states) {
          loslist_lci[[k]]=los_array_lci[,,k]
          
          if (length(vartime)==1) {loslist_lci[[k]]=as.data.frame(t(as.matrix(loslist_lci[[k]])))}
          else if (length(vartime)>1) {loslist_lci[[k]]=as.data.frame(as.matrix(loslist_lci[[k]]))}
          
         # loslist_lci[[k]]=as.data.frame(as.matrix(loslist_lci[[k]]))
        }
        
        for (k in states) {
          for (j in states) {
            colnames(loslist_lci[[k]])[j]=paste0("Los_",k,"_to_",j,"_lci")
          }
          colnames(loslist_lci[[k]])[nstates+1]  ="timevar"  
        } 
        
        loslist_lci
        
        ################################################################################
        ##################### Total length of stay uci #################################
        ################################################################################# 
        #loslist_uci=list()
        for (k in states) {
          loslist_uci[[k]]=los_array_uci[,,k]
          
          if (length(vartime)==1) {loslist_uci[[k]]=as.data.frame(t(as.matrix(loslist_uci[[k]])))}
          else if (length(vartime)>1) {loslist_uci[[k]]=as.data.frame(as.matrix(loslist_uci[[k]]))}
          
         # loslist_uci[[k]]=as.data.frame(as.matrix(loslist_uci[[k]]))
          
        }
        
        for (k in states) {
          for (j in states) {
            colnames(loslist_uci[[k]])[j]=paste0("Los_",k,"_to_",j,"_uci")
          }
          colnames(loslist_uci[[k]])[nstates+1]  ="timevar"  
        } 
        
        loslist_uci
      }
    }
    
    
    ###################################################################################################################
    
    ###########################################################################
    ############## Total length of stay #######################################
    
    ###### semi markov models ##############
    
    if (process=="semiMarkov") {
      
      
      los_array= array(dim=c(length(vartime),nstates+1,nstates),NA)
      los_array_lci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      los_array_uci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      
      
      
      los=list()
      los_ci=list()
      p=1
      for (j in states) {
        
        
        for (k in 1:length(vartime)) {
          
          los_ci[[k]]= totlos.simfs(x=model, trans=qmat, t = vartime[k], start = j, 
                                    newdata =covariates_list[[g]], ci = TRUE,
                                    tvar = "trans", tcovs = tcovs, group = NULL,
                                    M = Mjson, B = B.json, cl = cl.json)
          p=p+1  
          
        }
        los[[j]]=los_ci
      }
      
      
      
      if (ci.json==TRUE) {
        
        for (i in 1: length(vartime)  ) {
          for (j in 1: length(states)  ) {
            for (k in 1: length(states)  ) {
              los_array[i,k,j]=t(cbind(as.matrix(los[[j]][[i]][,1][k])))
              los_array_lci[i,k,j]=t(cbind(as.matrix(los[[j]][[i]][,2][k])))
              los_array_uci[i,k,j]=t(cbind(as.matrix(los[[j]][[i]][,3][k])))
            }
            los_array[i,nstates+1,j]=vartime[i]
            los_array_lci[i,nstates+1,j]=vartime[i]
            los_array_uci[i,nstates+1,j]=vartime[i]
            
          }
        }
        
      }    
      
      
      if (ci.json==FALSE) {
        for (i in 1: length(vartime)  ) {
          for (j in 1: length(states)  ) {
            for (k in 1: length(states)  ) {
              los_array[i,k,j]=t(cbind(as.matrix(los[[j]][[i]][k])))
            }
            los_array[i,nstates+1,j]=vartime[i]
          }
        }
        
      } 
      
      
      loslist=list()
      
      for (k in states) {
        loslist[[k]]=los_array[,,k]
        
        if (length(vartime)==1) {loslist[[k]]=as.data.frame(t(as.matrix(loslist[[k]])))}
        else if (length(vartime)>1) {loslist[[k]]=as.data.frame(as.matrix(loslist[[k]]))}
        
        
       # loslist[[k]]=as.data.frame(as.matrix(loslist[[k]]))
      }
      
      for (k in states) {
        for (j in states) {
          colnames(loslist[[k]])[j]=paste0("Los_",k,"_to_",j)
        }
        colnames(loslist[[k]])[nstates+1]  ="timevar"  
      } 
      loslist
      
      
      
      loslist_lci=list()
      loslist_uci=list()
      
      if (ci.json==TRUE) {
        
        ##############################################################################
        ##################### Total length of stay lci ###############################
        ##############################################################################
        
        for (k in states) {
          loslist_lci[[k]]=los_array_lci[,,k]
          
          
          if (length(vartime)==1) {loslist_lci[[k]]=as.data.frame(t(as.matrix(loslist_lci[[k]])))}
          else if (length(vartime)>1) {loslist_lci[[k]]=as.data.frame(as.matrix(loslist_lci[[k]]))}
          
          
         # loslist_lci[[k]]=as.data.frame(as.matrix(loslist_lci[[k]]))
        }
        
        for (k in states) {
          for (j in states) {
            colnames(loslist_lci[[k]])[j]=paste0("Los_",k,"_to_",j,"_lci")
          }
          colnames(loslist_lci[[k]])[nstates+1]  ="timevar"  
        } 
        
        loslist_lci
        
        ##################################################################################
        ##################### Total length of stay uci ##################################
        ################################################################################
        
        loslist_uci=list()
        for (k in states) {
          loslist_uci[[k]]=los_array_uci[,,k]
          
          if (length(vartime)==1) {loslist_uci[[k]]=as.data.frame(t(as.matrix(loslist_uci[[k]])))}
          else if (length(vartime)>1) {loslist_uci[[k]]=as.data.frame(as.matrix(loslist_uci[[k]]))}
          
          
         # loslist_uci[[k]]=as.data.frame(as.matrix(loslist_uci[[k]]))
        }
        
        for (k in states) {
          for (j in states) {
            colnames(loslist_uci[[k]])[j]=paste0("Los_",k,"_to_",j,"_uci")
          }
          colnames(loslist_uci[[k]])[nstates+1]  ="timevar"  
        } 
        
        loslist_uci
      }
      
      
      
    }
    
    
    pred[[g]]=  list(probs=pmlist,probs_lci=pmlist_lci,probs_uci=pmlist_uci,
         trans=trans, is.cumhaz=is.cumhaz,
         los=loslist,los_lci=loslist_lci,los_uci=loslist_uci)
  }

}  
  
  
  rm(list=ls(pattern="^forFlex"))

  ########################################################################################################
  ######## Declare the list elements even if they stay empty ####
  
  pjson    =list()
  pjson_lci=list()
  pjson_uci=list()
  hjson=list()
  losjson=list()
  losjson_lci=list()
  losjson_uci=list()
  
  
  ########################################################################################################
  ####  Probabilities
  ########################################################################################################
  
  ##### Main estimates 
  
  pjson=list()
  
  if (length(covariates_list)<=1) {
    p=1
    for (k in 1:nstates) {
      for (i in 1:nstates) {
        
        pjson[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$probs[[k]][,i])))
        pjson[[p]]= assign(paste0("forFlex",names(pred[[1]]$probs[[k]])[i]),  pjson[[p]])
        p=p+1  
      }
    }
  }
  
  if (length(covariates_list)>1) {
    
    p=1
    
     if (process=="Markov") {
       for (k in 1:nstates) {
         for (i in 1:nstates) {
           
           tmplist=matrix(nrow =length(covariates_list) , ncol= length(vartime) )
           tmplist[1,]=as.vector(pred[[1]]$probs[[k]][,i])
           
           for (j in 2:length(covariates_list)) {
             tmplist[j,]=as.vector(pred[[j]]$probs[[k]][,i])  
           }
           pjson[[p]]=tmplist
           
           assign(paste0("forFlex",names(pred[[1]]$probs[[k]])[i]),  pjson[[p]])
           
           p=p+1  
           
         }
       }
     }
    
     if (process=="semiMarkov") {
       for (k in states) {
         for (i in states) {
          
          tmplist=matrix(nrow =length(covariates_list) , ncol= length(vartime)  )
          tmplist[1,]=as.vector(pred[[1]]$probs[[k]][,i])
          
          for (j in 2:length(covariates_list)) {
            tmplist[j,]=as.vector(pred[[j]]$probs[[k]][,i])  
          }
          pjson[[p]]=tmplist
          
          assign(paste0("forFlex",names(pred[[1]]$probs[[k]])[i]),  pjson[[p]])
          
          p=p+1  
          
        }
      }
    }
  }
  
  pvector=vector()
  pvector=ls(pattern = "forFlexP_._to_.$")  
  
  pvector=sub("forFlex","",ls(pattern = "^forFlexP_._to_.$") )
  
  pvector=stringi::stri_sort(pvector, numeric = TRUE)
  
  names(pjson)=pvector[1:(p-1)]
  
  pjson_new=list()
  
  for (d in 1:length(pjson)) {
    
    pjson_new[[d]]=list()
    
   
   if (length(covariates_list)>=1) { 
    for (c in 1:length(covariates_list)) {
      
      pjson_new[[d]][[c]]=as.vector(pjson[[d]][c,])
    }
   }
    
   if (length(covariates_list)==0) { 
     
       
       pjson_new[[d]][[1]]=as.vector(pjson[[d]][1,])
     
   }
   
  }
  names(pjson_new)=names(pjson)
  pjson=pjson_new
  
 # for (d in 1:length(pjson)) {
#   pjson[[d]]=tapply(pjson[[d]],rep(1:nrow(pjson[[d]]),each=ncol(pjson[[d]])),function(i)i)
#    pjson[[d]]=as.list(as.data.frame( pjson[[d]]))
#  }
  
  
  ##### Lci possibilities estimates
  
  if (ci.json==TRUE ) {
    
    
    pjson_lci=list()
    
    if (length(covariates_list)<=1) {
      p=1
      for (k in states) {
        for (i in states) {
          
          pjson_lci[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$probs_lci[[k]][,i])))
          assign(paste0("forFlex",names(pred[[1]]$probs_lci[[k]])[i]),  pjson_lci[[p]])
          p=p+1  
        }
      }
    }
    
    if (length(covariates_list)>1) {
      
      p=1
      for (k in states) {
        for (i in states) {
          
          tmplist=matrix(nrow =length(covariates_list) , ncol= length(vartime))
          tmplist[1,]=as.vector(pred[[1]]$probs_lci[[k]][,i])
          
          for (j in 2:length(covariates_list)) {
            tmplist[j,]=as.vector(pred[[j]]$probs_lci[[k]][,i])  
          }
          pjson_lci[[p]]=tmplist
          
          assign(paste0("forFlex",names(pred[[1]]$probs_lci[[k]])[i]),  pjson_lci[[p]])
          
          p=p+1  
          
          
        }
      }
    }
    
    pvector_lci=vector()
    pvector_lci=ls(pattern = "^forFlexP.*lci$")  
    
    pvector_lci=sub("forFlex","",ls(pattern = "^forFlexP.*lci$") )
    
    pvector_lci=stringi::stri_sort(pvector_lci, numeric = TRUE)
    
    names(pjson_lci)=pvector_lci[1:(p-1)]
    

    pjson_lci_new=list()
    
    for (d in 1:length(pjson_lci)) {
      
      pjson_lci_new[[d]]=list()
      
      
      if (length(covariates_list)>=1) { 
        for (c in 1:length(covariates_list)) {
          
          pjson_lci_new[[d]][[c]]=as.vector(pjson_lci[[d]][c,])
        }
      }
      
      if (length(covariates_list)==0) { 
        
        
        pjson_lci_new[[d]][[1]]=as.vector(pjson_lci[[d]][1,])
        
      }
      
    }  
    names(pjson_lci_new)=names(pjson_lci)
    pjson_lci=pjson_lci_new 
    
    
    ##### Uci possibilities estimates
    
    
    pjson_uci=list()
    
    if (length(covariates_list)<=1) {
      p=1
      for (k in states) {
        for (i in states) {
          
          pjson_uci[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$probs_uci[[k]][,i])))
          assign(paste0("forFlex",names(pred[[1]]$probs_uci[[k]])[i]),  pjson_uci[[p]])
          p=p+1  
        }
      }
    }
    
    if (length(covariates_list)>1) {
      
      p=1
      for (k in states) {
        for (i in states) {
          
          tmplist=matrix(nrow =length(covariates_list) , ncol= length(vartime))
          tmplist[1,]=as.vector(pred[[1]]$probs_uci[[k]][,i])
          
          for (j in 2:length(covariates_list)) {
            tmplist[j,]=as.vector(pred[[j]]$probs_uci[[k]][,i])  
          }
          pjson_uci[[p]]=tmplist
          
          assign(paste0("forFlex",names(pred[[1]]$probs_uci[[k]])[i]),  pjson_uci[[p]])
          
          p=p+1  
          
          
        }
      }
    }
    
    pvector_uci=vector()
    pvector_uci=ls(pattern = "^forFlexP.*uci$")  
    
    pvector_uci=sub("forFlex","",ls(pattern = "^forFlexP.*uci$") )
    
    pvector_uci=stringi::stri_sort(pvector_uci, numeric = TRUE)
    
    names(pjson_uci)=pvector_uci[1:(p-1)]
    
    
    pjson_uci_new=list()
    
    for (d in 1:length(pjson_uci)) {
      
      pjson_uci_new[[d]]=list()
      
      
      if (length(covariates_list)>=1) { 
        for (c in 1:length(covariates_list)) {
          
          pjson_uci_new[[d]][[c]]=as.vector(pjson_uci[[d]][c,])
        }
      }
      
      if (length(covariates_list)==0) { 
        
        
        pjson_uci_new[[d]][[1]]=as.vector(pjson_uci[[d]][1,])
        
      }
      
    } 
    
    names(pjson_uci_new)=names(pjson_uci)
    pjson_uci=pjson_uci_new 
    
    
    
  } 
  
  #################################################################################################################
  
  ### Transitions main estimates ###
  hjson=list()
  
  if (length(covariates_list)<=1) {
    
    for (k in 1:ntransitions) {
      
      tmplisth=matrix(nrow =1 , ncol= length(vartime))
      tmplisth[1,]=as.vector(pred[[1]]$trans[,k])
      hjson[[1]]=tmplisth
      assign(paste0("forFlex",names(pred[[1]]$trans)[i]),  hjson[[1]])
    }
    
  }
    
  if (length(covariates_list)>1) {
    
    
    
    for (k in 1:ntransitions) {
      
      tmplisth=matrix(nrow =length(covariates_list) , ncol= length(vartime))
      
      tmplisth[1,]=as.vector(pred[[1]]$trans[,k])
      
      for (j in 2:length(covariates_list)) {
        
        tmplisth[j,]=as.vector(pred[[j]]$trans[,k])  
      }
      
      
      hjson[[k]]=tmplisth
      assign(paste0("forFlex",names(pred[[1]]$trans)[k]),  hjson[[k]])
    }
  }
  
  hvector=vector()
  hvector=ls(pattern = "^forFlexHaz_._to_.$")  
  
  hvector=sub("forFlex","",ls(pattern = "^forFlexHaz_._to_.$" ) )
  
  hvector=stringi::stri_sort(hvector, numeric = TRUE)
  
  names(hjson)=hvector
  
 # for (d in 1:length(hjson)) {
    
#    hjson[[d]]=tapply(hjson[[d]],rep(1:nrow(hjson[[d]]),each=ncol(hjson[[d]])),function(i)i)
#    hjson[[d]]=as.list(as.data.frame( hjson[[d]]))
#  }
  
  hjson_new=list()
  

  for (d in 1:length(hjson)) {
    hjson_new[[d]]=list()
    
    
    if (length(covariates_list)>=1) { 
      for (c in 1:length(covariates_list)) {
        
        hjson_new[[d]][[c]]=as.vector(hjson[[d]][c,])
      }
    }
    
    if (length(covariates_list)==0) { 
      
      
      hjson_new[[d]][[1]]=as.vector(hjson[[d]][1,])
      
    }
    
    
    
  } 
  
  
  
  
  names(hjson_new)=names(hjson)
  hjson=hjson_new  

  
  
  if (totlos==TRUE) {
    
    ### Los Main estimates #####
    losjson=list()
    
    if (length(covariates_list)<=1) {
      p=1
      for (k in 1:nstates) {
        for (i in 1:nstates) {
          
          losjson[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$los[[k]][,i])))
          assign(paste0("forFlex",names(pred[[1]]$los[[k]])[i]),  losjson[[p]])
          p=p+1  
        }
      }
    }
    
    if (length(covariates_list)>1) {
      
      p=1
      for (k in states) {
        for (i in states) {
          
          tmplistlos=matrix(nrow =length(covariates_list) , ncol= length(vartime))
          tmplistlos[1,]=as.vector(pred[[1]]$los[[k]][,i])
          
          for (j in 2:length(covariates_list)) {
            tmplistlos[j,]=as.vector(pred[[j]]$los[[k]][,i])  
          }
          losjson[[p]]=tmplistlos
          
          assign(paste0("forFlex",names(pred[[1]]$los[[k]])[i]),  losjson[[p]])
          
          p=p+1  
        }
      }
    }
    
    losvector=vector()
    losvector=ls(pattern = "^forFlexLos.*$")  
    
    losvector=sub("forFlex","",ls(pattern = "^forFlexLos.*$") )
    
    losvector=stringi::stri_sort(losvector, numeric = TRUE)
    
    names(losjson)=losvector
    
    #    for (d in 1:length(losjson)) {
    
    #     losjson[[d]]=tapply(losjson[[d]],rep(1:nrow(losjson[[d]]),each=ncol(losjson[[d]])),function(i)i)
    #     losjson[[d]]=as.list(as.data.frame( losjson[[d]]))
    #   }
    
    
    losjson_new=list()
    
    
    for (d in 1:length(losjson)) {
      
      losjson_new[[d]]=list()
      
      
      if (length(covariates_list)>=1) { 
        for (c in 1:length(covariates_list)) {
          
          losjson_new[[d]][[c]]=as.vector(losjson[[d]][c,])
        }
      }
      
      if (length(covariates_list)==0) { 
        
        
        losjson_new[[d]][[1]]=as.vector(losjson[[d]][1,])
        
      }
      
    } 
    
    
    hjson_new=list()
    
    
    
    
    names(losjson_new)=names(losjson)
    losjson=losjson_new  
    
    
  

    
    if (ci.json==TRUE ) {
      
      
      ### Los lci estimates #####
      
      losjson_lci=list()
      
      if (length(covariates_list)<=1) {
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            losjson_lci[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$los_lci[[k]][,i])))
            assign(paste0("forFlex",names(pred[[1]]$los_lci[[k]])[i]),  losjson_lci[[p]])
            p=p+1  
          }
        }
      }
      
      if (length(covariates_list)>1) {
        
        p=1
        for (k in states) {
          for (i in states) {
            
            tmplistlos=matrix(nrow =length(covariates_list) , ncol= length(vartime))
            tmplistlos[1,]=as.vector(pred[[1]]$los_lci[[k]][,i])
            
            for (j in 2:length(covariates_list)) {
              tmplistlos[j,]=as.vector(pred[[j]]$los_lci[[k]][,i])  
            }
            losjson_lci[[p]]=tmplistlos
            
            assign(paste0("forFlex",names(pred[[1]]$los_lci[[k]])[i]),  losjson_lci[[p]])
            
            p=p+1  
          }
        }
      }
      
      losvector_lci=vector()
      losvector_lci=ls(pattern = "^forFlexLos.*lci$")  
      
      losvector_lci=sub("forFlex","",ls(pattern = "^forFlexLos.*lci$") )
      
      losvector_lci=stringi::stri_sort(losvector_lci, numeric = TRUE)
      
      names(losjson_lci)=losvector_lci
      
      #    for (d in 1:length(losjson_lci)) {
      
      #     losjson_lci[[d]]=tapply(losjson_lci[[d]],rep(1:nrow(losjson_lci[[d]]),each=ncol(losjson_lci[[d]])),function(i)i)
      #     losjson_lci[[d]]=as.list(as.data.frame( losjson_lci[[d]]))
      #   }
      
      
      losjson_lci_new=list()
      
      for (d in 1:length(losjson_lci)) {
        
        losjson_lci_new[[d]]=list()
        
        
        if (length(covariates_list)>=1) { 
          for (c in 1:length(covariates_list)) {
            
            losjson_lci_new[[d]][[c]]=as.vector(losjson_lci[[d]][c,])
          }
        }
        
        if (length(covariates_list)==0) { 
          
          
          losjson_lci_new[[d]][[1]]=as.vector(losjson_lci[[d]][1,])
          
        }
        
      } 
      
      names(losjson_lci_new)=names(losjson_lci)
      losjson_lci=losjson_lci_new  
      

      
      
      ### Los uci estimates #####
      
      losjson_uci=list()
      
      if (length(covariates_list)<=1) {
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            losjson_uci[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$los_uci[[k]][,i])))
            assign(paste0("forFlex",names(pred[[1]]$los_uci[[k]])[i]),  losjson_uci[[p]])
            p=p+1  
          }
        }
      }
      
      if (length(covariates_list)>1) {
        
        p=1
        for (k in states) {
          for (i in states) {
            
            tmplistlos=matrix(nrow =length(covariates_list) , ncol= length(vartime))
            tmplistlos[1,]=as.vector(pred[[1]]$los_uci[[k]][,i])
            
            for (j in 2:length(covariates_list)) {
              tmplistlos[j,]=as.vector(pred[[j]]$los_uci[[k]][,i])  
            }
            losjson_uci[[p]]=tmplistlos
            
            assign(paste0("forFlex",names(pred[[1]]$los_uci[[k]])[i]),  losjson_uci[[p]])
            
            p=p+1  
          }
        }
      }
      
      losvector_uci=vector()
      losvector_uci=ls(pattern = "^forFlexLos.*uci$")  
      
      losvector_uci=sub("forFlex","",ls(pattern = "^forFlexLos.*uci$") )
      
      losvector_uci=stringi::stri_sort(losvector_uci, numeric = TRUE)
      
      names(losjson_uci)=losvector_uci
      
  #    for (d in 1:length(losjson_uci)) {
        
   #     losjson_uci[[d]]=tapply(losjson_uci[[d]],rep(1:nrow(losjson_uci[[d]]),each=ncol(losjson_uci[[d]])),function(i)i)
   #     losjson_uci[[d]]=as.list(as.data.frame( losjson_uci[[d]]))
   #   }
      
      
      losjson_uci_new=list()
      
      for (d in 1:length(losjson_uci)) {
        
        losjson_uci_new[[d]]=list()
        
        
        if (length(covariates_list)>=1) { 
          for (c in 1:length(covariates_list)) {
            
            losjson_uci_new[[d]][[c]]=as.vector(losjson_uci[[d]][c,])
          }
        }
        
        if (length(covariates_list)==0) { 
          
          
          losjson_uci_new[[d]][[1]]=as.vector(losjson_uci[[d]][1,])
          
        }
        
      } 
      
      names(losjson_uci_new)=names(losjson_uci)
      losjson_uci=losjson_uci_new  
      
      
 ###cijson
    }
  ###totlos  
  }     
  
  
  
  ##########################################################################################
  
  ### Timevar###
  
  timejson=list()
  
  timejson[[1]]=vartime
  
  ######################################################################################################
  
  atlistjson=list()

  
   name_paste=list() 

if (length(covariates_list)>=1) {
 
   atlistmatrix=matrix(nrow=1, ncol=length(covariates_list), NA)
  
  for (j in 1:length(covariates_list) ) {
    
    
    for (i in 1:ncol(covariates_list[[j]]) )  {
      
      name_paste[[i]]= paste0(names(covariates_list[[j]])[i]," ", as.character(covariates_list[[j]][1,i])  )
      
    }
  
    atlistmatrix[1,j]= paste(unlist(name_paste,recursive=FALSE), collapse = ' ')
   }
  }
  
if (length(covariates_list)==0) {
  atlistmatrix=matrix(nrow=1, ncol=1, NA)
  
      name_paste= "all"  
    
      atlistmatrix[1,1]= name_paste
  }

  
  atlistjson[[1]]= as.vector(atlistmatrix)
  
  
  
  ######################################################################################################
  is.cumhaz=1

  
  final_list=list(timevar=timejson, Nats=length(covariates_list) ,
                  atlist=atlistjson, tmat=tmatlist,  Ntransitions= Ntransitions,is.cumhaz=is.cumhaz, 
                  pjson,pjson_lci,pjson_uci,
                  hjson,
                  losjson,losjson_lci,losjson_uci )
  
 # final_list$timevar=as.vector(final_list$timevar)
  
  final_unlist= unlist(final_list, recursive=FALSE)
  
 # exportJson <- toJSON(final_unlist, pretty = TRUE,force = TRUE, flatten=TRUE, na='string')
  exportJson <- toJSON(final_unlist,force = TRUE, flatten=TRUE)
  
  
  write(exportJson, paste0(jsonpath,"/", name ) )
  
  rm(list=ls(pattern="^forFlex"))
  
  final_list
  
  }
