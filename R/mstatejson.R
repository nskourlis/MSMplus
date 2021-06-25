

#' @title mstatejson
#' @description Function mstatejson can receive models from mstate package.
#' It then uses functions from mstate package internally to estimate multi-state model 
#' measures such as transition probabilities, length of stay, and confidence intervals of the
#' estimations. Function mstatejson then take these results and reshapes them so that they can
#' be fed to MSMplus properly as a json file.
#' @param x The hazard model (or list of hazard models) 
#' @param qmat he user has to supply the transition matrix
#' @param process "Markov" for clock forward approach, "semiMarkov" 
#' for clock reset approach, Default: 'Markov'
#' @param totlos Estimate total length of stay spent in each state "TRUE", "FALSE",
#'  Default: "FALSE"
#' @param ci.json Estimate confidence intervals, "TRUE", "FALSE", Default: "FALSE"
#' @param cl.json Specify confidence level, Default: 0.95
#' @param B.json Number of simulations from the normal asymptotic distribution used to calculate variances. 
#' Decrease for greater speed at the expense of accuracy, Default: 100
#' @param variance A logical value indicating whether the (co-)variances of the subject-specific transition hazards should be computed. Default is FALSE
#' @param vartype A character string specifying the type of variances to be computed (so only needed if variance=TRUE). Possible values are "aalen" or "greenwood"
#' @param covariates_list The user can specify different covariate patterns
#' for which predictions will be made, Default: list()
#' @param Mjson Number of individuals to simulate in order to approximate the transition probabilities. 
#' Users should adjust this to obtain the required precision. Default: 100
#' @param jsonpath specify the path of the folder that the json file should be saved, Default: ""
#' @param name Specify the name of the output json file, Default: 'predictions.json'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE
#'  
#'library("MSMplus")
#'library("survival")
#'library("mstate")
#'
#'load("data/ebmt.rda", envir = parent.frame(), verbose = FALSE)
#'
#'
#'head(ebmt)
#' ### Let's first define the transition matrix
#'
#'tmat <- transMat(x = list(c(2, 3),c(3), c() ), names = c("Transplant", "Platelet Recovery", "Relapse/Death" ) )
#'
#' ### We will now create dummy variables for the age categories
#'
#'ebmt$age2=  recode(ebmt$age, ">40" =0, "20-40"=1,"<=20" =0 )
#'ebmt$age3=  recode(ebmt$age, ">40" =1, "20-40"=0,"<=20" =0 )
#'
#' #Data preparation- From one row per participant to multiple rows per participant, one for each allowed transition.
#'msebmt <- msprep(data = ebmt, trans = tmat, 
#'                 time = c(NA, "prtime", "rfstime"), status = c(NA, "prstat", "rfsstat"), keep=c("age2","age3"))
#'
#' ### Semi parametric analysis
#'
#'    #### Markov
#' ### Run the hazard models: Clock forward approach 
#' 
#' cfcox <- coxph(Surv(Tstart, Tstop, status) ~age2+age3+strata(trans), data = msebmt)
#' 
#' 
#' ### Prediction for different covariate patterns (the 3 age categories)
#' 
#' wh1 <- which(msebmt$age2 == 0 & msebmt$age3 == 0)
#' pat1 <- msebmt[rep(wh1[1], 3), 9:10]
#' pat1$trans <- 1:3
#' attr(pat1, "trans") <- tmat
#' pat1$strata <- pat1$trans
#' 
#' 
#' wh2 <- which(msebmt$age2 == 1 & msebmt$age3 == 0)
#' pat2 <- msebmt[rep(wh2[1], 3), 9:10]
#' pat2$trans <- 1:3
#' attr(pat2, "trans") <- tmat
#' pat2$strata <- pat2$trans
#' 
#' wh3 <- which(msebmt$age2 == 0 & msebmt$age3 == 1)
#' pat3 <- msebmt[rep(wh3[1], 3), 9:10]
#' pat3$trans <- 1:3
#' attr(pat3, "trans") <- tmat
#' pat3$strata <- pat3$trans
#' 
#' 
#' ##We now run the mstatejson function to perform the multi-state model analysis using the function 
#' ##from package mstate and the pack the predictions in a json file.
#'
#' results_semipar <- rpkg::mstatejson(x=cfcox,  qmat=tmat, process="Markov", 
#'                                     totlos=TRUE, ci.json=TRUE, cl.json=0.95, B.json=10,
#'                                     variance=FALSE, vartype="greenwood",
#'                                     covariates_list=list(pat1 ,pat2, pat3 ) , M=50,
#'                                     jsonpath="data",
#'                                     name="predictions_EBMT_mstate_fw.json")
#' 
#' 
#' results_semipar
#'  }
#' }
#' @seealso 
#'  \code{\link[stringi]{stri_sort}}
#' @rdname mstatejson
#' @export 
#' @importFrom stringi stri_sort
mstatejson <- function(x, qmat, process="Markov", 
                       totlos=FALSE, ci.json=FALSE, cl.json=0.95, B.json=10,
                       variance=FALSE, vartype="aalen",
                       covariates_list=list(), Mjson=50,  jsonpath="~",name="predictions.json"  )  {
  
  options(scipen = 999,"digits"=10)
  

  

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
  
  
  if (process=="Markov") clock="forward"
  if (process=="semiMarkov") clock="reset"
  
  
  ################################################################### 
  
  library("msm")
  library("stringi")
  library("RJSONIO")
  
  pred=list() 
  
  ### First case, overall predictions
  if (length(covariates_list) == 0) {
  

    
    #   g=1
    
    try( if (process!="Markov" & process!="semiMarkov") stop("Process must be either Markov or semiMarkov"))
    
    
    ### Calculate nstates, ntransitions, non absorbing states #### 
    nstates=ncol(qmat)
    states=c(seq(1:nstates))
    
    qmat2=qmat
    qmat2[is.na(qmat2)] <- 0
    Ntransitions=length(qmat2[which(qmat2>0)])
    
    
    modHaz <-  msfit(object=x,variance=variance,vartype=vartype, trans=qmat)
    
    ###################################    
    #      ##Cumulative hazard #####
    ####################################      
    #semi parametric semi markov
    transr= reshape(modHaz$Haz, idvar = "time", timevar = "trans", direction = "wide")
    transr$timevar=transr$time
    transr=transr[,-1]
    namesh=vector()
    for (i in 1:Ntransitions) {namesh[i]=paste0("Haz_",tr_start_state[i],"_to_",tr_end_state[i])}
    names(transr)[1:Ntransitions]=namesh
    
    ############################################################################
    #### Timevar#####
    
    timevar=transr$time
    
    
    ########################################################################
    
    ########## probabilities ################
    

    
    if (process=="Markov") {
      
      probm=list()
      
      
      ### Prob markov
      probm <- probtrans(modHaz,variance=variance, predt = 0, direction = "forward")
      
      for (i in 1:nstates) {
        probm[[i]]= probm[[i]][,    which(!startsWith(names(probm[[i]]), "se") )] 
        probm[[i]]= probm[[i]][which(probm[[i]]$pstate1>=0),] 
        probm[[i]]$timevar=probm[[i]]$time
        probm[[i]]=probm[[i]][,-1]
      }
      


      namesprob=vector()
      
      
      
      pmlist=list()
      
      for (k in states) {
        
          probm[[k]]=as.data.frame(as.matrix(probm[[k]]))
        
      }
      
      for (k in states) {
        for (j in states) {
          
          colnames(probm[[k]])[j]=paste0("P_",k,"_to_",j)
        }
        colnames(probm[[k]])[nstates+1]  ="timevar"  
      } 
      probm= probm[1:nstates]
      

      
      #############################################################
      #### Length of stay
      
      loslist=list()
      
      
      if (totlos==TRUE) {
        
        los_array= array(dim=c(length(timevar),nstates+1,nstates),NA)
        
        
        probm_all <- probtrans(modHaz,variance=variance, predt = 0, direction = "forward")
        
        for (j in 1:(nstates+1)) {
          
          for (r in 1:(nstates)) {
            
            for (i in 1:length(timevar)) {
              
              los.mstate <- ELOS(probm_all,tau=timevar[i])
              
              los_array[i,j,r]=rbind(t(los.mstate),timevar=timevar[i]  ) [j,r]
              
            }
          }
        }  
        
        
        
        for (k in states) {
          
          loslist[[k]]=los_array[,,k]
          loslist[[k]]=as.data.frame(loslist[[k]])
          
        }
        
        for (k in states) {
          for (j in states) {
            colnames(loslist[[k]])[j]=paste0("Los_",k,"_to_",j)
            
          }
          
          colnames(loslist[[k]])[nstates+1]  ="timevar"  
          
        }
        
        loslist  
        
      }
      
    }   
    
    if (process=="semiMarkov") {
      
      ### Prob semi markov
      set.seed(1234)
      tv <- unique(modHaz$Haz$time)
      probm=list()
      p=1
      for (start in states) {
        probm[[p]]=  mssample(Haz=modHaz$Haz[which(modHaz[[1]][,2]>=0),], trans=qmat,
                              history=list(state=start,time=0,tstate=NULL),
                              beta.state=NULL, clock=clock, output="state",
                              Mjson = 10, tvec = timevar, cens=NULL, do.trace=NULL)
        
        probm[[p]]$timevar=probm[[p]]$time
        probm[[p]]=probm[[p]][,-1]
        p=p+1   
      }
      
      
      for (k in states) {
        for (j in states) {
          colnames(probm[[k]])[j]=paste0("P_",k,"_to_",j)
        }
        colnames(probm[[k]])[nstates+1]  ="timevar"  
      } 
      probm

      
      
      loslist=list()
      
      
      if (totlos==TRUE) {
        
        los_array= array(dim=c(length(timevar),nstates+1,nstates),NA)
        
        
        probm_all <- probtrans(modHaz,variance=variance, predt = 0, direction = "forward")
        
        for (j in 1:(nstates+1)) {
          
          for (r in 1:(nstates)) {
            
            for (i in 1:length(timevar)) {
              
              los.mstate <- ELOS(probm_all,tau=timevar[i])
              
              los_array[i,j,r]=rbind(t(los.mstate),timevar=timevar[i]  ) [j,r]
              
            }
          }
        }  
        
        
        
        for (k in states) {
          
          loslist[[k]]=los_array[,,k]
          loslist[[k]]=as.data.frame(loslist[[k]])
          
        }
        
        for (k in states) {
          for (j in states) {
            colnames(loslist[[k]])[j]=paste0("Los_",k,"_to_",j)
            
          }
          
          colnames(loslist[[k]])[nstates+1]  ="timevar"  
          
        }
        loslist  
        
      }
      
    }
    is.cumhaz=1
    
    
    pred[[1]]=            list(probs=probm,
                               trans=transr, is.cumhaz=is.cumhaz,
                               los=loslist, timevar=timevar)   
  }
    

  
  
  if (length(covariates_list) != 0) {
  
    for (g in 1:length(covariates_list)) {
      
  
      
      try( if (process!="Markov" & process!="semiMarkov") stop("Process must be either Markov or semiMarkov"))
      
      
      ### Calculate nstates, ntransitions, non absorbing states #### 
      nstates=ncol(qmat)
      states=c(seq(1:nstates))
      
      qmat2=qmat
      qmat2[is.na(qmat2)] <- 0
      Ntransitions=length(qmat2[which(qmat2>0)])
      
      
      modHaz <-  msfit(object=x, newdata=as.data.frame(covariates_list[[g]]),
                       variance=variance,trans=qmat) 
      
      
      ###################################    
      #      ##Cumulative hazard #####
      ####################################      
      #semi parametric semi markov
      transr= reshape(modHaz$Haz, idvar = "time", timevar = "trans", direction = "wide")
      transr$timevar=transr$time
      transr=transr[,-1]
      namesh=vector()
      for (i in 1:Ntransitions) {namesh[i]=paste0("Haz_",tr_start_state[i],"_to_",tr_end_state[i])}
      names(transr)[1:Ntransitions]=namesh
      
      ############################################################################
      #### Timevar#####
      
      timevar=transr$time
      
      
      ########################################################################
      
      ########## probabilities ################
      
      if (process=="Markov") {
        
        probm=list()
        
        
        ### Prob markov
        probm <- probtrans(modHaz,variance=variance, predt = 0, direction = "forward")
        
        for (i in 1:nstates) {
          probm[[i]]= probm[[i]][,    which(!startsWith(names(probm[[i]]), "se") )] 
          probm[[i]]= probm[[i]][which(probm[[i]]$pstate1>=0),] 
          probm[[i]]$timevar=probm[[i]]$time
          probm[[i]]=probm[[i]][,-1]
        }
        
        
        
        namesprob=vector()
        
        
        
        pmlist=list()
        
        for (k in states) {
          
          probm[[k]]=as.data.frame(as.matrix(probm[[k]]))
          
        }
        
        for (k in states) {
          for (j in states) {
            
            colnames(probm[[k]])[j]=paste0("P_",k,"_to_",j)
          }
          colnames(probm[[k]])[nstates+1]  ="timevar"  
        } 
        probm= probm[1:nstates]
        
        
        
        
        #############################################################
        #### Length of stay
        
        loslist=list()
        
        
        if (totlos==TRUE) {
          
          los_array= array(dim=c(length(timevar),nstates+1,nstates),NA)
          
          
          probm_all <- probtrans(modHaz,variance=variance, predt = 0, direction = "forward")
          
          for (j in 1:(nstates+1)) {
            
            for (r in 1:(nstates)) {
              
              for (i in 1:length(timevar)) {
                
                los.mstate <- ELOS(probm_all,tau=timevar[i])
                
                los_array[i,j,r]=rbind(t(los.mstate),timevar=timevar[i]  ) [j,r]
                
              }
            }
          }  
          
          
          
          for (k in states) {
            
            loslist[[k]]=los_array[,,k]
            loslist[[k]]=as.data.frame(loslist[[k]])
            
          }
          
          for (k in states) {
            for (j in states) {
              colnames(loslist[[k]])[j]=paste0("Los_",k,"_to_",j)
              
            }
            
            colnames(loslist[[k]])[nstates+1]  ="timevar"  
            
          }
          
          loslist  
          
        }
        
      }   
      
      if (process=="semiMarkov") {
        
        ### Prob semi markov
        set.seed(1234)
        tv <- unique(modHaz$Haz$time)
        probm=list()
        p=1
        for (start in states) {
          probm[[p]]=  mssample(Haz=modHaz$Haz[which(modHaz[[1]][,2]>=0),], trans=qmat,
                                history=list(state=start,time=0,tstate=NULL),
                                beta.state=NULL, clock=clock, output="state",
                                Mjson = 10, tvec = timevar, cens=NULL, do.trace=NULL)
          
          probm[[p]]$timevar=probm[[p]]$time
          probm[[p]]=probm[[p]][,-1]
          p=p+1   
        }
        
        
        for (k in states) {
          for (j in states) {
            colnames(probm[[k]])[j]=paste0("P_",k,"_to_",j)
          }
          colnames(probm[[k]])[nstates+1]  ="timevar"  
        } 
        probm
        #probm=probm[-1,]
        
        
        loslist=list()
        
        
        if (totlos==TRUE) {
          
          los_array= array(dim=c(length(timevar),nstates+1,nstates),NA)
          
          
          probm_all <- probtrans(modHaz,variance=variance, predt = 0, direction = "forward")
          
          for (j in 1:(nstates+1)) {
            
            for (r in 1:(nstates)) {
              
              for (i in 1:length(timevar)) {
                
                los.mstate <- ELOS(probm_all,tau=timevar[i])
                
                los_array[i,j,r]=rbind(t(los.mstate),timevar=timevar[i]  ) [j,r]
                
              }
            }
          }  
          
          
          
          for (k in states) {
            
            loslist[[k]]=los_array[,,k]
            loslist[[k]]=as.data.frame(loslist[[k]])
            
          }
          
          for (k in states) {
            for (j in states) {
              colnames(loslist[[k]])[j]=paste0("Los_",k,"_to_",j)
              
            }
            
            colnames(loslist[[k]])[nstates+1]  ="timevar"  
            
          }
          loslist  
          
        }
        
      }
      is.cumhaz=1
      
      
      pred[[g]]=            list(probs=probm,
                                 trans=transr, is.cumhaz=is.cumhaz,
                                 los=loslist, timevar=timevar)   
    }  
}
  
  rm(list=ls(pattern="^forMSTATE"))
  
  
  
  ########################################################################################################
  ######## Declare the list elements even if they stay empty ####
  
  pjson    =list()
  hjson=list()
  losjson=list()
  
  
  
  ########################################################################################################
  ####  Probabilities
  ########################################################################################################
  
  ##### Main estimates 
  if (process=="semiMarkov") {
    
    pjson=list()
    
    if (length(covariates_list)<=1) {
      p=1
      for (k in 1:nstates) {
        for (i in 1:nstates) {
          
          pjson[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$probs[[k]][,i])))
          assign(paste0("forMSTATE",names(pred[[1]]$probs[[k]])[i]),  pjson[[p]])
          p=p+1  
        }
      }
    }
    
    if (length(covariates_list)>1) {
      
      p=1
      for (k in 1:nstates) {
        for (i in 1:nstates) {
          
          tmplist=matrix(nrow =length(covariates_list) , ncol= length(as.vector(pred[[1]]$probs[[k]][,i])))
          tmplist[1,]=as.vector(pred[[1]]$probs[[k]][,i])
          
          for (j in 2:length(covariates_list)) {
            tmplist[j,]=as.vector(pred[[j]]$probs[[k]][,i])  
          }
          pjson[[p]]=tmplist
          
          assign(paste0("forMSTATE",names(pred[[1]]$probs[[k]])[i]),  pjson[[p]])
          
          p=p+1  
          
          
        }
      }
    }
    
    pvector=vector()
    pvector=ls(pattern = "forMSTATEP_._to_.$")  
    
    pvector=sub("forMSTATE","",ls(pattern = "^forMSTATEP_._to_.$") )
    
    pvector=stringi::stri_sort(pvector, numeric = TRUE)
    
    names(pjson)=pvector
    
    pjson
    
  }
  
  if (process=="Markov") { 
      
      pjson=list()
      
      if (length(covariates_list)<=1) {
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            pjson[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$probs[[k]][,i])))
            assign(paste0("forMSTATE",names(pred[[1]]$probs[[k]])[i]),pjson[[p]])
            p=p+1  
          }
        }
      }
      
      if (length(covariates_list)>1) {
        
        tmplist=matrix(nrow =length(covariates_list) , ncol= length(timevar)+1)
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            tmplist[1,]=as.vector(pred[[1]]$probs[[k]][,i])
            
            for (j in 2:length(covariates_list)) {
              tmplist[j,]=as.vector(pred[[j]]$probs[[k]][,i])  
            }
            pjson[[p]]=tmplist
            
            assign(paste0("forMSTATE",names(pred[[1]]$probs[[k]])[i]),  pjson[[p]])
            
            p=p+1  
          }
        }
      }

      pvector=vector()
      pvector=ls(pattern = "forMSTATEP_._to_.$")  
      
      pvector=sub("forMSTATE","",ls(pattern = "^forMSTATEP_._to_.$") )
      
      pvector=stringi::stri_sort(pvector, numeric = TRUE)
      
      names(pjson)=pvector
      

      
    
  }
  
  
  #################################################################################################################
  
  ### Transitions main estimates ###
  hjson=list()
  
  #  if (trans.specific == "No") {
  
  if (length(covariates_list)<=1) {
    
    for (k in 1:ntransitions) {
      
      if (process=="Markov") {
        
        tmplisth=matrix(nrow =1 , ncol= length(timevar))
      }
      else if (process=="semiMarkov") {
        
        tmplisth=matrix(nrow =1 , ncol= length(transr$timevar))
        
      }       
      
      tmplisth[1,]=as.vector(pred[[1]]$trans[,k])
      hjson[[1]]=tmplisth
      assign(paste0("forMSTATE",names(pred[[1]]$trans)[i]),  hjson[[1]])
    }
    
  }
  
  if (length(covariates_list)>1) {
    
    for (k in 1:ntransitions) {
      
      if (process=="Markov") {
        
        tmplisth=matrix(nrow =length(covariates_list) , ncol= length(timevar))
      }
      else if (process=="semiMarkov") {
        
        tmplisth=matrix(nrow =length(covariates_list) , ncol= length(transr$timevar))
        
      }
      
      tmplisth[1,]=as.vector(pred[[1]]$trans[,k])
      
      
      
      for (j in 2:length(covariates_list)) {
        
        tmplisth[j,]=as.vector(pred[[j]]$trans[,k])  
      }
      
      
      hjson[[k]]=tmplisth
      assign(paste0("forMSTATE",names(pred[[1]]$trans)[k]),  hjson[[k]])
    }
  }
  
  hvector=vector()
  hvector=ls(pattern = "forMSTATEHaz_._to_.$")  
  
  hvector=sub("forMSTATE","",ls(pattern = "forMSTATEHaz_._to_.$") )
  
  hvector=stringi::stri_sort(hvector, numeric = TRUE)
  
  names(hjson)=hvector
  

  ##############################################################################################################
  
  if (process=="Markov") { 
    
    
    if (totlos==TRUE) { 
      
      
      #   if (trans.specific == "No") {
      ### Los Main estimates #####
      
      losjson=list()
      
      if (length(covariates_list)<=1) {
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            losjson[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$los[[k]][,i])))
            assign(paste0("forMSTATE",names(pred[[1]]$los[[k]])[i]),  losjson[[p]])
            p=p+1  
          }
        }
      }
      
      if (length(covariates_list)>1) {
        
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            tmplistlos=matrix(nrow =length(covariates_list) , ncol= length(timevar))
            tmplistlos[1,]=as.vector(pred[[1]]$los[[k]][,i])
            
            for (j in 2:length(covariates_list)) {
              tmplistlos[j,]=as.vector(pred[[j]]$los[[k]][,i])  
            }
            losjson[[p]]=tmplistlos
            
            assign(paste0("forMSTATE",names(pred[[1]]$los[[k]])[i]),  losjson[[p]])
            
            p=p+1  
          }
        }
      }
      
      losvector=vector()
      losvector=ls(pattern = "forMSTATELos_._to_.$")  
      
      losvector=sub("forMSTATE","",ls(pattern = "^forMSTATELos_._to_.$") )
      
      losvector=stringi::stri_sort(losvector, numeric = TRUE)
      
      names(losjson)=losvector
      

    }
  }
  
  
  
  ##########################################################################################
  
  ### Timevar###
  
  timejson=list()
  
  timejson[[1]]=timevar
  
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
                  atlist=atlistjson, tmat=tmatlist, is.cumhaz=is.cumhaz,
                  pjson,
                  hjson,
                  losjson
  )
  final_unlist= unlist(final_list, recursive=FALSE)
  
  exportJson <- toJSON(final_unlist,force = TRUE, flatten=TRUE)
  
  write(exportJson, paste0(jsonpath ,"/", name) )
  
  rm(list=ls(pattern="^forMSTATE"))
  
  final_list
  
  
}

