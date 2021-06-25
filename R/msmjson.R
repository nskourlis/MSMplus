#' @title msmjson
#' @description  Function msmjson can receive models from msm package.
#' It then uses functions from msm package internally to estimate multi-state model 
#' measures such as transition probabilities, transition intensity rates, length of stay, probability of visiting each state
#' probability each state is next, sojoutn times and confidence intervals of the
#' estimations. Function flexjson then take these results and reshapes them so that they can
#' be fed to MSMplus properly as a json file.
#' @param msm.model A fitted multi-state model, as returned by msm.
#' @param vartime The time points to estimate the transition probabilities for, by default one unit.
#' @param mat.init A transition intensity matrix. Default: NULL
#' @param totlos Estimate the total length of stay in each state for the time points 
#' specified by vartime argument, Default: "FALSE"
#' @param visit Estimate the probability of visiting each state for the time points 
#' specified by vartime argument, Default: "FALSE"
#' @param sojourn Estimate the mean sojourn time in each state, Default: "FALSE"
#' @param pnext Estimate the probability that each state is next, Default: "FALSE"
#' @param efpt Estimate the expected first passage time by each state, Default: "FALSE"
#' @param envisits Estimate the number of visits in each state , Default: "FALSE"
#' @param ci.json  Estimate confidence intervals, Default: 'normal'
#' @param cl.json Specify confidence level, Default: 0.95
#' @param B.json Number of simulations from the normal asymptotic distribution used to calculate variances. 
#' Decrease for greater speed at the expense of accuracy, Default: 50
#' @param cores.json Number of cores to use for bootstrapping using parallel processing. See boot.msm for more details., Default: NULL
#' @param piecewise.times.json 	Times at which piecewise-constant intensities change. 
#' See pmatrix.piecewise.msm for how to specify this. This is only required for time-inhomogeneous models 
#' specified using explicit time-dependent covariates, and should not be used for models specified using "pci"., Default: NULL
#' @param piecewise.covariates.json Covariates on which the piecewise-constant intensities depend. See pmatrix.piecewise.msm for how to specify this., Default: NULL
#' @param num.integ.json Use numerical integration instead of analytic solution (see below).
#' @param covariates_list The covariate values to estimate for. This can either be a list of lists:, Default: NULL
#' @param jsonpath specify the path of the folder that the json file should be saved, Default: "~"
#' @param name Specify the name of the output json file, Default: 'predictions.json'
#' @return returns a list of objects: the time variable
#' the number of covariate patterns, the names of covariate patterns, the transition matrix,
#' the number of transitions, the transition probabilities, transition intensity rates, length of stay, probability of visiting each state
#' probability each state is next, sojoutn times and confidence intervals of the
#' estimations(if estimated).
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#' 
#'# Multi-state model analysis: Using msmjson function together with msm package
#' 
#' library(MSMplus)
#' library(msm)
#' 
#' options(scipen = 999,"digits"=10)
#' 
#' head(cav)
#' 
#' ### Renaming variable PTNUM to id
#' 
#' cav$id=cav$PTNUM
#' 
#' ### Defining the transition matrix
#' 
#' tmat=matrix(NA,nrow=4,ncol=4)
#' tmat[1,2]=1; tmat[1,4]=2; tmat[2,1]=3; tmat[2,3]=4
#' tmat[2,4]=5; tmat[3,2]=6; tmat[3,4]=7
#' 
#' ### We can now call function msboxes_R  
#' 
#' results3_days=MSMplus::msboxes_R(data=cav,id= cav$id, yb=c(0.3,0.5,0.6,0.75), msm=TRUE,
#'                               xb=c(0.5,0.2,0.7,0.3),boxwidth=0.1,boxheight=0.1,
#'                               tmat.= tmat, vartime=seq(0,10,by=1),scale=1,
#'                               jsonpath="C:/Users/niksko/Desktop/mstate3/datasets/json/msm/json_present_msm", name="msboxes_cav_R.json" ) 
#' 
#' 
#' ### Defining the transition matrix with initial values under an initial assumption
#' 
#' #0 for transitions not allowed, initial values for rest of transitions under a rationale ##
#' 
#' Q<- rbind(c(0,0.25,0,0.25),c(0.166,0,0.166,0.166),c(0,0.25,0,0.25),c(0,0,0,0))
#' 
#' 
#' ### Getting initial Q matrix in a default way- Feed the hand made matrix 
#' 
#' q.crude<- crudeinits.msm(state~years, id,data=cav, qmatrix=Q)
#' 
#' 
#' ### Apply the msm model
#' 
#' cavsex.msm<- msm(state~years, covariates=~sex, id,data=cav,qmatrix=q.crude, deathexact = 4, control=list(trace=1,REPORT=1)) 
#' summary(cavsex.msm)
#' 
#' 
#' ### Prediction for different covariate patterns (males and females)
#' 
#' results <- MSMplus::msmjson(msm.model=cavsex.msm, vartime=seq(1,3,1), mat.init=q.crude,
#'                          totlos=TRUE, visit=TRUE, sojourn=TRUE, pnext=TRUE, efpt=TRUE, envisits=TRUE,
#'                          ci.json="normal", cl.json=0.95, B.json=100,
#'                          cores.json=NULL,piecewise.times.json=NULL, piecewise.covariates.json=NULL,num.integ.json=FALSE,
#'                          covariates_list=list(list(sex = 1),list(sex = 0)), 
#'                          jsonpath="C:/Users/niksko/Desktop/mstate3/datasets/json/msm/json_present_msm",
#'                          name="predictions_cav_R.json" ) 
#' 
#' results$timevar[[1]][1:3]
#' results$Nats
#' results$atlist
#' results$tmat
#' 
#' 
#'  }
#' }
#' @seealso 
#'  \code{\link[stringi]{stri_sort}}
#' @rdname msmjson
#' @export 
#' @importFrom stringi stri_sort



msmjson <- function(msm.model, vartime=seq(1,1,1), mat.init, 
                    totlos=FALSE, visit=FALSE, sojourn=FALSE, pnext=FALSE, efpt=FALSE, envisits=FALSE,
                    ci.json="normal", cl.json=0.95, B.json=50,
                    cores.json=NULL,piecewise.times.json=NULL, piecewise.covariates.json=NULL,num.integ.json=FALSE,
                    covariates_list=list(), jsonpath="~",name="predictions_R.json"  )  {
  
  
  
  ###  Default for jsonpath is user's home directory ####
  options(scipen = 999,"digits"=10)
  ################################################################  
  nstates=ncol(mat.init)
  
  p=1
  
  trmat=matrix(nrow=nstates, ncol=nstates, NA)
  
  mat=mat.init
  mat[is.na(mat)] <- 0
  ntransitions=length(mat[which(mat>0)])
  nstates=ncol(mat)
  states=c(seq(1:nstates))
  

  
  trmat=matrix(nrow=nstates, ncol=nstates, NA)
  
  for (i in 1:nstates) {
    for (k in 1:nstates) {
      
      if  (mat[i,k]>0)  {  
        
        trmat[i,k]=as.integer(p)
        p=p+1
      } 
      else if (mat[i,k]<=0)  {  
        trmat[i,k]=NA
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
  
  
  
  
  state_kind=vector()
  for (i in 1:nstates) {
    if (length(which(mat[,i]==0))==nrow(mat)) {state_kind[i]="Starting"}
    else if (length(which(mat[i,]==0))==ncol(mat)) {state_kind[i]="Absorbing"}
    else {state_kind[i]="Intermediate"}
  }
  st_states=which(state_kind=="Starting")
  na_states=which(state_kind=="Intermediate")
  ab_states=which(state_kind=="Absorbing")
  
  intermediate_list=list()
  intermediate_list[[1]]=unique(tr_start_state)
  
  Ntransitions=length(mat[which(mat>0)])
  
  
  
  ################################################################### 
  
 # library("msm")
 # library("stringi")
 # library("RJSONIO")
  
  pred=list() 
  
  #if (length(vartime)==1) {vartime=t(vartime)} 
  
  ### First case, overall predictions
  if (length(covariates_list) == 0) {
  
    
    ################################################################################################################
    
    ### Create all the lists to be output even if they will remain vacant ###
    
    
    loslist=list()
    loslist_lci=list()
    loslist_uci=list()
    sojlist=list()
    sojlist_lci=list()
    sojlist_uci=list()
    nextlist=list()
    nextlist_lci=list()
    nextlist_uci=list()
    firstlist=list()
    firstlist_lci=list()
    firstlist_uci=list()
    numberlist=list()
    numberlist_lci=list()
    numberlist_uci=list()
    vlist=list()
    vlist_lci=list()
    vlist_uci=list()
    trans_lci=vector()
    trans_uci=vector()
    
    #################################################################################################################
    
    ###Probability    ###
    mat=mat.init
    mat[is.na(mat)] <- 0
    ntransitions=length(mat[which(mat>0)])
    nstates=ncol(mat)
    states=c(seq(1:nstates))
    
    ##################### Main estimates#####################################################################
    
    p_array= array(dim=c(length(vartime),nstates+1,nstates),NA)
    p_array_lci= array(dim=c(length(vartime),nstates+1,nstates),NA)
    p_array_uci= array(dim=c(length(vartime),nstates+1,nstates),NA)
    
    
    for (i in vartime) {
      p.msm <- pmatrix.msm(msm.model,t=vartime[i], t1=0,  ci=ci.json , cl=cl.json, B=B.json,
                           cores=cores.json )
      
      if (ci.json=="none" ) {
        p_array[i,,]=t(cbind(p.msm,vartime=vartime[i] ))
      }
      
      if (ci.json!="none" ) {
        p_array[i,,]=t(cbind(p.msm$estimates,vartime=vartime[i] ))
        p_array_lci[i,,]=t(cbind(p.msm$L,vartime=vartime[i] ))
        p_array_uci[i,,]=t(cbind(p.msm$U,vartime=vartime[i] ))
      }
      
      
      
    }
    
    plist=list()
    for (k in states) {
      plist[[k]]=p_array[,,k]
      plist[[k]]=as.data.frame(plist[[k]])
    }
    
    if (length(vartime)==1) {
      for (k in states) {
        plist[[k]]=as.data.frame(t(plist[[k]])) 
      }
    }
    
    for (k in states) {
      for (j in states) {
        colnames(plist[[k]])[j]=paste0("P_",k,"_to_",j)
        
      }
      
      colnames(plist[[k]])[nstates+1]  ="vartime"  
      
    } 
    
    plist
    
    
    plist_lci=list()
    plist_uci=list()
    
    
    if (ci.json!="none" ) {
      
      ##################### Probabilities lci #####################################################################
      
      
      for (k in states) {
        plist_lci[[k]]=p_array_lci[,,k]
        plist_lci[[k]]=as.data.frame(plist_lci[[k]])
      }
      
      if (length(vartime)==1) {
        for (k in states) {
          plist_lci[[k]]=as.data.frame(t(plist_lci[[k]])) 
        }
      }
      
      for (k in states) {
        for (j in states) {
          colnames(plist_lci[[k]])[j]=paste0("P_",k,"_to_",j,"_lci")
          
        }
        
        colnames(plist_lci[[k]])[nstates+1]  ="vartime"  
        
      } 
      
      plist_lci
      
      
      ##################### Probabilities uci #####################################################################
      
      plist_uci=list()
      for (k in states) {
        plist_uci[[k]]=p_array_uci[,,k]
        plist_uci[[k]]=as.data.frame(plist_uci[[k]])
      }
      
      if (length(vartime)==1) {
        for (k in states) {
          plist_uci[[k]]=as.data.frame(t(plist_uci[[k]])) 
        }
      }
      
      for (k in states) {
        for (j in states) {
          colnames(plist_uci[[k]])[j]=paste0("P_",k,"_to_",j,"_uci")
          
        }
        
        colnames(plist_uci[[k]])[nstates+1]  ="vartime"  
        
      } 
      
      plist_uci
      
    }
    
    #################################################################################################################
    
    ###Transitions values for the period we want (of course they are constant)###
    
    
    ##################### Main estimates#####################################################################
    
    qmsm=qmatrix.msm(msm.model,ci=ci.json , cl=cl.json,
                     B=B.json, cores=cores.json)
    
    if (ci.json!="none" ) { 
      trans=matrix(nrow=length(vartime), ncol=length(qmsm$estimates[which(qmsm$estimates>0)]), NA)
      for (i in 1:length(vartime)){
        trans[i,]= qmsm$estimates[which(qmsm$estimates>0)]
      }
      
      trans=as.data.frame(trans)
      for (i in 1:length(qmsm$estimates[which(qmsm$estimates>0)])   ){
        colnames(trans)[i]<-paste0("Haz_",tr_start_state[i],"_to_",tr_end_state[i])
      }
    }
    
    if (ci.json=="none" ) { 
      trans=matrix(nrow=length(vartime), ncol=length(qmsm[which(qmsm>0)]), NA)
      for (i in 1:length(vartime)){
        trans[i,]= qmsm[which(qmsm>0)]
      }
      
      trans=as.data.frame(trans)
      for (i in 1:length(qmsm[which(qmsm>0)])   ){
        colnames(trans)[i]<-paste0("Haz_",tr_start_state[i],"_to_",tr_end_state[i])
      }
    }
    
    trans$vartime=vartime
    trans
    
    ##################### transitions lci and uci #####################################################################
    
    if (ci.json!="none" ) {
      
      trans_lci=matrix(nrow=length(vartime), ncol=length(qmsm$estimates[which(qmsm$L>0)]), NA)
      
      for (i in 1:length(vartime)){
        trans_lci[i,]= qmsm$L[which(qmsm$L>0)]
      }
      
      trans_lci=as.data.frame(trans_lci)
      for (i in 1:length(qmsm$L[which(qmsm$L>0)])   ){
        colnames(trans_lci)[i]<- paste0("Haz_",tr_start_state[i],"_to_",tr_end_state[i],"_lci")  
      }
      
      trans_lci$vartime=vartime
      trans_lci
      
      
      trans_uci=matrix(nrow=length(vartime), ncol=length(qmsm$estimates[which(qmsm$L>0)]), NA)
      
      for (i in 1:length(vartime)){
        trans_uci[i,]= qmsm$L[which(qmsm$L>0)]
      }
      
      trans_uci=as.data.frame(trans_uci)
      for (i in 1:length(qmsm$L[which(qmsm$L>0)])   ){
        colnames(trans_uci)[i]<-paste0("Haz_",tr_start_state[i],"_to_",tr_end_state[i],"_uci")  
      }
      
      trans_uci$vartime=vartime
      trans_uci
      
    }
    
    
    
    #################################################################################################################
    
    ###Los    ###
    if (totlos==TRUE) {
      
      los_array= array(dim=c(length(vartime),nstates+1,nstates),NA)
      los_array_lci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      los_array_uci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      
      
      for (j in states) {  
        for (i in vartime) {
          los.msm <- totlos.msm(msm.model, start=j, fromt=0, tot= vartime[i],ci=ci.json,cl=cl.json,
                                piecewise.times=piecewise.times.json, piecewise.covariates=piecewise.covariates.json,
                                num.integ=num.integ.json,  B=B.json,cores=cores.json)
          
          if (ci.json=="none" ) { los_array[i,,j]=cbind(t(los.msm),vartime=vartime[i]  ) }
          
          if (ci.json!="none" ) {
            los_array[i,,j]=cbind(t(los.msm[1,]),vartime=vartime[i]  ) 
            los_array_lci[i,,j]=cbind(t(round(los.msm[2,],5)),vartime=vartime[i]  )  
            los_array_uci[i,,j]=cbind(t(round(los.msm[3,],5)),vartime=vartime[i]  ) 
          }
          
        }
      }
      
      loslist=list()
      
      for (k in states) {
        
        loslist[[k]]=los_array[,,k]
        loslist[[k]]=as.data.frame(loslist[[k]])
        
      }
      
      if (length(vartime)==1) {
        for (k in states) {
          loslist[[k]]=as.data.frame(t(loslist[[k]])) 
        }
      }
      
      for (k in states) {
        for (j in states) {
          colnames(loslist[[k]])[j]=paste0("Los_",k,"_to_",j)
          
        }
        
        colnames(loslist[[k]])[nstates+1]  ="vartime"  
        
      }
      loslist
      ##################### los lci and uci #####################################################################
      
      loslist_lci=list()
      loslist_uci=list()
      
      if (ci.json!="none" ) {
        
        ###lci
        
        
        for (k in states) {
          
          loslist_lci[[k]]=los_array_lci[,,k]
          loslist_lci[[k]]=as.data.frame(loslist_lci[[k]])
          
        }
        
        if (length(vartime)==1) {
          for (k in states) {
            loslist_lci[[k]]=as.data.frame(t(loslist_lci[[k]])) 
          }
        }
        
        for (k in states) {
          for (j in states) {
            colnames(loslist_lci[[k]])[j]=paste0("Los_",k,"_to_",j,"_lci")
            
          }
          
          colnames(loslist_lci[[k]])[nstates+1]  ="vartime"  
          
        }
        loslist_lci
        
        ###uci
        
        for (k in states) {
          
          loslist_uci[[k]]=los_array_uci[,,k]
          loslist_uci[[k]]=as.data.frame(loslist_uci[[k]])
          
        }
        
        if (length(vartime)==1) {
          for (k in states) {
            loslist_uci[[k]]=as.data.frame(t(loslist_uci[[k]])) 
          }
        }
        
        for (k in states) {
          for (j in states) {
            colnames(loslist_uci[[k]])[j]=paste0("Los_",k,"_to_",j,"_uci")
            
          }
          
          colnames(loslist_uci[[k]])[nstates+1]  ="vartime"  
          
        }
        loslist_uci
        
        
      }
      
    }
    #################################################################################################################
    
    ###Sojourn    ###
    
    
    
    if   (sojourn==TRUE) {
      
      sojlist_=list()
      sojlist_lci=list()
      sojlist_uci=list()
      
      soj.msm=vector()
      
      if (ci.json!="none") {
        soj.msm=sojourn.msm(msm.model,ci=ci.json,cl=cl.json, B=B.json)
        sojnames= rownames(soj.msm)
        sojlist=list()
        sojlist[[1]]=cbind.data.frame(t( soj.msm$estimates))
        names(sojlist[[1]])[1:(ncol(sojlist[[1]]))]<-paste0("Soj_",sub("State ","",sojnames))
        sojlist
        
        
        
        sojlist_lci[[1]]=cbind.data.frame(t(soj.msm$L))
        
        sojlist_uci[[1]]=cbind.data.frame(t(soj.msm$U))
        
        names(sojlist_lci[[1]])[1:(ncol(sojlist_lci[[1]]))]<-paste0("Soj_",sub("State ","",sojnames),"_lci")
        names(sojlist_uci[[1]])[1:(ncol(sojlist_uci[[1]]))]<-paste0("Soj_",sub("State ","",sojnames),"_uci")
      }
      
      else if (ci.json =="none") {
        soj.msm=sojourn.msm(msm.model,ci=ci.json,cl=cl.json, B=B.json)
        sojlist=list()
        sojlist[[1]]=cbind.data.frame(t( soj.msm$estimates))
        sojnames=c(1:ncol(sojlist[[1]]))
        names(sojlist[[1]])[1:(ncol(sojlist[[1]]))]<-paste0("Soj_",sub("State ","",sojnames))
        sojlist
        
      }
      
      
    }  
    
    
    #################################################################################################################
    
    ###Next    ###
    
    ########################### Main estimates ####################################################
    
    if   (pnext==TRUE) {
      
      next_array= array(dim=c(1,nstates,nstates),NA)
      
      next.msm <-  pnext.msm(msm.model,ci=ci.json,cl=cl.json, B=B.json,cores=cores.json)
      
      next_array[1,,]=t(cbind.data.frame(next.msm$estimates  ))
      
      nextlist=list()
      for (k in states) {
        nextlist[[k]]=next_array[,,k]
        nextlist[[k]]=as.data.frame(t(nextlist[[k]]))
      }
      
#      if (length(vartime)==1) {
#        for (k in states) {
#          nextlist[[k]]=as.data.frame(t(nextlist[[k]])) 
#        }
#      }
      
      for (k in states) {
        for (j in states) {
          colnames(nextlist[[k]])[j]=paste0("Next_",k,"_to_",j)
        }
      } 
      
      nextlist
      
      ########################### next uci , lci  ####################################################
      
      nextlist_lci=list()
      nextlist_uci=list()
      
      if (ci.json!="none" ) {
        
        ###lci
        next_array_lci= array(dim=c(1,nstates,nstates),NA)
        
        next_array_lci[1,,]=t(cbind.data.frame(next.msm$L  ))
        
        
        for (k in states) {
          nextlist_lci[[k]]=next_array_lci[,,k]
          nextlist_lci[[k]]=as.data.frame(t(nextlist_lci[[k]]))
        }
        
 #       if (length(vartime)==1) {
 #         for (k in states) {
 #           nextlist_lci[[k]]=as.data.frame(t(nextlist_lci[[k]])) 
 #         }
 #       }
        
        for (k in states) {
          for (j in states) {
            colnames(nextlist_lci[[k]])[j]=paste0("Next_",k,"_to_",j,"_lci")
          }
        } 
        
        nextlist_lci 
        
        ### uci
        next_array_uci= array(dim=c(1,nstates,nstates),NA)
        
        next_array_uci[1,,]=t(cbind.data.frame(next.msm$U  ))
        
        
        for (k in states) {
          nextlist_uci[[k]]=next_array_uci[,,k]
          nextlist_uci[[k]]=as.data.frame(t(nextlist_uci[[k]]))
        }
        
  #      if (length(vartime)==1) {
  #        for (k in states) {
  #          nextlist_uci[[k]]=as.data.frame(t(nextlist_uci[[k]])) 
  #        }
  #      }
  #      
        for (k in states) {
          for (j in states) {
            colnames(nextlist_uci[[k]])[j]=paste0("Next_",k,"_to_",j,"_uci")
          }
        } 
        
        nextlist_uci 
      }
      
    } 
    ###############################################################################
    ##First
    
    if  (efpt==TRUE) {
      
      first.msm=vector()
      first.msm=  efpt.msm(msm.model, start="all", tostate=states, 
                           ci=ci.json,cl=cl.json, B=B.json,cores=cores.json)
      
      firstlist=list()
      
      if (ci.json=="none" ) {
        firstlist[[1]]=cbind.data.frame(t( first.msm)) 
      }
      
      if (ci.json!="none" ) {
        firstlist[[1]]=cbind.data.frame(t( first.msm[1,])) 
      }
      
      for (i in 1:nstates) {
        names( firstlist[[1]])[i]<-paste0("First_",i) 
      }
      
      firstlist_lci=list()
      firstlist_uci=list()
      
      if (ci.json!="none" ) {
        
        firstlist_lci[[1]]=cbind.data.frame(t(first.msm[2,]))
        
        firstlist_uci[[1]]=cbind.data.frame(t(first.msm[3,]))
        
        for (i in 1:nstates) {
          names( firstlist_lci[[1]])[i]<-paste0("First_",i,"_lci") 
          names( firstlist_uci[[1]])[i]<-paste0("First_",i,"_uci") 
        }
      }
      
    }
    
    ##########################################################################################
    
    ##Number
    
    if (envisits==TRUE) {
      
      ############Main estimates ########################
      
      number_array= array(dim=c(length(vartime),nstates+1,nstates),NA)
      number_array_lci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      number_array_uci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      
      
      for (j in states) {  
        for (i in vartime) {
          number.msm <- envisits.msm(msm.model, start=j, fromt=0, tot= vartime[i],
                                     piecewise.times=piecewise.times.json, piecewise.covariates=piecewise.covariates.json,
                                     num.integ=num.integ.json, 
                                     ci=ci.json, cl=cl.json, B=B.json,
                                     cores=cores.json)
          if (ci.json=="none" ) {  number_array[i,,j]=cbind(t(number.msm),vartime=vartime[i]  ) }
          
          if (ci.json!="none" ) {
            
            number_array[i,,j]=cbind(t(round(number.msm[1,],5)),vartime=vartime[i]  ) 
            number_array_lci[i,,j]=cbind(t(round(number.msm[2,],5)),vartime=vartime[i]  ) 
            number_array_uci[i,,j]=cbind(t(round(number.msm[3,],5)),vartime=vartime[i]  ) 
          }
        }
      }
      numberlist=list()
      
      for (k in states) {
        
        numberlist[[k]]=number_array[,,k]
        numberlist[[k]]=as.data.frame(numberlist[[k]])
        
      }
      
      if (length(vartime)==1) {
        for (k in states) {
          numberlist[[k]]=as.data.frame(t(numberlist[[k]])) 
        }
      }
      
      for (k in states) {
        for (j in states) {
          colnames(numberlist[[k]])[j]=paste0("Number_",k,"_to_",j)
          
        }
        
        colnames(numberlist[[k]])[nstates+1]  ="vartime"  
      }
      numberlist
      
      numberlist_lci=list()
      numberlist_uci=list()
      
      if (ci.json!="none" ) {
        
        ############ lci estimates ########################
        
        for (k in states) {
          
          numberlist_lci[[k]]=number_array_lci[,,k]
          numberlist_lci[[k]]=as.data.frame(numberlist_lci[[k]])
          
        }
        
        if (length(vartime)==1) {
          for (k in states) {
            numberlist_lci[[k]]=as.data.frame(t(numberlist_lci[[k]])) 
          }
        }
        
        for (k in states) {
          for (j in states) {
            colnames(numberlist_lci[[k]])[j]=paste0("Number_",k,"_to_",j,"_lci")
            
          }
          
          colnames(numberlist_lci[[k]])[nstates+1]  ="vartime"  
        }
        numberlist_lci
        
        ############ uci estimates ########################
        
        for (k in states) {
          
          numberlist_uci[[k]]=number_array_uci[,,k]
          numberlist_uci[[k]]=as.data.frame(numberlist_uci[[k]])
          
        }
        
        if (length(vartime)==1) {
          for (k in states) {
            numberlist_uci[[k]]=as.data.frame(t(numberlist_uci[[k]])) 
          }
        }
        
        for (k in states) {
          for (j in states) {
            colnames(numberlist_uci[[k]])[j]=paste0("Number_",k,"_to_",j,"_uci")
            
          }
          
          colnames(numberlist_uci[[k]])[nstates+1]  ="vartime"  
        }
        numberlist_uci
        
      }
      
    }
    
    
    ###########################################################################################################
    ##Visit  
    
    if (visit==TRUE) {
      
      visit.msm=vector()
      
      ######## What should we put in start? 1 or all states
      
      
      visit_array= array(dim=c(length(vartime),nstates+1,nstates),NA)
      visit_array_lci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      visit_array_uci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      
      
      for (i in vartime) {
        visit.msm <- ppass.msm(x=msm.model, qmatrix=NULL, tot=vartime[i], start="all",
                               piecewise.times=piecewise.times.json, piecewise.covariates=piecewise.covariates.json,
                               ci=ci.json, cl=cl.json, B=B.json,
                               cores=cores.json)
        
        if (ci.json=="none" ) {visit_array[i,,]=t(cbind(visit.msm,vartime=vartime[i]  )) }
        
        if (ci.json!="none" ) {
          visit_array[i,,]=t(cbind(visit.msm$estimates,vartime=vartime[i]  )) 
          visit_array_lci[i,,]=t(cbind(visit.msm$L,vartime=vartime[i]  )) 
          visit_array_uci[i,,]=t(cbind(visit.msm$U,vartime=vartime[i]  )) 
        }
      }
      
      ####################### Main estimates ########################
      vlist=list()
      for (k in states) {
        vlist[[k]]=visit_array[,,k]
        vlist[[k]]=as.data.frame(vlist[[k]])
      }
      
      
      
      if (length(vartime)==1) {
        for (k in states) {
          vlist[[k]]=as.data.frame(t(vlist[[k]])) 
        }
      }
      
      for (k in states) {
        for (j in states) {
          colnames(vlist[[k]])[j]=paste0("Visit_",k,"_to_",j)
          
        }
        
        colnames(vlist[[k]])[nstates+1]  ="vartime"  
      } 
      
      vlist
      
      vlist_lci=list()
      vlist_uci=list()
      
      if (ci.json!="none" ) {
        ####################### Lci estimates ########################
        for (k in states) {
          vlist_lci[[k]]=visit_array_lci[,,k]
          vlist_lci[[k]]=as.data.frame(vlist_lci[[k]])
        }
        
        if (length(vartime)==1) {
          for (k in states) {
            vlist_lci[[k]]=as.data.frame(t(vlist_lci[[k]])) 
          }
        }
        
        for (k in states) {
          for (j in states) {
            colnames(vlist_lci[[k]])[j]=paste0("Visit_",k,"_to_",j,"_lci")
            
          }
          
          colnames(vlist_lci[[k]])[nstates+1]  ="vartime"  
        } 
        vlist_lci
        ####################### Uci estimates ########################
        for (k in states) {
          vlist_uci[[k]]=visit_array_uci[,,k]
          vlist_uci[[k]]=as.data.frame(vlist_uci[[k]])
        }
        
        if (length(vartime)==1) {
          for (k in states) {
            vlist_uci[[k]]=as.data.frame(t(vlist_uci[[k]])) 
          }
        }
        
        for (k in states) {
          for (j in states) {
            colnames(vlist_uci[[k]])[j]=paste0("Visit_",k,"_to_",j,"_uci")
            
          }
          
          colnames(vlist_uci[[k]])[nstates+1]  ="vartime"  
        } 
        vlist_uci
      }
      
    }
    
    
    ###########################################################################################   
    
    #  ###Ratio of tr. intensities
    #  mat
    #  
    #   p=1
    #   qratio=matrix(nrow=1, ncol=length(which(mat>0))^2,NA)
    #   names.qratio=vector()
    #   
    #  for (i in 1:nstates) {
    #    for (j in 1:nstates) {
    #      for (k in 1:nstates) {
    #        for (f in 1:nstates) {
    #           
    #          if (mat[k,f]==0 | mat[i,j]==0 | k==f | i==j) {next}
    #           else if (mat[k,f]!=0 & mat[i,j]!=0 &  k!=f & i!=j ) {
    #  qratio[1,p]=qratio.msm(msm.model, ind1=c(i,j), ind2=c(k,f), covariates = covariates_list[[g]])[1]
    #  names.qratio[p]=paste0("Qratio_t1_",i,".",j,"_t2_",k,".",f)
    #              p=p+1
    #           }
    #        }
    #      }
    #    }
    #  }
    #  
    #   qratio=as.data.frame(qratio)
    #   names(qratio)=names.qratio
    #   
    #   qratiolist=list()
    #   qratiolist[[1]]=cbind.data.frame(qratio,cov_factor=rep(paste0(names(covariates_list[[g]]),covariates_list[[g]]),1))
    
    ##############################################################################################
    
    
    pred[[1]]=  list(probs=plist,probs_lci=plist_lci,probs_uci=plist_uci,
                     trans=trans, trans_lci=trans_lci,trans_uci=trans_uci,
                     los=loslist,los_lci=loslist_lci,los_uci=loslist_uci,
                     soj=sojlist,soj_lci=sojlist_lci,soj_uci=sojlist_uci,
                     nextv=nextlist,nextv_lci=nextlist_lci,nextv_uci=nextlist_uci,
                     first=firstlist,first_lci=firstlist_lci,first_uci=firstlist_uci,
                     number=numberlist,number_lci=numberlist_lci,number_uci=numberlist_uci,
                     visit=vlist,visit_lci=vlist_lci, visit_uci=vlist_uci
    )
    
  }
    
if (length(covariates_list) != 0) {
  
  for (g in 1:length(covariates_list)) {
    
 
    #g=1
    ################################################################################################################
    
    ### Create all the lists to be output even if they will remain vacant ###
    
    
    loslist=list()
    loslist_lci=list()
    loslist_uci=list()
    sojlist=list()
    sojlist_lci=list()
    sojlist_uci=list()
    nextlist=list()
    nextlist_lci=list()
    nextlist_uci=list()
    firstlist=list()
    firstlist_lci=list()
    firstlist_uci=list()
    numberlist=list()
    numberlist_lci=list()
    numberlist_uci=list()
    vlist=list()
    vlist_lci=list()
    vlist_uci=list()
    trans_lci=vector()
    trans_uci=vector()
    
    #################################################################################################################
    
    ###Probability    ###
    mat=mat.init
    mat[is.na(mat)] <- 0
    ntransitions=length(mat[which(mat>0)])
    nstates=ncol(mat)
    states=c(seq(1:nstates))
    
    ##################### Main estimates#####################################################################
    
    p_array= array(dim=c(length(vartime),nstates+1,nstates),NA)
    p_array_lci= array(dim=c(length(vartime),nstates+1,nstates),NA)
    p_array_uci= array(dim=c(length(vartime),nstates+1,nstates),NA)
    
    
    for (i in vartime) {
      p.msm <- pmatrix.msm(msm.model,t=vartime[i], t1=0,  ci=ci.json , cl=cl.json, B=B.json,
                           cores=cores.json ,covariates= covariates_list[[g]])
      
      if (ci.json=="none" ) {
        p_array[i,,]=t(cbind(p.msm,vartime=vartime[i] ))
      }
      
      if (ci.json!="none" ) {
        p_array[i,,]=t(cbind(p.msm$estimates,vartime=vartime[i] ))
        p_array_lci[i,,]=t(cbind(p.msm$L,vartime=vartime[i] ))
        p_array_uci[i,,]=t(cbind(p.msm$U,vartime=vartime[i] ))
      }
      
      
      
    }
    
    plist=list()
    for (k in states) {
      plist[[k]]=p_array[,,k]
      plist[[k]]=as.data.frame(plist[[k]])
    }
    
    if (length(vartime)==1) {
      for (k in states) {
      plist[[k]]=as.data.frame(t(plist[[k]])) 
      }
    }
    
    
    for (k in states) {
      for (j in states) {
        colnames(plist[[k]])[j]=paste0("P_",k,"_to_",j)
        
      }
      
      colnames(plist[[k]])[nstates+1]  ="vartime"  
      
    } 
    
    plist
    
    
    plist_lci=list()
    plist_uci=list()
    
    
    if (ci.json!="none" ) {
      
      ##################### Probabilities lci #####################################################################
      
      
      for (k in states) {
        plist_lci[[k]]=p_array_lci[,,k]
        plist_lci[[k]]=as.data.frame(plist_lci[[k]])
      }
      
      if (length(vartime)==1) {
        for (k in states) {
          plist_lci[[k]]=as.data.frame(t(plist_lci[[k]])) 
        }
      }
      
      
      for (k in states) {
        for (j in states) {
          colnames(plist_lci[[k]])[j]=paste0("P_",k,"_to_",j,"_lci")
          
        }
        
        colnames(plist_lci[[k]])[nstates+1]  ="vartime"  
        
      } 
      
      plist_lci
      
      
      ##################### Probabilities uci #####################################################################
      
      plist_uci=list()
      for (k in states) {
        plist_uci[[k]]=p_array_uci[,,k]
        plist_uci[[k]]=as.data.frame(plist_uci[[k]])
      }
      
      if (length(vartime)==1) {
        for (k in states) {
          plist_uci[[k]]=as.data.frame(t(plist_uci[[k]])) 
        }
      }
      
      for (k in states) {
        for (j in states) {
          colnames(plist_uci[[k]])[j]=paste0("P_",k,"_to_",j,"_uci")
          
        }
        
        colnames(plist_uci[[k]])[nstates+1]  ="vartime"  
        
      } 
      
      plist_uci
      
    }
    
    #################################################################################################################
    
    ###Transitions values for the period we want (of course they are constant)###
    
    
    ##################### Main estimates#####################################################################
    
    qmsm=qmatrix.msm(msm.model,covariates=covariates_list[[g]],ci=ci.json , cl=cl.json,
                     B=B.json, cores=cores.json)
    
    if (ci.json!="none" ) { 
      trans=matrix(nrow=length(vartime), ncol=length(qmsm$estimates[which(qmsm$estimates>0)]), NA)
      for (i in 1:length(vartime)){
        trans[i,]= qmsm$estimates[which(qmsm$estimates>0)]
      }
      
      trans=as.data.frame(trans)
      for (i in 1:length(qmsm$estimates[which(qmsm$estimates>0)])   ){
        colnames(trans)[i]<-paste0("Haz_",tr_start_state[i],"_to_",tr_end_state[i])
      }
    }
    
    if (ci.json=="none" ) { 
      trans=matrix(nrow=length(vartime), ncol=length(qmsm[which(qmsm>0)]), NA)
      for (i in 1:length(vartime)){
        trans[i,]= qmsm[which(qmsm>0)]
      }
      
      trans=as.data.frame(trans)
      for (i in 1:length(qmsm[which(qmsm>0)])   ){
        colnames(trans)[i]<-paste0("Haz_",tr_start_state[i],"_to_",tr_end_state[i])
      }
    }
    
    trans$vartime=vartime
    trans
    
    ##################### transitions lci and uci #####################################################################
    
    if (ci.json!="none" ) {
      
      trans_lci=matrix(nrow=length(vartime), ncol=length(qmsm$estimates[which(qmsm$L>0)]), NA)
      
      for (i in 1:length(vartime)){
        trans_lci[i,]= qmsm$L[which(qmsm$L>0)]
      }
      
      trans_lci=as.data.frame(trans_lci)
      for (i in 1:length(qmsm$L[which(qmsm$L>0)])   ){
        colnames(trans_lci)[i]<- paste0("Haz_",tr_start_state[i],"_to_",tr_end_state[i],"_lci")  
      }
      
      trans_lci$vartime=vartime
      trans_lci
      
      
      trans_uci=matrix(nrow=length(vartime), ncol=length(qmsm$estimates[which(qmsm$L>0)]), NA)
      
      for (i in 1:length(vartime)){
        trans_uci[i,]= qmsm$L[which(qmsm$L>0)]
      }
      
      trans_uci=as.data.frame(trans_uci)
      for (i in 1:length(qmsm$L[which(qmsm$L>0)])   ){
        colnames(trans_uci)[i]<-paste0("Haz_",tr_start_state[i],"_to_",tr_end_state[i],"_uci")  
      }
      
      trans_uci$vartime=vartime
      trans_uci
      
    }
    
    
    
    #################################################################################################################
    
    ###Los    ###
    if (totlos==TRUE) {
      
      los_array= array(dim=c(length(vartime),nstates+1,nstates),NA)
      los_array_lci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      los_array_uci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      
      
      for (j in states) {  
        for (i in vartime) {
          los.msm <- totlos.msm(msm.model, start=j, fromt=0, tot= vartime[i],covariates=covariates_list[[g]],ci=ci.json,cl=cl.json,
                                piecewise.times=piecewise.times.json, piecewise.covariates=piecewise.covariates.json,
                                num.integ=num.integ.json,  B=B.json,cores=cores.json)
          
          if (ci.json=="none" ) { los_array[i,,j]=cbind(t(los.msm),vartime=vartime[i]  ) }
          
          if (ci.json!="none" ) {
            los_array[i,,j]=cbind(t(los.msm[1,]),vartime=vartime[i]  ) 
            los_array_lci[i,,j]=cbind(t(round(los.msm[2,],5)),vartime=vartime[i]  )  
            los_array_uci[i,,j]=cbind(t(round(los.msm[3,],5)),vartime=vartime[i]  ) 
          }
          
        }
      }
      
      loslist=list()
      
      for (k in states) {
        
        loslist[[k]]=los_array[,,k]
        loslist[[k]]=as.data.frame(loslist[[k]])
        
      }
      
      if (length(vartime)==1) {
        for (k in states) {
          loslist[[k]]=as.data.frame(t(loslist[[k]])) 
        }
      }
      
      for (k in states) {
        for (j in states) {
          colnames(loslist[[k]])[j]=paste0("Los_",k,"_to_",j)
          
        }
        
        colnames(loslist[[k]])[nstates+1]  ="vartime"  
        
      }
      loslist
      ##################### los lci and uci #####################################################################
      
      loslist_lci=list()
      loslist_uci=list()
      
      if (ci.json!="none" ) {
        
        ###lci
        
        
        for (k in states) {
          
          loslist_lci[[k]]=los_array_lci[,,k]
          loslist_lci[[k]]=as.data.frame(loslist_lci[[k]])
          
        }
        
        if (length(vartime)==1) {
          for (k in states) {
            loslist_lci[[k]]=as.data.frame(t(loslist_lci[[k]])) 
          }
        }
        
        
        for (k in states) {
          for (j in states) {
            colnames(loslist_lci[[k]])[j]=paste0("Los_",k,"_to_",j,"_lci")
            
          }
          
          colnames(loslist_lci[[k]])[nstates+1]  ="vartime"  
          
        }
        loslist_lci
        
        ###uci
        
        for (k in states) {
          
          loslist_uci[[k]]=los_array_uci[,,k]
          loslist_uci[[k]]=as.data.frame(loslist_uci[[k]])
          
        }
        
        if (length(vartime)==1) {
          for (k in states) {
            loslist_uci[[k]]=as.data.frame(t(loslist_uci[[k]])) 
          }
        }
        
        for (k in states) {
          for (j in states) {
            colnames(loslist_uci[[k]])[j]=paste0("Los_",k,"_to_",j,"_uci")
            
          }
          
          colnames(loslist_uci[[k]])[nstates+1]  ="vartime"  
          
        }
        loslist_uci
        
        
      }
      
    }
    #################################################################################################################
    
    ###Sojourn    ###
    
    
    
    if   (sojourn==TRUE) {
      
      sojlist_=list()
      sojlist_lci=list()
      sojlist_uci=list()
      
      soj.msm=vector()
      
      if (ci.json!="none") {
        soj.msm=sojourn.msm(msm.model,covariates=covariates_list[[g]],ci=ci.json,cl=cl.json, B=B.json)
        sojnames= rownames(soj.msm)
        sojlist=list()
        sojlist[[1]]=cbind.data.frame(t( soj.msm$estimates))
        names(sojlist[[1]])[1:(ncol(sojlist[[1]]))]<-paste0("Soj_",sub("State ","",sojnames))
        sojlist
        
        
        
        sojlist_lci[[1]]=cbind.data.frame(t(soj.msm$L))
        
        sojlist_uci[[1]]=cbind.data.frame(t(soj.msm$U))
        
        names(sojlist_lci[[1]])[1:(ncol(sojlist_lci[[1]]))]<-paste0("Soj_",sub("State ","",sojnames),"_lci")
        names(sojlist_uci[[1]])[1:(ncol(sojlist_uci[[1]]))]<-paste0("Soj_",sub("State ","",sojnames),"_uci")
      }
      
      else if (ci.json =="none") {
        soj.msm=sojourn.msm(msm.model,covariates=covariates_list[[g]],ci=ci.json,cl=cl.json, B=B.json)
        sojlist=list()
        sojlist[[1]]=cbind.data.frame(t( soj.msm$estimates))
        sojnames=c(1:ncol(sojlist[[1]]))
        names(sojlist[[1]])[1:(ncol(sojlist[[1]]))]<-paste0("Soj_",sub("State ","",sojnames))
        sojlist
        
      }
      
      
    }  
    
    
    #################################################################################################################
    
    ###Next    ###
    
    ########################### Main estimates ####################################################
    
    if   (pnext==TRUE) {
      
      next_array= array(dim=c(1,nstates,nstates),NA)
      
      next.msm <-  pnext.msm(msm.model,ci=ci.json,cl=cl.json, B=B.json,cores=cores.json, covariates=covariates_list[[g]])
      
      next_array[1,,]=t(cbind.data.frame(next.msm$estimates  ))
      
      nextlist=list()
      for (k in states) {
        nextlist[[k]]=next_array[,,k]
        nextlist[[k]]=as.data.frame(t(nextlist[[k]]))
      }
      
#     if (length(vartime)==1) {
#       for (k in states) {
#         nextlist[[k]]=as.data.frame(t(nextlist[[k]])) 
#       }
#     }
      
      for (k in states) {
        for (j in states) {
          colnames(nextlist[[k]])[j]=paste0("Next_",k,"_to_",j)
        }
      } 
      
      nextlist
      
      ########################### next uci , lci  ####################################################
      
      nextlist_lci=list()
      nextlist_uci=list()
      
      if (ci.json!="none" ) {
        
        ###lci
        next_array_lci= array(dim=c(1,nstates,nstates),NA)
        
        next_array_lci[1,,]=t(cbind.data.frame(next.msm$L  ))
        
        
        for (k in states) {
          nextlist_lci[[k]]=next_array_lci[,,k]
          nextlist_lci[[k]]=as.data.frame(t(nextlist_lci[[k]]))
        }
        
 #       if (length(vartime)==1) {
 #         for (k in states) {
 #           nextlist_lci[[k]]=as.data.frame(t(nextlist_lci[[k]])) 
 #         }
 #       }
        
        for (k in states) {
          for (j in states) {
            colnames(nextlist_lci[[k]])[j]=paste0("Next_",k,"_to_",j,"_lci")
          }
        } 
        
        nextlist_lci 
        
        ### uci
        next_array_uci= array(dim=c(1,nstates,nstates),NA)
        
        next_array_uci[1,,]=t(cbind.data.frame(next.msm$U  ))
        
        
        for (k in states) {
          nextlist_uci[[k]]=next_array_uci[,,k]
          nextlist_uci[[k]]=as.data.frame(t(nextlist_uci[[k]]))
        }
        
 #      if (length(vartime)==1) {
 #        for (k in states) {
 #          nextlist_uci[[k]]=as.data.frame(t(nextlist_uci[[k]])) 
 #        }
 #      }
        
        for (k in states) {
          for (j in states) {
            colnames(nextlist_uci[[k]])[j]=paste0("Next_",k,"_to_",j,"_uci")
          }
        } 
        
        nextlist_uci 
      }
      
    } 
    ###############################################################################
    ##First
    
    if  (efpt==TRUE) {
      
      first.msm=vector()
      first.msm=  efpt.msm(msm.model, start="all", tostate=states, 
                           ci=ci.json,cl=cl.json, B=B.json,cores=cores.json, covariates = covariates_list[[g]])
      
      firstlist=list()
      
      if (ci.json=="none" ) {
        firstlist[[1]]=cbind.data.frame(t( first.msm)) 
      }
      
      if (ci.json!="none" ) {
        firstlist[[1]]=cbind.data.frame(t( first.msm[1,])) 
      }
      
      for (i in 1:nstates) {
        names( firstlist[[1]])[i]<-paste0("First_",i) 
      }
      
      firstlist_lci=list()
      firstlist_uci=list()
      
      if (ci.json!="none" ) {
        
        firstlist_lci[[1]]=cbind.data.frame(t(first.msm[2,]))
        
        firstlist_uci[[1]]=cbind.data.frame(t(first.msm[3,]))
        
        for (i in 1:nstates) {
          names( firstlist_lci[[1]])[i]<-paste0("First_",i,"_lci") 
          names( firstlist_uci[[1]])[i]<-paste0("First_",i,"_uci") 
        }
      }
      
    }
    
    ##########################################################################################
    
    ##Number
    
    if (envisits==TRUE) {
      
      ############Main estimates ########################
      
      number_array= array(dim=c(length(vartime),nstates+1,nstates),NA)
      number_array_lci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      number_array_uci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      
      
      for (j in states) {  
        for (i in vartime) {
          number.msm <- envisits.msm(msm.model, start=j, fromt=0, tot= vartime[i],covariates=covariates_list[[g]],
                                     piecewise.times=piecewise.times.json, piecewise.covariates=piecewise.covariates.json,
                                     num.integ=num.integ.json, 
                                     ci=ci.json, cl=cl.json, B=B.json,
                                     cores=cores.json)
          if (ci.json=="none" ) {  number_array[i,,j]=cbind(t(number.msm),vartime=vartime[i]  ) }
          
          if (ci.json!="none" ) {
            
            number_array[i,,j]=cbind(t(round(number.msm[1,],5)),vartime=vartime[i]  ) 
            number_array_lci[i,,j]=cbind(t(round(number.msm[2,],5)),vartime=vartime[i]  ) 
            number_array_uci[i,,j]=cbind(t(round(number.msm[3,],5)),vartime=vartime[i]  ) 
          }
        }
      }
      numberlist=list()
      
      for (k in states) {
        
        numberlist[[k]]=number_array[,,k]
        numberlist[[k]]=as.data.frame(numberlist[[k]])
        
      }
      
      if (length(vartime)==1) {
        for (k in states) {
          numberlist[[k]]=as.data.frame(t(numberlist[[k]])) 
        }
      }
      
      for (k in states) {
        for (j in states) {
          colnames(numberlist[[k]])[j]=paste0("Number_",k,"_to_",j)
          
        }
        
        colnames(numberlist[[k]])[nstates+1]  ="vartime"  
      }
      numberlist
      
      numberlist_lci=list()
      numberlist_uci=list()
      
      if (ci.json!="none" ) {
        
        ############ lci estimates ########################
        
        for (k in states) {
          
          numberlist_lci[[k]]=number_array_lci[,,k]
          numberlist_lci[[k]]=as.data.frame(numberlist_lci[[k]])
          
        }
        
        
        if (length(vartime)==1) {
          for (k in states) {
            numberlist_lci[[k]]=as.data.frame(t(numberlist_lci[[k]])) 
          }
        }
        
        for (k in states) {
          for (j in states) {
            colnames(numberlist_lci[[k]])[j]=paste0("Number_",k,"_to_",j,"_lci")
            
          }
          
          colnames(numberlist_lci[[k]])[nstates+1]  ="vartime"  
        }
        numberlist_lci
        
        ############ uci estimates ########################
        
        for (k in states) {
          
          numberlist_uci[[k]]=number_array_uci[,,k]
          numberlist_uci[[k]]=as.data.frame(numberlist_uci[[k]])
          
        }
        
        if (length(vartime)==1) {
          for (k in states) {
            numberlist_uci[[k]]=as.data.frame(t(numberlist_uci[[k]])) 
          }
        }
        
        for (k in states) {
          for (j in states) {
            colnames(numberlist_uci[[k]])[j]=paste0("Number_",k,"_to_",j,"_uci")
            
          }
          
          colnames(numberlist_uci[[k]])[nstates+1]  ="vartime"  
        }
        numberlist_uci
        
      }
      
    }
    
    
    ###########################################################################################################
    ##Visit  
    
    if (visit==TRUE) {
      
      visit.msm=vector()
      
      ######## What should we put in start? 1 or all states
      
      
      visit_array= array(dim=c(length(vartime),nstates+1,nstates),NA)
      visit_array_lci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      visit_array_uci= array(dim=c(length(vartime),nstates+1,nstates),NA)
      
      
      for (i in vartime) {
        visit.msm <- ppass.msm(x=msm.model, qmatrix=NULL, tot=vartime[i], start="all", covariates = covariates_list[[g]],
                               piecewise.times=piecewise.times.json, piecewise.covariates=piecewise.covariates.json,
                               ci=ci.json, cl=cl.json, B=B.json,
                               cores=cores.json)
        
        if (ci.json=="none" ) {visit_array[i,,]=t(cbind(visit.msm,vartime=vartime[i]  )) }
        
        if (ci.json!="none" ) {
          visit_array[i,,]=t(cbind(visit.msm$estimates,vartime=vartime[i]  )) 
          visit_array_lci[i,,]=t(cbind(visit.msm$L,vartime=vartime[i]  )) 
          visit_array_uci[i,,]=t(cbind(visit.msm$U,vartime=vartime[i]  )) 
        }
      }
      
      ####################### Main estimates ########################
      vlist=list()
      for (k in states) {
        vlist[[k]]=visit_array[,,k]
        vlist[[k]]=as.data.frame(vlist[[k]])
      }
      
      if (length(vartime)==1) {
        for (k in states) {
          vlist[[k]]=as.data.frame(t(vlist[[k]])) 
        }
      }
      
      for (k in states) {
        for (j in states) {
          colnames(vlist[[k]])[j]=paste0("Visit_",k,"_to_",j)
          
        }
        
        colnames(vlist[[k]])[nstates+1]  ="vartime"  
      } 
      
      vlist
      
      vlist_lci=list()
      vlist_uci=list()
      
      if (ci.json!="none" ) {
        ####################### Lci estimates ########################
        for (k in states) {
          vlist_lci[[k]]=visit_array_lci[,,k]
          vlist_lci[[k]]=as.data.frame(vlist_lci[[k]])
        }
        
        if (length(vartime)==1) {
          for (k in states) {
            vlist_lci[[k]]=as.data.frame(t(vlist_lci[[k]])) 
          }
        }
        
        for (k in states) {
          for (j in states) {
            colnames(vlist_lci[[k]])[j]=paste0("Visit_",k,"_to_",j,"_lci")
            
          }
          
          colnames(vlist_lci[[k]])[nstates+1]  ="vartime"  
        } 
        vlist_lci
        ####################### Uci estimates ########################
        for (k in states) {
          vlist_uci[[k]]=visit_array_uci[,,k]
          vlist_uci[[k]]=as.data.frame(vlist_uci[[k]])
        }
        
        if (length(vartime)==1) {
          for (k in states) {
            vlist_uci[[k]]=as.data.frame(t(vlist_uci[[k]])) 
          }
        }
        
        for (k in states) {
          for (j in states) {
            colnames(vlist_uci[[k]])[j]=paste0("Visit_",k,"_to_",j,"_uci")
            
          }
          
          colnames(vlist_uci[[k]])[nstates+1]  ="vartime"  
        } 
        vlist_uci
      }
      
    }
    
    
    ###########################################################################################   
    
    #  ###Ratio of tr. intensities
    #  mat
    #  
    #   p=1
    #   qratio=matrix(nrow=1, ncol=length(which(mat>0))^2,NA)
    #   names.qratio=vector()
    #   
    #  for (i in 1:nstates) {
    #    for (j in 1:nstates) {
    #      for (k in 1:nstates) {
    #        for (f in 1:nstates) {
    #           
    #          if (mat[k,f]==0 | mat[i,j]==0 | k==f | i==j) {next}
    #           else if (mat[k,f]!=0 & mat[i,j]!=0 &  k!=f & i!=j ) {
    #  qratio[1,p]=qratio.msm(msm.model, ind1=c(i,j), ind2=c(k,f), covariates = covariates_list[[g]])[1]
    #  names.qratio[p]=paste0("Qratio_t1_",i,".",j,"_t2_",k,".",f)
    #              p=p+1
    #           }
    #        }
    #      }
    #    }
    #  }
    #  
    #   qratio=as.data.frame(qratio)
    #   names(qratio)=names.qratio
    #   
    #   qratiolist=list()
    #   qratiolist[[1]]=cbind.data.frame(qratio,cov_factor=rep(paste0(names(covariates_list[[g]]),covariates_list[[g]]),1))
    
    ##############################################################################################
    
    
    pred[[g]]=  list(probs=plist,probs_lci=plist_lci,probs_uci=plist_uci,
                     trans=trans, trans_lci=trans_lci,trans_uci=trans_uci,
                     los=loslist,los_lci=loslist_lci,los_uci=loslist_uci,
                     soj=sojlist,soj_lci=sojlist_lci,soj_uci=sojlist_uci,
                     nextv=nextlist,nextv_lci=nextlist_lci,nextv_uci=nextlist_uci,
                     first=firstlist,first_lci=firstlist_lci,first_uci=firstlist_uci,
                     number=numberlist,number_lci=numberlist_lci,number_uci=numberlist_uci,
                     visit=vlist,visit_lci=vlist_lci, visit_uci=vlist_uci
    )
    
  }
  
  
}  
  
  
  
  ########################################################################################################
  ######## Declare the list elements even if they stay empty ####
  
  pjson_lci=list()
  pjson_uci=list()
  hjson_lci=list()
  hjson_uci=list()
  losjson=list()
  losjson_lci=list()
  losjson_uci=list()
  sojjson=list()
  sojjson_lci=list()
  sojjson_uci=list()
  nextjson=list()
  nextjson_lci=list()
  nextjson_uci=list()
  firstjson=list()
  firstjson_lci=list()
  firstjson_uci=list()
  numberjson=list()
  numberjson_lci=list()
  numberjson_uci=list()
  vjson=list()
  vjson_lci=list()
  vjson_uci=list()
  
  
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
        assign(paste0("forMSM",names(pred[[1]]$probs[[k]])[i]),  pjson[[p]])
        p=p+1  
      }
    }
  }
  
  if (length(covariates_list)>1) {
    
    p=1
    for (k in 1:nstates) {
      for (i in 1:nstates) {
        
        tmplist=matrix(nrow =length(covariates_list) , ncol= length(vartime))
        tmplist[1,]=as.vector(pred[[1]]$probs[[k]][,i])
        
        for (j in 2:length(covariates_list)) {
          tmplist[j,]=as.vector(pred[[j]]$probs[[k]][,i])  
        }
        pjson[[p]]=tmplist
        
        assign(paste0("forMSM",names(pred[[1]]$probs[[k]])[i]),  pjson[[p]])
        
        p=p+1  
        
        
      }
    }
  }
  
  pvector=vector()
  pvector=ls(pattern = "forMSMP_._to_.$")  
  
  pvector=sub("forMSM","",ls(pattern = "^forMSMP_._to_.$") )
  
  pvector=stringi::stri_sort(pvector, numeric = TRUE)
  
  names(pjson)=pvector[1:(p-1)]
  
  pjson
  
  ##### Lci possibilities estimates
  
  if (ci.json!="none" ) {
    
    
    pjson_lci=list()
    
    if (length(covariates_list)<=1) {
      p=1
      for (k in 1:nstates) {
        for (i in 1:nstates) {
          
          pjson_lci[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$probs_lci[[k]][,i])))
          assign(paste0("forMSM",names(pred[[1]]$probs_lci[[k]])[i]),  pjson_lci[[p]])
          p=p+1  
        }
      }
    }
    
    if (length(covariates_list)>1) {
      
      p=1
      for (k in 1:nstates) {
        for (i in 1:nstates) {
          
          tmplist=matrix(nrow =length(covariates_list) , ncol= length(vartime))
          tmplist[1,]=as.vector(pred[[1]]$probs_lci[[k]][,i])
          
          for (j in 2:length(covariates_list)) {
            tmplist[j,]=as.vector(pred[[j]]$probs_lci[[k]][,i])  
          }
          pjson_lci[[p]]=tmplist
          
          assign(paste0("forMSM",names(pred[[1]]$probs_lci[[k]])[i]),  pjson_lci[[p]])
          
          p=p+1  
          
          
        }
      }
    }
    
    pvector_lci=vector()
    pvector_lci=ls(pattern = "^forMSMP.*lci$")  
    
    pvector_lci=sub("forMSM","",ls(pattern = "^forMSMP.*lci$") )
    
    pvector_lci=stringi::stri_sort(pvector_lci, numeric = TRUE)
    
    names(pjson_lci)=pvector_lci[1:(p-1)]
    
    
    ##### Uci possibilities estimates
    
    pjson_uci=list()
    
    if (length(covariates_list)<=1) {
      p=1
      for (k in 1:nstates) {
        for (i in 1:nstates) {
          
          pjson_uci[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$probs_uci[[k]][,i])))
          assign(paste0("forMSM",names(pred[[1]]$probs_uci[[k]])[i]),  pjson_uci[[p]])
          p=p+1  
        }
      }
    }
    
    if (length(covariates_list)>1) {
      
      p=1
      for (k in 1:nstates) {
        for (i in 1:nstates) {
          
          tmplist=matrix(nrow =length(covariates_list) , ncol= length(vartime))
          tmplist[1,]=as.vector(pred[[1]]$probs_uci[[k]][,i])
          
          for (j in 2:length(covariates_list)) {
            tmplist[j,]=as.vector(pred[[j]]$probs_uci[[k]][,i])  
          }
          pjson_uci[[p]]=tmplist
          
          assign(paste0("forMSM",names(pred[[1]]$probs_uci[[k]])[i]),  pjson_uci[[p]])
          
          p=p+1  
          
          
        }
      }
    }
    
    pvector_uci=vector()
    pvector_uci=ls(pattern = "^forMSMP.*uci$")  
    
    pvector_uci=sub("forMSM","",ls(pattern = "^forMSMP.*uci$") )
    
    pvector_uci=stringi::stri_sort(pvector_uci, numeric = TRUE)
    
    names(pjson_uci)=pvector_uci[1:(p-1)]
    
  }         
  #################################################################################################################
  
  ### Transitions main estimates ###
  hjson=list()
  
  if (length(covariates_list)<=1) {
    
    for (k in 1:ntransitions) {
      
      tmplisth=matrix(nrow =1 , ncol= length(vartime))
      tmplisth[1,]=as.vector(pred[[1]]$trans[,k])
      hjson[[1]]=tmplisth
      assign(paste0("forMSM",names(pred[[1]]$trans)[i]),  hjson[[1]])
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
      assign(paste0("forMSM",names(pred[[1]]$trans)[k]),  hjson[[k]])
    }
  }
  
  hvector=vector()
  hvector=ls(pattern = "^forMSMHaz_._to_.$")  
  
  hvector=sub("forMSM","",ls(pattern = "^forMSMHaz_._to_.$") )
  
  hvector=stringi::stri_sort(hvector, numeric = TRUE)
  
  names(hjson)=hvector
  
  
  
  if (ci.json!="none" ) {
    
    ### Transitions lci estimates ###
    hjson_lci=list()
    
    if (length(covariates_list)<=1) {
      
      for (k in 1:ntransitions) {
        
        tmplisth=matrix(nrow =1 , ncol= length(vartime))
        tmplisth[1,]=as.vector(pred[[1]]$trans_lci[,k])
        hjson_lci[[1]]=tmplisth
        assign(paste0("forMSM",names(pred[[1]]$trans_lci)[i]),  hjson_lci[[1]])
      }
      
    }
    if (length(covariates_list)>1) {
      
      for (k in 1:ntransitions) {
        
        tmplisth=matrix(nrow =length(covariates_list) , ncol= length(vartime))
        
        tmplisth[1,]=as.vector(pred[[1]]$trans_lci[,k])
        
        for (j in 2:length(covariates_list)) {
          
          tmplisth[j,]=as.vector(pred[[j]]$trans_lci[,k])  
        }
        
        
        hjson_lci[[k]]=tmplisth
        assign(paste0("forMSM",names(pred[[1]]$trans_lci)[k]),  hjson_lci[[k]])
      }
    }
    
    hvector_lci=vector()
    hvector_lci=ls(pattern = "^forMSMHaz_._to_._lci$")  
    
    hvector_lci=sub("forMSM","",ls(pattern = "^forMSMHaz_._to_._lci$") )
    
    hvector_lci=stringi::stri_sort(hvector_lci, numeric = TRUE)
    
    names(hjson_lci)=hvector_lci
    
    ### Transitions uci estimates ###
    hjson_uci=list()
    
    if (length(covariates_list)<=1) {
      
      for (k in 1:ntransitions) {
        
        tmplisth=matrix(nrow =1 , ncol= length(vartime))
        tmplisth[1,]=as.vector(pred[[1]]$trans_uci[,k])
        hjson_uci[[1]]=tmplisth
        assign(paste0("forMSM",names(pred[[1]]$trans_uci)[i]),  hjson_uci[[1]])
      }
      
    }
    if (length(covariates_list)>1) {
      
      for (k in 1:ntransitions) {
        
        tmplisth=matrix(nrow =length(covariates_list) , ncol= length(vartime))
        
        tmplisth[1,]=as.vector(pred[[1]]$trans_uci[,k])
        
        for (j in 2:length(covariates_list)) {
          
          tmplisth[j,]=as.vector(pred[[j]]$trans_uci[,k])  
        }
        
        
        hjson_uci[[k]]=tmplisth
        assign(paste0("forMSM",names(pred[[1]]$trans_uci)[k]),  hjson_uci[[k]])
      }
    }
    
    hvector_uci=vector()
    hvector_uci=ls(pattern = "^forMSMHaz_._to_._uci$")  
    
    hvector_uci=sub("forMSM","",ls(pattern = "^forMSMHaz_._to_._uci$") )
    
    hvector_uci=stringi::stri_sort(hvector_uci, numeric = TRUE)
    
    names(hjson_uci)=hvector_uci
  }      
  
  ##########################################################################
  ### Los  ################################################################
  ##############################################################################
  
  if (totlos==TRUE) {
    
    ### Los Main estimates #####
    
    losjson=list()
    
    if (length(covariates_list)<=1) {
      p=1
      for (k in 1:nstates) {
        for (i in 1:nstates) {
          
          losjson[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$los[[k]][,i])))
          assign(paste0("forMSM",names(pred[[1]]$los[[k]])[i]),  losjson[[p]])
          p=p+1  
        }
      }
    }
    
    if (length(covariates_list)>1) {
      
      p=1
      for (k in 1:nstates) {
        for (i in 1:nstates) {
          
          tmplistlos=matrix(nrow =length(covariates_list) , ncol= length(vartime))
          tmplistlos[1,]=as.vector(pred[[1]]$los[[k]][,i])
          
          for (j in 2:length(covariates_list)) {
            tmplistlos[j,]=as.vector(pred[[j]]$los[[k]][,i])  
          }
          losjson[[p]]=tmplistlos
          
          assign(paste0("forMSM",names(pred[[1]]$los[[k]])[i]),  losjson[[p]])
          
          p=p+1  
        }
      }
    }
    
    losvector=vector()
    losvector=ls(pattern = "forMSMLos_._to_.$")  
    
    losvector=sub("forMSM","",ls(pattern = "^forMSMLos_._to_.$") )
    
    losvector=stringi::stri_sort(losvector, numeric = TRUE)
    
    names(losjson)=losvector
    
    if (ci.json!="none" ) {
      
      
      ### Los lci estimates #####
      
      losjson_lci=list()
      
      if (length(covariates_list)<=1) {
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            losjson_lci[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$los_lci[[k]][,i])))
            assign(paste0("forMSM",names(pred[[1]]$los_lci[[k]])[i]),  losjson_lci[[p]])
            p=p+1  
          }
        }
      }
      
      if (length(covariates_list)>1) {
        
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            tmplistlos=matrix(nrow =length(covariates_list) , ncol= length(vartime))
            tmplistlos[1,]=as.vector(pred[[1]]$los_lci[[k]][,i])
            
            for (j in 2:length(covariates_list)) {
              tmplistlos[j,]=as.vector(pred[[j]]$los_lci[[k]][,i])  
            }
            losjson_lci[[p]]=tmplistlos
            
            assign(paste0("forMSM",names(pred[[1]]$los_lci[[k]])[i]),  losjson_lci[[p]])
            
            p=p+1  
          }
        }
      }
      
      losvector_lci=vector()
      losvector_lci=ls(pattern = "^forMSMLos.*lci$")  
      
      losvector_lci=sub("forMSM","",ls(pattern = "^forMSMLos.*lci$") )
      
      losvector_lci=stringi::stri_sort(losvector_lci, numeric = TRUE)
      
      names(losjson_lci)=losvector_lci
      
      ### Los uci estimates #####
      
      losjson_uci=list()
      
      if (length(covariates_list)<=1) {
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            losjson_uci[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$los_uci[[k]][,i])))
            assign(paste0("forMSM",names(pred[[1]]$los_uci[[k]])[i]),  losjson_uci[[p]])
            p=p+1  
          }
        }
      }
      
      if (length(covariates_list)>1) {
        
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            tmplistlos=matrix(nrow =length(covariates_list) , ncol= length(vartime))
            tmplistlos[1,]=as.vector(pred[[1]]$los_uci[[k]][,i])
            
            for (j in 2:length(covariates_list)) {
              tmplistlos[j,]=as.vector(pred[[j]]$los_uci[[k]][,i])  
            }
            losjson_uci[[p]]=tmplistlos
            
            assign(paste0("forMSM",names(pred[[1]]$los_uci[[k]])[i]),  losjson_uci[[p]])
            
            p=p+1  
          }
        }
      }
      
      losvector_uci=vector()
      losvector_uci=ls(pattern = "^forMSMLos.*uci$")  
      
      losvector_uci=sub("forMSM","",ls(pattern = "^forMSMLos.*uci$") )
      
      losvector_uci=stringi::stri_sort(losvector_uci, numeric = TRUE)
      
      names(losjson_uci)=losvector_uci
    }
    
  }     
  
  ##########################################################################
  ### Sojourn  ################################################################
  ##############################################################################
  
  if ( sojourn==TRUE) {
    
    ### Sojourn Main estimates #####
    
    sojjson=list()
    
    if (length(covariates_list)<=1) {
      p=1
      for (k in na_states) {
        
        tmplistsoj=matrix(nrow =1 , ncol= 1)
        tmplistsoj[1,1]=as.matrix(pred[[1]]$soj[[1]][1])
        sojjson[[1]]=tmplistsoj
        assign(paste0("forMSM",names(pred[[1]]$soj)[i]),  tmplistsoj[[1]])
        p=p+1  
      }
    }
    
    if (length(covariates_list)>1) {
      p=1
      for (k in na_states) {
        
        tmplistsoj=matrix(nrow =length(covariates_list) , ncol= 1)
        
        
        for (j in 1:length(covariates_list)) {
          tmplistsoj[j,]=as.matrix(pred[[j]]$soj[[1]][k])  
        }
        
        sojjson[[p]]=tmplistsoj
        assign(paste0("forMSM",names(pred[[1]]$soj[[1]])[k]),  sojjson[[p]])
        p=p+1   
      }
    }
    
    sojvector=vector()
    sojvector=ls(pattern = "forMSMSoj_.$")  
    
    sojvector=sub("forMSM","",ls(pattern = "^forMSMSoj_.$") )
    
    sojvector=stringi::stri_sort(sojvector, numeric = TRUE)
    
    names(sojjson)=sojvector
    
    if (ci.json!="none" ) {
      
      ### Sojourn lci estimates #####
      
      sojjson_lci=list()
      
      if (length(covariates_list)<=1) {
        p=1
        for (k in na_states) {
          
          tmplistsoj=matrix(nrow =1 , ncol= 1)
          tmplistsoj[1,1]=as.matrix(pred[[1]]$soj_lci[[1]][1])
          sojjson_lci[[1]]=tmplistsoj
          assign(paste0("forMSM",names(pred[[1]]$soj_lci)[i]),  tmplistsoj[[1]])
          p=p+1 
        }
      }
      
      if (length(covariates_list)>1) {
        p=1
        for (k in na_states) {
          
          tmplistsoj=matrix(nrow =length(covariates_list) , ncol= 1)
          
          
          for (j in 1:length(covariates_list)) {
            tmplistsoj[j,]=as.matrix(pred[[j]]$soj_lci[[1]][k])  
          }
          
          sojjson_lci[[p]]=tmplistsoj
          assign(paste0("forMSM",names(pred[[1]]$soj_lci[[1]])[k]),  sojjson_lci[[p]])
          p=p+1   
        }
      }
      
      sojvector_lci=vector()
      sojvector_lci=ls(pattern = "^forMSMSoj.*lci$")  
      
      sojvector_lci=sub("forMSM","",ls(pattern = "^forMSMSoj.*lci$") )
      
      sojvector_lci=stringi::stri_sort(sojvector_lci, numeric = TRUE)
      
      names(sojjson_lci)=sojvector_lci      
      
      
      ### Sojourn uci estimates ##### 
      
      sojjson_uci=list()
      
      if (length(covariates_list)<=1) {
        p=1 
        for (k in na_states) {
          
          tmplistsoj=matrix(nrow =1 , ncol= 1)
          tmplistsoj[1,1]=as.matrix(pred[[1]]$soj_uci[[1]][1])
          sojjson_uci[[1]]=tmplistsoj
          assign(paste0("forMSM",names(pred[[1]]$soj_uci)[i]),  tmplistsoj[[1]])
          p=p+1  
        }
      }
      
      if (length(covariates_list)>1) {
        p=1
        for (k in na_states) {
          
          tmplistsoj=matrix(nrow =length(covariates_list) , ncol= 1)
          
          
          for (j in 1:length(covariates_list)) {
            tmplistsoj[j,]=as.matrix(pred[[j]]$soj_uci[[1]][k])  
          }
          
          sojjson_uci[[p]]=tmplistsoj
          assign(paste0("forMSM",names(pred[[1]]$soj_uci[[1]])[k]),  sojjson_uci[[p]])
          p=p+1   
        }
      }
      
      sojvector_uci=vector()
      sojvector_uci=ls(pattern = "^forMSMSoj.*uci$")  
      
      sojvector_uci=sub("forMSM","",ls(pattern = "^forMSMSoj.*uci$") )
      
      sojvector_uci=stringi::stri_sort(sojvector_uci, numeric = TRUE)
      
      names(sojjson_uci)=sojvector_uci    
    }        
  }        
  ##########################################################################
  ### Next  ################################################################
  ##############################################################################
  
  if (pnext==TRUE) {    
    
    ### Next Main estimates #####
    
    nextjson=list()
    
    if (length(covariates_list)<=1) {
      
      for (k in 1:nstates) {
        
        tmplistnext=matrix(nrow =1 , ncol= 1)
        tmplistnext[1,1]=as.matrix(pred[[1]]$nextv[[1]][1])
        nextjson[[1]]=tmplistnext
        assign(paste0("forMSM",names(pred[[1]]$nextv)[i]),  tmplistnext[[1]])
      }
    }
    
    if (length(covariates_list)>1) {
      p=1
      for (k in 1:nstates) {
        
        tmplistnext=matrix(nrow =length(covariates_list) , ncol= 1)
        
        
        for (j in 1:length(covariates_list)) {
          tmplistnext[j,]=as.matrix(pred[[j]]$nextv[[1]][k])  
        }
        
        nextjson[[p]]=tmplistnext
        assign(paste0("forMSM",names(pred[[1]]$nextv[[1]])[k]),  nextjson[[p]])
        p=p+1   
      }
    }
    
    nextvector=vector()
    nextvector=ls(pattern = "forMSMNext_._to_.$")  
    
    nextvector=sub("forMSM","",ls(pattern = "^forMSMNext_._to_.$") )
    
    nextvector=stringi::stri_sort(nextvector, numeric = TRUE)
    
    names(nextjson)=nextvector
    
    if (ci.json!="none" ) {
      
      ####### Next lci estimates ####################################
      nextjson_lci=list()
      
      if (length(covariates_list)<=1) {
        
        for (k in 1:nstates) {
          
          tmplistnext=matrix(nrow =1 , ncol= 1)
          tmplistnext[1,1]=as.matrix(pred[[1]]$nextv_lci[[1]][1])
          nextjson_lci[[1]]=tmplistnext
          assign(paste0("forMSM",names(pred[[1]]$nextv_lci)[i]),  tmplistnext[[1]])
        }
      }
      
      if (length(covariates_list)>1) {
        p=1
        for (k in 1:nstates) {
          
          tmplistnext=matrix(nrow =length(covariates_list) , ncol= 1)
          
          
          for (j in 1:length(covariates_list)) {
            tmplistnext[j,]=as.matrix(pred[[j]]$nextv_lci[[1]][k])  
          }
          
          nextjson_lci[[p]]=tmplistnext
          assign(paste0("forMSM",names(pred[[1]]$nextv_lci[[1]])[k]),  nextjson_lci[[p]])
          p=p+1   
        }
      }
      
      nextvector_lci=vector()
      nextvector_lci=ls(pattern = "^forMSMNext.*lci$")  
      
      nextvector_lci=sub("forMSM","",ls(pattern = "^forMSMNext.*lci$") )
      
      nextvector_lci=stringi::stri_sort(nextvector_lci, numeric = TRUE)
      
      names(nextjson_lci)=nextvector_lci
      
      ####### Next uci estimates ####################################
      
      nextjson_uci=list()
      
      if (length(covariates_list)<=1) {
        
        for (k in 1:nstates) {
          
          tmplistnext=matrix(nrow =1 , ncol= 1)
          tmplistnext[1,1]=as.matrix(pred[[1]]$nextv_uci[[1]][1])
          nextjson_uci[[1]]=tmplistnext
          assign(paste0("forMSM",names(pred[[1]]$nextv_uci)[i]),  tmplistnext[[1]])
        }
      }
      
      if (length(covariates_list)>1) {
        p=1
        for (k in 1:nstates) {
          
          tmplistnext=matrix(nrow =length(covariates_list) , ncol= 1)
          
          
          for (j in 1:length(covariates_list)) {
            tmplistnext[j,]=as.matrix(pred[[j]]$nextv_uci[[1]][k])  
          }
          
          nextjson_uci[[p]]=tmplistnext
          assign(paste0("forMSM",names(pred[[1]]$nextv_uci[[1]])[k]),  nextjson_uci[[p]])
          p=p+1   
        }
      }
      
      nextvector_uci=vector()
      nextvector_uci=ls(pattern = "^forMSMNext.*uci$")  
      
      nextvector_uci=sub("forMSM","",ls(pattern = "^forMSMNext.*uci$") )
      
      nextvector_uci=stringi::stri_sort(nextvector_uci, numeric = TRUE)
      
      names(nextjson_uci)=nextvector_uci
    }         
  }
  ###################################################################################################
  
  
  ##########################################################################
  ### First  ################################################################
  ##############################################################################
  
  if (efpt==TRUE) {  
    
    ### First Main estimates #####
    
    firstjson=list()
    
    if (length(covariates_list)<=1) {
      
      for (k in 1:3) {
        
        tmplistfirst=matrix(nrow =1 , ncol= 1)
        tmplistfirst[1,1]=as.matrix(pred[[1]]$first[[1]][1])
        firstjson[[1]]=tmplistfirst
        assign(paste0("forMSM",names(pred[[1]]$first)[i]),  tmplistfirst[[1]])
      }
    }
    
    if (length(covariates_list)>1) {
      p=1
      for (k in 1:3) {
        
        tmplistfirst=matrix(nrow =length(covariates_list) , ncol= 1)
        
        
        for (j in 1:length(covariates_list)) {
          tmplistfirst[j,]=as.matrix(pred[[j]]$first[[1]][k])  
        }
        
        firstjson[[p]]=tmplistfirst
        assign(paste0("forMSM",names(pred[[1]]$first[[1]])[k]),  firstjson[[p]])
        p=p+1   
      }
    }
    
    firstvector=vector()
    firstvector=ls(pattern = "forMSMFirst_.$")  
    
    firstvector=sub("forMSM","",ls(pattern = "^forMSMFirst") )
    
    firstvector=stringi::stri_sort(firstvector, numeric = TRUE)
    
    names(firstjson)=firstvector
    
    if (ci.json!="none" ) {
      
      ####### First lci estimates ####################################
      
      firstjson_lci=list()
      
      if (length(covariates_list)<=1) {
        
        for (k in 1:nstates) {
          
          tmplistfirst=matrix(nrow =1, ncol= 1)
          tmplistfirst[1,1]=as.matrix(pred[[1]]$first_lci[[1]][1])
          firstjson_lci[[1]]=tmplistfirst
          assign(paste0("forMSM",names(pred[[1]]$first_lci)[i]),  tmplistfirst[[1]])
        }
      }
      
      if (length(covariates_list)>1) {
        p=1
        for (k in 1:nstates) {
          
          tmplistfirst=matrix(nrow =length(covariates_list) , ncol= 1)
          
          
          for (j in 1:length(covariates_list)) {
            tmplistfirst[j,]=as.matrix(pred[[j]]$first_lci[[1]][k])  
          }
          
          firstjson_lci[[p]]=tmplistfirst
          assign(paste0("forMSM",names(pred[[1]]$first_lci[[1]])[k]),  firstjson_lci[[p]])
          p=p+1   
        }
      }
      
      firstvector_lci=vector()
      firstvector_lci=ls(pattern = "^forMSMFirst.*lci$")  
      
      firstvector_lci=sub("forMSM","",ls(pattern ="^forMSMFirst.*lci$" ) )
      
      firstvector_lci=stringi::stri_sort(firstvector_lci, numeric = TRUE)
      
      names(firstjson_lci)=firstvector_lci
      
      
      ####### First uci estimates ####################################
      
      firstjson_uci=list()
      
      if (length(covariates_list)<=1) {
        
        for (k in 1:nstates) {
          
          tmplistfirst=matrix(nrow =1 , ncol= 1)
          tmplistfirst[1,1]=as.matrix(pred[[1]]$first_uci[[1]][1])
          firstjson_uci[[1]]=tmplistfirst
          assign(paste0("forMSM",names(pred[[1]]$first_uci)[i]),  tmplistfirst[[1]])
        }
      }
      
      if (length(covariates_list)>1) {
        p=1
        for (k in 1:nstates) {
          
          tmplistfirst=matrix(nrow =length(covariates_list) , ncol= 1)
          
          
          for (j in 1:length(covariates_list)) {
            tmplistfirst[j,]=as.matrix(pred[[j]]$first_uci[[1]][k])  
          }
          
          firstjson_uci[[p]]=tmplistfirst
          assign(paste0("forMSM",names(pred[[1]]$first_uci[[1]])[k]),  firstjson_uci[[p]])
          p=p+1   
        }
      }
      
      firstvector_uci=vector()
      firstvector_uci=ls(pattern = "^forMSMFirst.*uci$")  
      
      firstvector_uci=sub("forMSM","",ls(pattern ="^forMSMFirst.*uci$" ) )
      
      firstvector_uci=stringi::stri_sort(firstvector_uci, numeric = TRUE)
      
      names(firstjson_uci)=firstvector_uci
    }
  }      
  ################################################################################################
  
  ##########################################################################
  ### Number  ################################################################
  ##############################################################################
  
  
  if  (envisits==TRUE) {      
    
    ### Number Main estimates #####
    
    numberjson=list()
    
    if (length(covariates_list)<=1) {
      p=1
      for (k in 1:nstates) {
        for (i in 1:nstates) {
          
          numberjson[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$number[[k]][,i])))
          assign(paste0("forMSM",names(pred[[1]]$number[[k]])[i]),  numberjson[[p]])
          p=p+1  
        }
      }
    }
    
    if (length(covariates_list)>1) {
      
      p=1
      for (k in 1:nstates) {
        for (i in 1:nstates) {
          
          tmplistnumber=matrix(nrow =length(covariates_list) , ncol= length(vartime))
          tmplistnumber[1,]=as.vector(pred[[1]]$number[[k]][,i])
          
          for (j in 2:length(covariates_list)) {
            tmplistnumber[j,]=as.vector(pred[[j]]$number[[k]][,i])  
          }
          numberjson[[p]]=tmplistnumber
          
          assign(paste0("forMSM",names(pred[[1]]$number[[k]])[i]),  numberjson[[p]])
          
          p=p+1  
        }
      }
    }
    
    numbervector=vector()
    numbervector=ls(pattern = "forMSMNumber_._to_.$")  
    
    numbervector=sub("forMSM","",ls(pattern = "^forMSMNumber_._to_.$") )
    
    numbervector=stringi::stri_sort(numbervector, numeric = TRUE)
    
    names(numberjson)=numbervector
    
    if (ci.json!="none" ) {
      
      ### Number lci estimates #####
      
      numberjson_lci=list()
      
      if (length(covariates_list)<=1) {
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            numberjson_lci[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$number_lci[[k]][,i])))
            assign(paste0("forMSM",names(pred[[1]]$number_lci[[k]])[i]),  numberjson_lci[[p]])
            p=p+1  
          }
        }
      }
      
      if (length(covariates_list)>1) {
        
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            tmplistnumber=matrix(nrow =length(covariates_list) , ncol= length(vartime))
            tmplistnumber[1,]=as.vector(pred[[1]]$number_lci[[k]][,i])
            
            for (j in 2:length(covariates_list)) {
              tmplistnumber[j,]=as.vector(pred[[j]]$number_lci[[k]][,i])  
            }
            numberjson_lci[[p]]=tmplistnumber
            
            assign(paste0("forMSM",names(pred[[1]]$number_lci[[k]])[i]),  numberjson_lci[[p]])
            
            p=p+1  
          }
        }
      }
      
      numbervector_lci=vector()
      numbervector_lci=ls(pattern = "^forMSMNumber.*lci$")  
      
      numbervector_lci=sub("forMSM","",ls(pattern = "^forMSMNumber.*lci$") )
      
      numbervector_lci=stringi::stri_sort(numbervector_lci, numeric = TRUE)
      
      names(numberjson_lci)=numbervector_lci
      
      ### Number uci estimates #####
      numberjson_uci=list()
      
      if (length(covariates_list)<=1) {
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            numberjson_uci[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$number_uci[[k]][,i])))
            assign(paste0("forMSM",names(pred[[1]]$number_uci[[k]])[i]),  numberjson_uci[[p]])
            p=p+1  
          }
        }
      }
      
      if (length(covariates_list)>1) {
        
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            tmplistnumber=matrix(nrow =length(covariates_list) , ncol= length(vartime))
            tmplistnumber[1,]=as.vector(pred[[1]]$number_uci[[k]][,i])
            
            for (j in 2:length(covariates_list)) {
              tmplistnumber[j,]=as.vector(pred[[j]]$number_uci[[k]][,i])  
            }
            numberjson_uci[[p]]=tmplistnumber
            
            assign(paste0("forMSM",names(pred[[1]]$number_uci[[k]])[i]),  numberjson_uci[[p]])
            
            p=p+1  
          }
        }
      }
      
      numbervector_uci=vector()
      numbervector_uci=ls(pattern = "^forMSMNumber.*uci$")  
      
      numbervector_uci=sub("forMSM","",ls(pattern = "^forMSMNumber.*uci$") )
      
      numbervector_uci=stringi::stri_sort(numbervector_uci, numeric = TRUE)
      
      names(numberjson_uci)=numbervector_uci
    }
  }
  
  ######################################################################################################
  
  ##########################################################################
  ### Visit  ################################################################
  ##############################################################################
  
  if (visit==TRUE) {   
    
    ### Visit Main estimates #####
    vjson=list()
    
    if (length(covariates_list)<=1) {
      p=1
      for (k in 1:nstates) {
        for (i in 1:nstates) {
          
          vjson[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$visit[[k]][,i])))
          assign(paste0("forMSM",names(pred[[1]]$visit[[k]])[i]),  vjson[[p]])
          p=p+1  
        }
      }
    }
    
    if (length(covariates_list)>1) {
      
      p=1
      for (k in 1:nstates) {
        for (i in 1:nstates) {
          
          tmplistvisit=matrix(nrow =length(covariates_list) , ncol= length(vartime))
          tmplistvisit[1,]=as.vector(pred[[1]]$visit[[k]][,i])
          
          for (j in 2:length(covariates_list)) {
            tmplistvisit[j,]=as.vector(pred[[j]]$visit[[k]][,i])  
          }
          vjson[[p]]=tmplistvisit
          
          assign(paste0("forMSM",names(pred[[1]]$visit[[k]])[i]),  vjson[[p]])
          
          p=p+1  
        }
      }
    }
    
    visvector=vector()
    visvector=ls(pattern = "forMSMVisit_._to_.$")  
    
    visvector=sub("forMSM","",ls(pattern = "^forMSMVisit_._to_.$") )
    
    visvector=stringi::stri_sort(visvector, numeric = TRUE)
    
    names(vjson)=visvector
    
    if (ci.json!="none" ) {
      
      ### Visit lci estimates #####
      
      vjson_lci=list()
      
      if (length(covariates_list)<=1) {
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            vjson_lci[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$visit_lci[[k]][,i])))
            assign(paste0("forMSM",names(pred[[1]]$visit_lci[[k]])[i]),  vjson_lci[[p]])
            p=p+1  
          }
        }
      }
      
      if (length(covariates_list)>1) {
        
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            tmplistvisit=matrix(nrow =length(covariates_list) , ncol= length(vartime))
            tmplistvisit[1,]=as.vector(pred[[1]]$visit_lci[[k]][,i])
            
            for (j in 2:length(covariates_list)) {
              tmplistvisit[j,]=as.vector(pred[[j]]$visit_lci[[k]][,i])  
            }
            vjson_lci[[p]]=tmplistvisit
            
            assign(paste0("forMSM",names(pred[[1]]$visit_lci[[k]])[i]),  vjson_lci[[p]])
            
            p=p+1  
          }
        }
      }
      
      visvector_lci=vector()
      visvector_lci=ls(pattern = "^forMSMVisit.*lci$")  
      
      visvector_lci=sub("forMSM","",ls(pattern ="^forMSMVisit.*lci$") )
      
      visvector_lci=stringi::stri_sort(visvector_lci, numeric = TRUE)
      
      names(vjson_lci)=visvector_lci[1:(p-1)]
      
      
      ### Visit uci estimates #####
      
      vjson_uci=list()
      
      if (length(covariates_list)<=1) {
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            vjson_uci[[p]]= as.data.frame(rbind(as.vector(pred[[1]]$visit_uci[[k]][,i])))
            assign(paste0("forMSM",names(pred[[1]]$visit_uci[[k]])[i]),  vjson_uci[[p]])
            p=p+1  
          }
        }
      }
      
      if (length(covariates_list)>1) {
        
        p=1
        for (k in 1:nstates) {
          for (i in 1:nstates) {
            
            tmplistvisit=matrix(nrow =length(covariates_list) , ncol= length(vartime))
            tmplistvisit[1,]=as.vector(pred[[1]]$visit_uci[[k]][,i])
            
            for (j in 2:length(covariates_list)) {
              tmplistvisit[j,]=as.vector(pred[[j]]$visit_uci[[k]][,i])  
            }
            vjson_uci[[p]]=tmplistvisit
            
            assign(paste0("forMSM",names(pred[[1]]$visit_uci[[k]])[i]),  vjson_uci[[p]])
            
            p=p+1  
          }
        }
      }
      
      visvector_uci=vector()
      visvector_uci=ls(pattern = "^forMSMVisit.*uci$")  
      
      visvector_uci=sub("forMSM","",ls(pattern ="^forMSMVisit.*uci$") )
      
      visvector_uci=stringi::stri_sort(visvector_uci, numeric = TRUE)
      
      names(vjson_uci)=visvector_uci[1:(p-1)]
      
    }       
  }
  ######################################################################################################
  
  
  
  ##########################################################################################
  
  ### vartime###
  
  timejson=list()
  #timematrix=matrix(nrow=1, ncol=length(vartime), NA)
  #timematrix[1,]=vartime
  timejson[[1]]=vartime
  
  
  
  ######################################################################################################
  
  ### At list ###
  
  atlistjson=list()
  
  
  
if (length(covariates_list)>=1) {
    
   atlistmatrix=matrix(nrow=1, ncol=length(covariates_list), NA)
  
   for (j in 1:length(covariates_list) ) {
    
      name_paste=list()
    
    for (i in 1:length(covariates_list[[j]]) )  {
      
      name_paste[[i]]= paste0(names(covariates_list[[j]])[i]," ", as.character(covariates_list[[j]][i])  )
      
    }
    atlistmatrix[1,j]= paste(unlist(name_paste,recursive=FALSE), collapse = ' ')
   }
   
}
  
  if (length(covariates_list)==0) {
    atlistmatrix=matrix(nrow=1, ncol=1, NA)
    
    name_paste= "all"  
    
    atlistmatrix[1,1]= name_paste
  }
  
  atlistjson[[1]]=as.vector(atlistmatrix)
  
  
  
  ######################################################################################################
  
  
  final_list=list(timevar=timejson, Nats=length(covariates_list) ,
                  atlist=atlistjson, tmat=tmatlist, intermediate_states=intermediate_list, Ntransitions= ntransitions,
                  pjson,pjson_lci,pjson_uci,
                  hjson,hjson_lci,hjson_uci,
                  losjson,losjson_lci,losjson_uci,
                  sojjson,sojjson_lci,sojjson_uci,
                  nextjson,nextjson_lci,nextjson_uci,
                  firstjson, firstjson_lci, firstjson_uci,
                  numberjson,numberjson_lci,numberjson_uci,
                  vjson,vjson_lci,vjson_uci
  )
  
  final_list$timevar=as.vector(final_list$timevar)
  
  # final_list$atlistjson=as.vector(final_list$atlistjson[[1]])
  
  final_unlist= unlist(final_list, recursive=FALSE)
  
  
  
  exportJson <- toJSON(final_unlist, pretty = TRUE,force = TRUE, flatten=TRUE, na='string')
  
  #exportJson
  
  write(exportJson, paste0(jsonpath ,"/", name ) )
  
  rm(list=ls(pattern="^forMSM"))
  
  final_list
  
}
