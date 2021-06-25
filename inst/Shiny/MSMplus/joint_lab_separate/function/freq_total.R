

freq_func_total <- function(msdata,msid,names_of_ststates, values_ststates,
                            names_of_nastates, values_nastates,
                            names_of_abstates,values_abstates,
                            names_of_transitions,values_of_transitions,
                            time, timevar,scale_inner=1) {
  
  library(plyr)
  library(viridis)
  library(dplyr)
  library(reshape2)
  library(tidyverse)
  
  fr=list()
  
  for (h in 1:length(timevar)) {
  #  from=dat$from, to=dat$to, trans=dat$trans,time=dat$rfstime, 
  
 # data<- read.csv("C:/Users/niksko/Desktop/mstate/jsonread/msset_ebmt.csv",header=TRUE, sep=";")
  options(scipen = 999)  
  data=msdata
  attach(data)
  names(data)
  
  data$time_tot=time       #######################################
  
  dis_id=vector()
  dis_id=unique(msid)   ########################
  
  listid=list()
  
  for (i in 1:length(dis_id)) {
    listid[[i]]=as.data.frame(data[data$id==dis_id[i],])
  }
  
  ############################################################################
  #Frequency of remaining in starting state until a certain time point##
  ############################################################################
  #Names of starting states
  names_ststates=names_of_ststates   ##############################################
  #Values of non absorbing states
  v_ststate=values_ststates         ##################################################  
  max(v_ststate)
  
  v_st=array(dim=c(length(dis_id),1,max(v_ststate)),"NA")
  for (k in v_ststate) {
    for (i in 1:length(dis_id)) {
      if (length(which(listid[[i]]$from==k & listid[[i]]$status==1 & listid[[i]]$time_tot<=timevar[h]*scale_inner))==0) {v_st[i,1,k]=TRUE} else {v_st[i,1,k]=FALSE}
    }
  }
  freq_stay_st=vector()
  for (k in v_ststate) {
    freq_stay_st[k]=length(which(v_st[,1,k]=="TRUE"))
  }
  freq_stay_st=freq_stay_st[!is.na(freq_stay_st)]
  freq_stay_st=matrix(nrow=1,ncol=length(freq_stay_st),freq_stay_st)
  freq_stay_st=freq_stay_st
  colnames(freq_stay_st)<-names_ststates
  freq_stay_st
  
  ############################################################################
  #Frequency of non absorbing intermediate states until a certain time point##
  ############################################################################
  #Names of non absorbing state
  names_nastates=names_of_nastates   ####################################
  #Values of non absorbing states
  v_nastate=values_nastates         ######################################
  
  max(v_nastate)
  
  v_na=array(dim=c(length(dis_id),1,max(v_nastate)),"NA")
  for (k in v_nastate) {
    for (i in 1:length(dis_id)) {
      if (length(which(listid[[i]]$from==k & listid[[i]]$status==1 & listid[[i]]$time_tot<=timevar[h]*scale_inner))==0 &
          length(which(listid[[i]]$from!=k & listid[[i]]$to==k & listid[[i]]$status==1 & listid[[i]]$time_tot<=timevar[h]*scale_inner))!=0) {
          v_na[i,1,k]=TRUE} else {v_na[i,1,k]=FALSE
          }
    }
  }
  
  freq_stay_na=vector()
  for (k in v_nastate) {
    freq_stay_na[k]=length(which(v_na[,1,k]=="TRUE"))
  }
  freq_stay_na=freq_stay_na[!is.na(freq_stay_na)]
  freq_stay_na=matrix(nrow=1,ncol=length(freq_stay_na),freq_stay_na)
  colnames(freq_stay_na)<-names_nastates
  freq_stay_na=freq_stay_na

  freq_stay_na
  
  ############################################################################
  #Frequency of ending up in absorbing states until a certain time point##
  ############################################################################
  #Names of  absorbing state
  names_abstates=names_of_abstates   #####################################
  #Values of non absorbing states 
  v_abstate=values_abstates           ###################################
  max(v_abstate)
  
  #Frequency of remaining in non absorbing states until a certain time point
  freq_stay_ab=vector()
  
  for (k in v_abstate) {
    
    freq_stay_ab[k]= length(which(data$to==k & data$status==1 & data$time_tot<=timevar[h]*scale_inner ))
  }
  
  freq_stay_ab=freq_stay_ab[!is.na(freq_stay_ab)]
  freq_stay_ab=matrix(nrow=1,ncol=length(freq_stay_ab),freq_stay_ab)
  freq_stay_ab=freq_stay_ab
  colnames(freq_stay_ab)<-names_abstates
  freq_stay_ab
  ######################################################
  frequencies_stay=cbind(freq_stay_st,freq_stay_na,freq_stay_ab)
 # frequencies_stay[ , order(names(frequencies_stay))]
  frequencies_stay
  #########################################################
  
  #Names of transitions
  names_transitions= names_of_transitions
  
  #Values of transitions
  v_trans=values_of_transitions
  
  trans=array(dim=c(length(dis_id),length(v_trans)),"NA")
  for (k in 1:length(v_trans)) {
    for (i in 1:length(dis_id)) {
      if (length(which(listid[[i]]$trans==k & listid[[i]]$status==1 & listid[[i]]$time_tot<=timevar[h]*scale_inner ))==1) 
      {trans[i,k]=TRUE} else {trans[i,k]=FALSE
        }
    }
  }
  
  freq_trans=vector()
  for (k in 1:length(v_trans)) {
    freq_trans[k]=length(which(trans[,v_trans[k]]=="TRUE"))
  }
  
  freq_trans=matrix(nrow=1,ncol=length(freq_trans),freq_trans)
  colnames(freq_trans)<-names_transitions
  freq_trans
  
  results=as.data.frame(cbind(frequencies_stay,freq_trans,timevar[h]))
  colnames(results)[ncol(results)]="timevar"
  
  fr[[h]]=results
  
  }
  

fr_time=bind_rows(fr, .id = "time_label")

fr_time

}

#dat<- read.csv("C:/Users/niksko/Desktop/mstate/jsonread/msset_ebmt.csv",header=TRUE, sep=";")

#p=freq_func_total(msdata=dat,msid=dat$id,
#                         names_of_ststates=c("State1"), values_ststates=1,
#                         names_of_nastates=c("State2"), values_nastates=2,
#                         names_of_abstates=c("State3"),values_abstates=3,
#                         names_of_transitions=c("h1","h2","h3"),values_of_transitions=c(1,2,3),
#                         time=dat$X_stop, timevar=c(seq(0,3000,150)))
#p
 