msboxes_R_nofreq<- function(yb,xb,boxwidth,boxheight,tmat.) {
  
  library(plyr)
  library(viridis)
  library(dplyr)
  library(reshape2)
  library(tidyverse)
  
  
  ##########Read info from transition matrix #############################
  ####Number of transitions and number of states##############
  tmat.[is.na(tmat.)] <- 0
  ntransitions=max(as.matrix(tmat.))
  nstates=ncol(tmat.)
  states=c(seq(1:nstates))
  #### Transitions values
  u=list()
  for (i in 1:nrow(tmat.)) {
    u[[i]]=unique(tmat.[i,][which(!is.na(tmat.[i,]))])
  }
  u_unlist=sort(unlist(u, recursive = TRUE, use.names = TRUE))
  transitions=u_unlist[u_unlist!=0]
  ######## Categorizing states###############
  state_kind=vector()
  for (i in 1:nstates) {
    if (length(which(tmat.[,i]==0))==nrow(tmat.)) {state_kind[i]="Starting"}
    else if (length(which(tmat.[i,]==0))==ncol(tmat.)) {state_kind[i]="Absorbing"}
    else {state_kind[i]="Intermediate"}
  }
  st_states=which(state_kind=="Starting")
  na_states=which(state_kind=="Intermediate")
  ab_states=which(state_kind=="Absorbing")
  ##############################################################################
   tname=vector()
  for (i in 1: ntransitions) {
    tname[i]=paste0("h",i) }
  
  statename=vector()
  for (i in 1: nstates) {
    statename[i]=paste0("State",i) }
  
  tr_start_state=vector()
  for (k in 1:ntransitions) {
    tr_start_state[k]=which(tmat. == k, arr.ind = TRUE)[1,1]
  }
  
  tr_end_state=vector()
  for (k in 1:ntransitions) {
    tr_end_state[k]=which(tmat. == k, arr.ind = TRUE)[1,2]
  }
  
names_ststates=vector(); names_nastates=vector(); names_abstates=vector();names_transitions=vector();
  
  for (i in st_states) {
    names_ststates[i]=paste0("State",i)
  }
  names_ststates=names_ststates[which(!is.na(names_ststates))]
  
  for (i in na_states) {
    names_nastates[i]=paste0("State",i)
  }
  names_nastates=names_nastates[which(!is.na(names_nastates))]
  
  for (i in ab_states) {
    names_abstates[i]=paste0("State",i)
  }
  names_abstates=names_abstates[which(!is.na(names_abstates))]
  
  for (i in transitions) {
    names_transitions[i]=paste0("h",i)
  }
  
  #####
  
  y=yb
  x=xb
  lefttoright=vector()
  toptobottom=vector()
  y1=vector()
  x1=vector()
  y2=vector()
  x2=vector()
  arrowstexty=vector()
  arrowstextx=vector()
  gradient=vector()
  cutoff=0.5
  textleft=vector()
  
  
  for (i in 1:ntransitions) {
    if (x[tr_start_state[i]]>x[tr_end_state[i]]) {textleft[i]=-1} 
    else {textleft[i]=1}
  }
  
  for (i in 1:ntransitions) {
    if (x[tr_start_state[i]]<x[tr_end_state[i]]) {lefttoright[i]=1} 
    else {lefttoright[i]=-1}
  }
  
  for (i in 1:ntransitions) {
    if (y[tr_start_state[i]]>y[tr_end_state[i]]) {toptobottom[i]=-1} 
    else {toptobottom[i]=1}
  }
  
  for (i in 1:ntransitions) {
    
    gradient[i]=abs(y[tr_start_state[i]]-y[tr_end_state[i]])/abs(x[tr_start_state[i]]-x[tr_end_state[i]])
  }
  
  
  for (i in 1:ntransitions) {
    ##Horizontal
    if (y[tr_start_state[i]]==y[tr_end_state[i]]) {
      y1[i]=y[tr_start_state[i]]
      x1[i]=x[tr_start_state[i]]+lefttoright[i]*boxwidth/2
      y2[i]=y[tr_end_state[i]]
      x2[i]=x[tr_end_state[i]]  -lefttoright[i]*boxwidth/2
      arrowstexty[i]=y[tr_start_state[i]]+0.01
      arrowstextx[i]=(x[tr_start_state[i]]+x[tr_end_state[i]])/2
    }
    ##Vertical
    
    else if (x[tr_start_state[i]]==x[tr_end_state[i]]) {
      y1[i]=y[tr_start_state[i]]+toptobottom[i]*boxheight/2
      x1[i]=x[tr_start_state[i]]
      y2[i]=y[tr_end_state[i]]-toptobottom[i]*boxheight/2
      x2[i]=x[tr_end_state[i]]
      arrowstexty[i]=(y[tr_start_state[i]]+y[tr_end_state[i]])/2
      arrowstextx[i]=x[tr_start_state[i]]-0.01
    }
    ##Dradient
    else{
      
      if (gradient[i]<cutoff) {
        
        y1[i]=y[tr_start_state[i]]
        x1[i]=x[tr_start_state[i]]+lefttoright[i]*boxwidth/2
        y2[i]=y[tr_end_state[i]]
        x2[i]=x[tr_end_state[i]]  -lefttoright[i]*boxwidth/2
        arrowstexty[i]=(y[tr_start_state[i]]+y[tr_end_state[i]])/2 
        arrowstextx[i]=(x[tr_start_state[i]]+x[tr_end_state[i]])/2 +textleft[i]*0.02
      }
      
      else {
        y1[i]=y[tr_start_state[i]]+toptobottom[i]*boxheight/2
        x1[i]=x[tr_start_state[i]]
        y2[i]=y[tr_end_state[i]]-toptobottom[i]*boxheight/2
        x2[i]=x[tr_end_state[i]]
        arrowstexty[i]=(y[tr_start_state[i]]+y[tr_end_state[i]])/2 
        arrowstextx[i]=(x[tr_start_state[i]]+x[tr_end_state[i]])/2 +textleft[i]*0.02
      }
      
    }
  }
  
  #list(x1,y1,x2,y2,arrowstextx,arrowstexty)
  arrows=list(x1=x1,y1=y1,x2=x2,y2=y2)
  arrowstext=list(x=arrowstextx,y=arrowstexty)
  
  list(xvalues=xb, yvalues=yb,
       boxwidth=boxwidth, boxheight=boxheight,
       arrows=arrows,arrowstext=arrowstext)
}


#results12=msboxes_R(data=msebmt,id= msebmt$id, yb=c(0.8,0.8,0.8,0.4,0.4,0.4),
#                    xb=c(0.2,0.5,0.8,0.2,0.5,0.8),boxwidth=0.1,boxheight=0.1,
#                    tmat.= tmat, time_study=msebmt$time,vartime=c(seq(0,2000,by=200)),scale=1) 
#
#results12
