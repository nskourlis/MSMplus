


msboxes_R<- function(data,id,yb,xb,boxwidth,boxheight,time_study,vartime, tmat., scale) {
   
   library(plyr)
   library(viridis)
   library(dplyr)
   library(reshape2)
   library(tidyverse)

  
  ##########Read info from transition matrix #############################
  
  ####Number of transitions and number of states##############
  tmat_inner=tmat.
  tmat_inner[is.na(tmat_inner)] <- 0
  ntransitions=max(tmat_inner)
  nstates=ncol(tmat_inner)
  states=c(seq(1:nstates))
  #### Transitions values
  u=list()
  for (i in 1:nrow(tmat_inner)) {
    u[[i]]=unique(tmat_inner[i,][which(!is.na(tmat_inner[i,]))])
  }
  u_unlist=sort(unlist(u, recursive = TRUE, use.names = TRUE))
  transitions=u_unlist[u_unlist!=0]
  ######## Categorizing states###############
  state_kind=vector()
  for (i in 1:nstates) {
    if (length(which(tmat_inner[,i]==0))==nrow(tmat_inner)) {state_kind[i]="Starting"}
    else if (length(which(tmat_inner[i,]==0))==ncol(tmat_inner)) {state_kind[i]="Absorbing"}
    else {state_kind[i]="Intermediate"}
  }
  st_states=which(state_kind=="Starting")
  na_states=which(state_kind=="Intermediate")
  ab_states=which(state_kind=="Absorbing")
  ##############################################################################
  
  ##########Read info from data #############################
  
#  ntransitions=max(data$trans)
#  nstates= max(data$to)
#  transitions=order(unique(data$trans))  
#
#  d1=unique(from)
#  d2=unique(to)
#  states=unique(c(d1,d2))
#  states=order(states)
#  st_states=vector()
#  na_states=vector()
#  ab_states=vector()
#
#  for (i in 1:length(states)) {
#    if (length(which(data$from==i))==0) {ab_states[i]=states[i]}   }
#      ab_states=ab_states[!is.na(ab_states)]
#
#  for (i in 1:length(states)) {
#    if (length(which(data$to==i))==0) {st_states[i]=states[i]}   }
#    st_states=st_states[!is.na(st_states)]
#
#  na_states=states[which(states!=ab_states & states!=st_states)]
###################################################################################
tname=vector()
for (i in 1: ntransitions) {
  tname[i]=paste0("h",i) }

statename=vector()
for (i in 1: nstates) {
  statename[i]=paste0("State",i) }


tr_start_state=vector()
for (k in 1:ntransitions) {
  tr_start_state[k]=which(tmat_inner == k, arr.ind = TRUE)[1,1]
}

tr_end_state=vector()
for (k in 1:ntransitions) {
  tr_end_state[k]=which(tmat_inner == k, arr.ind = TRUE)[1,2]
}

#########Count initials in each state- Freq total does that anyway###########

#data=data[order(data$id,data$from,data$to),]
#
#dis_id=vector()
#dis_id=unique(data$id)   ########################
#
#listid=list()
#
#for (i in 1:length(dis_id)) {
#  listid[[i]]=as.data.frame(data[data$id==dis_id[i],])
#}
#
#init=array(dim=c(length(dis_id),1,nstates),"NA")
#for (k in 1:nstates) {
#  for (i in 1:length(dis_id)) {
#    if (length(which(listid[[i]]$from[1]==k))!=0) {init[i,1,k]=TRUE} else {init[i,1,k]=FALSE}
#  }
#}
#init_freq=vector()
#for (k in 1:nstates) {
#  init_freq[k]=length(which(init[,1,k]=="TRUE"))
#}
#init_freq

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

source("joint_lab_separate/function/freq_total.R",local=T)$value
 p=freq_func_total(msdata=data,msid=id,
                          names_of_ststates=names_ststates, values_ststates=st_states,
                          names_of_nastates=names_nastates, values_nastates=na_states,
                          names_of_abstates=names_abstates, values_abstates=ab_states,
                          names_of_transitions=names_transitions,values_of_transitions=transitions,
                          time=time_study, timevar=vartime, scale_inner=scale)
 
 
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
 
# xb=xb-boxwidth/2
# yb=yb+boxheight/2
 
 #list(x1,y1,x2,y2,arrowstextx,arrowstexty)
 arrows=list(x1=x1,y1=y1,x2=x2,y2=y2)
 arrowstext=list(x=arrowstextx,y=arrowstexty)
 
# for (i in 1:max(p$time_label)) {
# p$timevar[i]=p$timevar[i]/scale
# }
 
 list(Nstates=nstates,Ntransitions=ntransitions, 
      xvalues=xb, yvalues=yb,
      boxwidth=boxwidth, boxheight=boxheight,
      statenames=statename, transnames=tname,
      arrows=arrows,arrowstext=arrowstext,tmat=tmat_inner,frequencies=p)
}


#results12=msboxes_R(data=msebmt,id= msebmt$id, yb=c(0.8,0.8,0.8,0.4,0.4,0.4),
#                    xb=c(0.2,0.5,0.8,0.2,0.5,0.8),boxwidth=0.1,boxheight=0.1,
#                    tmat.= tmat, time_study=msebmt$time,vartime=c(seq(0,2000,by=200)),scale=1) 
#


