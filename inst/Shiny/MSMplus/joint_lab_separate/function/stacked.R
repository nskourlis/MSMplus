
stacked_function<-function(json, data ) {
  
  stacked_list_long=list()
  
  stacked_list=list()
  
  ###Outer loop k is the covariate pattern k- we bring all the stacked states probabilities into a common list- 
  ### that list have now the stackes probabilities matrices for all the covariate patterns
  
  for (k in 1:length(json$cov$atlist)) {
    
    stacked=matrix(nrow=ncol(as.data.frame(json$P[k])),ncol=length(json$P)+2,NA)
    
    ##Stack probabilities for state 1
    pro_stack=list()
    pro_stack[[1]]=data[which(data$state==1),k]
    
    ### Inner loop to bring probabilities same covariate pattern but different states together
    
    for (i in 2:(length(json$P))) {
      pro_stack[[i]]=cbind(pro_stack[[i-1]],data[which(data$state==i),k])
    }
    ### Inner loop to stack those probabilities with same covariate pattern but different states
    
    pro_stack2=as.data.frame(pro_stack[[length(json$P)]])
    
    pstate=vector()
    for (j in 1:length(json$P)) { 
      pstate[j]=paste0("P_state",j)
    }
    
    colnames(pro_stack2)[1:length(json$P)]<-pstate
    
    pro_stack2$timevar=json$timevar
    pro_stack2$cov=as.character(rep(json$cov$atlist[k],nrow(pro_stack2)))
    
    pro_stack3=matrix(nrow=nrow(pro_stack2),ncol=ncol(pro_stack2)-2,NA)
    
    pro_stack3[,1]=pro_stack2[,1]
    
    for (i in 2:(length(json$P))) {
      col=c(1:i)
      pro_stack3[,i]=apply(pro_stack2[,col],1,sum)
    }
    pro_stack3=as.data.frame(pro_stack3)
    colnames(pro_stack3)[1:length(json$P)]<-pstate
    
    pro_stack3$timevar=json$timevar
    
    pro_stack3$cov=as.character(rep(labels_cov()[k],nrow(pro_stack3)))
    
    stacked_list[[k]]=pro_stack3
    
    stacked_list_d =list()
    for (d in 1:length(json$P)) {
      
      stack_wide= as.data.frame(stacked_list[[k]])
      
      stacked_list_d[[d]]=cbind.data.frame(stack_wide[,d],
                                           stack_wide[,ncol( stack_wide)-1],
                                           stack_wide[,ncol( stack_wide)],
                                           rep(d,length( stack_wide[,d])) )
      colnames( stacked_list_d[[d]]) <- c("P","timevar","cov_factor","state")
    }
    
    stack_long <- bind_rows(stacked_list_d, .id = "column_label")
    
    stack_long= as.data.frame(stack_long)
    
    for (o in 1:(length(json$P))) {
      for (g in 1:nrow(stack_long))  {
        if  (stack_long$state[g]==o) {stack_long$state_factor[g]=input[[paste0('state', o)]][1] }  
      }
    }
    
    stacked_list_long[[k]]=stack_long
  }
  stacked_list_long
}