MICKM2 <- function(dt,lb,K,SF_id,stop_thr){
  
  data_num <- dim(dt)[1]
  feat_num <- dim(dt)[2]
  # Initialize objective
  Obj <- NULL
  conv <- 0
  iter <- 0
  
  ## Step 1
  # Random select K representative feature
  if(is.null(SF_id)){
    SF_id <- sample(feat_num,K,replace = F)
  }
  # Unrepresentative feature
  CF_id <- seq(from = 1,to = feat_num,by = 1)[-SF_id]
  
  ## Step 2
  # Compare the mutual information in each features with Class
  MI = foreach(m = 1:feat_num,.combine="c",.packages = 'minerva')%dopar%{
    mine(cbind(dt[,m],lb))$MIC[2]
  }
  MI <- t(MI)
  
  while(conv==0){
    iter <- iter+1
    Obj[iter] <- 0
    
    CC <- matrix(0,nrow = 1,ncol = feat_num)
    
    ## Step 3 
    # Grouping the unrepresentative feature in K clusters
    deltaI = foreach(i = CF_id,.combine="rbind",.packages = 'minerva') %:% foreach(j = SF_id,.combine="c",.packages = 'SSKM') %dopar% {
      mine(cbind(dt[,i],dt[,j]))$MIC[2]
    }
    maxMI_arg <- apply(data.frame(deltaI), 1, which.max)
    
    CC[CF_id] <- t(sapply(maxMI_arg,FUN = Change <-function(x) {return(SF_id[x])}))
    CC[SF_id] <- SF_id
    
    ## Step 4  update each cluster C*
    for(j in 1:K){
      SFC_id <- which(CC == SF_id[j])
      max_SFC_MI <- max(MI[SFC_id])
      max_SFC_id <- which.max(MI[SFC_id])
      SF_id[j] <- SFC_id[max_SFC_id]
      CC[SFC_id] <- SFC_id[max_SFC_id]
      Obj[iter] <- Obj[iter] + max_SFC_MI
    }
    
    # Update unrepresentative feature
    CF_id <- seq(from = 1,to = feat_num,by = 1)[-SF_id]
    
    if(iter > 5){
      if(abs(Obj[iter] - Obj[(iter-1)]) < stop_thr){
        conv <- 1
      }
    }
  }
  
  return(sort(SF_id))
}
