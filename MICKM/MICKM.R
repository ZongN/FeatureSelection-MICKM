library(minerva)

############# FOR TEST #############
library(mlbench)
data(Sonar)

# dt <- Sonar[,1:60]
# cl <- Sonar[,61]

##For test
#c(26,43,59,34,56,42,7,32,4,18)

dt <- Sonar[,c(12,49,33,36,58)]
cl <- Sonar[,61]

MICKM_SM <- mine(dt,alpha = 0.85)$MIC
K <- 1
stop_thr <- 0.0000001

MICKM(dt,MICKM_SM,2,0.0000001)

# Pressure test
re <- NULL
for(i in 1:100){
  RF <- MICKM(dt,MICKM_SM,5,0.0000001)
  #RF <- SSKM(dt,SSKM_SM,SSKM_MI,2,SF_id = NULL,stop_thr)
  
  re <- c(re,RF)
  cat("R: ",i," F: ",RF,"\n")
}
####################################
MICKM <- function(dt,SM = NULL,K,stop_thr){
  
  #####LOG
  # Creat the log file
  cat(" Features : ",colnames(dt),"\n",
      "K=",K,"\n",
      "------------------------------------------------------------------------- \n",
      file = "Log.txt",append = T)
  
  #Similarity Matrix
  if(is.null(SM)){
    SM <- mine(dt)$MIC
  }

  feat_num <- length(dt)
  
  # Initialize objective
  Obj <- NULL
  # Randomly select K features as SF_id
  SF_id <- sample(length(dt),K,replace = F)
  
  #####LOG
  cat("Random Pick:",SF_id,"\n",file = "Log.txt",append = T)
  
  # Unrepresentative feature
  CF_id <- c(1:feat_num)[-SF_id]
  
  conv <- 0
  iter <- 0
  
  while(conv == 0){
    iter <- iter+1
    Obj[iter] <- 0
    
    CC <- matrix(0,nrow = 1,ncol = feat_num)
    
    maxMI <- t(apply(SM[SF_id,CF_id], 2, max)) # Get the max MIC feature
    maxMI_arg <- t(apply(SM[SF_id,CF_id], 2, which.max)) # Get the max MI feature address
    CC[CF_id] <- t(sapply(maxMI_arg,FUN = Change <-function(x) {return(SF_id[x])})) # Change the feature address to feature number
    CC[SF_id] <- SF_id
    
    # Update the SF* in each K cluster
    for(j in 1:K){
      
      SFC_id <- which(CC == SF_id[j])
      
      if(length(SFC_id) > 1){
        #One cluster features similarity matrix
        max_SFC_id <- SFC_id[which.max(apply(SM[SFC_id,SFC_id],2,sum))]
        max_SFC_Value <- max(apply(SM[SFC_id,SFC_id],2,sum))
      }else{
        max_SFC_id <- SFC_id
        max_SFC_Value <- as.numeric(SM[SFC_id,SFC_id])
      }
      
      #####LOG
      cat("Cluster [",SF_id[j],"] ",SFC_id,"\n","Update Medoide [",max_SFC_id,"] \n",file = "Log.txt",append = T)
      
      #Update the Medoide
      SF_id[j] <- max_SFC_id
      CC[SFC_id] <- max_SFC_id
      
      Obj[iter] <- Obj[iter] + max_SFC_Value
    }
    
    # Update unrepresentative feature
    CF_id <- c(1:feat_num)[-SF_id]
    
    if(iter > 1){
      if(abs(Obj[iter] - Obj[(iter-1)]) < stop_thr){
        conv <- 1
      }
    }
    
    #####LOG
    cat("--------------------",iter,"-------------------- \n",file = "Log.txt",append = T)
    
  }
  return(sort(SF_id))
}
