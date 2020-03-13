CallFAMICKM <- function(dt,lb,index,fs){
  
  dt_train <- dt[index,]
  lb_train <- lb[index]
  
  K <- fs$K
  Mul <- fs$Mul
  lanbda <- fs$lanbda
  stop_thr <- as.double(fs$stop_thr)
  
  K_index <- FAMICKM2(dt_train,lb_train,K,SF_id = NULL,stop_thr,Mul,lanbda)
  
  return(K_index)
}