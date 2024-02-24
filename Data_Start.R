cov_generate<-function(p,corr){
  R=matrix(rep(0,p*p),ncol=p)
  for(j in 1:(p-1)){
    R[j,(j+1):p]=c(1:(p-j))
  }
  R=R+t(R)
  R=matrix(rep(corr,p*p),ncol=p)**R
  return(R)
}

split_data <-function(n,m,k,ind){
  return (ind[((k-1)*n/m+1):(k*n/m)])
}

K_h<-function(x,h){
  return((2*pi)^(-0.5)*exp(-(x/h)^2/2)/h)
}

quantile_loss<-function(X, Y, beta, tau){
  u = Y - X %*% beta
  return(mean(u*(tau-ifelse(u<=0,1,0))))
}

FPR<-function(beta,s,p){
  FP=length(which(beta>0))-s
  return(FP/(p+1-s))
}

TPR<-function(beta,s,p){
  TP=length(which(beta[1:s]>0))
  return(TP/s)
}

F1_score <- function(beta,s,p){
  TP = length(which(beta[1:s]>0))
  FP = length(which(beta>0.015)) - s
  FN = length(which(beta[1:s] == 0))
  return(2*TP / (2*TP + FP + FN))
}

F1_score0 <- function(beta,s,p){
  TP = length(which(beta[1:s]>0))
  FP = length(which(beta>0)) - s
  FN = length(which(beta[1:s] == 0))
  return(2*TP / (2*TP + FP + FN))
}

precision <- function(beta,s,p){
  TP = length(which(beta[1:s]>0))
  FP = length(which(beta>0.02)) - s
  return(TP / (TP + FP))
}

precision0 <- function(beta,s,p){
  TP = length(which(beta[1:s]>0))
  FP = length(which(beta>0)) - s
  return(TP / (TP + FP))
}

reacall <- function(beta,s,p){
  TP = length(which(beta[1:s]>0))
  FN = length(which(beta[1:s] == 0))
  return(TP / (TP + FN))
}

