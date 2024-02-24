dis_conquer<-function(X,Y,tau,ind){
  X_i=X[ind,]
  Y_i=Y[ind]
  result_i=conquer.cv.reg(X_i[,2:(p+1)], Y_i, tau = tau, penalty = "lasso")
  beta_i=result_i$coeff.min
  return(beta_i)
}