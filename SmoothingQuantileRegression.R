validation_lambda_SQR <- function(lambda_list, beta, tau, b_SQR)
{
  error_list=c()
  for (idx in 1: length(lambda_list))
  {
    lambda = lambda_list[idx]
    
    
    #global and local residual
    res=Y-X%*%beta
    res_1=Y_1-X_1%*%beta
    
    #calculate A and a
    A=t(X_1)%*%(X_1*c(K_h(-res_1,b_SQR)))/n
    a=t(X)%*%(pnorm(-res/h) - tau)/N - t(X_1)%*%(X_1*c(K_h(-res_1,b_SQR)))%*%beta / n
    
    # Solve for regression parameters with coordinate descent with Training set
     regress_result = coord_descent(A=0.5*A,B=a,c=0,lambda=lambda,init_x0=as.matrix(beta),plt = 0)
    # Fitted parameters
     beta_est = regress_result$min_point
    
    
    #beta_est = Fista(A= A, B = a, lambda = lambda, init_x0 = as.matrix(beta), max_iter = 2000, max_dist = 10^-3)
    
    # check loss on valid data
    error_list[idx] = quantile_loss(valid_X,valid_Y,beta_est,tau)
    
  }
  
  min_lambda = lambda_list[which.min(error_list)]
  return(min_lambda)
}

SQR<-function(beta,lambda,b_SQR){
  #global and local residual
  res=Y-X%*%beta
  res_1=Y_1-X_1%*%beta
  
  #calculate A and a
  A=t(X_1)%*%(X_1*c(K_h(-res_1,b_SQR)))/n
  a=t(X)%*%(pnorm(-res/h) - tau)/N - t(X_1)%*%(X_1*c(K_h(-res_1,b_SQR)))%*%beta / n
  
  #optimization
  result=coord_descent(A=0.5*A,B=a,c=0,lambda=lambda,init_x0=as.matrix(beta),plt = 0)
  beta_est=result$min_point
 
  #beta_est = Fista(A= A, B = a, lambda = lambda, init_x0 = as.matrix(beta), max_iter = 2000, max_dist = 10^-3)
  
  return(beta_est)
}
