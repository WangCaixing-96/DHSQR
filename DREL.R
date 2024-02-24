validation_lambda_REL <- function(lambda_list, beta, tau, h)
{
  error_list=c()
  for (idx in 1: length(lambda_list))
  {
    lambda = lambda_list[idx]
    
    
    y_=X%*%beta
    f_0_hat=1/(N*h)*sum(dnorm((Y-y_)/h))
    y_bar=y_-(ifelse(Y-y_<=0,1,0)-tau)/f_0_hat
    
    #calculate A and a
    A=(t(X_1)%*%X_1)/n
    a=-t(X)%*%y_bar/N-((t(X_1)%*%X_1)/n-(t(X)%*%X)/N)%*%beta
    
    # Solve for regression parameters with coordinate descent with Training set
    regress_result = coord_descent(A=0.5*A,B=a,c=0,lambda=lambda,init_x0=as.matrix(beta_0),plt = 0)
    # Fitted parameters
    beta_est = regress_result$min_point
    
    # check loss on valid data
    error_list[idx] = quantile_loss(valid_X,valid_Y,beta_est,tau)
    
  }
  
  min_lambda = lambda_list[which.min(error_list)]
  return(min_lambda)
}

Dis_REL<-function(beta,lambda,h){
  
  y_=X%*%beta
  f_0_hat=1/(N*h)*sum(dnorm((Y-y_)/h))
  y_bar=y_-(ifelse(Y-y_<=0,1,0)-tau)/f_0_hat
  
  #calculate A and a
  A=(t(X_1)%*%X_1)/n
  a= -t(X)%*%y_bar/N - ((t(X_1)%*%X_1)/n-(t(X)%*%X)/N)%*%beta
  
  #optimization
  result=coord_descent(A=0.5*A, B=a, c=0, lambda=lambda, init_x0=as.matrix(beta_0), plt = 0)
  beta_est=result$min_point
  return(beta_est)
}