compute_y <- function(A, B, c, lambda, x)
{
  y = t(x) %*% A %*% x + t(B) %*% x + c + lambda * norm(x, '1')
  return(y)
}

coord_descent <- function(A, B, c, lambda, init_x0,
                          max_iter = 1000, max_dist = 10^-5, plt = 0)
{
  library(shape)
  # #Check if A is positive definite
  # eig_A <- eigen(A)$values
  # 
  # for (idx in eig_A)
  # {
  #   if (idx <= 0)
  #   {
  #     print('Input matrix A is not positive definite!')
  #     return()
  #   }
  # }
  nrow = dim(A)[1]
  ncol = dim(A)[2]
  #lambda_save = lambda
  prev_y = compute_y(A, B, c, lambda, init_x0)
  x = init_x0
  iter_num = 0
  distance_list = list() # Check if distance to converging to 0
  while (T){
    if (plt == 1) # Draw starting point if plt == 1
    {
      plot(x[1],x[2], xlim = range(c(-3,3)), ylim = range(c(-3,3)))
      start_x_arrow = x[1]
      start_y_arrow = x[2]
      par(new = TRUE)
    }
    
    for (idx in 1:nrow)
    {
      a = x
      a[idx] = 0
      val = B[idx] + (A[idx,] + A[,idx]) %*% a
      
      #  if (is.null(val) || is.na(val) || length(val) == 0) {
      #    val <- 0
      #  }
      #  # 检查是否为空或包含缺失值
      #  if (is.null(lambda) || is.na(lambda) || length(lambda) == 0) {
      #    lambda <- lambda_save
      #  }
      # print(val)
      # print(lambda)
      
      if (val  < -lambda)
      {
        x[idx] = (-lambda - val) / (A[idx,] + A[,idx])[idx]
      }
      else if (val > lambda)
      {
        x[idx] = (lambda - val) / (A[idx,] + A[,idx])[idx]
      }
      else if ((-lambda <= val) & (val <= lambda))
      {
        x[idx] = 0
      }
      if (plt == 1) # Draw plot if plt == 1
      {
        # Draw the solution path for illustration
        plot(x[1],x[2], xlim = range(c(-3,3)), ylim = range(c(-3,3)))
        par(new = TRUE)
        stop_x_arrow = x[1]
        stop_y_arrow = x[2]
        Arrows(start_x_arrow, start_y_arrow, x1 = stop_x_arrow, 
               y1 = stop_y_arrow, col = 'red', arr.type = 'triangle',
               arr.width = 0.1, arr.length = 0.15)
        start_x_arrow = stop_x_arrow
        start_y_arrow = stop_y_arrow
        par(new = TRUE)
      }
    }
    
    y = compute_y(A,B,c, lambda, x)
    distance = norm((y-prev_y))
    if (is.na(distance)) {
      distance <- 1
    }
    distance_list = append(distance_list, distance)
    prev_y = y
    
    if ((distance <= max_dist) || (iter_num == max_iter))
    {
      result_list = list('min_point' = x, 'Optimize value' = y, 'distance' = distance_list)
      if (plt == 1)
      {
        par(new = F)
        iteration = seq(from = 1, to = length(distance_list), by = 1)
        plot(iteration, distance_list)
      }
      
      return (result_list)
      break
    }
    
    iter_num = iter_num + 1
    
  }
}


validation_lambda <- function(lambda_list, beta, tau)
{
  error_list=c()
  for (idx in 1: length(lambda_list)){
    lambda = lambda_list[idx]
  # idx = 3
  # beta = beta_0
    #global and local residual
    res=Y-X%*%beta
    res_1=Y_1-X_1%*%beta
    
    #calculate A and a
    A=t(X_1)%*%(X_1*c(K_h(res_1,b)))/n
    a=t(X)%*%(ifelse(res<=0,1,0)-tau)/N-A%*%beta
    
    # Solve for regression parameters with coordinate descent with Training set
     regress_result = coord_descent(A=0.5*A,B = a,c=0,lambda=lambda, init_x0=as.matrix(beta), plt = 0)
    # Fitted parameters
     beta_est = regress_result$min_point
    
    # beta_est = Fista(A= A, B = a, lambda = lambda, init_x0 = as.matrix(beta), max_iter = 2000, max_dist = 10^-3)
    
    # check loss on valid data
    error_list[idx] = quantile_loss(valid_X,valid_Y,beta_est,tau)
    
  }
  
  min_lambda = lambda_list[which.min(error_list)]
  
  #print(min_lambda)
  return(min_lambda)
}

CSQR<-function(beta,lambda){
  #global and local residual
  res=Y-X%*%beta
  res_1=Y_1-X_1%*%beta
  
  #calculate A and a
  A=t(X_1)%*%(X_1*c(K_h(res_1,b)))/n
  a=t(X)%*%(ifelse(res<=0,1,0)-tau)/N-A%*%beta
  
  #optimization
  result=coord_descent(A=0.5*A,B = a,c=0,lambda=lambda,init_x0=as.matrix(beta),plt = 0)
  beta_est=result$min_point
  
  #beta_est = Fista(A= A, B = a, lambda = lambda, init_x0 = as.matrix(beta), max_iter = 2000, max_dist = 10^-3)
  
  
  return(beta_est)
}

validation_lambda_pooled <- function(lambda_list, beta, tau)
{
  error_list=c()
  for (idx in 1: length(lambda_list))
  {
    lambda = lambda_list[idx]
    
    
    #global residual
    res=Y-X%*%beta
    
    #calculate A and a
    A=t(X)%*%(X*c(K_h(res,h)))/N
    a=t(X)%*%(ifelse(res<=0,1,0)-tau)/N - A%*%beta
    
    # Solve for regression parameters with coordinate descent with Training set
    result=coord_descent(A=0.5*A,B=a,c=0,lambda=lambda,init_x0=as.matrix(beta),plt = 0)
    beta_est=result$min_point
    
    # check loss on valid data
    #error_list[idx] = norm(beta_est-beta_true,'2')
    error_list[idx] = quantile_loss(valid_X,valid_Y,beta_est,tau)
    
  }
  
  min_lambda = lambda_list[which.min(error_list)]
  #print(error_list)
  return(min_lambda)
}

CSQR_pooled<-function(beta,lambda){
  #global residual
  res=Y-X%*%beta
  
  
  #calculate A and a
  A=t(X)%*%(X*c(K_h(res,h)))/N
  a=t(X)%*%(ifelse(res<=0,1,0)-tau)/N - A%*%beta
  
  #optimization
  result=coord_descent(A=0.5*A,B=a,c=0,lambda=lambda,init_x0=as.matrix(beta),plt = 0)
  beta_est=result$min_point
  return(beta_est)
}
