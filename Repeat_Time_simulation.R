library(conquer)
library(quantreg)
library(MultiRNG)
library(MASS)
library(EnvStats)
library(openxlsx)
library(jmuOutlier)
library(ggplot2)


source("C:/Users/12151/Downloads/CSQR_CSDA/DATA_Start.R")
source("C:/Users/12151/Downloads/CSQR_CSDA/DSQR.R")
source("C:/Users/12151/Downloads/CSQR_CSDA/SmoothingQuantileRegression.R")
source("C:/Users/12151/Downloads/CSQR_CSDA/DREL.R")
source("C:/Users/12151/Downloads/CSQR_CSDA/AVG-DC.R")

num_repetitions <- 20
N_set = c(5000,10000,15000,20000) 
# repetitions
# Initialize the storage 
time_CSQR_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 1)
time_DPQR_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 1)
time_REL_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 1)
time_pool_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 1)
time_avgDC_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 1)

length1 = 4
time_CSQR_simulation_results <- vector("list", length = length1)
time_DPQR_simulation_results <- vector("list", length = length1)
time_REL_simulation_results <- vector("list", length = length1)
time_pool_simulation_results <- vector("list", length = length1)
time_avgDC_simulation_results <- vector("list", length = length1)

for (ind_N in 1:3) {
  N = N_set[ind_N] # samler size
  n = 500
  s = 5
  print(N)
  for (n_re in 1:num_repetitions) {
    
    p = 500 # dimension 
    tau = 0.5
    I = 10 # number of maximal iterations 
    M = N/n
    
    set.seed(n_re)
    
    cov_mat=cov_generate(p,0.5)
    X=cbind(1,draw.d.variate.normal(no.row=N,d=p,mean.vec=rep(0,p),cov.mat=cov_mat))
    beta_true = c(1,1:5, rep(0,p-s))
    
    
    epsilon_t = rnorm(N)
    Y = X%*%beta_true+(1+ 0.4*X[,2])*(epsilon_t-qt(tau,3))
    n_valid = 1000
    valid_X=cbind(1,draw.d.variate.normal(no.row=n_valid, d=p, mean.vec=rep(0,p), cov.mat=cov_mat))
    valid_epsilon = rnorm(n_valid)
    valid_Y = valid_X%*%beta_true+(1+ 0.4*valid_X[,2])*(valid_epsilon-qt(tau,3))
    
    # split_data
    index=sample(c(1:N),N,rep=F)
    ind_1=split_data(N,M,1,index)
    X_1=X[ind_1,]
    Y_1=Y[ind_1]
    
    #local_data_conquer
    local_fit.lasso=conquer.cv.reg(X_1[,2:(p+1)], Y_1, tau = tau, penalty = "lasso")
    beta_0=local_fit.lasso$coeff.min
    l_2_error_0 = norm(beta_0 - beta_true,'2')
    FPR_0 = precision(beta_0,s,p)
    TPR_0 = reacall(beta_0,s,p)
    F1_score_0 = F1_score(beta_0,s,p)
    
    #global and local bandwidth
    c_h <- 0.2
    c_b = 0.53
    h=c_h*(s*log(N)/N)^(1/3)
    b=c_b*(s*log(n)/n)^(1/3)
    
    # lambda_list = c(10^(-(3:9)/4))
    lambda_list = c(10^(-(0:5)/2))
    
    # CSQR_pool: a single machine with pooled data
    lambda_pool = validation_lambda_pooled(lambda_list, beta_0, tau)
    start_time <- Sys.time()
    beta_pool_1 = CSQR_pooled(beta_0, lambda_pool) 
    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
    print(elapsed_time)
    time_poll = elapsed_time
    
    c_h=5
    c_b=0.53
    h=c_h*(s*log(N)/N)^(1/3)
    b=c_b*(s*log(n)/n)^(1/3)
    
    # CSQR
    min_lambda=validation_lambda(lambda_list,beta_0,tau)
    # Initial iteration and time calculation
    start_time <- Sys.time()
    beta_CSQR_1 = CSQR(beta_0,min_lambda)
    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
    print(elapsed_time)
    time_CSQR = elapsed_time
    
    
    # Dis_REL
    h_REL=sqrt(s*log(N)/N)+s^(-0.5)*(0.1*(s^2)*log(N)/n)
    min_lambda_REL = validation_lambda_REL(lambda_list,beta_0,tau,h_REL)
    # Initial iteration and time calculation
    start_time <- Sys.time()
    beta_1_REL = Dis_REL(beta_0,min_lambda_REL,h_REL)
    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
    print(elapsed_time)
    time_REL = elapsed_time
    
    
    # SQR---Smoothing quantile regression for a distributed system
    b_SQR = 4.1*(s*log(n)/n)^(1/3)
    min_lambda_SQR = validation_lambda_SQR(lambda_list,beta_0,tau,b_SQR)
    # Initial iteration and time calculation
    start_time <- Sys.time()
    beta_1_DPQR = SQR(beta_0,min_lambda_SQR,b_SQR)
    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
    print(elapsed_time)
    time_DPQR = elapsed_time
    
    
    #Avg-DC
    beta_tot=c()
    # Time calculation
    start_time <- Sys.time()
    for(i in 1:M){
      ind=split_data(N,M,i,index)
      beta_local=dis_conquer(X,Y,tau,ind)
      beta_tot=cbind(beta_tot,beta_local)
    }
    beta_DC=apply(beta_tot,1,mean)
    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
    time_DC = elapsed_time
    print(elapsed_time)
    
    # storage
    time_pool_seq[n_re] <- time_poll
    time_CSQR_seq[n_re] <- time_CSQR
    time_DPQR_seq[n_re] <- time_DPQR
    time_REL_seq[n_re] <- time_REL
    time_avgDC_seq[n_re] <- time_DC
    
    print(n_re)
  }
  time_CSQR_simulation_results[[ind_N]] <- time_CSQR_seq
  time_pool_simulation_results[[ind_N]] <- time_pool_seq
  time_DPQR_simulation_results[[ind_N]] <- time_DPQR_seq
  time_REL_simulation_results[[ind_N]] <- time_REL_seq
  time_avgDC_simulation_results[[ind_N]] <- time_avgDC_seq
}

ind_N = 1
mean(time_CSQR_simulation_results[[ind_N]])
apply(time_CSQR_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)
mean(time_pool_simulation_results[[ind_N]])
apply(time_pool_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)
mean(time_DPQR_simulation_results[[ind_N]])
apply(time_DPQR_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)
mean(time_REL_simulation_results[[ind_N]])
apply(time_REL_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)
time_avgDC_simulation_results[[ind_N]]
##¡¡Convert minute units to seconds
for (j in 1:num_repetitions) {
  if(time_avgDC_simulation_results[[ind_N]][j] < 5){
    time_avgDC_simulation_results[[ind_N]][j] = time_avgDC_simulation_results[[ind_N]][j] * 60
  }
}
mean(time_avgDC_simulation_results[[ind_N]])
apply(time_avgDC_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)
