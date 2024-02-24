library(conquer)
library(quantreg)
library(MultiRNG)
library(MASS)
library(openxlsx)
library(ggplot2)

source("C:/Users/12151/Downloads/CSQR_CSDA/DATA_Start.R")
source("C:/Users/12151/Downloads/CSQR_CSDA/DSQR.R")

I = 10
num_repetitions <- 50
N_set = c(5000, 10000 ,20000)
n_set = c(200, 500 ,1000)
c_bset <- c(1,2,5,10)
c_hset <- c(1,2,5,10)
list_n = 16

l_2_error_CSQR_simulation_results <- vector("list", length = list_n)
F1_score_CSQR_simulation_results <- vector("list", length = list_n)
precision_CSQR_simulation_results <- vector("list", length = list_n)
recall_CSQR_simulation_results <- vector("list", length = list_n)

l_2_error_CSQR_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = I + 1)
F1_score_CSQR_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = I + 1)
precision_CSQR_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = I + 1)
recall_CSQR_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = I + 1)

for (j1 in 1:4) {
  for (j2 in 1:4) {
  for (n_re in 1:num_repetitions) {
    set.seed(n_re)
    N=20000# samler size
    p=500 # dimension 
    tau=0.5
    s=6 
    
    cov_mat=cov_generate(p,0.5)
    X=cbind(1,draw.d.variate.normal(no.row=N,d=p,mean.vec=rep(0,p),cov.mat=cov_mat))
    beta_true=c(1,1,2,3,4,5,rep(0,p-s+1))
    
    epsilon_t = rnorm(N)
    #Y = X%*%beta_true+(1+ 0.4*X[,2])*(epsilon_t-qt(tau,3))
    Y = X%*%beta_true+(epsilon_t-qnorm(tau))
    
    n_valid = 1000
    valid_X=cbind(1,draw.d.variate.normal(no.row=n_valid, d=p, mean.vec=rep(0,p), cov.mat=cov_mat))
    valid_epsilon = rnorm(n_valid)
    # valid_Y = valid_X%*%beta_true+(1+ 0.4*valid_X[,2])*(valid_epsilon-qt(tau,3))
    valid_Y = valid_X%*%beta_true+(valid_epsilon-qnorm(tau))
    
    # split_data
    n=500
    M=N/n
    index=sample(c(1:N),N,rep=F)
    ind_1=split_data(N,M,1,index)
    X_1=X[ind_1,]
    Y_1=Y[ind_1]
    
    #local_data_conquer
    local_fit.lasso=conquer.cv.reg(X_1[,2:(p+1)], Y_1, tau = tau, penalty = "lasso")
    beta_0=local_fit.lasso$coeff.min
    l_2_error_0 = norm(beta_0 - beta_true,'2')
    F1_score_0 = F1_score(beta_0,s,p)
    precision_0 = precision(beta_0,s,p) 
    recall_0 = reacall(beta_0,s,p) 
    
    lambda_list = c(10^(-(3:9)/4))
    
    c_h = c_hset[j1]
    c_b = c_bset[j2]
    h=c_h*(s*log(N)/N)^(1/3)
    b=c_b*(s*log(n)/n)^(1/3)
    
    # CSQR
    min_lambda=validation_lambda(lambda_list,beta_0,tau)
    # Initial iteration and time calculation
    #start_time <- Sys.time()
    beta_CSQR_1 = CSQR(beta_0,min_lambda)
    #end_time <- Sys.time()
    #elapsed_time <- end_time - start_time
    #print(elapsed_time)
    
    l_2_error_CSQR = c(l_2_error_0)
    l_2_error_CSQR = c(l_2_error_CSQR, norm(beta_CSQR_1-beta_true,'2'))
    F1_score_CSQR = c(F1_score_0)
    F1_score_CSQR = c(F1_score_CSQR, F1_score(beta_CSQR_1,s,p))
    precision_CSQR = c(precision_0)
    precision_CSQR = c(precision_CSQR, precision(beta_CSQR_1,s,p))
    recall_CSQR = c(recall_0)
    recall_CSQR = c(recall_CSQR, reacall(beta_CSQR_1,s,p))
    bata_CSQR=beta_CSQR_1
    for(i in 2:I){
      min_lambda=validation_lambda(lambda_list,bata_CSQR,tau)
      beta_i_CSQR=CSQR(bata_CSQR,min_lambda)
      l_2_error_CSQR = c(l_2_error_CSQR, norm(beta_i_CSQR-beta_true,'2'))
      F1_score_CSQR = c(F1_score_CSQR, F1_score(beta_i_CSQR,s,p))
      precision_CSQR = c(precision_CSQR, precision(beta_i_CSQR,s,p))
      recall_CSQR = c(recall_CSQR, reacall(beta_i_CSQR,s,p))
      bata_CSQR=beta_i_CSQR
    }
  
    print(c_h)
    print(c_b)
    print(n_re)
    cat("L2 CSQR is: ", l_2_error_CSQR[I+1], "\n\n")
    # cat("F1 CSQR is: ", F1_score_CSQR[I+1], "\n\n")
    # storage
    
    l_2_error_CSQR_seq[n_re, ] <- l_2_error_CSQR
    F1_score_CSQR_seq[n_re, ] <- F1_score_CSQR
    precision_CSQR_seq[n_re, ] <- precision_CSQR
    recall_CSQR_seq[n_re, ] <- recall_CSQR
  }
  }
  l_2_error_CSQR_simulation_results[[j]] <- l_2_error_CSQR_seq
  F1_score_CSQR_simulation_results[[j]] <- F1_score_CSQR_seq
  precision_CSQR_simulation_results[[j]] <- precision_CSQR_seq
  recall_CSQR_simulation_results[[j]] <- recall_CSQR_seq
}

IND = 3
data_table <- data.frame(
  y1 = colMeans(l_2_error_CSQR_simulation_results[[IND]])[I+1],
  sd1 = sd(l_2_error_CSQR_simulation_results[[IND]][ , I+1]),
  y2 = colMeans(F1_score_CSQR_simulation_results[[IND]])[I+1],
  sd2 = sd(F1_score_CSQR_simulation_results[[IND]][ , I+1])
)
data_table <- round(data_table, 3)
Tableresult <- paste(data_table$y1, "(", data_table$sd1, ")", 
                     " ", data_table$y2, "(", data_table$sd2, ")",
                     sep = "")
print(Tableresult)


