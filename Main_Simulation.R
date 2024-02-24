cov_mat=cov_generate(p,0.5)
X=cbind(1,draw.d.variate.normal(no.row=N,d=p,mean.vec=rep(0,p),cov.mat=cov_mat))


beta_true = c(1,1:5, rep(0,p-s+1))
## Homoscedasticity
# epsilon = rnorm(N)
# epsilon = rt(N, 3)
# epsilon = rcauchy(N)
# Y=X%*%beta_true+(epsilon-qnorm(tau))


## Heteroscedasticity
epsilon = rnorm(N)
# epsilon = rt(N, 3
# epsilon = rcauchy(N)
Y = X%*%beta_true+(1+ 0.4*X[,2])*(epsilon-qt(tau,3))

#valid_data
n_valid = 1000
valid_X=cbind(1,draw.d.variate.normal(no.row=n_valid, d=p, mean.vec=rep(0,p), cov.mat=cov_mat))

valid_epsilon = rnorm(n_valid)
#valid_epsilon = rt(n_valid, 3)
#valid_epsilon = rexp(n_valid)
#valid_epsilon = rcauchy(n_valid)
#valid_epsilon = rpareto(n_valid, location = 0.0001)
#valid_epsilon <- rpareto(n_valid, location = location_param, shape = dispersion_param)
#valid_epsilon <- valid_epsilon - 2
#valid_epsilon <- rlaplace(n_valid)

#valid_Y=valid_X%*%beta_true+(valid_epsilon-qnorm(tau))
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

lambda_list = c(10^(-(3:9)/4))
# lambda_list = c(10^(-(0:10)/2))
                
# CSQR_pool: a single machine with pooled data
lambda_pool = validation_lambda_pooled(lambda_list, beta_0, tau)
beta_pool_1 = CSQR_pooled(beta_0, lambda_pool) 


l_2_error_pool = c(l_2_error_0)
l_2_error_pool = c(l_2_error_pool, norm(beta_pool_1 - beta_true,'2'))
FPR_pool=c(FPR_0)
TPR_pool=c(TPR_0)
FPR_pool=c(FPR_pool, precision(beta_pool_1,s,p))
TPR_pool=c(TPR_pool, reacall(beta_pool_1,s,p))
F1_score_pool = c(F1_score_0)
F1_score_pool = c(F1_score_pool, F1_score(beta_pool_1,s,p))
bata_pool=beta_pool_1
for(i in 2:I){
  min_lambda_pool = validation_lambda_pooled(lambda_list,bata_pool,tau)
  beta_pool_i = CSQR_pooled(bata_pool,min_lambda_pool)
  l_2_error_pool = c(l_2_error_pool, norm(beta_pool_i - beta_true,'2'))
  FPR_pool=c(FPR_pool, precision(beta_pool_i,s,p))
  TPR_pool=c(TPR_pool, reacall(beta_pool_i,s,p))
  F1_score_pool = c(F1_score_pool, F1_score(beta_pool_i,s,p))
  bata_pool=beta_pool_i
}

c_h = 5
c_b <- 0.6



b=c_b*(s*log(n)/n)^(1/3)
# CSQR
min_lambda=validation_lambda(lambda_list,beta_0,tau)
# Initial iteration and time calculation
beta_CSQR_1 = CSQR(beta_0,min_lambda)

l_2_error_CSQR = c(l_2_error_0)
l_2_error_CSQR = c(l_2_error_CSQR, norm(beta_CSQR_1-beta_true,'2'))
FPR_CSQR = c(FPR_0)
TPR_CSQR = c(TPR_0)
FPR_CSQR = c(FPR_CSQR, precision(beta_CSQR_1,s,p))
TPR_CSQR = c(TPR_CSQR, reacall(beta_CSQR_1,s,p))
F1_score_CSQR = c(F1_score_0)
F1_score_CSQR = c(F1_score_CSQR, F1_score(beta_CSQR_1,s,p))
bata_CSQR=beta_CSQR_1
for(i in 2:I){
  min_lambda=validation_lambda(lambda_list,bata_CSQR,tau)
  beta_i_CSQR=CSQR(bata_CSQR,min_lambda)
  l_2_error_CSQR = c(l_2_error_CSQR, norm(beta_i_CSQR-beta_true,'2'))
  FPR_CSQR=c(FPR_CSQR, precision(beta_i_CSQR,s,p))
  TPR_CSQR=c(TPR_CSQR, reacall(beta_i_CSQR,s,p))
  F1_score_CSQR = c(F1_score_CSQR, F1_score(beta_i_CSQR,s,p))
  bata_CSQR=beta_i_CSQR
}


# Dis_REL
h_REL=sqrt(s*log(N)/N)+(s^(-0.5))*(0.1*(s^2)*log(N)/n)
min_lambda_REL = validation_lambda_REL(lambda_list,beta_0,tau,h_REL)
# Initial iteration and time calculation
beta_1_REL = Dis_REL(beta_0,min_lambda_REL,h_REL)


l_2_error_REL = c(l_2_error_0)
FPR_REL = c(FPR_0)
TPR_REL = c(TPR_0)
l_2_error_REL = c(l_2_error_REL, norm(beta_1_REL-beta_true,'2'))
FPR_REL = c(FPR_REL, precision(beta_1_REL,s,p))
TPR_REL = c(TPR_REL, reacall(beta_1_REL,s,p))
F1_score_REL = c(F1_score_0)
F1_score_REL = c(F1_score_REL, F1_score(beta_1_REL,s,p))
beta_REL = beta_1_REL

for(i in 2:I){
   h_REL=sqrt(s*log(N)/N)+s^(-0.5)*(0.1*(s^2)*log(N)/n)^((i+1)/2)
   min_lambda_REL=validation_lambda_REL(lambda_list,beta_REL,tau,h_REL)
   beta_i_REL=Dis_REL(beta_REL,min_lambda_REL,h_REL)
   l_2_error_REL=c(l_2_error_REL,norm(beta_i_REL - beta_true,'2'))
   FPR_REL=c(FPR_REL, precision(beta_i_REL,s,p))
   TPR_REL=c(TPR_REL, reacall(beta_i_REL,s,p))
   F1_score_REL = c(F1_score_REL, F1_score(beta_i_REL,s,p))
   beta_REL=beta_i_REL
}


c_h <- 2
h = c_h*(s*log(N)/N)^(1/3)
# SQR---Smoothing quantile regression for a distributed system
b_SQR = 5*(s*log(n)/n)^(1/3)


min_lambda_SQR = validation_lambda_SQR(lambda_list,beta_0,tau,b_SQR)
# Initial iteration and time calculation
beta_1_DPQR = SQR(beta_0,min_lambda_SQR,b_SQR)


l_2_error_DPQR = c(l_2_error_0)
l_2_error_DPQR = c(l_2_error_DPQR, norm(beta_1_DPQR - beta_true,'2'))
FPR_DPQR=c(FPR_0)
TPR_DPQR=c(TPR_0)
FPR_DPQR=c(FPR_DPQR, precision(beta_1_DPQR,s,p))
TPR_DPQR=c(TPR_DPQR, reacall(beta_1_DPQR,s,p))
F1_score_DPQR = c(F1_score_0)
F1_score_DPQR = c(F1_score_DPQR, F1_score(beta_1_DPQR,s,p))
bata_DPQR=beta_1_DPQR
for(i in 2:I){
   min_lambda_DPQR=validation_lambda_SQR(lambda_list,bata_DPQR,tau,b_SQR)
   beta_i_DPQR=SQR(bata_DPQR, min_lambda_DPQR,b_SQR)
   l_2_error_DPQR = c(l_2_error_DPQR, norm(beta_i_DPQR-beta_true,'2'))
   FPR_DPQR=c(FPR_DPQR, precision(beta_i_DPQR,s,p))
   TPR_DPQR=c(TPR_DPQR, reacall(beta_i_DPQR,s,p))
   F1_score_DPQR = c(F1_score_DPQR, F1_score(beta_i_DPQR,s,p))
   bata_DPQR=beta_i_DPQR
}

#Avg-DC
beta_tot=c()

for(i in 1:M){
   ind=split_data(N,M,i,index)
   beta_local=dis_conquer(X,Y,tau,ind)
   beta_tot=cbind(beta_tot,beta_local)
}
beta_DC=apply(beta_tot,1,mean)
l_2_error_avgDC = norm(beta_DC-beta_true,'2')
FPR_DC=precision0(beta_DC,s,p)
TPR_DC=reacall(beta_DC,s,p)
F1_score_DC = F1_score0(beta_DC,s,p)
