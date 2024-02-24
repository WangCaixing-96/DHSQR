library(conquer)
library(quantreg)
library(MultiRNG)
library(MASS)
library(EnvStats)
library(openxlsx)
library(jmuOutlier)
library(ggplot2)


source("DATA_Start.R")
source("C:/Users/12151/Downloads/CSQR_CSDA/DSQR.R")
source("C:/Users/12151/Downloads/CSQR_CSDA/SmoothingQuantileRegression.R")
source("C:/Users/12151/Downloads/CSQR_CSDA/DREL.R")
source("C:/Users/12151/Downloads/CSQR_CSDA/AVG-DC.R")


num_repetitions <- 50

# Initialize the storage 
l_2_error_CSQR_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
l_2_error_DPQR_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
l_2_error_REL_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
l_2_error_pool_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
l_2_error_avgDC_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 1)

FPR_CSQR_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
FPR_DPQR_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
FPR_REL_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
FPR_pool_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
FPR_avgDC_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 1)

TPR_CSQR_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
TPR_DPQR_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
TPR_REL_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
TPR_pool_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
TPR_avgDC_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 1)

F1_score_CSQR_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
F1_score_DPQR_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
F1_score_REL_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
F1_score_pool_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 10 + 1)
F1_score_avgDC_seq = matrix(0, nrow = as.numeric(num_repetitions), ncol = 1)

N_set = c(5000, 10000, 20000)
n_set = c(200, 500, 1000)

length1 = 3

l_2_error_CSQR_simulation_results <- vector("list", length = length1)
l_2_error_DPQR_simulation_results <- vector("list", length = length1)
l_2_error_REL_simulation_results <- vector("list", length = length1)
l_2_error_pool_simulation_results <- vector("list", length = length1)
l_2_error_avgDC_simulation_results <- vector("list", length = length1)

l_2_error_CSQR_simulation_results_relative <- vector("list", length = length1)
l_2_error_DPQR_simulation_results_relative <- vector("list", length = length1)
l_2_error_REL_simulation_results_relative <- vector("list", length = length1)
l_2_error_pool_simulation_results_relative <- vector("list", length = length1)
l_2_error_avgDC_simulation_results_relative <- vector("list", length = length1)

F1_score_CSQR_simulation_results <- vector("list", length = length1)
F1_score_DPQR_simulation_results <- vector("list", length = length1)
F1_score_REL_simulation_results <- vector("list", length = length1)
F1_score_pool_simulation_results <- vector("list", length = length1)
F1_score_avgDC_simulation_results <- vector("list", length = length1)

FPR_CSQR_simulation_results <- vector("list", length = length1)
FPR_DPQR_simulation_results <- vector("list", length = length1)
FPR_REL_simulation_results <- vector("list", length = length1)
FPR_pool_simulation_results <- vector("list", length = length1)
FPR_avgDC_simulation_results <- vector("list", length = length1)

TPR_CSQR_simulation_results <- vector("list", length = length1)
TPR_DPQR_simulation_results <- vector("list", length = length1)
TPR_REL_simulation_results <- vector("list", length = length1)
TPR_pool_simulation_results <- vector("list", length = length1)
TPR_avgDC_simulation_results <- vector("list", length = length1)

# repetitions
for (ind_N in 1:3) {
  N = N_set[ind_N] # Total samler size
  n = 200          # Local samler size
  s = 6
  print(N)
  for (n_re in 1:num_repetitions) {
    p = 500 # dimension 
    tau = 0.5
    M = N/n
    I = 10
    
    set.seed(n_re)
    
    source("C:/Users/12151/Downloads/CSQR_CSDA/Main_Simulation.R")
    
    # storage
    l_2_error_pool_seq[n_re, ] <- l_2_error_pool
    l_2_error_CSQR_seq[n_re, ] <- l_2_error_CSQR
    l_2_error_DPQR_seq[n_re, ] <- l_2_error_DPQR
    l_2_error_REL_seq[n_re, ] <- l_2_error_REL
    l_2_error_avgDC_seq[n_re] <- l_2_error_avgDC
    
    FPR_CSQR_seq[n_re, ] <- FPR_CSQR
    FPR_DPQR_seq[n_re, ] <- FPR_DPQR
    FPR_REL_seq[n_re, ] <- FPR_REL
    FPR_pool_seq[n_re, ] <- FPR_pool
    FPR_avgDC_seq[n_re] <- FPR_DC
    
    TPR_CSQR_seq[n_re, ] <- TPR_CSQR
    TPR_DPQR_seq[n_re, ] <- TPR_DPQR
    TPR_REL_seq[n_re, ] <- TPR_REL
    TPR_pool_seq[n_re, ] <- TPR_pool
    TPR_avgDC_seq[n_re] <- TPR_DC
    
    F1_score_CSQR_seq[n_re, ] <- F1_score_CSQR
    F1_score_DPQR_seq[n_re, ] <- F1_score_DPQR
    F1_score_REL_seq[n_re, ] <- F1_score_REL
    F1_score_pool_seq[n_re, ] <- F1_score_pool
    F1_score_avgDC_seq[n_re] <- F1_score_DC
    
    print(n_re)
  }
  
  l_2_error_CSQR_simulation_results[[ind_N]] <- l_2_error_CSQR_seq
  l_2_error_DPQR_simulation_results[[ind_N]] <- l_2_error_DPQR_seq
  l_2_error_REL_simulation_results[[ind_N]] <- l_2_error_REL_seq
  l_2_error_pool_simulation_results[[ind_N]] <- l_2_error_pool_seq
  l_2_error_avgDC_simulation_results[[ind_N]] <- l_2_error_avgDC_seq
  
  l_2_error_CSQR_simulation_results_relative[[ind_N]] <- 100*l_2_error_CSQR_seq/norm(beta_true,'2')
  l_2_error_DPQR_simulation_results_relative[[ind_N]] <- 100*l_2_error_DPQR_seq/norm(beta_true,'2')
  l_2_error_REL_simulation_results_relative[[ind_N]] <- 100*l_2_error_REL_seq/norm(beta_true,'2')
  l_2_error_pool_simulation_results_relative[[ind_N]] <- 100*l_2_error_pool_seq/norm(beta_true,'2')
  l_2_error_avgDC_simulation_results_relative[[ind_N]] <- 100*l_2_error_avgDC_seq/norm(beta_true,'2')
  
  TPR_CSQR_simulation_results[[ind_N]] <- TPR_CSQR_seq
  TPR_DPQR_simulation_results[[ind_N]] <- TPR_DPQR_seq
  TPR_REL_simulation_results[[ind_N]] <- TPR_REL_seq
  TPR_pool_simulation_results[[ind_N]] <- TPR_pool_seq
  TPR_avgDC_simulation_results[[ind_N]] <- TPR_avgDC_seq
  
  FPR_CSQR_simulation_results[[ind_N]] <- FPR_CSQR_seq
  FPR_DPQR_simulation_results[[ind_N]] <- FPR_DPQR_seq
  FPR_REL_simulation_results[[ind_N]] <- FPR_REL_seq
  FPR_pool_simulation_results[[ind_N]] <- FPR_pool_seq
  FPR_avgDC_simulation_results[[ind_N]] <- FPR_avgDC_seq
  
  F1_score_CSQR_simulation_results[[ind_N]] <- F1_score_CSQR_seq
  F1_score_DPQR_simulation_results[[ind_N]] <- F1_score_DPQR_seq
  F1_score_REL_simulation_results[[ind_N]] <- F1_score_REL_seq
  F1_score_pool_simulation_results[[ind_N]] <- F1_score_pool_seq
  F1_score_avgDC_simulation_results[[ind_N]] <- F1_score_avgDC_seq
}    
  
# Read data according to the index, keeping three decimal places
ind_N = 1
# l_2 error
y1 = colMeans(l_2_error_CSQR_simulation_results_relative[[ind_N]])[I+1]
sd1 = apply(l_2_error_CSQR_simulation_results_relative[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y2 = colMeans(l_2_error_pool_simulation_results_relative[[ind_N]])[I+1]
sd2 = apply(l_2_error_pool_simulation_results_relative[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y3 = colMeans(l_2_error_DPQR_simulation_results_relative[[ind_N]])[I+1]
sd3 = apply(l_2_error_DPQR_simulation_results_relative[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y4 = colMeans(l_2_error_REL_simulation_results_relative[[ind_N]])[I+1]
sd4 = apply(l_2_error_REL_simulation_results_relative[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y5 = colMeans(l_2_error_avgDC_simulation_results_relative[[ind_N]])
sd5 = apply(l_2_error_avgDC_simulation_results_relative[[ind_N]], MARGIN = 2, FUN = sd)
data_tableL2 <- data.frame(y1,sd1,y2,sd2,y3,sd3,y4,sd4,y5,sd5)
data_tableL2 <- round(data_tableL2, 3)
TableresultL2 <- paste(data_tableL2$y1, "(", data_tableL2$sd1, ")", 
                       " ", data_tableL2$y2, "(", data_tableL2$sd2, ")",
                       " ", data_tableL2$y3, "(", data_tableL2$sd3, ")",
                       " ", data_tableL2$y4, "(", data_tableL2$sd4, ")",
                       " ", data_tableL2$y5, "(", data_tableL2$sd5, ")",
                       sep = "")
print(TableresultL2)
# F_1 score 
y1 = colMeans(F1_score_CSQR_simulation_results[[ind_N]])[I+1]
sd1 = apply(F1_score_CSQR_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y2 = colMeans(F1_score_pool_simulation_results[[ind_N]])[I+1]
sd2 = apply(F1_score_pool_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y3 = colMeans(F1_score_DPQR_simulation_results[[ind_N]])[I+1]
sd3 = apply(F1_score_DPQR_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y4 = colMeans(F1_score_REL_simulation_results[[ind_N]])[I+1]
sd4 = apply(F1_score_REL_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y5 = colMeans(F1_score_avgDC_simulation_results[[ind_N]])
sd5 = apply(F1_score_avgDC_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)
data_tableF1 <- data.frame(y1,sd1,y2,sd2,y3,sd3,y4,sd4,y5,sd5)
data_tableF1 <- round(data_tableF1, 3)
TableresultF1 <- paste(data_tableF1$y1, "(", data_tableF1$sd1, ")", 
                     " ", data_tableF1$y2, "(", data_tableF1$sd2, ")",
                     " ", data_tableF1$y3, "(", data_tableF1$sd3, ")",
                     " ", data_tableF1$y4, "(", data_tableF1$sd4, ")",
                     " ", data_tableF1$y5, "(", data_tableF1$sd5, ")",
                     sep = "")
print(TableresultF1)
# Rrecision
FPR_CSQR_simulation_results[[ind_N]] <- FPR_CSQR_simulation_results[[ind_N]][!is.infinite(rowSums(FPR_CSQR_simulation_results[[ind_N]])),]
y1 = colMeans(FPR_CSQR_simulation_results[[ind_N]])[I+1]
sd1 = apply(FPR_CSQR_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y2 = colMeans(FPR_pool_simulation_results[[ind_N]])[I+1]
sd2 = apply(FPR_pool_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y3 = colMeans(FPR_DPQR_simulation_results[[ind_N]])[I+1]
sd3 = apply(FPR_DPQR_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y4 = colMeans(FPR_REL_simulation_results[[ind_N]])[I+1]
sd4 = apply(FPR_REL_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y5 = colMeans(FPR_avgDC_simulation_results[[ind_N]])
sd5 = apply(FPR_avgDC_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)
data_tableFPR <- data.frame(y1,sd1,y2,sd2,y3,sd3,y4,sd4,y5,sd5)
data_tableFPR <- round(data_tableFPR, 3)
TableresultFPR <- paste(data_tableFPR$y1, "(", data_tableFPR$sd1, ")", 
                     " ", data_tableFPR$y2, "(", data_tableFPR$sd2, ")",
                     " ", data_tableFPR$y3, "(", data_tableFPR$sd3, ")",
                     " ", data_tableFPR$y4, "(", data_tableFPR$sd4, ")",
                     " ", data_tableFPR$y5, "(", data_tableFPR$sd5, ")",
                     sep = "")
print(TableresultFPR)
# Recall
y1 = colMeans(TPR_CSQR_simulation_results[[ind_N]])[I+1]
sd1 = apply(TPR_CSQR_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y2 = colMeans(TPR_pool_simulation_results[[ind_N]])[I+1]
sd2 = apply(TPR_pool_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y3 = colMeans(TPR_DPQR_simulation_results[[ind_N]])[I+1]
sd3 = apply(TPR_DPQR_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y4 = colMeans(TPR_REL_simulation_results[[ind_N]])[I+1]
sd4 = apply(TPR_REL_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)[I+1]
y5 = colMeans(TPR_avgDC_simulation_results[[ind_N]])
sd5 = apply(TPR_avgDC_simulation_results[[ind_N]], MARGIN = 2, FUN = sd)
data_tableTPR <- data.frame(y1,sd1,y2,sd2,y3,sd3,y4,sd4,y5,sd5)
data_tableTPR <- round(data_tableTPR, 3)
TableresultTPR <- paste(data_tableTPR$y1, "(", data_tableTPR$sd1, ")", 
                        " ", data_tableTPR$y2, "(", data_tableTPR$sd2, ")",
                        " ", data_tableTPR$y3, "(", data_tableTPR$sd3, ")",
                        " ", data_tableTPR$y4, "(", data_tableTPR$sd4, ")",
                        " ", data_tableTPR$y5, "(", data_tableTPR$sd5, ")",
                        sep = "")
print(TableresultTPR)


## Save raw data
excel_file <- "l_2_error_50(tau0.5)_normal(N=_,n=_).xlsx"
wb <- createWorkbook()
addWorksheet(wb, "pool")
writeData(wb, sheet = 1, l_2_error_pool_simulation_results[[ind_N]])
addWorksheet(wb, "CSQR")
writeData(wb, sheet = 2, l_2_error_CSQR_simulation_results[[ind_N]])
addWorksheet(wb, "DPQR")
writeData(wb, sheet = 3, l_2_error_DPQR_simulation_results[[ind_N]])
addWorksheet(wb, "REL")
writeData(wb, sheet = 4, l_2_error_REL_simulation_results[[ind_N]])
addWorksheet(wb, "avgDC")
writeData(wb, sheet = 5, l_2_error_avgDC_simulation_results[[ind_N]])
saveWorkbook(wb, excel_file)

excel_file <- "Precision_50(tau0.5)_normal(N=_,n=_).xlsx"
wb <- createWorkbook()
addWorksheet(wb, "pool")
writeData(wb, sheet = 1, FPR_pool_simulation_results[[ind_N]])
addWorksheet(wb, "CSQR")
writeData(wb, sheet = 2, FPR_CSQR_simulation_results[[ind_N]])
addWorksheet(wb, "DPQR")
writeData(wb, sheet = 3, FPR_DPQR_simulation_results[[ind_N]])
addWorksheet(wb, "REL")
writeData(wb, sheet = 4, FPR_REL_simulation_results[[ind_N]])
addWorksheet(wb, "avgDC")
writeData(wb, sheet = 5, FPR_avgDC_simulation_results[[ind_N]])
saveWorkbook(wb, excel_file)

excel_file <- "Recall_50(tau0.5)_normal(N= ).xlsx"
wb <- createWorkbook()
addWorksheet(wb, "pool")
writeData(wb, sheet = 1, TPR_pool_simulation_results[[ind_N]])
addWorksheet(wb, "CSQR")
writeData(wb, sheet = 2, TPR_CSQR_simulation_results[[ind_N]])
addWorksheet(wb, "DPQR")
writeData(wb, sheet = 3, TPR_DPQR_simulation_results[[ind_N]])
addWorksheet(wb, "REL")
writeData(wb, sheet = 4, TPR_REL_simulation_results[[ind_N]])
addWorksheet(wb, "avgDC")
writeData(wb, sheet = 5, TPR_avgDC_simulation_results[[ind_N]])
saveWorkbook(wb, excel_file)

excel_file <- "F1_score_50(tau0.5)_normal(N= ).xlsx"
wb <- createWorkbook()
addWorksheet(wb, "pool")
writeData(wb, sheet = 1, F1_score_pool_simulation_results[[ind_N]])
addWorksheet(wb, "CSQR")
writeData(wb, sheet = 2, F1_score_CSQR_simulation_results[[ind_N]])
addWorksheet(wb, "DPQR")
writeData(wb, sheet = 3, F1_score_DPQR_simulation_results[[ind_N]])
addWorksheet(wb, "REL")
writeData(wb, sheet = 4, F1_score_REL_simulation_results[[ind_N]])
addWorksheet(wb, "avgDC")
writeData(wb, sheet = 5, F1_score_avgDC_simulation_results[[ind_N]])
saveWorkbook(wb, excel_file)

