library(openxlsx)
library(ggplot2)
library(latex2exp)


excel_file <- "C:/Users/12151/Downloads/CSQR_CSDA/同方差tau0.5/l_2_error_50(tau0.5)Normal.xlsx"
excel_file <- "C:/Users/12151/Downloads/CSQR_CSDA/同方差tau0.5/l_2_error_50(tau0.5)_cauchy.xlsx"
excel_file <- "C:/Users/12151/Downloads/CSQR_CSDA/同方差tau0.5/l_2_error_50(tau0.5)_t3.xlsx"
excel_file <- "C:/Users/12151/Downloads/CSQR_CSDA/异方差tau0.5/l_2_error_50(tau0.5)Normal.xlsx"
excel_file <- "C:/Users/12151/Downloads/CSQR_CSDA/异方差tau0.5/l_2_error_50(tau0.5)_cauchy.xlsx"
excel_file <- "C:/Users/12151/Downloads/CSQR_CSDA/异方差tau0.5/l_2_error_50(tau0.5)t3.xlsx"


sheets <- getSheetNames(excel_file)
matrix_list <- list()

for (sheet in sheets) {
  current_matrix <- read.xlsx(excel_file, sheet = sheet, startRow = 1)
  matrix_list[[sheet]] <- current_matrix
}

I = 10

data_l2 <- data.frame(
  y1 = colMeans(matrix_list$l_2_error_CSQR),
  y2 = colMeans(matrix_list$l_2_error_DPQR),
  y3 = colMeans(matrix_list$l_2_error_REL),
  y4 = colMeans(matrix_list$l_2_error_pool),
  upper1 = colMeans(matrix_list$l_2_error_CSQR) + 0.5*apply(matrix_list$l_2_error_CSQR, MARGIN = 2, FUN = sd),
  lower1 = colMeans(matrix_list$l_2_error_CSQR) - 0.5*apply(matrix_list$l_2_error_CSQR, MARGIN = 2, FUN = sd),
  upper2 = colMeans(matrix_list$l_2_error_DPQR) + 0.5*apply(matrix_list$l_2_error_DPQR, MARGIN = 2, FUN = sd),
  lower2 = colMeans(matrix_list$l_2_error_DPQR) - 0.5*apply(matrix_list$l_2_error_DPQR, MARGIN = 2, FUN = sd),
  upper3 = colMeans(matrix_list$l_2_error_REL) +  0.5*apply(matrix_list$l_2_error_REL, MARGIN = 2, FUN = sd),
  lower3 = colMeans(matrix_list$l_2_error_REL) -  0.5*apply(matrix_list$l_2_error_REL, MARGIN = 2, FUN = sd),
  y5 = rep(colMeans(matrix_list$l_2_error_avgDC), I+1))


plot0 <- ggplot(data_l2, aes(x = 0:I)) +
  geom_line(aes(y = y1, color = "DHSQR", linetype = "DHSQR"), size = 2) +
  geom_point(aes(y = y1, color = "DHSQR", shape = "DHSQR"), size = 6) +
  geom_line(aes(y = y2, color = "DPQR", linetype = "DPQR"), size = 2) +
  geom_point(aes(y = y2, color = "DPQR", shape = "DPQR"), size = 6) +
  geom_line(aes(y = y3, color = "DREL", linetype = "DREL"), size = 2) +
  geom_point(aes(y = y3, color = "DREL", shape = "DREL"), size = 6) +
  geom_line(aes(y = y4, color = "Pooled DHSQR", linetype = "Pooled DHSQR"), size = 2) +
  geom_point(aes(y = y4, color = "Pooled DHSQR", shape = "Pooled DHSQR"), size = 6) +
  geom_line(aes(y = y5, color = "Avg-DC", linetype = "Avg-DC"), size = 2) +
  geom_point(aes(y = y5, color = "Avg-DC", shape = "Avg-DC"), size = 6) +
  labs(x = "Number of Iterations", 
       y = "l2-error", 
       title = "") +
  theme_minimal() +
  scale_color_manual(name = "Method", 
                     values = c("DHSQR" = "blue", "DPQR" = "red", "DREL" = "green", "Pooled DHSQR" = "purple",
                                "Avg-DC" = "black")) +
  scale_shape_manual(name = "Method", 
                     values = c("DHSQR" = 15, "DPQR" = 16, "DREL" = 17, "Pooled DHSQR" = 18, "Avg-DC" = 2)) +
  scale_linetype_manual(name = "Method", 
                        values = c("DHSQR" = "twodash", "DPQR" = "dotdash", "DREL" = "longdash",
                                   "Pooled DHSQR" = "dashed", "Avg-DC" = "dotted")) +
  theme(axis.line = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 30), 
        axis.title = element_text(size = 40),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        panel.background = element_rect(fill = "#eaeaf2"), 
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),  
        legend.background = element_rect(fill = "#eaeaf2", size = 0.5, linetype = "solid", colour = "black")) +  
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  geom_errorbar(aes(ymin = upper1, ymax = lower1), color = "blue", width = 0.2) +
  geom_errorbar(aes(ymin = upper2, ymax = lower2), color = "red", width = 0.2) +
  geom_errorbar(aes(ymin = upper3, ymax = lower3), color = "green", width = 0.2) 

print(plot0)

file_savepath <- "C:/Users/12151/Downloads/CSQR_CSDA/plot/iter_0.5_hom_normal.png"
file_savepath <- "C:/Users/12151/Downloads/CSQR_CSDA/plot/iter_0.5_hom_cauchy.png"
file_savepath <- "C:/Users/12151/Downloads/CSQR_CSDA/plot/iter_0.5_hom_t3.png"
file_savepath <- "C:/Users/12151/Downloads/CSQR_CSDA/plot/iter_0.5_het_normal.png"
file_savepath <- "C:/Users/12151/Downloads/CSQR_CSDA/plot/iter_0.5_het_cauchy.png"
file_savepath <- "C:/Users/12151/Downloads/CSQR_CSDA/plot/iter_0.5_het_t3.png"
width_pixels <- 1200  
height_pixels <- 800  
dpi <- 100 
ggsave(filename = file_savepath, plot = plot0, width = width_pixels / dpi, height = height_pixels / dpi, dpi = dpi)
