library(openxlsx)
library(ggplot2)


excel_file <- "C:/Users/12151/Downloads/CSQR_CSDA/plot/iter_n_0.5_het_normal (2).xlsx"
sheets <- getSheetNames(excel_file)
current_data <- read.xlsx(excel_file, sheet = sheets[1], startRow = 1)

data_l2 <- data.frame(
  y1 = current_data$CSQR,
  y2 = current_data$POOLED.CSQR,
  y3 = current_data$DPQR,
  y4 =  current_data$DREL,
  y5 = current_data$`Avg-DC`,
  upper1 = current_data$CSQR + 0.5*current_data$sd1,
  lower1 = current_data$CSQR - 0.5*current_data$sd1,
  upper2 = current_data$DPQR + 0.5*current_data$sd3,
  lower2 = current_data$DPQR - 0.5*current_data$sd3,
  upper3 = current_data$DREL + 0.5*current_data$sd4,
  lower3 = current_data$DREL - 0.5*current_data$sd4
)

N_set <- c(current_data$N)
n_set <- c(current_data$n)

plot0 <- ggplot(data_l2, aes(x = n_set)) +
  geom_line(aes(y = y1, color = "DHSQR", linetype = "DHSQR"), size = 2) +
  geom_point(aes(y = y1, color = "DHSQR", shape = "DHSQR"), size = 6) +
  geom_line(aes(y = y3, color = "DPQR", linetype = "DPQR"), size = 2) +
  geom_point(aes(y = y3, color = "DPQR", shape = "DPQR"), size = 6) +
  geom_line(aes(y = y4, color = "DREL", linetype = "DREL"), size = 2) +
  geom_point(aes(y = y4, color = "DREL", shape = "DREL"), size = 6) +
  geom_line(aes(y = y2, color = "Pooled DHSQR", linetype = "Pooled DHSQR"), size = 2) +
  geom_point(aes(y = y2, color = "Pooled DHSQR", shape = "Pooled DHSQR"), size = 6) +
  geom_line(aes(y = y5, color = "Avg-DC", linetype = "Avg-DC"), size = 2) +
  geom_point(aes(y = y5, color = "Avg-DC", shape = "Avg-DC"), size = 6) +
  labs(x = "Number of Local sample size", y = "???2-Error", title = "") +
  theme_minimal() +
  scale_color_manual(name= "Method", values = c("DHSQR" = "blue", "DPQR" = "red", "DREL" = "green", 
                                                "Pooled DHSQR" = "purple",
                                                "Avg-DC" = "black")) +
  scale_shape_manual(name= "Method", values = c("DHSQR" = 15, "DPQR" = 16, "DREL" = 17, 
                                                "Pooled DHSQR" = 18, "Avg-DC" = 2)) +
  scale_linetype_manual(name= "Method", values = c("DHSQR" = "twodash", "DPQR" = "dotdash", 
                                                   "DREL" = "longdash",
                                                   "Pooled DHSQR" = "dashed", "Avg-DC" = "dotted")) +
  theme(axis.line = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 40), 
        axis.title = element_text(size = 50),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        panel.background = element_rect(fill = "#eaeaf2"), 
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25), 
        legend.position = c(1, 1),
        legend.justification = c(1,1),
        legend.background = element_rect(fill = "#eaeaf2", size=0.5, linetype="solid",colour ="black")) +  
  geom_errorbar(aes(ymin = upper1, ymax = lower1), color = "blue", width = 80) +
  geom_errorbar(aes(ymin = upper2, ymax = lower2), color = "red", width = 80) +
  geom_errorbar(aes(ymin = upper3, ymax = lower3), color = "green", width = 80) 

print(plot0)
file_savepath <- "C:/Users/12151/Downloads/CSQR_CSDA/plot/iter_(n)_0.5_het_normal_l2.png"
width_pixels <- 1200  
height_pixels <- 1000  
dpi <- 100 
ggsave(filename = file_savepath, plot = plot0, width = width_pixels / dpi, height = height_pixels / dpi, dpi = dpi)



current_data <- read.xlsx(excel_file, sheet = sheets[2], startRow = 1)
data_F1 <- data.frame(
  y1 = current_data$CSQR,
  y2 = current_data$POOLED.CSQR,
  y3 = current_data$DPQR,
  y4 =  current_data$DREL,
  y5 = current_data$`Avg-DC`
)

plot1 <- ggplot(data_F1, aes(x = n_set)) +
  geom_line(aes(y = y1, color = "DHSQR", linetype = "DHSQR"), size = 2) +
  geom_point(aes(y = y1, color = "DHSQR", shape = "DHSQR"), size = 6) +
  geom_line(aes(y = y3, color = "DPQR", linetype = "DPQR"), size = 2) +
  geom_point(aes(y = y3, color = "DPQR", shape = "DPQR"), size = 6) +
  geom_line(aes(y = y4, color = "DREL", linetype = "DREL"), size = 2) +
  geom_point(aes(y = y4, color = "DREL", shape = "DREL"), size = 6) +
  geom_line(aes(y = y2, color = "Pooled DHSQR", linetype = "Pooled DHSQR"), size = 2) +
  geom_point(aes(y = y2, color = "Pooled DHSQR", shape = "Pooled DHSQR"), size = 6) +
  geom_line(aes(y = y5, color = "Avg-DC", linetype = "Avg-DC"), size = 2) +
  geom_point(aes(y = y5, color = "Avg-DC", shape = "Avg-DC"), size = 6) +
  labs(x = "Number of Local sample size", y = "F1-score", title = "") +
  theme_minimal() +
  scale_color_manual(name= "Method", values = c("DHSQR" = "blue", "DPQR" = "red", "DREL" = "green", "Pooled DHSQR" = "purple",
                                                "Avg-DC" = "black")) +
  scale_shape_manual(name= "Method", values = c("DHSQR" = 15, "DPQR" = 16, "DREL" = 17, "Pooled DHSQR" = 18, "Avg-DC" = 2)) +
  scale_linetype_manual(name= "Method", values = c("DHSQR" = "twodash", "DPQR" = "dotdash", "DREL" = "longdash",
                                                   "Pooled DHSQR" = "dashed", "Avg-DC" = "dotted")) +
  theme(axis.line = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 40), 
        axis.title = element_text(size = 50),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        panel.background = element_rect(fill = "#eaeaf2"), 
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20), 
        legend.background = element_rect(fill = "#eaeaf2", size=0.5, linetype="solid",colour ="black"))+
  theme(legend.position = "top")

print(plot1)

file_savepath <- "C:/Users/12151/Downloads/CSQR_CSDA/plot/iter_(n)_0.5_het_normal_F1.png"
width_pixels <- 1200  
height_pixels <- 1000  
dpi <- 100 
ggsave(filename = file_savepath, plot = plot1, width = width_pixels / dpi, height = height_pixels / dpi, dpi = dpi)




excel_file <- "C:/Users/12151/Downloads/CSQR_CSDA/plot/iter_N_0.5_het_normal.xlsx"
sheets <- getSheetNames(excel_file)
current_data <- read.xlsx(excel_file, sheet = sheets[1], startRow = 1)

data_l2 <- data.frame(
  y1 = current_data$CSQR,
  y2 = current_data$POOLED.CSQR,
  y3 = current_data$DPQR,
  y4 =  current_data$DREL,
  y5 = current_data$`Avg-DC`,
  upper1 = current_data$CSQR + 0.5*current_data$sd1,
  lower1 = current_data$CSQR - 0.5*current_data$sd1,
  upper2 = current_data$DPQR + 0.5*current_data$sd3,
  lower2 = current_data$DPQR - 0.5*current_data$sd3,
  upper3 = current_data$DREL + 0.5*current_data$sd4,
  lower3 = current_data$DREL - 0.5*current_data$sd4
)

N_set <- c(current_data$N)
n_set <- c(current_data$n)

plot0 <- ggplot(data_l2, aes(x = N_set)) +
  geom_line(aes(y = y1, color = "DHSQR", linetype = "DHSQR"), size = 2) +
  geom_point(aes(y = y1, color = "DHSQR", shape = "DHSQR"), size = 6) +
  geom_line(aes(y = y3, color = "DPQR", linetype = "DPQR"), size = 2) +
  geom_point(aes(y = y3, color = "DPQR", shape = "DPQR"), size = 6) +
  geom_line(aes(y = y4, color = "DREL", linetype = "DREL"), size = 2) +
  geom_point(aes(y = y4, color = "DREL", shape = "DREL"), size = 6) +
  geom_line(aes(y = y2, color = "Pooled DHSQR", linetype = "Pooled DHSQR"), size = 2) +
  geom_point(aes(y = y2, color = "Pooled DHSQR", shape = "Pooled DHSQR"), size = 6) +
  geom_line(aes(y = y5, color = "Avg-DC", linetype = "Avg-DC"), size = 2) +
  geom_point(aes(y = y5, color = "Avg-DC", shape = "Avg-DC"), size = 6) +
  labs(x = "Number of Total sample size", y = "???2-Error", title = "") +
  theme_minimal() +
  scale_color_manual(name= "Method", values = c("DHSQR" = "blue", "DPQR" = "red", "DREL" = "green", 
                                                "Pooled DHSQR" = "purple",
                                                "Avg-DC" = "black")) +
  scale_shape_manual(name= "Method", values = c("DHSQR" = 15, "DPQR" = 16, "DREL" = 17, 
                                                "Pooled DHSQR" = 18, "Avg-DC" = 2)) +
  scale_linetype_manual(name= "Method", values = c("DHSQR" = "twodash", "DPQR" = "dotdash", "DREL" = "longdash",
                                                   "Pooled DHSQR" = "dashed", "Avg-DC" = "dotted")) +
  theme(axis.line = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 40), 
        axis.title = element_text(size = 50),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        panel.background = element_rect(fill = "#eaeaf2"), 
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25), 
        legend.position = c(1, 1),
        legend.justification = c(1,1),
        legend.background = element_rect(fill = "#eaeaf2", size=0.5, linetype="solid",colour ="black")) +  
  geom_errorbar(aes(ymin = upper1, ymax = lower1), color = "blue", width = 800) +
  geom_errorbar(aes(ymin = upper2, ymax = lower2), color = "red", width = 800) +
  geom_errorbar(aes(ymin = upper3, ymax = lower3), color = "green", width = 800) 

print(plot0)
file_savepath <- "C:/Users/12151/Downloads/CSQR_CSDA/plot/iter_N_0.5_het_normal_l2.png"
width_pixels <- 1200  
height_pixels <- 1000  
dpi <- 100 
ggsave(filename = file_savepath, plot = plot0, width = width_pixels / dpi, height = height_pixels / dpi, dpi = dpi)



current_data <- read.xlsx(excel_file, sheet = sheets[2], startRow = 1)
data_F1 <- data.frame(
  y1 = current_data$CSQR,
  y2 = current_data$POOLED.CSQR,
  y3 = current_data$DPQR,
  y4 =  current_data$DREL,
  y5 = current_data$`Avg-DC`
)

plot1 <- ggplot(data_F1, aes(x = N_set)) +
  geom_line(aes(y = y1, color = "DHSQR", linetype = "DHSQR"), size = 2) +
  geom_point(aes(y = y1, color = "DHSQR", shape = "DHSQR"), size = 6) +
  geom_line(aes(y = y3, color = "DPQR", linetype = "DPQR"), size = 2) +
  geom_point(aes(y = y3, color = "DPQR", shape = "DPQR"), size = 6) +
  geom_line(aes(y = y4, color = "DREL", linetype = "DREL"), size = 2) +
  geom_point(aes(y = y4, color = "DREL", shape = "DREL"), size = 6) +
  geom_line(aes(y = y2, color = "Pooled DHSQR", linetype = "Pooled DHSQR"), size = 2) +
  geom_point(aes(y = y2, color = "Pooled DHSQR", shape = "Pooled DHSQR"), size = 6) +
  geom_line(aes(y = y5, color = "Avg-DC", linetype = "Avg-DC"), size = 2) +
  geom_point(aes(y = y5, color = "Avg-DC", shape = "Avg-DC"), size = 6) +
  labs(x = "Number of Total sample size", y = "F1-score", title = "") +
  theme_minimal() +
  scale_color_manual(name= "Method", values = c("DHSQR" = "blue", "DPQR" = "red", "DREL" = "green", 
                                                "Pooled DHSQR" = "purple",
                                                "Avg-DC" = "black")) +
  scale_shape_manual(name= "Method", values = c("DHSQR" = 15, "DPQR" = 16, "DREL" = 17,
                                                "Pooled DHSQR" = 18, "Avg-DC" = 2)) +
  scale_linetype_manual(name= "Method", values = c("DHSQR" = "twodash", "DPQR" = "dotdash", "DREL" = "longdash",
                                                   "Pooled DHSQR" = "dashed", "Avg-DC" = "dotted")) +
  theme(axis.line = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 40), 
        axis.title = element_text(size = 50),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        panel.background = element_rect(fill = "#eaeaf2"), 
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20), 
        legend.background = element_rect(fill = "#eaeaf2", size=0.5, linetype="solid",colour ="black"))+
  theme(legend.position = "top")

print(plot1)
file_savepath <- "C:/Users/12151/Downloads/CSQR_CSDA/plot/iter_N_0.5_het_normal_F1.png"
width_pixels <- 1200  
height_pixels <- 1000  
dpi <- 100 
ggsave(filename = file_savepath, plot = plot1, width = width_pixels / dpi, height = height_pixels / dpi, dpi = dpi)