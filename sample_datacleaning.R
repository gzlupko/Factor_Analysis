# sample data cleaning for research survey text items 



library(readxl) 
library(openxlsx) 
library(rio)

sample <- read_excel("responsibility_text_response.xlsx") 

sample <- as.data.frame(sample)

sample[sample == "Strongly disagree"] <- 0
sample[sample == "Somewhat disagree"] <- 1
sample[sample == "Neither agree nor disagree"] <- 2
sample[sample == "Somewhat agree"] <- 3
sample[sample == "Strongly agree"] <- 4


sample <- sample[-1, ]
sample <- sample[ , 1:50]

scale <- sample[1:5, 1:50]
scale <- sapply(scale, as.numeric) 
str(scale)
View(scale) 


#export(scale, "sample_R_output.xlsx") 




# test EFA model 

sample_EFA_cor <- cor(scale, use = "pairwise.complete.obs") 
EFA_model <- fa(scale, nfactors = 4) 
EFA_model$loadings


