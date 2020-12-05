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

# test numeric exported data set 


scale1 <- read_csv("numeric_1.csv") 
View(scale1) 


scale1 <- scale1[3:10 , 18:50]
scale1 <- sapply(scale1, as.numeric) 
scale1 <- scale[ , 3:50]

View(scale1)


# test EFA with numeric survey data output 
scale1_EFA_cor <- cor(scale1, use = "pairwise.complete.obs") 
EFA_model1 <- fa(scale1, nfactors = 4) 
EFA_model1$loadings





# load likert package to use for EDA - specifically for item response frequencies 

library(likert) 

response.frequencies(scale1) 

scale_likert <- scale1 %>% mutate_if(is.integer, as.factor) 

scale_likert

results <- scale_likert[ ,1:5 ]
results 
summary(results) 


# Reverse code "opposite" items with psych package function 
#scale1$Q2 <- recode(brand_qual$tired, "1 = 5; 2 = 4; 4 = 2; 5 = 1")


