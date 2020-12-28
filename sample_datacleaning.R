# sample data cleaning for research survey text items 



library(readxl) 
library(openxlsx) 
library(rio)

sample <- read_excel("responsibility_text_response.xlsx") 


sample <- as.data.frame(sample)

# convert text entry items to numeric 
sample[sample == "Strongly disagree"] <- 1
sample[sample == "Somewhat disagree"] <- 2
sample[sample == "Neither agree nor disagree"] <- 3
sample[sample == "Somewhat agree"] <- 4
sample[sample == "Strongly agree"] <- 5


sample[sample == "Definitely not"] <- 1
sample[sample == "Probably not"] <- 2
sample[sample == "Might or might not"] <- 3
sample[sample == "Probably yes"] <- 4
sample[sample == "Definitely yes"] <- 5

sample[sample == "Never"] <- 1
sample[sample == "Sometimes"] <- 2
sample[sample == "About half the time"] <- 3
sample[sample == "Most of the time"] <- 4
sample[sample == "Always"] <- 5


sample[sample == "Does not describe me"] <- 1
sample[sample == "Describes me slightly well"] <- 2
sample[sample == "Describes me moderately well"] <- 3
sample[sample == "Describes me very well"] <- 4
sample[sample == "Describes me extremely well"] <- 5


sample[sample == "Disagree strongly"] <- 1
sample[sample == "Disagree a little"] <- 2
sample[sample == "Neither agree or disagree"] <- 3
sample[sample == "Agree a little"] <- 4
sample[sample == "Agree strongly"] <- 5


# remove select fields

sample <- sample[-1, ]
sample <- sample[ , 1:50]

scale <- sample[1:5, 1:50]
scale <- sapply(scale, as.numeric) 
str(scale)




# reverse code select items 

#keys <- c(1,1,1,1,1,1,-1,1,-1,-1,1,1,1,-1)
#new_sub_scale <- reverse.code(keys, scale) 

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

# convert data set from list to data frame class 
sample <- data.frame(matrix(unlist(sample), nrow = length(sample), byrow = T), 
                     stringsAsFactors = FALSE) 



# check missing answers for items 

nrow(na.omit(sample)) 
colSums(is.na(sample)) 



# item correlations 
library(corrplot) 

corrplot(cor(scale), method = "circle") 

corr.test(x = scale[ , 1:5])


# Multi-trait scaling using psy package's mtmm() function 
# for assessing convergent and discriminant validity 
# produces boxpolots of distributions of Pearson correlations between items of subscales 


library(psy)
mtmm(items,list(c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", 
                  "Q11", "Q12", "Q13"),
                c("Q56", "Q57", "Q58", "Q59", "Q60", "Q98",
                  "Q103", "Q108", "Q113", "Q118", "Q123", "Q128", "Q133", "Q138")))





# create data frame for basic item statistics 
items_avg_values <- sapply(data, mean) 
items_sd_values <- sapply(data, sd)
item_stats <- cbind(items_avg_values, items_sd_values) 
item_stats



# For convergent and discriminant validity, use scoreItems() from 
# the psych package
# using keys argument, scoreItems provides avg scores for each participant 
# on a scale; you can then run correlation for each participant on two scales

