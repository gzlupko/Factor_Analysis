# Factor Analysis Techniques 
# Humor Styles Questionnaire (HSQ) from Martin & Doris (2003) 

# libraries used

library(readr) 
library(psych) 
library(sem) 
library(GPArotation) 
library(ggplot2) 
library(dplyr) 


# load data 

humor <- read_csv("humordata.csv") 
head(humor) 
View(humor) 

# remove non-item data 

humor_items <- humor[ ,1:32]
View(humor_items) 


# data split for EFA and CFA workstreams

N <- nrow(humor_items) 
indices <- seq(1, N) 

indices_EFA <- sample(indices, floor((0.5* N)))
indices_CFA <- indices[!(indices %in% indices_EFA)]
humor_EFA <- humor_items[indices_EFA, ] 
humor_CFA <- humor_items[indices_CFA, ]


# check descriptive statistics to assess similarity between data splits
describe(indices_EFA); describe(indices_CFA) 

# create grouping variable to compare halves of the data set 

group_var <- vector("numeric", nrow(humor_items)) 
group_var[indices_EFA] <- 1
group_var[indices_CFA] <- 2
group_var

# attach the grouping variable onto 
humor_grouped <- cbind(humor_items, group_var)

# now compare descriptive statistics of the two data sets by 
# the group_var column that we created to tag the two separate sets 

describeBy(humor_grouped, group = group_var) 
statsBy(humor_grouped, group = "group_var") 


# lower correlations for data set 

lowerCor(humor_items) 


# p-values for correlations 
corr.test(humor_items, use = "pairwise.complete.obs")$p

# confidence intervals for correlations 
corr.test(humor_items, use = "pairwise.complete.obs")$ci

# coefficient (Cronbach') alpha 
alpha(humor_items, check.keys = TRUE) 


alpha# split-half reliability 
splitHalf(humor_items) 

error.bars(humor_items)
error.dots(humor_items) 



# calculate eigenvalues 

humor_items_EFA_cor <- cor(humor_EFA, use = "pairwise.complete.obs") 

eigenvals <- eigen(humor_items_EFA_cor)
eigenvals$values 


# generate scree plot, which visually represents eigenvalues 
# and eigenvalues represent the number of dimensions in the data 
# first you calculate a correlation matrix, which the scree function takes 
# as its input 

scree(humor_items_EFA_cor, factors = FALSE) 





# use parallel analysis scree plot 
# psych prints out suggested number of factors = 6 and number of components = 2 

fa.parallel(humor_items) 




# Exploratory Factor Analysis 

humor_EFA_model <- fa(humor_EFA, nfactors = 6) 
humor_EFA_model
humor_EFA_model$loadings


# visualize EFA model 


fa.diagram(humor_EFA_model, marg=c(3,.5,1,3), 
           main = "Humor EFA Diagram")
fa.graph(humor_EFA_model, ) 
?fa.diagram

# assessing absolute model fit statistics 

humor_EFA_model
str(humor_EFA_model)
humor_EFA_model$TLI
humor_EFA_model$RMSEA
humor_EFA_model$chi
str(humor_EFA_model$loadings)
humor_EFA_model$loadings[1:6]

# build grid for factor loadings and color the good loadings 
# for presentation of factor loadings 


humor_EFA_model$loadings
humor_EFA_model$uniquenesses

humor_EFA_model$loadings 


# check other numbers of factors with EFA 
# how many valid factors do we have? 


fa(humor_items, nfactors = 32, rotate = "oblimin") 
fa(humor_items, nfactors = 6, rotate = "oblimin") 
fa(humor_items, nfactors = 5, rotate = "oblimin") 

# check EFA model fit statistics 

humor_EFA_model$TLI
humor_EFA_model$RMSEA
humor_EFA_model$chi


# item refinement 




