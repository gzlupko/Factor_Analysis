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
# evaluate primary item loadings 

humor_EFA_model$loadings

# check eigenvalues greater than 1
# a good indication that we have the right number of factors 
# is if the number of eigenvalues greater than one are equivalent to the factors 

humor_EFA_model$e.values

# shows 7 values above 1 and an 8th eigenvalue that is close to 1
# which may be measuring something other than the 6 dimensions observed in the scree


# factor score correlations 
# if factor score correlations are too similar this may mean that our 
# separate dimensions are measuring the same thing 
# 0.6 is a good indication of whether factors are too correlated 

humor_EFA_model$score.cor




# look for items to drop 
# ideally, each survey item only loads onto one factor; this is called the primary loading 
# it is ok for an item to load onto additional factors so long as the secondary loadings are below 0.4 
# if an item does not load seriously onto any one factor then the item does not clearly reflect any dimension of the construct 

humor_EFA_model$loadings
# Q3 may be a candidate to drop as it does not seem to load seriously onto one factor 
# Q6 may also be a candidate as the primary loading is poor also Q28 

# drop item 6 to start and compare EFA models with same number of factors 
# also update naming convention for new objects to reflect number of items remaining in edited data sets 

humor_items_31 <- humor_items %>%
  select(-Q6) 

humor_items_31_a <- humor_items %>%
  select(-Q28) 

humor_EFA_31_a <- fa(humor_items_31_a, nfactors = 6) 

humor_EFA_31 <- fa(humor_items_31, nfactors = 6) 
humor_EFA_31$loadings
humor_EFA_model$loadings 

humor_EFA_model$loadings

humor_items_30 <- humor_items_31 %>%
  select(-Q28) 
humor_EFA_30 <- fa(humor_items_30, nfactors = 6) 

# compare differences in eigenvalues 


humor_EFA_model$e.values
humor_EFA_31$e.values 
humor_EFA_31_a$e.values
humor_EFA_30$e.values

# compare item correlations 

humor_EFA_model$score.cor
humor_EFA_31$score.cor
humor_EFA_31_a$score.cor
humor_EFA_30$score.cor

# compare model fit statistics 

humor_EFA_model$TLI
humor_EFA_31$TLI
humor_EFA_31_a$TLI
humor_EFA_30$TLI

humor_EFA_model$RMSEA
humor_EFA_31$RMSEA
humor_EFA_31_a$RMSEA
humor_EFA_30$RMSEA

humor_EFA_model$chi
humor_EFA_31$chi
humor_EFA_31_a$chi 
humor_EFA_30$chi


# so far the humor_EFA_31 had the best fit 

humor_items_30 <- humor_items_31 %>%
  select(-Q28) 
humor_EFA_30_new <- fa(humor_items_30, nfactors = 6) 
humor_EFA_30_new$loadings

humor_items_29 <- humor_items_30 %>%
  select(-Q19) 
humor_EFA_29 <- fa(humor_items_29, nfactors = 6) 

humor_EFA_29$loadings

humor_items_28 <- humor_items_29 %>%
  select(-Q21)
humor_EFA_28 <- fa(humor_items_28, nfactors = 6) 
humor_EFA_28$loadings


# compare original EFA to 28 item 
humor_EFA_model$e.values 
humor_EFA_28$e.values
humor_EFA_model$TLI
humor_EFA_28$TLI
humor_EFA_model$chi
humor_EFA_28$chi 
humor_EFA_model$RMSEA
humor_EFA_28$RMSEA
