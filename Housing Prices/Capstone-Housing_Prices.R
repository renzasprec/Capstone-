if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("knitr", repos = "http://cran.us.r-project.org")

#load required libraries
library(tidyverse)
library(caret)
library(readr)
library(knitr)
library(Rborist)

# set options
options(digits=5)

url<- "https://raw.githubusercontent.com/renzasprec/House-Prices/main/train.csv"
dat<- read_csv(url)

# Exploratory Data Analysis -----
str(dat, give.attr=FALSE)

# split into a training and testing set
set.seed(1, sample.kind = "Rounding")
test_ind<- createDataPartition(dat$SalePrice, times=1, p=0.1, list=FALSE)
test_set<- dat %>% slice(test_ind)
train_set<- dat %>% slice(-test_ind)

# match class of feature based on information on data
num_to_factor<- c("MSSubClass","OverallQual","OverallCond")
train_set[num_to_factor]<- lapply(train_set[num_to_factor], FUN = factor)

# convert characters to factors
char_to_factor<-lapply(train_set,class)
char_to_factor_list<- names(char_to_factor[which(char_to_factor=="character")])

train_set[char_to_factor_list]<- lapply(train_set[char_to_factor_list], FUN = factor)

# convert dates to num
date_to_num<- c("YearBuilt","GarageYrBlt","YearRemodAdd","MoSold","YrSold")
train_set[date_to_num]<- apply(train_set[date_to_num],
                               MARGIN = 2,
                               FUN = as.numeric)

# dimensions of train set
init_dim<- dim(train_set)
init_dim

## check data for NAs -----
nas<- apply(X = train_set, MARGIN = 2, FUN = function(x){sum(is.na(x))})
n_rows<- nrow(train_set)
nas<- rownames_to_column(data.frame(total_na = nas),var = "feature") %>% 
  mutate(prop_na = total_na/n_rows) %>% 
  arrange(desc(prop_na))

# output table of NAs
nas %>% filter(prop_na>0) %>% kable()

# drop features with proportion of NAs greater than 0.80
drop_cols_na<- with(nas, feature[prop_na>=0.80])
train_set<- train_set %>% select(-drop_cols_na)
drop_cols_na

# drop related features
train_set<- train_set %>% select(-c("PoolArea","MiscVal"))

## check maximum weight of a single value ------
single_val_wt_dat<- data.frame(single_val_wt= apply(train_set, MARGIN = 2, FUN = function(x){
  tab<- table(x, useNA = "ifany")
  sort(tab, decreasing = TRUE)[1]/n_rows
})) %>%
  arrange(desc(single_val_wt)) %>%
  rownames_to_column(var = "feature")

single_val_wt_dat %>% kable()

# remove features with single value weight more than 0.8 as these features won't be helpful
drop_cols_swt<- with(single_val_wt_dat, feature[single_val_wt>0.8])
drop_cols_swt %>% kable()

train_set<- train_set %>% select(-drop_cols_swt)

## Filling NA values------

# define indexes for NAs
ind_na<- apply(X = train_set,MARGIN = 2,FUN = function(x){
  any(is.na(x))
})

# features with NAs
na_cols<- data.frame(feature= names(train_set[,ind_na]))
na_cols %>% kable()

# select features where NAs mean 0 (numerical)
num_feat_0<- c("LotFrontage","MasVnrArea", "LotArea","BsmtFinSF1","BsmtUnfSF","TotalBsmtSF","1stFlrSF","2ndFlrSF","GrLivArea","BsmtFullBath","FullBath","HalfBath","BedroomAbvGr","TotRmsAbvGrd","Fireplaces","GarageCars","GarageArea","WoodDeckSF","OpenPorchSF")

data.frame(feature = num_feat_0) %>% kable()

# replace NAs which corresponds to 0
train_set[,num_feat_0]<- apply(train_set[,num_feat_0], MARGIN = 2, FUN = function(x){
  replace(x, is.na(x), 0)
})

# select features where NAs mean none (characters)
char_feat_none<- c("MasVnrType","BsmtQual","BsmtExposure","BsmtFinType1","FireplaceQu","GarageType","GarageFinish")

data.frame(feature = char_feat_none) %>% kable()

# replace NAs which corresponds to none
train_set[,char_feat_none]<- apply(train_set[,char_feat_none], MARGIN = 2,FUN = function(x){
  replace(x, is.na(x),"none")
})

# select features where NAs mean that there are no inputted data
char_feat_mode<- c("MSSubClass","MSZoning","LotShape","LotConfig","Neighborhood","HouseStyle","OverallQual","OverallCond","YearBuilt","YearRemodAdd","RoofStyle","Exterior1st","Exterior2nd","ExterQual","Foundation","HeatingQC","KitchenQual")

data.frame(feature = char_feat_mode) %>% kable()

# replace NAs for factor with the most common value
train_set[,char_feat_mode]<- apply(train_set[,char_feat_mode], MARGIN = 2,FUN = function(x){
  replace(x, is.na(x), names(which.max(table(x))))
})

# other NAs
train_set<- train_set %>% mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt),YearBuilt,GarageYrBlt))

## Investigation of outcome ------

# distribution of sale price
train_set %>%
  ggplot(aes(SalePrice)) +
  geom_histogram(bins=30, color="black")

# distribution of log of sale price
train_set %>%
  ggplot(aes(SalePrice)) +
  geom_histogram(bins=30, color="black")+
  scale_x_log10()

# transform sale price to log10
train_set<- train_set %>%
  mutate(SalePrice = log10(SalePrice))

# Feature Engineering------

# investigate similar features

# plot GrLivArea, 1stFlrSF, and 2ndFlrSF
train_set %>% 
  select(GrLivArea,`1stFlrSF`,`2ndFlrSF`) %>% 
  apply(X = .,MARGIN = 2,FUN=sum) %>%
  as.data.frame() %>% rownames_to_column(var="feature") %>%
  rename(value=".") %>%
  ggplot(aes(feature,value, fill=feature)) +
  geom_col()

# drop 1stFlrSF and 2ndFlrSF
train_set<- train_set %>% select(-c(`1stFlrSF`,`2ndFlrSF`))

# drop GarageCars since it is just similar to GarageArea
train_set<- train_set %>% select(-GarageCars)

# remove Id 
train_set<- train_set %>% select(-Id)

# match class of feature based on information on data
num_to_factor<- c("MSSubClass","OverallQual","OverallCond")
train_set[num_to_factor]<- lapply(train_set[num_to_factor], FUN = factor)

# convert characters to factors
char_to_factor<-lapply(train_set,class)
char_to_factor_list<- names(char_to_factor[which(char_to_factor=="character")])

train_set[char_to_factor_list]<- lapply(train_set[char_to_factor_list], FUN = factor)

# convert dates to num
date_to_num<- c("YearBuilt","GarageYrBlt","YearRemodAdd","MoSold","YrSold")
train_set[date_to_num]<- apply(train_set[date_to_num],
                               MARGIN = 2,
                               FUN = as.numeric)

# training set dimensions
final_dim<- dim(train_set)

# Model Evaluation and Metrics ------

# RMSE function
RMSE<- function(SalePrice_predicted, SalePrice_true){
  sqrt(mean((SalePrice_predicted-SalePrice_true)^2))
}

# Model Training ------

## train model - lm -----
train_set_lm<- train_set

fit_lm<- lm(SalePrice~., data = train_set_lm)

rmse_train_lm<- RMSE(10^fit_lm$fitted.values, 10^train_set_lm$SalePrice)

# obtain 15 most important features
varImp(fit_lm) %>% arrange(desc(Overall)) %>% slice_head(n=35) %>% kable()

imp_lm<- c("GrLivArea","MSZoning","OverallQual","GarageArea","FullBath","BsmtFinType1","HalfBath","BsmtExposure","MSSubClass","Neighborhood","BsmtFullBath","LotArea","KitchenQual","YearRemodAdd","GarageType")

# redefine train_set with only the 15 most important features
train_set_lm<- train_set %>% select(imp_lm,SalePrice)

# new training set dimensions
final_dim_lm<- dim(train_set_lm)

# new model based on redefined data
fit_lm<- lm(SalePrice~., data = train_set_lm)

# rmse of the redefined training data
rmse_train_imp_lm<- RMSE(10^fit_lm$fitted.values,10^train_set_lm$SalePrice)

## TEST SET
# perform data cleaning and feature engineering for the test set

# drop features with proportion of NAs greater than 0.80 (based on train_set data)
test_set<- test_set %>% select(-drop_cols_na)

# drop related features
test_set<- test_set %>% select(-c("PoolArea","MiscVal"))

# remove features with single value weight more than 0.8 as removed from the train_set
test_set<- test_set %>% select(-drop_cols_swt)

# Filling NA values

# define values for NAs
ind_na<- apply(X = test_set,MARGIN = 2,FUN = function(x){
  any(is.na(x))
})

# features with NAs
na_cols<- names(test_set[,ind_na])

# replace NAs which corresponds to 0
test_set[,num_feat_0]<- apply(test_set[,num_feat_0], MARGIN = 2, FUN = function(x){
  replace(x, is.na(x), 0)
})

# replace NAs which corresponds to none
test_set[,char_feat_none]<- apply(test_set[,char_feat_none], MARGIN = 2,FUN = function(x){
  replace(x, is.na(x),"none")
})

# replace NAs for factors with the most common value
test_set[,char_feat_mode]<- apply(test_set[,char_feat_mode], MARGIN = 2,FUN = function(x){
  replace(x, is.na(x), names(which.max(table(x))))
})

# other NAs
test_set<- test_set %>% mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt),YearBuilt,GarageYrBlt))

# drop 1stFlrSF and 2ndFlrSF
test_set<- test_set %>% select(-c(`1stFlrSF`,`2ndFlrSF`))

# drop GarageCars since it is just similar to GarageArea
test_set<- test_set %>% select(-GarageCars)

# remove Id 
test_set<- test_set %>% select(-Id)

# match class of feature based on information on data
num_to_factor<- c("MSSubClass","OverallQual","OverallCond")
test_set[num_to_factor]<- lapply(test_set[num_to_factor], FUN = factor)

# convert characters to factors
char_to_factor<-lapply(test_set,class)
char_to_factor_list<- names(char_to_factor[which(char_to_factor=="character")])

test_set[char_to_factor_list]<- lapply(test_set[char_to_factor_list], FUN = factor)

# convert dates to num
date_to_num<- c("YearBuilt","GarageYrBlt","YearRemodAdd","MoSold","YrSold")
test_set[date_to_num]<- apply(test_set[date_to_num],
                              MARGIN = 2,
                              FUN = as.numeric)

# transform sale price to log10
test_set<- test_set %>%
  mutate(SalePrice = log10(SalePrice))

# predict SalePrice with the test_set containing only the 15 most imporant features (based on lm model)
test_set_lm<- test_set %>% select(imp_lm,SalePrice)
levels(test_set_lm$OverallQual)<- levels(train_set_lm$OverallQual)
levels(test_set_lm$MSSubClass)<- levels(train_set_lm$MSSubClass)

SalePrice_predicted_lm<- predict(fit_lm,newdata = test_set_lm %>% select(-SalePrice))

rmse_lm<- RMSE(10^SalePrice_predicted_lm,10^test_set_lm$SalePrice)

## train model - Random Forest ----
train_set_rf<- train_set
tune_grid<- expand.grid(predFixed= seq(round(final_dim[2]/3),42,3),
                        minNode = seq(2,20,2))

control<- trainControl(method="oob",
                number=20,
                p=0.9)

train_rf<- train(SalePrice~.,
                 method="Rborist",
                 data = train_set_rf,
                 tuneGrid= tune_grid,
                 trControl= control,
                 nTree=500)

# best tune
train_rf$bestTunetTune

# create model using best tuning parameters
fit_rf<- Rborist(x = train_set_rf %>% select(-SalePrice),
                 y = train_set_rf$SalePrice,
                 predFixed=train_rf$bestTune$predFixed,
                 minNode=train_rf$bestTune$minNode)

# compute RMSE of the model based on the train data
rmse_train_rf<- RMSE(10^fit_rf$validation$yPred,10^train_set_rf$SalePrice)
rmse_train_rf

# Obtain importance of each feature
varImp(train_rf)

# Select 15 most important variables
imp_rf<- c("GrLivArea","TotalBsmtSF","GarageArea","YearBuilt","Foundation","FireplaceQu","KitchenQual","GarageYrBlt","LotArea","Fireplaces","TotRmsAbvGrd","BsmtFinSF1","ExterQual","OverallQual","LotFrontage")

# redefine train set based on the 15 most important features
train_set<- train_set %>% select(imp_rf, SalePrice)

# training set dimensions
final_dim_imp<- dim(train_set)

# train model
tune_grid<- expand.grid(predFixed= seq(round(15/3),14,1),
                        minNode = seq(2,20,2))

control<- trainControl(method="oob",
                       number=20,
                       p=0.9)

train_rf<- train(SalePrice~.,
                 method="Rborist",
                 data = train_set,
                 tuneGrid= tune_grid,
                 trControl= control,
                 nTree=500)

# create model using best tuning parameters
fit_rf<- Rborist(x = train_set %>% select(-SalePrice),
                 y = train_set$SalePrice,
                 predFixed=train_rf$bestTune$predFixed,
                 minNode=train_rf$bestTune$minNode)

# compute RMSE of the model based on the train data
rmse_train_imp_rf<- RMSE(10^fit_rf$validation$yPred,10^train_set$SalePrice)
rmse_train_imp_rf

# redefine test set with the 15 most important features (based on rf)
test_set_rf<- test_set %>% select(imp_rf, SalePrice)

# predict sale price of test set using train_rf with the optimized parameters
SalePrice_predicted_rf<- predict(fit_rf, newdata = test_set_rf %>% select(-SalePrice))

# compute RMSE of the antilogs of the predicted and true sale prices
rmse_rf<- RMSE(10^SalePrice_predicted_rf$yPred, 10^test_set$SalePrice)
rmse_rf

# summary of RMSEs for the two models -----
data.frame(model = c("linear regression","random forest"), training_RMSE = c(rmse_train_imp_lm, rmse_train_imp_rf), test_RMSE = c(rmse_lm,rmse_rf)) %>% kable()


