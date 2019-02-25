#Hyndman's example based
library(data.table)
library(Hmisc)
library(missForest)
require(fpp)
require(forecast)
library(fBasics)
library(data.table)
library(timeSeries)
library(ggplot2)
library(xts)
require(dplyr)

df <- fread("dengue_labels_train_rem_53_add_52.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
df_xreg.miss <- fread("dengue_features_train_xreg_missforest_input.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
df_xreg_imp <- missForest(df_xreg.miss, maxiter = 1)
#EDA
# total cases of dengue: histograms
df.iq <- df[df$city=="iq",]
df.sj <- df[df$city=="sj",]

# qplot((rbind(df.sj,df.iq)), data=df, facets = city~.)
# load libraries
pkgs <- c('tidyverse', 'corrplot', 'magrittr', 'zoo', 'RColorBrewer', 'gridExtra','MASS')
invisible(lapply(pkgs, require, character.only = T))
# total cases of dengue: histograms
# Colors
colors <- c(rep("red",7), rep("blue",4))
rbind((rbind(df.sj,df.iq))) %>% 
  ggplot(aes(x = total_cases,fill = ..count..)) + 
  geom_histogram(bins = 12, colour = 'black') + ggtitle('Total Cases of Dengue') +
  scale_y_continuous(breaks = seq(0,700,100)) + facet_wrap(~city)
 
#heat map
# corerlations between features
dengue_feature_label_train <- cbind(df,df_xreg_imp$ximp)
df.xreg_imp.sj <- dengue_feature_label_train[dengue_feature_label_train$city=="sj",]
df.xreg_imp.iq <- dengue_feature_label_train[dengue_feature_label_train$city=="iq",]
# 
# # dengue_feature_label_train_iq <- df[df$city=="iq",]
# df.xreg_imp.sj %<>% mutate('total_cases' = df.sj$total_cases)
#   df.xreg_imp.iq %<>% mutate('total_cases' = df.iq$total_cases)

# plot san juan correlation matrix
df.xreg_imp.sj %>% 
  dplyr::select(-city, -year, -weekofyear) %>%
  cor(use = 'pairwise.complete.obs') -> M1

corrplot(M1, type="lower", method="color",
         col=brewer.pal(n=8, name="RdBu"),diag=FALSE)
# plot iquitos correlation matrix
df.xreg_imp.iq %>% 
  dplyr::select(-city, -year, -weekofyear) %>%
  cor(use = 'pairwise.complete.obs') -> M2

corrplot(M2, type="lower", method="color",
         col=brewer.pal(n=8, name="RdBu"),diag=FALSE)
# see the correlations as barplot
sort(M1[21,-21]) %>%  
  as.data.frame %>% 
  `names<-`('correlation') %>%
  ggplot(aes(x = reorder(row.names(.), -correlation), y = correlation, fill = correlation)) + 
  geom_bar(stat='identity', colour = 'black') + scale_fill_continuous(guide = FALSE) + scale_y_continuous(limits =  c(-.15,.25)) +
  labs(title = 'San Jose\n Correlations', x = NULL, y = NULL) + coord_flip() -> cor1

# can use ncol(M1) instead of 21 to generalize the code
sort(M2[21,-21]) %>%  
  as.data.frame %>% 
  `names<-`('correlation') %>%
  ggplot(aes(x = reorder(row.names(.), -correlation), y = correlation, fill = correlation)) + 
  geom_bar(stat='identity', colour = 'black') + scale_fill_continuous(guide = FALSE) + scale_y_continuous(limits =  c(-.15,.25)) +
  labs(title = 'Iquitos\n Correlations', x = NULL, y = NULL) + coord_flip() -> cor2

grid.arrange(cor1, cor2, nrow = 1)


#make a data frame from df and df_xreg.miss to check for correlation 
all.train <- cbind(df$total_cases,df_xreg.miss)
# pairs(all.train)
df_xreg_imp <- missForest(df_xreg.miss, maxiter = 1)

hist(log(df$total_cases))
plot(ts(df$total_cases))
# pairs(df)
# xreg - data frame of predictors
xreg <- cbind(df_xreg_imp$ximp$ndvi_ne,df_xreg_imp$ximp$ndvi_nw)
pairs(xreg)
# Rename columns
# colnames(xreg) <- c("ndvi_ne","ndvi_nw")
# colnames(xreg) <- c("ndvi_ne","ndvi_nw","ndvi_se","ndvi_sw")
colnames(xreg) <- c("reanalysis_dew_point_temp_k","reanalysis_max_air_temp_k")

# Variable to be modelled
total_cases <- ts(df$total_cases, frequency=52)

# Find ARIMAX model
# modArima <- auto.arima(visits, xreg=xreg)
#modArima <- auto.arima((total_cases), xreg=(df_xreg_imp$ximp), d=0, D=1, max.order=9, stepwise = TRUE)
modArima <- auto.arima((total_cases), xreg=(df_xreg_imp$ximp),max.order=9, stepwise = TRUE)
fit.nn <- nnetar((total_cases), xreg = df_xreg_imp$ximp, repeats = 2)

tsdisplay(residuals(modArima))
Box.test(residuals(modArima), lag=36, fitdf=8, type="Ljung")

df.test <- fread("dengue_features_test.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
df.test_xreg <- fread("dengue_features_test_xreg_missforest_impute.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
df.test_xreg_imp <- missForest(df.test_xreg, maxiter = 1)
# xreg.test <- cbind(df.test$ndvi_ne,df.test$ndvi_nw)
xreg.test <- cbind(df.test_xreg_imp$ximp$ndvi_ne,df.test_xreg_imp$ximp$ndvi_nw)
# Rename columns
colnames(xreg.test) <- c("reanalysis_dew_point_temp_k","reanalysis_max_air_temp_k")
# colnames(xreg.test) <- c("ndvi_ne","ndvi_nw")
# fcast.hynd <- forecast(modArima, newreg=xreg, newdata=df.test)
fcast.hynd <- predict(modArima,newxreg =df.test_xreg_imp$ximp,newdata=df.test$total_cases)#predict wants newxreg
fcast.hynd.fcast <- forecast(modArima,xreg =df.test_xreg_imp$ximp,newdata=df.test)# forecast wants xreg, both are same in meaning, called differently
plot(fcast.hynd.fcast, ylab="total cases", xlab="week of the year")
checkresiduals(fcast.hynd.fcast)
#accuracy on test cannot be checked from predict results. It can be checked from froecast
fcast.nn <- predict(fit.nn,xreg=df.test_xreg_imp$ximp, newdata=df.test)
plot(fcast.nn, ylab="total cases", xlab="week of the year")
checkresiduals(fit.nn)
checkresiduals(fcast.nn)


# fcast.hynd <- forecast(df.test, xreg=xreg, model=modArima)
predicted <- fcast.hynd$pred
predicted.nn <- fcast.nn$mean
pred.round <- round(predicted.nn, digits = 0)
pred.round
write.csv(pred.round,"dengue_nn.csv")

#### nnetar modeled as 2 cities ####
#### sj ####
df.sj <- fread("dengue_labels_train_rem_53_add_52_sj.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
# Variable to be modelled
total_cases.sj <- ts(df.sj$total_cases, frequency=52)
df.xreg.sj <- fread("dengue_labels_train_rem_53_add_52_sj_imp.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)

df.xreg.sj_imp <- missForest(df.xreg.sj, maxiter = 1)

#EDA
cor(cbind(df.sj[,4],df.xreg.sj_imp$ximp))
# df.sj.lagp <- window(ausbeer, start=1992, end=2006-.1)
lag.plot(df.sj$total_cases, lags=9, do.lines=FALSE)
acf(df.sj$total_cases)

df.sj.ts <- ts(df.sj$total_cases, frequency = 52, start = c(1990,18))
plot((df.sj.ts), xlab="Year",ylab="Total_cases", main= "Time series plot for San Juan", col="red")

# hist(df.sj$total_cases, col = "red")
summary(df.sj$total_cases)
# library(plyr)
# library(psych)
df.sj.hist <- cbind(df.sj,df.xreg.sj_imp$ximp)
# multi.hist(mpg) #error, not numeric
# multi.hist(df.sj.hist[,sapply(df.sj.hist, is.numeric)])
#missing values
# for (Var in names(df.xreg.sj)) {
#   missing <- sum(is.na(df.xreg.sj[,c(Var)]))
#   if (missing > 0) {
#     print(c(Var,missing))
#   }
# }
#missing values
sapply(df.xreg.sj, function(x) sum(is.na(x)))

# library(reshape2)
# df.sj.hist.id <- mutate(df.sj.hist, id=as.numeric(rownames(df.sj.hist)))
# df.sj.hist.stack <- melt(df.sj.hist.id, id="id")
# pp <- qplot(value, data=df.sj.hist.stack) + facet_wrap(~variable, scales="free")
# # pp + stat_bin(geom="text", aes(label=..count.., vjust=-1))
# ggsave("mpg-histograms.pdf", pp, scale=2)

fit.nn.sj <- nnetar((total_cases.sj), xreg = df.xreg_imp$ximp, repeats = 2)

df.test.sj <- fread("dengue_features_test_sj.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
df.test.sj.4imp <- fread("dengue_features_test_imp_sj.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
df.test.sj.imp <- missForest(df.test.sj.4imp, maxiter = 1)

fcast.nn.sj <- predict(fit.nn.sj,xreg=df.test.sj.imp$ximp, newdata=df.test.sj)
#plots
plot(fcast.nn.sj, ylab="total cases", xlab="week of the year")
checkresiduals(fit.nn.sj)
checkresiduals(fcast.nn.sj)
fcast.nn.sj <- round(fcast.nn.sj$mean, digits = 0)

#### iq ####
df.iq <- fread("dengue_labels_train_rem_53_add_52_iq.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
# Variable to be modelled
total_cases.iq <- ts(df.iq$total_cases, frequency=52)
df.xreg.iq <- fread("dengue_labels_train_rem_53_add_52_iq_imp.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)

df.xreg.iq_imp <- missForest(df.xreg.iq, maxiter = 1)
#EDA
cor(cbind(df.iq[,4],df.xreg.iq_imp$ximp))
lag.plot(df.sj$total_cases, lags=9, do.lines=FALSE)
acf(df.iq$total_cases)
df.iq.ts <- ts(df.iq$total_cases, frequency = 52, start = c(2000,26))
plot((df.iq.ts), xlab="Year",ylab="Total_cases", main= "Time series plot for Iquitos", col="blue")

summary(df.iq$total_cases)

#missing values
sapply(df.xreg.iq, function(x) sum(is.na(x)))


fit.nn.iq <- nnetar((total_cases.iq), xreg = df.xreg.iq_imp$ximp, repeats = 2)

df.test.iq <- fread("dengue_features_test_iq.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
df.test.iq.4imp <- fread("dengue_features_test_imp_iq.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
df.test.iq.imp <- missForest(df.test.iq.4imp, maxiter = 1)

fcast.nn.iq <- predict(fit.nn.iq,xreg=df.test.iq.imp$ximp, newdata=df.test.iq)
#plots
plot(fcast.nn.iq, ylab="total cases", xlab="week of the year")
checkresiduals(fit.nn.iq)
checkresiduals(fcast.nn.iq)
fcast.nn.iq <- round(fcast.nn.iq$mean, digits = 0)


#combine the 2 cities
sj.nnetar <- as.data.frame(fcast.nn.sj)
colnames(sj.nnetar) <- "total_cases"
sj.nnetar <- as.data.frame(sj.nnetar)
iq.nnetar <- as.data.frame(fcast.nn.iq)
colnames(iq.nnetar) <- "total_cases"
iq.nnetar <- as.data.frame(iq.nnetar)
fcast.twocity.preds <- rbind(sj.nnetar$total_cases,iq.nnetar$total_cases)#rbind gives unwabted things
write.csv(sj.nnetar,"twocity_model_sj_nnetar.csv")
write.csv(iq.nnetar,"twocity_model_iq_nnetar.csv")

# ### xgboost taking city, year, week of year into account ###
# library(quantmod); library(TTR); library(xgboost);
# model <- data.frame(cbind(df,df_xreg_imp$ximp))
# # Split data into train and test siqs 
# train_size = 2/3
# breakpoint = nrow(model) * train_size
# 
# training_data = model[1:breakpoint,]
# test_data = model[(breakpoint+1):nrow(model),]
# 
# # Split data training and test data into X and Y
# X_train = as.matrix(training_data[,c(1:3,5:24)]) ;Y_train = as.matrix(training_data[,4])
# class(X_train)[1]; class(Y_train)
# 
# X_test = as.matrix(test_data[,c(1:3,5:24)]) ; Y_test = as.matrix(test_data[,4])
# class(X_test)[1]; class(Y_test)
# 
# # Train the xgboost model using the "xgboost" function
# dtrain = xgb.DMatrix(data = X_train, label = Y_train)
# xgModel = xgboost(data = dtrain, nround = 5, objective = "reg:linear")
# 
# # Using cross validation
# dtrain = xgb.DMatrix(data = X_train, label = Y_train)
# cv = xgb.cv(data = dtrain, nround = 10, nfold = 5, objective = "reg:linear")
# 
# # Make the predictions on the test data
# df.test <- fread("dengue_features_test.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
# df.test_4imp <- fread("dengue_features_test_xreg_missforest_impute.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
# df.test_imp <- missForest(df.test_4imp, maxiter = 1)
# df.test_xgb <- (cbind(df.test[,2:4],df.test_imp$ximp))
# df.test_xgb <- as.matrix(df.test_xgb)
# preds = predict(xgModel,as.matrix(df.test_imp$ximp))
# preds.round.ts <- round(preds, digits = 0)
# write.csv(preds.round,"dengue_xgb.csv")

##### xgboost - ignores, city, year and week of year #####
### xgboost ###
library(quantmod); library(TTR); library(xgboost);
model <- data.frame(cbind(df,df_xreg_imp$ximp))
# Split data into train and test sets 
train_size = 2/3
breakpoint = nrow(model) * train_size

training_data = model[1:breakpoint,]
test_data = model[(breakpoint+1):nrow(model),]

# Split data training and test data into X and Y
X_train = as.matrix(training_data[,5:24]) ;Y_train = as.matrix(training_data[,4])
class(X_train)[1]; class(Y_train)

X_test = as.matrix(test_data[,5:24]) ; Y_test = as.matrix(test_data[,4])
class(X_test)[1]; class(Y_test)

#Train the XGBoost model on the training dataset
# Train the xgboost model using the "xgboost" function
dtrain = xgb.DMatrix(data = X_train, label = Y_train)
xgModel.2city = xgboost(data = dtrain, nround = 5, objective = "reg:linear")

# Using cross validation
dtrain = xgb.DMatrix(data = X_train, label = Y_train)
cv = xgb.cv(data = dtrain, nround = 10, nfold = 5, objective = "reg:linear")

# Make the predictions on the test data
preds = predict(xgModel, as.matrix(df.test_xreg_imp$ximp))
preds.round.xgb <- round(preds, digits = 0)
write.csv(preds.round,"dengue_xgb.csv")

model <- xgb.dump(xgModel, with.stats = T)
model[1:10] #This statement prints top 10 nodes of the model
# View feature importance from the learnt model
# names <- dimnames(data.matrix(df.test_xreg_imp$ximp))

# View the trees from a model
library(DiagrammeR)
xgb.plot.tree(model = xgModel)

# View only the first tree in the XGBoost model
xgb.plot.tree(model = xgModel, n_first_tree = 1)
names <- dimnames(data.matrix(df.test.sj.imp$ximp))
acimportance_matrix = xgb.importance(names,model = xgModel.2city)
print(importance_matrix)
xgb.plot.importance(importance_matrix)
barplot(importance_matrix)
##########################################################

#### split model with xgboost ####
#### sj ####
df <- fread("dengue_labels_train_rem_53_add_52_sj.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
# df <- df[df$city=="sj",]
df.xreg <- fread("dengue_labels_train_rem_53_add_52_sj_imp.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)

df.xreg_imp <- missForest(df.xreg, maxiter = 1)

model <- data.frame(cbind(df,df_xreg_imp$ximp))

# Split data into train and test sets 
train_size = 2/3
breakpoint = nrow(model) * train_size

training_data = model[1:breakpoint,]
test_data = model[(breakpoint+1):nrow(model),]

# Split data training and test data into X and Y
X_train = as.matrix(training_data[,5:24]) ;Y_train = as.matrix(training_data[,4])
class(X_train)[1]; class(Y_train)

X_test = as.matrix(test_data[,5:24]) ; Y_test = as.matrix(test_data[,4])
class(X_test)[1]; class(Y_test)

# Train the xgboost model using the "xgboost" function
dtrain = xgb.DMatrix(data = X_train, label = Y_train)
xgModel.sj = xgboost(data = dtrain, nround = 5, objective = "reg:linear")

# Using cross validation
dtrain = xgb.DMatrix(data = X_train, label = Y_train)
cv = xgb.cv(data = dtrain, nround = 10, nfold = 5, objective = "reg:linear")

names <- dimnames(data.matrix(df.test.iq.imp$ximp))
acimportance_matrix = xgb.importance(model = xgModel.sj)
print(importance_matrix)
xgb.plot.importance(importance_matrix)

# Make the predictions on the test data
df.test.sj <- fread("dengue_features_test_sj.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
df.test.sj.4imp <- fread("dengue_features_test_imp_sj.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
df.test.sj.imp <- missForest(df.test.sj.4imp, maxiter = 1)
preds = predict(xgModel.sj, as.matrix(df.test.sj.imp$ximp))
preds.round.sj <- round(preds, digits = 0)
# write.csv(preds.round,"dengue_xgb_sj.csv")

#### iq ####

df <- fread("dengue_labels_train_rem_53_add_52_iq.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
# df <- df[df$city=="iq",]
df.xreg <- fread("dengue_labels_train_rem_53_add_52_iq_imp.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)

df.xreg_imp <- missForest(df.xreg, maxiter = 1)

model <- data.frame(cbind(df,df_xreg_imp$ximp))

# Split data into train and test sets 
train_size = 2/3
breakpoint = nrow(model) * train_size

training_data = model[1:breakpoint,]
test_data = model[(breakpoint+1):nrow(model),]

# Split data training and test data into X and Y
X_train = as.matrix(training_data[,5:24]) ;Y_train = as.matrix(training_data[,4])
class(X_train)[1]; class(Y_train)

X_test = as.matrix(test_data[,5:24]) ; Y_test = as.matrix(test_data[,4])
class(X_test)[1]; class(Y_test)

# Train the xgboost model using the "xgboost" function
dtrain = xgb.DMatrix(data = X_train, label = Y_train)
xgModel.iq = xgboost(data = dtrain, nround = 5, objective = "reg:linear")

# Using cross validation
dtrain = xgb.DMatrix(data = X_train, label = Y_train)
cv = xgb.cv(data = dtrain, nround = 10, nfold = 5, objective = "reg:linear")

# Make the predictions on the test data
df.test.iq <- fread("dengue_features_test_iq.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
df.test.iq.4imp <- fread("dengue_features_test_imp_iq.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
df.test.iq.imp <- missForest(df.test.iq.4imp, maxiter = 1)
preds = predict(xgModel.iq, as.matrix(df.test.iq.imp$ximp))
preds.round.iq <- round(preds, digits = 0)
write.csv(preds.round,"dengue_xgb_iq.csv")

# View feature importance from the learnt model
importance_matrix = xgb.importance(model = xgModel.iq)
print(importance_matrix)
xgb.plot.importance(importance_matrix)

#####
#combine the 2 cities
sj <- as.data.frame(preds.round.sj)
colnames(sj) <- "total_cases"
iq <- as.data.frame(preds.round.iq)
colnames(iq) <- "total_cases"
twocity.preds <- rbind(sj,iq)
write.csv(twocity.preds$total_cases,"twocity_model_xgbo.csv")

en2.best <- fread("submission_format_en3_em_nn_xgbo.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)

en3 <- (twocity.preds$total_cases+en2.best$total_cases)/2
en3 <- round(en3, digits = 0)
write.csv(en3,"en3_abg_en2_twocityxgb.csv")

en4 <- ((en3)+(fcast.twocity.preds))/2
write.csv((round(en4,digits = 0)),"avg_en3_twocitynnetar.csv")
