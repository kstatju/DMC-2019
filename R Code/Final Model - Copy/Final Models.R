## Lasso

library(caret)
library(mda)
library(earth)
library(data.table)
library(glmnet)

path =  "D:/ISU/Data Mining Cup DMC/DMC 2019/R code/Final Model/"

Test  = fread(paste0(path, "test.csv") , header = T , sep="|")
Train = fread(paste0(path, "train.csv") , header = T , sep="|")
Train$fraud = as.factor(Train$fraud)

Train$X27_Nitems = Train$totalScanTimeInSeconds*Train$scannedLineItemsPerSecond
Train$X78_valueItem = Train$valuePerSecond * Train$scannedLineItemsPerSecond
Test$X27_Nitems = Test$totalScanTimeInSeconds*Test$scannedLineItemsPerSecond
Test$X78_valueItem = Test$valuePerSecond * Test$scannedLineItemsPerSecond
Train = as.data.frame(Train)

source(paste(path, "Feature Generate.R", sep = ""))

dt11 = feature.generate(Train = Train, Test = Test, path = path, standardize = F)
Train = as.data.frame(dt11[[1]])
Test = as.data.frame(dt11[[2]])

Train$ID = c(1:nrow(Train))
Test$fraud = NA
Test$ID = c(1:nrow(Test))


nm = names(Train)
xnm = nm[!(nm %in% c('fraud', 'ID'))]
df_train = upSample(x=Train[,xnm], 
                    y=as.factor(Train$fraud) , 
                    yname = "fraud")

cv.lasso <- cv.glmnet(x = as.matrix(df_train[,xnm]), 
                      y = df_train$fraud, 
                      alpha = 0.8783673, 
                      family = "binomial")
fit_glmnet = glmnet(x = as.matrix(df_train[,xnm]), y = df_train$fraud, 
             alpha = 0.8783673, 
             family = "binomial", 
             lambda = cv.lasso$lambda.min)

yhat_train_logit = predict(fit_glmnet, 
                     as.matrix(Train[,xnm]),
                     type = "response")

yhat_test_logit  = predict(fit_glmnet,
                     as.matrix(Test[,xnm]),
                     type = "response")

cutoff = 0.965

fraud_hat_logit_tr = ifelse(c(yhat_train_logit)>cutoff, 1,0)
fraud_hat_logit_ts = ifelse(c(yhat_test_logit)>cutoff, 1,0)

fraud_hat_logit_tr[which(Train$trustLevel>2)] = 0
fraud_hat_logit_ts[which(Test$trustLevel>2)] = 0


# Train_with_predict_logit = data.frame(Train, fraud_hat_logit = c(fraud_hat_logit_tr))
# Test_with_predict_logit = data.frame(Test, fraud_hat_logit = c(fraud_hat_logit_ts))
# 
# 
# Train_with_predict_logit$dttype = "Train"
# Test_with_predict_logit$dttype = "Test"
# dt = rbind(Train_with_predict_logit, Test_with_predict_logit)
# 
# 
# 
# prop.table(table(Train_with_predict_logit$fraud))
# prop.table(table(Train_with_predict_logit$fraud_hat_logit))
# prop.table(table(Test_with_predict_logit$fraud_hat_logit))
# a1 = data.frame(prop.table(table(Train_with_predict_logit$trustLevel, Train_with_predict_logit$fraud)))
# b1 = data.frame(prop.table(table(Train_with_predict_logit$trustLevel, Train_with_predict_logit$fraud_hat_logit)))
# c1 = data.frame(prop.table(table(Test_with_predict_logit$trustLevel, Test_with_predict_logit$fraud_hat_logit)))
# d1 = data.frame(a1, b1$Freq, c1$Freq)
# 
# 
# a2 = data.frame(prop.table(table(Train_with_predict_logit$lineItemVoids, Train_with_predict_logit$fraud)))
# b2 = data.frame(prop.table(table(Train_with_predict_logit$lineItemVoids, Train_with_predict_logit$fraud_hat_logit)))
# c2 = data.frame(prop.table(table(Test_with_predict_logit$lineItemVoids, Test_with_predict_logit$fraud_hat_logit)))
# d2 = data.frame(a2, b2$Freq, c2$Freq)
# 
# a3 = data.frame(prop.table(table(Train_with_predict_logit$scansWithoutRegistration, Train_with_predict_logit$fraud)))
# b3 = data.frame(prop.table(table(Train_with_predict_logit$scansWithoutRegistration, Train_with_predict_logit$fraud_hat_logit)))
# c3 = data.frame(prop.table(table(Test_with_predict_logit$scansWithoutRegistration, Test_with_predict_logit$fraud_hat_logit)))
# d3 = data.frame(a3, b3$Freq, c3$Freq)
# 
# 
# a3 = data.frame(prop.table(table(Train_with_predict_logit$quantityModifications, Train_with_predict_logit$fraud)))
# b3 = data.frame(prop.table(table(Train_with_predict_logit$quantityModifications, Train_with_predict_logit$fraud_hat_logit)))
# c3 = data.frame(prop.table(table(Test_with_predict_logit$quantityModifications, Test_with_predict_logit$fraud_hat_logit)))
# d3 = data.frame(a3, b3$Freq, c3$Freq)


##############################################################################
## MDA Model
##############################################################################


library(caret)
library(mda)
library(earth)
library(data.table)
library(glmnet)


path =  "D:/ISU/Data Mining Cup DMC/DMC 2019/R code/Final Model/"

Test  = fread(paste0(path, "test.csv") , header = T , sep="|")
Train = fread(paste0(path, "train.csv") , header = T , sep="|")
Train$fraud = as.factor(Train$fraud)

Train$X27_Nitems = Train$totalScanTimeInSeconds*Train$scannedLineItemsPerSecond
Train$X78_valueItem = Train$valuePerSecond * Train$scannedLineItemsPerSecond
Test$X27_Nitems = Test$totalScanTimeInSeconds*Test$scannedLineItemsPerSecond
Test$X78_valueItem = Test$valuePerSecond * Test$scannedLineItemsPerSecond
Train = as.data.frame(Train)

# source(paste(path, "Feature Generate.R", sep = ""))
# 
# dt11 = feature.generate(Train = Train, Test = Test, path = path, standardize = F)
# Train = as.data.frame(dt11[[1]])
# Test = as.data.frame(dt11[[2]])

Train$ID = c(1:nrow(Train))
Test$fraud = NA
Test$ID = c(1:nrow(Test))


nm = names(Train)
xnm = nm[!(nm %in% c('fraud', 'ID'))]
df_train = upSample(x=Train[,xnm], 
                    y=as.factor(Train$fraud) , 
                    yname = "fraud")

fit_mda = mda::mda(formula = fraud~., df_train, 
         subclasses = c(7,1), 
         method = gen.ridge, 
         lambda = 431)

yhat_train_mda = predict(fit_mda, newdata = Train, type = "posterior")[,2]
yhat_test_mda  = predict(fit_mda, newdata = Test , type = "posterior")[,2]

cutoff = 0.977
fraud_hat_mda_tr = ifelse(c(yhat_train_mda)>cutoff, 1,0)
fraud_hat_mda_ts = ifelse(c(yhat_test_mda)>cutoff, 1,0)

fraud_hat_mda_tr[which(Train$trustLevel>2)] = 0
fraud_hat_mda_ts[which(Test$trustLevel>2)] = 0


# Train_with_predict_mda = data.frame(Train, fraud_hat_mda = c(fraud_hat_mda_tr))
# Test_with_predict_mda = data.frame(Test, fraud_hat_mda = c(fraud_hat_mda_ts))
# 
# 
# Train_with_predict_mda$dttype = "Train"
# Test_with_predict_mda$dttype = "Test"
# dt = rbind(Train_with_predict_mda, Test_with_predict_mda)
# 
# table(Train_with_predict_mda$fraud, Train_with_predict_mda$fraud_hat_mda)
# 
# prop.table(table(Train_with_predict_mda$fraud))
# prop.table(table(Train_with_predict_mda$fraud_hat_mda))
# prop.table(table(Test_with_predict_mda$fraud_hat_mda))
# a1 = data.frame(prop.table(table(Train_with_predict_mda$trustLevel, Train_with_predict_mda$fraud)))
# b1 = data.frame(prop.table(table(Train_with_predict_mda$trustLevel, Train_with_predict_mda$fraud_hat_mda)))
# c1 = data.frame(prop.table(table(Test_with_predict_mda$trustLevel, Test_with_predict_mda$fraud_hat_mda)))
# d1 = data.frame(a1, b1$Freq, c1$Freq)
# 
# 
# a2 = data.frame(prop.table(table(Train_with_predict_mda$lineItemVoids, Train_with_predict_mda$fraud)))
# b2 = data.frame(prop.table(table(Train_with_predict_mda$lineItemVoids, Train_with_predict_mda$fraud_hat_mda)))
# c2 = data.frame(prop.table(table(Test_with_predict_mda$lineItemVoids, Test_with_predict_mda$fraud_hat_mda)))
# d2 = data.frame(a2, b2$Freq, c2$Freq)
# 
# a3 = data.frame(prop.table(table(Train_with_predict_mda$scansWithoutRegistration, Train_with_predict_mda$fraud)))
# b3 = data.frame(prop.table(table(Train_with_predict_mda$scansWithoutRegistration, Train_with_predict_mda$fraud_hat_mda)))
# c3 = data.frame(prop.table(table(Test_with_predict_mda$scansWithoutRegistration, Test_with_predict_mda$fraud_hat_mda)))
# d3 = data.frame(a3, b3$Freq, c3$Freq)
# 
# 
# a3 = data.frame(prop.table(table(Train_with_predict_mda$quantityModifications, Train_with_predict_mda$fraud)))
# b3 = data.frame(prop.table(table(Train_with_predict_mda$quantityModifications, Train_with_predict_mda$fraud_hat_mda)))
# c3 = data.frame(prop.table(table(Test_with_predict_mda$quantityModifications, Test_with_predict_mda$fraud_hat_mda)))
# d3 = data.frame(a3, b3$Freq, c3$Freq)





##############################################################################
## FDA Model
##############################################################################


library(caret)
library(mda)
library(earth)
library(data.table)
library(glmnet)


path =  "D:/ISU/Data Mining Cup DMC/DMC 2019/R code/Final Model/"

Test  = fread(paste0(path, "test.csv") , header = T , sep="|")
Train = fread(paste0(path, "train.csv") , header = T , sep="|")
Train$fraud = as.factor(Train$fraud)

Train$X27_Nitems = Train$totalScanTimeInSeconds*Train$scannedLineItemsPerSecond
Train$X78_valueItem = Train$valuePerSecond * Train$scannedLineItemsPerSecond
Test$X27_Nitems = Test$totalScanTimeInSeconds*Test$scannedLineItemsPerSecond
Test$X78_valueItem = Test$valuePerSecond * Test$scannedLineItemsPerSecond
Train = as.data.frame(Train)

# source(paste(path, "Feature Generate.R", sep = ""))
# 
# dt11 = feature.generate(Train = Train, Test = Test, path = path, standardize = F)
# Train = as.data.frame(dt11[[1]])
# Test = as.data.frame(dt11[[2]])

Train$ID = c(1:nrow(Train))
Test$fraud = NA
Test$ID = c(1:nrow(Test))


nm = names(Train)
xnm = nm[!(nm %in% c('ID'))]

fit_fda = mda::fda(formula = fraud~.,
         data = Train[,xnm],
         method = earth,
         degree          = 3,
         penalty         = 3.5,
         nk              = 80,
         thresh          = 1e-4,
         minspan         = 8,
         endspan         = 75,
         fast.k          = 0,
         pmethod         = "backward",
         nprune          = 14,
         nfold           = 1,
         ncross          = 1,
         Adjust.endspan  = 8,
         Get.leverages   = 0)


yhat_train_fda = predict(fit_fda, newdata = Train, type = "posterior")[,2]
yhat_test_fda  = predict(fit_fda, newdata = Test , type = "posterior")[,2]

cutoff = 0.99965
fraud_hat_fda_tr = ifelse(c(yhat_train_fda)>cutoff, 1,0)
fraud_hat_fda_ts = ifelse(c(yhat_test_fda)>cutoff, 1,0)

fraud_hat_fda_tr[which(Train$trustLevel>2)] = 0
fraud_hat_fda_ts[which(Test$trustLevel>2)] = 0


# Train_with_predict_fda = data.frame(Train, fraud_hat_fda = c(fraud_hat_fda_tr))
# Test_with_predict_fda = data.frame(Test, fraud_hat_fda = c(fraud_hat_fda_ts))
# 
# Train_with_predict_fda$dttype = "Train"
# Test_with_predict_fda$dttype = "Test"
# dt = rbind(Train_with_predict_fda, Test_with_predict_fda)
# 
# table(Train_with_predict_fda$fraud, Train_with_predict_fda$fraud_hat_fda)
# 
# prop.table(table(Train_with_predict_fda$fraud))
# prop.table(table(Train_with_predict_fda$fraud_hat_fda))
# prop.table(table(Test_with_predict_fda$fraud_hat_fda))
# a1 = data.frame(prop.table(table(Train_with_predict_fda$trustLevel, Train_with_predict_fda$fraud)))
# b1 = data.frame(prop.table(table(Train_with_predict_fda$trustLevel, Train_with_predict_fda$fraud_hat_fda)))
# c1 = data.frame(prop.table(table(Test_with_predict_fda$trustLevel, Test_with_predict_fda$fraud_hat_fda)))
# d1 = data.frame(a1, b1$Freq, c1$Freq)
# 
# 
# a2 = data.frame(prop.table(table(Train_with_predict_fda$lineItemVoids, Train_with_predict_fda$fraud)))
# b2 = data.frame(prop.table(table(Train_with_predict_fda$lineItemVoids, Train_with_predict_fda$fraud_hat_fda)))
# c2 = data.frame(prop.table(table(Test_with_predict_fda$lineItemVoids, Test_with_predict_fda$fraud_hat_fda)))
# d2 = data.frame(a2, b2$Freq, c2$Freq)
# 
# a3 = data.frame(prop.table(table(Train_with_predict_fda$scansWithoutRegistration, Train_with_predict_fda$fraud)))
# b3 = data.frame(prop.table(table(Train_with_predict_fda$scansWithoutRegistration, Train_with_predict_fda$fraud_hat_fda)))
# c3 = data.frame(prop.table(table(Test_with_predict_fda$scansWithoutRegistration, Test_with_predict_fda$fraud_hat_fda)))
# d3 = data.frame(a3, b3$Freq, c3$Freq)
# 
# 
# a3 = data.frame(prop.table(table(Train_with_predict_fda$quantityModifications, Train_with_predict_fda$fraud)))
# b3 = data.frame(prop.table(table(Train_with_predict_fda$quantityModifications, Train_with_predict_fda$fraud_hat_fda)))
# c3 = data.frame(prop.table(table(Test_with_predict_fda$quantityModifications, Test_with_predict_fda$fraud_hat_fda)))
# d3 = data.frame(a3, b3$Freq, c3$Freq)


################################################################################
## ensemble all
################################################################################

library(caret)
library(mda)
library(earth)
library(data.table)
library(glmnet)


path =  "D:/ISU/Data Mining Cup DMC/DMC 2019/R code/Final Model/"

Test  = fread(paste0(path, "test.csv") , header = T , sep="|")
Train = fread(paste0(path, "train.csv") , header = T , sep="|")
Train$fraud = as.factor(Train$fraud)

Train$X27_Nitems = Train$totalScanTimeInSeconds*Train$scannedLineItemsPerSecond
Train$X78_valueItem = Train$valuePerSecond * Train$scannedLineItemsPerSecond
Test$X27_Nitems = Test$totalScanTimeInSeconds*Test$scannedLineItemsPerSecond
Test$X78_valueItem = Test$valuePerSecond * Test$scannedLineItemsPerSecond
Train = as.data.frame(Train)
Test = as.data.frame(Test)
Train$ID = c(1:nrow(Train))
Test$ID = c(1:nrow(Test))
Test$fraud = NA

dt_names = c("ID",
             "trustLevel",
             "totalScanTimeInSeconds",
             "grandTotal",
             "lineItemVoids",
             "scansWithoutRegistration",
             "quantityModifications",
             "scannedLineItemsPerSecond",
             "valuePerSecond",
             "lineItemVoidsPerPosition",
             "X27_Nitems",
             "X78_valueItem",
             "fraud")
Train = Train[,dt_names]
Test = Test[,dt_names]

fraud_hat_c5_tr = unlist(read.csv(paste(path, "C50_predictions_train.csv", sep = "")))
fraud_hat_c5_ts = unlist(read.csv(paste(path, "C50_predictions.csv", sep = "")))


Train_with_predict = data.frame(Train, 
                                fraud_hat_logit = c(fraud_hat_logit_tr), 
                                fraud_hat_mda = c(fraud_hat_mda_tr),
                                fraud_hat_fda = c(fraud_hat_fda_tr),
                                fraud_hat_c5 = c(fraud_hat_c5_tr))


Test_with_predict = data.frame(Test, 
                               fraud_hat_logit = c(fraud_hat_logit_ts), 
                               fraud_hat_mda = c(fraud_hat_mda_ts),
                               fraud_hat_fda = c(fraud_hat_fda_ts),
                               fraud_hat_c5 = c(fraud_hat_c5_ts))

Train_with_predict$Final_prediction = NA
Test_with_predict$Final_prediction = NA

Train_with_predict$Final_count = rowSums(Train_with_predict[, c("fraud_hat_logit", "fraud_hat_mda", 
                                                                "fraud_hat_fda", "fraud_hat_c5")])

Test_with_predict$Final_count = rowSums(Test_with_predict[, c("fraud_hat_logit", "fraud_hat_mda", 
                                                                "fraud_hat_fda", "fraud_hat_c5")])

Train_with_predict$Final_prediction[Train_with_predict$Final_count>2]=1
Train_with_predict$Final_prediction[Train_with_predict$Final_count==2 & Train_with_predict$fraud_hat_logit==1 & Train_with_predict$fraud_hat_c5==1]=1
Train_with_predict$Final_prediction[(Train_with_predict$Final_count==2 & !(Train_with_predict$fraud_hat_logit==1 & Train_with_predict$fraud_hat_c5==1))]=0
Train_with_predict$Final_prediction[Train_with_predict$Final_count<2]=0

Test_with_predict$Final_prediction[Test_with_predict$Final_count>2]=1
Test_with_predict$Final_prediction[Test_with_predict$Final_count==2 & Test_with_predict$fraud_hat_logit==1 & Test_with_predict$fraud_hat_c5==1]=1
Test_with_predict$Final_prediction[(Test_with_predict$Final_count==2 & !(Test_with_predict$fraud_hat_logit==1 & Test_with_predict$fraud_hat_c5==1))]=0
Test_with_predict$Final_prediction[Test_with_predict$Final_count<2]=0


sum(Train_with_predict$Final_prediction)
sum(Test_with_predict$Final_prediction)
prop.table(table(Train_with_predict$Final_prediction))
prop.table(table(Test_with_predict$Final_prediction))

write.csv(data.frame(fraud = Test_with_predict$Final_prediction), file = "D:/ISU/Data Mining Cup DMC/DMC 2019/R code/Final Model/Uni_State_Iowa_1.csv", row.names = F, quote = F)

Train_with_predict$dttype = "Train"
Test_with_predict$dttype = "Test"
dt = rbind(Train_with_predict, Test_with_predict)

table(Train_with_predict$fraud, Train_with_predict$fraud_hat_logit)
table(Train_with_predict$fraud, Train_with_predict$fraud_hat_mda)
table(Train_with_predict$fraud, Train_with_predict$fraud_hat_fda)

a1 = as.data.frame(table(Train_with_predict$trustLevel, Train_with_predict$fraud))
a2 = as.data.frame(table(Train_with_predict$trustLevel, Train_with_predict$fraud_hat_logit))
a3 = as.data.frame(table(Train_with_predict$trustLevel, Train_with_predict$fraud_hat_mda))
a4 = as.data.frame(table(Train_with_predict$trustLevel, Train_with_predict$fraud_hat_fda))
a5 = as.data.frame(table(Train_with_predict$trustLevel, Train_with_predict$fraud_hat_c5))

a10 = as.data.frame(table(Test_with_predict$trustLevel, Test_with_predict$fraud_hat_logit))
a11 = as.data.frame(table(Test_with_predict$trustLevel, Test_with_predict$fraud_hat_mda))
a12 = as.data.frame(table(Test_with_predict$trustLevel, Test_with_predict$fraud_hat_fda))
a13 = as.data.frame(table(Test_with_predict$trustLevel, Test_with_predict$fraud_hat_c5))
a = data.frame(trustLevel = a1$Var1, fraud = a1$Var2, true_freq = a1$Freq,
               train_logit = a2$Freq, train_mda = a3$Freq, train_fda = a4$Freq,
               train_c5 = a5$Freq,
               test_logit = a10$Freq, test_mda = a11$Freq, test_fda = a12$Freq,
               test_c5 = a13$Freq)



# prop.table(table(Train_with_predict$fraud))
# prop.table(table(Train_with_predict$fraud_hat_fda))
# prop.table(table(Test_with_predict$fraud_hat_fda))
# a1 = data.frame(prop.table(table(Train_with_predict$trustLevel, Train_with_predict$fraud)))
# b1 = data.frame(prop.table(table(Train_with_predict$trustLevel, Train_with_predict$fraud_hat_fda)))
# c1 = data.frame(prop.table(table(Test_with_predict$trustLevel, Test_with_predict$fraud_hat_fda)))
# d1 = data.frame(a1, b1$Freq, c1$Freq)
# 
# 
# a2 = data.frame(prop.table(table(Train_with_predict$lineItemVoids, Train_with_predict$fraud)))
# b2 = data.frame(prop.table(table(Train_with_predict$lineItemVoids, Train_with_predict$fraud_hat_fda)))
# c2 = data.frame(prop.table(table(Test_with_predict$lineItemVoids, Test_with_predict$fraud_hat_fda)))
# d2 = data.frame(a2, b2$Freq, c2$Freq)
# 
# a3 = data.frame(prop.table(table(Train_with_predict$scansWithoutRegistration, Train_with_predict$fraud)))
# b3 = data.frame(prop.table(table(Train_with_predict$scansWithoutRegistration, Train_with_predict$fraud_hat_fda)))
# c3 = data.frame(prop.table(table(Test_with_predict$scansWithoutRegistration, Test_with_predict$fraud_hat_fda)))
# d3 = data.frame(a3, b3$Freq, c3$Freq)
# 
# 
# a3 = data.frame(prop.table(table(Train_with_predict$quantityModifications, Train_with_predict$fraud)))
# b3 = data.frame(prop.table(table(Train_with_predict$quantityModifications, Train_with_predict$fraud_hat_fda)))
# c3 = data.frame(prop.table(table(Test_with_predict$quantityModifications, Test_with_predict$fraud_hat_fda)))
# d3 = data.frame(a3, b3$Freq, c3$Freq)
# 
