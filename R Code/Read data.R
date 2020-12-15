train = read.csv(file = "D:/ISU/Data Mining Cup DMC/DMC 2019/Data/train_TL12.csv", header = T, sep = ",")
test = read.csv(file = "D:/ISU/Data Mining Cup DMC/DMC 2019/Data/test_TL12.csv", header = T, sep = ",")
train$ID <- seq.int(nrow(train))
test$ID <- seq.int(nrow(test))


test$fraud = NA
train$dttype = "Train"
test$dttype = "Test"
combin_dt = rbind(train, test)
combin_dt$totalScanTimeInSeconds = log(combin_dt$totalScanTimeInSeconds+1)
combin_dt$grandTotal = log(combin_dt$grandTotal+1)
combin_dt$scannedLineItemsPerSecond = log(combin_dt$scannedLineItemsPerSecond+1)
combin_dt$valuePerSecond = log(combin_dt$valuePerSecond+1)
combin_dt$lineItemVoidsPerPosition = log(combin_dt$lineItemVoidsPerPosition+1)

combin_dt1 = combin_dt[combin_dt$totalScanTimeInSeconds>5.5,]
combin_dt1 = combin_dt1[combin_dt1$scannedLineItemsPerSecond<.1,]
combin_dt1 = combin_dt1[combin_dt1$valuePerSecond<.2,]
combin_dt1 = combin_dt1[combin_dt1$lineItemVoidsPerPosition<.5,]

source('feature_create_data.R')
path = "D:/ISU/Data Mining Cup DMC/DMC 2019/R code/feature_create/Final_data_with_feature"
dt = data_feature(path)

aa <- function(dt)
{
  smp_size <- floor(0.75 * nrow(dt))
  
  ## set the seed to make your partition reproducible
  
  train_ind <- sample(seq_len(nrow(dt)), size = smp_size)
  
  tr <- dt[train_ind, ]
  tr$fraud = as.factor(tr$fraud)
  te <- dt[-train_ind, ]
  te$fraud = as.factor(te$fraud)
  
  bstDense <- xgboost(data = as.matrix(tr[,-c(2)]), 
                      label = as.numeric(as.character(tr$fraud)), max.depth = 2, 
                      eta = .1, nthread = 4, nrounds = 2000, 
                      objective = "binary:logistic", verbose = F)
  
  pp = predict(bstDense, as.matrix(te[,-c(2)]))
  
  rf.md <- randomForest(as.factor(fraud) ~ ., data = tr, ntree = 1500)
  pred = predict(rf.md, te)
  b = data.frame(fr = as.numeric(as.character(te$fraud)), pr = as.numeric(as.character(pred)), xp = ifelse(pp>.9,1,0))
  return(b)
}

pred = sapply(1:20, function(x) aa(dt$data))
a = data.frame(pred)
c = NULL
for (i in seq(1,60,3)){
  c = rbind(c,as.data.frame(table(pred[[i]], pred[[i+2]])))
}

library(xgboost)
library(DiagrammeR)
bstDense <- xgboost(data = as.matrix(tr[,-c(6)]), 
                    label = as.numeric(as.character(tr$fraud)), max.depth = 3, 
                    eta = .1, nthread = 4, nrounds = 2000, 
                    objective = "binary:logistic", verbose = F)

pp = predict(bstDense, as.matrix(te[,-c(6)]))
xgb.plot.tree(model = bstDense, trees = 0:3, show_node_id = TRUE, 
              width=3000, height=4000)
pp = data.frame(y = te$fraud, pred = pp)
pp$ypred = ifelse(pp$pred>.9,1,0)
table(pp$ypred, pp$y)
plot(rf.md, type = 'simple')

mean(c[c$Var1==1 & c$Var2 ==0, 3])
mean(c[c$Var1==0 & c$Var2 ==1, 3])
