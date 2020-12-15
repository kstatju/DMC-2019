# #------------------------------------------------------------------------------------
# #                                   Load Libraries
# #------------------------------------------------------------------------------------
# library(randomForest)
# library(rpart)
# library(ranger)
# library(caret)
# library(data.table)
# library(h2o)
# library(dplyr)
# library(gtools)
# library(DMwR)
# library(reader)
# library(glmnet)
# library(xgboost)
# library(MASS)
# library(klaR)
# library(mda)
# library(earth)
# h2o.init()
# #------------------------------------------------------------------------------------
# #                                Experiment configuration 
# #-----------------------------------------------------------------------------------
# start         = 1                       # start tune experiment id
# end           = 1                       # end tune experiment id  RF:162 Nnet:180 Xg:1458
# Model         = "Logistic"                   # Model used to train
# 
# F_smote       = T                       # apply SMOTE so solve class imbalance
# n_folds       = 5                       # number of folds for cross validation
# n_trial       = 10                      # number of trials for repeated CV
# cutoff        = seq(0.99,0.9,by = -0.001)# probability cutoff for predicting fraud
# 
# #------------------------------------------------------------------------------------
# Test  = fread("file:///D:/ISU/Data Mining Cup DMC/DMC 2019/R code/feature_create/Final_data_with_feature/test.csv" , header = T , sep="|")
# Train = fread("file:///D:/ISU/Data Mining Cup DMC/DMC 2019/R code/feature_create/Final_data_with_feature/train.csv" , header = T , sep="|")
# Train$fraud = as.factor(Train$fraud)
# 
# Train$X27_Nitems = Train$totalScanTimeInSeconds*Train$scannedLineItemsPerSecond
# Train$X78_valueItem = Train$valuePerSecond * Train$scannedLineItemsPerSecond
# Test$X27_Nitems = Test$totalScanTimeInSeconds*Test$scannedLineItemsPerSecond
# Test$X78_valueItem = Test$valuePerSecond * Test$scannedLineItemsPerSecond


ptm <- proc.time()

#------------------------------------------------------------------------------------
Parallel_function <- function(i, Train, e){
  
  #------------------------------------------------------------------------------------
  #                                   Load Libraries
  #------------------------------------------------------------------------------------
  # library(randomForest)
  library(rpart)
  library(ranger)
  library(caret)
  library(data.table)
  # library(h2o)
  library(dplyr)
  library(gtools)
  library(DMwR)
  library(reader)
  library(glmnet)
  library(xgboost)
  library(MASS)
  library(klaR)
  library(mda)
  library(earth)

  # h2o.init()
  #------------------------------------------------------------------------------------
  #                                Experiment configuration 
  #-----------------------------------------------------------------------------------
  start         = 1                       # start tune experiment id
  end           = 1                       # end tune experiment id  RF:162 Nnet:180 Xg:1458
  Model         = "Logistic"                   # Model used to train
  Sampling      = "up"
  
  F_smote       = T                       # apply SMOTE so solve class imbalance
  n_folds       = 5                       # number of folds for cross validation
  n_trial       = 100                      # number of trials for repeated CV
  cutoff        = seq(0.995, 0.91, by = -0.001)# probability cutoff for predicting fraud
  
  
  My_ConfusionMatrix  = function(pred = NULL , ref = NULL, Positive_Class = "1" , Negative_Class = "0"){
    
    TP  = length(which(pred == Positive_Class & ref == Positive_Class))
    TN  = length(which(pred == Negative_Class & ref == Negative_Class))
    FP  = length(which(pred == Positive_Class & ref == Negative_Class))
    FN  = length(which(pred == Negative_Class & ref == Positive_Class))
    CM  = matrix(c(TP,FP,FN,TN) , nrow=2 , byrow = T)
    
    AC  = (TP + TN) / (TP + FP + FN + TN)
    SN  = TP/(TP + FN)
    SP  = TN/(TN + FP)
    l   = TP*5 - FP*25 - FN*5
    
    return(list( Accuracy = AC , Sensitivity = SN , Specificity = SP , ConfusionMatrix = CM , loss = l))
  }
  
  Measure_Performance_prob = function(pred= NULL,  ref = NULL, prob_thr = NULL){
    
    CM_list    = lapply(c(1:length(prob_thr)) , function(x) My_ConfusionMatrix(ifelse(pred >=prob_thr[x] ,"1" , "0") , ref ))
    
    AC  = sapply  (c(1:length(prob_thr)) , function(x) CM_list[[x]]$Accuracy)
    SP  = sapply  (c(1:length(prob_thr)) , function(x) CM_list[[x]]$Specificity)
    SN  = sapply  (c(1:length(prob_thr)) , function(x) CM_list[[x]]$Sensitivity)
    CM  = sapply  (c(1:length(prob_thr)) , function(x) CM_list[[x]]$ConfusionMatrix)
    Loss= sapply  (c(1:length(prob_thr)) , function(x) CM_list[[x]]$loss)
    
    
    
    return(list( predictions  = as.matrix(pred),
                 Accuracy     = round(as.numeric(AC ) ,digits = 2),
                 Specificity  = round(as.numeric(SP ) ,digits = 2),
                 Sensitivity  = round(as.numeric(SN ) ,digits = 2),
                 ConfusionMatrix  = CM,
                 PredictionCost = Loss))
  }
  
  
  build_datasets = function(df , n_folds=5, seed = NULL){
    
    if(!is.null(seed)) set.seed(seed)
    
    n_obs = nrow(df)  
    fraud_index     = which(df$fraud == 1)
    non_fraud_index = which(df$fraud == 0)
    
    fraud_folds     = createFolds(fraud_index     , k = n_folds+1)
    non_fraud_folds = createFolds(non_fraud_index , k = n_folds+1)
    
    folds = lapply(c(1:(n_folds+1)) , function(x){
      c(fraud_folds[[x]] , non_fraud_folds[[x]])
    })
    
    train_folds = list()
    test_folds = list()
    for(i in 1:n_folds){
      train_folds[[i]] = c(1:n_obs)[-c(folds[[i]] , folds[[n_folds+1]])]
      test_folds[[i]]  = folds[[i]]
    }
    
    return(list(train_index = train_folds , test_index = test_folds))
    
  }
  
  Percentile_detector = function(df=NULL , Feature = NULL , start_q = 0.1 , end_q = 1){
    start_val = quantile(df[,get(Feature)], start_q)
    end_val   = quantile(df[,get(Feature)], end_q)
    return (df[,get(Feature)] >=start_val & df[,get(Feature)]<=end_val )
  }
  chunk  = function(x, n) split(x, sort(rank(x) %% n))
  Conditional_outlier = function(df = NULL , Base = NULL, Feature = NULL , start_q = 0.1 , end_q = 1, n_splits = 10){
    
    Base_order = order(df[,get(Base)])
    print(head(Base_order))
    Base_folds = chunk(Base_order , n_splits)
    output = rep(F , nrow(df))
    for(x in c(1:n_splits)){
      x_index = Base_folds[[x]]
      output[x_index] = Percentile_detector(df[x_index,] , Feature , start_q  , end_q )
    }
    return(output)
    
  }
  
  
  #------------------------------------------------------------------------------------
  # Build Set of parameters
  #------------------------------------------------------------------------------------
  
  Config_Logistic   = function(){
    Exp_Params = list()
    Exp_Params[[1]] = seq(0.7, 0.8, length.out = 30)                       #alpha
        
    Experiment = expand.grid(Exp_Params)
    Experiment = data.frame(id = 1:nrow(Experiment), Experiment)
    colnames(Experiment) = c('id', paste0("P", 1))
    
    
    Exp_cols = c("P1.alpha")
    
    return(list(Exp = Experiment , names = Exp_cols , params = Exp_Params))
    
    
  }
  
  #------------------------------------------------------------------------------------
  # generate list of parameters from a pool of experiments
  #------------------------------------------------------------------------------------
  Set_Logistic_Params   = function(Exp = NULL , id = 1){
    
    p = list(alpha = Exp$P1[id])
    return(params = p)
  }
  
  
  Set_params      = function (Model, e){
    if(Model == "Logistic"){return(Set_Logistic_Params(Logistic_Exp, e))}
  }
  
  #------------------------------------------------------------------------------------
  # Fit a model on data
  #------------------------------------------------------------------------------------
  
  Fit_Logistic   = function(df , df_folds = NULL , n_folds = 5, params = NULL){

    
    Logistic_models = lapply(c(1:n_folds) , function(x){
      if(Sampling == "down"){
        df_train = downSample(x=df[df_folds$train_index[[x]],-"fraud"] , 
                              y=as.factor(df[df_folds$train_index[[x]],]$fraud) , 
                              yname = "fraud")
      }
      if(Sampling == "up"){
        nm = names(df)
        xnm = nm[!(nm%in%c('fraud'))]
        df_train = upSample(x=df[df_folds$train_index[[x]],xnm] , 
                            y=as.factor(df[df_folds$train_index[[x]],]$fraud) , 
                            yname = "fraud")
      }
      if(Sampling == "smote"){
        df_train = SMOTE(fraud~.,data = df[df_folds$train_index[[x]],])
      }
      if(Sampling == "none"){
        df_train = as.data.frame(df[df_folds$train_index[[x]],])
      }
      
    	nm = names(df_train)
    	xnm = nm[!(nm%in%c('fraud'))]

    	cv.lasso <- cv.glmnet(x = as.matrix(df_train[,xnm]), 
    	                      y = df_train$fraud, 
    	                      alpha = params[[1]], 
    	                      family = "binomial")
      fit = glmnet(x = as.matrix(df_train[,xnm]), y = df_train$fraud, 
             alpha = params[[1]], 
             family = "binomial", 
             lambda = cv.lasso$lambda.min)
      return(list(fit = fit, lambda = cv.lasso$lambda.min))
    })
    
    nm = names(df)
    xnm = nm[!(nm%in%c('fraud'))]
    
    yhat_train = lapply(c(1:n_folds), function(x) predict(Logistic_models[[x]][[1]] , 
                                                          as.matrix(df[df_folds$train_index[[x]],xnm]),
                                                          type = "response"))
    yhat_test  = lapply(c(1:n_folds), function(x) predict(Logistic_models[[x]][[1]] , 
                                                          as.matrix(df[df_folds$test_index[[x]],xnm]),
                                                          type = "response"))
    lambda = unlist(lapply(c(1:n_folds), function(x) Logistic_models[[x]][[2]]))
    
    return(list(model      = Logistic_models, 
                Params     = params,
                yhat_test  = yhat_test,
                yhat_train = yhat_train,
                lambda     = lambda))  
  
  }
  
  Train_model = function(Model = NULL , df = NULL , df_folds = NULL , n_folds=5 , params=NULL){
    if(Model == "Logistic"){return(Fit_Logistic(df , df_folds ,n_folds ,params ))}
  }


  
  
  #------------------------------------------------------------------------------------
  #                 configure the training model tuning grid
  #------------------------------------------------------------------------------------
  if(Model == "Logistic"){
    Logistic_Config = Config_Logistic()
    Logistic_Exp    = Logistic_Config$Exp
    Exp_Params  = Logistic_Config$params
    tune_names  = Logistic_Config$names
    ncombin = nrow(Logistic_Exp)
  }
  
  
  
  
  print(paste0("----Trial " , i))
  df_folds = build_datasets(Train , n_folds)
  # for (k in 1:ncombin){
  all_cutoff = function(k){
    params = Set_params(Model[e], k)
    trial_model = Train_model(Model[e], Train , df_folds , n_folds , params)
    Exp_out_1 = NULL
    Tunning_out = NULL
    
    for(p in 1:length(cutoff)){
      Train_out = lapply(c(1:n_folds) , function(x)Measure_Performance_prob(pred= trial_model$yhat_train[[x]],  
                                                                            ref = Train$fraud[df_folds$train_index[[x]]], 
                                                                            prob_thr = cutoff[p]) )
      
      Test_out = lapply(c(1:n_folds) , function(x)Measure_Performance_prob(pred= trial_model$yhat_test[[x]],  
                                                                           ref = Train$fraud[df_folds$test_index[[x]]], 
                                                                           prob_thr = cutoff[p]) )
      
      Exp_out = data.frame(  "Exp_ID"               = sapply(c(1:n_folds) , function(x) c(e)),
                             "Cutoff"               = sapply(c(1:n_folds) , function(x) cutoff[p]),
                             "Trial"                = sapply(c(1:n_folds) , function(x) c(i)),
                             "Folds"                = sapply(c(1:n_folds) , function(x) c(x)),
                             "Lambda"               = trial_model$lambda,
                             "Train_Accuracy"       = sapply(c(1:n_folds) , function(x) Train_out[[x]]$Accuracy),
                             "Train_Sensitivity"    = sapply(c(1:n_folds) , function(x) Train_out[[x]]$Sensitivity),
                             "Train_Specificity"    = sapply(c(1:n_folds) , function(x) Train_out[[x]]$Specificity),
                             "Train_PredictionCost" = sapply(c(1:n_folds) , function(x) Train_out[[x]]$PredictionCost),
                             "Train_MissingFraud"   = sapply(c(1:n_folds) , function(x) Train_out[[x]]$ConfusionMatrix[2]),
                             "Train_FalseAlarm"     = sapply(c(1:n_folds) , function(x) Train_out[[x]]$ConfusionMatrix[3]),
                             
                             "Test_Accuracy"        = sapply(c(1:n_folds) , function(x) Test_out[[x]]$Accuracy),
                             "Test_Sensitivity"     = sapply(c(1:n_folds) , function(x) Test_out[[x]]$Sensitivity),
                             "Test_Specificity"     = sapply(c(1:n_folds) , function(x) Test_out[[x]]$Specificity),
                             "Test_PredictionCost"  = sapply(c(1:n_folds) , function(x) Test_out[[x]]$PredictionCost),
                             "Test_MissingFraud"    = sapply(c(1:n_folds) , function(x) Test_out[[x]]$ConfusionMatrix[2]),
                             "Test_FalseAlarm"      = sapply(c(1:n_folds) , function(x) Test_out[[x]]$ConfusionMatrix[3])
      )
      Exp_out_1 = rbind(Exp_out_1, Exp_out)
      # Exp_out = data.frame(
      #   "Exp_ID" = e,
      #   "Cutoff" = cutoff[p],
      #   "Trial"  = i,
      #   "Train_Accuracy"       = mean(folds_out$Train_Accuracy),
      #   "Train_Sensitivity"    = mean(folds_out$Train_Sensitivity),
      #   "Train_Specificity"    = mean(folds_out$Train_Specificity),
      #   "Train_PredictionCost" = mean(folds_out$Train_PredictionCost),
      #   "Train_MissingFraud"   = mean(folds_out$Train_MissingFraud),
      #   "Train_FalseAlarm"     = mean(folds_out$Train_FalseAlarm),
      #   
      #   "Test_Accuracy"        = mean(folds_out$Test_Accuracy),
      #   "Test_Sensitivity"     = mean(folds_out$Test_Sensitivity),
      #   "Test_Specificity"     = mean(folds_out$Test_Specificity),
      #   "Test_PredictionCost"  = mean(folds_out$Test_PredictionCost),
      #   "Test_MissingFraud"    = mean(folds_out$Test_MissingFraud),
      #   "Test_FalseAlarm"      = mean(folds_out$Test_MissingFraud)
      # )
      
    } #end of cutoff loop
    
    param_out = matrix ( rep( unlist(params) , n_folds*length(cutoff)) , ncol = length(params) ,byrow = T)
    colnames(param_out) = tune_names
    Exp_out_2 = cbind(Exp_out_1 , param_out)
    Tunning_out = rbind(Tunning_out , Exp_out_2)
    return(Tunning_out)
  }
  all_res = lapply(c(1:ncombin), FUN = function(k) all_cutoff(k))
  Tunning_out = NULL
  for (i in 1:ncombin){
    Tunning_out = rbind(Tunning_out, all_res[[i]])
  }
  # } #end loop for all prem combin
  
  return(Tunning_out)
}
#------------------------------------------------------------------------------------
library(rpart)
library(ranger)
library(caret)
library(data.table)
# library(h2o)
library(dplyr)
library(gtools)
library(DMwR)
library(reader)
library(glmnet)
library(xgboost)
library(MASS)
library(klaR)
library(mda)
library(earth)
library(parallel)

path = "/work/STAT/kanak/"      #"D:/ISU/Data Mining Cup DMC/DMC 2019/R code/Condo Out using new feature/"

Test  = fread(paste0(path, "test.csv") , header = T , sep="|")
Train = fread(paste0(path, "train.csv") , header = T , sep="|")
Train$fraud = as.factor(Train$fraud)

Train$X27_Nitems = Train$totalScanTimeInSeconds*Train$scannedLineItemsPerSecond
Train$X78_valueItem = Train$valuePerSecond * Train$scannedLineItemsPerSecond
Test$X27_Nitems = Test$totalScanTimeInSeconds*Test$scannedLineItemsPerSecond
Test$X78_valueItem = Test$valuePerSecond * Test$scannedLineItemsPerSecond

source(paste(path, "Feature Generate.R", sep = ""))


dt11 = feature.generate(Train = Train, Test = Test, path = path, standardize = T)
Train = as.data.frame(dt11[[1]])
Test = as.data.frame(dt11[[2]])


#------------------------------------------------------------------------------------
# Tune Model
#------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------
#                                Experiment configuration 
#-----------------------------------------------------------------------------------
start         = 1                       # start tune experiment id
end           = 1                       # end tune experiment id  RF:162 Nnet:180 Xg:1458
Model         = "Logistic"                   # Model used to train
Sampling      = "up"

n_folds       = 5                       # number of folds for cross validation
n_trial       = 100                      # number of trials for repeated CV

Tunning_out = NULL

for (e in start:end){
  
  print(paste0("--Experiment " , e))
  
  Tunning_out = NULL
  
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  
  print(paste("Number of coreS working: ", no_cores, sep = ""))
  
  clexp = list("Parallel_function", "Train", "e")
  clusterExport(cl, clexp)
  
  
  
  # Compare time to run function using parApply and apply
  
  fold_estimates <-  parLapplyLB(cl = cl, X = 1:n_trial, fun = function(i) Parallel_function(i = i, Train = Train, e = e))
  
  stopCluster(cl)
  
  
  for (i in 1:n_trial){
    Tunning_out = rbind(Tunning_out , fold_estimates[[i]])  

  }
  
  write.csv(Tunning_out , paste0(path, Model , "_" , Sampling, "_Tunning_out_set1_std.csv") , row.names = F)
  
  summary_table = Tunning_out %>% group_by(P1.alpha, Cutoff) %>%
    summarise_at(c("Lambda"                 ,
                   "Train_Accuracy"         ,
                   "Train_Sensitivity"      ,
                   "Train_Specificity"      ,
                   "Train_PredictionCost"   ,
                   "Train_MissingFraud"     ,
                   "Train_FalseAlarm"       ,
                   "Test_Accuracy"          ,
                   "Test_Sensitivity"       ,
                   "Test_Specificity"       ,
                   "Test_PredictionCost"    ,
                   "Test_MissingFraud"      ,
                   "Test_FalseAlarm"), .funs = c(mean = mean, var = var, max = max, min = min))
  
  write.csv(summary_table , paste0(path, Model , "_" , Sampling, "_Tunning_summary_out_set1_std.csv") , row.names = F)
  
  # summary_table = Tunning_out %>% group_by(P1.alpha, Cutoff, Trial) %>%
  #   summarise_at(c("Lambda"                 ,
  #                  "Train_Accuracy"         ,
  #                  "Train_Sensitivity"      ,
  #                  "Train_Specificity"      ,
  #                  "Train_PredictionCost"   ,
  #                  "Train_MissingFraud"     ,
  #                  "Train_FalseAlarm"       ,
  #                  "Test_Accuracy"          ,
  #                  "Test_Sensitivity"       ,
  #                  "Test_Specificity"       ,
  #                  "Test_PredictionCost"    ,
  #                  "Test_MissingFraud"      ,
  #                  "Test_FalseAlarm"), .funs = c(mean))
  # 
  # summary_table = summary_table %>% group_by(P1.alpha, Cutoff) %>%
  #   summarise_at(c("Lambda"                 ,
  #                  "Train_Accuracy"         ,
  #                  "Train_Sensitivity"      ,
  #                  "Train_Specificity"      ,
  #                  "Train_PredictionCost"   ,
  #                  "Train_MissingFraud"     ,
  #                  "Train_FalseAlarm"       ,
  #                  "Test_Accuracy"          ,
  #                  "Test_Sensitivity"       ,
  #                  "Test_Specificity"       ,
  #                  "Test_PredictionCost"    ,
  #                  "Test_MissingFraud"      ,
  #                  "Test_FalseAlarm"), .funs = c(mean = mean, var = var, max = max, min = min))
  # 
  # 
  # 
  # write.csv(summary_table , paste0(path, Model , "_" , Sampling, "_Tunning_summary_out_byTrial.csv") , row.names = F)
  
}#end of tuning loop


print(proc.time() - ptm)

