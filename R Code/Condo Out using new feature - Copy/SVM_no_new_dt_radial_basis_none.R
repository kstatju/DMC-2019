#------------------------------------------------------------------------------------
#                                   Load Libraries
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
# h2o.init()
library(e1071)
#h2o.init()
#------------------------------------------------------------------------------------
#                                Experiment configuration 
#-----------------------------------------------------------------------------------
start         = 1                       # start tune experiment id
end           = 1536                     # end tune experiment id  RF:162 Nnet:180 Xg:1458 svm:
Model         = "svm"                   # Model used to train
Sampling      = "up"                    # sampling method none,smote,up,down
n_folds       = 10                       # number of folds for cross validation
n_trial       = 50                      # number of trials for repeated CV
cutoff        = seq(0.99,0.6,by = -0.01)# probability cutoff for predicting fraud

#------------------------------------------------------------------------------------
path = "/work/STAT/kanak/"

Test  = fread(paste(path, "test.csv", sep = "") , header = T , sep="|")
Train = fread(paste(path, "train.csv",sep = "") , header = T , sep="|")
Train$fraud = as.factor(Train$fraud)

Train$X27_Nitems = Train$totalScanTimeInSeconds*Train$scannedLineItemsPerSecond
Train$X78_valueItem = Train$valuePerSecond * Train$scannedLineItemsPerSecond
Test$X27_Nitems = Test$totalScanTimeInSeconds*Test$scannedLineItemsPerSecond
Test$X78_valueItem = Test$valuePerSecond * Test$scannedLineItemsPerSecond

# source(paste("Feature Generate.R" , sep = ""))
# 
# dt11 = feature.generate(Train = Train, Test = Test, path = path, standardize = F)
# Train = as.data.frame(dt11[[1]])
# Test = as.data.frame(dt11[[2]])
Train = as.data.frame(Train)
Test = as.data.frame(Test)

#------------------------------------------------------------------------------------

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


build_datasets = function(df , n_folds=5){
  
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

#------------------------------------------------------------------------------------
# Build Set of parameters
#------------------------------------------------------------------------------------
Config_Nnet = function(){
  
  Nodes         = c(200,500)              # nunmber of hidden noes
  Layers        = c(3:12)                 # number of hidden layers
  
  Exp_Params = list()
  
  Exp_Params[[1]]  = c("Rectifier", 
                       "Tanh", 
                       "TanhWithDropout", 
                       "RectifierWithDropout")        #activation
  Exp_Params[[2]]  = list( c(rep(Nodes[1],Layers[1])), 
                           c(rep(Nodes[1],Layers[2])), 
                           c(rep(Nodes[2],Layers[1])), 
                           c(rep(Nodes[2],Layers[2])),
                           
                           c(rep(Nodes[1],Layers[3])), 
                           c(rep(Nodes[1],Layers[4])), 
                           c(rep(Nodes[2],Layers[3])), 
                           c(rep(Nodes[2],Layers[4])),
                           
                           c(rep(Nodes[1],Layers[5])), 
                           c(rep(Nodes[1],Layers[6])), 
                           c(rep(Nodes[2],Layers[5])), 
                           c(rep(Nodes[2],Layers[6])),
                           
                           c(rep(Nodes[1],Layers[7])), 
                           c(rep(Nodes[1],Layers[8])), 
                           c(rep(Nodes[2],Layers[7])), 
                           c(rep(Nodes[2],Layers[8])),
                           
                           c(rep(Nodes[1],Layers[9])), 
                           c(rep(Nodes[1],Layers[10])), 
                           c(rep(Nodes[2],Layers[9])), 
                           c(rep(Nodes[2],Layers[10])))#hidden
  Exp_Params[[3]]  = c(10, 15, 5)                      #epochs
  Exp_Params[[4]]  = c(0.005, 0.001, 0.025)            #rate alpha
  Exp_Params[[5]]  = c(0, 0.1, 0.2)                    #L2
  Exp_Params[[6]]  = c("Automatic", 
                       "CrossEntropy",
                       "Quadratic")                   #loss
  
  permu = gtools:: permutations(n=6,r=6  , set = F , repeats.allowed=T)
  keep  = which(permu[,1] ==1 & permu[,2] <=4 & permu[,3] <=3 & permu[,4]  <=3 & permu[,5]  ==1 & permu[,6] ==2)
  permu = permu[keep,]
  permu = apply(permu , 2, rep , 5)
  permu[-1,2] = sapply(c(2:nrow(permu)) , function(x) permu[x,2] + 4*floor((x-1)/36))
  Experiment = as.data.frame(matrix(ncol = 7 , nrow = nrow(permu)))
  
  for(i in 1:nrow(Experiment)){
    Experiment[i,1] = i
    for(j in 2:7){
      if(j == 3)
        Experiment[i,j] = permu[i,j-1]
      else
        Experiment[i,j] = Exp_Params[[j-1]][permu[i,j-1]]
    }
  }
  colnames(Experiment) = c("id" , paste("P", c(1:6) , sep = "") )
  
  
  Exp_cols = c("P1.activation" , 
               "P2.hidden"     , 
               "P3.epochs"     ,
               "P4.rate"       , 
               "P5.L2"         ,
               "P6.loss"       )
  
  return(list(Exp = Experiment , names = Exp_cols , params = Exp_Params))
  
}
Config_svm   = function(){
  Exp_Params = list()
  Exp_Params[[1]] = c("radial basis") #kernel
  Exp_Params[[2]] = c(3:6)                                                  #degree
  Exp_Params[[3]] = c(0.1 , 0.2 , 0.5, 1)                                   #gamma
  Exp_Params[[4]] = c(0 ,0.1,0.2,0.5)                                       #coef
  Exp_Params[[5]] = c(1,2,5)                                                #cost
  Exp_Params[[6]] = c(0.95,0.05)                                            #classweight
  Exp_Params[[7]] = c(0.1, 0.01, 0.2,0.5)                                   #epsilon
  
  permu = expand.grid(Exp_Params)
  # permu = gtools :: permutations(n=5 ,r=7  , set = F ,repeats.allowed=T)
  # 
  # keep  = which(permu[,1] <=4 & permu[,2] <=5 & permu[,3] <=4 & permu[,4] <=4 & permu[,5] <=3 & permu[,6] <=2 & permu[,7]<=4 )
  # permu = permu[keep,]
  # 
  # # removing degree>1 for non-degree models
  # non_degree = which(!permu[,1] == 2 & !permu[,1] ==4  & permu[,2]>1) 
  # permu = permu[-non_degree,]
  # 
  # #remiving gamma for linear model
  # non_linear = which(permu[,1] == 1 & permu[,3] >1)
  # permu = permu[-non_linear , ]
  # 
  # 
  # Experiment = as.data.frame(matrix(ncol = 8 , nrow = nrow(permu)))
  # 
  # for(i in 1:nrow(Experiment)){
  #   Experiment[i,1] = i
  #   for(j in 2:8){
  #     Experiment[i,j] = Exp_Params[[j-1]][permu[i,j-1]]
  #   }
  # }
  Experiment = data.frame(id = 1:nrow(permu), permu)
  colnames(Experiment) = c("id" , paste("P", c(1:7) , sep = "") )
  
  
  Exp_cols = c("P1.kernel"    , 
               "P2.degree"        , 
               "P3.gamma"      ,
               "P4.coef0"  , 
               "P5.cost",
               "P6.classweight"  ,
               "P7.epsilon" ,
               "P8.probability"
  )
  
  return(list(Exp = Experiment , names = Exp_cols , params = Exp_Params))
  
  
}

#------------------------------------------------------------------------------------
# generate list of parameters from a pool of experiments
#------------------------------------------------------------------------------------
Set_Nnet_Params = function(Exp = NULL , id = 1){
  
  p = list(P1   = Exp$P1 [id],
           P2   = Exp$P2 [id],
           P3   = Exp$P3 [id],
           P4   = Exp$P4 [id],
           P5   = Exp$P5 [id],
           P6   = Exp$P6 [id])
  return(params = p)
}
Set_svm_Params   = function(Exp = NULL , id = 1){
  
  p = list(kernel         = Exp$P1[id],
           degree         = Exp$P2[id],
           gamma          = Exp$P3[id],
           coef0          = Exp$P4[id],
           cost           = Exp$P5[id],
           class.weights  = Exp$P6[id],
           epsilon        = Exp$P7[id],
           probability    = T
  )
  return(params = p)
}
Set_params      = function (Model, e){
  if(Model == "Nnet"){return(Set_Nnet_Params(Nnet_Exp, e))}
  if(Model == "svm")  {return(Set_svm_Params  (svm_Exp, e))}
}

#------------------------------------------------------------------------------------
# Fit a model on data
#------------------------------------------------------------------------------------
Fit_Nnet  = function(df , df_folds , n_folds = 5,params = NULL){
  
  #neural network
  # set.seed(1234)
  
  x_index = which(!(colnames(df) == "fraud"))
  y_index = which( colnames(df) == "fraud")
  
  df$fraud = ifelse(df$fraud == 1,T,F)
  df= Train
  
  
  Nnet_models = lapply(c(1:n_folds),function(x){
    print(paste0("------ Train on Fold " , x))
    
    h2o.deeplearning(x                     = x_index , 
                     y                     = y_index , 
                     training_frame        = as.h2o(df[df_folds$train_index[[x]],]), 
                     validation_frame      = as.h2o(df[df_folds$test_index[[x]],]),
                     activation            = params[[1]],
                     hidden                = unlist(Exp_Params[[2]][params[[2]]]),
                     epochs                = params[[3]],
                     rate                  = params[[4]],
                     l2                    = params[[5]],
                     loss                  = params[[6]])
  })
  
  yhat_train = lapply(c(1:n_folds), function(x)  {
    print(paste0("-------- Predict on Train Fold " , x))
    as.matrix(h2o.predict(Nnet_models[[x]] , as.h2o(df[df_folds$train_index[[x]],]  ))[,3])
  })
  yhat_test  = lapply(c(1:n_folds), function(x)  {
    print(paste0("-------- Predict on Test Fold " , x))
    as.matrix(h2o.predict(Nnet_models[[x]] , as.h2o(df[df_folds$test_index[[x]],]  ))[,3])
  })
  
  return(list(model      = Nnet_models, 
              Params     = params,
              yhat_test  = yhat_test,
              yhat_train = yhat_train))  
}
Fit_svm   = function(df , df_folds = NULL , n_folds = 5, params = NULL){
  
  svm_models = lapply(c(1:n_folds) , function(x){
    if(Sampling == "down"){
      nm = names(df)
      nxm = nm[!(names(df) %in% c("fraud"))]
      df_train = downSample(x=df[df_folds$train_index[[x]],nxm] , 
                            y=as.factor(df[df_folds$train_index[[x]],]$fraud) , 
                            yname = "fraud")
    }
    if(Sampling == "up"){
      nm = names(df)
      nxm = nm[!(names(df) %in% c("fraud"))]
      df_train = upSample(x=df[df_folds$train_index[[x]],nxm] , 
                          y=as.factor(df[df_folds$train_index[[x]],]$fraud) , 
                          yname = "fraud")
    }
    if(Sampling == "smote"){
      df_train = SMOTE(fraud~.,data = df[df_folds$train_index[[x]],])
    }
    if(Sampling == "none"){
      df_train = df[df_folds$train_index[[x]],]
    }
    
    
    
    svm(fraud~.,  data = df_train,
        kernel = params[[1]],
        degree = params[[2]],
        gamma  = params[[3]],
        coef0  = params[[4]],
        cost   = params[[5]],
        epsilon = params[[7]],
        probability = T
    )
  })
  
  
  
  #df = as.matrix(df)
  #df = apply(df , c(1,2) , as.numeric)
  #y_index = which(colnames(df )=="fraud")
  
  
  yhat_train = lapply(c(1:n_folds), function(x) attr(predict(svm_models[[x]] , df[df_folds$train_index[[x]], ] , probability = T),"probabilities")[,2])
  yhat_test  = lapply(c(1:n_folds), function(x) attr(predict(svm_models[[x]] , df[df_folds$test_index[[x]], ]  , probability = T),"probabilities")[,2])
  
  
  
  return(list(model      = svm_models, 
              Params     = params,
              yhat_test  = yhat_test,
              yhat_train = yhat_train))  
}

Train_model = function(Model = NULL , df = NULL , df_folds = NULL , n_folds=5 , params=NULL){
  if(Model == "Nnet"){return(Fit_Nnet(df , df_folds ,n_folds ,params ))}
  if(Model == "svm")  {return(Fit_svm  (df , df_folds ,n_folds ,params ))}
}

#------------------------------------------------------------------------------------
#                 configure the training model tuning grid
#------------------------------------------------------------------------------------
if(Model == "Nnet"){
  Nnet_Config = Config_Nnet()
  Nnet_Exp    = Nnet_Config$Exp
  Exp_Params  = Nnet_Config$params
  tune_names  = Nnet_Config$names
}
if(Model == "svm")  {
  svm_Config   = Config_svm()
  svm_Exp      = svm_Config$Exp
  Exp_Params   = svm_Config$params
  tune_names   = svm_Config$names
}

#------------------------------------------------------------------------------------
# Tune Model
#------------------------------------------------------------------------------------

Tunning_out = NULL

for (e in start:end){
  
  print(paste0("--Experiment " , e))
  
  Exp_out = NULL
  
  for(i in 1:n_trial){
    
    print(paste0("----Trial " , i))
    df_folds = build_datasets(Train , n_folds)
    params = Set_params(Model , e)
    trial_model = Train_model(Model, Train , df_folds , n_folds , params)
    
    
    
    for(p in 1:length(cutoff)){
      Train_out = lapply(c(1:n_folds) , function(x)Measure_Performance_prob(pred= trial_model$yhat_train[[x]],  
                                                                            ref = Train$fraud[df_folds$train_index[[x]]], 
                                                                            prob_thr = cutoff[p]) )
      
      Test_out = lapply(c(1:n_folds) , function(x)Measure_Performance_prob(pred= trial_model$yhat_test[[x]],  
                                                                           ref = Train$fraud[df_folds$test_index[[x]]], 
                                                                           prob_thr = cutoff[p]) )
      
      folds_out = data.frame("Train_Accuracy"       = sapply(c(1:n_folds) , function(x) Train_out[[x]]$Accuracy),
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
      Exp_out = rbind(Exp_out , data.frame(
        "Exp_ID" = e,
        "Cutoff" = cutoff[p],
        "Trial"  = i,
        "Train_Accuracy"       = mean(folds_out$Train_Accuracy),
        "Train_Sensitivity"    = mean(folds_out$Train_Sensitivity),
        "Train_Specificity"    = mean(folds_out$Train_Specificity),
        "Train_PredictionCost" = mean(folds_out$Train_PredictionCost),
        "Train_MissingFraud"   = mean(folds_out$Train_MissingFraud),
        "Train_FalseAlarm"     = mean(folds_out$Train_FalseAlarm),
        
        "Test_Accuracy"        = mean(folds_out$Test_Accuracy),
        "Test_Sensitivity"     = mean(folds_out$Test_Sensitivity),
        "Test_Specificity"     = mean(folds_out$Test_Specificity),
        "Test_PredictionCost"  = mean(folds_out$Test_PredictionCost),
        "Test_MissingFraud"    = mean(folds_out$Test_MissingFraud),
        "Test_FalseAlarm"      = mean(folds_out$Test_FalseAlarm)
      ))
    }#end of cutoff loop
    
  }#end of trial loop
  
  param_out = matrix ( rep( unlist(params) , n_trial*length(cutoff)) , ncol = length(params) ,byrow = T)
  colnames(param_out) = tune_names
  Exp_out = cbind(Exp_out , param_out)
  Tunning_out = rbind(Tunning_out , Exp_out)
  write.csv(Tunning_out , paste0(path, Model , "_" , Sampling, "_" , "Tunning_out_no_new_dt_radial.csv") , row.names = F)
  
}#end of tuning loop





perf_in = read.csv(paste(path, "svm_up_Tunning_out_no_new_dt_radial.csv", sep = ""))

perf_summary = perf_in %>% group_by(Exp_ID , Cutoff)%>%summarize( Mean_Train_AC = mean(Train_Accuracy),
                                                                  Mean_Train_Sn = mean(Train_Sensitivity),
                                                                  Mean_Train_Sp = mean(Train_Specificity),
                                                                  Mean_Train_PC = mean(Train_PredictionCost),
                                                                  Mean_Train_MF = mean(Train_MissingFraud),
                                                                  Mean_Train_FA = mean(Train_FalseAlarm),
                                                                  
                                                                  Mean_Test_AC = mean(Test_Accuracy),
                                                                  Mean_Test_Sn = mean(Test_Sensitivity),
                                                                  Mean_Test_Sp = mean(Test_Specificity),
                                                                  Mean_Test_PC = mean(Test_PredictionCost),
                                                                  Mean_Test_MF = mean(Test_MissingFraud),
                                                                  Mean_Test_FA = mean(Test_FalseAlarm),
                                                                  Test_PC_var  = sd(Test_PredictionCost)
) 
write.csv(perf_summary , paste(path, "svm_up_summary_no_new_dt_radial.csv",sep = ""))

#df1 = data.frame(
#  "Test_PredictionCost_mean" = perf_summary$Mean_Test_PC,
#  "Test_PredictionCost_var"  = perf_summary$Test_PC_var^2)

#ggplot(df1, aes(x = Test_PredictionCost_mean, y = Test_PredictionCost_var))+geom_point()


#data_svm = read.csv("file:///C:/Users/Reyhaneh/Box Sync/STAT 602/Project/DMC_2019_task/DMC_2019_task/Data/svm_up_summary_latex.csv")
#xtable(data_svm)
