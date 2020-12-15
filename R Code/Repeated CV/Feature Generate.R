feature.generate <- function(Train, Test, path, standardize = F){
  std.data <- function(data.train, data.test, var.to.std){
    
    ## First we store fraud
    fraud <- data.train$fraud
    
    ## First we remove fraud from training data
    data.train <- data.train[, !(names(data.train) %in% c('fraud'))]
    
    ## Here we combine the two data sets
    data <- rbind(data.train, data.test)
    
    ## Here we standardize only numerical variables (given their names as argument)
    data.trans <- data.frame(apply(data[, names(data) %in% var.to.std], 2, function(x) (x - mean(x)) / sd(x)))
    
    ## Here we put everything together
    data.tot <- data.frame(data[, !(names(data) %in% var.to.std)], data.trans)
    
    ## Here we split the data into training and testing 
    data.train <- data.frame(data.tot[1:nrow(data.train),], fraud = fraud)
    data.test <- data.tot[-c(1:nrow(data.train)),]
    
    ## Here we create the list to be output
    data.out <- list(data.train, data.test)
    
    return(data.out)
  }
  
  continuous_to_discreat <- function(dt, test_dt, varlist, ngroup, path) 
  {
    test_dt$fraud = NA 
    aa <- function(dt, var1, ngroup, path)
    {
      bb = rbind(dt, test_dt)
      bin = quantile(x = dt[[var1]], probs = seq(0,1,1/ngroup))
      mintr = min(dt[[var1]])
      mints = min(test_dt[[var1]])
      maxtr = max(dt[[var1]])
      maxts = max(test_dt[[var1]])
      bin[1] = min(mintr, mints)-1
      bin[length(bin)] = max(maxtr, maxts)+1
      b = rowSums(sapply(1:(length(bin)-1), FUN = function (a) ifelse(dt[[var1]]>=bin[a] & dt[[var1]] < bin[a+1], a, 0)))
      write.table(bin, file = paste(path, var1, "_bin.txt",sep = ""), append = F, row.names = F, col.names = F)    
      return(b)
    }
    
    bb = data.frame(sapply(varlist, FUN = function(x) aa(dt, x, ngroup, path)))
    names(bb) = paste(varlist, "_2_discrete", sep = "")
    write.table(x = names(bb), file = paste(path,"varlist_cont_2_disc.txt", sep = ""),
                row.names = F, col.names = F,sep = ",", quote = F)
    return(bb)
  }
  
  
  continuous_to_discreat_test <- function(dt, varlist, path) 
  {
    aa <- function(dt, var1, path)
    {
      bin = unlist(read.table(file = paste(path, var1, "_bin.txt",sep = "")))
      b = rowSums(sapply(1:(length(bin)-1), FUN = function (a) ifelse(dt[[var1]]>=bin[a] & dt[[var1]] < bin[a+1], a, 0)))
      return(b)
    }
    
    bb = data.frame(lapply(varlist, FUN = function(x) aa(dt, x, path)))
    names(bb) = paste(varlist, "_2_discrete", sep = "")
    return(bb)
  }
  
  likelihood_ratio <- function(train, varlist_disc, path)
  {
    library(dplyr)
    train$fraud = as.numeric(as.character(train$fraud))
    train$ID = c(1:nrow(train))
    nm = names(train)
    
    abc <- function(train, var1, path)
    {
      sum_group = as.data.frame(train %>% group_by_at(var1) %>% 
                                  summarise_at("fraud", funs(sum(.)/(n()-sum(.)))))
      
      names(sum_group)[-1] = paste("likelihood_", var1, sep = "")
      write.table(sum_group, file = paste(path, "likelihood_", var1, ".txt", sep = ""), quote = F, row.names = F, sep = ",")
      return(sum_group)
    }
    
    gg = lapply(varlist_disc, FUN = function(x) abc(train, x, path))
    
    for (i in c(1:length(varlist_disc))){
      train = merge(train, gg[[i]], by = varlist_disc[i], all = T, sort = F)
      
    }
    
    nm1 = names(train)[!(names(train) %in% nm)]
    write.table(nm1, file = paste(path, "varlist_likelihood_feature.txt", sep = ""), quote = F, row.names = F, sep = ",")
    
    train = data.frame(train[order(train$ID),])
    train = train[,as.character(unlist(nm1))]
    return(train)
  }
  
  
  
  likelihood_ratio_test <- function(test, varlist_disc, path)
  {
    test$ID = c(1:nrow(test)) 
    for (i in c(1:length(varlist_disc))){
      # names(sum_group)[-1] = paste("likelihood_", varlist_disc[i], sep = "")
      gg = read.table(file = paste(path, "likelihood_", varlist_disc[i], ".txt", sep = ""), sep = ",", header = T)
      test = merge(test, gg, by = varlist_disc[i], all = T, sort = F)
    }
    test = data.frame(test[order(test$ID),])
    gg = unlist(read.table(file = paste(path, "varlist_likelihood_feature.txt", sep = ""), sep = ",", header = T))
    test = test[,as.character(gg)]
    return(test)
  }
  
  
  
   # path = "D:/ISU/Data Mining Cup DMC/DMC 2019/R code/Repeated CV/"
  
  con_2_dis_var = c("totalScanTimeInSeconds", 
                    "scannedLineItemsPerSecond", 
                    "valuePerSecond")
  
  con_2_dis_variable = continuous_to_discreat(dt = Train, 
                                              test_dt = Test, 
                                              varlist = con_2_dis_var, 
                                              ngroup = 20, 
                                              path = path)
  con_2_dis_variable_test = continuous_to_discreat_test(dt = Test, 
                                                        varlist = con_2_dis_var, 
                                                        path = path)
  
  
  likelihood_var = c("trustLevel", 
                     "lineItemVoids", 
                     "scansWithoutRegistration", 
                     "totalScanTimeInSeconds_2_discrete",
                     "scannedLineItemsPerSecond_2_discrete",
                     "valuePerSecond_2_discrete")
  
  likelihood_variable = likelihood_ratio(train = cbind(Train, con_2_dis_variable), 
                                         varlist_disc = likelihood_var, 
                                         path = path)
  
  likelihood_variable_test = likelihood_ratio_test(test = cbind(Test, con_2_dis_variable_test), 
                                         varlist_disc = likelihood_var, 
                                         path = path)
  
  
  Train = data.frame(cbind(Train, con_2_dis_variable, likelihood_variable))
  Test = data.frame(cbind(Test, con_2_dis_variable_test, likelihood_variable_test))
  
  rm(con_2_dis_variable, con_2_dis_variable_test, likelihood_variable, likelihood_variable_test)
  
  mean_TSTIS = mean(Train$totalScanTimeInSeconds)
  mean_Nitem = mean(Train$X27_Nitems)
  
  Train$Cross_SLIPS_2_Dis_TSTIS = Train$scannedLineItemsPerSecond_2_discrete*(Train$totalScanTimeInSeconds-mean_TSTIS)
  Train$Nitem_SWR = (Train$X27_Nitems-mean_Nitem)*(Train$scansWithoutRegistration)
  Train$Nitem_LVPS_2_dis = Train$X27_Nitems*Train$likelihood_valuePerSecond_2_discrete
  Train$GT_by_TSTIN_LIV = (Train$grandTotal/Train$totalScanTimeInSeconds)*(Train$lineItemVoids)
  Train$GT_by_TSTIN_LIV = log(1+Train$GT_by_TSTIN_LIV)
  Train$X27_Nitems_pow3 = (Train$X27_Nitems-mean_Nitem)^3
  
  
  # Train$trustLevel1 = ifelse(Train$trustLevel==1, 1,0)
  # Train$trustLevel2 = ifelse(Train$trustLevel==2, 1,0)
  # Train = Train[,!(names(Train) %in% c("trustLevel"))]
  
  # Train$Nitem_LIV = Train$X27_Nitems*Train$lineItemVoids
  # Train$Nitem_LTL = Train$X27_Nitems*Train$likelihood_trustLevel
  # Train$Nitem_TL = Train$X27_Nitems*Train$trustLevel
  # Train$SLIPS_2_dis_TSTIS_2_dis_LTL = Train$scannedLineItemsPerSecond_2_discrete*Train$totalScanTimeInSeconds_2_discrete*Train$likelihood_trustLevel
  # Train$SLIPS_2_dis_TSTIS_LTL = Train$scannedLineItemsPerSecond_2_discrete*Train$totalScanTimeInSeconds*Train$likelihood_trustLevel
  # Train$SLIPS_TSTIS_2_dis_LTL = Train$scannedLineItemsPerSecond*Train$totalScanTimeInSeconds_2_discrete*Train$likelihood_trustLevel
  # Train$Nitem_VPS_VOID = (Train$X27_Nitems/(1+Train$valuePerSecond))*Train$trustLevel*(1+Train$scansWithoutRegistration)*(1+Train$lineItemVoids)*(1+Train$quantityModifications)
  # Train$Nitem_LLIV = Train$X27_Nitems*Train$likelihood_lineItemVoids
  # Train$TSTIS_2_dis_SLIPS_LSWR = Train$totalScanTimeInSeconds_2_discrete*Train$scannedLineItemsPerSecond*Train$likelihood_scansWithoutRegistration
  # Train$Nitem_LSWR = Train$X27_Nitems*Train$likelihood_scansWithoutRegistration
  # Train$SLIPS_2_dis_SWR_LTSTIS_2_dis = Train$scannedLineItemsPerSecond_2_discrete*Train$scansWithoutRegistration*Train$likelihood_totalScanTimeInSeconds_2_discrete
  # Train$SLIPS_2_dis_LTL_LTSTIS_2_dis = Train$scannedLineItemsPerSecond_2_discrete*Train$likelihood_trustLevel*Train$likelihood_totalScanTimeInSeconds_2_discrete
  # Train$SLIPS_LTL_LTSTIS_2_dis = Train$scannedLineItemsPerSecond*Train$likelihood_trustLevel*Train$likelihood_totalScanTimeInSeconds_2_discrete
  # Train$TSTIS_2_dis_LTL_LSLIPS_2_dis = Train$totalScanTimeInSeconds_2_discrete*Train$likelihood_trustLevel*Train$likelihood_scannedLineItemsPerSecond_2_discrete
  # Train$TSTIS_LTL_LSLIPS_2_dis = Train$totalScanTimeInSeconds*Train$likelihood_trustLevel*Train$likelihood_scannedLineItemsPerSecond_2_discrete
  # Train$LTL_LSWR_LLSLIPS_2_dis = Train$likelihood_trustLevel*Train$likelihood_scansWithoutRegistration*Train$likelihood_scannedLineItemsPerSecond_2_discrete
  # Train$GT_by_TSTIN_LIVPP = (Train$grandTotal/Train$totalScanTimeInSeconds)*(Train$lineItemVoidsPerPosition)
  # Train$LTL_LSLIPS_2_dis_LVPS_2_dis = Train$likelihood_trustLevel*Train$likelihood_scannedLineItemsPerSecond_2_discrete*Train$likelihood_valuePerSecond_2_discrete
  # Train$GT_by_Nitem = Train$grandTotal/Train$X27_Nitems
  # Train$GT_by_TSTIN = Train$grandTotal/Train$totalScanTimeInSeconds
  # Train$GT_by_TSTIN_QM = (Train$grandTotal/Train$totalScanTimeInSeconds)*Train$quantityModifications
  # 
  # Train$GT_by_TSTIN_SWR = (Train$grandTotal/Train$totalScanTimeInSeconds)*(1+Train$scansWithoutRegistration)
  # 
  # Train$GT_by_LIV_SWR_QM = Train$grandTotal/((1+Train$lineItemVoids)*(1+Train$scansWithoutRegistration)*(1+Train$quantityModifications))
  # 
  # Train$Nitem_VPS_TL = (Train$X27_Nitems/(1+Train$valuePerSecond))*Train$trustLevel
  # Train$Nitem_LIVPP_VOID = (Train$X27_Nitems/(1+Train$lineItemVoidsPerPosition))*Train$trustLevel*(1+Train$scansWithoutRegistration)*(1+Train$lineItemVoids)*(1+Train$quantityModifications)
  # Train$Nitem_LIVPP_TL = (Train$X27_Nitems/(1+Train$lineItemVoidsPerPosition))*Train$trustLevel
  # Train$CPS_LSWR = Train$valuePerSecond/(1+Train$scansWithoutRegistration)
  # # Train$X27_Nitems_pow2 = Train$X27_Nitems^2
  # 
  # # Train$X27_Nitems_TL_pow2 = (Train$trustLevel*Train$X27_Nitems)^2
  # # Train$X27_Nitems_TL_pow3 = (Train$trustLevel*Train$X27_Nitems)^3
  # # Train$X27_Nitems_SWR_pow2 = (Train$scansWithoutRegistration*Train$X27_Nitems)^2
  # Train$X27_Nitems_SWR_pow3 = (Train$scansWithoutRegistration*Train$X27_Nitems)^3
  # # Train$X27_Nitems_LIV_pow2 = (Train$lineItemVoids*Train$X27_Nitems)^2
  # # Train$X27_Nitems_LIV_pow3 = (Train$lineItemVoids*Train$X27_Nitems)^3

  
  # 
  # "totalScanTimeInSeconds_2_discrete"              
  # "scannedLineItemsPerSecond_2_discrete" 
  # "valuePerSecond_2_discrete"                      
  # "likelihood_trustLevel"                          
  # "likelihood_scannedLineItemsPerSecond_2_discrete"
  # "likelihood_valuePerSecond_2_discrete"           
  # "Cross_SLIPS_2_Dis_TSTIS"                        
  # "Nitem_SWR"                                      
  # "Nitem_LTL"                                      
  # "Nitem_LVPS_2_dis"                               
  # "GT_by_TSTIN_LIV"                                
  # "Nitem_VPS_VOID"                                 
  # "X27_Nitems_pow3"
  
  final_var_list = c(  "trustLevel"                                     ,
                       "totalScanTimeInSeconds"                         ,
                       "grandTotal"                                     ,
                       "lineItemVoids"                                  ,
                       "scansWithoutRegistration"                       ,
                       "quantityModifications"                          ,
                       "scannedLineItemsPerSecond"                      ,
                       "valuePerSecond"                                 ,
                       "lineItemVoidsPerPosition"                       ,
                       "X27_Nitems"                                     ,
                       "X78_valueItem"                                  ,
                       "totalScanTimeInSeconds_2_discrete"              ,
                       "scannedLineItemsPerSecond_2_discrete"           ,
                       "valuePerSecond_2_discrete"                      ,
                       "likelihood_trustLevel"                          ,
                       "likelihood_scannedLineItemsPerSecond_2_discrete",
                       "likelihood_valuePerSecond_2_discrete"           ,
                       "Cross_SLIPS_2_Dis_TSTIS"                        ,
                       "Nitem_SWR"                                      ,
                       "Nitem_LVPS_2_dis"                               ,
                       "GT_by_TSTIN_LIV"                                ,
                       "X27_Nitems_pow3"                                ,
                       "fraud")
  
  
  # final_var_list = c(  "totalScanTimeInSeconds"                         ,
  #                      "grandTotal"                                     ,
  #                      "lineItemVoids"                                  ,
  #                      "quantityModifications"                          ,
  #                      "scannedLineItemsPerSecond"                      ,
  #                      "scansWithoutRegistration"                       ,
  #                      "valuePerSecond"                                 ,
  #                      "lineItemVoidsPerPosition"                       ,
  #                      "X27_Nitems"                                     ,
  #                      "X78_valueItem"                                  ,
  #                      "totalScanTimeInSeconds_2_discrete"              ,
  #                      "scannedLineItemsPerSecond_2_discrete"           ,
  #                      "valuePerSecond_2_discrete"                      ,
  #                      "likelihood_trustLevel"                          ,
  #                      "likelihood_lineItemVoids"                       ,
  #                      "likelihood_scansWithoutRegistration"            ,
  #                      "likelihood_totalScanTimeInSeconds_2_discrete"   ,
  #                      "likelihood_scannedLineItemsPerSecond_2_discrete",
  #                      "likelihood_valuePerSecond_2_discrete"           ,
  #                      "Cross_SLIPS_2_Dis_TSTIS"                        ,
  #                      "Nitem_SWR"                                      ,
  #                      "Nitem_LIV"                                      ,
  #                      "Nitem_TL"                                       ,
  #                      "SLIPS_2_dis_TSTIS_2_dis_LTL"                    ,
  #                      "SLIPS_TSTIS_2_dis_LTL"                          ,
  #                      "Nitem_LTL"                                      ,
  #                      "Nitem_LLIV"                                     ,
  #                      "SLIPS_2_dis_SWR_LTSTIS_2_dis"                   ,
  #                      "SLIPS_2_dis_LTL_LTSTIS_2_dis"                   ,
  #                      "LTL_LSWR_LLSLIPS_2_dis"                         ,
  #                      "Nitem_LVPS_2_dis"                               ,
  #                      "LTL_LSLIPS_2_dis_LVPS_2_dis"                    ,
  #                      "GT_by_TSTIN"                                    ,
  #                      "GT_by_TSTIN_QM"                                 ,
  #                      "GT_by_TSTIN_LIV"                                ,
  #                      "GT_by_TSTIN_SWR"                                ,
  #                      "Nitem_VPS_VOID"                                 ,
  #                      "Nitem_LIVPP_VOID"                               ,
  #                      "Nitem_LIVPP_TL"                                 ,
  #                      "CPS_LSWR"                                       ,
  #                      "X27_Nitems_pow3"                                ,
  #                      "X27_Nitems_SWR_pow3"                            ,
  #                      "trustLevel1"                                    ,
  #                      "trustLevel2"                                    ,
  #                      "fraud")
  
  
  # final_var_list_with_pnlty_1 = c("grandTotal"                          ,
  #                      "lineItemVoids"                                  ,
  #                      "quantityModifications"                          ,
  #                      "totalScanTimeInSeconds_2_discrete"              ,
  #                      "scannedLineItemsPerSecond_2_discrete"           ,
  #                      "valuePerSecond_2_discrete"                      ,
  #                      "likelihood_trustLevel"                          ,
  #                      "likelihood_scannedLineItemsPerSecond_2_discrete",
  #                      "likelihood_valuePerSecond_2_discrete"           ,
  #                      "Cross_SLIPS_2_Dis_TSTIS"                        ,
  #                      "Nitem_SWR"                                      ,
  #                      "SLIPS_2_dis_TSTIS_2_dis_LTL"                    ,
  #                      "SLIPS_TSTIS_2_dis_LTL"                          ,
  #                      "Nitem_LTL"                                      ,
  #                      "Nitem_LLIV"                                     ,
  #                      "SLIPS_2_dis_LTL_LTSTIS_2_dis"                   ,
  #                      "SLIPS_LTL_LTSTIS_2_dis"                         ,
  #                      "LTL_LSWR_LLSLIPS_2_dis"                         ,
  #                      "Nitem_LVPS_2_dis"                               ,
  #                      "LTL_LSLIPS_2_dis_LVPS_2_dis"                    ,
  #                      "Nitem_VPS_VOID"                                 ,
  #                      "X27_Nitems_pow3"                                ,
  #                      "X27_Nitems_SWR_pow3"                            ,
  #                      "trustLevel1"                                    ,
  #                      "trustLevel2"                                    ,
  #                      "fraud")
  
  
  
  # 
  # final_var_list_with_pnlty_92 = c("grandTotal"                                     ,
  #                                  "lineItemVoids"                                  ,   
  #                                  "quantityModifications"                          ,  
  #                                  "scannedLineItemsPerSecond"                      , 
  #                                  "X27_Nitems"                                     ,
  #                                  "totalScanTimeInSeconds_2_discrete"              ,
  #                                  "scannedLineItemsPerSecond_2_discrete"           ,
  #                                  "valuePerSecond_2_discrete"                      ,
  #                                  "likelihood_trustLevel"                          ,
  #                                  "likelihood_scansWithoutRegistration"            ,
  #                                  "likelihood_totalScanTimeInSeconds_2_discrete"   ,
  #                                  "likelihood_scannedLineItemsPerSecond_2_discrete",
  #                                  "likelihood_valuePerSecond_2_discrete"           ,
  #                                  "Cross_SLIPS_2_Dis_TSTIS"                        ,
  #                                  "Nitem_SWR"                                      ,
  #                                  "Nitem_TL"                                       ,
  #                                  "SLIPS_2_dis_TSTIS_2_dis_LTL"                    ,
  #                                  "SLIPS_TSTIS_2_dis_LTL"                          ,
  #                                  "Nitem_LTL"                                      ,
  #                                  "Nitem_LLIV"                                     ,
  #                                  "SLIPS_2_dis_LTL_LTSTIS_2_dis"                   ,
  #                                  "SLIPS_LTL_LTSTIS_2_dis"                         ,
  #                                  "LTL_LSWR_LLSLIPS_2_dis"                         ,
  #                                  "Nitem_LVPS_2_dis"                               ,
  #                                  "LTL_LSLIPS_2_dis_LVPS_2_dis"                    ,
  #                                  "GT_by_TSTIN_QM"                                 ,
  #                                  "GT_by_TSTIN_LIV"                                ,
  #                                  "GT_by_TSTIN_SWR"                                ,
  #                                  "Nitem_VPS_VOID"                                 ,
  #                                  "CPS_LSWR"                                       ,
  #                                  "X27_Nitems_pow3"                                ,
  #                                  "X27_Nitems_SWR_pow3"                            ,
  #                                  "trustLevel1"                                    ,
  #                                  "trustLevel2"                                    ,
  #                                  "fraud")
  
  # final_var_list_using_redge = c("grandTotal"                                      ,
  #                                "lineItemVoids"                                   ,
  #                                "quantityModifications"                           ,
  #                                "X27_Nitems"                                      ,
  #                                "totalScanTimeInSeconds_2_discrete"               ,
  #                                "scannedLineItemsPerSecond_2_discrete"            ,
  #                                "valuePerSecond_2_discrete"                       ,
  #                                "likelihood_trustLevel"                           ,
  #                                "likelihood_lineItemVoids"                        ,
  #                                "likelihood_totalScanTimeInSeconds_2_discrete"   ,
  #                                "likelihood_scannedLineItemsPerSecond_2_discrete",
  #                                "likelihood_valuePerSecond_2_discrete"           ,
  #                                "Cross_SLIPS_2_Dis_TSTIS"                        ,
  #                                "Nitem_SWR"                                      ,
  #                                "Nitem_LIV"                                      ,
  #                                "SLIPS_2_dis_TSTIS_2_dis_LTL"                    ,
  #                                "SLIPS_TSTIS_2_dis_LTL"                          ,
  #                                "Nitem_LTL"                                      ,
  #                                "SLIPS_2_dis_SWR_LTSTIS_2_dis"                   ,
  #                                "SLIPS_2_dis_LTL_LTSTIS_2_dis"                   ,
  #                                "TSTIS_LTL_LSLIPS_2_dis"                         ,
  #                                "LTL_LSWR_LLSLIPS_2_dis"                         ,
  #                                "Nitem_LVPS_2_dis"                               ,
  #                                "GT_by_TSTIN_LIV"                                ,
  #                                "Nitem_VPS_VOID"                                 ,
  #                                "trustLevel1"                                    ,
  #                                "trustLevel2"                                    ,
  #                                "fraud")
  
  
  
  Train = Train[,final_var_list]
  
  
  Test$Cross_SLIPS_2_Dis_TSTIS = Test$scannedLineItemsPerSecond_2_discrete*(Test$totalScanTimeInSeconds-mean_TSTIS)
  Test$Nitem_SWR = (Test$X27_Nitems-mean_Nitem)*(Test$scansWithoutRegistration)
  Test$Nitem_LVPS_2_dis = Test$X27_Nitems*Test$likelihood_valuePerSecond_2_discrete
  Test$GT_by_TSTIN_LIV = (Test$grandTotal/Test$totalScanTimeInSeconds)*(Test$lineItemVoids)
  Test$GT_by_TSTIN_LIV = log(1+Test$GT_by_TSTIN_LIV)
  Test$X27_Nitems_pow3 = (Test$X27_Nitems-mean_Nitem)^3
  
  
  # Test$trustLevel1 = ifelse(Test$trustLevel==1, 1,0)
  # Test$trustLevel2 = ifelse(Test$trustLevel==2, 1,0)
  # Test = Test[,!(names(Test) %in% c("trustLevel"))]
  
  # Test$Nitem_VPS_VOID = (Test$X27_Nitems/(1+Test$valuePerSecond))*Test$trustLevel*(1+Test$scansWithoutRegistration)*(1+Test$lineItemVoids)*(1+Test$quantityModifications)
  # Test$Cross_SLIPS_2_Dis_TSTIS = Test$scannedLineItemsPerSecond_2_discrete*Test$totalScanTimeInSeconds
  # Test$Nitem_SWR = Test$X27_Nitems*Test$scansWithoutRegistration
  # Test$Nitem_LIV = Test$X27_Nitems*Test$lineItemVoids
  # Test$Nitem_LTL = Test$X27_Nitems*Test$likelihood_trustLevel
  # Test$Nitem_TL = Test$X27_Nitems*Test$trustLevel
  # Test$SLIPS_2_dis_TSTIS_2_dis_LTL = Test$scannedLineItemsPerSecond_2_discrete*Test$totalScanTimeInSeconds_2_discrete*Test$likelihood_trustLevel
  # Test$SLIPS_2_dis_TSTIS_LTL = Test$scannedLineItemsPerSecond_2_discrete*Test$totalScanTimeInSeconds*Test$likelihood_trustLevel
  # Test$SLIPS_TSTIS_2_dis_LTL = Test$scannedLineItemsPerSecond*Test$totalScanTimeInSeconds_2_discrete*Test$likelihood_trustLevel
  # Test$Nitem_LLIV = Test$X27_Nitems*Test$likelihood_lineItemVoids
  # Test$TSTIS_2_dis_SLIPS_LSWR = Test$totalScanTimeInSeconds_2_discrete*Test$scannedLineItemsPerSecond*Test$likelihood_scansWithoutRegistration
  # Test$Nitem_LSWR = Test$X27_Nitems*Test$likelihood_scansWithoutRegistration
  # Test$SLIPS_2_dis_SWR_LTSTIS_2_dis = Test$scannedLineItemsPerSecond_2_discrete*Test$scansWithoutRegistration*Test$likelihood_totalScanTimeInSeconds_2_discrete
  # Test$SLIPS_2_dis_LTL_LTSTIS_2_dis = Test$scannedLineItemsPerSecond_2_discrete*Test$likelihood_trustLevel*Test$likelihood_totalScanTimeInSeconds_2_discrete
  # Test$SLIPS_LTL_LTSTIS_2_dis = Test$scannedLineItemsPerSecond*Test$likelihood_trustLevel*Test$likelihood_totalScanTimeInSeconds_2_discrete
  # Test$TSTIS_2_dis_LTL_LSLIPS_2_dis = Test$totalScanTimeInSeconds_2_discrete*Test$likelihood_trustLevel*Test$likelihood_scannedLineItemsPerSecond_2_discrete
  # Test$TSTIS_LTL_LSLIPS_2_dis = Test$totalScanTimeInSeconds*Test$likelihood_trustLevel*Test$likelihood_scannedLineItemsPerSecond_2_discrete
  # Test$LTL_LSWR_LLSLIPS_2_dis = Test$likelihood_trustLevel*Test$likelihood_scansWithoutRegistration*Test$likelihood_scannedLineItemsPerSecond_2_discrete
  # Test$Nitem_LVPS_2_dis = Test$X27_Nitems*Test$likelihood_valuePerSecond_2_discrete
  # Test$LTL_LSLIPS_2_dis_LVPS_2_dis = Test$likelihood_trustLevel*Test$likelihood_scannedLineItemsPerSecond_2_discrete*Test$likelihood_valuePerSecond_2_discrete
  # Test$GT_by_Nitem = Test$grandTotal/Test$X27_Nitems
  # Test$GT_by_TSTIN = Test$grandTotal/Test$totalScanTimeInSeconds
  # Test$GT_by_TSTIN_QM = (Test$grandTotal/Test$totalScanTimeInSeconds)*Test$quantityModifications
  # Test$GT_by_TSTIN_LIV = (Test$grandTotal/Test$totalScanTimeInSeconds)*(1+Test$lineItemVoids)
  # Test$GT_by_TSTIN_SWR = (Test$grandTotal/Test$totalScanTimeInSeconds)*(1+Test$scansWithoutRegistration)
  # Test$GT_by_TSTIN_LIVPP = (Test$grandTotal/Test$totalScanTimeInSeconds)*(Test$lineItemVoidsPerPosition)
  # Test$GT_by_LIV_SWR_QM = Test$grandTotal/((1+Test$lineItemVoids)*(1+Test$scansWithoutRegistration)*(1+Test$quantityModifications))
  # Test$Nitem_VPS_VOID = (Test$X27_Nitems/(1+Test$valuePerSecond))*Test$trustLevel*(1+Test$scansWithoutRegistration)*(1+Test$lineItemVoids)*(1+Test$quantityModifications)
  # Test$Nitem_VPS_TL = (Test$X27_Nitems/(1+Test$valuePerSecond))*Test$trustLevel
  # Test$Nitem_LIVPP_VOID = (Test$X27_Nitems/(1+Test$lineItemVoidsPerPosition))*Test$trustLevel*(1+Test$scansWithoutRegistration)*(1+Test$lineItemVoids)*(1+Test$quantityModifications)
  # Test$Nitem_LIVPP_TL = (Test$X27_Nitems/(1+Test$lineItemVoidsPerPosition))*Test$trustLevel
  # Test$CPS_LSWR = Test$valuePerSecond/(1+Test$scansWithoutRegistration)
  # # Test$X27_Nitems_pow2 = Test$X27_Nitems^2
  # Test$X27_Nitems_pow3 = Test$X27_Nitems^3
  # # Test$X27_Nitems_TL_pow2 = (Test$trustLevel*Test$X27_Nitems)^2
  # # Test$X27_Nitems_TL_pow3 = (Test$trustLevel*Test$X27_Nitems)^3
  # # Test$X27_Nitems_SWR_pow2 = (Test$scansWithoutRegistration*Test$X27_Nitems)^2
  # Test$X27_Nitems_SWR_pow3 = (Test$scansWithoutRegistration*Test$X27_Nitems)^3
  # # Test$X27_Nitems_LIV_pow2 = (Test$lineItemVoids*Test$X27_Nitems)^2
  # # Test$X27_Nitems_LIV_pow3 = (Test$lineItemVoids*Test$X27_Nitems)^3

  
  final_var_list_test = c("trustLevel"                                     ,
                          "totalScanTimeInSeconds"                         ,
                          "grandTotal"                                     ,
                          "lineItemVoids"                                  ,
                          "scansWithoutRegistration"                       ,
                          "quantityModifications"                          ,
                          "scannedLineItemsPerSecond"                      ,
                          "valuePerSecond"                                 ,
                          "lineItemVoidsPerPosition"                       ,
                          "X27_Nitems"                                     ,
                          "X78_valueItem"                                  ,
                          "totalScanTimeInSeconds_2_discrete"              ,
                          "scannedLineItemsPerSecond_2_discrete"           ,
                          "valuePerSecond_2_discrete"                      ,
                          "likelihood_trustLevel"                          ,
                          "likelihood_scannedLineItemsPerSecond_2_discrete",
                          "likelihood_valuePerSecond_2_discrete"           ,
                          "Cross_SLIPS_2_Dis_TSTIS"                        ,
                          "Nitem_SWR"                                      ,
                          "Nitem_LVPS_2_dis"                               ,
                          "GT_by_TSTIN_LIV"                                ,
                          "X27_Nitems_pow3"                                )
  
  
  # final_var_list_test = c("totalScanTimeInSeconds"                      ,
  #                      "grandTotal"                                     ,
  #                      "lineItemVoids"                                  ,
  #                      "quantityModifications"                          ,
  #                      "scannedLineItemsPerSecond"                      ,
  #                      "scansWithoutRegistration"                       ,
  #                      "valuePerSecond"                                 ,
  #                      "lineItemVoidsPerPosition"                       ,
  #                      "X27_Nitems"                                     ,
  #                      "X78_valueItem"                                  ,
  #                      "totalScanTimeInSeconds_2_discrete"              ,
  #                      "scannedLineItemsPerSecond_2_discrete"           ,
  #                      "valuePerSecond_2_discrete"                      ,
  #                      "likelihood_trustLevel"                          ,
  #                      "likelihood_lineItemVoids"                       ,
  #                      "likelihood_scansWithoutRegistration"            ,
  #                      "likelihood_totalScanTimeInSeconds_2_discrete"   ,
  #                      "likelihood_scannedLineItemsPerSecond_2_discrete",
  #                      "likelihood_valuePerSecond_2_discrete"           ,
  #                      "Cross_SLIPS_2_Dis_TSTIS"                        ,
  #                      "Nitem_SWR"                                      ,
  #                      "Nitem_LIV"                                      ,
  #                      "Nitem_TL"                                       ,
  #                      "SLIPS_2_dis_TSTIS_2_dis_LTL"                    ,
  #                      "SLIPS_TSTIS_2_dis_LTL"                          ,
  #                      "Nitem_LTL"                                      ,
  #                      "Nitem_LLIV"                                     ,
  #                      "SLIPS_2_dis_SWR_LTSTIS_2_dis"                   ,
  #                      "SLIPS_2_dis_LTL_LTSTIS_2_dis"                   ,
  #                      "LTL_LSWR_LLSLIPS_2_dis"                         ,
  #                      "Nitem_LVPS_2_dis"                               ,
  #                      "LTL_LSLIPS_2_dis_LVPS_2_dis"                    ,
  #                      "GT_by_TSTIN"                                    ,
  #                      "GT_by_TSTIN_QM"                                 ,
  #                      "GT_by_TSTIN_LIV"                                ,
  #                      "GT_by_TSTIN_SWR"                                ,
  #                      "Nitem_VPS_VOID"                                 ,
  #                      "Nitem_LIVPP_VOID"                               ,
  #                      "Nitem_LIVPP_TL"                                 ,
  #                      "CPS_LSWR"                                       ,
  #                      "X27_Nitems_pow3"                                ,
  #                      "X27_Nitems_SWR_pow3"                            ,
  #                      "trustLevel1"                                    ,
  #                      "trustLevel2"                                    )
  
  Test = Test[,final_var_list_test]
  
  
  # Train$scannedLineItemsPerSecond = sapply(1:nrow(Train), FUN = function(i) {
  #   ifelse(Train$scannedLineItemsPerSecond[i] == 0, log(1e-10), log(Train$scannedLineItemsPerSecond[i]))})
  # 
  # Train$valuePerSecond = sapply(1:nrow(Train), FUN = function(i) {
  #   ifelse(Train$valuePerSecond[i] == 0, log(1e-10), log(Train$valuePerSecond[i]))})
  # 
  # Train$GT_by_TSTIN = sapply(1:nrow(Train), FUN = function(i) {
  #   ifelse(Train$GT_by_TSTIN[i] == 0, log(1e-10), log(Train$GT_by_TSTIN[i]))})
  # 
  # Train$GT_by_TSTIN_QM = sapply(1:nrow(Train), FUN = function(i) {
  #   ifelse(Train$GT_by_TSTIN_QM[i] == 0, log(1e-10), log(Train$GT_by_TSTIN_QM[i]))})
  # 
  # Train$GT_by_TSTIN_LIV = sapply(1:nrow(Train), FUN = function(i) {
  #   ifelse(Train$GT_by_TSTIN_LIV[i] == 0, log(1e-10), log(Train$GT_by_TSTIN_LIV[i]))})
  # 
  # Train$GT_by_TSTIN_SWR = sapply(1:nrow(Train), FUN = function(i) {
  #   ifelse(Train$GT_by_TSTIN_SWR[i] == 0, log(1e-10), log(Train$GT_by_TSTIN_SWR[i]))})
  # 
  # Train$Nitem_VPS_VOID = sapply(1:nrow(Train), FUN = function(i) {
  #   ifelse(Train$Nitem_VPS_VOID[i] == 0, log(1e-10), log(Train$Nitem_VPS_VOID[i]))})
  # 
  # Train$Nitem_LIVPP_VOID = sapply(1:nrow(Train), FUN = function(i) {
  #   ifelse(Train$Nitem_LIVPP_VOID[i] == 0, log(1e-10), log(Train$Nitem_LIVPP_VOID[i]))})
  # 
  # Train$Nitem_LIVPP_TL = sapply(1:nrow(Train), FUN = function(i) {
  #   ifelse(Train$Nitem_LIVPP_TL[i] == 0, log(1e-10), log(Train$Nitem_LIVPP_TL[i]))})
  # 
  # Train$CPS_LSWR = sapply(1:nrow(Train), FUN = function(i) {
  #   ifelse(Train$CPS_LSWR[i] == 0, log(1e-10), log(Train$CPS_LSWR[i]))})
  # 
  # Train$X27_Nitems_SWR_pow3 = sapply(1:nrow(Train), FUN = function(i) {
  #   ifelse(Train$X27_Nitems_SWR_pow3[i] == 0, log(1e-10), log(Train$X27_Nitems_SWR_pow3[i]))})
  # 
  # 
  # 
  # # Train$lineItemVoidsPerPosition = sapply(1:nrow(Train), FUN = function(i) {
  # #   ifelse(Train$lineItemVoidsPerPosition[i] == 0, log(1e-10), log(Train$lineItemVoidsPerPosition[i]))})
  # 
  # 
  # Test$scannedLineItemsPerSecond = sapply(1:nrow(Test), FUN = function(i) {
  #   ifelse(Test$scannedLineItemsPerSecond[i] == 0, log(1e-10), log(Test$scannedLineItemsPerSecond[i]))})
  # 
  # Test$valuePerSecond = sapply(1:nrow(Test), FUN = function(i) {
  #   ifelse(Test$valuePerSecond[i] == 0, log(1e-10), log(Test$valuePerSecond[i]))})
  # 
  # Test$GT_by_TSTIN = sapply(1:nrow(Test), FUN = function(i) {
  #   ifelse(Test$GT_by_TSTIN[i] == 0, log(1e-10), log(Test$GT_by_TSTIN[i]))})
  # 
  # Test$GT_by_TSTIN_QM = sapply(1:nrow(Test), FUN = function(i) {
  #   ifelse(Test$GT_by_TSTIN_QM[i] == 0, log(1e-10), log(Test$GT_by_TSTIN_QM[i]))})
  # 
  # Test$GT_by_TSTIN_LIV = sapply(1:nrow(Test), FUN = function(i) {
  #   ifelse(Test$GT_by_TSTIN_LIV[i] == 0, log(1e-10), log(Test$GT_by_TSTIN_LIV[i]))})
  # 
  # Test$GT_by_TSTIN_SWR = sapply(1:nrow(Test), FUN = function(i) {
  #   ifelse(Test$GT_by_TSTIN_SWR[i] == 0, log(1e-10), log(Test$GT_by_TSTIN_SWR[i]))})
  # 
  # Test$Nitem_VPS_VOID = sapply(1:nrow(Test), FUN = function(i) {
  #   ifelse(Test$Nitem_VPS_VOID[i] == 0, log(1e-10), log(Test$Nitem_VPS_VOID[i]))})
  # 
  # Test$Nitem_LIVPP_VOID = sapply(1:nrow(Test), FUN = function(i) {
  #   ifelse(Test$Nitem_LIVPP_VOID[i] == 0, log(1e-10), log(Test$Nitem_LIVPP_VOID[i]))})
  # 
  # Test$Nitem_LIVPP_TL = sapply(1:nrow(Test), FUN = function(i) {
  #   ifelse(Test$Nitem_LIVPP_TL[i] == 0, log(1e-10), log(Test$Nitem_LIVPP_TL[i]))})
  # 
  # Test$CPS_LSWR = sapply(1:nrow(Test), FUN = function(i) {
  #   ifelse(Test$CPS_LSWR[i] == 0, log(1e-10), log(Test$CPS_LSWR[i]))})
  # 
  # Test$X27_Nitems_SWR_pow3 = sapply(1:nrow(Test), FUN = function(i) {
  #   ifelse(Test$X27_Nitems_SWR_pow3[i] == 0, log(1e-10), log(Test$X27_Nitems_SWR_pow3[i]))})
  # 
  # 
  # # Test$lineItemVoidsPerPosition = sapply(1:nrow(Test), FUN = function(i) {
  # #   ifelse(Test$lineItemVoidsPerPosition[i] == 0, log(1e-10), log(Test$lineItemVoidsPerPosition[i]))})
  # 
  
  
  std_var_list = c(  "totalScanTimeInSeconds"                         ,
                     "grandTotal"                                     ,
                     "scannedLineItemsPerSecond"                      ,
                     "valuePerSecond"                                 ,
                     "lineItemVoidsPerPosition"                       ,
                     "X27_Nitems"                                     ,
                     "X78_valueItem"                                  ,
                     "Cross_SLIPS_2_Dis_TSTIS"                        ,
                     "Nitem_SWR"                                      ,
                     "GT_by_TSTIN_LIV"                                )
  
  if(standardize){
    dt = std.data(data.train = Train, data.test = Test, var.to.std = std_var_list)
    Train = as.data.frame(dt[[1]])
    Test = as.data.frame(dt[[2]])
    rm(dt)
  }
  
  Train$fraud = as.factor(Train$fraud)
  Train = Train[,final_var_list]
  Test = Test[,final_var_list_test]

  
  # 
  # "totalScanTimeInSeconds_2_discrete"              
  # "scannedLineItemsPerSecond_2_discrete" 
  # "valuePerSecond_2_discrete"                      
  # "likelihood_trustLevel"                          
  # "likelihood_scannedLineItemsPerSecond_2_discrete"
  # "likelihood_valuePerSecond_2_discrete"           
  # "Cross_SLIPS_2_Dis_TSTIS"                        
  # "Nitem_SWR"                                      
  # "Nitem_LTL"                                      
  # "Nitem_LVPS_2_dis"                               
  # "GT_by_TSTIN_LIV"                                
  # "Nitem_VPS_VOID"                                 
  # "X27_Nitems_pow3"         
  return(list(Train = Train, Test = Test))
}
