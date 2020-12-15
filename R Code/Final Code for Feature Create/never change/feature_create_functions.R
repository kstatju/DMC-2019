#############################################################
## DMC 2019
## Author: Kanak Choudhury
##
#############################################################
##
## Function Lists
##
##
## standardize_data             : Standardize train data for given variables or all variables
## standardize_data_test        : Standardize test data for based on training data
## normalize_data               : Normalize given variable list between x and x+1
## normalize_data_test          : Normalize test data based on training data between x and x+1
## discrete_to_binary           : Convert discrete variable to Binary (1,-1) 
## continuous_to_discreat       : Convert continuous variable to discrete
##                                for given number of groups 
## continuous_to_discreat_test  : Convert continuous test data variable to discrete
##                                based on the training data
## condi_summary_var            : Create conditional variable 5 number summary
## condi_summary_var_test       : Create conditional variable based on training data
## crossprod_var                : Create crossprod for given number of order
## sum_var                      : Create sum of variables for given order
## power_product                : Create power for selected variables for given order
## likelihood_ratio             : Create likelihood ratio features for given discreat variables
## likelihood_ratio_test        : Create likelihood ratio features for for test data
##
##
#############################################################


source('feature_selection.R')
source('outlier_knn_score.R')
source('outlier_princ_comp_score.R')


flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

sin_feature <- function(dt, varlist, path)
{
  b = as.data.frame(lapply(varlist, FUN = function(a) sin(dt[[a]])))
  names(b) = paste("sin_", varlist, sep = "")
  write.table(x = names(b), file = paste(path,"/varlist_sin.txt", sep = ""),
              row.names = F, col.names = F,sep = ",", quote = F)
  return(b)
}



inverse_feature <- function(dt, varlist, path)
{
  
  aa <- function(dd){
    minx = 1/ifelse(dd==0, min(dd[dd>0])/10, dd)
  }
  b = as.data.frame(lapply(varlist, FUN = function(x) aa(dt[[x]])))
  names(b) = paste("inverse_", varlist, sep = "")
  write.table(x = names(b), file = paste(path,"/varlist_inverse.txt", sep = ""),
              row.names = F, col.names = F,sep = ",", quote = F)
  return(b)
}



cos_feature <- function(dt, varlist, path)
{
  b = as.data.frame(lapply(varlist, FUN = function(a) cos(dt[[a]])))
  names(b) = paste("cos_", varlist, sep = "")
  write.table(x = names(b), file = paste(path,"/varlist_cos.txt", sep = ""),
              row.names = F, col.names = F,sep = ",", quote = F)
  return(b)
}



x124_combin <- function(dt)
{
  X124_combination <- as.data.frame(ifelse(dt$trustLevel == 1 & dt$totalScanTimeInSeconds <= 1312 & 
                                             dt$lineItemVoidsPerPosition > 0.458, 1, 0))
  names(X124_combination) = "x124_combination"
  return(X124_combination)
}


good_feature <- function(dt, rf_feature_cut = 5)
{
  library(caret)
  library(randomForest)
  rnd_for_fea_select = randomForest_selection(data = dt)
  
  rf_imp_conditional_var = row.names(rnd_for_fea_select)[rnd_for_fea_select$`0`>=rf_feature_cut & 
                                                           rnd_for_fea_select$`1`>=rf_feature_cut]
  
  # Boruta Feature Selection
  library(Boruta)
  boruta_fea_select = boruta_selection(data = dt)
  
  ## Common feature in both random forest and Boruta
  
  return(unique(c(rf_imp_conditional_var, boruta_fea_select)))
}



standardize_data <- function(train, path, varlist=NULL)
{
  if (is.null(varlist))
  {
    varlist = names(train)[!(names(train) %in% c('fraud', 'ID'))]
  }
  
  mean_var = data.frame(meanx = colMeans(train[,varlist]),varx = sqrt(diag(var(train[,varlist]))))
  
  bb <- function(dt, i, varlist, mean_var)
  {
    cc = as.data.frame((dt[[varlist[i]]]-mean_var[i,1])/mean_var[i,2])
    names(cc) = paste(varlist[i], "_std", sep = "")
    return(cc)
  }
  
  aa = as.data.frame(lapply(1:length(varlist), function(x) bb(train, x, varlist, mean_var)))
  write.table(names(aa), file = paste(path, "/varlist_standardize_feature.txt", sep = ""),sep = ",", 
              row.names = F, col.names = F, quote = F)
  write.table(mean_var, file = paste(path, "/standardize_mean_var.txt", sep = ""),sep = ",", 
              row.names = F, quote = F)
  return(aa)
  
  
}



standardize_data_test <- function(test, path, varlist=NULL)
{
  if (is.null(varlist))
  {
    varlist = names(test)[!(names(test) %in% c('fraud', 'ID'))]
  }
  
  
  bb <- function(dt, i, varlist, mean_var)
  {
    cc = as.data.frame((dt[[varlist[i]]]-mean_var[i,1])/mean_var[i,2])
    names(cc) = paste(varlist[i], "_std", sep = "")
    return(cc)
  }
  
  mean_var = read.table(file = paste(path, "/standardize_mean_var.txt", sep = ""),sep = ",", header = T)
  
  aa = as.data.frame(lapply(1:length(varlist), function(x) bb(test, x, varlist, mean_var)))

  return(aa)
  
  
}



normalize_data <- function(train, test, varlist, path, normalize_min = 0)
{
  library(dplyr)
  dt = rbind(train[,varlist], test[,varlist])
  min_max = dt %>% summarise_at(varlist, funs(maxx = max, minx = min))
  
  aa <- function(dt, min_max, var1)
  {
    var1_m = paste(var1, "_maxx", sep = "")
    var1_mi = paste(var1, "_minx", sep = "")
    bb = as.data.frame(((dt[[var1]]-min_max[[var1_mi]])/(min_max[[var1_m]]-min_max[[var1_mi]]))+normalize_min)
    names(bb) = paste(var1, "_normalize", sep = "")
    return(bb)
  }
  
  dt1 = as.data.frame(lapply(varlist, function(x) aa(train, min_max, x)))
  vname = names(dt1)
  write.table(vname, file = paste(path, "/varlist_normalize_feature.txt", sep = ""),sep = ",", 
              row.names = F, col.names = F, quote = F)
  write.table(min_max, file = paste(path, "/normalize_prem.txt", sep = ""),sep = ",", 
              row.names = F, quote = F)
  return(dt1)
  
}



normalize_data_test <- function(test, varlist, path, normalize_min = 0)
{

  aa <- function(dt, min_max, var1)
  {
    var1_m = paste(var1, "_maxx", sep = "")
    var1_mi = paste(var1, "_minx", sep = "")
    bb = as.data.frame(((dt[[var1]]-min_max[[var1_mi]])/(min_max[[var1_m]]-min_max[[var1_mi]]))+normalize_min)
    names(bb) = paste(var1, "_normalize", sep = "")
    return(bb)
  }
  
  min_max = read.table(file = paste(path, "/normalize_prem.txt", sep = ""), sep = ",", header = T)
  
  dt1 = as.data.frame(lapply(varlist, function(x) aa(test, min_max, x)))

  return(dt1)
  
}



discrete_to_binary <- function(dt, varlist, path)
{
  foreachvariabel <- function(dt, var1)
  {
    uni_num = sort(unique(dt[[var1]]))
    a = as.data.frame(sapply(1:length(uni_num), FUN = function(x) ifelse(dt[[var1]]==uni_num[x], 1, -1), simplify = T))
    names(a) = paste(var1, uni_num, sep = "")
    return(a)
  }
  
  b = as.data.frame(sapply(varlist, FUN = function(a) foreachvariabel(dt, a), simplify = T))
  names(b) = gsub("\\.", "_", names(b))
  write.table(x = names(b), file = paste(path,"/varlist_dis_2_bin.txt", sep = ""),
              row.names = F, col.names = F,sep = ",", quote = F)
  return(b)
}



continuous_to_discreat <- function(dt, test_dt, varlist, ngroup, path) 
{
  aa <- function(dt, var1, ngroup, path)
  {
    bin = quantile(x = dt[[var1]], probs = seq(0,1,1/ngroup))
    mintr = min(dt[[var1]])
    mints = min(test_dt[[var1]])
    maxtr = max(dt[[var1]])
    maxts = max(test_dt[[var1]])
    bin[1] = min(mintr, mints)-1
    bin[length(bin)] = max(maxtr, maxts)+1
    b = rowSums(sapply(1:(length(bin)-1), FUN = function (a) ifelse(dt[[var1]]>=bin[a] & dt[[var1]] < bin[a+1], a, 0)))
    write.table(bin, file = paste(path, "/", var1, "_bin.txt",sep = ""), append = F, row.names = F, col.names = F)    
    return(b)
  }
  
  bb = data.frame(sapply(varlist, FUN = function(x) aa(dt, x, ngroup, path)))
  names(bb) = paste(varlist, "_2_discrete", sep = "")
  write.table(x = names(bb), file = paste(path,"/varlist_cont_2_disc.txt", sep = ""),
              row.names = F, col.names = F,sep = ",", quote = F)
  return(bb)
}



continuous_to_discreat_test <- function(dt, varlist, path) 
{
  aa <- function(dt, var1, path)
  {
    bin = unlist(read.table(file = paste(path, "/", var1, "_bin.txt",sep = "")))
    b = rowSums(sapply(1:(length(bin)-1), FUN = function (a) ifelse(dt[[var1]]>=bin[a] & dt[[var1]] < bin[a+1], a, 0)))
    return(b)
  }
  
  bb = data.frame(lapply(varlist, FUN = function(x) aa(dt, x, path)))
  names(bb) = paste(varlist, "_2_discrete", sep = "")
  return(bb)
}




condi_summary_var <- function(train, varlist_cont, varlist_disc, path)
{
  library('dplyr')
  nm = names(train)
  
  abc <- function(train, var1, varlist_cont, path)
  {
    sum_group = as.data.frame(train %>% group_by_at(var1) %>% 
                                summarise_at(varlist_cont, funs(quantile(.,probs = c(1)),
                                                                quantile(.,probs = c(.75)),
                                                                quantile(.,probs = c(.5)),
                                                                quantile(.,probs = c(.25)),
                                                                quantile(.,probs = c(0)),
                                                                mean(.,na.rm = T))))
    
    
    sum_vars = c("_max", "_75q", "_50q", "_25q", "_min", "_mean")
    d = as.vector(sapply(sum_vars, FUN = function(x) paste(var1, varlist_cont, x, sep = "")))
    names(sum_group)[-1] = d
    write.table(sum_group, file = paste(path, "/conditional_summary_", var1, ".txt", sep = ""), quote = F, row.names = F, sep = ",")
    return(sum_group)
  }
  
  gg = lapply(varlist_disc, FUN = function(x) abc(train, x, varlist_cont, path))

  for (i in c(1:length(varlist_disc))){
    train = merge(train, gg[[i]], by = varlist_disc[i], all = T, sort = F)
    
  }
  
  nm1 = names(train)[!(names(train) %in% nm)]
  write.table(nm1, file = paste(path, "/varlist_conditional_feature.txt", sep = ""), quote = F, row.names = F, sep = ",")
  
  train = train[order(train$ID),]
  return(train)
}



condi_summary_var_test <- function(test, varlist_disc, path)
{
  for (i in c(1:length(varlist_disc)))
  {
    gg = read.table(file = paste(path, "/conditional_summary_", var1, ".txt", sep = ""), sep = ",", header = T)
    test = merge(test, gg, by = varlist_disc[i], all = T, sort = F)
  }
  
  test = test[order(test$ID),]
  
  return(test)
}



crossprod_var = function(dt, varlist, order =2, path)
{
  library(dplyr)
  
  nv = length(varlist)
  x = rep(list(c(0,1)),nv)
  
  xr = expand.grid(x) %>% mutate(sum = rowSums(.[1:nv])) %>% 
      filter(sum >1 & sum<=order) %>% arrange(sum) %>% select(-"sum")    
  k = nrow(xr)
  
  a = as.data.frame(sapply(1:k, function(i) apply(dt[,varlist[which(xr[i,]==1)]],1,prod)))
  nam = sapply(1:k, function(i) paste("cross_", paste(varlist[which(xr[i,]==1)],sep = "", collapse = "_"), collapse = "_", sep = ""))
  names(a) = nam
  write.table(nam, file = paste(path, "/varlist_cross_prod.txt", sep = ""), row.names = F, col.names = F)
  
  return(a)
}



sum_var_feature = function(dt, varlist, order =2, path)
{
  library(dplyr)
  
  nv = length(varlist)
  x = rep(list(c(0,1)),nv)
  
  xr = expand.grid(x) %>% mutate(sum = rowSums(.[1:nv])) %>% 
    filter(sum >1 & sum<=order) %>% arrange(sum) %>% select(-"sum")    
  k = nrow(xr)
  
  a = as.data.frame(sapply(1:k, function(i) apply(dt[,varlist[which(xr[i,]==1)]],1,sum)))
  nam = sapply(1:k, function(i) paste("sum_", paste(varlist[which(xr[i,]==1)],sep = "", collapse = "_"), collapse = "_", sep = ""))
  names(a) = nam
  write.table(nam, file = paste(path, "/varlist_sum.txt", sep = ""), row.names = F, col.names = F)
  
  return(a)
}



ratio_var_feature = function(dt, varlist, path)
{
  library(dplyr)
  
  nv = length(varlist)
  x = rep(list(c(0,1)),nv)
  
  xr = expand.grid(x) %>% mutate(sum = rowSums(.[1:nv])) %>% 
    filter(sum==2) %>% arrange(sum) %>% select(-"sum")    
  k = nrow(xr)
  
  aa <- function(dat, i, varlist, xr1){
    d1 = dat[, varlist[which(xr1[i,]==1)]]
    d1[,1] = d1[,1]+ifelse(min(d1[,1])<1, 1-min(d1[,1]),0)
    d1[,2] = d1[,2]+ifelse(min(d1[,2])<1, 1-min(d1[,2]),0)
    return(d1[,1]/d1[,2])
  }
  
  a = as.data.frame(sapply(1:k, function(i) aa(dt, i, varlist, xr)))
  nam = sapply(1:k, function(i) paste("ratio_", paste(varlist[which(xr[i,]==1)],sep = "", collapse = "_"), collapse = "_", sep = ""))
  names(a) = nam
  write.table(nam, file = paste(path, "/varlist_ratio.txt", sep = ""), row.names = F, col.names = F)
  
  return(a)
}



diff_var_feature = function(dt, varlist, path)
{
  library(dplyr)
  
  nv = length(varlist)
  x = rep(list(c(0,1)),nv)
  
  xr = expand.grid(x) %>% mutate(sum = rowSums(.[1:nv])) %>% 
    filter(sum==2) %>% arrange(sum) %>% select(-"sum")    
  k = nrow(xr)
  
  a = as.data.frame(sapply(1:k, function(i) apply(dt[,varlist[which(xr[i,]==1)]],1,function(x) x[1]-x[2])))
  nam = sapply(1:k, function(i) paste("diff_", paste(varlist[which(xr[i,]==1)],sep = "", collapse = "_"), collapse = "_", sep = ""))
  names(a) = nam
  write.table(nam, file = paste(path, "/varlist_diff.txt", sep = ""), row.names = F, col.names = F)
  
  return(a)
}



sum_var = function(dt, varlist, order = 2, path)
{
  library(dplyr)
  
  nv = length(varlist)
  x = rep(list(c(0,1)),nv)
  
  xr = expand.grid(x) %>% mutate(sum = rowSums(.[1:nv])) %>% 
    filter(sum == order) %>% arrange(sum) %>% select(-"sum")    
  k = nrow(xr)
  
  a = as.data.frame(sapply(1:k, function(i) apply(dt[,varlist[which(xr[i,]==1)]],1,sum)))
  nam = sapply(1:k, function(i) paste("sum", paste(which(xr[i,]==1),sep = "", collapse = "_"), collapse = "_", sep = ""))
  names(a) = nam
  write.table(nam, file = paste(path, "/varlist_sum.txt", sep = ""), row.names = F, col.names = F)
  
  return(a)
}



power_product = function(dt, varlist, order =2, path)
{
  a = as.data.frame(lapply(varlist, function(x) sapply(2:order, function(i) dt[,x]^i)))
  nam = as.vector(sapply(varlist, function(i) paste("pow", paste(i, 2:order, sep = "_"), sep = "_")))
  names(a) = nam
  write.table(nam, file = paste(path, "/varlist_power_porduct.txt", sep = ""), row.names = F, col.names = F)
  
  return(a)
}



likelihood_ratio <- function(train, varlist_disc, path)
{
  library('dplyr')
  nm = names(train)
  
  abc <- function(train, var1, path)
  {
    sum_group = as.data.frame(train %>% group_by_at(var1) %>% 
                                summarise_at("fraud", funs(sum(.)/(n()-sum(.)))))
    
    names(sum_group)[-1] = paste("likelihood_", var1, sep = "")
    write.table(sum_group, file = paste(path, "/likelihood_", var1, ".txt", sep = ""), quote = F, row.names = F, sep = ",")
    return(sum_group)
  }
  
  gg = lapply(varlist_disc, FUN = function(x) abc(train, x, path))
  
  for (i in c(1:length(varlist_disc))){
    train = merge(train, gg[[i]], by = varlist_disc[i], all = T, sort = F)
    
  }
  
  nm1 = names(train)[!(names(train) %in% nm)]
  write.table(nm1, file = paste(path, "/varlist_likelihood_feature.txt", sep = ""), quote = F, row.names = F, sep = ",")
  
  train = train[order(train$ID),]
  return(train)
}



likelihood_ratio_test <- function(test, varlist_disc, path)
{
  for (i in c(1:length(varlist_disc))){
    names(sum_group)[-1] = paste("likelihood_", var1, sep = "")
    gg = read.table(file = paste(path, "/likelihood_", varlist_disc[i], ".txt", sep = ""), sep = ",", header = T)
    test = merge(test, gg, by = varlist_disc[i], all = T, sort = F)
  }
  test = test[order(test$ID),]
  return(test)
}
