#############################################################
## DMC 2019
## Author: Kanak Choudhury
##
#############################################################
##
## Function Lists
##
##
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


condi_summary_var <- function(dt_c, varlist_cont, varlist_disc, path)
{
  library('dplyr')
  
  abc <- function(dt_c, var1, varlist_cont, path)
  {
    sum_group = as.data.frame(dt_c %>% group_by_at(var1) %>% 
                                summarise_at(varlist_cont, funs(quantile(.,probs = c(1)),
                                                                quantile(.,probs = c(.75)),
                                                                quantile(.,probs = c(.5)),
                                                                quantile(.,probs = c(.25)),
                                                                quantile(.,probs = c(0)),
                                                                mean(.,na.rm = T))))
    
    
    sum_vars = c("_max", "_75q", "_50q", "_25q", "_min", "_mean")
    d = as.vector(sapply(sum_vars, FUN = function(x) paste(var1, varlist_cont, x, sep = "")))
    names(sum_group)[-1] = d
    write.table(sum_group, file = paste(path, "/varlist", var1, "_cond_summary.txt", sep = ""), quote = F, row.names = F, sep = ",")
    return(sum_group)
  }
  
  gg = lapply(varlist_disc, FUN = function(x) abc(dt_c, x, varlist_cont, path))

  for (i in c(1:length(varlist_disc))){
    dt_c = merge(dt_c, gg[[i]], by = varlist_disc[i], all = T, sort = F)
    
  }
  dt_c = dt_c[order(dt_c$ID),]
  return(dt_c)
}


condi_summary_var_test <- function(dt_c, varlist_disc, path)
{
  for (i in c(1:length(varlist_disc)))
  {
    gg = read.table(file = paste(path, "/varlist", varlist_disc[i], "_cond_summary.txt", sep = ""), sep = ",", header = T)
    dt_c = merge(dt_c, gg, by = varlist_disc[i], all = T, sort = F)
  }
  
  dt_c = dt_c[order(dt_c$ID),]
  
  return(dt_c)
}


crossprod_var = function(dt, varlist, maxorder =2, path)
{
  library(dplyr)
  
  nv = length(varlist)
  x = rep(list(c(0,1)),nv)
  
  xr = expand.grid(x) %>% mutate(sum = rowSums(.[1:nv])) %>% 
      filter(sum >1 & sum <= maxorder) %>% arrange(sum) %>% select(-"sum")    
  k = nrow(xr)
  
  a = as.data.frame(sapply(1:k, function(i) apply(dt[,varlist[which(xr[i,]==1)]],1,prod)))
  nam = sapply(1:k, function(i) paste("cross", paste(which(xr[i,]==1),sep = "", collapse = "_"), collapse = "_", sep = ""))
  names(a) = nam
  write.table(nam, file = paste(path, "/varlist_cross_prod.txt", sep = ""), row.names = F, col.names = F)
  
  return(a)
}

sum_var = function(dt, varlist, maxorder = 2, path)
{
  library(dplyr)
  
  nv = length(varlist)
  x = rep(list(c(0,1)),nv)
  
  xr = expand.grid(x) %>% mutate(sum = rowSums(.[1:nv])) %>% 
    filter(sum >1 & sum <= maxorder) %>% arrange(sum) %>% select(-"sum")    
  k = nrow(xr)
  
  a = as.data.frame(sapply(1:k, function(i) apply(dt[,varlist[which(xr[i,]==1)]],1,sum)))
  nam = sapply(1:k, function(i) paste("sum", paste(which(xr[i,]==1),sep = "", collapse = "_"), collapse = "_", sep = ""))
  names(a) = nam
  write.table(nam, file = paste(path, "/varlist_sum.txt", sep = ""), row.names = F, col.names = F)
  
  return(a)
}


power_product = function(dt, varlist, maxorder =2, path)
{
  a = as.data.frame(lapply(varlist, function(x) sapply(2:maxorder, function(i) dt[,x]^i)))
  nam = as.vector(sapply(varlist, function(i) paste("pow", paste(i, 2:maxorder, sep = ""), sep = "_")))
  names(a) = nam
  write.table(nam, file = paste(path, "/varlist_power_porduct.txt", sep = ""), row.names = F, col.names = F)
  
  return(a)
}



likelihood_ratio <- function(dt_c, varlist_disc, path)
{
  library('dplyr')
  
  abc <- function(dt_c, var1, path)
  {
    sum_group = as.data.frame(dt_c %>% group_by_at(var1) %>% 
                                summarise_at("fraud", funs(sum(fraud)/(n()-sum(fraud)))))
    
    names(sum_group)[-1] = paste("likelihood_", var1, sep = "")
    write.table(sum_group, file = paste(path, "/likelihood_", var1, ".txt", sep = ""), quote = F, row.names = F, sep = ",")
    return(sum_group)
  }
  
  gg = lapply(varlist_disc, FUN = function(x) abc(dt_c, x, path))
  
  for (i in c(1:length(varlist_disc))){
    dt_c = merge(dt_c, gg[[i]], by = varlist_disc[i], all = T, sort = F)
    
  }
  dt_c = dt_c[order(dt_c$ID),]
  return(dt_c)
}



likelihood_ratio_test <- function(dt_c, varlist_disc, path)
{
  for (i in c(1:length(varlist_disc))){
    names(sum_group)[-1] = paste("likelihood_", var1, sep = "")
    gg = read.table(file = paste(path, "/likelihood_", varlist_disc[i], ".txt", sep = ""), sep = ",", header = T)
    dt_c = merge(dt_c, gg, by = varlist_disc[i], all = T, sort = F)
  }
  dt_c = dt_c[order(dt_c$ID),]
  return(dt_c)
}
