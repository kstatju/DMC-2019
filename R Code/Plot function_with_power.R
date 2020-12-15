plot_all_with_power = function(dt, varname, yvar, xvar = NULL, maxorder = NULL, colvar = "fraud", 
                    fillvar = NULL, path = NULL)
{
  library(rlang)
  library(ggplot2)
  library(stringi)
  require(gridExtra)
  library(dplyr)
  
  varlist = NULL
  ifelse(is.null(path),
         setwd("D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/"),
         setwd(path))
  # dt$fraud = as.factor(dt$fraud)
  dt1 = dt[dt$dttype=="Train",]
  xr = NULL
  nv = length(varname)
  
  if (maxorder>nv) {
    message("Maxorder can't be grater than number of variable in varname!")
    return()
  }
  
  # fillvar = ifelse(is.null(fillvar), colvar, fillvar)
  
  maxorder = ifelse (is.null(maxorder), ifelse(nv > 4, 4, nv), maxorder)
  x = rep(list(c(0,1)),nv)
  
  if (is.null(xvar)){
    xr = expand.grid(x) %>% mutate(sum = rowSums(.[1:nv])) %>% 
      filter(sum >0 & sum <= maxorder) %>% arrange(sum) %>% select(-"sum")    
  }
  
  k = ifelse (!is.null(xr), nrow(xr), ifelse(is.vector(xvar), 1, nrow(xvar)))
  

    pdf(paste(yvar,".pdf", sep = "", collapse = ""), onefile = TRUE) 

  for (i in 1:k){
    
    if(!is.null(xr)){
      xxvar = as.character(paste(varname[which(xr[i,]==1)], collapse = " * "))
    } else {
      if(is.vector(xvar)){
        xxvar = as.character(paste(xvar, collapse = " * "))
      } else {
        xxvar = as_string(paste(xvar[i,], collapse = " * "))
      }
    }
    
    varlist = c(varlist, xxvar)
    
    p1 = ggplot(dt1, aes(x=!!parse_expr(xxvar), y = !!parse_expr(yvar), 
                           col = !!parse_expr(colvar), size = !!parse_expr(colvar)
                         )) + geom_point(alpha = .4)+
      theme(axis.title=element_text(size=7))+scale_size_continuous(range = c(1,5))

    
    p2 = ggplot(dt, aes(x=!!parse_expr(yvar) * !!parse_expr(xxvar), 
                          col = !!parse_expr(colvar), 
                        fill = !!parse_expr(fillvar))) + 
        geom_density(alpha=.3)+theme(axis.title=element_text(size=7))
    
    p3 = ggplot(dt1, aes(x=!!parse_expr(yvar) * !!parse_expr(xxvar), 
                           y = !!parse_expr(colvar), size = !!parse_expr(colvar),
                           col = !!parse_expr(colvar))) + geom_point(alpha = .4)+
      theme(axis.title=element_text(size=7))+scale_size_continuous(range = c(1,5))
    
    p4 = ggplot(dt1, aes(x=1/!!parse_expr(xxvar), 
                         y = 1/!!parse_expr(yvar), size = !!parse_expr(colvar),
                         col = !!parse_expr(colvar))) + geom_point(alpha = .4)+
      theme(axis.title=element_text(size=7))+scale_size_continuous(range = c(1,5))
    
    p5 = ggplot(dt1, aes(x=(!!parse_expr(xxvar))^2, 
                         y = (!!parse_expr(yvar))^2, size = !!parse_expr(colvar),
                         col = !!parse_expr(colvar))) + geom_point(alpha = .4)+
      theme(axis.title=element_text(size=7))+scale_size_continuous(range = c(1,5))
    
    p6 = ggplot(dt1, aes(x=(!!parse_expr(xxvar))^3, 
                         y = (!!parse_expr(yvar))^3, size = !!parse_expr(colvar),
                         col = !!parse_expr(colvar))) + geom_point(alpha = .4)+
      theme(axis.title=element_text(size=7))+scale_size_continuous(range = c(1,5))

    a = grid.arrange(p1, p2, p3, p4, p5, p6, ncol=2)
    print(a)
  }
  dev.off()
  return(varlist)
}



train = read.csv(file = "D:/ISU/Data Mining Cup DMC/DMC 2019/Data/train_TL12.csv", header = T, sep = ",")
test = read.csv(file = "D:/ISU/Data Mining Cup DMC/DMC 2019/Data/test_TL12.csv", header = T, sep = ",")
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
combin_dt1 = combin_dt1[combin_dt1$scannedLineItemsPerSecond<.06,]
combin_dt1 = combin_dt1[combin_dt1$scannedLineItemsPerSecond>.01,]
combin_dt1 = combin_dt1[combin_dt1$valuePerSecond<.12,]
combin_dt1 = combin_dt1[combin_dt1$lineItemVoidsPerPosition<.4,]
combin_dt1 = combin_dt1[combin_dt1$grandTotal>1.5,]

# mean_all = colMeans(combin_dt1[combin_dt1$dttype == "Train",-c(10,11)])
# var_all = diag(var(combin_dt1[combin_dt1$dttype == "Train",-c(10,11)]))
# 
# combin_dt2 = sapply(as.list(1:9), function(i) (combin_dt1[,i] - mean_all[i])/var_all[i])
# 
# combin_dt2 = as.data.frame(cbind(combin_dt2, combin_dt1[,c(10,11)]))
# names(combin_dt2) = names(combin_dt1)



varname = names(train)


yaxisvar = "trustLevel"
varname1 = names(train)[-c(1,10, 11)]
file_save_path = "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/"


plot_var_list_1 = plot_all_with_power(dt = combin_dt1, varname = varname1, yvar = yaxisvar, 
                                      fillvar = "dttype", maxorder = 3,
                                      path = file_save_path)

write.csv(plot_var_list_1, "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/varlist_trustLevel.csv", quote = F,row.names = T)



# Same as above for another Y-axis variable
yaxisvar = "totalScanTimeInSeconds"
varname1 = names(train)[-c(2,10, 11)]


plot_var_list_2 = plot_all_with_power(dt = combin_dt1, varname = varname1, yvar = yaxisvar, 
                                      fillvar = "dttype", maxorder = 4,
                                      path = file_save_path)

write.csv(plot_var_list_2, "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/varlist_totalScanTimeInSeconds.csv", quote = F,row.names = T)

# Same as above for another Y-axis variable
yaxisvar = "grandTotal"
varname1 = names(train)[-c(3,10, 11)]


plot_var_list_3 = plot_all_with_power(dt = combin_dt1, varname = varname1, yvar = yaxisvar, 
                                      fillvar = "dttype", maxorder = 4,
                                      path = file_save_path)

write.csv(plot_var_list_3, "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/varlist_grandTotal.csv", quote = F,row.names = T)

yaxisvar = "lineItemVoids"
varname1 = names(train)[-c(4,10, 11)]


plot_var_list_4 = plot_all_with_power(dt = combin_dt1, varname = varname1, yvar = yaxisvar, 
                                      fillvar = "dttype", maxorder = 4,
                                      path = file_save_path)

write.csv(plot_var_list_4, "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/varlist_lineItemVoids.csv", quote = F,row.names = T)

# Y-axis variable name
yaxisvar = "scansWithoutRegistration"
# list of variables for the X-axis 
# (remove "dttype" from this list also response variable and Y-asix variable)
varname1 = names(train)[-c(5,10, 11)]
# directory path where the pdf file will be saved
file_save_path = "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/"

plot_var_list_5 = plot_all_with_power(dt = combin_dt1, varname = varname1, yvar = yaxisvar, 
                           fillvar = "dttype", maxorder = 4,
                           path = file_save_path)

write.csv(plot_var_list_5, "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/varlist_scansWithoutRegistration.csv", quote = F,row.names = T)


yaxisvar = "quantityModifications"
varname1 = names(train)[-c(6,10, 11)]


plot_var_list_6 = plot_all_with_power(dt = combin_dt1, varname = varname1, yvar = yaxisvar, 
                                      fillvar = "dttype", maxorder = 4,
                                      path = file_save_path)

write.csv(plot_var_list_6, "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/varlist_quantityModifications.csv", quote = F,row.names = T)

yaxisvar = "scannedLineItemsPerSecond"
varname1 = names(train)[-c(7,10, 11)]


plot_var_list_7 = plot_all_with_power(dt = combin_dt1, varname = varname1, yvar = yaxisvar, 
                                      fillvar = "dttype", maxorder = 4,
                                      path = file_save_path)

write.csv(plot_var_list_7, "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/varlist_scannedLineItemsPerSecond.csv", quote = F,row.names = T)

yaxisvar = "valuePerSecond"
varname1 = names(train)[-c(8,10, 11)]


plot_var_list_8 = plot_all_with_power(dt = combin_dt1, varname = varname1, yvar = yaxisvar, 
                                      fillvar = "dttype", maxorder = 4,
                                      path = file_save_path)

write.csv(plot_var_list_8, "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/varlist_valuePerSecond.csv", quote = F,row.names = T)

yaxisvar = "lineItemVoidsPerPosition"
varname1 = names(train)[-c(9,10, 11)]


plot_var_list_9 = plot_all_with_power(dt = combin_dt1, varname = varname1, yvar = yaxisvar, 
                                      fillvar = "dttype", maxorder = 4,
                                      path = file_save_path)

write.csv(plot_var_list_9, "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/varlist_lineItemVoidsPerPosition.csv", quote = F,row.names = T)


# hist(train$trustLevel)
# hist(train$totalScanTimeInSeconds)
# hist(train$grandTotal)
# hist(train$lineItemVoids)
# hist(train$scansWithoutRegistration)
# hist(train$quantityModifications)
# hist(train$scannedLineItemsPerSecond)
# hist(log(train$valuePerSecond))

x = sort(runif(n = 100, min = 0, max = 20))
plot(x)
lines(log(x))


combin_dt$X27_product = combin_dt$totalScanTimeInSeconds*combin_dt$scannedLineItemsPerSecond
table(combin_dt[combin_dt$dttype=="Train",]$fraud,
                        combin_dt[combin_dt$dttype=="Train",]$trustLevel, 
                        combin_dt[combin_dt$dttype=="Train",]$lineItemVoids)
