plot_all_with_res = function(dt, varname, yvar, xvar = NULL, maxorder = NULL, path = NULL)
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
  
  pdf(paste(yvar,"_sidebyside.pdf", sep = "", collapse = ""), onefile = TRUE)
  
  for (i in seq(1, k, 4)){
    
    if (k%%4 > 0 & (k %/% 4 == (i-1)%/%4)){
      if(k%%4 == 1){
        xxvar1 = as.character(paste(varname[which(xr[i,]==1)], collapse = " * "))
        
        p1 = ggplot(dt1, aes(x=!!parse_expr(xxvar1), 
                             y = !!parse_expr(yvar), 
                             col = !!parse_expr(yvar))) + geom_point()
        
        print(grid.arrange(p1, ncol=2))
        varlist = c(varlist, xxvar1)
        
      }else if(k%%4 == 2){
        xxvar1 = as.character(paste(varname[which(xr[i,]==1)], collapse = " * "))
        xxvar2 = as.character(paste(varname[which(xr[i+1,]==1)], collapse = " * "))
        
        p1 = ggplot(dt1, aes(x=!!parse_expr(xxvar1), 
                             y = !!parse_expr(yvar), 
                             col = !!parse_expr(yvar))) + geom_point()
        
        p2 = ggplot(dt1, aes(x=!!parse_expr(xxvar2), 
                             y = !!parse_expr(yvar), 
                             col = !!parse_expr(yvar))) + geom_point()
        
        print(grid.arrange(p1, p2, ncol=2))
        varlist = c(varlist, xxvar1, xxvar2)
        
      }else if(k%%4 == 3){
        xxvar1 = as.character(paste(varname[which(xr[i,]==1)], collapse = " * "))
        xxvar2 = as.character(paste(varname[which(xr[i+1,]==1)], collapse = " * "))
        xxvar3 = as.character(paste(varname[which(xr[i+2,]==1)], collapse = " * "))
        
        p1 = ggplot(dt1, aes(x=!!parse_expr(xxvar1), 
                             y = !!parse_expr(yvar), 
                             col = !!parse_expr(yvar))) + geom_point()
        
        p2 = ggplot(dt1, aes(x=!!parse_expr(xxvar2), 
                             y = !!parse_expr(yvar), 
                             col = !!parse_expr(yvar))) + geom_point()
        
        p3 = ggplot(dt1, aes(x=!!parse_expr(xxvar3), 
                             y = !!parse_expr(yvar), 
                             col = !!parse_expr(yvar))) + geom_point()
        
        print(grid.arrange(p1, p2, p3, ncol=2))
        varlist = c(varlist, xxvar1, xxvar2, xxvar3)
        
        
      }
    }else{
      xxvar1 = as.character(paste(varname[which(xr[i,]==1)], collapse = " * "))
      xxvar2 = as.character(paste(varname[which(xr[i+1,]==1)], collapse = " * "))
      xxvar3 = as.character(paste(varname[which(xr[i+2,]==1)], collapse = " * "))
      xxvar4 = as.character(paste(varname[which(xr[i+3,]==1)], collapse = " * "))
      
      p1 = ggplot(dt1, aes(x=!!parse_expr(xxvar1), 
                           y = !!parse_expr(yvar), 
                           col = !!parse_expr(yvar))) + geom_point()
      
      p2 = ggplot(dt1, aes(x=!!parse_expr(xxvar2), 
                           y = !!parse_expr(yvar), 
                           col = !!parse_expr(yvar))) + geom_point()
      
      p3 = ggplot(dt1, aes(x=!!parse_expr(xxvar3), 
                           y = !!parse_expr(yvar), 
                           col = !!parse_expr(yvar))) + geom_point()
      
      p4 = ggplot(dt1, aes(x=!!parse_expr(xxvar4), 
                           y = !!parse_expr(yvar), 
                           col = !!parse_expr(yvar))) + geom_point()
      
      print(grid.arrange(p1, p2, p3, p4, ncol=2))
      varlist = c(varlist, xxvar1, xxvar2, xxvar3, xxvar4)
    }


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
combin_dt1 = combin_dt1[combin_dt1$scannedLineItemsPerSecond<.1,]
combin_dt1 = combin_dt1[combin_dt1$valuePerSecond<.2,]
combin_dt1 = combin_dt1[combin_dt1$lineItemVoidsPerPosition<.5,]

nrow(combin_dt) - nrow(combin_dt1)
nrow(combin_dt1[combin_dt1$dttype == "Train",])
varname = names(train)



# list of variables (remove "dttype" from this list also response variable)

varname1 = names(train)[-c(10, 11)]
# directory path where the pdf file will be saved
file_save_path = "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/"

# This following code will produce a pdf file named with the fraud_sidebside.pdf and 
# contains all plots or order "maxorder" with response variable (fraud)
# "plot_all_with_res" function will return all variable list for each plot

plot_var_list_1 = plot_all_with_res(dt = combin_dt2, varname = varname1, yvar = "fraud", maxorder = 3,
                           path = file_save_path)
