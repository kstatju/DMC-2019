plot_all = function(dt, varname, yvar, xvar = NULL, maxorder = NULL, colvar = "fraud", 
                    fillvar = NULL, xylimr=NULL, path = NULL, sidebyside = F, testplot = F)
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
  
  if(!sidebyside & !testplot){
    pdf(paste(yvar,".pdf", sep = "", collapse = ""), onefile = TRUE) 
  }else if(sidebyside){
    pdf(paste(yvar,"_sidebyside.pdf", sep = "", collapse = ""), onefile = TRUE)
  }else if(testplot){
    pdf(paste(yvar,"_Train_Test.pdf", sep = "", collapse = ""), onefile = TRUE)
  }
  
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
    
    # png(paste(paste(i,"V", which(xr[i,]==1),sep = "",collapse = ""),".png", sep = "", collapse = ""))
    
    if(!is.null(xylimr)){
      xlimr = c(if(is.na(xylimr$xlim1[i])) NULL else {xylimr$xlim1[i]},
                  if(is.na(xylimr$xlim2[i])) NULL else {xylimr$xlim2[i]})
      ylimr =  c(if(is.na(xylimr$ylim1[i])) NULL else {xylimr$ylim1[i]},
                 if(is.na(xylimr$ylim2[i])) NULL else {xylimr$ylim2[i]})
    
    } else{
      xlimr = NULL
      ylimr = NULL
    }
    
    if (sidebyside){
      p1 = ggplot(dt1, aes(x=!!parse_expr(xxvar), y = !!parse_expr(yvar), 
                           col = !!parse_expr(colvar))) + geom_point()
    }
    
    if(is.null(xylimr) || (is.null(xlimr) & is.null(ylimr))){
      p2 = ggplot(dt1, aes(x=!!parse_expr(xxvar), y = !!parse_expr(yvar), 
                           col = !!parse_expr(colvar))) + geom_point()
      
    } else if(!is.null(xylimr) & is.null(xlimr) & !is.null(ylimr)) {
      p2 = ggplot(dt1, aes(x=!!parse_expr(xxvar), y = !!parse_expr(yvar), 
                           col = !!parse_expr(colvar))) + geom_point()+ylim(ylimr)

    } else if(!is.null(xylimr) & !is.null(xlimr) & is.null(ylimr)) {
      p2 = ggplot(dt, aes(x=!!parse_expr(xxvar), y = !!parse_expr(yvar), 
                           col = !!parse_expr(colvar))) + geom_point()+xlim(xlimr)

    }else if(!is.null(xylimr) & !is.null(xlimr) & !is.null(ylimr)) {
      p2= ggplot(dt1, aes(x=!!parse_expr(xxvar), y = !!parse_expr(yvar), 
                           col = !!parse_expr(colvar))) + geom_point()+xlim(xlimr)+ylim(ylimr)
    }
    
    
    if (sidebyside & !is.null(fillvar) & testplot){
      p5 = ggplot(dt, aes(x=!!parse_expr(yvar) * !!parse_expr(xxvar), 
                          col = !!parse_expr(colvar), fill = !!parse_expr(fillvar))) + 
        geom_density(alpha=.3)
    }
    
    if((is.null(xylimr) || is.null(xlimr)) & !is.null(fillvar) & testplot){
      p6 = ggplot(dt, aes(x=!!parse_expr(yvar) * !!parse_expr(yvar) * !!parse_expr(xxvar), 
                          col = !!parse_expr(colvar), fill = !!parse_expr(fillvar))) + 
        geom_density(alpha=.3)
    
    } else if((!is.null(xylimr) & !is.null(xlimr) & !is.null(fillvar) & !testplot)) {
      p6 = ggplot(dt, aes(x=!!parse_expr(yvar) * !!parse_expr(xxvar), 
                          col = !!parse_expr(colvar), fill = !!parse_expr(fillvar))) + 
        geom_density(alpha=.3)+xlim(xlimr)
      
    }
    
    
    
    if (sidebyside){
      p3 = ggplot(dt1, aes(x=!!parse_expr(yvar) * !!parse_expr(xxvar), 
                           y = !!parse_expr(colvar), 
                           col = !!parse_expr(colvar))) + geom_point()
    }
    
    if(is.null(xylimr) || (is.null(xlimr) & testplot)){
      p4 = ggplot(dt1, aes(x=!!parse_expr(yvar) * !!parse_expr(xxvar), 
                           y = !!parse_expr(colvar),  
                           col = !!parse_expr(colvar))) +
        geom_point()
    
    } else if(!is.null(xylimr) & !is.null(xlimr) & testplot) {
      p4 = ggplot(dt1, aes(x=!!parse_expr(yvar) * !!parse_expr(xxvar), 
                           y = !!parse_expr(colvar),
                           col = !!parse_expr(colvar))) + 
        geom_point()+xlim(xlimr)
      
    }
    
    
    if(sidebyside){
      # png(paste(ifelse(i<9, paste("0",i,sep = ""),i), "_", yvar, "_",stri_replace_all_fixed(gsub("\\*","_",xxvar), " ", ""),
      #           ".png", sep = "", collapse = ""))
      print(grid.arrange(p1, p2, p3, p4, p5, p6, ncol=2))
      # dev.off()
    
    }else if(testplot){
      # png(paste(ifelse(i<9, paste("0",i,sep = ""),i), "_", yvar, "_",stri_replace_all_fixed(gsub("\\*","_",xxvar), " ", ""),
      #           "_1",".png", sep = "", collapse = ""))
      print(p2)
      # dev.off()
      # png(paste(ifelse(i<9, paste("0",i,sep = ""),i), "_", yvar, "_",stri_replace_all_fixed(gsub("\\*","_",xxvar), " ", ""),
      #           "_2",".png", sep = "", collapse = ""))
      print(p4)
      # dev.off()
      # png(paste(ifelse(i<9, paste("0",i,sep = ""),i), "_", yvar, "_",stri_replace_all_fixed(gsub("\\*","_",xxvar), " ", ""),
      #           "_3",".png", sep = "", collapse = ""))
      print(p6)
      # dev.off()
    
    }else if(!testplot){
      # png(paste(ifelse(i<9, paste("0",i,sep = ""),i), "_", yvar, "_",stri_replace_all_fixed(gsub("\\*","_",xxvar), " ", ""),
      #           ".png", sep = "", collapse = ""))
      print(p2)
      # dev.off()
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

varname = names(train)


# Y-axis variable name
yaxisvar = "scansWithoutRegistration"
# list of variables for the X-axis 
# (remove "dttype" from this list also response variable and Y-asix variable)
varname1 = names(train)[-c(5,10, 11)]
# directory path where the pdf file will be saved
file_save_path = "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/"

# This following code will produce a pdf file named with the Y-axis variable name and 
# contains all plots or order "maxorder"
# "plot_all" function will return all variable list for each plot

plot_var_list_1 = plot_all(dt = combin_dt, varname = varname1, yvar = yaxisvar, 
                         fillvar = "dttype",testplot = F,maxorder = 3,
                         path = file_save_path)

# Chack that pdf file and create a csv file contains X-axis and Y-axis limits 
# File format uploaded on BOX "xylim.csv"
# read that CSV file here

limfile = read.csv("D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/xylim.csv", sep = ",")

# Following code will produce a pdf file contains all sidebyside plots with trancated axis

plot_var_list_1 = plot_all(dt = combin_dt, varname = varname1, yvar = yaxisvar, 
                           fillvar = "dttype", xylimr = limfile, 
                           sidebyside = T, testplot = T, maxorder = 3,
                           path = file_save_path)


# Same as above for another Y-axis variable
yaxisvar = "totalScanTimeInSeconds"
varname1 = names(train)[-c(2,10, 11)]


plot_var_list_2 = plot_all(dt = combin_dt, varname = varname1, yvar = yaxisvar, 
                           fillvar = "dttype",testplot = F,maxorder = 3,
                           path = file_save_path)

limfile = read.csv("D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/xylim.csv", sep = ",")


plot_var_list_3 = plot_all(dt = combin_dt, varname = varname1, yvar = yaxisvar, 
                           fillvar = "dttype", xylimr = limfile, 
                           sidebyside = T, testplot = T, maxorder = 3,
                           path = file_save_path)


# Same as above for another Y-axis variable
yaxisvar = "grandTotal"
varname1 = names(train)[-c(3,10, 11)]


plot_var_list_3 = plot_all(dt = combin_dt, varname = varname1, yvar = yaxisvar, 
                           fillvar = "dttype",testplot = F,maxorder = 3,
                           path = file_save_path)

limfile = read.csv("D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/xylim.csv", sep = ",")

plot_var_list_3 = plot_all(dt = combin_dt, varname = varname1, yvar = yaxisvar, 
                           fillvar = "dttype", xylimr = limfile, 
                           sidebyside = T, testplot = T, maxorder = 3,
                           path = file_save_path)


