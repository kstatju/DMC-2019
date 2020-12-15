library(rlang)
library(ggplot2)
library(stringi)


varname = names(train)[-c(5,10)]

plot_all = function(dt, varname, yvar, xvar = NULL, maxorder = NULL, colvar = "fraud", fillvar = NULL, xlimr=NULL, ylimr=NULL, path = NULL)
{
  ifelse(is.null(path),
         setwd("D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/"),
         setwd(path))
  xr = NULL
  nv = length(varname)
  fillvar = ifelse(is.null(fillvar), colvar, fillvar)
  maxorder = ifelse (is.null(maxorder), ifelse(nv > 4, 4, nv), maxorder)
  x = rep(list(c(0,1)),nv)
  if (is.null(xvar)){
    xr = expand.grid(x) %>% mutate(sum = rowSums(.[1:nv])) %>% 
      filter(sum >0 & sum <= maxorder) %>% arrange(sum) %>% select(-"sum")    
  }
  k = ifelse (!is.null(xr), nrow(xr), ifelse(is.vector(xvar), 1, nrow(xvar)))
  
  
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
    print(xxvar)
    
    # png(paste(paste(i,"V", which(xr[i,]==1),sep = "",collapse = ""),".png", sep = "", collapse = ""))

    png(paste(ifelse(i<9, paste("0",i,sep = ""),i), "_", yvar, "_",stri_replace_all_fixed(gsub("\\*","_",xxvar), " ", ""),
              ".png", sep = "", collapse = ""))    
    
    if(is.null(xlimr) & is.null(ylimr)){
      print(ggplot(dt, aes(x=!!parse_expr(xxvar), y = !!parse_expr(yvar), 
                      col = !!parse_expr(colvar), fill = !!parse_expr(fillvar))) + 
        geom_point()+theme(legend.text=element_text(size=20)))
      dev.off()
    } else if(is.null(xlimr) & !is.null(ylimr)) {
      print(ggplot(dt, aes(x=!!parse_expr(xxvar), y = !!parse_expr(yvar), 
                      col = !!parse_expr(colvar), fill = !!parse_expr(fillvar))) + 
        geom_point()+theme(legend.text=element_text(size=20))+ylim(ylimr))
      dev.off()
    } else if(!is.null(ylimr) & is.null(ylimr)) {
      print(ggplot(dt, aes(x=!!parse_expr(xxvar), y = !!parse_expr(yvar), 
                      col = !!parse_expr(colvar), fill = !!parse_expr(fillvar))) + 
        geom_point()+theme(legend.text=element_text(size=20))+xlim(xlimr))
      dev.off()
    }
  }
  
}

varname1 = names(train)[-c(5,10)]
plot_all(dt = train, varname = varname1, yvar = "scansWithoutRegistration", path = "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/scansWithoutRegistration/")


varname1 = names(train)[-c(2,10)]
plot_all(dt = train, varname = varname1, yvar = "totalScanTimeInSeconds", path = "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/totalScanTimeInSeconds/")

varname1 = names(train)[-c(3,10)]
plot_all(dt = train, varname = varname1, yvar = "grandTotal", path = "D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/grandTotal/")


ylimr = c(0,.15)
xvar = "totalScanTimeInSeconds"
yvar = "scannedLineItemsPerSecond"


limfile = read.csv("D:/ISU/Data Mining Cup DMC/DMC 2019/Plot/xylim.csv")
