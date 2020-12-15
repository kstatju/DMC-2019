library(MASS)
library(klaR)
library(mda)
library(earth)

a = createDataPartition(df$fraud, times = 1, p = .7, list = T)


a1 = mda::fda(formula = fraud~., data = df, method = mars)
a2 = mda::fda(formula = fraud~., data = df, method = bruto)
options(digits=20)
a3 = mda::fda(formula = fraud~., data = df[a$Resample1,], method = earth, 
              degree = 4, nprune =20, Get.leverages = F,
              penalty = 4.4, fast.k = 0, nk = 100, 
              thresh = 0.01, minspan = -40, endspan = 80, Adjust.endspan = 10,
              fast.beta = 0,pmethod = "exhaustive", nfold = 5, ncross= 5)
b = predict(a3, newdata = df[-a$Resample1,], type = 'posterior')[,2]
table(df[-a$Resample1,'fraud'],b$D1)
a3$confusion



Config_Fda   = function(){
  Exp_Params = list()
  Exp_Params[[1]] = c(3, 4, 5, 6)                          #degree
  Exp_Params[[2]] = seq(2, 5, by = 1)                      #penalty
  Exp_Params[[3]] = c(80)                                  #nk
  Exp_Params[[4]] = c(0.0001)                              #thresh
  Exp_Params[[5]] = seq(-50, 0, 10)                        #minspan
  Exp_Params[[6]] = c(seq(0, 50, 10))                      #endspan
  Exp_Params[[7]] = c(0)                                   #fast.k
  Exp_Params[[8]] = c("exhaustive")                        #pmethod
  Exp_Params[[9]] = c(seq(2, 50, 10))                      #nprune
  Exp_Params[[10]] = c(5)                                  #nfold
  Exp_Params[[11]] = c(30)                                 #ncross
  Exp_Params[[12]] = seq(0, 10, 3)                         #Adjust.endspan
  Exp_Params[[13]] = c(F)                                  #Get.leverages
  
  Experiment = expand.grid(Exp_Params)
  Experiment$id = c(1:nrow(Experiment))
  colnames(Experiment) = c('id', paste0("P",c(1:13)))
  
  
  Exp_cols = c("P1.degree"            , 
               "P2.penalty"           , 
               "P3.nk"                ,
               "P4.thresh"            , 
               "P5.minspan"           ,
               "P6.endspan"           ,
               "P7.fast.k"            ,
               "P8.pmethod"           ,
               "P9.nprune"            ,
               "P10.nfold"            ,
               "P11.ncross"           ,
               "P12.Adjust.endspan"   ,
               "P13.Get.leverages"    )
  
  return(list(Exp = Experiment , names = Exp_cols , params = Exp_Params))
  
  
}

Set_Fda_Params   = function(Exp = NULL , id = 1){
  
  p = list(degree          = Exp$P1[id],
           penalty         = Exp$P2[id],
           nk              = Exp$P3[id],
           thresh          = Exp$P4[id],
           minspan         = Exp$P5[id],
           endspan         = Exp$P6[id],
           fast.k          = Exp$P7[id],
           pmethod         = Exp$P8[id],
           nprune          = Exp$P9[id],
           nfold           = Exp$P10[id],
           ncross          = Exp$P11[id],
           Adjust.endspan  = Exp$P12[id],
           Get.leverages   = Exp$P13[id])
  return(params = p)
}

Set_params      = function (Model, e){
  if(Model == "Fda"){return(Set_Fda_Params(Nnet_Exp, e))}

}


degree = c(2, 3, 4, 5, 6)
penalty = seq(2, 6, by = .5)
nk = 80
thresh = 0.0001
minspan = seq(-50, 0, 5)
endspan = c(seq(0, 20, 2), seq(25,100, 10))
fast.k = 0
pmethod = "exhaustive"
nprune = c(seq(2, 20, 1), seq(25,70,10))
nfold = 10
ncross= 50
Adjust.endspan = seq(0, 10, 1)
Get.leverages = F


a4 = update(a3, degree = 3, nprune=8, fast.beta = 0)
a4 = update(a3, degree = 8, nprune=100, penalty = 4)
a1$confusion
a2$confusion


a4$confusion
plot(a3)
plotmo(a3, type="variates", nresponse=1, clip=F)
summary(a4)
