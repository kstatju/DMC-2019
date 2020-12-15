0.9885

df1 = scale(df[,!(names(df) %in% c("fraud"))])
df = data.frame(df1, fraud = df[,"fraud"])
df_train = upSample(x=df[,!(names(df) %in% c("fraud"))] , 
                      y=as.factor(df$fraud) , 
                      yname = "fraud")
nm = names(df_train)
xnm = nm[!(nm%in%c('fraud'))]
cv.lasso <- cv.glmnet(x = as.matrix(df_train[,xnm]), 
                      y = df_train$fraud, 
                      alpha = .88, 
                      family = "binomial")
fit = glmnet(x = as.matrix(df_train[,xnm]), y = df_train$fraud, 
             alpha = .88, 
             family = "binomial", 
             lambda = cv.lasso$lambda.min)

myCoefs <- coef(fit, s="lambda.min");
myCoefs[which(myCoefs != 0 ) ] 
nam = as.vector(unlist(myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ][-1]))


df_train = upSample(x=df[,!(names(df) %in% c("fraud"))] , 
                    y=as.factor(df$fraud) , 
                    yname = "fraud")

a = mda::fda(formula = fraud~.,
         data = df_train,
         method = earth,
         degree          = 3,
         penalty         = 4.5,
         nk              = 80,
         thresh          = 0.0001,
         minspan         = 3,
         endspan         = 75,
         fast.k          = 0,
         pmethod         = "backward",
         nprune          = 15,
         nfold           = 5,
         ncross          = 1,
         Adjust.endspan  = 8,
         Get.leverages   = F)

opt = options()
options(digits=15)
options(opt)

pred = predict(fit, as.matrix(Train[,xnm]), type = "response")
pred1 = predict(fit, as.matrix(Test), type = "response")
prop.table(table(Train$fraud, pred>0.946))
prop.table(table(pred>0.946))
prop.table(table(pred1>0.946))


[1] "trustLevel"                "totalScanTimeInSeconds"    "grandTotal"                "lineItemVoids"            
[5] "scansWithoutRegistration"  "quantityModifications"     "scannedLineItemsPerSecond" "valuePerSecond"           
[9] "lineItemVoidsPerPosition"  "fraud"                     "X27_Nitems"                "X78_valueItem"


aa = data.frame(Train, pred = ifelse(pred>0.946, 1,0))
bb = aa[(aa$s0==0 & aa$fraud==1) | (aa$s0==1 & aa$fraud==0),]

ggplot(Train, aes(y = X27_Nitems, x = quantityModifications, col = fraud, size=fraud))+
  geom_point(alpha =0.4)

ggplot(Train, aes(y = grandTotal/(1+scansWithoutRegistration*quantityModifications), x = trustLevel, col = fraud, size=fraud))+
  geom_point(alpha =0.4)

ggplot(Train, aes(y = grandTotal/(1+scansWithoutRegistration*quantityModifications), x = trustLevel, col = fraud, size=fraud))+
  geom_point(alpha =0.4)


ggplot(Train, aes(y = grandTotal/(1+lineItemVoidsPerPosition), x = scannedLineItemsPerSecond, col = fraud, size=fraud))+
  geom_point(alpha =0.4)+scale_x_continuous(limits = c(0,.4))

ggplot(Train, aes(x = quantityModifications/X27_Nitems, y = grandTotal, col = fraud, size=fraud))+
  geom_point(alpha =0.4)+scale_x_continuous(limits = c(0,.5))





table(con_2_dis_variable$totalScanTimeInSeconds_2_discrete, con_2_dis_variable$scannedLineItemsPerSecond_2_discrete)
table(con_2_dis_variable$totalScanTimeInSeconds_2_discrete, con_2_dis_variable$valuePerSecond_2_discrete)
table(con_2_dis_variable$scannedLineItemsPerSecond_2_discrete, con_2_dis_variable$valuePerSecond_2_discrete)

table(con_2_dis_variable_test$totalScanTimeInSeconds_2_discrete, con_2_dis_variable_test$scannedLineItemsPerSecond_2_discrete)
table(con_2_dis_variable_test$totalScanTimeInSeconds_2_discrete, con_2_dis_variable_test$valuePerSecond_2_discrete)
table(con_2_dis_variable_test$scannedLineItemsPerSecond_2_discrete, con_2_dis_variable_test$valuePerSecond_2_discrete)

Train$gr = "Train"
Test$fraud = NA
Test$gr = "Test"


df = rbind(Train, Test)

final_var_list = c(  "totalScanTimeInSeconds"                         ,
                     "grandTotal"                                     ,
                     "lineItemVoids"                                  ,
                     "quantityModifications"                          ,
                     "scannedLineItemsPerSecond"                      ,
                     "valuePerSecond"                                 ,
                     "X27_Nitems"                                     ,
                     "totalScanTimeInSeconds_2_discrete"              ,
                     "scannedLineItemsPerSecond_2_discrete"           ,
                     "valuePerSecond_2_discrete"                      ,
                     "likelihood_trustLevel"                          ,
                     "likelihood_lineItemVoids"                       ,
                     "likelihood_scansWithoutRegistration"            ,
                     "likelihood_totalScanTimeInSeconds_2_discrete"   ,
                     "likelihood_scannedLineItemsPerSecond_2_discrete",
                     "likelihood_valuePerSecond_2_discrete"           ,
                     "Cross_SLIPS_2_Dis_TSTIS"                        ,
                     "Nitem_SWR"                                      ,
                     "Nitem_LIV"                                      ,
                     "Nitem_TL"                                       ,
                     "SLIPS_2_dis_TSTIS_2_dis_LTL"                    ,
                     "SLIPS_TSTIS_2_dis_LTL"                          ,
                     "Nitem_LTL"                                      ,
                     "Nitem_LLIV"                                     ,
                     "SLIPS_2_dis_SWR_LTSTIS_2_dis"                   ,
                     "SLIPS_2_dis_LTL_LTSTIS_2_dis"                   ,
                     "LTL_LSWR_LLSLIPS_2_dis"                         ,
                     "Nitem_LVPS_2_dis"                               ,
                     "LTL_LSLIPS_2_dis_LVPS_2_dis"                    ,
                     "GT_by_TSTIN"                                    ,
                     "GT_by_TSTIN_QM"                                 ,
                     "GT_by_TSTIN_LIV"                                ,
                     "GT_by_TSTIN_SWR"                                ,
                     "Nitem_VPS_VOID"                                 ,
                     "Nitem_LIVPP_VOID"                               ,
                     "Nitem_LIVPP_TL"                                 ,
                     "CPS_LSWR"                                       ,
                     "X27_Nitems_pow3"                                ,
                     "X27_Nitems_SWR_pow3"                            ,
                     "trustLevel1"                                    ,
                     "trustLevel2"                                    )


for (i in final_var_list){
  print(ggplot(df, aes(!!parse_expr(i), fill = gr)) + geom_density(alpha = 0.2)  )
}

print(ggplot(df, aes(sapply(1:nrow(df), FUN = function(i) {
  ifelse(df$SLIPS_2_dis_TSTIS_2_dis_LTL[i] == 0, log(1e-10), 
         log(df$SLIPS_2_dis_TSTIS_2_dis_LTL[i]))}), fill = gr)) + geom_density(alpha = 0.2)  )

print(ggplot(df, aes((GT_by_TSTIN_LIV-mean(GT_by_TSTIN_LIV))/sd(GT_by_TSTIN_LIV), fill = gr)) + geom_density(alpha = 0.2))
print(ggplot(df, aes((GT_by_TSTIN_LIV), fill = gr)) + geom_density(alpha = 0.2))


+scale_x_continuous(limits = c(0,2)))
