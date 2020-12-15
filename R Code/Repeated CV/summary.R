

a = Tunning_out %>% group_by(P1.degree, P2.penalty, Cutoff) %>% 
  summarise(mean = mean(Test_PredictionCost), var = var(Test_PredictionCost))


b = mda::mda(formula = fraud~., df_train, subclasses = c(4,5), method = gen.ridge, lambda = 100)
c = as.data.frame(predict(b, newdata = Train, type = 'posterior')[,2])

for (i in 1:n_folds){
  a = cbind(a, df[df_folds$test_index[[i]],'fraud'], yhat_test[[i]])
}

a = cbind(df[df_folds$test_index[[i]],'fraud'], yhat_test[[i]])


df_nf = read.csv("D:/ISU/Data Mining Cup DMC/DMC 2019/R code/Condo Out using new feature/Output/Logistic_up_Tunning_summary_out_set2_10f.csv")
              
df_nf = df_nf[df_nf$Test_PredictionCost_mean>14,]

write.csv(df, "D:/ISU/Data Mining Cup DMC/DMC 2019/R code/Condo Out using new feature/Output/Logistic_up_Tunning_summary_out_set2_short.csv", row.names = F)


library(ggplot2)
library(dplyr)

bb = df %>%group_by(Test_PredictionCost_mean) %>% 
  summarise_at(c("Test_PredictionCost_var", "Train_PredictionCost_var", 
                 "Train_PredictionCost_mean", "Test_Accuracy_mean", "Train_Accuracy_mean"), 
               .funs = c(mean = mean, min = min, max = max))
ggplot(bb, aes(x = Test_PredictionCost_mean, y = Test_Accuracy_mean_mean))+geom_point()+
  geom_point(aes(x = Test_PredictionCost_mean, y = Train_Accuracy_mean_mean, col = Train_Accuracy_mean_mean))

df = read.csv("D:/ISU/Data Mining Cup DMC/DMC 2019/R code/Condo Output/Logistic_none_Tunning_summary_out.csv")
df1 = df[df$Test_PredictionCost_mean>30,]

df1 = df[df$Test_PredictionCost_mean>22 & df$Test_PredictionCost_var<800 & df$Cutoff>0.97 & df$P1.alpha>0.5,]
df2 = df[df$Test_PredictionCost_mean>22 & df$Test_PredictionCost_var<800 & df$Cutoff>0.97 & df$P1.alpha<0.5,]

a = df$P1.alpha[df$Test_PredictionCost_mean>30]
b = sort(unique(a))

ggplot(df1, aes(x = Test_PredictionCost_mean, y = Test_PredictionCost_var))+geom_point()
ggplot(df1, aes(x = Cutoff, y = Test_PredictionCost_mean))+geom_point()
ggplot(df1, aes(x = Test_Sensitivity_mean, y = Test_Specificity_mean))+geom_point()
ggplot(df1, aes(x = Test_Sensitivity_mean, y = Test_PredictionCost_mean))+geom_point()
ggplot(df1, aes(x = Test_Specificity_mean, y = Test_PredictionCost_mean))+geom_point()
ggplot(df1, aes(x = Test_Accuracy_mean, y = Test_PredictionCost_mean))+geom_point()
ggplot(df1, aes(x = Test_MissingFraud_mean, y = Test_FalseAlarm_mean))+geom_point()
ggplot(df1, aes(x = Test_MissingFraud_mean, y = Test_PredictionCost_mean))+geom_point()
ggplot(df1, aes(x = Test_FalseAlarm_mean, y = Test_PredictionCost_mean))+geom_point()


ggplot(df2, aes(x = Test_PredictionCost_mean, y = Test_PredictionCost_var))+geom_point()
ggplot(df2, aes(x = Cutoff, y = Test_PredictionCost_mean))+geom_point()
ggplot(df2, aes(x = Test_Sensitivity_mean, y = Test_Specificity_mean))+geom_point()
ggplot(df2, aes(x = Test_Sensitivity_mean, y = Test_PredictionCost_mean))+geom_point()
ggplot(df2, aes(x = Test_Specificity_mean, y = Test_PredictionCost_mean))+geom_point()
ggplot(df2, aes(x = Test_Accuracy_mean, y = Test_PredictionCost_mean))+geom_point()
ggplot(df2, aes(x = Test_MissingFraud_mean, y = Test_FalseAlarm_mean))+geom_point()
ggplot(df2, aes(x = Test_MissingFraud_mean, y = Test_PredictionCost_mean))+geom_point()
ggplot(df2, aes(x = Test_FalseAlarm_mean, y = Test_PredictionCost_mean))+geom_point()



df = read.csv("D:/ISU/Data Mining Cup DMC/DMC 2019/R code/Condo Output/Fda_none_Tunning_summary_out.csv")
df1 = df[df$Test_PredictionCost_mean>25,]

write.csv(df[df$Train_PredictionCost_mean>22 & df$Test_PredictionCost_var<700,], "D:/ISU/Data Mining Cup DMC/DMC 2019/R code/Condo Output/Fda_short.csv")

