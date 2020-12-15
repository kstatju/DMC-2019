
data_feature <- function(path)
{
  ## Location where file will be saved and data and everything
  library(caret)
  library(randomForest)
  library(Boruta)
  
  setwd(path)
  
  ## Sourcing feature engineering functions
  
  source('feature_create_functions.R')
  source('feature_selection.R')
  
  
  ## Read train and test data
  
  train = read.csv(file = paste(path, "/train.csv",sep = ""), header = T, sep = "|")
  test = read.csv(file = paste(path, "/test.csv",sep = ""), header = T, sep = "|")
  
  
  ## Subset data for Trust Level < 3
  
  train = train[train$trustLevel <3,]
  test = test[test$trustLevel <3,]
  
  train$ID <- seq.int(nrow(train))
  test$ID <- seq.int(nrow(test))
  
  
  
  ## Variable list for discrete, continuous, response and all
  
  var_name = names(train)
  dis_var = var_name[c(1,4,5,6)]
  con_var = var_name[c(2,3,7,8,9)]
  res_var = var_name[10]
  
  train = train[,c("ID", 
                   "fraud",
                   "trustLevel",
                   "totalScanTimeInSeconds",
                   "grandTotal",
                   "lineItemVoids",
                   "scansWithoutRegistration",
                   "quantityModifications",
                   "scannedLineItemsPerSecond",
                   "valuePerSecond",
                   "lineItemVoidsPerPosition")]
  
  ## Log transformation for skewed variables
  
  train$scannedLineItemsPerSecond = log(train$scannedLineItemsPerSecond+
                                          ifelse(min(train$scannedLineItemsPerSecond) > 0, 1, 
                                                 min(train$scannedLineItemsPerSecond)+1))
  train$valuePerSecond = log(train$valuePerSecond+
                                          ifelse(min(train$valuePerSecond) > 0, 1, 
                                                 min(train$valuePerSecond)+1))
  train$lineItemVoidsPerPosition = log(train$lineItemVoidsPerPosition+
                                          ifelse(min(train$lineItemVoidsPerPosition) > 0, 1, 
                                                 min(train$lineItemVoidsPerPosition)+1))
  
  
  test$scannedLineItemsPerSecond = log(test$scannedLineItemsPerSecond+
                                          ifelse(min(test$scannedLineItemsPerSecond) > 0, 1, 
                                                 min(test$scannedLineItemsPerSecond)+1))
  test$valuePerSecond = log(test$valuePerSecond+
                               ifelse(min(test$valuePerSecond) > 0, 1, 
                                      min(test$valuePerSecond)+1))
  test$lineItemVoidsPerPosition = log(test$lineItemVoidsPerPosition+
                                         ifelse(min(test$lineItemVoidsPerPosition) > 0, 1, 
                                                min(test$lineItemVoidsPerPosition)+1))
  
  ####################################################################################
  ##
  ## Training data preparation
  ##
  ####################################################################################
  
  ## Transform continuous variable to discrete
  
  con_to_dic_var = continuous_to_discreat(dt = train, 
                                              test_dt = test, 
                                              varlist = con_var, 
                                              ngroup = 20, 
                                              path = path)
  con_to_dis_var_list = names(con_to_dic_var)
  # write.table(x = con_to_dis_var_list, file = paste(path, "/FEATURE_con_to_disc_imp.txt", sep = ""),
  #             quote = F, sep = ",", row.names = F, col.names = F)
  
  all_discrete_var = c(dis_var, names(con_to_dic_var))
  train_with_con_2_dis = data.frame(train, con_to_dic_var)
  rm(test)
  
  ## Likelihod ratio feature for all discrete variables
  
  like_ratio_feature = likelihood_ratio(train = train_with_con_2_dis, 
                                        varlist_disc = all_discrete_var, 
                                        path = path)
  like_var_list = names(like_ratio_feature)[!(names(like_ratio_feature) %in% c(con_to_dis_var_list, var_name))]
  # write.table(x = like_var_list, file = paste(path, "/FEATURE_likelihood_ratio_imp.txt", sep = ""),
  #             quote = F, sep = ",", row.names = F, col.names = F)
  
  likelihood_ratio_var = like_ratio_feature[,like_var_list]
  
  all_continuous_var = c(con_var, like_var_list)
  
  rm(train_with_con_2_dis)
  
  ## Discrete to Binary (1, -1) conversion
  
  disc_to_bin_ftr_dis_var = discrete_to_binary(dt = like_ratio_feature,
                                               varlist = dis_var,
                                               path = path)
  dis_2_bin_var_name = names(disc_to_bin_ftr_dis_var)
  
  # disc_to_bin_ftr_all_dis_var = discrete_to_binary(dt = like_ratio_feature, 
  #                                                  varlist = all_discrete_var, 
  #                                                  path = path)
  # dis_2_bin_all_var_name = names(disc_to_bin_ftr_all_dis_var)
  
  # write.table(x = dis_2_bin_all_var_name, file = paste(path, "/FEATURE_disc_2_bin_all_imp.txt", sep = ""),
  #             quote = F, sep = ",", row.names = F, col.names = F)
  
  
  ## Conditional summary variabls for continuous variable based on discrete
  
  condi_sumry_vari = condi_summary_var(train = like_ratio_feature, 
                                     varlist_cont = all_continuous_var, 
                                     varlist_disc = all_discrete_var, 
                                     path = path)
  
  # condi_sumry_var_list = names(condi_sumry_vari)[!(names(condi_sumry_vari) %in% c(like_var_list, var_name))]
  
  # conditional_imp_feature = good_feature(dt = condi_sumry_vari, rf_feature_cut = 5)
  # conditional_imp_feature = conditional_imp_feature[!(conditional_imp_feature %in% c(like_var_list, var_name))]
  # condi_sumry_vari = condi_sumry_vari[,conditional_imp_feature]
  # write.table(x = as.vector(unlist(conditional_imp_feature)), file = paste(path, "/FEATURE_conditional_imp.txt", sep = ""), 
  #             quote = F, sep = ",", row.names = F, col.names = F)
  
  
  conditional_imp_feature = as.vector(unlist(read.table(file = paste(path, "/FEATURE_conditional_imp.txt", sep = ""))))
  condi_sumry_vari = condi_sumry_vari[,c(names(like_ratio_feature), conditional_imp_feature)]
  condi_sumry_var = condi_sumry_vari[,conditional_imp_feature]
  
  
  ## 5th order polinomial 
  
  pow_varlist  = names(condi_sumry_vari)
  pow_varlist = pow_varlist[!(pow_varlist %in% c("ID", "fraud"))]
  pol_5_var = power_product(dt = condi_sumry_vari, varlist = pow_varlist, order = 5, path = path)
  
  
  # dt2 = data.frame(condi_sumry_vari, pol_5_var)
  # pow5_imp_feature_1 = good_feature(dt = dt2, rf_feature_cut = 5)
  # pow5_imp_feature = pow5_imp_feature_1[!(pow5_imp_feature_1 %in% c(pow_varlist, "ID"))]
  # write.table(x = as.vector(unlist(pow5_imp_feature)), file = paste(path, "/FEATURE_5th_order_imp.txt", sep = ""),
  #             quote = F, sep = ",", row.names = F, col.names = F)
  # rm(dt2)
  
  pow5_imp_feature = as.vector(unlist(read.table(file = paste(path, "/FEATURE_5th_order_imp.txt", sep = ""), sep = ",")))
  pol_5_var = pol_5_var[,pow5_imp_feature]
  
  rm(condi_sumry_vari)
  
  ## 3th order cross product 
  
  cross_var_list = names(like_ratio_feature)[!(names(like_ratio_feature) %in% c("fraud", "ID"))]
  cross_var = crossprod_var(dt = like_ratio_feature, varlist = cross_var_list, order = 3, path = path)
  
  # dt_cross = data.frame(like_ratio_feature, cross_var)
  # rf_cross_fea_select = randomForest_selection(data = dt_cross)
  # rf_imp_cross_var = row.names(rf_cross_fea_select)[rf_cross_fea_select$`0`>=5 & 
  #                                                           rf_cross_fea_select$`1`>=5]
  # cross_imp_feature = rf_imp_cross_var[!(rf_imp_cross_var %in% names(like_ratio_feature))]
  # write.table(x = as.vector(unlist(cross_imp_feature)), file = paste(path, "/FEATURE_cross_prod_imp.txt", sep = ""),
  #             quote = F, sep = ",", row.names = F, col.names = F)
  # # rm(cross_var)
  # rm(dt_cross, rf_imp_cross_var)
  
  cross_imp_feature = as.vector(unlist(read.table(file = paste(path, "/FEATURE_cross_prod_imp.txt", sep = ""),
                                                  sep = ",")))
  cross_var = cross_var[,cross_imp_feature]
  
  ## 3 order sum feature
  
  sum_var_list = names(like_ratio_feature)[!(names(like_ratio_feature) %in% c("fraud", "ID"))]
  sum_var = sum_var_feature(dt = like_ratio_feature, varlist = sum_var_list, order = 3, path = path)
  
  # dt_sum = data.frame(like_ratio_feature, sum_var)
  # rf_sum_fea_select = randomForest_selection(data = dt_sum)
  # rf_imp_sum_var = row.names(rf_sum_fea_select)[rf_sum_fea_select$`0`>=5 & 
  #                                                     rf_sum_fea_select$`1`>=5]
  # sum_imp_feature = rf_imp_sum_var[!(rf_imp_sum_var %in% names(like_ratio_feature))]
  # write.table(x = as.vector(unlist(sum_imp_feature)), file = paste(path, "/FEATURE_sum_imp.txt", sep = ""),
  #             quote = F, sep = ",", row.names = F, col.names = F)
  # rm(dt_sum, rf_imp_sum_var)
  
  sum_imp_feature = as.vector(unlist(read.table(file = paste(path, "/FEATURE_sum_imp.txt", sep = ""),
                                                  sep = ",")))
  sum_var = sum_var[,sum_imp_feature]
  
  ## 2nd order Diff feature
  
  diff_var_list = names(like_ratio_feature)[!(names(like_ratio_feature) %in% c("fraud", "ID"))]
  diff_var = diff_var_feature(dt = like_ratio_feature, varlist = diff_var_list, path = path)
  
  # dt_diff = data.frame(like_ratio_feature, diff_var)
  # rf_diff_fea_select = good_feature(dt =  dt_diff, rf_feature_cut = 5)
  # diff_imp_feature = rf_diff_fea_select[!(rf_diff_fea_select %in% names(like_ratio_feature))]
  # write.table(x = as.vector(unlist(diff_imp_feature)), file = paste(path, "/FEATURE_diff_imp.txt", sep = ""),
  #             quote = F, sep = ",", row.names = F, col.names = F)
  # rm(dt_diff, rf_diff_fea_select)
  
  diff_imp_feature = as.vector(unlist(read.table(file = paste(path, "/FEATURE_diff_imp.txt", sep = ""),
                                                sep = ",")))
  diff_var = diff_var[,diff_imp_feature]
  
  ## Ratio feature
  
  ratio_var_list = names(like_ratio_feature)[!(names(like_ratio_feature) %in% c("fraud", "ID"))]
  ratio_var = ratio_var_feature(dt = like_ratio_feature, varlist = ratio_var_list, path = path)
  
  # dt_ratio = data.frame(like_ratio_feature, ratio_var)
  # rf_ratio_fea_select = good_feature(dt = dt_ratio, rf_feature_cut = 5)
  # ratio_imp_feature = rf_ratio_fea_select[!(rf_ratio_fea_select %in% names(like_ratio_feature))]
  # write.table(x = as.vector(unlist(ratio_imp_feature)), file = paste(path, "/FEATURE_ratio_imp.txt", sep = ""),
  #             quote = F, sep = ",", row.names = F, col.names = F)
  # rm(dt_ratio, rf_ratio_fea_select)
  
  ratio_imp_feature = as.vector(unlist(read.table(file = paste(path, "/FEATURE_ratio_imp.txt", sep = ""),
                                                 sep = ",")))
  ratio_var = ratio_var[,ratio_imp_feature]
  
  
  ## sig feature
  sin_var_list = names(like_ratio_feature)[!(names(like_ratio_feature) %in% c("fraud", "ID"))]
  sin_var = sin_feature(dt = like_ratio_feature, varlist = sin_var_list, path = path)
  # write.table(x = names(sin_var), file = paste(path, "/FEATURE_sin_imp.txt", sep = ""),
  #             quote = F, sep = ",", row.names = F, col.names = F)
  
  sin_var_list = as.vector(unlist(read.table(file = paste(path, "/FEATURE_sin_imp.txt", sep = ""),sep = ",")))
  sin_var = sin_var[,sin_var_list]
  
  ## cos feature
  cos_var_list = names(like_ratio_feature)[!(names(like_ratio_feature) %in% c("fraud", "ID"))]
  cos_var = cos_feature(dt = like_ratio_feature, varlist = cos_var_list, path = path)
  # write.table(x = names(cos_var), file = paste(path, "/FEATURE_cos_imp.txt", sep = ""),
  #             quote = F, sep = ",", row.names = F, col.names = F)
  
  cos_var_list = as.vector(unlist(read.table(file = paste(path, "/FEATURE_cos_imp.txt", sep = ""),sep = ",")))
  cos_var = cos_var[,cos_var_list]
  
  
  ## inverse feature
  
  inverse_var_list = names(like_ratio_feature)[!(names(like_ratio_feature) %in% c("fraud", "ID"))]
  inverse_var = inverse_feature(dt = like_ratio_feature, varlist = inverse_var_list, path = path)
  # write.table(x = names(inverse_var), file = paste(path, "/FEATURE_inverse_imp.txt", sep = ""),
  #             quote = F, sep = ",", row.names = F, col.names = F)
  
  inverse_var_list = as.vector(unlist(read.table(file = paste(path, "/FEATURE_inverse_imp.txt", sep = ""),sep = ",")))
  inverse_var = inverse_var[,inverse_var_list]
  
  
  ## x124 combination
  x124_combin = x124_combin(dt = train)
  
  ## outlier scores based on knn
  outlier_knn_score <- outlier_knn_score(data = train)
  
  ## outlier scores based on principal components
  outlier_princ_comp_score <- outlier_princ_comp_score(data = train)
  
  
  ## power feature for Conditional, Cross_product, Sum, Diff, Ratio, Sin, Cos, Inverse
  
  pow_condi_feature = power_product(dt = condi_sumry_var, 
                                    varlist = names(condi_sumry_var),
                                    order = 5, path = path)
  pow_cross_feature = power_product(dt = cross_var, varlist = names(cross_var),
                                    order = 5, path = path)
  pow_sum_feature = power_product(dt = sum_var, varlist = names(sum_var),
                                    order = 5, path = path)
  pow_diff_feature = power_product(dt = diff_var, varlist = names(diff_var),
                                    order = 5, path = path)
  pow_ratio_feature = power_product(dt = ratio_var, varlist = names(ratio_var),
                                    order = 5, path = path)
  pow_sin_feature = power_product(dt = sin_var, varlist = names(sin_var),
                                    order = 5, path = path)
  pow_cos_feature = power_product(dt = cos_var, varlist = names(cos_var),
                                    order = 5, path = path)
  pow_inv_feature = power_product(dt = inverse_var, varlist = names(inverse_var),
                                    order = 5, path = path)
  pow_pow_var = data.frame(pow_condi_feature, pow_cross_feature, 
                           pow_sum_feature, pow_diff_feature, 
                           pow_ratio_feature, pow_sin_feature,
                           pow_cos_feature, pow_inv_feature)
  # pow_pow_var_names = names(pow_pow_var)
  # pow_pow_var = data.frame(like_ratio_feature, pow_pow_var)
  
  rm(pow_condi_feature, pow_cross_feature, 
     pow_sum_feature, pow_diff_feature, 
     pow_ratio_feature, pow_sin_feature,
     pow_cos_feature, pow_inv_feature)
  
  # rf_pow_pow_fea_select = good_feature(dt = pow_pow_var, rf_feature_cut = 5)
  # pow_pow_imp_feature = rf_pow_pow_fea_select[!(rf_pow_pow_fea_select %in% names(like_ratio_feature))]
  # write.table(x = as.vector(unlist(pow_pow_imp_feature)), file = paste(path, "/FEATURE_pow_pow_imp.txt", sep = ""),
  #             quote = F, sep = ",", row.names = F, col.names = F)
  # rm(rf_pow_pow_fea_select)
  
  pow_pow_imp_feature = as.vector(unlist(read.table(file = paste(path, "/FEATURE_pow_pow_imp.txt", sep = ""),
                                                  sep = ",")))
  pow_pow_var = pow_pow_var[,pow_pow_imp_feature]
  
  rm(like_ratio_feature)
  
  ## Final Data Combination
  
  final_train_data = data.frame(train, 
                                con_to_dic_var,
                                likelihood_ratio_var,
                                disc_to_bin_ftr_dis_var,
                                condi_sumry_var,
                                pol_5_var,
                                cross_var,
                                sum_var,
                                diff_var,
                                ratio_var,
                                sin_var,
                                cos_var,
                                inverse_var,
                                x124_combin,
                                outlier_knn_score,
                                outlier_princ_comp_score,
                                pow_pow_var)
  
  rm(condi_sumry_var,
     pol_5_var,
     cross_var,
     sum_var,
     diff_var,
     ratio_var,
     sin_var,
     cos_var,
     inverse_var,
     x124_combin,
     outlier_knn_score,
     outlier_princ_comp_score,
     pow_pow_var)
  
  all_var_names = names(final_train_data)
  keepvar = c(names(train[,var_name[!(var_name %in% dis_var)]]), 
              names(con_to_dic_var), 
              names(likelihood_ratio_var), 
              names(disc_to_bin_ftr_dis_var))
  
  # library(Hmisc)
  # corrm = rcorr(as.matrix(final_train_data[,-c(1,2)]))
  # corrm_1 = flattenCorrMatrix(corrm$r, corrm$P)
  # corrm_1 = corrm_1[!(abs(corrm_1$cor)==1),]
  # univar = unique(corrm_1$row)
  
  # # Random Forest Feature Selection
  # final_train_data$fraud = as.factor(final_train_data$fraud)
  # rnd_final_select = randomForest_selection(data = final_train_data, ntree = 4000)
  # 
  # rf_imp_final_var = row.names(rnd_final_select)[rnd_final_select$`0`>=4 & 
  #                                                  rnd_final_select$`1`>=4]
  # 
  # # Boruta Feature Selection
  # boruta_fea_final_select = boruta_selection(data = final_train_data)
  # 
  # rf_feature_not_in_boruta = rf_imp_final_var[!(rf_imp_final_var %in% boruta_fea_final_select)]
  # 
  # ## SMOTE data balance
  # 
  # final_train_data_1 = DMwR::SMOTE(fraud ~., data = final_train_data, perc.over = 200, perc.under = 150)
  # 
  # rnd_final_select_1 = randomForest_selection(data = final_train_data_1, ntree = 4000)
  # 
  # rf_imp_final_var_1 = row.names(rnd_final_select_1)[rnd_final_select_1$`0`>=4 & 
  #                                                            rnd_final_select_1$`1`>=4]
  # 
  # # Boruta Feature Selection
  # boruta_fea_final_select_1 = boruta_selection(data = final_train_data_1)
  # 
  # 
  # final_feature_list = unique(c(boruta_fea_final_select, boruta_fea_final_select_1))
  # final_feature_list_1 = final_feature_list[!(final_feature_list %in% keepvar)]
  # 
  # write.table(x = as.vector(unlist(final_feature_list_1)), file = paste(path, "/FEATURE_final_feature.txt", sep = ""),
  #             quote = F, sep = ",", row.names = F, col.names = F)
  
  final_feature_list_1 = as.vector(unlist(read.table(file = paste(path, "/FEATURE_final_feature.txt", sep = ""), sep = ",")))
  
  final_train_data = final_train_data[, c(keepvar, final_feature_list_1)]
  
  final_var_name = names(final_train_data)
  final_feature_list_2 = gsub("\\.", "_", final_var_name)
  names(final_train_data) = final_feature_list_2
  
  # write.table(x = as.vector(unlist(final_feature_list_2)), file = paste(path, "/FEATURE_final_data_variable_names.txt", sep = ""),
  #             quote = F, sep = ",", row.names = F, col.names = F)
  
  FEATURE_final_data_variable_names = as.vector(unlist(read.table(file = paste(path, "/FEATURE_final_data_variable_names.txt", sep = ""), sep = ",")))
  return(list(data = final_train_data, variable_list = FEATURE_final_data_variable_names))
}
