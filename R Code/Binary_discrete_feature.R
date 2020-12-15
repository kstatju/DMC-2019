#############################################################
## DMC 2019
##
## Create Binary feature (1, -1) for each variable
#############################################################


hist(test[[varlist[6]]])

varlist = c("totalScanTimeInSeconds",
            "grandTotal",
            "lineItemVoids",
            "scannedLineItemsPerSecond",
            "valuePerSecond",
            "lineItemVoidsPerPosition")
a1 = normalize_data(train = train, test = test, varlist = varlist, path = path, normalize_min = 5)
a2 = normalize_data_test(test = test, varlist = varlist, path = path, normalize_min = 5)
a3 = standardize_data(train = train, path = path, varlist = varlist)
a4 = standardize_data_test(test = test, path = path, varlist = varlist)

a = crossprod_var(dt = dt, varlist = varlist, maxorder = 4, path = path)
b = power_product(dt = dt, varlist = varlist, maxorder = 4, path = path)



path = "D:/ISU/Data Mining Cup DMC/DMC 2019/R code"
varlist = c("trustLevel",
            "lineItemVoids",
            "scansWithoutRegistration",
            "quantityModifications")

aa = discrete_to_binary(dt = dt, varlist = varlist, path = path)

lik_r = likelihood_ratio(dt_c = dt_c, varlist_disc = varlist, path = path)

lik_t = likelihood_ratio_test(dt_c = test, varlist_disc = varlist, path = path)



varlist = c("totalScanTimeInSeconds",
            "grandTotal",
            "lineItemVoids",
            "scannedLineItemsPerSecond",
            "valuePerSecond",
            "lineItemVoidsPerPosition")

bb = continuous_to_discreat(dt = dt, test_dt = test, varlist = varlist, ngroup = 20, path = path)



varlist = c("totalScanTimeInSeconds",
            "grandTotal",
            "lineItemVoids",
            "scannedLineItemsPerSecond",
            "valuePerSecond",
            "lineItemVoidsPerPosition")





dt_c = data.frame(dt, bb)

varlist_cont = c("totalScanTimeInSeconds",
                 "grandTotal",
                 "lineItemVoids",
                 "scannedLineItemsPerSecond",
                 "valuePerSecond",
                 "lineItemVoidsPerPosition")

varlist_disc = c("totalScanTimeInSeconds_2_discrete",
                 "grandTotal_2_discrete",
                 "lineItemVoids_2_discrete",
                 "scannedLineItemsPerSecond_2_discrete",
                 "valuePerSecond_2_discrete",
                 "lineItemVoidsPerPosition_2_discrete")

cc = condi_summary_var(dt_c = dt_c, varlist_cont = varlist_cont, 
                       varlist_disc = varlist_disc, path = path)
dd = continuous_to_discreat_test(dt = test, varlist = varlist_cont, path = path)
dd = data.frame(test, dd)

dd = condi_summary_var_test(dt_c = dd, varlist_disc = varlist_disc, path = path)

dt = train
dt = data.frame(dt, bb)
dt1 = data.frame(test, b)
# 
# write.table(t(table(dt$totalScanTimeInSeconds_2_discrete, dt$fraud)),"aa.txt",col.names = F,row.names = F)
# write.table(t(table(dt1$totalScanTimeInSeconds_2_discrete)),"aa.txt",col.names = F,row.names = F,append = T)
# write.table(t(table(dt$totalScanTimeInSeconds_2_discrete)),"aa.txt",col.names = F,row.names = F,append = T)
# write.table(t(table(dt$grandTotal_2_discrete, dt$fraud)),"aa.txt",col.names = F,row.names = F,append = T)
# write.table(t(table(dt1$grandTotal_2_discrete)),"aa.txt",col.names = F,row.names = F,append = T)
# write.table(t(table(dt$grandTotal_2_discrete)),"aa.txt",col.names = F,row.names = F,append = T)
# write.table(t(table(dt$scannedLineItemsPerSecond_2_discrete, dt$fraud)),"aa.txt",col.names = F,row.names = F,append = T)
# write.table(t(table(dt1$scannedLineItemsPerSecond_2_discrete)),"aa.txt",col.names = F,row.names = F,append = T)
# write.table(t(table(dt$scannedLineItemsPerSecond_2_discrete)),"aa.txt",col.names = F,row.names = F,append = T)
# write.table(t(table(dt$valuePerSecond_2_discrete, dt$fraud)),"aa.txt",col.names = F,row.names = F,append = T)
# write.table(t(table(dt1$valuePerSecond_2_discrete)),"aa.txt",col.names = F,row.names = F,append = T)
# write.table(t(table(dt$valuePerSecond_2_discrete)),"aa.txt",col.names = F,row.names = F,append = T)
# write.table(t(table(dt$lineItemVoidsPerPosition_2_discrete, dt$fraud)),"aa.txt",col.names = F,row.names = F,append = T)
# write.table(t(table(dt1$lineItemVoidsPerPosition_2_discrete)),"aa.txt",col.names = F,row.names = F,append = T)
# write.table(t(table(dt$lineItemVoidsPerPosition_2_discrete)),"aa.txt",col.names = F,row.names = F,append = T)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
