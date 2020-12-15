train = read.csv(file = "D:/ISU/Data Mining Cup DMC/DMC 2019/Data/train_TL12.csv", header = T, sep = ",")
test = read.csv(file = "D:/ISU/Data Mining Cup DMC/DMC 2019/Data/test_TL12.csv", header = T, sep = ",")
test$fraud = NA
train$dttype = "Train"
test$dttype = "Test"
combin_dt = rbind(train, test)

crosstab(train$trustLevel, train$fraud, prop.r = T, prop.t = T)
crosstab(train$lineItemVoids, train$fraud, prop.r = T, prop.t = T)
crosstab(train$scansWithoutRegistration, train$fraud, prop.r = T, prop.t = T)
crosstab(train$quantityModifications, train$fraud, prop.r = T, prop.t = T)

library(ggplot2)


ggplot(combin_dt, aes(x=totalScanTimeInSeconds, fill=dttype, col = fraud)) + 
  geom_density(alpha=.3)+theme(legend.text=element_text(size=20))

ggplot(combin_dt, aes(x=grandTotal , fill=dttype, col = fraud)) + 
  geom_density(alpha=.3)+theme(legend.text=element_text(size=20))


ggplot(combin_dt %>%
         group_by(dttype, lineItemVoids ) %>%
         summarise (n = n()) %>%
         mutate(Prop = n / sum(n)),
       aes(x = lineItemVoids, fill = dttype)) +
  geom_histogram(aes(weight = Prop), stat = 'count', position = 'dodge')+
  theme(legend.text=element_text(size=20))



ggplot(combin_dt %>%
         group_by(dttype, scansWithoutRegistration) %>%
         summarise (n = n()) %>%
         mutate(Prop = n / sum(n)),
       aes(x = scansWithoutRegistration, fill = dttype)) +
  geom_histogram(aes(weight = Prop), stat = 'count', position = 'dodge')+
  theme(legend.text=element_text(size=20))

ggplot(combin_dt %>%
         group_by(dttype, quantityModifications ) %>%
         summarise (n = n()) %>%
         mutate(Prop = n / sum(n)),
       aes(x = quantityModifications , fill = dttype)) +
  geom_histogram(aes(weight = Prop), stat = 'count', position = 'dodge')+
  theme(legend.text=element_text(size=20))


ggplot(combin_dt, aes(x=scannedLineItemsPerSecond , fill=dttype, col = fraud)) + 
  geom_density(alpha=.3)+theme(legend.text=element_text(size=20))+xlim(0,.15)


ggplot(combin_dt, aes(x=valuePerSecond  , fill=dttype, col = fraud)) + 
  geom_density(alpha=.3)+theme(legend.text=element_text(size=20))+ xlim(0,.5)


ggplot(combin_dt, aes(x=lineItemVoidsPerPosition  , fill=dttype, col = fraud)) + 
  geom_density(alpha=.3)+theme(legend.text=element_text(size=20))


ggplot(combin_dt[combin_dt$dttype=="Train",], 
       aes(x=totalScanTimeInSeconds, y = scannedLineItemsPerSecond, 
           fill=dttype, col = fraud, size = fraud)) + 
  geom_point(alpha = 0.3)+theme(legend.text=element_text(size=20))+ ylim(0,.15)+xlim(4,8)

ggplot(combin_dt[combin_dt$dttype=="Train",], aes(x=totalScanTimeInSeconds*scannedLineItemsPerSecond, y = fraud, fill=dttype, col = fraud)) + 
  geom_point()+theme(legend.text=element_text(size=20))

ggplot(combin_dt[combin_dt$dttype=="Train",], aes(x=totalScanTimeInSeconds, y = scansWithoutRegistration , fill=dttype, col = fraud)) + 
  geom_point()+theme(legend.text=element_text(size=20))
