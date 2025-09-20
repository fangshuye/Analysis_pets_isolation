library(lme4)
library(ggplot2)
# model
alldata <- read.csv('/Users/fangshu/Desktop/research/Analysis_pets_isolation/Compliance_mixed_logistic/Compliance.csv',header = T)
alldata$time <- as.factor(alldata$time)
alldata$Compliance <- as.factor(alldata$Compliance)
alldata$MostAttach <- relevel(alldata$MostAttach,ref = "Dog")
summary(glmer(Compliance ~ MostAttach + time +(1|workerId), data = alldata, family = "binomial"))
# plot
sumdata1 <- alldata %>%
  group_by(time,MostAttach) %>%
  summarise(count = n())

sumdata2 <- alldata %>%
  group_by(time,MostAttach,Compliance) %>%
  summarise(percent = n())

sumdata2 <- merge(sumdata2,sumdata1,by=c("time","MostAttach"))
sumdata2$percent <- sumdata2$percent/sumdata2$count

ggplot(sumdata2, aes(x=MostAttach,y=percent,fill=Compliance))+geom_col(width = 0.5)+facet_grid(time~.)+
  geom_text(aes(label = scales::percent(percent)),position = position_stack(vjust = .5))
