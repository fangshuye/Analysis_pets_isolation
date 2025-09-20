# for the need of modifing the paper

# aim1: Redesign a plot: Figure 3 in a paper

rm(list = ls())
# load packages
library(dplyr)
library(ggplot2)
library(reshape2)

# read the data
setwd('/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/')
savedir <- '/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/Figure3/'

alldata <- c()
policy <- read.csv("cleaned_SIP.csv",header = T)
policy$date <- as.Date(policy$date)
policy <- policy[,-1]

for (d in 1:4) {
  if(d %in% c(1,2)){
    temp_dat <- read.csv(paste0("wave",d,"_cleaned_0805.csv")) 
  }else{
    temp_dat <- read.csv(paste0("wave",d,"_cleaned_20210615.csv")) 
  }
  
  temp_dat <- temp_dat %>%
    filter(PetOwner==1) %>%
    filter(MostAttach %in% c(1,2))
  
  wave <- data.frame(dplyr::select(temp_dat,workerId,MostAttach,
                                   Compliance,Interact,Concern,
                                   RecordedDate,state))
  
  wave$Wave <- rep(d,nrow(wave))
  alldata <- rbind(alldata,wave)
}

unlist_date <- unlist(strsplit(alldata$RecordedDate, " "))
position <- seq(1,nrow(alldata)*2,2)
alldata$date <- unlist_date[position]
alldata$date <- as.Date(alldata$date,format="%m/%d/%y")
alldata <- merge(alldata,policy,by=c("state","date"))
alldata <- alldata %>%
  mutate(RecordedDate=NULL)

cols <- colnames(alldata)[3:9]
alldata[cols] <- lapply(alldata[cols], factor) 
levels(alldata$MostAttach) <- c("Dog","Cat")
levels(alldata$Interact) <- c("No","Yes")
levels(alldata$Compliance) <- c("No","Yes")
levels(alldata$Concern) <- c("Not at all concerned","A little concerned ",
                              "Moderately concerened","Very concerned",
                              "Extremely concerned")

# compliance
sumdata1 <- alldata %>%
  group_by(if_shelter,MostAttach) %>%
  summarise(count = n())

sumdata2 <- alldata %>%
  group_by(if_shelter,MostAttach,Compliance) %>%
  summarise(event = n())

sumdata2 <- merge(sumdata2,sumdata1,by=c("if_shelter","MostAttach"))
sumdata2$label_percent <- paste0(round(sumdata2$event*100/sumdata2$count,1),"%")
sumdata2$label_event <- paste0('(n = ',sumdata2$event,')')
sumdata2$percent <- sumdata2$event/sumdata2$count
sumdata2$label <- paste0(sumdata2$label_percent, '\n', sumdata2$label_event)
levels(sumdata2$if_shelter) <- c("shelter: no","shelter: yes")
# ggplot(sumdata2, aes(x=MostAttach,y=percent,fill=Compliance))+geom_col(width = 0.5)+facet_grid(if_shelter~.)+
#   geom_text(aes(label = label_percent),position = position_stack(vjust = .7), size = 3.6)+
#   geom_text(aes(label = label_event),position = position_stack(vjust = .3), size = 3.6)+
#   labs(x = 'Species', y = 'Percent') +
#   ggtitle("Does your pet/companion animal(s) influence your compliance \n with quarantine recommendations and requirements in your region?")

ggplot(sumdata2, aes(x=MostAttach,y=percent,fill=Compliance))+geom_col(width = 0.5)+facet_grid(if_shelter~.)+
  geom_text(aes(label = label),position = position_stack(vjust = .5), size = 3.6) +
  labs(x = 'Species', y = 'Percent') +
  ggtitle("Does your pet/companion animal(s) influence your compliance \n with quarantine recommendations and requirements in your region?")


ggsave(paste0(savedir, "Compliance",".eps"), 
       width = 6.1,  # 2.63 - 7.5
       height = 6, # <8.75
       dpi = 300,
       units = "in",
       device = "eps")


# interact
sumdata1 <- alldata %>%
  group_by(if_shelter,MostAttach) %>%
  summarise(count = n())

sumdata2 <- alldata %>%
  group_by(if_shelter,MostAttach,Interact) %>%
  summarise(event = n())

sumdata2 <- merge(sumdata2,sumdata1,by=c("if_shelter","MostAttach"))
sumdata2$label_percent <- paste0(round(sumdata2$event*100/sumdata2$count,1),"%")
sumdata2$label_event <- paste0('(n = ',sumdata2$event,')')
sumdata2$percent <- sumdata2$event/sumdata2$count
sumdata2$label <- paste0(sumdata2$label_percent, '\n', sumdata2$label_event)

levels(sumdata2$if_shelter) <- c("shelter: no","shelter: yes")
# ggplot(sumdata2, aes(x=MostAttach,y=percent,fill=Interact))+geom_col(width = 0.5)+facet_grid(if_shelter~.)+
#   geom_text(aes(label = label_percent),position = position_stack(vjust = .6), size = 3.6)+
#   geom_text(aes(label = label_event),position = position_stack(vjust = .3), size = 3.6)+
#   labs(x = 'Species', y = 'Percent') +
#   ggtitle("Has COVID-19 affected your normal activities and \n interactions with your pet/companion animal(s)?")


ggplot(sumdata2, aes(x=MostAttach,y=percent,fill=Interact))+geom_col(width = 0.5)+facet_grid(if_shelter~.)+
  geom_text(aes(label = label),position = position_stack(vjust = .5), size = 3.6)+
  labs(x = 'Species', y = 'Percent') +
  ggtitle("Has COVID-19 affected your normal activities and \n interactions with your pet/companion animal(s)?")

ggsave(paste0(savedir, "Interact",".eps"), 
       width = 6.1,  # 2.63 - 7.5
       height = 6, # <8.75
       dpi = 300,
       units = "in",
       device = "eps")

# concern
sumdata1 <- alldata %>%
  group_by(if_shelter,MostAttach) %>%
  summarise(count = n())

sumdata2 <- alldata %>%
  group_by(if_shelter,MostAttach,Concern) %>%
  summarise(event = n())

sumdata2 <- merge(sumdata2,sumdata1,by=c("if_shelter","MostAttach"))
sumdata2$label_percent <- paste0(round(sumdata2$event*100/sumdata2$count,1),"%")
sumdata2$label_event <- paste0('(n = ',sumdata2$event,')')
sumdata2$percent <- sumdata2$event/sumdata2$count
sumdata2$label <- paste0(sumdata2$label_percent, '\n', sumdata2$label_event)

levels_2 <- c("Not at all concerned","A little concerned ")

sumdata2 <- sumdata2 %>%
  mutate(label_new = ifelse(Concern %in% levels_2, label, label_percent))

levels(sumdata2$if_shelter) <- c("shelter: no","shelter: yes")

ggplot(sumdata2, aes(x=MostAttach,y=percent,fill=Concern))+geom_col(width = 0.5)+facet_grid(if_shelter~.)+
  geom_text(aes(label = label_new),position = position_stack(vjust = .5), size = 3.6)+
  labs(x = 'Species', y = 'Percent') +
  ggtitle("How concerned are you about your ability \n to care for your pets/companion animal(s)?")

ggsave(paste0(savedir, "Concern",".eps"), 
       width = 6.1,  # 2.63 - 7.5
       height = 7, # <8.75
       dpi = 300,
       units = "in",
       device = "eps")
