rm(list = ls())
# load packages
library(dplyr)
library(ggplot2)
library(reshape2)

# read the data
setwd('/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/')
savedir <- '/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/boxplot/'

policy <- read.csv("cleaned_SIP.csv",header = T)
policy$date <- as.Date(policy$date)
policy <- policy[order(policy$statename,policy$date),]
policy <- policy[,-1]

alldata <- c()

for (d in 1:4) {
  if(d %in% c(1,2)){
    temp_dat <- read.csv(paste0("wave",d,"_cleaned_0805.csv")) 
  }else{
    temp_dat <- read.csv(paste0("wave",d,"_cleaned_20210615.csv")) 
  }
  
  wave <- data.frame(dplyr::select(temp_dat,workerId,PetOwner,
                                   Gad1,Gad2,Gad3,Gad4,Gad5,Gad6,Gad7,
                                   Lonely1,Lonely2,Lonely3,
                                   Risk1,Risk2,Risk3,
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
  mutate(GAD_Sum=Gad1+Gad2+Gad3+Gad4+Gad5+Gad6+Gad7) %>%
  mutate(Loneliness=Lonely1+Lonely2+Lonely3) %>%
  mutate(Risk=Risk1+Risk2+Risk3) %>%
  mutate(RecordedDate=NULL)

# select the columns we need
alldata <- data.frame(dplyr::select(alldata,
                                    workerId,
                                    PetOwner,if_shelter,
                                    GAD_Sum,Loneliness,Risk
))


cols <- c('workerId', 'PetOwner', 'if_shelter')
alldata[cols] <- lapply(alldata[cols], factor) 
levels(alldata$PetOwner) <- c("No","Yes")
levels(alldata$if_shelter) <- c("shelter: no","shelter: yes")

############################# GAD ####################

ggplot(alldata, aes(x=PetOwner, y = GAD_Sum)) + geom_boxplot() + 
  labs(y = 'Cumulative GAD Score', x = 'PetOwner') +
  facet_grid(if_shelter~.)


# Inch = pixel/(DPI)
ggsave(paste0(savedir, "GAD_boxplot",".eps"), 
       width = 4,  # 2.63 - 7.5
       height = 4, # <8.75
       dpi = 300,
       units = "in",
       device = "eps")

############################# Loneliness ####################

ggplot(alldata, aes(x=PetOwner, y = Loneliness)) + geom_boxplot() + 
  labs(y = 'Cumulative Loneliness Score', x = 'PetOwner') +
  facet_grid(if_shelter~.)


# Inch = pixel/(DPI)
ggsave(paste0(savedir, "Loneliness_boxplot",".eps"), 
       width = 4,  # 2.63 - 7.5
       height = 4, # <8.75
       dpi = 300,
       units = "in",
       device = "eps")


############################# Risk ####################

ggplot(alldata, aes(x=PetOwner, y = Risk)) + geom_boxplot() + 
  labs(y = 'Cumulative Risk Perception Score', x = 'PetOwner') +
  facet_grid(if_shelter~.)


# Inch = pixel/(DPI)
ggsave(paste0(savedir, "Risk_boxplot",".eps"), 
       width = 4,  # 2.63 - 7.5
       height = 4, # <8.75
       dpi = 300,
       units = "in",
       device = "eps")

######################### new section #################
rm(list = ls())
# load packages
library(dplyr)
library(ggplot2)
library(reshape2)

# read the data
setwd('/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/')
savedir <- '/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/boxplot/'

policy <- read.csv("cleaned_SIP.csv",header = T)
policy$date <- as.Date(policy$date)
policy <- policy[order(policy$statename,policy$date),]
policy <- policy[,-1]

alldata <- c()

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
                                   CCAS1,CCAS2,CCAS3,CCAS4,CCAS5,CCAS6,CCAS7,
                                   CCAS8,CCAS9,CCAS10,CCAS11,CCAS12,CCAS13,
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
  mutate(RecordedDate=NULL) %>%
  mutate(CCAS=CCAS1+CCAS2+CCAS3+CCAS4+CCAS5+CCAS6+CCAS7+CCAS8+CCAS9+CCAS10+CCAS11+CCAS12+CCAS13)

# select the columns we need
alldata <- data.frame(dplyr::select(alldata,
                                    workerId,
                                    MostAttach,if_shelter,
                                    CCAS))


cols <- c('workerId', 'MostAttach', 'if_shelter')
alldata[cols] <- lapply(alldata[cols], factor) 
levels(alldata$MostAttach) <- c("Dog","Cat")
levels(alldata$if_shelter) <- c("shelter: no","shelter: yes")

############################# CCAS ####################

ggplot(alldata, aes(x=MostAttach, y = CCAS)) + geom_boxplot() + 
  labs(y = 'Cumulative CCAS', x = 'Species') +
  facet_grid(if_shelter~.)


# Inch = pixel/(DPI)
ggsave(paste0(savedir, "CCAS_boxplot",".eps"), 
       width = 4,  # 2.63 - 7.5
       height = 4, # <8.75
       dpi = 300,
       units = "in",
       device = "eps")


