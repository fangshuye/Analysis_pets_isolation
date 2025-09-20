rm(list = ls())
# load packages
library(dplyr)
library(ggplot2)
library(reshape2)

# read the data
setwd('/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/')
savedir <- '/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/lineplot/'

alldata <- c()

for (d in 1:4) {
  if(d %in% c(1,2)){
    temp_dat <- read.csv(paste0("wave",d,"_cleaned_0805.csv")) 
  }else{
    temp_dat <- read.csv(paste0("wave",d,"_cleaned_20210615.csv")) 
  }
  
  wave <- data.frame(dplyr::select(temp_dat,workerId,PetOwner,
                                   Kessler1,Kessler2,Kessler3,Kessler4,Kessler5,
                                   Kessler6,Kessler7,Kessler8,Kessler9,Kessler10,
                                   Gad1,Gad2,Gad3,Gad4,Gad5,Gad6,Gad7,
                                   Lonely1,Lonely2,Lonely3,
                                   Risk1,Risk2,Risk3))
  
  wave$Wave <- rep(d,nrow(wave))
  alldata <- rbind(alldata,wave)
}

alldata <- alldata %>%
  mutate(Kessler_Sum=Kessler1+Kessler2+Kessler3+Kessler4+Kessler5+Kessler6+Kessler7+Kessler8+Kessler9+Kessler10) %>%
  mutate(GAD_Sum=Gad1+Gad2+Gad3+Gad4+Gad5+Gad6+Gad7) %>%
  mutate(Loneliness=Lonely1+Lonely2+Lonely3) %>%
  mutate(Risk=Risk1+Risk2+Risk3)
alldata$PetOwner <- as.factor(alldata$PetOwner)
levels(alldata$PetOwner) <- c("Non-pet owners","Pet owners")
############################# Kessler ####################
ggplot(alldata, aes(x=Kessler_Sum, linetype = PetOwner)) + geom_density() + xlab('Sum of Kessler 1-10') +
  theme(legend.position = "top")

#ggplot(alldata, aes(x=Risk, fill = PetOwner)) + geom_histogram(binwidth = 1, alpha = 0.3, position = 'identity') + xlab('Sum of Risk 1-3')


# Inch = pixel/(DPI)
ggsave(paste0(savedir, "Kessler",".eps"), 
       width = 4,  # 2.63 - 7.5
       height = 4, # <8.75
       dpi = 300,
       units = "in",
       device = "eps")


############################# GAD ####################

ggplot(alldata, aes(x=GAD_Sum, linetype = PetOwner)) + geom_density() + xlab('Sum of GAD 1-7') +
  theme(legend.position = "top")

#ggplot(alldata, aes(x=Risk, fill = PetOwner)) + geom_histogram(binwidth = 1, alpha = 0.3, position = 'identity') + xlab('Sum of Risk 1-3')


# Inch = pixel/(DPI)
ggsave(paste0(savedir, "GAD",".eps"), 
       width = 4,  # 2.63 - 7.5
       height = 4, # <8.75
       dpi = 300,
       units = "in",
       device = "eps")

############################# Loneliness ####################
ggplot(alldata, aes(x=Loneliness, linetype = PetOwner)) + geom_density() + xlab('Sum of Lonely 1-3') +
  theme(legend.position = "top")

#ggplot(alldata, aes(x=Risk, fill = PetOwner)) + geom_histogram(binwidth = 1, alpha = 0.3, position = 'identity') + xlab('Sum of Risk 1-3')


# Inch = pixel/(DPI)
ggsave(paste0(savedir, "Lonely",".eps"), 
       width = 4,  # 2.63 - 7.5
       height = 4, # <8.75
       dpi = 300,
       units = "in",
       device = "eps")


############################# Risk ####################
ggplot(alldata, aes(x=Risk, linetype = PetOwner)) + geom_density() + xlab('Sum of Risk 1-3') +
  theme(legend.position = "top")

#ggplot(alldata, aes(x=Risk, fill = PetOwner)) + geom_histogram(binwidth = 1, alpha = 0.3, position = 'identity') + xlab('Sum of Risk 1-3')


# Inch = pixel/(DPI)
ggsave(paste0(savedir, "Risk",".eps"), 
       width = 4,  # 2.63 - 7.5
       height = 4, # <8.75
       dpi = 300,
       units = "in",
       device = "eps")





