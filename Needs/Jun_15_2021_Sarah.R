rm(list = ls())
# load packages
library(dplyr)
library(ggplot2)
library(reshape2)

# read the data
setwd('/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/')

alldata <- c()

for (d in 1:4) {
  if(d %in% c(1,2)){
    temp_dat <- read.csv(paste0("wave",d,"_cleaned_0805.csv")) 
  }else{
    temp_dat <- read.csv(paste0("wave",d,"_cleaned_20210615.csv")) 
  }
  
  
  wave <- temp_dat %>%
    select(starts_with(c("PetTypes", "MostAttach", "PetOwner",
                       "workerId","age","gender","ethnic")))
  
  print(nrow(wave))
  
  wave$Wave <- rep(d,nrow(wave))
  alldata <- rbind(alldata,wave)
}

# 1. age
alldata %>% group_by(Wave) %>% summarise(meanage = mean(age), sdage = sd(age))

# 2. gender
tab = table(alldata$gender, alldata$Wave)
round(prop.table(tab, 2), 3) *100
# check
apply(tab, 2, sum)

# 3. ethnic
tab = table(alldata$ethnic, alldata$Wave)

tab[,1] = round(tab[,1]/718, 3)*100
tab[,2] = round(tab[,2]/565, 3)*100
tab[,3] = round(tab[,3]/472, 3)*100
tab[,4] = round(tab[,4]/428, 3)*100
tab
#round(prop.table(tab, 2), 3) *100
# check
#apply(tab, 2, sum)

# 4. PetTypes
petdat <- alldata %>% filter(PetOwner == 1)
cols = paste0('PetTypes_', c(1:8)) 
cols = c(cols, 'workerId','Wave')
subdat <- petdat[,cols]
melt_subdat = melt(subdat, id = c('workerId','Wave'))
# remove 0
melt_subdat <- melt_subdat %>% filter(value !=0)

tab = table(melt_subdat$variable, melt_subdat$Wave)
# sum fish anf other
subtab = tab[7:8,]
tab[7,] = apply(subtab, 2, sum)
tab = tab[-8,]
tab

tab[,1] = round(tab[,1]/718, 3)*100
tab[,2] = round(tab[,2]/565, 3)*100
tab[,3] = round(tab[,3]/472, 3)*100
tab[,4] = round(tab[,4]/428, 3)*100

# 5. MostAttach
tab = table(petdat$MostAttach, petdat$Wave)
tab

tab[,1] = round(tab[,1]/718, 3)*100
tab[,2] = round(tab[,2]/565, 3)*100
tab[,3] = round(tab[,3]/472, 3)*100
tab[,4] = round(tab[,4]/428, 3)*100


tab

# new puppy since lockdown
newpuppy = petdat %>% filter(MostAttachTime ==1) %>% filter(MostAttach ==1)
table(newpuppy$Wave)

############################# heat plot ################
rm(list = ls())
# load packages
library(dplyr)
library(ggplot2)
library(reshape2)
library(usmap)
# read the data
setwd('/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/')
state_info <- read.csv("state_info.csv")
state_info <- state_info %>% filter(statenum != 53)
savedir <- '/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/heatmap/'

for (d in 1:4) {
  if(d %in% c(1,2)){
    temp_dat <- read.csv(paste0("wave",d,"_cleaned_0805.csv")) 
  }else{
    temp_dat <- read.csv(paste0("wave",d,"_cleaned_20210615.csv")) 
  }
  
  
  wave <- temp_dat %>%
    select(starts_with(c("workerId","state")))
  
  summary_tab = wave %>% group_by(state) %>% summarise(num_respondent = n())
  names(summary_tab)[1] <- 'statenum'
  summary_tab = merge(summary_tab, state_info, by = 'statenum', all.y = T)
  
  summary_tab = summary_tab %>% mutate(statenum = NULL)
  #summary_tab[is.na(summary_tab)] <- 0
  #plot_dat = summary_tab
  #colnames(plot_dat)[2] = 'full'
  
  plot_usmap(data = summary_tab, values = "num_respondent", color = "white") +
    scale_fill_continuous(name = "Number of respondents",limits=c(0,75), breaks=seq(10,75,by=20),
                          low = "#BCD2E8", high = "#1E3F66") + 
    theme(legend.position = "right")
  
  # Inch = pixel/(DPI)
  ggsave(paste0(savedir, "heatmap_wave",d,".eps"), 
         width = 5,  # 2.63 - 7.5
         height = 3, # <8.75
         dpi = 300,
         units = "in",
         device = "eps")
  
}






