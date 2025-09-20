############### use start date of SIP and end date of SIP from the same source: google sheet ############
rm(list = ls())
savedir <- '/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/'
library(dplyr)
library(lubridate)
library(gsheet)
state_all_num <- c()
for (d in 1:4) {
  dat <- read.csv(paste0(savedir,"wave",d,"_cleaned_0805.csv"))
  state_all_num <- c(state_all_num,dat$state)
}
state_all_num <- unique(state_all_num)
state_info <- read.csv(paste0(savedir,"state_info.csv"),header = T)
state_info <- state_info[state_info$statenum %in% state_all_num,]
state_all <- state_info$state

dat_end <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1269444822')
dat_end <- dat_end[1:51,c(1,4)]
colnames(dat_end)[2] <- 'End_Date_Shelter'
dat_end <- dat_end%>%
  filter(State %in% state_all) %>%
  mutate(End_Date_Shelter = as.Date(End_Date_Shelter,format="%m/%d/%y"))

dat_start <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1894978869')
dat_start <- dat_start[1:51,c(1,4)]
colnames(dat_start)[2] <- 'Start_Date_Shelter'
dat_start <- dat_start%>%
  filter(State %in% state_all) %>%
  mutate(Start_Date_Shelter = as.Date(Start_Date_Shelter,format="%m/%d/%y"))

final_dat <- merge(dat_start,dat_end,by="State")
write.csv(final_dat,paste0(savedir,"SIP.csv"),row.names = F)


len_s <- length(state_all)
date <- c("2020-04-17","2020-04-20",
          "2020-05-04","2020-05-05","2020-05-06",
          "2020-05-26","2020-05-27","2020-05-28",
          "2020-06-16","2020-06-17")
policy <- data.frame(date=rep(date,each=len_s),state=rep(state_all,10))
# if the state has Shelter in Place order so far
No_Shelter_state <- final_dat[which(is.na(final_dat$Start_Date_Shelter)),"State"]
policy <- policy %>%
  mutate(if_shelter=ifelse(state %in% No_Shelter_state,"no","yes"))

for(r in 1:nrow(policy)){
  
  if(policy[r,3]=="yes"){
    state_r <- policy$state[r]
    date_r <- policy$date[r]
    
    #################### whether there is shelter in place the past 14 days##########
    start_sip <- final_dat[final_dat$State==state_r,2]
    end_sip <- final_dat[final_dat$State==state_r,3]
    
    if(is.na(end_sip)){end_sip=today()}
    
    date_sip <- seq(as.Date(start_sip), as.Date(end_sip), "days")
    date_r_14 <- seq(as.Date(date_r)-14,as.Date(date_r),"days")
    inter_date_re <- intersect(date_sip,date_r_14)
    if(length(inter_date_re)==0){
      policy[r,3] <- "no"
    }else{
      policy[r,3] <- "yes"
    }
  }
  
}

policy <- merge(policy,state_info,by="state")
colnames(policy)[1] <- "statename"
colnames(policy)[4] <- "state"
write.csv(policy,paste0(savedir,"cleaned_SIP.csv"),row.names = F)


###########################################################
########## data from Dr. Lily Wang and the google sheet####
###########################################################
setwd("/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/rawdata/")
rm(list = ls())
library(dplyr)
library(lubridate)
library(gsheet)
savedir <- '/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/'
dat_new <- read.delim(file = 'Control_Reopen.tsv', sep = '\t', header = TRUE)
state_all_num <- c()
for (d in 1:4) {
  dat <- read.csv(paste0(savedir,"wave",d,"_cleaned_0805.csv"))
  state_all_num <- c(state_all_num,dat$state)
}
state_all_num <- unique(state_all_num)
state_info <- read.csv(paste0(savedir,"state_info.csv"),header = T)
state_info <- state_info[state_info$statenum %in% state_all_num,]
state_all <- state_info$state
dat_new[dat_new$State=="DistrictofColumbia","State"] <- "District of Columbia"
dat_new[dat_new$State=="NewHampshire","State"] <- "New Hampshire"
dat_new[dat_new$State=="NewJersey","State"] <- "New Jersey"
dat_new[dat_new$State=="NewMexico","State"] <- "New Mexico"
dat_new[dat_new$State=="NewYork","State"] <- "New York"
dat_new[dat_new$State=="NorthCarolina","State"] <- "North Carolina"
dat_new[dat_new$State=="NorthDakota","State"] <- "North Dakota"
#dat_new[dat_new$State=="PuertoRico","State"] <- "Puerto Rico"
dat_new[dat_new$State=="RhodeIsland","State"] <- "Rhode Island"
dat_new[dat_new$State=="SouthCarolina","State"] <- "South Carolina"
dat_new[dat_new$State=="SouthDakota","State"] <- "South Dakota"
dat_new[dat_new$State=="WestVirginia","State"] <- "West Virginia"

dat_new <- dat_new %>%
  filter(State %in% state_all)
dat_new <- dat_new[,c(1,7)]
State <- dat_new$State
len_s <- length(State)
date <- c("2020-04-17","2020-04-20",
          "2020-05-04","2020-05-05","2020-05-06",
          "2020-05-26","2020-05-27","2020-05-28",
          "2020-06-16","2020-06-17")
policy <- data.frame(date=rep(date,each=len_s),state=rep(State,10))
# if the state has Shelter in Place order so far
No_Shelter_state <- dat_new[which(is.na(dat_new$Shelter.in.Place)),"State"]
policy <- policy %>%
  mutate(if_shelter=ifelse(state %in% No_Shelter_state,"no","yes"))


dat_g <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1269444822')
dat_g <- dat_g[1:51,c(1,4)]
colnames(dat_g)[2] <- 'End_Date_Shelter'
dat_g <- dat_g%>%
  filter(State %in% state_all) %>%
  mutate(End_Date_Shelter = as.Date(End_Date_Shelter,format="%m/%d/%y"))

final_dat <- merge(dat_new,dat_g,by="State")
colnames(final_dat)[2] <- 'Start_Date_Shelter'
write.csv(final_dat,paste0(savedir,"SIP.csv"),row.names = F)

for(r in 1:nrow(policy)){
  
  if(policy[r,3]=="yes"){
    state_r <- policy$state[r]
    date_r <- policy$date[r]
    
    #################### whether there is shelter in place the past 14 days##########
    start_sip <- final_dat[final_dat$State==state_r,2]
    end_sip <- final_dat[final_dat$State==state_r,3]
    
    
    re_open <- dat_new[dat_new$State==state_r,4]
    re_close <- dat_new[dat_new$State==state_r,3]
    
    if(is.na(end_sip)){end_sip=today()}
    
    date_sip <- seq(as.Date(start_sip), as.Date(end_sip), "days")
    date_r_14 <- seq(as.Date(date_r)-14,as.Date(date_r),"days")
    inter_date_re <- intersect(date_sip,date_r_14)
    if(length(inter_date_re)==0){
      policy[r,3] <- "no"
    }else{
      policy[r,3] <- "yes"
    }
  }
  
}

write.csv(policy,paste0(savedir,"cleaned_SIP.csv"),row.names = F)

########################################
############ Dr. Lily Wang's data#######
#######################################
setwd("/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/rawdata/")
# see how many people change in the past 14 days
rm(list = ls())
library(dplyr)
library(lubridate)
savedir <- '/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/'
dat_new <- read.delim(file = 'Control_Reopen.tsv', sep = '\t', header = TRUE)

state_all_num <- c()
for (d in 1:4) {
  dat <- read.csv(paste0(savedir,"wave",d,"_cleaned_0805.csv"))
  state_all_num <- c(state_all_num,dat$state)
}
state_all_num <- unique(state_all_num)
state_info <- read.csv(paste0(savedir,"state_info.csv"),header = T)
state_info <- state_info[state_info$statenum %in% state_all_num,]
state_all <- state_info$state
dat_new[dat_new$State=="DistrictofColumbia","State"] <- "District of Columbia"
dat_new[dat_new$State=="NewHampshire","State"] <- "New Hampshire"
dat_new[dat_new$State=="NewJersey","State"] <- "New Jersey"
dat_new[dat_new$State=="NewMexico","State"] <- "New Mexico"
dat_new[dat_new$State=="NewYork","State"] <- "New York"
dat_new[dat_new$State=="NorthCarolina","State"] <- "North Carolina"
dat_new[dat_new$State=="NorthDakota","State"] <- "North Dakota"
#dat_new[dat_new$State=="PuertoRico","State"] <- "Puerto Rico"
dat_new[dat_new$State=="RhodeIsland","State"] <- "Rhode Island"
dat_new[dat_new$State=="SouthCarolina","State"] <- "South Carolina"
dat_new[dat_new$State=="SouthDakota","State"] <- "South Dakota"
dat_new[dat_new$State=="WestVirginia","State"] <- "West Virginia"

dat_new <- dat_new %>%
  filter(State %in% state_all)

State <- dat_new$State
len_s <- length(State)
date <- c("2020-04-17","2020-04-20",
          "2020-05-04","2020-05-05","2020-05-06",
          "2020-05-26","2020-05-27","2020-05-28",
          "2020-06-16","2020-06-17")
policy <- data.frame(date=rep(date,each=len_s),state=rep(State,10))
# if the state has Shelter in Place order so far
No_Shelter_state <- dat_new[which(is.na(dat_new$Shelter.in.Place)),"State"]
policy <- policy %>%
  mutate(if_shelter=ifelse(state %in% No_Shelter_state,"no","yes"))

for(r in 1:nrow(policy)){
  state_r <- policy$state[r]
  date_r <- policy$date[r]
  
  ####################has the restaurant closed in the past 14 days##########
  #Closed.restaurants.except.take.out
  #Reopen.restaurants
  re_open <- dat_new[dat_new$State==state_r,4]
  re_close <- dat_new[dat_new$State==state_r,3]
  
  if(is.na(re_open)){re_open=today()}
  
  date_re_close <- seq(as.Date(re_close), as.Date(re_open), "days")
  date_r_14 <- seq(as.Date(date_r)-14,as.Date(date_r),"days")
  inter_date_re <- intersect(date_re_close,date_r_14)
  if(length(inter_date_re)==0){
    policy[r,4] <- "no"
  }else{
    policy[r,4] <- "yes"
  }
  
  ######in the past 14 days, has businesses reopen?#####
  #Began.to.reopen.businesses
  bus_open <- dat_new[dat_new$State==state_r,2]
  date_bus_open <- seq(as.Date(bus_open), as.Date(today()), "days")
  inter_date_bus <- intersect(date_bus_open,date_r_14)
  if(length(inter_date_bus)==0){
    policy[r,5] <- "no"
  }else{
    policy[r,5] <- "yes"
  }
  
  
}
colnames(policy)[4:5] <- c("restaurant_closed_14","businesses_reopen_14")
policy <- merge(policy,state_info,by="state")
colnames(policy)[1] <- "statename"
colnames(policy)[6] <- "state"

write.csv(policy,paste0(savedir,"Control_Reopen.csv"),row.names = F)


################ new confirmed case##################
setwd("/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/rawdata/JHU/")
rm(list = ls())
library(dplyr)
savedir <- '/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/'
dat_new <- read.csv("Daily_new_positive.csv",header = T)
state_info <- read.csv(paste0(savedir,"state_info.csv"),header = T)
state_all <- state_info$state[1:52]

dat_new[dat_new$State=="DistrictofColumbia","State"] <- "District of Columbia"
dat_new[dat_new$State=="NewHampshire","State"] <- "New Hampshire"
dat_new[dat_new$State=="NewJersey","State"] <- "New Jersey"
dat_new[dat_new$State=="NewMexico","State"] <- "New Mexico"
dat_new[dat_new$State=="NewYork","State"] <- "New York"
dat_new[dat_new$State=="NorthCarolina","State"] <- "North Carolina"
dat_new[dat_new$State=="NorthDakota","State"] <- "North Dakota"
dat_new[dat_new$State=="PuertoRico","State"] <- "Puerto Rico"
dat_new[dat_new$State=="RhodeIsland","State"] <- "Rhode Island"
dat_new[dat_new$State=="SouthCarolina","State"] <- "South Carolina"
dat_new[dat_new$State=="SouthDakota","State"] <- "South Dakota"
dat_new[dat_new$State=="WestVirginia","State"] <- "West Virginia"


dat_new <- dat_new %>%
  filter(State %in% state_all)
date <- c("X2020.04.17","X2020.04.20",
          "X2020.05.04","X2020.05.05","X2020.05.06",
          "X2020.05.26","X2020.05.27","X2020.05.28",
          "X2020.06.16","X2020.06.17")
State <- dat_new$State
len_s <- length(State)
high_last_14 <- data.frame(date=rep(date,each=len_s),state=rep(State,10))
high_last_14$highest_new_case_num=rep(0,nrow(high_last_14))

date_position <- c()
for(d in 1:10){
  date_position <- c(date_position,which(colnames(dat_new)==date[d]))
}
date_position_past <- date_position-14

for(d in 1:10){
    dat <- dat_new[,c(date_position_past[d]:date_position[d])]
    start_row <- len_s*(d-1)+1
    high_last_14[start_row:(len_s*d),3] <- apply(dat, 1, FUN=max)
}
date <- c("2020-04-17","2020-04-20",
          "2020-05-04","2020-05-05","2020-05-06",
          "2020-05-26","2020-05-27","2020-05-28",
          "2020-06-16","2020-06-17")
high_last_14$date <- rep(date,each=len_s)
# savedir <- '/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/'
# write.csv(high_last_14,paste0(savedir,"high_num_last_14.csv"),row.names = F)
high_last_14_1 <- high_last_14

# dat_rate
dat_new <- read.csv("Prevalence_rate.csv",header = T)
dat_new[dat_new$State=="DistrictofColumbia","State"] <- "District of Columbia"
dat_new[dat_new$State=="NewHampshire","State"] <- "New Hampshire"
dat_new[dat_new$State=="NewJersey","State"] <- "New Jersey"
dat_new[dat_new$State=="NewMexico","State"] <- "New Mexico"
dat_new[dat_new$State=="NewYork","State"] <- "New York"
dat_new[dat_new$State=="NorthCarolina","State"] <- "North Carolina"
dat_new[dat_new$State=="NorthDakota","State"] <- "North Dakota"
dat_new[dat_new$State=="PuertoRico","State"] <- "Puerto Rico"
dat_new[dat_new$State=="RhodeIsland","State"] <- "Rhode Island"
dat_new[dat_new$State=="SouthCarolina","State"] <- "South Carolina"
dat_new[dat_new$State=="SouthDakota","State"] <- "South Dakota"
dat_new[dat_new$State=="WestVirginia","State"] <- "West Virginia"


dat_new <- dat_new %>%
  filter(State %in% state_all)

date <- c("X2020.04.17","X2020.04.20",
          "X2020.05.04","X2020.05.05","X2020.05.06",
          "X2020.05.26","X2020.05.27","X2020.05.28",
          "X2020.06.16","X2020.06.17")
State <- dat_new$State
len_s <- length(State)
high_last_14 <- data.frame(date=rep(date,each=len_s),state=rep(State,10))
high_last_14$highest_new_case_rate=rep(0,nrow(high_last_14))

date_position <- c()
for(d in 1:10){
  date_position <- c(date_position,which(colnames(dat_new)==date[d]))
}
date_position_past <- date_position-14

for(d in 1:10){
  dat <- dat_new[,c(date_position_past[d]:date_position[d])]
  start_row <- len_s*(d-1)+1
  high_last_14[start_row:(len_s*d),3] <- apply(dat, 1, FUN=max)
}
date <- c("2020-04-17","2020-04-20",
          "2020-05-04","2020-05-05","2020-05-06",
          "2020-05-26","2020-05-27","2020-05-28",
          "2020-06-16","2020-06-17")
high_last_14$date <- rep(date,each=len_s)

high_last_14 <- merge(high_last_14_1,high_last_14,by=c("state","date"))
high_last_14 <- high_last_14[order(high_last_14$date,high_last_14$state),]
write.csv(high_last_14,paste0(savedir,"high_last_14_JHU.csv"),row.names = F)




