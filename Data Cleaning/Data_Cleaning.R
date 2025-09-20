setwd('/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/rawdata')
library(dplyr)
library(sf)
library(spData)
rm(list = ls())
savedir <- "/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/"
rawdata1 <- read.csv("2020-4-17\ COVID_Raw_Data.csv")
rawdata1 <- rawdata1[-3,] # remove the test row
rawdata1 <- rawdata1[-which(rawdata1$age=="110"),] # delete the outlier of age
rawdata2 <- read.csv("2020-5-6\ COVID_Raw_Data.csv")
rawdata3 <- read.csv('2020-5-26\ COVID-Raw_Data.csv')
rawdata4 <- read.csv('2020-6-16\ COVID-Raw_Data.csv')
rawdata4[which(rawdata4$workerId=="AEF74ZYJTTEIA"),"gender"] <- 1
rawdatalist <- list(rawdata1,rawdata2,rawdata3,rawdata4)

# define a function
lonlat_to_state <- function(pointsDF,
                            states = spData::us_states,
                            name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}
statename <- state.name
statename <- c(statename,"District of Columbia","Puerto Rico")
statename <- statename[order(statename)]
statedata <- data.frame(state=statename,statenum=c(1:52))
statedata[53,] <- c("I do not reside in the United States",53)

n <- length(rawdatalist)
for(d in 1:n){
  dat <- rawdatalist[[d]]
  dat <- dat[-c(1,2),] # remove the first+second row for easier data manipulation
  dat <- dat %>%
    mutate(workerId=as.character(as.factor(workerId))) %>%
    mutate(age=as.character(as.factor(age))) %>%
    mutate(gender=as.character(as.factor(gender)))
  
  dat <- dat %>%
    mutate(age=as.numeric(age)) 
  ##########################Data Checking##############################
  # 1) remove anyone that did not answer Q66 correctly 
  if(d<3){
    dat <- dat %>% 
      filter(Q66 == "5")
  }else{
    dat <- dat %>% 
      filter(Trap == "5")
    
    #add column 'state' for wave3 and wave4
    testPoints <- data.frame(x = as.numeric(dat$LocationLongitude),
                             y = as.numeric(dat$LocationLatitude))
    dat$state <- lonlat_to_state(testPoints)
    
    dat[which(dat$LocationLatitude=="21.30670166" & dat$LocationLongitude=="-157.8558044"),"state"] <- "Hawaii"
    if(d==3){
      dat[which(dat$IPAddress=="84.17.58.33"),"state"] <- "I do not reside in the United States"
    }else{
      dat[which(dat$LocationLatitude=="40.62080383" & dat$LocationLongitude=="-74.04260254"),"state"] <- "New York"
      dat[which(dat$LocationLatitude=="48.17889404" & dat$LocationLongitude=="-122.5180969"),"state"] <- "Washington"
    }
    
    dat <- merge(dat,statedata,by="state")
    dat <- dat[,-1]
    colnames(dat)[which(names(dat) == "statenum")] <- "state"
    
  }
  
  
  # 2) remove anyone who doesn’t have the same demographic information for “AGE” and “Gender” 
  ## 2.1) remove anyone who has multiple obs
  
  countid <- dat %>%
    group_by(workerId) %>%
    summarise(freq = n())
  
  idset <- countid$workerId[countid$freq==1]
  dat <- dat %>%
    filter(workerId %in% idset)
  
  rawdatalist[[d]] <- dat
}

## 2.2) check the consistent of demographic information among different waves for the same person.
subdat1 <- data.frame(select(rawdatalist[[1]],age,gender,MostAttach,workerId))
subdat2 <- data.frame(select(rawdatalist[[2]],age,gender,MostAttach,workerId))
subdat3 <- data.frame(select(rawdatalist[[3]],age,gender,MostAttach,workerId))
subdat4 <- data.frame(select(rawdatalist[[4]],age,gender,MostAttach,workerId))
colnames(subdat1)[1:3] <- c("age1","gender1","MostAttach1")
colnames(subdat2)[1:3] <- c("age2","gender2","MostAttach2")
colnames(subdat3)[1:3] <- c("age3","gender3","MostAttach3")
colnames(subdat4)[1:3] <- c("age4","gender4","MostAttach4")

# check if complete it 4 times
subdat_merge1 <- merge(subdat1,subdat2,by="workerId")
subdat_merge2 <- merge(subdat3,subdat4,by="workerId")
subdat_merge_all <- merge(subdat_merge1,subdat_merge2,by="workerId")
subdat_same_all <- subdat_merge_all %>%
  filter(age1==age2) %>%
  filter(age3==age4) %>%
  filter(age1==age3) %>%
  filter(gender1==gender2) %>%
  filter(gender3==gender4) %>%
  filter(gender1==gender3)
diff_worker_four <- setdiff(subdat_merge_all$workerId,subdat_same_all$workerId)

subdat_same_all_pet <- subdat_same_all %>%
  mutate(MostAttach1=as.character(MostAttach1)) %>%
  mutate(MostAttach2=as.character(MostAttach2)) %>%
  mutate(MostAttach3=as.character(MostAttach3)) %>%
  mutate(MostAttach4=as.character(MostAttach4)) %>%
  droplevels() %>%
  filter(MostAttach1==MostAttach2) %>%
  filter(MostAttach3==MostAttach4) %>%
  filter(MostAttach1==MostAttach3)

diff_worker_pet <- setdiff(subdat_same_all$workerId,subdat_same_all_pet$workerId)

diff_worker_all <- c(diff_worker_four,diff_worker_pet)


# check if complete it three times
workId_four <- subdat_merge_all$workerId

subdat1 <- subdat1 %>%
  filter(!(workerId %in% workId_four))
subdat2 <- subdat2 %>%
  filter(!(workerId %in% workId_four))
subdat3 <- subdat3 %>%
  filter(!(workerId %in% workId_four))
subdat4 <- subdat4 %>%
  filter(!(workerId %in% workId_four))

common_col <- c("age","gender","MostAttach")
colnames(subdat1)[1:3] <- common_col
colnames(subdat2)[1:3] <- common_col
colnames(subdat3)[1:3] <- common_col
colnames(subdat4)[1:3] <- common_col

# except 1st
subdat_merge1 <- merge(subdat2,subdat3,by="workerId")
subdat_merge1 <- merge(subdat_merge1,subdat4,by="workerId")

# except 2nd
subdat_merge2 <- merge(subdat1,subdat3,by="workerId")
subdat_merge2 <- merge(subdat_merge2,subdat4,by="workerId")

# except 3rd
subdat_merge3 <- merge(subdat1,subdat2,by="workerId")
subdat_merge3 <- merge(subdat_merge3,subdat4,by="workerId")

# except 4th
subdat_merge4 <- merge(subdat1,subdat2,by="workerId")
subdat_merge4 <- merge(subdat_merge4,subdat3,by="workerId")

# all into list
subdat_merge_three_list <- list(subdat_merge1,subdat_merge2,subdat_merge3,subdat_merge4)


for(d in 1:n){
  subdat_merge_d <- subdat_merge_three_list[[d]]
  subdat_same_d <- subdat_merge_d %>%
    filter(age.x==age.y) %>%
    filter(age.x==age) %>%
    filter(gender.x==gender.y) %>%
    filter(gender.x==gender) 
  diff_worker_1 <- setdiff(subdat_merge_d$workerId,subdat_same_d$workerId)
  
  subdat_pet <- subdat_same_d %>%
    mutate(MostAttach.x=as.character(MostAttach.x)) %>%
    mutate(MostAttach.y=as.character(MostAttach.y)) %>%
    mutate(MostAttach=as.character(MostAttach)) %>%
    droplevels() %>%
    filter(MostAttach.x==MostAttach.y)%>%
    filter(MostAttach.x==MostAttach)
  
  diff_worker_2 <- setdiff(subdat_same_d$workerId,subdat_pet$workerId)
  
  diff_worker_d <- c(diff_worker_1,diff_worker_2)
  
  diff_worker_all <- c(diff_worker_all,diff_worker_d)
  
  print(paste0("except",d,": ",length(diff_worker_1)," ; ",length(diff_worker_2)))
}



# check if complete it only twice
workId_three <- c(subdat_merge1$workerId,subdat_merge2$workerId,
                  subdat_merge3$workerId,subdat_merge4$workerId)

subdat1 <- subdat1 %>%
  filter(!(workerId %in% workId_three))
subdat2 <- subdat2 %>%
  filter(!(workerId %in% workId_three))
subdat3 <- subdat3 %>%
  filter(!(workerId %in% workId_three))
subdat4 <- subdat4 %>%
  filter(!(workerId %in% workId_three))

subdat_merge12 <- merge(subdat1,subdat2,by="workerId")
subdat_merge23 <- merge(subdat2,subdat3,by="workerId")
subdat_merge13 <- merge(subdat1,subdat3,by="workerId")
subdat_merge14 <- merge(subdat1,subdat4,by="workerId")
subdat_merge24 <- merge(subdat2,subdat4,by="workerId")
subdat_merge34 <- merge(subdat3,subdat4,by="workerId")

subdat_merge_twice_list <- list(subdat_merge12,subdat_merge23,subdat_merge13,
                                subdat_merge14,subdat_merge24,subdat_merge34)
merge_name <- c("12","23","13","14","24","34")

for(d in 1:length(subdat_merge_twice_list)){
  subdat_merge_d <- subdat_merge_twice_list[[d]]
  subdat_same_d <- subdat_merge_d %>%
    filter(age.x==age.y) %>%
    filter(gender.x==gender.y) 
  
  diff_worker_1 <- setdiff(subdat_merge_d$workerId,subdat_same_d$workerId)
  
  subdat_pet <- subdat_same_d %>%
    mutate(MostAttach.x=as.character(MostAttach.x)) %>%
    mutate(MostAttach.y=as.character(MostAttach.y)) %>%
    droplevels() %>%
    filter(MostAttach.x==MostAttach.y)
  
  diff_worker_2 <- setdiff(subdat_same_d$workerId,subdat_pet$workerId)
  
  diff_worker_d <- c(diff_worker_1,diff_worker_2)
  
  diff_worker_all <- c(diff_worker_all,diff_worker_d)
  
  print(paste0(merge_name[d],": ",length(diff_worker_1)," ; ",length(diff_worker_2)))
}


for(d in 1:n){
  dat <- rawdatalist[[d]]
  dat <- dat %>%
    filter(!(workerId %in% diff_worker_all)) %>%
    mutate(gender=as.factor(gender))
  dat <- droplevels(dat)
  ##### output the data
  write.csv(dat,paste0(savedir,'wave',d,"_cleaned_0627.csv"),row.names = F)
}


