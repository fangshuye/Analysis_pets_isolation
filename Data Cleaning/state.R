statename <- state.name
statename <- c(statename,"District of Columbia","Puerto Rico")
statename <- statename[order(statename)]
statedata <- data.frame(state=statename,statenum=c(1:52))
statedata[53,] <- c("I do not reside in the United States",53)


savedir <- "/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/"
write.csv(statedata,paste0(savedir,"state_info.csv"),row.names = F)


# add state column
rm(list = ls())
library(dplyr)
library(sf)
library(spData)
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

setwd("/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/")
rawdatalist <- list()
for (d in 1:4) {
  rawdatalist[[d]] <- read.csv(paste0("wave",d,"_cleaned_0627.csv"))
  rawdatalist[[d]] <- data.frame(dplyr::select(rawdatalist[[d]],workerId,state))
}

wave1 <- rawdatalist[[1]]
wave2 <- rawdatalist[[2]]
wave3 <- rawdatalist[[3]]
wave4 <- rawdatalist[[4]]
sub_cbind_12 <- rbind(wave1,wave2)
sub_merge_34 <- rbind(wave3,wave4)
workerId_34 <- unique(sub_merge_34$workerId)
sub_merge_12 <- merge(wave1,wave2,by="workerId")
workerId_12 <- sub_merge_12$workerId

# appear in wave1 and wave2 
workerId_part1 <- intersect(workerId_34,workerId_12)
workerId_left <- setdiff(workerId_34,workerId_part1)
# appear only in wave1
workerId_part2 <- intersect(workerId_left,wave1$workerId)
# appear only in wave2
workerId_part3 <- intersect(workerId_left,wave2$workerId)
# neither in wave1 nor in wave2
workerId_part4 <- setdiff(workerId_34,sub_cbind_12$workerId)
workerId_part4_1 <- setdiff(workerId_part4,intersect(wave3$workerId,wave4$workerId))
workerId_part4_2 <- setdiff(workerId_part4,workerId_part4_1)


table_part2 <- wave1[which(wave1$workerId %in% workerId_part2),]
table_part3 <- wave2[which(wave2$workerId %in% workerId_part3),]
table_part4_1 <- sub_merge_34[which(sub_merge_34$workerId %in% workerId_part4_1),]
table_part4_2 <- merge(wave3,wave4,by="workerId")
table_part4_2 <- table_part4_2[which(table_part4_2$workerId %in% workerId_part4_2),]
print(table_part4_2)
# see the consistency of state in wave1 and wave2
dat_part1 <- sub_merge_12[which(sub_merge_12$workerId %in% workerId_part1),]
dat_part1_same <- dat_part1 %>%
  filter(state.x==state.y)
table_part1 <- dat_part1_same[,c(1,2)]
colnames(table_part1) <- colnames(table_part2)
dat_part1_diff <- dat_part1 %>%
  filter(state.x!=state.y)
# deal with this diff one
workid <- dat_part1_diff$workerId
x <- c()
y <- c()
for (d in 1:4) {
  dat <- read.csv(paste0("wave",d,"_cleaned_0627.csv"))
  x <- c(x,dat[which(dat$workerId==workid),"LocationLongitude"])
  y <- c(y,dat[which(dat$workerId==workid),"LocationLatitude"])
}

# print
lonlat_to_state(data.frame(x,y))

table_part1[465,1] <- workid
table_part1[465,2] <- 23

all_table <- rbind(table_part1,table_part2,table_part3)
#  re-create the data
for(d in 1:4){
  
  dat <- read.csv(paste0("wave",d,"_cleaned_0627.csv"))
  
  if(d %in% c(3,4)){
    
    print(nrow(dat))
    workidall <- dat$workerId
    
    dat_1 <- dat[which(dat$workerId %in% workerId_part4),]
    dat_2 <- dat[which(dat$workerId %in% all_table$workerId),]
    print(nrow(dat_1))
    print(nrow(dat_2))
    
    dat_2 <- dat_2 %>%
      mutate(state=NULL)
    dat_2 <- merge(dat_2,all_table,by="workerId")
    dat <- rbind(dat_1,dat_2)
  }
  ##### output the data
  write.csv(dat,paste0('wave',d,"_cleaned_0802.csv"),row.names = F)
}


# Manual error correction
for(d in 1:4){
  
  dat <- read.csv(paste0("wave",d,"_cleaned_0802.csv"))
  
  print(nrow(dat))
  if(d==2){
    dat[dat$workerId=="A1C7XI68SED8JE","state"] <- 23
  }
  
  ##### output the data
  write.csv(dat,paste0('wave',d,"_cleaned_0805.csv"),row.names = F)
}
