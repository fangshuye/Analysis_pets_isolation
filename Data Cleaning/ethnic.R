# add ethnic column for wave 3 and wave 4
# load packages
library(dplyr)
library(reshape)

# read the data
setwd('/Users/fangshu/Desktop/research/Analysis_pets_isolation/COVIDandPETS-master/cleaned_data/')


alldata <- c()
rawdatalist <- list()
for (d in 1:4) {
  rawdatalist[[d]] <- read.csv(paste0("wave",d,"_cleaned_0805.csv"))
  
  if(d %in% c(1,2)){
    rawdatalist[[d]] <- data.frame(dplyr::select(rawdatalist[[d]],workerId,ethnic))
  }
  
}

wave1 <- rawdatalist[[1]]
wave2 <- rawdatalist[[2]]
wave3 <- rawdatalist[[3]]
wave4 <- rawdatalist[[4]]
# check the consistency between wave 1 and wave 2 in ethnic
# common_1_2 <-  intersect(wave1$workerId, wave2$workerId)
merged_1_2 <- merge(wave1, wave2, by = "workerId")
merged_1_2_diff <- merged_1_2 %>% filter(ethnic.x != ethnic.y)

print(merged_1_2_diff)
diff_ID = merged_1_2_diff$workerId # only 2 rows
# check if those two appears in wave 3 and wave4
wave3_4_id = c(wave3$workerId, wave4$workerId)
wave3_4_id = unique(wave3_4_id)

diff_ID_appear_3_4 = intersect(wave3_4_id, diff_ID) # only workId: A38E0LSBY662SJ

# fill in wave 3 and wave4 using the the info from wave 1 and wave 2

# only need wave1 and wave2 with columns (id and ethnic here)
for (d in 3:4) {
  temp_dat <- read.csv(paste0("wave",d,"_cleaned_0805.csv"))
  
  # merge the data from wave1 first
  temp_dat  <- merge(temp_dat, wave1, by = 'workerId', all.x = TRUE)
  names(temp_dat)[ncol(temp_dat)] <- 'ethnic_wave1'
  temp_dat <- merge(temp_dat, wave2, by = 'workerId', all.x = TRUE)
  names(temp_dat)[ncol(temp_dat)] <- 'ethnic_wave2'
  
  temp_dat <- temp_dat %>%
    mutate(ethnic = ifelse(is.na(ethnic_wave1), ethnic_wave2, 
                           ifelse(is.na(ethnic_wave2), ethnic_wave1, 
                                  ifelse(ethnic_wave1==ethnic_wave2, ethnic_wave1, NA))))
  
  # remove cols
  temp_dat <- temp_dat %>%
    mutate(ethnic_wave1 = NULL) %>%
    mutate(ethnic_wave2 = NULL)
  
  write.csv(temp_dat,paste0('wave',d,"_cleaned_20210615.csv"),row.names = F)
}

