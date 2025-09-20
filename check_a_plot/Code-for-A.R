setwd("/Users/fangshu/Desktop/research/other")
library(dplyr)
library(readxl)
library(ggplot2)
turk <- read.csv("2020-4-17\ COVID_Raw_Data.csv")
turk$PetOwner <- factor(turk$PetOwner, levels = c("0","1"), labels = c("Pets=No", "Pets=Yes"))
turk$Compliance<-factor(turk$Compliance, levels = c("0","1"), labels = c("No", "Yes"))
turk$MostAttach<-factor(turk$MostAttach, levels = c("1","2","3","4","5","6","7","8"), labels = c("Dog", "Cat", "Horse", "Other", "Other", "Other" , "Other", "Other"))
tCompliance <- table(turk$Compliance, turk$MostAttach)

Compliance <- turk %>%
  group_by(Compliance, MostAttach)%>%
  summarise(Frequency =length(Compliance)) %>%
  subset(MostAttach %in% c("Dog","Cat"), na.rm = TRUE) 

ggplot(Compliance, aes(fill= (MostAttach), y=Frequency, x=Compliance)) +
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x = element_text(face="bold", color="#993333",size=10, angle=90),
        axis.text.y = element_text(face="bold", color="#993333",size=10, angle=0)) +
  ggtitle("Frequency distribution of ownership time  by pet") +
  labs(y="Freqency of Responces", x = "Question: Are you complying with quarantine regulations?")
