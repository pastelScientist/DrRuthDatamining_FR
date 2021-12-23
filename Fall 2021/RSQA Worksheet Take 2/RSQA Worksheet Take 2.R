##Downloading Data (see procedure for RSQA website steps)

#2: for downloaded data, picking California pesticides data

#see RSQA worksheet for where to find different types of info within RSQA data downloads


##Getting Set Up

#7: making this file + setting working directory
setwd("C:/Users/Fawn-Rose/Documents/20XX College/Urban Ecosystems/DrRuthDatamining_FR/Fall 2021/RSQA Worksheet Take 2")
getwd()


##Loading Data into R
library(readr)
library(dplyr)
library(ggplot2)

#10:
mydata <- read.csv("12.20.2021 CSQA Pesticides Data/Results.csv")

#12: Observation count of each parameter measured
expcount <- mydata %>%
  group_by(PARM_NM) %>%
    summarize(observations = n())
expcount

parm_count <- ggplot(data=expcount,aes(x=observations)) +
  geom_histogram(binwidth=100) +
  labs(title = "Number of parameters (y) that had x number of observations") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5, size=11), axis.title = element_text(size=12))
parm_count

#14: dataframe of all parameters with observations>50, ordered in a decreasing way
expcount <- expcount[expcount$observations >50,]
expcount <- expcount[order(expcount$observations,decreasing=T),]
head(expcount)


##Plotting one parameter

#15: Myclobutanil, wf dataframe
Myclobutanil_data <- mydata[mydata$PARM_NM == "Myclobutanil, wf",]

