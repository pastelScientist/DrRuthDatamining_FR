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

#16: Myclobutanil info in lab notebook

#18: scatterplot of RESULT_VA of Myclobutanil data
myclobutanil_plot <- ggplot(data=Myclobutanil_data, aes(x=PARM_NM, y=RESULT_VA)) +
    geom_point() +
    labs(title = "Myclobutanil concentration summary")
myclobutanil_plot

#19: JITTERPLOT of RESULT_VA of Myclubtanil data
myclobutanil_plot <- ggplot(data=Myclobutanil_data, aes(x=PARM_NM, y=RESULT_VA)) +
  geom_jitter() +
  labs(title = "Myclobutanil concentration summary", x = "Myclobutanil, wf", y = "Concentration detected (ng/L)")+
  theme_classic() +
  theme(axis.text.x =element_blank())
myclobutanil_plot


##Plotting multiple parameters
#21:
top_5_data <- mydata[mydata$PARM_NM %in% expcount$PARM_NM[1:5], ]

#22: jitterplot of top_5_data
top_5_data_plot <- ggplot(data=top_5_data, aes(x=PARM_NM, y=RESULT_VA, color=PARM_NM)) +
  geom_jitter() +
  labs(title = "Top 5 Most Detected Pesticides", x = "Pesticide name", y = "Concentration detected (ng/L)")+
  theme_classic() +
  theme(axis.text.x = element_blank())
top_5_data_plot

#24: facet plot
top_5_data_plot <- ggplot(data=top_5_data, aes(x=PARM_NM, y=RESULT_VA)) +
  geom_jitter() +
  facet_wrap(. ~PARM_NM, scales = "free") +
  labs(title = "Top 5 Most Detected Pesticides", x = "Pesticide name", y = "Concentration detected (ng/L)")+
  theme_classic() +
  theme(axis.text.x = element_blank())
top_5_data_plot

top_5_data_plot <- ggplot(data=top_5_data, aes(x=PARM_NM, y=RESULT_VA)) +
  geom_jitter() +
  facet_wrap(.~PARM_NM, scales = "free") +
  labs(title = "Top 5 Most Detected Pesticides", x = "Pesticide name", y = "Concentration detected (ng/L)")+
  theme_classic() +
  theme(axis.text.x = element_blank())
top_5_data_plot

#lol looks like 3 of the 5 most detected are actually all nondetects (just looking qualitatively)


##Dealing with non-detects

#USGS collabs say: all estimates are true, all nondetects are replaced with zeroes

#27:
Myclobutanil_no_nondetects <- Myclobutanil_data %>%
  mutate(RESULT_CORR = ifelse(REMARK_CD == "<",0,RESULT_VA))

#27 with mydata instead:
mydatacorr <- mydata %>%
  mutate(RESULT_CORR = ifelse(REMARK_CD == "<",0,RESULT_VA))

#28: filtering out nondetects
detectcount <- mydatacorr %>%
  filter(REMARK_CD != "<") %>%
  group_by(PARM_NM) %>%
  summarize(observations = n())
detectcount

detected_hist <- ggplot(data=detectcount, aes(x=observations)) +
    geom_histogram(binwidth = 50) +
    labs(title = "Number of parameters (y) that had x number of observations") +
    theme_classic() +
    theme(plot.title = element_text(hjust=0.5, size=11), axis.title = element_text(size=12))
detected_hist

#29: dealing with NA values
detectcount <- mydatacorr %>%
  filter(REMARK_CD != "<") %>%
  filter(RESULT_CORR != "NA") %>%
  group_by(PARM_NM) %>%
  summarize(observations = n())
detectcount

detected_hist <- ggplot(data=detectcount, aes(x=observations)) +
  geom_histogram(binwidth = 50) +
  labs(title = "Number of parameters (y) that had x number of observations") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5, size=11), axis.title = element_text(size=12))
detected_hist

#31: re-doing jitterplot of myclobutanil with nondetects

Myclobutanil_corr <- Myclobutanil_data %>%
  mutate(RESULT_CORR = ifelse(REMARK_CD == "<",0,RESULT_VA))

myclobutanil_plot <- ggplot(data=Myclobutanil_corr, aes(x=PARM_NM, y=RESULT_CORR)) +
  geom_jitter() +
  labs(title = "Myclobutanil concentration summary", x = "Myclobutanil, wf", y = "Concentration detected (ng/L)")+
  theme_classic() +
  theme(axis.text.x =element_blank())
myclobutanil_plot
#this looks pretty similar to my other plot... interesting

#33: using it on whole dataframe, detect frequency
mydatacorr <- mydata %>%
  mutate(RESULT_CORR = ifelse(REMARK_CD == "<",0,RESULT_VA))

detectfreq <- mydatacorr %>%
  filter(RESULT_CORR != "0") %>%
  group_by(PARM_NM) %>%
  summarize(observations = n())
#this yielded the same results as #29 above

#37: detectfreq with decent numbers
detectfreq <- detectfreq[detectfreq$observations >25, ]
detectfreq <- detectfreq[order(detectcount$observations, decreasing=T),]

##Plotting Multiple Parameters: Re-Do with nondetects

#21 (note that this only works bc of #37 work):
corr_top_5_data <- mydatacorr[mydatacorr$PARM_NM %in% detectfreq$PARM_NM[1:5], ]

#22: jitterplot of top_5_data
top_5_data_plot <- ggplot(data=corr_top_5_data, aes(x=PARM_NM, y=RESULT_VA, color=PARM_NM)) +
  geom_jitter() +
  labs(title = "Top 5 Most Detected Pesticides", x = "Pesticide name", y = "Concentration detected (ng/L)")+
  theme_classic() +
  theme(axis.text.x = element_blank())
top_5_data_plot

#24: facet plot
top_5_data_plot <- ggplot(data=corr_top_5_data, aes(x=PARM_NM, y=RESULT_VA)) +
  geom_jitter() +
  facet_wrap(. ~PARM_NM, scales = "free") +
  labs(title = "Top 5 Most Detected Pesticides", x = "Pesticide name", y = "Concentration detected (ng/L)")+
  theme_classic() +
  theme(axis.text.x = element_blank())
top_5_data_plot


##Plotting Data on maps

