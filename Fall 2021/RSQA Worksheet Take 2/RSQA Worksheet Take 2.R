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
#49: map packages
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rgeos)

#50: grabbing site location data from Sites.csv
getwd()
sites <- read.csv("12.20.2021 CSQA Pesticides Data/Sites.csv")

#50: merging sites data and data data
pesticides_with_sites_top_5_corr <- corr_top_5_data %>%
  left_join(sites)

#51: world dataframe + world map
world <- ne_countries(scale="medium", returnclass="sf")

world_map <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title = "World Map", x = "Longitude", y = "Latitude")
world_map

#52 editing the map, one line at a time
world_map <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title = "Map of CSQA Pesticides sampling sites", x = "Longitude", y = "Latitude", subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites")) +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12)) +
  geom_point(data=pesticides_with_sites_top_5_corr, aes(x=DEC_LONG_VA, y = DEC_LAT_VA), size = 1, shape=19)
world_map

#53: zooming in the map!
world_map <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title = "Map of CSQA Pesticides sampling sites", x = "Longitude", y = "Latitude", subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites")) +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12)) +
  geom_point(data=pesticides_with_sites_top_5_corr, aes(x=DEC_LONG_VA, y = DEC_LAT_VA), size = 1, shape=19) +
  coord_sf(xlim = c((min(sites$DEC_LONG_VA)-1), (max(sites$DEC_LONG_VA)+1)), 
           ylim=c((min(sites$DEC_LAT_VA)-1), (max(sites$DEC_LAT_VA)+1)))
world_map
#^coord_sf for coordinates for simple features map, xlim and ylim based on min and max values of our sites lat and long
#^54: adding +1 and -1 for sligtly more zoomed out map

#55: irrelevant here bc CSQA is only in CA


#messing around with maps for state lines
library(maps)

states <- map_data("state")

ggplot(data = world) +
  geom_sf() +
  geom_polygon(data=states, aes(x=long, y=lat, fill = region)) +
  coord_sf(xlim = c((min(states$long)-1), (max(states$long)+1)), 
           ylim=c((min(states$lat)-1), (max(states$lat)+1))) +
  guides(fill=FALSE)

#ok seems a bit funky but we are just gonna try this out...

world_map <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title = "Map of CSQA Pesticides sampling sites", x = "Longitude", y = "Latitude", subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites")) +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12)) +
  geom_polygon(data=states, aes(x=long, y=lat, fill = region)) +
  geom_point(data=pesticides_with_sites_top_5_corr, aes(x=DEC_LONG_VA, y = DEC_LAT_VA), size = 1, shape=19) +
  coord_sf(xlim = c((min(sites$DEC_LONG_VA)-1), (max(sites$DEC_LONG_VA)+1)), 
           ylim=c((min(sites$DEC_LAT_VA)-1), (max(sites$DEC_LAT_VA)+1))) +
  guides(fill=FALSE)
world_map

#hmm yeah they do still seem a little bit off... what if we just switched over to the maps package completely?

us_map <- map_data("usa")

test_map <- ggplot() + 
  geom_polygon(data = us_map, aes(x=long, y = lat, group = group), fill = "white", color = "black") + 
  geom_polygon(data=states, aes(x=long, y=lat, fill = region, group=group), color = "black") +
  geom_point(data=pesticides_with_sites_top_5_corr, aes(x=DEC_LONG_VA, y = DEC_LAT_VA), size = 1, shape=19) +
  coord_fixed(xlim = c((min(sites$DEC_LONG_VA)-1), (max(sites$DEC_LONG_VA)+1)), 
           ylim=c((min(sites$DEC_LAT_VA)-1), (max(sites$DEC_LAT_VA)+1))) +
  guides(fill=FALSE)
test_map

#that... just might have worked??
#I don't even think we really needed the US map undereath it, let's see...
test_map <- ggplot() + 
  geom_polygon(data=states, aes(x=long, y=lat, fill = region, group=group), color = "black") +
  geom_point(data=pesticides_with_sites_top_5_corr, aes(x=DEC_LONG_VA, y = DEC_LAT_VA), size = 1, shape=19) +
  coord_fixed(xlim = c((min(sites$DEC_LONG_VA)-5), (max(sites$DEC_LONG_VA)+5)), 
              ylim=c((min(sites$DEC_LAT_VA)-5), (max(sites$DEC_LAT_VA)+5))) +
  guides(fill=FALSE)
test_map
#yeah we didn't even need the big US map okay okay


##Replicating Plots -- moreso experimenting with nondetects here

problemsetData <- read.csv("ProblemsetRSQAData/Results.csv")
problemsetSites <- read.csv("ProblemsetRSQAData/Sites.csv")

#so before I actually replicate the plots, I'm going to get nondetects outta here

corr_problemset_data <- problemsetData %>%
  mutate(RESULT_CORR = ifelse(REMARK_CD == "<",0,RESULT_VA))

#looking at detected numbers

detectcount <- corr_problemset_data %>%
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

#changing detectcount to only include observations >25

detectcount <- detectcount[detectcount$observations >25, ]
detectcount <- detectcount[order(detectcount$observations, decreasing=T),]

#applying that to top 10 of corrected data

corr_top_10_data <- corr_problemset_data[corr_problemset_data$PARM_NM %in% detectcount$PARM_NM[1:10], ]

#plots
top_10_data_plot <- ggplot(data=corr_top_10_data, aes(x=PARM_NM, y=RESULT_CORR, color=PARM_NM)) +
  geom_jitter() +
  labs(title = "Top 10 most detected chemicals", x = "Chemical name", y = "Concentration detected (ng/L)")+
  theme_classic() +
  theme(axis.text.x = element_blank())
top_10_data_plot


top_10_data_plot <- ggplot(data=corr_top_10_data, aes(x=PARM_NM, y=RESULT_CORR, color=PARM_NM)) +
  geom_jitter() +
  facet_wrap(. ~PARM_NM, scales = "free") +
  labs(title = "Top 10 most detected chemicals", x = "Chemical name", y = "Concentration detected (ng/L)")+
  theme_classic() +
  theme(axis.text.x = element_blank())
top_10_data_plot

#hmmm... I think for now I'm going to skip this replicating plots section since it won't rly do me much good with nondetects not being in there



##Running Statistical Tests


