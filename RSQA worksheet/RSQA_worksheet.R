#start by looking at RSQA data (USGS RSQA)

setwd("C:/Users/Fawn-Rose/Documents/20XX College/Urban Ecosystems/DrRuthDatamining_Fr/RSQA worksheet")
getwd()

library(dplyr)
library(ggplot2)
library(readr)

#10: reading CSV into R using readr package (part of tidyverse)
myData <- read.csv("PNSQA pharm data 6.8/Results.csv")

#in environment tab (top right), observations are rows and variables are columns

#12: counting parameters (how many of each parameter is in the dataset?)
exp_count <- myData %>%
    group_by(PARM_NM) %>%
    summarize(count = n())

head(exp_count)

parameterCount <- ggplot(data = exp_count, aes(x=count)) +
    geom_histogram(binwidth = 100) +
  theme_classic() +
  labs(title = "How frequently was each pharmaceutical measured?") +
  theme(plot.title = element_text(hjust=0.5, size = 14), axis.title= element_text(size=12))

parameterCount

#13: taking out data with insignificant counts (<50) and sorting in decreasing count order

exp_count <- exp_count[exp_count$count>50,]
exp_count <- exp_count[order(exp_count$count, decreasing = T), ]
head(exp_count)

##Plotting one parameter

#picking 1 experimental parameter w lots of data points
#for the 6.9.2021 dataset, choosing Acetaminophen
#this is Tylenol (pain med)

HIV_med_data <- myData[myData$PARM_NM == "Acetaminophen, w,f<0.2um", ]
#^dataframe wtih only avacavir data

#Abacavir is measured in ng/l
#values collected under var RESULT_VA

#17: scatterplot, then geom_jitter

Tylenol_plot <- ggplot(data=HIV_med_data, aes(x=PARM_NM, y = RESULT_VA)) +
  geom_jitter() +
  theme_classic() +
  labs(title = "Measurements of Acetaminophen in Pacific Northwest") +
  xlab("Pharmaceutical type") +
  ylab("Concentration detected (ng/L)") +
  theme(plot.title = element_text(hjust=0.5, size = 14), axis.title= element_text(size=12))

Tylenol_plot

##Plotting multiple parameters

#picking 5 pharmaceuticals with most measurements

top_5_data <- myData[myData$PARM_NM %in% exp_count$PARM_NM[1:5], ]
#the above sorts mydata by exp_count's order, which has most to least data points

top_5_plot <- ggplot(data=top_5_data, aes(x=PARM_NM, y = RESULT_VA, color = PARM_NM)) +
  geom_jitter() +
  theme_classic() +
  labs(title = "Measurements of Pharmaceuticals in Pacific Northwest") +
  xlab("Pharmaceutical type") +
  ylab("Concentration detected (ng/L)") +
  theme(plot.title = element_text(hjust=0.5, size = 14), axis.title= element_text(size=12)) +
  theme(axis.text.x = element_blank()) +
#the above line of code REMOVES THE X-AXIS LABELS (see aes(color) for how to make legend)
  theme(legend.position = "left" )
#above = attempting to change legend position

top_5_plot

#22: chems on diff concentration scales -- facet plot

facet_plot_top <- ggplot(data= top_5_data, aes(x=PARM_NM, y = RESULT_VA)) +
  geom_jitter() +
  theme_classic() +
  labs(title = "Measurements of Pharmaceuticals in Pacific Northwest") +
  xlab("Pharmaceutical type") +
  ylab("Concentration detected (ng/L)") +
  theme(plot.title = element_text(hjust=0.5, size = 14), axis.title= element_text(size=12)) +
  theme(axis.text.x = element_blank()) +
  #the above line of code REMOVES THE X-AXIS LABELS (see aes(color) for how to make legend)
  theme(legend.position = "left" ) +
  #facet_grid(.~PARM_NM) <-- does them all in a grid
  facet_wrap(.~PARM_NM, scales="free")
  #facet wrap does all of them in separate grids with separate scales

facet_plot_top

##Plotting data on maps

install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggspatial")
install.packages("rgeos")

library(rgeos)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)

#need to get Sites.csv to match to SITE_NO in data csv

sites <- read.csv("PNSQA pharm data 6.8/Sites.csv")

#new dataframe: all top 5 data with only matching data from sites
#use left_join command

top_5_data_sites <- top_5_data %>%
  left_join(sites, by="SITE_NO")

#MAKING A WORLD MAP BELOW! WOAH!

world <- ne_countries(scale="medium", returnclass = "sf")

world_map <- ggplot(data=world) +
  geom_sf() +
  theme_classic() +
  labs(title="World Map", x="Longitude", y="Latitude")

world_map

#plot our data onto the world map
world_map <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title= "Map of PNSQA Pharmaceuticals Sampling Sites", x = "Longitude", y = "Latitude",
       subtitle=paste0("A total of ",(length(unique(sites$SITE_NO)))," sites")) +
  geom_point(data=top_5_data_sites, aes(x = DEC_LONG_VA, y=DEC_LAT_VA), size =1, shape=19)

world_map
#but lol we can't see our points!

#27: zooming in on the world map to be min and max latitude and long of the sites

world_map <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title= "Map of PNSQA Pharmaceuticals Sampling Sites", x = "Longitude", y = "Latitude",
       subtitle=paste0("A total of ",(length(unique(sites$SITE_NO)))," sites")) +
  geom_point(data=top_5_data_sites, aes(x = DEC_LONG_VA, y=DEC_LAT_VA), size =1, shape=19) +
  coord_sf(xlim =
    c((min(sites$DEC_LONG_VA)),(max(sites$DEC_LONG_VA))),
    ylim =
    c((min(sites$DEC_LAT_VA)),(max(sites$DEC_LAT_VA))))
world_map

#28: zooming out a little so recognizable map

world_map <- ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title= "Map of PNSQA Pharmaceuticals Sampling Sites", x = "Longitude", y = "Latitude",
       subtitle=paste0("A total of ",(length(unique(sites$SITE_NO)))," sites")) +
  geom_point(data=top_5_data_sites, aes(x = DEC_LONG_VA, y=DEC_LAT_VA,color=STATE_NM.x), size =1, shape=19) +
  coord_sf(xlim =
             c((min(sites$DEC_LONG_VA)-2),(max(sites$DEC_LONG_VA)+2)),
           ylim =
             c((min(sites$DEC_LAT_VA)-2),(max(sites$DEC_LAT_VA)+2))) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
world_map

#29 on rsqa worksheet has MAPPING RESOURCES

##Replicating plots -- a useful skill when looking at papers
#Look closely and make a list of all diff things included in figure
#then make the figure one thing at a time

#before I even look at these figures, I'm going to import the Results and Sites csvs for these figures

resultsData <- read.csv("replicating plots data/Results.csv")
sitesInfo <- read.csv("replicating plots data/Sites.csv")

length(unique(resultsData$SITE_NO))

#1st figure -- max pyrene concs at each site sorted by county
#elements:
#getting the max pyrene conc of each site
#which also means SORTING BY SITE
#then sorting that data by county
#then need geom_point() of that sorted data with county on x and conc on y
#making sure to put legend to the right and have it sorted by county

#for this first figure, I don't even need to import sites data!

#dataframe with only pyrene
pyreneData <- resultsData[resultsData$PARM_NM == "Pyrene, solids", ]

length(unique(pyreneData$SITE_NO))

#sorting by county -- just for my own sanity
pyreneByCounty <- pyreneData %>%
    arrange(COUNTY_NM)

#figuring out the max pyrene value for each site

maxPyrene <- pyreneByCounty %>%
  group_by(SITE_NO, COUNTY_NM) %>%
    summarize(largest_pyrene_conc = max(RESULT_VA))

#now I need to make a geom_point to see if this worked
figureOneReplicant <- ggplot(data=maxPyrene, aes(x=COUNTY_NM, y=largest_pyrene_conc, color = COUNTY_NM)) +
    geom_jitter() +
    theme_bw() +
    theme(axis.text.x = element_blank()) +
    labs(x = "", y = "Max Concentration of Pyrene per Site (ug/kg", title="Max Pyrene Concentration for Each Site By County")

figureOneReplicant

ggsave(figureOneReplicant, filename = 'figure_one_replication.png')

#woohoo!! Figure 1 completed!

#Figure 2: fancy MAP of California sampling sites
#elements:
#Map has longitude and latitude values
#map is zoomed into the California coast
#There is a legend for colors by county again
#Max pyrene concentration (ug/kg) measures are listed with CIRCLES FOR IMPACT SIZE (sorted by 50, 100, 150, 200)

#ok so it seems like we can still use maxPyrene dataset but with map stuff

library(rgeos)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)

#adding sites data to pyrene data

pyreneWithMapData <- pyreneData %>%
  left_join(sitesInfo)

#adding lat and long values to maxPyrene dataset

#going to do map based on min and max long and lat vaLUES OH WAIT^
#BUT FIRST we have to make the object for the world map! here it is!

world <- ne_countries(scale="medium", returnclass = "sf")

figure2replicant <- ggplot(data=world) +
  geom_sf() + theme_bw() +
  labs(title= "Map of California Sampling Sites", x = "Longitude", y = "Latitude",
       subtitle=paste0("A total of ",(length(unique(pyreneWithMapData$SITE_NO)))," sites"), size = "Max Pyrene concentration (ug/kg)") +
  geom_point(data=pyreneWithMapData, aes(x = DEC_LONG_VA, y=DEC_LAT_VA, size = RESULT_VA, color = COUNTY_NM), alpha=0.5) +
  coord_sf(xlim =
             c((min(pyreneWithMapData$DEC_LONG_VA)-1),(max(pyreneWithMapData$DEC_LONG_VA)+1)),
           ylim =
             c((min(pyreneWithMapData$DEC_LAT_VA)-1),(max(pyreneWithMapData$DEC_LAT_VA)+1)))
  

figure2replicant

#the size function determines the size of each point!!! if you set it to a specific dataset it will automatically group it by subset


