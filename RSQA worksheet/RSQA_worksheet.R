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

Tylenol_data <- myData[myData$PARM_NM == "Acetaminophen, w,f<0.2um", ]
#^dataframe wtih only avacavir data

#Abacavir is measured in ng/l
#values collected under var RESULT_VA

#17: scatterplot, then geom_jitter

Tylenol_plot <- ggplot(data=Tylenol_data, aes(x=PARM_NM, y = RESULT_VA)) +
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
  labs(title= "Map of California Sampling Sites", x = "Longitude", y = "Latitude", color = "California County",
       subtitle=paste0("A total of ",(length(unique(pyreneWithMapData$SITE_NO)))," sites"), size = "Max Pyrene concentration (ug/kg)") +
  geom_point(data=pyreneWithMapData, aes(x = DEC_LONG_VA, y=DEC_LAT_VA, size = RESULT_VA, color = COUNTY_NM), alpha=0.5) +
  coord_sf(xlim =
             c((min(pyreneWithMapData$DEC_LONG_VA)-1),(max(pyreneWithMapData$DEC_LONG_VA)+1)),
           ylim =
             c((min(pyreneWithMapData$DEC_LAT_VA)-1),(max(pyreneWithMapData$DEC_LAT_VA)+1))) +
  theme(legend.key = element_rect(fill = "gray92"),
        panel.background = element_rect(fill="gray92"), 
        panel.grid = element_line(color="white",size=1)) +
  scale_size(range = c(0.5, 10))
#THE ABOVE LINE DOES THE SCALE_SIZE FOR THE RANGE OF BUBBLE SIZES!!

figure2replicant

#the size function determines the size of each point!!! if you set it to a specific dataset it will automatically group it by subset



#33: same thing but with a diff chemical

resultsData <- read.csv("replicating plots data/Results.csv")
sitesInfo <- read.csv("replicating plots data/Sites.csv")

exp_count <- resultsData %>%
  group_by(PARM_NM) %>%
  summarize(count = n())

head(exp_count)

exp_count <- exp_count[exp_count$count>50,]
exp_count <- exp_count[order(exp_count$count, decreasing = T), ]
head(exp_count)

#based on this, choosing Imidacloprid, wf

imidData <- resultsData[resultsData$PARM_NM == "Imidacloprid, wf", ]

maxImida <- imidData %>%
  group_by(SITE_NO, COUNTY_NM) %>%
  summarize(largest_imida = max(RESULT_VA))

#now I need to make a geom_point to see if this worked
figureOneReplicant <- ggplot(data=maxImida, aes(x=COUNTY_NM, y=largest_imida, color = COUNTY_NM)) +
  geom_jitter() +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  labs(x = "", y = "Max Concentration of Imidacloprid per Site (ug/kg)", title="Max Imidacloprid Concentration for Each Site By County")

figureOneReplicant

#lol kinda boring data but whatever

imidaWithMapData <- imidData %>%
  left_join(sitesInfo)

world <- ne_countries(scale="medium", returnclass = "sf")

figure2replicant <- ggplot(data=world) +
  geom_sf() + theme_bw() +
  labs(title= "Map of California Sampling Sites", x = "Longitude", y = "Latitude", color = "California County",
       subtitle=paste0("A total of ",(length(unique(pyreneWithMapData$SITE_NO)))," sites"), size = "Max Imidacloprid concentration (ug/kg)") +
  geom_point(data=imidaWithMapData, aes(x = DEC_LONG_VA, y=DEC_LAT_VA, size = RESULT_VA, color = COUNTY_NM), alpha=0.3) +
  coord_sf(xlim =
             c((min(pyreneWithMapData$DEC_LONG_VA)-1),(max(pyreneWithMapData$DEC_LONG_VA)+1)),
           ylim =
             c((min(pyreneWithMapData$DEC_LAT_VA)-1),(max(pyreneWithMapData$DEC_LAT_VA)+1))) +
  theme(legend.key = element_rect(fill = "gray92"),
        panel.background = element_rect(fill="gray92"), 
        panel.grid = element_line(color="white",size=1)) +
  scale_size(range = c(0.5, 10))

figure2replicant

#34: use data from dataset I downloaded way back in step 3 (PNSQA pharmaceuticals)

#myData is the datset, exp_count shows 6 most plentiful chemicals

head(exp_count)

#deciding on Hydroxyphthalazinone, wf to plot

hydroPhthalData <- myData[myData$PARM_NM == "Hydroxyphthalazinone, wf", ]

maxHydrophthal <- hydroPhthalData %>%
  group_by(SITE_NO, COUNTY_NM) %>%
  summarize(largest_hydrop_conc = max(RESULT_VA))

#now I need to make a geom_point to see if this worked
figureOneReplicant <- ggplot(data=maxHydrophthal, aes(x=COUNTY_NM, y=largest_hydrop_conc, color = COUNTY_NM)) +
  geom_jitter() +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  labs(x = "", y = "Max Concentration of Hydroxyphthalazinone per Site (ug/kg", title="Max Hydroxyphthalazinone Concentration for Each Site By County")

figureOneReplicant

#now for figure 2, needing to import sites data bc PNSQA not CaSQA

sitesInfo <- read.csv("PNSQA pharm data 6.8/Sites.csv")

hydropWithMapData <- hydroPhthalData %>%
  left_join(sitesInfo)

world <- ne_countries(scale="medium", returnclass = "sf")

figure2replicant <- ggplot(data=world) +
  geom_sf() + theme_bw() +
  labs(title= "Map of Pacific Northwest Sampling Sites", x = "Longitude", y = "Latitude", color = "Pacific Northwest County",
       subtitle=paste0("A total of ",(length(unique(hydropWithMapData$SITE_NO)))," sites"), size = "Max Hydroxyphthalazinone concentration (ug/kg)") +
  geom_point(data=hydropWithMapData, aes(x = DEC_LONG_VA, y=DEC_LAT_VA, size = RESULT_VA, color = COUNTY_NM), alpha=0.5) +
  coord_sf(xlim =
             c((min(hydropWithMapData$DEC_LONG_VA)-1),(max(hydropWithMapData$DEC_LONG_VA)+1)),
           ylim =
             c((min(hydropWithMapData$DEC_LAT_VA)-1),(max(hydropWithMapData$DEC_LAT_VA)+1))) +
  theme(legend.key = element_rect(fill = "gray92"),
        panel.background = element_rect(fill="gray92"), 
        panel.grid = element_line(color="white",size=1)) +
  scale_size(range = c(0.5, 5)) +
  theme(axis.text.x = element_text(angle=-45),
        legend.box="horizontal")
#THE ABOVE LINE DOES THE SCALE_SIZE FOR THE RANGE OF BUBBLE SIZES!!

figure2replicant
#woohoo!

##Adding statistics to your figures

install.packages(ggpubr)
#ggpubr useful function: compare_means() has lots of stats in a dataframe

library(ggpubr)

#36: using compare_means() on all pyrene data

pyreneData <- resultsData[resultsData$PARM_NM == "Pyrene, solids", ]

compare_means(RESULT_VA~COUNTY_NM,data=pyreneData, method="kruskal.test", paired=FALSE,p.adjust.method="fdr")
#compare_means(measured_values(y)~values_changing(x)
#method can be "t.test", "wilcox.test", "anova", and "kruskal.test"

#p.signif is ns, so not significantly different

#39: running compare_means on another chem pyrene and chem downloaded data set

#for Acetaminophen, w,f<0.2um, aka "Tylenol_data"

compare_means(RESULT_VA~COUNTY_NM,data=Tylenol_data,method="kruskal.test",paired=FALSE,p.adjust.method="fdr")

#^p.signif is * so p<0.05 (95% confidence)

#now for Hydroxyphthalazinone, wf aka "hydroPhthalData" :

compare_means(RESULT_VA~COUNTY_NM,data=hydroPhthalData,method="kruskal.test",paired=FALSE,p.adjust.method="fdr")

#^p.signif is ns so p>0.05 (no significant difference)

#40: post-hoc tests woohoo!
#first analyzing pyrene:
posthoc_results_pyrene <- compare_means(RESULT_VA~COUNTY_NM,data=pyreneData, method="wilcox.test", paired=FALSE,p.adjust.method="fdr")

#^dang all of those post-hocs and ONE set is diff from one another even though ns?!
#lol is she about to teach us about adjusted p-values
#the reason why we need p values is Multiplicity Effect

#41: boxplot by county of pyrene data

pyreneByCounty <- ggplot(data=pyreneData, aes(x=COUNTY_NM, fill=COUNTY_NM, y=RESULT_VA))+
    geom_boxplot() +
    theme_classic() +
    theme(axis.text.x = element_blank()) +
    labs(title="Pyrene Concentration by California County", x = "Pyrene concentration (ug/kg)", y = "California County Name") +
    stat_pvalue_manual(inherit.aes=FALSE,data=posthoc_results_pyrene_sig,label="p",y.position = 220)
  #^43: adding posthoc result to my plot from 41!
  #inherit.aes tells graph to use posthoc_results_sig rather than pyrene_data file 
  #label adds text as label on our plot
  #y.position tells graph at what value of y the bracket should be placed

pyreneByCounty

#42: finding only significant p values results

posthoc_results_pyrene_sig <- posthoc_results_pyrene[posthoc_results_pyrene$p<0.05,]

head(posthoc_results_pyrene_sig)

#45: running post-hoc on tylenol and hypth data

compare_means(RESULT_VA~COUNTY_NM,data=Tylenol_data,method="kruskal.test",paired=FALSE,p.adjust.method="fdr")

#^p.signif is * so p<0.05 (95% confidence)
#post-hoc shows 3 with significant differences:
post_hoc_tylenol <- compare_means(RESULT_VA~COUNTY_NM,data=Tylenol_data, method="wilcox.test", paired=FALSE,p.adjust.method="fdr")

#now for Hydroxyphthalazinone, wf aka "hydroPhthalData" :

compare_means(RESULT_VA~COUNTY_NM,data=hydroPhthalData,method="kruskal.test",paired=FALSE,p.adjust.method="fdr")

#post-hoc:
post_hoc_hydroPh <- compare_means(RESULT_VA~COUNTY_NM,data=hydroPhthalData, method="wilcox.test", paired=FALSE,p.adjust.method="fdr")
#^all insigifnicant still

#plotting Tylenol with differences

post_hoc_Tylenol_sig <- post_hoc_tylenol[post_hoc_tylenol$p<0.05,]

tylenolByCounty <- ggplot(data=Tylenol_data, aes(x=COUNTY_NM, fill=COUNTY_NM, y=RESULT_VA))+
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_blank()) +
  labs(title="Tylenol Concentration by PNSQA County", y = "Tylenol concentration (ug/kg)", legend = "County Name") +
  stat_pvalue_manual(inherit.aes=FALSE,data=post_hoc_Tylenol_sig,label="p.signif",y.position = 220, step.increase=1)

tylenolByCounty

# y.position, which is the location of the first bracket
#step.increase, which tells the plot what fraction of the total height of the plot spacing should be added between each bracket (each value of p)


##Multivariable analysis: preparing/cleaning data

#46: new data time!Inorganics all 5 regions

inorganicResults <- read.csv("all 5 regions inorganic 7.2/Results.csv")
inorganicSites <- read.csv("all 5 regions inorganic 7.2/Sites.csv")

#finding median of each chemical at each site

medianInorganicsBySite <- inorganicResults %>%
    group_by(SITE_NO, PARM_NM) %>%
        summarize(median_val = median(RESULT_VA)) %>%
    ungroup()

head(medianInorganicsBySite)

#49

library(tidyr)

#converting from long form to short form
#long form: measurements of one chemical at one site
#short form: every row is one site, every column is one chemical, cells are RESULT_VA
#metadata lost in short form, but consolidates it

shortFormInorganics <- medianInorganicsBySite %>%
    pivot_wider(id_cols = SITE_NO, names_from = PARM_NM, values_from = median_val)

#wow there's a lot of NA values... I guess not all chemicals were tested at all sites (makes sense)

#50: lol let's address the NA values

#Na = not tested, 0 = tested and nothing found

#PCA can't do NA values so we have to remove them

colSums(is.na(shortFormInorganics))

#choose an interesting set of columns with minimal NAs in them

Interestingcolumns_data <- as.data.frame(shortFormInorganics[,c(2,7,8,9,11,12,16,17)])

#making site number row name so it won't be used in PCA

rownames(Interestingcolumns_data)<- shortFormInorganics$SITE_NO

#now to actually remove the NAs:

finaldata <- Interestingcolumns_data[complete.cases(Interestingcolumns_data), ]
#^keeping all columns and removing "incomplete" rows (rows with NA)

#ideally, have 200+ observations (rows) for PCA analysis

#creating metadata file to accompany finaldata:

metadata <- inorganicSites[match(rownames(finaldata),inorganicSites$SITE_NO),]

#^rows are in the same order as the sites in finaldata

##Multivariable analysis: Heatmaps and Hierarchical Clustering

#54

#create correlation matrix for variables:

data_cor <- cor(finaldata)

#heatmap: need pheatmap package

library(pheatmap)

inorganicsHeatMap <- pheatmap(data_cor,annotations=rownames(data_cor),show_rownames =T,show_colnames = T)

#red is positive correlation (vars increase w/each other)
#blue is neg correlation (1 incr while other decr)
#white is no correlation

#expectations: red diagnoal line bc chems are perfectly correlated with each other

