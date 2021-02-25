# My super cool Pirates Worksheet :D

#Working Directory

setwd("~/2020-2021 College Files/Fall 2020/dr. ruth research + learning R")
#the directory can be set to anywhere, this is just where I wanted it in Fall 2020!

#Packages Libraries and Databases
#install packages from the bottom-right of R studio! (or install.packages(the_package))
library(yarrr)
library(dplyr)
library(ggplot2)

#first 6 rows of data
head(pirates)
#variable names
names(pirates)
#what type of data each variable is
str(pirates)
#shows us the entire dataset in a different file!
View(pirates)


#Descriptive Statistics


#A $ after the dataframe will give the mean of that particular variable!
mean(pirates$beard.length)

#11
average_age <- pirates %>%
    summarize(mean = mean(age))
average_age

#12
tallest_pirate <- pirates %>%
    summarize(tall_friend = max(height))
tallest_pirate

#13
pirates_sex_count <- pirates %>%
  group_by(sex) %>%
    summarize(n())
pirates_sex_count

#14
avg_age_by_sex <- pirates %>%
  group_by(sex) %>%
    summarize(mean(age))
avg_age_by_sex

#summary statistics of all columns of the dataframe
summary(pirates)
#age range is 11 to 46! Sa fint!


#making a scatterplot


#weight vs height of all pirates
#alpha controls opacity of points
weight_vs_height <- ggplot(data=pirates,aes(x=height, y=weight)) +
  geom_point(alpha=0.5, colour="red3") +
  labs(title = "Weight vs Height of all Pirates!", x = "height (cm)", y = "weight (kg)") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5, size=14)) +
  theme(axis.title = element_text(size = 12)) +
  #creates a linear (lm) line of best fit, se for CI
  geom_smooth(method='lm', se=FALSE)

ggsave(weight_vs_height, filename = 'pirates_scatterplot.png')

#get a bunch of aesthetic choices w the following line of code:
#vignette("ggplot2-specs")

#21
age_vs_parrots <- ggplot(data=pirates,aes(x=age,y=parrots)) +
  #pch for solid round marker, markers grey and transparent
  geom_point(pch=6,col=gray(level=0.3,alpha=0.6)) +
  labs(title = "Parrot Ownership vs Age") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12))
  

#Data Plotting: Bar Graphs







  

