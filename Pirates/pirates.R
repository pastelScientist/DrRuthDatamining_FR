# My super cool Pirates Worksheet :D

#Working Directory


setwd("~/20XX College Files/Urban Ecosystems/DrRuthDatamining_FR")

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



#Making Scatterplots


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
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12))
  theme_classic() 
  #add custom elements to a pre-set theme by adding the arguments in a theme() ABOVE the pre-set theme

age_vs_parrots

#Data Plotting: Bar Graphs

#geom_bar() makes a histogram

yuckySexHistogram <- ggplot(data=pirates,aes(x=sex))+ 
  geom_bar(fill="#bf5700") +
  labs(title = "Distribution of sex on the S.S. Rrrrr") 

yuckySexHistogram

secondBlehSexHis <- ggplot(data=pirates,aes(x=sex))+
  geom_bar(aes(fill=headband))

secondBlehSexHis

ggsave(secondBlehSexHis, filename = 'pirates_histogram.png')

#making dataframe of avg and stdev beard length
avgBeard <- pirates %>%
  group_by(sex) %>%
  summarize(avgBeardLength = mean(beard.length), stDevBeardLength = sd(beard.length))

avgBeard

#geom_col for bar chart (not histogram)
#geom_errorbar needs aes(ymin and ymax) in order to work

beardLengthByFakeGender <- ggplot(data=avgBeard, aes(x=sex, y=avgBeardLength)) +
  geom_col(aes(fill="#ff0")) +
  geom_errorbar(aes(ymin = avgBeardLength - stDevBeardLength, ymax = avgBeardLength + stDevBeardLength), width = 0.2) +
  labs(title = "Distribution of beard length by 'gender'")
  
beardLengthByFakeGender

#Data Plotting: Box Plot

  

