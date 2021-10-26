#Pirates Fall 2021 -- Iteration 2 for Fawn-Rose!


#Working Directory
getwd()
#can change wd using Files tab on bottom right
#navigate to desired wd, then "More" --> "Set as Working Directory"
setwd("C:/Users/Fawn-Rose/Documents/20XX College/Urban Ecosystems/DrRuthDatamining_FR/Fall 2021")
#run setwd every day I work on Pirates
#Saving: save R scripts but not workspace or environment


#Packages, Libraries and Databases
library(yarrr)
library(dplyr)
library(ggplot2)

#must re-run setwd and library commands each day
#don't re-install packages, just load the libraries each day

head(pirates) #column headings + first 6 rows for all columns

names(pirates) #names of columns ONLY

str(pirates) #how many rows (obs.) and columns (variables), datatype for each

View(pirates) #contains entire dataset for us to view


#Descriptive Statistics

#13: calculating avg pirate age:
mean(pirates$age)
# $ will refer to a specific column
pirates %>%
  summarize(mean_age = mean(age))

#14: height of tallest pirate
pirates %>%
  summarize(max_height = max(height))

#15: how many pirates of each sex
pirates %>%
  group_by(sex) %>%
    summarize(amount_gender = n())

#16: avg age of pirates of each sex
pirates %>%
  group_by(sex) %>%
    summarize(avg_age_by_sex = mean(age))

#17: summary stats for all columns
summary(pirates)
#age range: 11 to 46


#Data Plotting: Scatter Plot

#19-21
weight_vs_height <- ggplot(pirates,aes(x=weight, y = height)) +
  labs(title = "Weight vs Height of Pirates", x = "Weight (kg)", y = "Height (cm)") +
  geom_point(alpha=0.5) +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12))
#the above line adjust the title to be in the center and makes the text size 12pt
#alpha argument adjusts how see-through the points are
weight_vs_height

#22: previous qs with line of best fit
weight_vs_height <- ggplot(pirates,aes(x=weight, y = height)) +
  labs(title = "Weight vs Height of Pirates", x = "Weight (kg)", y = "Height (cm)") +
  geom_point(alpha=0.5) +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12)) +
#the above line adjust the title to be in the center and makes the text size 12pt
  geom_smooth(method="lm",se=FALSE)
#geom_smooth makes a line of best fit, method='lm' for linear model, se is hiding confidence interval
#alpha argument adjusts how see-through the points are
weight_vs_height

#23: age vs parrot ownership
age_vs_parrots <- ggplot(pirates,aes(x=age,y=parrots)) +
  labs(title = "Age vs Parrot Ownership", x = "Age (years)", y = "How many parrots do you own?") +
  geom_point(pch=16,col=gray(level=0.5,alpha=0.6)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12))
age_vs_parrots


#Data Plotting: Bar Graphs

#24: two ways of making bar graphs
#first is geom_bar() for histograms
#second is geom_col()
myplot <- ggplot(data=pirates,aes(sex)) +
  geom_bar(fill="#bf5700") +
  labs(title = "How many of each sex are in this pirate crew?") +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12))
myplot
#fill command in geom_bar changes what color the bars are filled with

#25: adding additional category
myplot <- ggplot(data=pirates,aes(sex)) +
  geom_bar(aes(fill=headband)) +
  labs(title = "How many of each sex are in this pirate crew?") +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12))
myplot
#^filling bar with headband yes/no for each sex

myplot <- ggplot(data=pirates,aes(sex)) +
  geom_bar(aes(fill=headband),position="dodge") +
  labs(title = "How many of each sex are in this pirate crew?") +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12))
myplot
#^two different bars for headband yes/no

myplot <- ggplot(data=pirates,aes(sex)) +
  geom_bar(aes(fill=headband),position="fill") +
  labs(title = "How many of each sex are in this pirate crew?") +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12))
myplot
#^percentage of each sex with headband yes or headband no

#26: geom_col
avg_beard_by_sex <- pirates %>%
  group_by(sex) %>%
    summarize(average = mean(beard.length), stdev = sd(beard.length))
View(avg_beard_by_sex)

beard_vs_sex_column <- ggplot(data=avg_beard_by_sex,aes(x=sex,y=average)) +
  geom_col() +
  labs(title = "Average beard length by sex")
beard_vs_sex_column

#27: adding error bar
beard_vs_sex_column <- ggplot(data=avg_beard_by_sex,aes(x=sex,y=average)) +
  geom_col() +
  labs(title = "Average beard length by sex") +
  geom_errorbar()
beard_vs_sex_column