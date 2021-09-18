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

#17: 
