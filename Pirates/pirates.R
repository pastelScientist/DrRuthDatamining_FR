# My super cool Pirates Worksheet :D

#Working Directory


setwd("~/20XX College Files/Urban Ecosystems/DrRuthDatamining_FR")
getwd()

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
#remember that a Boxplot shows categorical data split up by some kind of numeric data!
#Can't contain 2 quantitative variables

ageByFakeGender <- ggplot(data=pirates, aes(x=sex,y=age, fill=headband)) +
    geom_boxplot() +
    facet_wrap(~headband)

ageByFakeGender

favePixarMovieByAge <- ggplot(data=pirates, aes(x=age, y=fav.pixar)) +
      geom_boxplot()

favePixarMovieByAge

tattooCountBySwordType <- ggplot(data=pirates, aes(x=sword.type, y = tattoos)) +
    geom_boxplot()

tattooCountBySwordType

#Data Plotting: Violin plot

#note: pirateplot is specific to the pirate library

rangeOfAgeBySword <- pirateplot(formula=age~sword.type, data=pirates, main="Pirateplot of ages by favorite sword")

rangeOfAgeBySword

#31

heightRangeBySex <- pirateplot(formula=height~sex, data=pirates, pal="pony", theme = 3)

#32

piratepal(pal="pony", plot.result=TRUE)

piratepal(pal="all", plot.result=TRUE)


#Hypothesis testing: Histograms

#34 - Grouping age by headband usage and then getting the mean
avgHeadbandsAge <- pirates %>%
    group_by(headband) %>%
    summarize(averageAges = mean(age))
    
avgHeadbandsAge

#35 - dataframe with only pirates with no headband
no_headband <- pirates %>%
  group_by(headband) %>%
  filter(headband == "no")

head(no_headband, n=10)

#36 - dataframe with just no headband wearers and their ages
no_headband_shorter <- pirates %>%
  group_by(headband) %>%
  filter(headband == "no") %>%
  select(age)

head(no_headband_shorter, n=10)

#37

yes_headband_shorter <- pirates %>%
  group_by(headband) %>%
  filter(headband == "yes") %>%
  select(age)

head(yes_headband_shorter, n=10)

#38, 39, 40
yes_headband_histogram <- ggplot(data=yes_headband_shorter, aes(x=age)) +
    geom_histogram(color="black", fill = "#bf5700", binwidth=5) +
    labs(title = "Headband-Wearing Pirates' Distribution of Age") +
    theme_classic() +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12)) +
  #the line above MAKES TITLES GO TO THE MIDDLE OF THE PLOT! horizontal adjustment of element text! and changes font size
  geom_vline(aes(xintercept=mean(age)), color="blue", linetype="dashed",size=1)  

yes_headband_histogram

#41
no_headband_histogram <- ggplot(data=no_headband_shorter, aes(x=age)) +
  geom_histogram(color="black", fill = "#bf5700", binwidth=5) +
  labs(title = "No Headbanded Pirates' Distribution of Age") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12)) +
  #the line above MAKES TITLES GO TO THE MIDDLE OF THE PLOT! horizontal adjustment of element text! and changes font size
  geom_vline(aes(xintercept=mean(age)), color="blue", linetype="dashed",size=1)  

no_headband_histogram

#42, 43 added alpha = 0.5, position="identity" to superimpose
final_headband_hists <- ggplot(data=pirates, aes(x = age, color=headband, fill = headband)) +
  geom_histogram(binwidth=5, alpha = 0.5, position="identity") +
  labs(title = "Pirates' Headband Distribution by Age") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12)) +
  #the line above MAKES TITLES GO TO THE MIDDLE OF THE PLOT! horizontal adjustment of element text! and changes font size
  geom_vline(aes(xintercept=mean(age)), color="blue", linetype="dashed",size=1)  

final_headband_hists

#44 (adjustments to 43)
final_headband_hists <- ggplot(data=pirates, aes(x = age)) +
  geom_histogram(binwidth=5, alpha = 0.5, color="black", fill="white") +
  labs(title = "Pirates' Headband Distribution by Age") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12)) +
  #the line above MAKES TITLES GO TO THE MIDDLE OF THE PLOT! horizontal adjustment of element text! and changes font size
  geom_vline(aes(xintercept=mean(age)), color="blue", linetype="dashed",size=1) +
  facet_grid(.~headband)
#the above line is actually what separates by age rather than only being grouped by age
#.~ in front means display horizontally side by side and ~. in back means display vertically

final_headband_hists

#45
headband_comp_boxplot <- ggplot(data=pirates, aes(x=headband, y = age)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Pirates' age distribution by headband usage") +
  theme(plot.title = element_text(hjust=0.5, size = 14), axis.title= element_text(size=12))

headband_comp_boxplot

#Statistical tests section

age_headband.htest <- t.test(no_headband_shorter$age, yes_headband_shorter$age, paired = FALSE, alternative='two.sided')

age_headband.htest$p.value

#htest is a hypothesis test object that holds all statistical results from a hypothesis test

big_data_headband.htest <- t.test(formula = age ~ headband, data = pirates)

big_data_headband.htest$p.value

#for using one dataset for a t-test, formula = y (indep) ~ x (dep) is what you use

age_headband.htest

#48 correlation without assigning function to a variable
cor.test(formula = ~height + weight, data = pirates)

#50
#Matrices of data can be used to pull out specific values
#[ ] used to define range of values or concatenate c() to select specific values
#[row index,column index]

no_headband[1,1]

no_headband[1:3, 4:8]

no_headband[3:1,c(12,11,16)]
#reordering 1:3 vs 3:1 displays rows in forward and reverse order

#51
new_sword_time <- no_headband[1:180, 14]

new_sword_time

mean_st <- new_sword_time %>%
    summarize(mean = mean(sword.time, na.rm = TRUE))

mean_st

length_st <- new_sword_time %>%
    summarize(lengthof = length(sword.time, na.rm=TRUE))

length_st

sum_st <- new_sword_time %>%
    summarize(sum = sum(sword.time, na.rm = TRUE))

sum_st

results1 <- c(mean_st, length_st, sum_st)

results1

sample(results1, 1)
#sample randomly samples from a dataset! holy shit! love that for them!

#52 -- if statements and loops

coin <- data.frame(head = "H", tail = "T")

coin

result <- sample(coin, 1)

if(result == "T"){
  print("TAILS Woohoo!")
}

count = 0

for(i in 1:4){
  
  result <- sample(coin, 1)
  
  if(result == "T"){
    count = count + 1
    print("TAILS Woohoo!")
  }
  if(i == 4){
    message1 = "We got"
    message2 = "tails"
    print(c(message1, count, message2))
  }
}

#creating function

coin.toss.time <- function(how_many) {
  
  count = 0
  
  for(i in 1:how_many){
    
    result <- sample(coin, 1)
    
    if(result == "T"){
      count = count + 1
    }
  }
  
  return(count)
}

coin.toss.time(8)

#5000 experiments where each experiment tosses a coin 40 times

means_for_hist <- rep(NA, 5000)

for(i in 1:5000){
  tail_count_i = coin.toss.time(40)
  means_for_hist[i] <- tail_count_i
}

experiment_data <- data.frame(tail_counts = means_for_hist)

head(experiment_data)

experiment_histogram <- ggplot(data=experiment_data, aes(x=tail_counts)) +
  geom_histogram(binwidth = 5,color="black", fill = "#bf5700") +
  theme_classic() +
  labs(title = "40 Coin Toss' Tail Counts for 5000 Experiments") +
  xlab("How Many Were Tails? (of 40 coin tosses)")
  theme(plot.title = element_text(hjust=0.5, size = 14), axis.title= element_text(size=12))

experiment_histogram
