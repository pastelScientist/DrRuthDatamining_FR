#Pirates Fall 2021 -- Iteration 2 for Fawn-Rose!


##Working Directory
getwd()
#can change wd using Files tab on bottom right
#navigate to desired wd, then "More" --> "Set as Working Directory"
setwd("C:/Users/Fawn-Rose/Documents/20XX College/Urban Ecosystems/DrRuthDatamining_FR/Fall 2021")
#run setwd every day I work on Pirates
#Saving: save R scripts but not workspace or environment


##Packages, Libraries and Databases
library(yarrr)
library(dplyr)
library(ggplot2)

#must re-run setwd and library commands each day
#don't re-install packages, just load the libraries each day

head(pirates) #column headings + first 6 rows for all columns

names(pirates) #names of columns ONLY

str(pirates) #how many rows (obs.) and columns (variables), datatype for each

View(pirates) #contains entire dataset for us to view


##Descriptive Statistics

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


##Data Plotting: Scatter Plot

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


##Data Plotting: Bar Graphs

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
  geom_col(fill="#bf5700") +
  labs(title = "Average beard length by sex") +
  geom_errorbar(data=avg_beard_by_sex, aes(ymin=average-stdev, ymax=average+stdev), width = 0.2)
beard_vs_sex_column


##Data Plotting: Box Plot
sex_vs_age_boxplot <- ggplot(data=pirates, aes(x=sex,y=age, fill = headband)) +
  geom_boxplot() +
  facet_wrap(~headband)
sex_vs_age_boxplot
#29: fill=headband made 6 boxplots rather than 3, 2 for each sex that has them sorted by headband status
#30: facet_wrap(~headband) makes 2 different figures, 1 for no and 1 for yes headband. with all 6 boxplot

##Data Plotting: Violin Plot
#31:
violin_sword_age <- pirateplot(formula=age~sword.type,data=pirates,main="Pirateplot of ages by favorite sword")
violin_sword_age
#pirateplot: formula= y-axis ~ x-axis, main is title

#32:
violin_height_sex <- pirateplot(formula=height~sex,data=pirates, 
                                main = "Height of each sex on pirate ship",
                                pal="pony",
                                theme=3)
violin_height_sex

#33:
piratepal(palette="pony",plot.result=TRUE)

piratepal(palette="all",plot.result=TRUE)

#this is so cute!!

##Hypothesis Testing: Histograms

#35:
avg_age_by_headband <- pirates %>%
                          group_by(headband) %>%
                            summarize(avg_age = mean(age))

View(avg_age_by_headband)

#36: dataframe with only pirates who don't wear headbands
no_headband <- pirates %>%
  group_by(headband) %>%
    filter(headband == "no")
View(no_headband)

#37:
head(no_headband,n=10)

#38:
no_headband_shorter <- pirates %>%
  group_by(headband) %>%
    filter(headband == "no") %>%
      select(headband,age)
View(no_headband_shorter)

#39:
yes_headband_shorter <- pirates %>%
  group_by(headband) %>%
  filter(headband == "yes") %>%
  select(headband,age)
View(yes_headband_shorter)

#40:
yes_headband_hist <- ggplot(data=yes_headband_shorter, aes(x=age)) +
  geom_histogram(color="black", fill = "#bf5700") +
  theme_classic() +
  labs(title = "Age distribution of Pirates who Wear Headbands") +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12))

yes_headband_hist
#geom_histogram: color argument makes outline of each bar a certain color

#41: binwidth
yes_headband_hist <- ggplot(data=yes_headband_shorter, aes(x=age)) +
  geom_histogram(color="black", fill = "#bf5700", binwidth=5) +
  theme_classic() +
  labs(title = "Age distribution of Pirates who Wear Headbands") +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12))

yes_headband_hist

#42: dashed blue vert line indicating mean age
yes_headband_hist <- ggplot(data=yes_headband_shorter, aes(x=age)) +
  geom_histogram(color="black", fill = "#bf5700", binwidth=5) +
  theme_classic() +
  labs(title = "Age distribution of Pirates who Wear Headbands") +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12)) +
  geom_vline(aes(xintercept=mean(age)),
             color="blue", linetype="dashed", size=1)

yes_headband_hist

#43: no_headband hist
no_headband_hist <- ggplot(data=no_headband_shorter, aes(x=age)) +
  geom_histogram(color="black", fill = "#bf5700", binwidth=5) +
  theme_classic() +
  labs(title = "Age distribution of Pirates who Don't Wear Headbands") +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12)) +
  geom_vline(aes(xintercept=mean(age)),
             color="blue", linetype="dashed", size=1)

no_headband_hist

#44: plotting both groups from the original pirates database
age_headband_summary_hist <- ggplot(data=pirates, aes(x=age, fill=headband, color=headband)) +
  geom_histogram()
age_headband_summary_hist

#45: superimposing histograms
age_headband_summary_hist <- ggplot(data=pirates, aes(x=age, fill=headband, color=headband)) +
  geom_histogram(position="identity", alpha=0.5)
age_headband_summary_hist

#46: separate the histograms from one another 
age_headband_summary_hist <- ggplot(data=pirates, aes(x=age)) +
  geom_histogram(fill="white", color="black") +
  facet_grid(. ~headband) + 
  geom_vline(aes(xintercept=mean(age)),
             color="blue", linetype="dashed", size=1)
age_headband_summary_hist

#48: boxplots comparing same two
age_vs_headbands_boxplot <- ggplot(data=pirates, aes(x=headband, y = age)) +
  geom_boxplot() 
age_vs_headbands_boxplot

#could make these plots look pretty with themes and labs but ehhh

##Hypothesis Testing: Statistical tests

#running student's t-test on headband vs age data

#50:
age_headband.htest <- t.test(no_headband_shorter$age,yes_headband_shorter$age,paired=FALSE,alternative='two.sided')

age_headband.htest$p.value

#t-test argument: datasets, default is not paired and two-tailed test

#51: same test but using pirates dataframe rather than 2 sep ones
age_headband.htest <- t.test(formula = age~headband, data=pirates)

age_headband.htest$p.value

#52: same thing but using piping

age_headband.htest <- pirates %>%
    t.test(formula= age~headband, data = .)
age_headband.htest$p.value

#data = . is needed because t.test function is from BaseR whereas piping is dyplr

#53:
age_headband.htest

#54: correlation of height vs weight

cor.test(formula = ~ height + weight, data = pirates)

#55: ANOVA for tattoos (numeric) vs swords (four types)

tattoos_vs_sword_type <- ggplot(data=pirates, aes(x=sword.type, y = tattoos)) +
  geom_boxplot()
tattoos_vs_sword_type

res.aov <- aov(tattoos ~ sword.type,pirates)
summary(res.aov)
#shows stat diff between at least 2 groups -- time for post-hoc! but lol not until RSQA

<<<<<<< HEAD
##Some programming practice

no_headband[1,1]
#^ calls row 1, column 1 data
no_headband[1:3,4:8]
#^calls rows 1 through 3, with data from columns 4 through 8
no_headband[3:1,c(12,11,16)]
#^calls rows 1 through 3, with data from columns 12, 11, and 16 (in that order)

#59:
View(pirates)
sword.time_data <- pirates[,14]

mean(sword.time_data)

length(sword.time_data)

sum(sword.time_data)

swordresults <- c(mean(sword.time_data), length(sword.time_data), sum(sword.time_data))

sample(swordresults,1)

#60:
#making cointoss vector:
cointoss <- c(0,1)

sample(cointoss,1)

#making if statement to print tails if cointoss = 1, else printing heads
if(sample(cointoss,1) == 1){
  print("TAILS Woohoo!")
  } else{
  print("Ya got heads")
  }

#creating for loop to automate coin tosses (5 coin tosses)
for(i in 1:5){
  
  if(sample(cointoss,1) == 1){
    print("TAILS Woohoo!")
  } else{
    print("Ya got heads")
  }
  
}

#make a counter to add up how many times to get a tail (I also printed the count each time)
counter = 0
for(i in 1:5){
  
  if(sample(cointoss,1) == 1){
    print("TAILS Woohoo!")
    counter <- counter + 1
  } else{
    print("Ya got heads")
  }
  print(counter)
}

#Custom function: argument is tossing a coin any number of times, result is how many tails
tailCounter <-function(howmany){
  counter = 0
  for(i in 1:howmany){
    
    if(sample(cointoss,1) == 1){
      counter <- counter + 1
    }
  }
  return(counter)
}

tailCounter(15)

#5000 experiments, each one tossing a coin 40 times
experiment_data <- rep(NA, 5000)

for(i in 1:5000){
  
  experiment_data[i] <- tailCounter(40)
  
}

#histogram of this experiment

coinexpdf <- as.data.frame(experiment_data)

coinexperiment_results <- ggplot(data=coinexpdf, aes(x=experiment_data)) +
  geom_histogram(color="black", fill = "#bf5700", binwidth = 2) +
  theme_classic() +
  labs(title = "Coin flipping experiment results",x = "Number of tails per 40 coins flipped") +
  theme(plot.title = element_text(hjust=0.5, size=14), axis.title = element_text(size=12))

coinexperiment_results

