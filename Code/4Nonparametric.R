
#############
# GENERALISED LINEAR MODEL (binomial)
#############



#Clear your previous work
rm(list=ls())

#set your working directory
#I'm lazy so I autodetect where this file is and set it one level higher
#if this fails for any reason, set setwd manually to the folder path
curpath<-dirname(rstudioapi::getSourceEditorContext()$path)
curpath
setwd(curpath)
#go up a level out of the code folder for neatness
setwd("../")


#############
### EXAMPLE 1: wilcox.test
#############

#read in data
bb1<-read.csv("Data/Cd.BeetBarley.csv")

#these are statistics for crops processing cadmium in the soil through phytoremediation
#is redbeet more efficent than barley

boxplot(
  bb1$redbeet,
  bb1$barley,
  col = "lightgrey",
  main = "Phytoremediation Efficiency of Crop Plants",
  xlab = "Crop type",
  ylab = "Cadmium reduction (%)",
  names = c("Redbeet", "Barley"),
  ylim =  c(0, 25),
  las = 1,
  boxwex = 0.6
)
#looks pretty similar

#let's look at histograms
hist(bb1$redbeet)
hist(bb1$barley)
#definitely non-normal

#If these were normal this would be a straight-forward t-test
#but unfortunately that's not an option
#we could mess about with transformation, alternative distributions etc.
#OR we could just do a non-parametric test
#easier and more robust

#Either we look this up with google, or maybe we just know it
#the non-parametric equivalent to a t-test is a wilcox test

wilcox.test(bb1$redbeet, bb1$barley, exact = FALSE)

#p value is non-significant
#there is no difference between the two species


#############
### EXAMPLE 2: Kruskal-Wallis Test
#############

#this dataset shows chickweights under a series of different diets
chickwts<-read.csv("Data/chickwts.csv")
head(chickwts)

#plot the data!
boxplot(
  weight~feed,
  data=chickwts,
  col= "lightgray",
  main= "",
  xlab= "Feed type", 
  ylab= "Weight (g)", 
  ylim= c(100,450), 
  las= 1)
#some clear differences

#let's look at the histogram
hist(chickwts$weight)
#doesnt' look too bad, but some categories are very non-normal
hist(chickwts$weight[chickwts$feed=="casein"])

#let's try a normal linear regression first, 
#in this case because we only have one predictor, made of multiple categories, this is aka ANOVA
an1<-lm(weight~feed, data=chickwts)

#check the residuals!
plot(an1)
#not bad at all!

#However, if you are worried that this model doesn't fit, we can always compare it to a non-parametric model
#you should get around the same answer if the data are normal!

#the non-parametric equivalent to an ANOVA is a Kruskall-Wallis
#again, I would either find this by looking it up or by experience
kr1<-kruskal.test(weight ~ feed, data=chickwts)

#let's compare
summary(an1)
kr1

#linear regression is a lot more detailed, but they agree that the model shows an overall significant effect of feed!

#if we wanted to get a detailed pair-wise comparison using a non-parametric method, we can
#using a wilcox test. Remember to correct the p values for multiple testing!
pairwise.wilcox.test(chickwts$weight, chickwts$feed,
                     p.adjust.method = "BH")
#you will probably see a warning about p-values, this is normal as it's based on data ranks
#as normal we should interpret these values with care, and use our brains and plots to aid interpretation



##############################################################
### EXERCISE NON-PARAMETRIC TESTS!
##############################################################

#the following is a dataset of grouse populations, tracked across two years
grouse<-read.csv("data/grouse.csv")


##TASK: build a linear model exploring if year 2 correlates with the population in year 1. Does it work? Why? Why not?
#compare results with a non-parametric equivalent (spoiler: try a Spearman's correlation using the cor.test() function)


#Remember the steps to follow
# What are your goals? To describe? To predict? To explain? What do you expect to find?
# Look at your data, what type is it? Categorical? Continuous? Binomial?
# What distribution does your data have? 
# Pick a model (or make a shortlist) (SPOILER: it's a linear model, then a Spearman's correlation)
# What assumptions does the chosen model have? Does your data meet them?
# Run the model
# Check the model fits properly

# Make conclusions on what the model tells you. What is significant, what is not? What are the effect sizes?
# Make at least one output plot showing what you think is the most interesting conclusion

