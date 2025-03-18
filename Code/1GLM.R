
#############
# LINEAR MODEL
# Generalised Linear Model with a Gaussian Distribution
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

#load libraries
source("./Code/0Packages.R")
theme_set(theme_bw()) # set black and white background



#############
### LOAD AND EXPLORE DATA
#############

df1<-read.delim("Data/SkyWhales_2024.txt", sep=" ")

#check data has loaded properly
# View(df1)
head(df1) #does this look as we expect?
dim(df1) #dimensions of data

#check summary, what is numeric, what is categorical? Any count data?
summary(df1)
#spotted an issue: site should be categorical (i.e. factor)
df1$site<-as.factor(df1$site)

#plot an example set of possibly correlated variables
#do sky whales get larger linearly as they age?
ggplot(df1, aes(age, length_m)) +
  geom_point() +
  xlab("Estimated age (years)") +
  ylab("Whale length (m)")
#some signs of positive correlation, but a lot of variability in the data


#CHECK THE DISTRIBUTION OF THE RESPONSE
hist(df1$length_m)
#nearly normal!
#so we will use a model based around a normal distribution

#we will do the predictor as well, this doesn't need to be normal, but we will check for huge skew or outliers
hist(df1$age)
#looks ok so far



#############
### RUN A LINEAR REGRESSION
#aka a Generalised Linear Model based on a Gaussian (normal) distribution
#############

#let's do a very basic investigation first
lm.age <- lm(length_m ~ age, data = df1)

lm.age

#at a very basic level this tells us the overall coefficients
#y= ax+b+ϵ
#y=0.288x+29.95
#so if we want to predict how long on average an whale of 40 years old
#0.288*40+29.95 = 41.47m

#remember:
#The intercept = the average value of Y when all predictors are set equal to 0 (E[Y|X=0]).
#The slope = predicted change in Y per unit increase in X

#so there are some signs of an interesting correlation.

#let's build a proper model now.

### 1) what are our predictive variables and what type of data are they?: 
  # age (continuous), magic_thaums (continuous), location (categorical)

### 2) do any of these correlate together?
#check with a spearman correlation
#check for correlations
cor(df1$age, df1$magic_thaums) #low, >0.5 is a warning, >0.7 is a big problem
#checking for lack of cross factoring with categorical is tricky, but boxplot is a useful way to check, should ideally be overlapping to some degree
boxplot(df1$age~df1$location)
boxplot(df1$magic_thaums~df1$location)
#all looks good to me


### 3) OPTIONAL: scale the parameters (optional)
#we have two continuous paramters on very difficult scales
#it is difficult to compare effect sizes when parameters are on completely different scales, so we can (if we choose) standardise
#everything will have mean of 0 and SD of 1.
#this is not always necessary, and makes interpretation more difficult
#unscaled data is easier to intuit how x correlates with y
#scaled data is easier to intuit whether x1 has a greater effect than x2
df1$age_s<-scale(df1$age, center = TRUE, scale = TRUE)
df1$magic_thaums_s<-scale(df1$magic_thaums, center = TRUE, scale = TRUE)
mean(df1$age_s); sd(df1$age_s)


### 4) run the model
#our full model is Whale Length ~ age + environmental background magic + location
#I have chosen to use unscaled variables here but we could always change this later
lm1 <- lm(length_m ~ age+magic_thaums+location, data = df1)



### 5) check the model fit and residuals!
par(mfrow=c(2,2)); plot(lm1)

#they look good! But to be sure, we can have a look at some more diagnostics
par(mfrow=c(1,1)); 
car::qqPlot(lm1, id=FALSE)
#definitely a good fit! The residuals all fall within an expected random distribution (blue shading)

#there are many ways to visualise residuals and other diagnostics of model fit
#they all show the same thing but in different ways
resid_panel(lm1)
# to create the multi-panel plot
dplots <- sjPlot::plot_model(lm1, type="diag")
((dplots[[1]] + dplots[[2]])/ dplots[[3]]) 

#and yet another way!
check_model(lm1, check = c("linearity", "homogeneity", "qq", "normality"))
#a minor issue with normality of residuals, but not substantial

### 6) look at model output
#I say the model passes! A good set of normally distributed residuals with no major issues

summary(lm1)

#Interpretation:
#There is a significant difference between Location W and Location E
#Location West (on average) has whales which are 0.727m shorter
#average in W is 6.768m, average in E is 6.042 (6.76829-0.72677)

#Age positively, and significantly, correlates with whale length. 
#for every year of age, whale length increases by 0.11m

#Background magic correlates with whale length
#for every Thaum of magic, whale length increases by 0.06

#in addition we can generate confidence intervals around our parameters
#in frequentist statistics we draw from a "sampling distribution" to estimate how confident we can be
#this is dependent on the sample size, the degrees of freedom, and a number of other factors
#in practical terms it states that we can be 95% confidence our "true" parameter estimates list within these two figures
confint(lm1)
#note that while the lower and upper founds are very similar for magic, they are larger for age, and very large for location!


### 7) build predictions and start plotting

#we can make predictions to tell us what to expect (on average)
#if we found a whale and we know its age and location, but not its length, could we predict it?
newdata <- data.frame(age = 60, magic_thaums=462, location =c("E", "W"))
predict(lm1, newdata, interval = "prediction", level = 0.95)

#interpretation: a 60y old whale at a 462 background Thaum in Site E would be 40.82m in length on average
#a 60y old whale at a 462 background Thaum in Site W would be 40.10m in length on average

#let's make some general predictions and plot them!
predage=seq(min(df1$age), max(df1$age), length=100)
predmag=seq(min(df1$magic_thaums), max(df1$magic_thaums), length=100)
predloc=c("W", "E")

#plot correlation between age and whale length, including confidence and prediction intervals
#for simplicity, we will only plot for location W
newdata1 <- data.frame(age = predage, magic_thaums=median(df1$magic_thaums), location="W")
predict.mean <- cbind(newdata1, predict(lm1, newdata1, interval = "confidence"))
predict.ind <- cbind(newdata1, predict(lm1, newdata1, interval = "prediction"))
ggplot(df1, aes(age, length_m)) +geom_point() + 
  geom_line(data=predict.mean, aes(age, lwr), color="red", linetype = "dashed") +
  geom_line(data=predict.mean, aes(age, upr), color="red", linetype = "dashed") +
  geom_line(data=predict.ind, aes(age, lwr), color="black", linetype = "dashed") +
  geom_line(data=predict.ind, aes(age, upr), color="black", linetype = "dashed") +
  geom_line(data = predict.mean, aes(age, fit)) +
  xlab("Age") + ylab("Length (m)")

#SIDE NOTE: prediction and confidence intervals are different concepts
#confidence intervals: how confident are we that the true mean lies within this margin?
#prediction intervals: if we picked a random interval how confident are we that they would fall within this margin?

#they have different applications, in general confidence intervals are more useful for model interpretation.



##############################################################
### EXERCISE GLM!
##############################################################

#this is a dataset of wild birds (of a single species) confiscated from an illegal live market
#we are interested in what sets the market value of these birds

bdm<-read.csv("data/BirdMarket_lm.csv")
#weight: weight of birds in grammes
#primary length: length of primary feather (prized in decorations) in cm
#colour: colour intensity as an ordinal scale, 1 is the brightest plumage, 8 is the dullest
#value: quoted price of bird on market ($)

#TASK: build a linear model, investigating what variables correlate with bird value

#Remember the steps to follow
# What are your goals? To describe? To predict? To explain? What do you expect to find?
# Look at your data, what type is it? Categorical? Continuous? Binomial?
# What distribution does your data have? 
# Pick a model (or make a shortlist) (SPOILER: it's a linear model)
# What assumptions does the chosen model have? Does your data meet them?
# Run the model
# Check the model fits properly
# Make conclusions on what the model tells you. What is significant, what is not? What are the effect sizes?
# Make at least one output plot showing what you think is the most interesting conclusion

