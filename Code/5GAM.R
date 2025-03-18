
#############
# GENERALISED ADDITIVE MODEL
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

#load packages
source("./code/0Packages.R")


#############
### LOAD AND EXPLORE DATA
#############

#read in data
gala<-read.csv("data/gala.csv")

#we are interested in how area predicts species diversity, what is the relationship?
plot(gala$Area, gala$Species)
#obviously non-linear! This cannot be modelled easily, due to the high outliers

#from previous experience, I know that the logArea is more likely to linearly correlate with diversity
#what does that look like?
gala$logarea<-log10(gala$Area)
plot(gala$logarea, gala$Species)
#still not linear! An obvious curve

#check distribution of response data
hist(gala$Species, breaks=20)
#not normally distributed. Could be a negative binomial?


#############
### GAMS (and comparisons)
#############

#let's try a linear model first
lin1<-lm(Species ~ logarea, data = gala)
plot(lin1)
#alt:
resid_panel(lin1)
#not great! But actually not terrible, let's make a quick plot


ggplot(gala, aes(x = logarea, y = Species)) + geom_point() +
  stat_smooth(method = "lm",  se = TRUE) + 
  xlab("Area (logged km^2)") +
  ylab("Species Diversity") +
  ggtitle("Area (logged) vs species Diversity")+theme_classic()
#we can see (as we expected) that the straight is doing its best, but isn't fitting the obvious curve in the data
#a good reminder that plots are very very useful! Always plot results

#let's see if a negative binomial does better
#note: we have to use the MASS package to do this
glnb1 <- MASS::glm.nb(Species ~ logarea, 
                          data = gala)
plot(glnb1)
residualPlots(glnb1, plot = T)
#also pretty bad! What does the fit look like on a plot?

newdat <- data.frame(logarea = seq(-2, 4, length = 500))
dy <- predict(glnb1, type = "resp", se.fit = T,newdata = newdat)

newdat <- cbind(newdat, dy = dy$fit)
ggplot(newdat, aes(logarea, dy)) + geom_line() +
  geom_point(data=gala, aes(logarea, Species))+
  xlab("area (logged)") + ylab("species diversity")
#this actually looks pretty good
#but the poor residuals worry me. It implies that behind the scenes it is fitting the log scale poorly

#so let's try an alternative, let's try a GAM
sfit <- gam(Species ~ s(logarea), data = gala)

#a GAM (in some ways) is just like a linear model with an extra capability bolted in (flexible linearity)
#as such you still need to check residuals
par(mfrow=c(2,2))
gam.check(sfit)
par(mfrow=c(1,1))
#these are not good. on the top right we can see variance in residuals increase across the predictor
#and in the top left we see it is not really on the diagonal 1:1 line

#under the circumstances I probably prefer the negative binomial
#BUT, for the sake of argument let's see what the GAM says
#if only for illustrative purporse
plot(sfit, scheme=1)

#looks pretty similar to our NB GLM!

summary(sfit)
#the summary for a GAM is a little different to a GLM
#the edf is important, it informs you how many degrees of freedom were required to estimate the GAM curve
#the higher the number, the more complex the modelled relationship
#p-values for GAMs should be interpreted with EXTREME CAUTION
#"S. N. Wood (2017) suggests p-values are often too small and should be multiplied by a factor of 2"
#in general I would just plot the results and make informed conclusions off that

dy_gam <- predict(sfit, se.fit = T,newdata = newdat)
newdat_gam <- cbind(newdat, dy_gam = dy_gam$fit)

#let's compare our NB GLM and GAM results
ggplot(newdat, aes(logarea, dy)) + geom_line() +
  geom_line(data=newdat_gam, aes(logarea, dy_gam), col="blue") +
  geom_point(data=gala, aes(logarea, Species))+
  xlab("area (logged)") + ylab("species diversity")
#as we suspected, the negative binomial (black line) fits better



#############
### GAMS: more complex example
#############

#Let's try a more complex example

#The data set has been constructed using average Science scores by country from the 
#Programme for International Student Assessment (PISA) 2006, 
#along with GNI per capita (Purchasing Power Parity, 2005 dollars), 
#Educational Index, Health Index, and Human Development Index from UN data. 

#The key variables are as follows:
# Overall Science Score (average score for 15 year olds)
# Interest in science
# Identifying scientific Issues
# Explaining phenomena scientifically
# Support for scientific inquiry
# Income Index
# Health Index
# Education Index
# Human Development Index (composed of the Income index, Health Index, and Education Index)

pisa<-read.csv("data/pisasci2006.csv")
head(pisa)

#start with a basic linear model (using gam()). For comparison purposes
mod_lm = gam(Overall ~ Income, data = pisa)

#check residuals
par(mfrow=c(2,2))
gam.check(mod_lm)
par(mfrow=c(1,1))
#not great, some outliers. I would probably call this a bad fit

summary(mod_lm)

#let's try a non-linear GAM instead
#I specify a cubic spline here, to demonstrate how to, but we can normally let gam() pick for itself
mod_gam1 = gam(Overall ~ s(Income, bs = "cr"), data = pisa)
par(mfrow=c(2,2))
gam.check(mod_gam1)
par(mfrow=c(1,1))
#still some outliers but looks a bit better


summary(mod_gam1)
#what do we take from this?
#income has a significant effect on overall education grade
#we don't trust p-values much in a GAM, but this is so low we can be confident something is happening here
#the income parameter used 6.895 effective degrees of freedom (compared to 7.741 for the reference)
#these two combined determine the p value

#r-squared is provided, a high r-squared means a large amount of the data is explained by the model
#but beware, a very high r-squared may indicate overfitting!

#GCV is also useful, it's conceptually similar to AIC. Doesn't mean much by itself, but can be used to compare GAMs

#let's plot it
plot(mod_gam1)
#quite wiggly! Importantly, does this pattern make sense? Can we explain it?
#is it over-fitted?


#how much better was the GAM over the linear model?
summary(mod_lm)$sp.criterion
summary(mod_gam1)$sp.criterion
#GAM with non-linear element was much better!

#we can also compare models with an ANOVA
#again, be careful with p-values here, but it generally shows the non-linear GAM is better
anova(mod_lm, mod_gam1, test = "Chisq")


#what if we want to look at multiple predictors?
#this is more complicated but possible!
mod_gam2 = gam(Overall ~ s(Income) + s(Edu) + s(Health), data = pisa)
par(mfrow=c(2,2))
gam.check(mod_gam2)
par(mfrow=c(1,1))
#residuals look good!

summary(mod_gam2)
#health is non-significat, AND edf is at exactly 1
#The effective degrees of freedom with value 1 suggests that it has essentially been reduced to a simple linear effect.

#extracting indivdual effects and parameters from GAMs is extremely difficult
#instead we use "component" plots
plot(ggeffects::ggpredict(mod_gam2), facets = TRUE)
gratia::draw(mod_gam2)
#note that the health plot is basically staight!

#let's make a full plot as an example

newdat <- data.frame(Income = seq(0.4, 1, length = 500), 
                     Edu=median(pisa$Edu, na.rm=T),
                     Health=median(pisa$Health, na.rm=T)
)
dy <- predict(mod_gam2, se.fit = T,newdata = newdat)
lcl.l <- dy$fit + (1.96 * dy$se.fit)
ucl.l <- dy$fit - (1.96 * dy$se.fit)
newdat <- cbind(newdat, dy = dy$fit,  lcl.l, ucl.l)


ggplot(newdat, aes(Income, dy)) + geom_line() +
  geom_line(aes(Income, lcl.l), lty = 2, col = "blue") +
  geom_line(aes(Income, ucl.l), lty = 2, col = "blue") +
  geom_point(data=pisa, aes(Income, Overall))+
  xlab("Income") + ylab("Overall Science Scores")
#what do we take from this?
#income generally increase science score, but unevenly
#extrapolating past the main parts of the data causes strange results! So we shouldn't do it!

#one of the reasons GAMs are difficult to visualise is because they correlate in non-linear ways
#it can be helpful to plot in multiple dimensons
vis.gam(
  mod_gam2,
  type      = 'response',
  plot.type = 'persp',
  phi       = 30,
  theta     = 30,
  n.grid    = 500,
  border    = NA
)


##############################################################
### EXERCISE GAM!
##############################################################

#the following is a tracking dataset taken from a migrating European bee-eater
#times: the time (compared to 0 baseline)
#accel: acceleration in m/m/s
#sunlight: sunlight intensity (dependent on cloud cover and time of day)
track<-read.csv("data/tracking.csv")
head(track)

##TASK: build a GAM to investigate what might affect bird acceleration in flight
#what correlates? what doesn't? What does that relationship look like?


#Remember the steps to follow
# What are your goals? To describe? To predict? To explain? What do you expect to find?
# Look at your data, what type is it? Categorical? Continuous? Binomial?
# What distribution does your data have? 
# Pick a model (or make a shortlist) (SPOILER: it's a GAM)
# What assumptions does the chosen model have? Does your data meet them?
# Run the model
# Check the model fits properly

# Make conclusions on what the model tells you. What is significant, what is not? What are the effect sizes?
# Make at least one output plot showing what you think is the most interesting conclusion

