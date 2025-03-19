
#############
# GENERALISED LINEAR MODEL (count data)
# poisson, quasipoisson, negative binomial
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


#read in our dataset
#abundance data of longnose dace (Rhinichthys cataractae) 
#and in-stream variables collected from the Maryland Biological Stream Survey.
dace<-read.csv("data/longnosedace.csv")
head(dace)

#some basic plots, what patterns can we see?
plot(dace$do2, dace$longnosedace)
plot(dace$acreage, dace$longnosedace)
plot(dace$so4, dace$longnosedace)
#difficult to tell at this scale, but a positive correlation between acreage and number?

#check response variable distribution
hist(dace$longnosedace, breaks=20)
#looks like a poisson distribution, but quite uneven.
#we can try a poisson distribution, but we should probably plan a backup plan

#in the meantime, let's try a normal linear model
#this is mostly to provide an example of what a BADLY fitting model looks like
lmdace <- lm(longnosedace ~ acreage + do2 + maxdepth + no3 + so4 + temp,
             data = dace)
plot(lmdace)
#the QQplot has issues with higher values. model breaks down at higher values of x

#try a few other plots 
check_model(lmdace, check = c("linearity", "homogeneity", "qq", "normality"))
#it's not great! Bad fit in many places

#let's try a different distribution


#############
### POISSON/QUASIPOISSON/NEGATIVE BINOMIAL EXAMPLE
#############

#what are the assumptions of a Poisson distribution?
# Poisson Response: The response variable is a count per unit of time or space, described by a Poisson distribution.
# Independence: The observations must be independent of one another.
# Mean=Variance: By definition, the mean of a Poisson random variable must be equal to its variance.
# Linearity: The log of the mean rate, log(λ), must be a linear function of x.


### POISSON
#so let's try a Poisson distribution to see if it improves anything compared to our linear model
glmPdace <- glm(longnosedace ~ acreage + do2 + maxdepth + no3 + so4 + temp, 
                data = dace, family = poisson(link="log"))
plot(glmPdace)
#looks bad!

#try some other ways to look at these
residualPlots(glmPdace)
simulateResiduals(glmPdace, plot = TRUE)

#check for overdispersion
check_overdispersion(glmPdace)
#also bad!

#we could manually calculate overdispersion from residual deviance and DF
summary(glmPdace)
1590.0/ 6
#awful!
#what are our other options?

### QUASIPOISSON
#Poisson distribution did not work well (as is often the case)
#so let's try a backup option: a quasipoisson
#this has two parameters and can account for overdispersion (to some extent)
#maybe it will do better than the very inflexible poisson?

glmQdace<-glm(longnosedace ~ acreage + do2 + maxdepth + no3 + so4 + temp, 
              data = dace, family = quasipoisson(link="log")) 
plot(glmQdace)
#better! Some issues at high values of x

#try some other ways to look at these
residualPlots(glmQdace)
check_model(glmQdace, check = c("linearity", "homogeneity", "qq", "normality"))

#this is a lot better!
#but let's look at overdispersion
#Quasipoisson is a lot more flexible than poisson, but it has limits
#if dispersion ratio is over 15 we might want to consider alternatives
#again, before I ran the model I looked up assumptions of quasipoisson distribution, I didn't magically know this!


#check for overdispersion
check_overdispersion(glmQdace)
#it's 29.4 Bad news
#double check of residual deviance over df is 1590.0/61 (26.1). So not good.

#conclusion: this model is functional but has some issues, we might want to try an alternative

summary(glmQdace)


### NEGATIVE BINOMIAL
#Negative binomial is a more flexible choice with more parameters.
#it has a built-in dispersion parameter which can scale independent of the mean
#When θ (dispersion) is large, the Negative Binomial model will converge to a Poisson with variance equal to the mean.

#let's see if it does better than the quasibinomial
#note: we have to use the MASS package to do this
glmNBdace <- MASS::glm.nb(longnosedace ~ acreage + do2 + maxdepth + no3 + so4 + temp, 
                          data = dace)
plot(glmNBdace)
#looks great so far, let's look at some other plots
residualPlot(glmNBdace, main = "Negative Binomial model")
residualPlots(glmNBdace, plot = T)
simulateResiduals(glmNBdace, plot = TRUE)

#this looks good!
#as a final step we can ask if the model with the negative binomial distribution is better than the poisson model
#this can only be done because we used exactly the same data
#quasipoisson cannot be compared easily, so we ignore for now
AIC(glmPdace, glmNBdace)
#AIC is much lower for NB, so it's working great

#now we have a good model, let's see what it says
summary(glmNBdace)

#acreage, do2, maxdepth, no3 and temp are all significant
#but the estimates vary a lot

summary(dace)
#acreage: for every unit of acreage, number of dace increases by 0.00004651
#do2: for every unit of do2, number of dace increases by 0.03419
#maxdepth: for every unit of maxdepth, number of dace increases by 0.009538
#no3: for every unit of no3, number of dace increases by 0.2072
#temp: for every unit of temp, number of dace increases by 0.09460

#this is a bit difficult to interpret, because they are all on different scales!
#so what if we scale and center and now compare?

dace$acreage_s<-scale(dace$acreage, center = TRUE, scale = TRUE)
dace$do2_s<-scale(dace$do2, center = TRUE, scale = TRUE)
dace$maxdepth_s<-scale(dace$maxdepth, center = TRUE, scale = TRUE)
dace$no3_s<-scale(dace$no3, center = TRUE, scale = TRUE)
dace$temp_s<-scale(dace$temp, center = TRUE, scale = TRUE)
dace$so4_s<-scale(dace$so4, center = TRUE, scale = TRUE)

glmNBdace_s <- MASS::glm.nb(longnosedace ~ acreage_s + do2_s + maxdepth_s + no3_s + so4_s + temp_s, 
                          data = dace)
#plot(glmNBdace_s)

summary(glmNBdace_s)

#now we can compare with everything on the same scale
#each unit of variance for all significant variables results in a similar increase in abundance
#least important: maxdepth (0.27951)
#most important: no3 (0.37751)
#remember this is on a link function scale, so we can back transform to get a better idea on what this means
#NOTE: remember this is a curve so our back transform is a ball park figure
exp(0.27951)
exp(0.37751)


#let's make an example plot of no3 versus abundance
#make a predicted curve
summary(dace)
#make a dummy dataset to build our line of best fit from
#allow our variable of interest to vary, set everything else to median as a control
newdat <- data.frame(no3 = seq(0, 8, length = 500), 
                     acreage=median(dace$acreage),
                     do2=median(dace$do2),
                     maxdepth=median(dace$maxdepth),
                     so4=median(dace$so4),
                     temp=median(dace$temp)
                     )
dy <- predict(glmNBdace, type = "resp", se.fit = T,newdata = newdat)

#We can predict standard errors and confidence intervals on the response scale
#BUT this introduces errors, it's better to calculate on link scale and back transform
# Predictions link scale
dy2<-predict(glmNBdace, type = "link", se.fit = T, newdata = newdat)
lcl.l <- exp(dy2$fit + 1.96 * dy2$se.fit)
ucl.l <- exp(dy2$fit - 1.96 * dy2$se.fit)
pe.l <- exp(dy2$fit)

# Combine and plot
newdat <- cbind(newdat, dy = dy$fit, dy2 = dy2$fit, lcl.l, ucl.l, pe.l)
ggplot(newdat, aes(no3, dy)) + geom_line() +
  geom_line(aes(no3, lcl.l), lty = 2, col = "blue") +
  geom_line(aes(no3, ucl.l), lty = 2, col = "blue") +
  geom_point(data=dace, aes(no3, longnosedace))+
  xlab("no3") + ylab("abundance")

#it may look like the line doesn't go through the "middle" of the points
#but this makes sense if we think about it!
#the model is fitted on the log scale, so the line fits the logged response
#we can visualise this!

ggplot(newdat, aes(no3, dy2)) + geom_line() +
  geom_point(data=dace, aes(no3, log(longnosedace)))+
  xlab("no3") + ylab("logged abundance")
#the line fits the logged data well


##############################################################
### EXERCISE GLM!
##############################################################


#the number of trees of the species Faramea occidentalis was measured in 43 quadrants 
#in Barro Colorado Island in Panama. For each quadrant, environmental characteristics 
#were also recorded such as elevation or precipitation
fara<-read.csv("data/faramea.csv")
head(fara)
#variables of interest:
#Faramea.occidentalis: count of F. occidentalis in a given quadrant
#Precipitation: precipitation per year (mm/m^2) in quadrat
#Elevation: elevation above sea level (m)


##TASK: build a GLM with a count response, 
#investigate whether there is a correlation between precipitation+elevation and abundance of F. occidentalis
#look at the response variable and choose whichever distribution you think fits best


#Remember the steps to follow
# What are your goals? To describe? To predict? To explain? What do you expect to find?
# Look at your data, what type is it? Categorical? Continuous? Binomial?
# What distribution does your data have? 
# Pick a model (or make a shortlist) (SPOILER: it's a count-based model)
# What assumptions does the chosen model have? Does your data meet them?
# Run the model
# Check the model fits properly

# Make conclusions on what the model tells you. What is significant, what is not? What are the effect sizes?
# Make at least one output plot showing what you think is the most interesting conclusion



