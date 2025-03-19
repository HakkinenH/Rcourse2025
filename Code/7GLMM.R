
#############
# GENERALISED LINEAR MIXED MODEL
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

#read in the data
clutch<-read.csv("Data/clutch.csv")
head(clutch)

#this is complicated data
#In the late 90’s, the Minnesota Department of Natural Resources (MN DNR) conducted a study 
#to compare the cost-effectiveness of single- and double-cylinder mallard nesting structures 
#and to identify the best places to locate them in the landscape 

#To accomplish the project objectives, 110 nest structures (53 single-cylinder and 57 double-cylinder) 
#were placed in 104 wetlands (the largest eight wetlands included two structures each).

#the nests were inspected 4 or more times per year, and 139 nests had their clutch sizes counted

#so in this dataset we have multiple observations per structure, and we might expect this
#to predict occupancy and clutch size, more so than other variables
#so we need to account for this uneven, "noise" of unstructured repeated measures
#i.e. we are going to treat this as a random variable

#first tidy the data
# Get rid of observations with nest initiation dates > 30 May
# and a few outliers representing nests that were likely parasitized
clutch<-clutch[clutch$CLUTCH<13 & clutch$date<149,]
clutch$year<-as.factor(clutch$year)
clutch$Ideply<-ifelse(clutch$DEPLY==2, TRUE, FALSE)

#Ideply is the type of nest structure (TRUE = double sided, FALSE = single sided)
#date and year are also covariates in this case

#do a basic plot
ggplot(clutch, aes(date, CLUTCH, col=Ideply))+
  geom_point() + 
  geom_smooth(method= lm, formula = y ~ x) +
  facet_wrap(~year)
#hmm potentially a complicated relationship!
#double (true) structures get worse as the season goes on faster than single
#but does this pattern hold up?

#try a model!
#first double check the response variable
hist(clutch$CLUTCH)
#despite it being a count, this is actually quite close to normal!
#we will try a linear model first
clutch$strtno<-as.factor(clutch$strtno)
clutch.ri <- lmer(CLUTCH ~ date + Ideply + (1 | strtno), data=clutch, REML=T)
#in this model the structure ID is our random intercept.

clutch.fix <- lm(CLUTCH ~ date + Ideply, data=clutch)

#SIDE NOTE: REML, ML and
#REML stands for the Restricted Maximum Likelihood
# REML to compare models with nested random effects and the same fixed effect structure
# ML to compare models with nested fixed effects and the same random effect structure
# ML to compare models with and without random effects
#in other words if you want to build mixed models with different fixed effects, turn REML off!

#check residuals as usual!
#it should be noted though that residuals don't work in the same way as for non-mixed models
#for a start each residual is "fuzzy" based around the random effects
#in effect this often means we can be a little more lax about our residuals, but we still need to check them!
plot(clutch.ri)
#or
# Plot predicted values vs residual values
par(mar = c(4, 4, 0.5, 0.5))
plot(resid(clutch.ri) ~ fitted(clutch.ri), xlab = "Predicted values", ylab = "Normalized residuals")
abline(h = 0, lty = 2)
#pretty good!

#let's do more checks
# In order to check the independence of the model residuals
# we need to plot residuals vs each covariate of the model
par(mfrow = c(1, 3), mar = c(4, 4, 0.5, 0.5))
plot(resid(clutch.ri) ~ clutch$date, xlab = "date", ylab = "Normalized residuals")
abline(h = 0, lty = 2)

boxplot(resid(clutch.ri) ~ Ideply, data = clutch, xlab = "Ideply",
        ylab = "Normalized residuals")
abline(h = 0, lty = 2)

#this last one is for our random effect. It will likely be all over the place, but should averaged around at 0
boxplot(resid(clutch.ri) ~ strtno, data = clutch, xlab = "structure no.", ylab = "Normalized residuals")
abline(h = 0, lty = 2)
#these all look ok!

#check the normality of residuals
par(mfrow = c(1, 1))
hist(resid(clutch.ri))
#also ok!

#yet more checks!
check_model(clutch.ri)
#note the top left plot. The many lines show our many sites (random effects)
#still looks good

#now we look at the summary
summary(clutch.ri)

#what do we infer from this?
#there are no p-values! 
#They mean little in a GLMM, as there are no exact predictions, since random effects add noise to everything, 
#though you can estimate them with bootstrapping, we won't cover that here

#date has a small effect on success
#deployment type has a stronger negative effect

#we can find confidence intervals from this
#these are often more useful in GLMMs, we can infer whether there is a consistent effect and how likely that is
confint(clutch.ri, method="Wald")
#effects for date are consistently negative
#but for deployment are not! Some doubt on whether this is a consistent trend of not


#what about the random effects
#well the strtno explains:
#0.2532 of variance in the data, or 0.5032 as a standard deviation

#a random effect this means that any estimate we make will vary between individuals
#how much? Well 1 SD of each estimate (68% of the population) will be +/- 0.532, 2 SD (95% of population) will be 1.064
#so 95% of the population will have ~1 egg more or less than the average estimate

#an example:
#What is the predicted clutch size of a bird laying at date 110, if Ideply=F?
16.093267 + (-0.047134*110) #10.909
#and how much will this vary? by 1.064 (accounting for 95% of the population)
#so for 95% of sites they will have 9.909-11.909 (around 10% of the total variance in the population)

#That's quite a lot, but plenty left over for other effects



#let's try and visualise this

#make a plot showing the random effects

newdata<-data.frame(date=seq(90,145, length=500),
                  Ideply=rep(c(TRUE, FALSE), times=250),
                  strtno=50)

newdata <- with(clutch, expand.grid(date=unique(date), Ideply=unique(Ideply), strtno=unique(strtno)))

newdata_t<-newdata[newdata$Ideply==TRUE,]
newdata_f<-newdata[newdata$Ideply==FALSE,]

newdata_t$py<-predict(clutch.ri, newdata_t)
newdata_f$py<-predict(clutch.ri, newdata_f)

ggplot(clutch, aes(date, CLUTCH, col=strtno)) + geom_point() +
  geom_line(data=newdata_t, aes(date, py, group=strtno), lty = 2, col = "blue", size=0.9) +
  geom_line(data=newdata_f, aes(date, py, group=strtno), lty = 2, col = "green4", size=0.9) +
  xlab("date") + ylab("Clutch Size")
#this plot looks a bit of a mess, each line represents a different site (i.e. a random effect)!
#we will need to get median predictions to make it a bit cleaner

newdata_tm<-aggregate(newdata_t$py, by=list(newdata_t$date), FUN=mean)
sd1<-aggregate(newdata_t$py, by=list(newdata_t$date), FUN=sd)
newdata_tm<-cbind(newdata_tm, sd1$x)
names(newdata_tm)<-c("date", "meanClutch", "sdClutch"); newdata_tm$date<-as.numeric(newdata_tm$date)

newdata_fm<-aggregate(newdata_f$py, by=list(newdata_f$date), FUN=mean)
sd2<-aggregate(newdata_f$py, by=list(newdata_f$date), FUN=sd)
newdata_fm<-cbind(newdata_fm, sd1$x)
names(newdata_fm)<-c("date", "meanClutch", "sdClutch"); newdata_fm$date<-as.numeric(newdata_fm$date)

#in this diagram the solid lines show median predictions across all sites
#the dotted show the standard deviation around them, based on random effects
ggplot(clutch, aes(date, CLUTCH, col=strtno)) + geom_point() +
  geom_line(data=newdata_tm, aes(date, meanClutch), lty = 1, col = "red", size=1) +
  geom_line(data=newdata_tm, aes(date, meanClutch+sdClutch), lty = 2, col = "red", size=1) +
  geom_line(data=newdata_tm, aes(date, meanClutch-sdClutch), lty = 2, col = "red", size=1) +
  geom_line(data=newdata_fm, aes(date, meanClutch), lty = 1, col = "blue", size=1) +
  geom_line(data=newdata_fm, aes(date, meanClutch+sdClutch), lty = 2, col = "blue", size=1) +
  geom_line(data=newdata_fm, aes(date, meanClutch-sdClutch), lty = 2, col = "blue", size=1) +
  xlab("date") + ylab("Clutch Size")

#so a clear difference between the two designs, but a large amount of variance from the sites!



#Some concluding thoughts
#mixed effects models are complicated
# lmer() can be used to look at many different types of mixed models, based on a normally distributed response
#     a mixed-effects (or mixed model)
#     a random-effects model
#     a hierarchical or multi-level model
#     a random-intercept model.
#in general, they are worth exploring if you have repeated, but uneven and unstructured, repeat measurements
#and critically you want to control for the random differences between sites, not quantify it

#the example given here is a random intercept model
#there are also random slope models! Where parameters vary around random sites
#these are useful, but beware, these again may be overfitting and just producing a model that explains data well
#but means little
#think before you implement, why do you need a random effect? why do you need a random slope?
#if you can't answer these questions, you may be overcomplicating your model with little justification!

#Keep it simple if you can!

#if you really want to quantify random effects, you may run into Best Linear Unbiased Predictions (BLUPs) 
#these can quantify site-level random effects, without making them fixed effects
#I have little experience with them, and if you feel you need them, I would maybe recommend abandoning a frequentist approach

#if random variables, intercepts and slopes are a constant headache
#consider swapping to Bayesian frameworks, where fixed and random variables are effectively the same thing
#and hierarchical models are incredibly easy to implement


##############################################################
### EXERCISE LMM!
##############################################################


salamander<-read.csv("Data/salamander.csv")
salamander$Female<-as.factor(salamander$Female)
salamander$Mate<-as.factor(salamander$Mate)

#this dataset records whether pairs of salamanders mate or not
#male and female are unique IDs, and should be treated as factors

#Mate is a 0/1 response, do they mate or not?

#Cross, is a category describing the patterning of the salamanders
#For example, Cross = W/R indicates a White Side female was crossed with a Rough Butt male.




##TASK: build a GLMMto investigate this dataset, 
#we want to know whether the cross category affects the likelihood of mating, while controlled for the repeated
#measurements of the same individuals (maybe some individuals are just more likely to mate than others)
#look at the response variable and choose whichever distribution you think fits best
#treat male and female ID as random variables


#Remember the steps to follow
# What are your goals? To describe? To predict? To explain? What do you expect to find?
# Look at your data, what type is it? Categorical? Continuous? Binomial?
# What distribution does your data have? 
# Pick a model (or make a shortlist)
# What assumptions does the chosen model have? Does your data meet them?
# Run the model
# Check the model fits properly

# Make conclusions on what the model tells you. What is significant, what is not? What are the effect sizes?
# Make at least one output plot showing what you think is the most interesting conclusion


### SOME HINTS:
#if you're stuck look at the GLM_binomial.R file. Most of the steps in there will apply to this!

#the function to run a GLMM, is glmer, see the 2GLM_binomial.R file, it follows a similar template
#family will be binomial!

#plotting residuals are tricky for GLMMs, I recommend the simulateResiduals() function to check the model is working ok

#if you're stuck look at the GLM_binomial.R file. Most of the steps in there will apply to this!
