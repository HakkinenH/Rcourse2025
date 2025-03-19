
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


#load packages
source("./code/0Packages.R")



#############
### LOAD AND EXPLORE DATA
#############


#load in data set
mite1<-read.csv("data/mites.csv")

#check it loaded correctly
head(mite1)
summary(mite1)

#for this simple example we will only use a few variables
#pa is presence absence, i.e. 0/1
#WatrCont is the local availability of water (continuous)
#Topo is the local topography of the sample site (categorical)

plot(mite1$WatrCont,mite1$pa)
table(mite1$Topo, mite1$pa)
#some basic indications that presence goes down as water content goes up
#and more presences (proportionally) in hummock

#check distribution of response
hist(mite1$pa)
#yep that's binomial

#check distribution of predictor. This does not need to confirm to any particular distribution, but shouldn't have massive outliers
hist(mite1$WatrCont)


#############
### RUN A GLM
#aka a Generalised Linear Model based on a binomial distribution
#############


#run the model
logit.reg <- glm(pa ~ WatrCont + Topo,
                 data = mite1,
                 family = binomial(link = "logit"))

#check residuals
plot(logit.reg)

#residuals can be difficult to interpret in binomial model, other packages can help
residualPlot(logit.reg)
binplot<-binned_residuals(logit.reg)
plot(binplot)
#looks ok! Some problems on the extremes, near 0 and 100. Otherwise ok


#SIDE NOTE: Formal Goodness-of-fit Tests
#You may see online that you can formally test for goodness of fit of models
#these test the residual distribution against a standardised null set,
#and return whether the distribution significantly differs or not
#For binomial models, this is the Hosmer-Lemeshow test
#BUT they are sensitive to sample sizes, category sizes, implementation in package
#and many other things. I generally don't recommend them, but be aware they exist
#your own judgement is usually better than a battery of tests


#SIDE NOTE: OVER DISPERSION
#this is an additional check we need to carry out for Binomial GLMs that we don't for linear GLMs: overdispersion
#binomial distributions do not have a parameter for data variance (unlike Gaussian distributions)
#this means they assume that the data mean is proportional to variance. If it isn't, the data is overdispersed!
#This is a major issue as it means your data doesn't strictly fit a binomial distribution
#If you have over dispersion, you probably need another distribution (like a quasibinomial)
#To check for overdispersion, a rule of thumb is divide residual deviance by degrees of freedom
#if it's greater than 1, then you have issues!
summary(logit.reg)
48.762/67
#in this case we are good (using figures from the model output)
#if anything it's a bit underdispersed!

#now we are confident the model fits ok, we can look at the output properly
summary(logit.reg)

#conclusions:
#WaterContent has a significant impact on presence
#Topography has a significant impact on presence, 


#for Water content, every unit of water (g per dry weight), the probability of presence goes down by -0.01
#BUT remember the model is on the logit scale! To interpret this, we need to back transform it
inv.logit(-0.015813) #0.496
#BEWARE: this is a curved relationship, not a linear one, so this is only a ball park figure
#useful for interpretation, but a lot less reliable that plotting it.

#For topography, on a hummock the probability is 2.09 higher than on blanket moss
#BUT remember this is all on the log scale! To get it on a scale we understand, need to back-transform it
#probability of presence on blanket moss (intercept):
inv.logit(4.464402) #0.9886194
#That doesn't make much sense! we never saw such high rates of prevalence
#this is because the intercept is where continuous variables are at 0. In this case that's Water Content
#in real life we never saw water content below 134, but the model has extrapolated back to 0
#hence it doesn't make much sense
#this is one reason we might use scaling! Then the mean of water content is 0, and we scale around that.

#let's try it
mite1$WatrCont_s<-scale(mite1$WatrCont, center = TRUE, scale = TRUE)
#run the model
logit.reg1 <- glm(pa ~ WatrCont_s + Topo,
                 data = mite1,
                 family = binomial(link = "logit"))

#check residuals
plot(logit.reg1)
summary(logit.reg1)

#let's try again with the new figures from our scaled dataset
#at the mean water content (i.e. scaled WatrCont_s = 0), the probability of presence on blanket moss is:
inv.logit(-2.0288) #0.116
#on hummocks it is
inv.logit(-2.0288 + 2.0908) #0.515

#we can also find confidence intervals, where are the parameters likely to be?
ci.prof<-confint(logit.reg1)
#can back transform if that is useful
exp(ci.prof)


#let's make a plot!
ggplot(mite1, aes(x = WatrCont, y = pa)) + geom_point() +
  stat_smooth(method = "glm", method.args = list(family=binomial), se = TRUE) + 
  xlab("Water content") +
  ylab("Probability of presence") +
  ggtitle("Probability of presence of  Galumna sp. against the water content")+theme_classic()


#let's make one with confidence intervals

newdat <- data.frame(WatrCont = seq(100, 900, length = 500), Topo="Blanket")

#make a predicted curve
phat <- predict(logit.reg, type = "resp", se.fit = T,newdata = newdat)

#We can predict standard errors and confidence intervals on the response scale
#BUT this introduces errors, it's better to calculate on link scale and back transform
# Predictions link scale
phat2<-predict(logit.reg, type = "link", se.fit = T, newdata = newdat)
lcl.l <- plogis(phat2$fit + 1.96 * phat2$se.fit)
ucl.l <- plogis(phat2$fit - 1.96 * phat2$se.fit)
pe.l <- plogis(phat2$fit)

# Combine and plot
newdat <- cbind(newdat, phat = phat$fit, phat2 = phat2$fit, lcl.l, ucl.l, pe.l)
ggplot(newdat, aes(WatrCont, phat)) + geom_line() +
  geom_line(aes(WatrCont, lcl.l), lty = 2, col = "blue") +
  geom_line(aes(WatrCont, ucl.l), lty = 2, col = "blue") +
  xlab("Water Content") + ylab(expression(hat(p)))



#let's make a plot that includes the various levels of our categorical predictor
#note I add a jitter to points to make them overlap less so we can see what's going on
#it's bad practice, but very useful!
ggplot(mite1, aes(x=WatrCont, y=pa, colour=Topo)) + theme_bw()+
  geom_point(position = position_jitter(w = 2, h = 0.05), size=3) + 
  stat_smooth(method="glm", method.args = list(family = "binomial"),
              se=FALSE)



##############################################################
### EXERCISE GLM!
##############################################################

#this is a dataset of wild birds (of a single species) confiscated from an illegal live market
#we are interested in what sets the market value of these birds

moom<-read.csv("data/moose_binomial.csv")
head(moom)
#observed: whether a moose was seen (1/0 presence/absence)
#year: year of observation
#voc: the amount of cover that shields the moose from view, termed visual obstruction
#grpsize: the size of the group out attempting to spot moose

#TASK: build a binomial GLM model, investigating what variables correlate with whether moose were seen or not
#If we model the probability of detection as a function of VOC, 
#we can then adjust counts of observed animals in future surveys that also record this information, 
#providing a formal method for estimating moose abundance that accounts for imperfect detection


#Remember the steps to follow
# What are your goals? To describe? To predict? To explain? What do you expect to find?
# Look at your data, what type is it? Categorical? Continuous? Binomial?
# What distribution does your data have? 
# Pick a model (or make a shortlist) (SPOILER: it's a binomial model)
# What assumptions does the chosen model have? Does your data meet them?
# Run the model
# Check the model fits properly
  #HINT: if you have overdispersion, try a quasibinomial distribution!
# Make conclusions on what the model tells you. What is significant, what is not? What are the effect sizes?
# Make at least one output plot showing what you think is the most interesting conclusion


