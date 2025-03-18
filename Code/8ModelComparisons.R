
#############
# MODEL COMPARISON
#############


#credit: the following code and examples are from
#https://statistics4ecologists-v2a.netlify.app/
#modified by me!


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

#there are occasions when we may wish to consider several models and make informed choices between them
#if we only have a pair of models, then a simple anova() or lrtest() are enough to compare them
#but what if we have lots of potential models (e.g. based on different distributions?
#what if we have lots of parameters, and can't (or shouldn't) include all of them?

#in this case we may need ways of comparing models easily
#these often rely on information criteria such as AIC (or AICc for small sample sizes)
#or similar

#in these cases we explore how well they explain the data, and compare AIC scores
#the lower the better.

#remember though that AIC is relative!!
#your "best" model may still be terrible
#so always look at residuals, plots and summaries as well


#############
### BACKWARD ELIMINATION
#############

#IMPORTANT NOTE: this whole section is not particularly recommended!
#I have included it because it is a very common approach, and you are likely to see it in the literature!

#in this approach we start with a complex model and try to cut out anything superfluous
#this is a bit of "throwing everything at the wall and see what sticks"
#it is nevertheless a very popular approach
#BUT it is prone to errors, as we are essentially trawling for patterns
#a good rule of thumb: if you have no idea what a correlation would mean if you find one, DON'T PUT that variable in the model

#read data in
mammalsc<-read.csv("Data/mammalsc.csv")
head(mammalsc)

#a very popular way a few years ago was to start with a complex model and work back
#we use AIC comparison to remove anything that doesn't aid the model
#remember we are tempting errors by doing this!
#also if we have covarying variables, then the order we drop is REALLY important
#this is typically not account for, so again errors can creep in
MASS::stepAIC(lm(total_sleep ~ life_span + gestation + log(brain_wt) + 
                   log(body_wt) + predation + exposure + danger, data=mammalsc))
#the final model has the lowest AIC: total_sleep ~ log(brain_wt) + predation + exposure + danger


#a stronger approach is an "augmented" backwards elimination
#this penalises removing parameters that influence other parameters (e.g. through co-variation)
fullmodel <- lm(total_sleep ~ life_span + gestation + log(brain_wt) + 
                  log(body_wt) + predation + exposure + danger, data=mammalsc, 
                x = TRUE, y = TRUE)
abe(fullmodel, criterion="AIC", verbose = TRUE, exp.beta = FALSE, data=mammalsc)
#our final model: total_sleep ~ life_span + gestation + log(brain_wt) + log(body_wt) + predation + exposure + danger

#variables that, when dropped, lead to increases in AIC are “blacklisted” and therefore not considered for potential exclusion
#in this case we didn't actually remove any parameters!

#we can loosen the assumptions of this test, and set a threshold for AIC change
abe(fullmodel, criterion="AIC", verbose = TRUE, exp.beta = FALSE, tau= 0.1, data=mammalsc)
#new best model: total_sleep ~ gestation + log(brain_wt) + predation + exposure + danger
#now we've dropped a few parameters which are *mostly* not important


#############
### MODEL AVERAGING
#############

#another (better) approach is to average all your potential models
#weighted by performance
#this should account for variation across models, while still favouring the better ones
#and removes the need to select a single "best" model
#this approach is used very commonly in spatial analysses and climate models
#where consensus across models is more important than a single "best" prediction

options(na.action = "na.fail")
fullmodel<-lm(total_sleep ~ life_span + gestation + log(brain_wt) + 
                log(body_wt) + predation + exposure + danger, data=mammalsc)
allsubsets <- dredge(fullmodel)
allsubsets
#this has made all possible models, and saved how good they are

#then we average them
modaverage <- model.avg(allsubsets, subset = delta < 4)    
summary(modaverage)
#full average includes 0's for parameters where they are dropped from models
#conditional average only uses values for parameters from models in which they were retained

#The former is generally better, as it shows which variables trend towards 0 (i.e. are unimportant)
#this is known as parameter shrinkage


# pull off average coefficients and their SEs
avecoef <- (summary(modaverage))$coefmat.full

#reorder terms so they appear the same as in fullmodelsum, below
avecoef <- avecoef[c(1,8,6,3,7,4,5,2),]
fullmodelsum <- broom::tidy(fullmodel)
combinedcoef <- data.frame(coefs = c(avecoef[,1], fullmodelsum$estimate), 
                           se = c(avecoef[,3], fullmodelsum$std.error), 
                           term = as.factor(rep(fullmodelsum$term, 2)), 
                           method = rep(c("Model Average", "Full Model"), each =8))
combinedcoef$upci <- combinedcoef$coefs + 1.96*combinedcoef$se
combinedcoef$loci <- combinedcoef$coefs - 1.96*combinedcoef$se


ggplot(combinedcoef, aes(method, coefs)) + geom_point()+ 
  geom_errorbar(aes(ymin = loci, ymax = upci, width = 0.2)) +
  facet_wrap(~term, scales="free") +theme_bw()
#and there are our parameter averages!

#we can see from this that our model averages vary in some case from the "full" model
#this can happen for a variety of reasons, including co-variation, lack of degrees of freedom, interactions etc.
#in this case model averaging has given us a better sense on what our parameters should be based on all available models


#############
### REGULARISATION AND PENALISATION
#############

# AKA the LASSO (Least Absolute Shrinkage and Selection Operator) and ridge regression.
# in this approach, our LASSO attempts to shrink unimportant variables to 0
# this allows us to at least try all variables in a model
# but also to maximise degrees of freedom for the important variables, while minimising the less important ones
# if a variable shrinks to 0, it probably isnt important so is excluded from the model

mammalsc$logbrain_wt<-log(mammalsc$brain_wt)
mammalsc$logbody_wt<-log(mammalsc$body_wt)

x = as.matrix(mammalsc[, c("life_span", "gestation", "logbrain_wt", 
                           "logbody_wt", "predation", "exposure", "danger")])
fitlasso <- glmnet(x = x, 
                   y= mammalsc$total_sleep, alpha=1)
plot(fitlasso, xvar = "lambda", label = TRUE)

#the main parameter for LASSO is the "lambda" parameter
#lamda is our parameter for penalising variables. If it's too low, then it does nothing and retains all variables
#if it's too high, it lowers everything to 0!
#so we obviously need to find a middle ground, one that optimises the model

#we can train a model to look for the optimum value of lamda
cvfit.lasso <- cv.glmnet(x = x, 
                         y= mammalsc$total_sleep, alpha=1)
plot(cvfit.lasso)
#we want the smallest MSE!
#we can extract this
cvfit.lasso$lambda.min

#and use it to find out what our best parameter estimates are
coef(cvfit.lasso, s="lambda.min")

#many variables have been reduced to 0, but we see that gestation, logbrain weight and danger are retained!

#the issue with this approach is you don't get the full model output
#if the model fits badly, you will simply be left with the "least bad model"
#you also don't get standard errors or confidence intervals
#so not that desirable as a way to build predictions or plot your data
#BUT it's a good way to explore your data and build a set of sensible parameters
#to include in a final "standard" GLM!


