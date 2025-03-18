
#############
# BAYESIAN MODELS (in JAGs)
#############

#credit: the following code and examples are from
#https://statistics4ecologists-v2a.netlify.app/bayesmcmc
#modified by me!

#load (or install) packages required
if(!require("rjags")) install.packages("rjags")
if(!require("R2jags")) install.packages("R2jags")
if(!require("mcmcplots")) install.packages("mcmcplots")
if(!require("MCMCvis")) install.packages("MCMCvis")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("ggthemes")) install.packages("ggthemes")


library(rjags)
library(R2jags)
library(mcmcplots)
library(MCMCvis) 
library(ggplot2)
library(ggthemes)

#you may need to install JAGs manually (depending on version of R)
#this will require admin rights!
#https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Windows/JAGS-4.3.0.exe/download

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


#Bayesian statistics work in a very different way to frequentist
#in this case there is no null model, no test statistic, no p-value
#instead we have a "prior" and a "posterior" distribution
#we tell the model how we think the data is distributed, or at least all the possible distributions it could be
#it then iterates across this prior "parameter" space and finds the best fit


#the steps for a Bayesian model in JAGs are:
  #define the model
  #define the priors
  #run the model
  #check model ran properly (convergence, fuzzy caterpillars etc)
  #check parameter output
  #make conclusions and plots

#other packages in R (notably stan) are also available
#but I like RJAGs because
  #it  has been around a while and has good documentation
  #it forces you to define your own model
  #I'm a creature of habit


#this file is meant to give you a practical understanding of JAGs
#I strongly recommend looking up Bayesian specific teachings online if you're interested!



#############
### LOAD AND EXPLORE DATA
#############

#our data for now. male and female jackal jaw lengths
males<-c(120, 107, 110, 116, 114, 111, 113, 117, 114, 112)
females<-c(110, 111, 107, 108, 110, 105, 107, 106, 111, 111)

#specify the model
#note that JAGs has a peculiar format, you just have to get used to it
#I have decided that the mean jawq length is around 100, but could vary
#I also think it might vary between females and males. so I code this in to allow the model to test for this
#however I think the variance is the same between males and females, so I keep this constant
#remember a normal distribution is defined by its mean and SD
jaw.mod<-function(){
  
  # Priors 
  #specify what is the available range of means for males
  mu.male ~ dnorm(100, 0.001) # mean of male jaw lengths
  #quick explanation: the model will expect the mean to be around 100, but we have given it a SD of 0.001 to allow variation around that
  #we specify the available parameter space 
    #1) to make it faster 
    #2) so it doesn't look at impossible solutions 
    #3) because our prior is now effectively our hypothesis so we compare against that
  #specify what is the available range of means for females
  mu.female ~ dnorm(100, 0.001) # mean of female jaw lengths
  
  sigma ~ dunif(0, 30) # common sigma
  tau <- 1/(sigma*sigma) #precision
  
  # Likelihood (Y | mu.male, mu.female, sigma) = Normal(mu[sex], sigma^2)  
  for(i in 1:nmales){
    males[i] ~ dnorm(mu.male, tau) 
  }
  for(i in 1:nfemales){
    females[i] ~ dnorm(mu.female, tau)
  }
  
  # Derived quantities:  difference in means
  mu.diff <- mu.male - mu.female
}


#we now need to make a starting point for the model fitting process
#these are known as initial values
# Function to generate initial values
init.vals<-function(){
  mu.male <- rnorm(1, 100, 100)
  mu.female <- rnorm(1, 100, 100)
  sigma <- runif(1, 0, 10) 
  out <- list(mu.male = mu.male, mu.female = mu.female, sigma = sigma)
}

#' Create rest of the data for the model
nmales<-length(males)
nfemales<-length(females)


t.test.jags <- jags(data=c("males", "females", "nmales",  "nfemales"),
                    inits = init.vals,
                    parameters.to.save = c("mu.male", "mu.female", "sigma", "mu.diff"), 
                    progress.bar = "none",
                    n.iter = 10000,
                    n.burnin = 5000,
                    model.file = jaw.mod,
                    n.thin = 1,
                    n.chains = 3)

#Note the following arguments:
# data will contain all modeled and unmodeled data (here, males and females containing the jaw lengths and nmales and nfemales containing the number of males and females).
# parameters.to.save is a list of parameters for which we want to save the MCMC samples. Here, we specify that we want to keep track of μm,μf,σ, and μm−μf. By contrast, we do not save τau
# since we do not intend to examine the posterior distribution of the precision parameter.
# progress.bar = "none" (for Windows users, you can also try progress.bar = gui). If we don’t supply this argument, you will get a lot of output in your html file when using Rmarkdown.
# n.iter = 10000 specifies the total number of samples we want to generate
# n.burnin = 5000 specifies that we want to throw away the first 5000 samples
# model.file = jaw.mod specifies the function containing the model specification
# n.thin = 1 specifies that we want to keep all of the samples. We can save memory by saving say every other sample if we change this to n.thin = 2. If the chains are highly autocorrelated, we won’t loose much information by keeping every other sample.
# n.chains = 3 specifies that we want to generate 3 Markov chains, each generated with a different set of starting values.

#quick side note, we can use parallel processing if we want to!
# t.test.jags <- jags.parallel(data = c("males", "females", "nmales",  "nfemales"),  
#                              inits = init.vals,
#                              parameters.to.save = c("mu.male", "mu.female", "sigma", "mu.diff"),
#                              n.iter = 10000,
#                              n.burnin = 5000,
#                              model.file = jaw.mod,
#                              n.thin = 1,
#                              n.chains = 3)

t.test.jags


#what can we see here?
# mu.vect = the mean of the posterior distribution
# sd.vect = the standard deviation of the posterior distribution
# 2.5% to 97.5% = quantiles of posterior distribution
# Rhat is the Gelman-Rubin statistic for evaluating convergence of the MCMC samples.
# n.eff = an estimate of the effective sample size. As we will see in a bit, our MCMC samples will typically be autocorrelated. Thus, they will contain less information than if we were able to somehow generate a set of independent samples from the posterior distribution.

#we can select only things we are interested in
MCMCsummary(t.test.jags, params = c("mu.male", "mu.female"))
# Use MCMCsummary to pull off posterior means
bayesests <- MCMCpstr(t.test.jags, params = c("mu.male", "mu.female"), func = mean)
bayesests
# Use MCMCsummary to pull of upper and lower 95% credible interval limits
bayesci <-  MCMCpstr(t.test.jags, params = c("mu.male", "mu.female"), 
                     func = function(x) quantile(x, probs = c(0.025, 0.975)))
bayesci

#for comparison we can do a frequentist linear model!
# Fit linear model in frequentist framework
jawdat<-data.frame(jaws=c(males, females), 
                   sex=c(rep("Male", length(males)), 
                         rep("Female", length(females))))
lm.jaw<-lm(jaws~sex-1, data=jawdat)
# Store results
betaf <- coef(lm.jaw)
CIf <-confint(lm.jaw) 
ests<-data.frame(estimate = c(bayesests$mu.female, bayesests$mu.male, betaf), 
                 LCL = c(bayesci$mu.female[1],   bayesci$mu.male[1], CIf[,1]), 
                 UCL = c(bayesci$mu.female[2],   bayesci$mu.male[2], CIf[,2]), 
                 param = c("Mean females", "Mean males"),
                 Method = rep(c("Bayesian", "Frequentist"), each = 2))


ggplot(ests, aes(param,estimate, col = Method)) + 
  geom_point(position = position_dodge(width = 0.2))+ 
  geom_pointrange(aes(ymin = LCL, ymax= UCL), position = position_dodge(width = 0.2))+
  ylab("Estimate") + xlab("") +
  scale_x_discrete(labels = c('Mean females' = expression(mu[f]),
                              'Mean males'   = expression(mu[m]))) + 
  scale_colour_colorblind()+
  theme(text = element_text(size = 20))  
#massive surprise, they are the same!


#there are a number of things we can check regarding our Bayesian model

#what do our posterior estimates look like?
denplot(t.test.jags, ask = FALSE)  


#what do our traceplots look like?
#this is important! They have to converge to something relatively steady
traplot(t.test.jags, ask=FALSE)
#looks good

#can plot parameter overlaps
caterplot(t.test.jags, ask = FALSE, 
          parms = c("mu.male", "mu.female"))  
#looks interesting! A clear difference between males and females at 67% CI and 95% CI!


##############################################################
### EXERCISE: BAYESIAN MODELS!
##############################################################

##TASK: more reading and model building
#read the Bayesian section from https://statistics4ecologists-v2a.netlify.app/bayesmcmc

#after that, recreate our model from 1GLM.R but in a Bayesian framework!
df1<-read.delim("Data/SkyWhales_2024.txt", sep=" ")
#use whatever parameters you like! 
#but I would recommend to start with only one or two parameters and buildup from there

