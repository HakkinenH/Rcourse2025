

#############
# INSTALLS AND DEPENDENCIES
#############

#run this file to install all the packages you need
if(!require("ggResidpanel")) install.packages("ggResidpanel")
if(!require("patchwork")) install.packages("patchwork")
if(!require("sjPlot")) install.packages("sjPlot")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("performance")) install.packages("performance")
if(!require("see")) install.packages("see")
if(!require("boot")) install.packages("boot")
if(!require("car")) install.packages("car")
if(!require("DHARMa")) install.packages("DHARMa")
if(!require("lme4")) install.packages("lme4")
if(!require("ggeffects")) install.packages("ggeffects")
if(!require("gratia")) install.packages("gratia")
if(!require("ggthemes")) install.packages("ggthemes")
if(!require("abe")) install.packages("abe")
if(!require("MuMIn")) install.packages("MuMIn")
if(!require("glmnet")) install.packages("glmnet")
if(!require("glmm")) install.packages("glmm")
if(!require("MASS")) install.packages("MASS")
if(!require("nlme")) install.packages("nlme")
if(!require("interactions")) install.packages("interactions")
if(!require("mgcv")) install.packages("mgcv")
if(!require("sp")) install.packages("sp")


### FOR THOSE INTERESTED IN BAYESIAN STATS:
#NOTE: the following are only necessary if you want to run the Bayesian JAGs example script
#these are more involved and may require admin rights!
#only run if you want to work through the Bayesian examples
#load (or install) packages required
if(!require("mcmcplots")) install.packages("mcmcplots")
if(!require("MCMCvis")) install.packages("MCMCvis")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("ggthemes")) install.packages("ggthemes")
#these two seem to struggle on the ZSL network. If they fail, see notes below
# if(!require("rjags")) install.packages("rjags")
# if(!require("R2jags")) install.packages("R2jags")
# 
# #check these load
# library("rjags")
# library("R2jags")
#if these two lines return an error, you may need to install JAGs manually (depending on your version of R)
#this will require admin rights! Install from here:
#https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Windows/JAGS-4.3.0.exe/download
#download, install, and use default settings. Relaunch R and try again
#If you run a non-Windows OS see:
#https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Mac%2520OS%2520X/
#for linux see: https://mcmc-jags.sourceforge.io/


#now check they all load
library(abe) 
library(MuMIn)
library(glmnet)
library(lme4)
library(glmm)
library(MASS)
library(nlme)
library(ggResidpanel)
library(patchwork) 
library(sjPlot)
library(ggplot2) # for plots
library(performance)
library(see)
library(boot)
library(car)
library(DHARMa)
library(mgcv)
library(ggeffects)
library(gratia)
library(ggthemes)
library(interactions)
library(sp)

