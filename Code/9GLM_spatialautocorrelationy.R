
#############
# GENERALISED LINEAR MODEL (with spatial autocorrelation)
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


data.spatialCor<-read.csv("Data/dataspatialCor.csv")
#in this made-up example we have x (a predictor) y (a response) and coordinates of data
head(data.spatialCor)

hist(data.spatialCor$y)

#looks normal, so let's start with a basic linear model
data.spatialCor.lm <- lm(y ~ x, data.spatialCor)
par(mfrow = c(2, 2))
plot(data.spatialCor.lm, which = 1:4)

#this looks ok, BUT I happen to know there may be autocorrelation in the data

#let's explore! we can check for autocorrelation using a bubble plot
data.spatialCor$Resid <- rstandard(data.spatialCor.lm)
coordinates(data.spatialCor) <- ~LAT + LONG  #effectively convert the data into a spatial data frame
bubble(data.spatialCor, "Resid")

#we have plotted our data poins, but the values are now our model residuals
#we want this to look as random as possible
#But in this case there is really obvious clustering, similar residuals are clustered spatially
#this is big issue in spatial analysis as we typically assume locations are independent
#so we would conclude we have spatial autocorrelation


#alternative method: use a variogram
#side note: for a variety of reasons this is easier in a different package, more designed for this
#hence the new model with gls() it's practically the same as glm() for this purpose
data.spatialCor.gls <- gls(y ~ x, data.spatialCor,
                           method = "REML")
plot(data.spatialCor.gls)
plot(nlme:::Variogram(data.spatialCor.gls, form = ~LAT +
                        LONG, resType = "normalized"))
#in a variogram we measure the independence of each point based on distance
#we should see a flat line if there is no autocorrelation (i.e. dependence goes not vary byt distance)
#a clear bend! we have issues

#the issue we have is there are a lot of ways autocorrelation works
#does autocorrelation work linearly with distance? exponentially?
#we could look at this carefully, but it can be very tricky to work out
#another option is to try lots of autocorrelation options and see which fits the best

data.spatialCor.glsExp <- gls(y ~ x, data = data.spatialCor,
                              correlation = corExp(form = ~LAT + LONG, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsGaus <- gls(y ~ x, data = data.spatialCor,
                               correlation = corGaus(form = ~LAT + LONG, nugget = TRUE),
                               method = "REML")
data.spatialCor.glsLin <- gls(y ~ x, data = data.spatialCor,
                              correlation = corLin(form = ~LAT + LONG, nugget = TRUE),
                              method = "REML")
data.spatialCor.glsRatio <- gls(y ~ x, data = data.spatialCor,
                                correlation = corRatio(form = ~LAT + LONG, nugget = TRUE),
                                method = "REML")
data.spatialCor.glsSpher <- gls(y ~ x, data = data.spatialCor,
                                correlation = corSpher(form = ~LAT + LONG, nugget = TRUE),
                                method = "REML")

#now we do a model comparison to see which one performs best
#and if models with an autocorrelation term work better than one without
AIC(data.spatialCor.gls, data.spatialCor.glsExp, data.spatialCor.glsGaus,
    data.spatialCor.glsLin, data.spatialCor.glsRatio,
    data.spatialCor.glsSpher)
#exp model is the best, closely followed by several others. So autocorrelation increase exponentially with distance
#but all are better than the model with no correlation structure!

#let's check the residuals of our favoured model
par(mfrow = c(1,1))
plot(residuals(data.spatialCor.glsExp, type = "normalized") ~
       fitted(data.spatialCor.glsExp))
#looks good!
plot(nlme:::Variogram(data.spatialCor.glsExp, form = ~LAT +
                        LONG, resType = "normalized"))
#much much better! nearly flat in the variogram!

#now we can look at the results and be confident that the autocorrelation is controlled for
summary(data.spatialCor.glsExp)
