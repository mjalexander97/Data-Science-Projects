projdata <- read.table("C:/Users/Micheal/Desktop/PSTAT 126/Project/projdata.txt",header=TRUE) # read in the data set

head(projdata) # show the top observations of the data set
attach(projdata)
pairs(happy~gender+workhrs+relationship,data=projdata) # create a scatterplot matrix with the variables gender, workhrs, and relationship
plot(projdata$workhrs,projdata$happy) # graph that shows the relationship between workhrs and levels of happiness

mainfit<-lm(happy~relationship+workhrs+gender,data=projdata) # fit a linear model with happiness levels as the dependent variable and relationship, workhrs, and gender as the predictors 
summary(mainfit) # provide summary statistics

# fit more models between happiness and each of the individual predictor variables
genderfit<-lm(happy~gender,data=projdata) 
summary(genderfit)

workhrsfit<-lm(happy~workhrs,data=projdata)
summary(workhrsfit)

relationshipfit<-lm(happy~relationship,data=projdata)
summary(relationshipfit)

# variance and normality checks 
plot(fitted(mainfit),residuals(mainfit)) # residual plot
qqnorm(residuals(workhrsfit))
qqline(residuals(workhrsfit))
hist(residuals(workhrsfit))


#with interaction
interactionfit<-lm(happy~relationship*gender+workhrs,data=projdata)
summary(interactionfit)
plot(residuals(interactionfit))
plot(fitted(interactionfit),residuals(interactionfit))
abline(h=0)
qqnorm(residuals(interactionfit))
qqline(residuals(interactionfit))
hist(residuals(interactionfit))
