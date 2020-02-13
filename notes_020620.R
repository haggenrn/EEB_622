setwd("Dropbox/spring_2020/622/datasets")

### Lecture 8: Poisson regression ######

#notes below carried over from last lecture notes#
### NEW TOPIC: POISSON DISTRIBUTION ######
#Binomial regression is appropraite when there is an upper bound to your number of trials! Something from 0 to n. 
#Poisson = count outcome without a known maximum...if we go fishing and come home with 7 fish, how many trials were
#there? What was the upper bound? Poisson formula is pretty much the binomial formula where x is now a k. 
#events must occur independent of eachother, not impacting eachother
#data must be integers 
#know rate 

#why can't we just use the binomial distribution? 
  #1) binomial can't have more than one success in a trial (has to be 0 or 1)
  #2) we don't actually know the probability or success and we don't know our number of successes (so we use lambda!)

#k = number of times an event occurs in an interval. 
#Lambda = expected number of occurences per unit of time or space ('rate') ... lambda = (n*p) or p = lambda/n




########so poisson is a "special case of the binomial" where n approaches infinity and p is very very small######




#examples of poisson counts:
#number of eggs in a nest
#number of spots on an owl
#number of insects at a light 

#does a mean value of negative counts make sense? NO. Lambda can't equal 0. Similar to binomial dist., there is a 
#restraint on lambda 


#how do we write a poisson distribution?
#Y~Poisson(lambda)
#lambda = ????(a + b * Xi) (what do we put in the ???? We need something that we keep lambda positive)
#exp() <- will transform answer into something that has to be between 0 and +infinity so lambda = exp(a + b * Xi)




#now on to current lecture notes
setwd("Dropbox/spring_2020/622/datasets")

### EXERCISE ONE: FUNGUS GNATS ###########

# Record the data (count the # of gnats on your units)
counts <- c(4, 4, 5, 6, 6, 5, 7, 3, 1, 5, 7, 7, 4, 2, 4, 9,
            ## treat A -- 16
            15 ,15, 21, 8, 18, 18, 13, 7, 9, 17) ## treat B -- 10
treatment <- c(rep("1", 16), rep("0", 10)) #create a categorical varible for difference between treated and not

gnats <- data.frame(counts, treatment)
View(gnats)

# plot data
plot(counts~treatment) #this plot doesn't make a ton of sense because we only have zeros and ones...but it works

# Run poisson regression
gnatmod <- glm(counts~treatment,family="poisson") 
summary(gnatmod) #'treatment1' is a negative parameter which means the treatment worked and decrease the counts SO
#the treatment of insecticide in Trevor's office worked to decrease the number of gnats 



# Exercise1: Categorical Fungus gnats: ###########

# Load the data we recorded in class
gnats <- read.csv("gnats_categorical.csv", header=T)

# in the treatment variable, 1 = treated, 0 = not treated.

# plot data
plot(counts~treatment, data=gnats) 

# Run poisson regression
gnatmod <- glm(counts~treatment, data=gnats, family="poisson") 
summary(gnatmod)
coef(gnatmod)


## How do you interpret the parameter estimates?
coef(gnatmod) #it's hard to interpret this estimates so we need to tranform them

#Given the log link function, how do we transform the coefficients in R?
#Type it in here:
exp(coef(gnatmod)) #the rate of gnats (gnats per paper) = intercept + slope *treatment, we can look at intercept
#directly to figure out the number of gnats in the office before the treatment (just plug 0 into "treatment")

## Alternatively, because the intercept signifies the number of gnats BEFORE treatment (Treatment=0), we can 
#calculate the effect of the treatment by plugging in values, using a similar approach as before:
exp(2.64+(-1.0493*0))-exp(2.64+(-1.0493*1)) #values of parameters before transformation


# Visualizing treatment effect for a categorical variable with boxplots:
boxplot(counts~treatment, data=gnats, ylab="gnats counted", xlab="Insecticide treatment", names=c("Untreated", "Treated"), outline=TRUE, col="lightblue") 

# Ridgeplots are another great way to compare treatment effects:
library(ggplot2) ## for ridge plots, install if you do not have this already
library(ggridges)
ggplot(gnats, aes(x = counts, y = treatment, fill=as.factor(treatment))) + 
  geom_density_ridges()




### Exercise 2: Sage grouse reproduction
#when do you include more than one predictor? 
    #if you hypothesize that multiple variables have a DIRECT influence on the response
    #to account for addiitonal drivers of variation in your dataset
    #if these multiple variables are not highly correlated (multicollinearity)

sagegrouse <- read.csv("sagegrouse.csv", header=T)
View(sagegrouse)
# install.packages("coefplot") # only run this once!
library(coefplot)


# Bryanna is curious about the relationship between glucuronic acid concentrations in sage grouse
plot(eggs~glucacid, data=sagegrouse)

eggcount <- glm(eggs~glucacid , data=sagegrouse, family="poisson")
summary(eggcount) #there's almost certainly a negative trend in the data

#How do we interpret the parameter estimates?
coef(eggcount)
exp(coef(eggcount)) #number of eggs avg. bird produces when 0 toxin present is 8. 




###ADD MORE VARIABLES TO MODEL TO SEE HOW THEY CHANGE OUR UNDERSTANDING###

# Bryanna is concerned that some of this effect might just be associated with bird size, rather than just the 
#effect of glucoronic acid, so she decides to add another predictor for height:
eggcount.ht <- glm(eggs~glucacid +height, data=sagegrouse, family="poisson")
coef(eggcount.ht)
coef(eggcount)
exp(coef(eggcount.ht)) #intercept has shifted when we add more information!! The addition of height changes the way
#we intuit the baseline level of egg production 
exp(coef(eggcount))

# What happens if we add an additional predictor?
eggcount.age <- glm(eggs~height + glucacid + ageyrs, data=sagegrouse, family="poisson")
summary(eggcount.age) #slope of height changed a lot in comparison to previous model 
#why did it change? colinearity. Why? those variables (height and ageyrs) are highly corelated. 
exp(coef(eggcount.age))
exp(coef(eggcount.ht))
exp(coef(eggcount))



# Why did our result change?
coefplot(eggcount, coefficients=c( "glucacid", "height"))
coefplot(eggcount.age, coefficients=c( "glucacid", "height", "ageyrs"))

cor(sagegrouse) #values closer to 1 indicated high correlation between variables. Just look at height and age!!


#a parameter estimate is measure of how much a given variable impacts the response of the model if ALL OTHER 
#VARIABLES ARE HELD CONSTANT
#when variables are highly correlated/collinear, WE DON'T HAVE A SET OF OBSERVATIONS IN WHICH EACH VARIABLE CHANGES 
    #solutions for multicollinearity? 
#      1)create a conceptual model 
#      2)considering subsetting the data, dividing your analysis into stages 
#      3)experimental design (collect additional information)
#      4)how much collinearity is too much? You have to run the model and compare, you should understand how your 
#        inference changes due to correlation

## How do we assess if variance is approximately similar to mean?

summary(eggcount)


