# Lecture 5-7: Binomial regression & Poisson regression (adapted from existing notes by ASW)

setwd("Dropbox/spring_2020/622/datasets")

### Lecture 5-6: Binomial regression and probability for Jan 28-30 ######

# Calculate the likelihood that p=0.8, 0.9, and 0.1, given that 5 out of 10 boxes were occupied by owls

p <- c(0.8, 0.9, 0.1)
choose(10,5)*(p^5)*((1-p)^5)

# using a shortcut to identify the max likelihood value of p:
curve(dbinom(x=5,size=10,prob=x))

# What are ODDS? What are LOG ODDS? How do they change the range of values that p can vary over?
p <- seq(from=0, to=1, length.out=100)

odds <- p/(1-p)
plot(odds~p, pch=1)

logodds <- log(p/(1-p))
plot(logodds~p)

# the logistic function constrains p to be between 0 and 1:
exp(1)/(1+exp(1))
curve(exp(x)/(1+exp(x)),from=-100,to=100)
curve(plogis(x),from=-100,to=100,add=T)



### EXERCISE: OWL BOX OCCUPANCY: ######

### Load and plot data
setwd("Dropbox/spring_2020/622/datasets")

owlbox <- read.csv("owlbox.csv", header=T)

plot(owlbox$owl_present~owlbox$road_dist, xlab="dist from road (m)", ylab="prob. of owl presence")
plot(jitter(owlbox$owl_present)~owlbox$road_dist, 
     xlab="dist from road (m)", ylab="prob. of owl presence")



### run a binomial glm:
mod1 <- glm(owl_present~road_dist, data=owlbox, family="binomial"(link="logit"))

coef(mod1) 
#our parameters are expressed as LOG ODDS an we want to express it as probbility. 
#We need to transform it to ODDS, then to probability. We use a link function to do this.

##That link function is....'plogis'!!! #oh also, we can bump probabiltiy back up to LOG ODDS using 'logit'function.
plogis(coef(mod1))
##intercept of 0.004485556 means there is a 4% chance of having an owlbox occupied when distance from road is 0. 



### evaluate how probability differs between different values of x. 
plogis(-3+0.03*200)-plogis(-3+0.03*100)
# You could phrase this as, between 100m from the road and 200m from the road, probability of owl box occupancy 
#increases by 0.45.



### Plot out the prediction of the model:
# First, create new fake data. Gradient of road values that vary from min to max that are same length as our dataset.
#Values will be between 0 and 300.
roadsims <- seq(from=min(owlbox$road_dist),
                to = max(owlbox$road_dist), 
                length.out=nrow(owlbox))
roadsims

#Then ask model to predict response (probability of owl presence based on our new data), using "predict" to 
#find values between 0 and 1 that are probabilities:
owlpreds <- predict(mod1, 
                    newdata=data.frame(road_dist=roadsims),
                    type="response")
owlpreds

# This part will sort the data so it stays in order (lowest to highest), for anyone who was getting a squiggly 
#line instead of a smooth one. If this doesn't work for you, we'll chat more in class on Tues:
predictowl <- data.frame(roadsims, owlpreds)
predictowl <- predictowl[order(roadsims), ]

# Then plot:
plot(owlbox$owl_present~owlbox$road_dist, pch = 16, 
     xlab = "distance from road (m)", 
     ylab = "prob of owl occupancy", col="lightblue") # Plots raw data
lines(predictowl$roadsims, predictowl$owlpreds) # Plots model's prediction



###Confidence intervals
plogis(confint(mod1))






### EXERCISE 2: EPIDEMIOLOGY OF A FOREST DISEASE ######
infections <- read.csv("forest_infections.csv", header=T)

mod2 <- glm(cbind(tanoakinf, (tottanoak-tanoakinf))~bayinf, 
            data=infections, family="binomial")
summary(mod2)

#n = number of trails, x = number of successes
#we need to combine/bind (using 'cbind' in the glm function) the data so we can run a binomial regression
#if we were to only feed the function a proporation of infected/not infected then we lose the fact that some 
#plots had 30 trees and others only had 3...that's why we need to use cbind!!!

plogis(coef(mod2)) #even if there is no baylaurel in our plot, then there's still a 28% chance a tanoak is infected...
#slope in this context is a little tricky to undertand on its own. You can use the divide-by-four rule if you want 
#or you can just plot it out and look. Also the dbf rule only looks at the maximum effect! You can also just math it
#and see what happens when you change x...logistic(a+bx1) - logistic(a+b2)
      #what is the effect of going from 10 to 20 meters on the road:
      #plogis(-2.7+0.03*20)-plogis(-2.7+0.03*10)

plogis(confint(mod2))



### Plot the model's prediction and the real data
baysims <- seq(from=min(infections$bayinf),
               to = max(infections$bayinf), 
               length.out=nrow(infections))
infpreds <- predict(mod2, 
                    list(bayinf=baysims),
                    type="response")
predictinf <- data.frame(baysims, infpreds)
predictinf <- predictinf[order(baysims), ] ## Sort the data so it stays in order

plot((infections$tanoakinf/(infections$tottanoak))~infections$bayinf, pch = 16, 
     xlab = "Infected bay laurel trees", ylab = "Proportion of tanoaks infected", 
     col="forestgreen")
lines(predictinf$baysims,predictinf$infpreds, lwd=3)

#the intercept is high enough that it suggests maybe there is something else at play here, but we can clearly 
#see that an increase in the # of baylaurel trees in a plots corresponds to an increase in tanoaks infected.






### NEW TOPIC: POISSON DISTRIBUTION ######
#Binomial regression is appropraite when there is an upper bound to your number of trials! Something from 0 to n. 
#Poisson = count outcome without a known maximum...if we go fishing and come home with 7 fish, how many trials were
#there? What was the upper bound? 
  #events must occur independent of eachother, not impacting eachother
  #data must be integers 
  #know rate 

#k = number of times an event occurs in an interval. 
#Lambda = expected number of occurences per unit of time or space ('rate')


#examples of poisson counts:
  #number of eggs in a nest
  #number of spots on an owl
  #number of insects at a light 

#does a mean value of negative counts make sense? NO. Lambda can't equal 0. Similar to binomial dist., there is a 
#restraint on lambda 


#how do we write a poisson distribution?
#Y~Poisson(lambda)
#lambda = ????(a + b * Xi) (what do we put in the ???? We need something that we keep lambda positive)
    #exp() <- will transform answer into something that has to be between 0 and +infinity 


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

# How do we interpret these parameters given the link function we used?
coef(gnatmod)

# Visualizing treatment effect:
boxplot(counts~treatment, xlab="Treatment", names=c("Untreated", "Treated")) 

library(ggplot2) ## for ridge plots, install if you do not have this already
library(ggridges)
gnats <- data.frame(counts, treatment)
ggplot(gnats, aes(x = counts, y = treatment, fill=treatment)) + geom_density_ridges()



# Exercise2: Fern spore dispersal ########
ferns <- read.csv("fernspores.csv", header=T)

# plot data
plot(ferns$spores~ferns$km_from_forest, col="purple", pch=16, xlab="distance from forest (km)",ylab= "fern spores in trap")

#create poisson regression
mod <- glm(spores~km_from_forest, data=ferns, family="poisson")

#Interpret model results and parameter estimates:
coef(mod)
summary(mod)


## plot the results of your model:
xsim <- seq(from=min(ferns$km_from_forest), 
            to=max(ferns$km_from_forest), length.out=500)
ypred <- predict(mod, list(km_from_forest = xsim),type="response")
predictfern <- data.frame(ypred, xsim)
predictfern <- predictfern[order(xsim), ]

plot(ferns$spores~ferns$km_from_forest, col="purple", pch=16, xlab="distance from forest (km)",ylab= "fern spores in trap")
lines(predictfern$xsim, predictfern$ypred)

#Different way of plotting it: Just add the parameter estimates to "curve", and transform them using log-1:
plot(ferns$spores~ferns$km_from_forest, col="purple", pch=16, xlab="distance from forest (km)",ylab= "fern spores in trap")
coef(mod)
curve(exp(2.8199185-0.9438275*x),add=T)

