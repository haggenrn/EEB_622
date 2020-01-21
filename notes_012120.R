#Residuals and Prediction
#Prediction: assessing model fit 
  #Y"hat" = predicted data 
  #Y"hat"i = a+b*prediction
  #Residuals = (Yi - Y"hat"i)^2
  
  #Yhat = a+b*Water 

#Assess model fit using R^2 
  #R^2 = "explained" variance/total variance in data 
  #value is useful because it is between 0 and 1 and represents variance explained by the model!! 



#Pull up code from last week to use as example for this class. I copied and pasted it below: 

#how to set working directory# 
setwd("~/Dropbox/spring_2020/622")

#read in the dataset
catfish= read.csv("catfish_water.csv")

#create a histogram for fish_weight 
hist(catfish$fish_weight)

#plot the relationship between catfish weight and water quality data 
plot(catfish$fish_weight~catfishr$water_quality)

#now we want to estimate the paramters in the model
#use the EYEBALL APPROACH: us curve function 
#curve(0+1*x,add=T)
#the best line I found was: 
curve(20+5*x,add=T)

#intercept: fish weight when water quality is 0
#slope: increase in fish weight with an incrase of 1 unit in water quality 

#why would this be useful for interence?
#we could use this to predict fish weight 

#For water quality in 5 new ponds (0.01, 0.1, 0.2, 0.5, 1.0), calculate fish weight
#Make and equation in R that feeds our curve equation this values...
intercept = 20
slope = 5
newponds <- c(0.01, 0.1, 0.2, 0.5, 1.0)
prediciton = (intercept+slope*newponds)

#now we want to calculate how well this actually fits:

catfish$yhat = (intercept+(slope*catfish$water_quality))

#catfish$residuals = (catfish$yhat - catfish$fish_weight)^2

#RSS = sum(catfish$residuals)

SSy = sum((catfish$fish_weight-mean(catfish$fish_weight))^2)

RSS = sum((catfish$fish_weight-yhat)^2)

Rsquared = ((SSy-RSS)/SSy)          

#criticisms of R^2
  #can be high when model is wrong or variables couldn't possibly explain eachother
  #doesn't incorporate new values 
  #says little about the prediction error (if you were to change the scale of the dataset, you would change your R^2 value)
  #hard to compare between transformed and untransformed variables 
#IS THERE ANOTHER OPTION?? YES -> RMSE

#Root mean squared error = RMSE
  #doesn't actually incorporate all variance in the data
  #"how close is the model fit to the actual line?" 

#Develop a function in R:

#rmse = function(yhat,catfish$fish_weight) {
   # sqrt(mean((catfish$fish_weight - yhat)^2,
  #  na.rm=T))
#}
