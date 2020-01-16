# In class exercise 

#how to set working directory# 
setwd("~/Dropbox/spring_2020/622")

#read in the dataset
catfish_water= read.csv("catfish_water.csv")

#create a histogram for fish_weight 
hist(catfish_water$fish_weight)

#plot the relationship between catfish weight and water quality data 
plot(catfish_water$fish_weight~catfish_water$water_quality)

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


