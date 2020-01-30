#generalied linear model (GLM) = a way of modeling data linearlly if the data isn't actually normally distributed 
  #binary data (0 and 1, counts of successes in groups)
    #survival/mortality
    #occupancy
    #whether a pixel is forest or not forest, water or not 
#binomial regression is a GLM that replaces the normal distribution part of the linear model with the binomial dist.
#Binomial dist. is hsaped by the parameters probability (p), number of trials (n), number of successes in those trials (X)

#today we will visualize how the logistic "link" function helps a B model 

  #how does owl occupancy of nest boxes relate to distance from the road?
    #p=probability that nest box is occupied by a barn owl
    #predictor variable is distance from the road 
  
  # p(box occupied)_ = intercept+slope*distance from road
  
#p has to be between 0 and 1...how do you constrain p then? TRANSFORM THE P OF THE MODEL INTO LOG ODDS, so linear part of model can range from -infinity to +inf.
    #what do odds mean? What do log odds mean?
        #odds are probability transformed into something not constrained by 0 and 1

#log(p/(1-p)) = intercept+slope*varaible
  #p = (exp(intercept+slope*x)/1+exp(intercept+slope*x))

#p = (exp(x)/(1+exp(x)) is the LOGISTIC EQUATION. Try exploring in R with a range of values...

exp(1)/(1+exp(1))
exp(4.5)/(1+exp(4.5))
exp(7.8)/(1+exp(7.8))
exp(21)/(1+exp(21))
exp(467)/(1+exp(467))
exp(0.1)/(1+exp(0.1))

#even with big numbers and small numbers, this equation keeps our values between 0 and 1 
#SEE??? 

curve(exp(x)/(1+exp(x)),from = -100,to = 100)

#here is a simple equation for the binomial logsitic function:
#plogis(x)






#...how does a binomial model help us find the effect of roads on probability of nest occupancy?
  #load owl box dataset (simulated data that represent the occupancy of a series of nest boxes, across a gradient of distance from a major road)
  #plot occupancy~road

library(readr)
owlbox <- read_csv("Dropbox/spring_2020/622/datasets/owlbox.csv")
View(owlbox)

plot(owlbox$owl_present~owlbox$road_dist)

#is this data binomial? YES. Do you think there is a significant relationship? YES. Why? There's a lot of density of 0's close to the road, and 1's further from the road

#run binomial glm:
mod <- glm(owlbox$owl_present~owlbox$road_dist, data = owlbox,
           family = "binomial"(link = "logit")) #specifying the family of probability dist. and specifying the link function - logit - is default, so not necessary...
coef(mod)
plot(mod)

#now how to we interpret these parameter estimates? THESE NUMBERS ARE STILL ON THE LOG ODDS SCALE
#intercept is baseline own occupancy at the road (0m from road) so use plogis() function to scale probability

plogis(-3.058) #what is baseline owl occupancy? 0.04487334...the probability of an owl occupying a box right next to the road? pretty damn low. 

#how do we interpret slope? road_dist 0.03206121...it's a little trickier than linear regression...
  #divide-by-four rule: logistic curve is steepest at its center where probability is 0.5, or 0 on odds scale...but we won't write more of this down because it's not that useful
    #if you do dbf rule then divide road_dist value by 4 and get...0.008015303
      0.032061212/4
  #evaluate how probability differs between values of x...
    #logistic(a+bx1) - logistic(a+b2)
  #what is the effect of going from 10 to 20 meters on the road:
    plogis(-2.7+0.03*20)-plogis(-2.7+0.03*10)
    #you're 2% more likely to have an owl in your box if you increase the distance from road from 10 to 20
    

  #best option though? PLOT IT OUT. (copied from 'lecture6_startingcode(1).R')
    ## Plot out the prediction of the model:
    # First, create new fake data 
    roadsims <- seq(from=min(owlbox$road_dist),
                    to = max(owlbox$road_dist), 
                    length.out=nrow(owlbox))
    
    #Then ask model to predict response, using "predict" -- Change the name "mod" to whatever your glm was called.
    owlpreds <- predict(mod, 
                        list(road_dist=roadsims),
                        type="response")
    # Then plot:
    plot(owlbox$owl_present~owlbox$road_dist, pch = 16, 
         xlab = "distance from road (m)", 
         ylab = "prob of owl occupancy", col="lightblue") # Plots raw data
    lines(roadsims, owlpreds) # Plots model's prediction
    
    
    
#how do we deal with multiple trials then?
    #you can multiply the results...
    
    
    