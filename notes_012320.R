                #PART ONE:

#Running linear regressions in R (a simpler method than the previous lectures)
  #For example:

    #model1 <- lm(response~predictor,
                       #data=dataset)
    #coef(model1)

#now run a linear regression using the catfish data! 

#how to set working directory# 
#setwd("~/Dropbox/spring_2020/622")

#read in the dataset
#catfish= read.csv("catfish_water.csv")

#create a histogram for fish_weight 
#hist(catfish$fish_weight)

#plot the relationship between catfish weight and water quality data 
#plot(catfish$fish_weight~catfish$water_quality)

#try and run a linear regression using the first line of code in the notes: 

#catfish_model <- lm(catfish$fish_weight~catfish$water_quality,data = catfish)
#plot(catfish_model) #visualize the linear regression




#coef(catfish_model) #prints the intercept and slope of the regression, how does it compare to our own values from 011620?
#summary(catfish_model) #output of linear regression. Important to look at parameter estimates! Notice how the liner regression R^2 is .902 and the one I came up with was .901 (SO CLOSE!). also take a look at the p-value...

                  #PART TWO:

# P-value and what they mean and why they are important 
  #falsification of hypotheses
  #how do you go about getting a p-value? 
#definition:
  #"the probability that the observed differences in your dataset would be found in the null hypothesis is true" 

                                #OR 

  #"probability of falsely rejecting the null hypothesis (Type I error)

#Depends on 3 things: (1) number of observations in the sample, (2) difference between means of the samples, (3) level of variation in the data (the sigma)

#Monte Carlo analysis: a non-parametric approach, best for non-normally distributed data
    #shuffling data and redistributing it to see how different versions of the data could play out
    #let's you come up with a null hypothesis using randomization

#Steps for in class example: hypothesis "blue light attracts more insects than red light"
    #Mean(Blue light)-Mean(Red light)=DIFobs

    #Create a null distribution: shuffle cards and split into group of 6 and 4
      #run 1
        #mean red = 9.33 and blue mean = 7.27, difference = 2.06
      #run 2
        #mean blue = 8 and mean red=8.83, difference = 0.83

#we would need to repeat this over and over and over again and see how many times you get the same difference in means that you got from your expiriment!!!


#statistical significance: the probability of the data occuring due to random chance is p-value less that 0.05 (arbitrary threshold, comes from agriculture)
#p-values in a frequentist, parametric context: assumes your data is normally distributed




#how else do we communicate uncertainty in our models?
    #confidence intervals: numerical intervals constructed around the estimate of parameter ("if we were to do this expiriment again and again and again, 95% of the values would land within that bar, the confidence intervals contain the true mean")
          #assessment of the process, not the values...
#Hypothesis testing with confidence intervals: 
    #What is the null hypothesis for slope parameter in least squares? Slope = 0 

#calculate confidence intervals for paramters in catfish linear regression - using confint()
#confint(catfish_model)
    #would we reject or accept the null hypothesis given the output for confint()?  

                  #PART THREE:

#Data simulation!!! 
#rnorm(...)
#args(rnorm) function (n, mean = 0, std = 1)

#generate predictor variable
#water_quality <- runif(100, min=0, max=1)

#create slope, intercept, sigma
#slope <-5
#intercept <-20
#sigma <-0.2

#fish_weight <- rnorm(n,
     # mean=intercept+slope*water_quality, sd=0.2)


#simulate data with 3 values of sigma and plot each 

water_quality_sim <- (runif(100, min = 0, max = 1))
slope <- 5
intercept <- 20
sigma <- 14.0
fish_weight_sim <- rnorm(100,
    mean = intercept+slope*water_quality_sim, sd=0.2)

plot(fish_weight_sim~water_quality_sim)
curve(intercept+slope*x, add = T)


