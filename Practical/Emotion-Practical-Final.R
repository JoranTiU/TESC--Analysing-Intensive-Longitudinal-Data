#==============================================================================#
# Emotions 2023:
#==============================================================================#


### Packages required for Practical --------------------------------------------


#install.packages("ggplot2")
#install.packages("lmtest")
#install.packages("brms")
#install.packages("lme4")
#install.packages("tidyverse")
#install.pachages("tidybayes")
#install.packages("mlVAR")

library(ggplot2)
library(lmerTest)
library(brms)
library(lme4)
library(tidyverse)
library(tidybayes)
library(mlVAR)


######################### Part 1: Nested Data ##################################

#------------------------------------------------------------------------------#
## Exercise 1 Nested Data
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#
# These assignments are about a study on the popularity of highschool students.
# A total of 246 students from 12 different classes took part in this study in 
# which we determined how extraversion, gender, and teacher experience 
# influenced a student's popularity. The data of this study show a clear 
# hierarchy - or nesting - , with students nested in classes. Since observations
# of students from the same class are most likely not independent, we can't 
# just analyse these data using normal linear regression. In addition, we are 
# dealing with variables on different levels. We have pupil level variables 
# like extraversion and gender, but we also have class characteristics like 
# teacher experience. Only with multilevel analysis can we estimate both types 
# of variables in a single analysis. 
#------------------------------------------------------------------------------#

#### Assignment 1 --------------------------------------------------------------
# Go through all the steps of a multilevel analysis. Calculate the ICC and the 
# amount of explained variance in each step.

# Start with the intercept only model in which you use Popular as the dependent 
# variable, and calculate the ICC. Also determine if multilevel analysis is 
# necessary. If needed ask for instruction using ?lme

IO <- lm(Popular ~ 1, Total)
IO_ML <- lmer(Popular ~ 1 + (1 | Class), Total)
rand(IO_ML)# Or, instead of rand(), you can use:
anova(IO_ML,IO)

summary(IO_ML)
# The ICC is equal to 0.223/(0.223 + 1.016) = 0.18

# Now, add the first level variables extraversion and gender to the model as 
# fixed effects. What is the explained variance on level 1 and level 2, what 
# "weird result" do you notice? 
# What could cause this?
# What are your conclusions about these predictors?

Lvl1 <- lmer(Popular ~ 1 + Gender + Extraversion + (1 | Class), Total)

summary(Lvl1)# Both predictors are significantly related to Popularity

# Now, we calculate the explained variances on Level 1 and 2:
VarianceIO <- as.data.frame(VarCorr(IO_ML))
VarianceLv1 <- as.data.frame(VarCorr(Lvl1))

# Explained Variance on Level 1
(VarianceIO[2,4] - VarianceLv1[2,4])/ VarianceIO[2,4]
# .5183

# Explained Variance on Level 2
(VarianceIO[1,4] - VarianceLv1[1,4])/ VarianceIO[1,4]
# -.2370, explained variance can't be negative! This is likely because there is 
# less variance between classes in either extraversion, gender, or both, than 
# would be expected based on random sampling.

# Now, add the second level variable teacher experience to the model.
# What is the explained variance on level 1 and level 2, and what are your 
# conclusions about this predictor?

FixedEffects <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                       (1 | Class), Total)
summary(FixedEffects)# Teacher experience is not significantly related to popularity.

VarianceFE <- as.data.frame(VarCorr(FixedEffects))

# Explained Variance on Level 1 (compared to model with just level 1 predictors)
(VarianceLv1[2,4] - VarianceFE[2,4])/ VarianceLv1[2,4]
# 0. No explained variance since you only added a level 2 predictor

# Explained Variance on Level 2 (compared to model with just level 1 predictors)                                   
(VarianceLv1[1,4] - VarianceFE[1,4])/ VarianceLv1[1,4]
# .1662

# Now, check if the relation between the first level predictors and popularity 
# is the same across classes. What type of effect do you need to add for testing 
# this hypothesis? And is this effect significant?

# You need to add random slopes.
RandomEffectsGender <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                              (1 + Gender | Class), Total)
anova(RandomEffectsGender, FixedEffects)

RandomEffectsExtraversion <- lmer(Popular ~ 1 + Gender + Extraversion + 
                                    teacherExp + 
                                    (1 + Extraversion | Class), Total)
anova(RandomEffectsExtraversion, FixedEffects)# Or, use
rand(RandomEffectsExtraversion)

# Neither level 1 predictor has a random slope.

# Finally, decide if you need to add a cross-level interaction?
# No! There is no random slope, so you don't need to add a cross-level 
# interaction.

# What happens if you do add a cross-level interaction when there is no
# random slope?


#### Assignment 2 --------------------------------------------------------------
# We choose the FixedEffects model as the final model since there are no random       
# slopes. Check if the assumptions for the model are met .        

# For level 1
hist(residuals(FixedEffects))
qqnorm(residuals(FixedEffects))

# For level 2
hist(ranef(FixedEffects)$Class[,1])
qqnorm(ranef(FixedEffects)$Class[,1])

#### Assignment 3 --------------------------------------------------------------
# In Assignment 1, we built the model step by step. As mentioned in 
# the lecture, this is one way of running an analysis. Alternatively, you can 
# run the full model immediately. This is called the maximum approach.
# Run the full model in one go. Does your conclusion differ from the conclusion
# you drew based on Assignment 1?

MaximumModel <- lmer(Popular ~ 1 + Gender + Extraversion +                      
                       teacherExp + 
                       (1 + Gender + Extraversion | Class), Total)
summary(MaximumModel)

# The maximum approach can be difficult for frequentist estimation because of
# the complexity of the model. This is why I prefer to use Bayesian statistics
# with it. It's more stable, and if there is a problem, it's easier to pinpoint.

# This model might take a long time to run, so we already added it to the 
# workspace
#MaximumModel_Bayes <- brm(Popular ~ 1 + Gender + Extraversion + 
#                            teacherExp + 
#                            (1 + Gender + Extraversion | Class), 
#                          control = list(adapt_delta = 0.9), Total)
summary(MaximumModel_Bayes)

# Conclusions with respect to fixed effects are similar. There is an effect
# of gender and extraversion but not teacherExp.

# Now for the random part:

# Get names of parameters in the model
get_variables(MaximumModel_Bayes)

# Extract entire distribution of the random-intercept variance
SdInt<- MaximumModel_Bayes %>%
  spread_draws(sd_Class__Intercept)

# Check the probability the the intercept variance is smaller than the smallest
# value of interest (e.g., .02)
sum((SdInt$sd_Class__Intercept)^2 < .02)/length(SdInt$sd_Class__Intercept)
# 0% chance that the intercept-variance is smaller than .02, so there is
# definitely variance in the intercept.

# Extract entire distribution of the random-slope for gender
SdGender <- MaximumModel_Bayes %>%
              spread_draws(sd_Class__Gender)

# Check the probability the the slope variance is smaller than the smallest
# value of interest (e.g., .02)
sum((SdGender$sd_Class__Gender)^2 < .02)/length(SdGender$sd_Class__Gender)
# approx. 67% chance that the slope-variance is smaller than .02

# Extract entire distribution of the random-slope for extraversion
SdExtra <- MaximumModel_Bayes %>%
              spread_draws(sd_Class__Extraversion)

# Check the probability the the slope variance is smaller than the smallest
# value of interest (e.g., .02)
sum((SdExtra$sd_Class__Extraversion)^2 < .02)/
  length(SdExtra$sd_Class__Extraversion)
# approx. 40% chance that the slope-variance is smaller than .02

# So random slope variances are really small, which is why we won't model
# them further.                                                                

#################### Part 2: Common Issues, Small Level 2 N ####################

#------------------------------------------------------------------------------#
### Exercise 4a Small level 2 N: Fixed Effects Models---------------------------
#------------------------------------------------------------------------------#

#### Assignment 1 --------------------------------------------------------------
# Let's first run an multilevel analysis on data with only 3 level 2 units.
# For this we'll just use the data of the first three classes from the 
# Total data.
Multilevel_SN <- lmer(Popular ~ 1 + Gender + Extraversion + 
                        (1 | Class), Total_SN)                                 

summary(Multilevel_SN)
# Why is the above analysis not recommended?
# The level 2 variance is calculated on only 3 observations.

# Now, let's analyse the data using a fixed effects model
FE_SN <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion, 
            data = Total_SN)
summary(FE_SN)

# What does the "0" do in the code above?  
# We use this approach to get estimates of the means of each class in our
# output.

#### Assignment 2 --------------------------------------------------------------
# Now, let's add a level 2 predictor for both the multilevel analysis and the     
# fixed effect model.

# Multilevel model
Multilevel_SN2 <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                         (1 | Class), data = Total_SN)
summary(Multilevel_SN2)

# Fixed effects model
FE_SN2 <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion + teacherExp,
             data = Total_SN)
summary(FE_SN2)

# What do you notice in the fixed effects model?
# There is no estimate for teacherExp.

# Can you explain why?
# The dummies capture all level 2 differences, including the ones caused by
# differences in teacherExp, so the predictor is perfectly colinear with the
# class dummies. In other words, all differences between classes due to 
# differences in teacherExp are already accounted for by the differences
# between the estimated means of each class. Adding the teacherExp
# to the model therefore is like adding the same predictor twice.

#### Assignment 3 --------------------------------------------------------------
#Now, let's add cross-level interaction.

# Multilevel model
Multilevel_SN3 <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                         Extraversion:teacherExp + (1 + Extraversion | Class), 
                       Total_SN)
summary(Multilevel_SN3)

# Fixed effects model
FE_SN3 <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion  
             + Extraversion:teacherExp, data=Total_SN)
summary(FE_SN3)

# Let's also run both options for the cross-level interaction on the larger 
# dataset with 12 classes.                                                                  

# Multilevel model
Multilevel_SN4 <- lmer(Popular ~ 1 + Gender + Extraversion + teacherExp + 
                         Extraversion:teacherExp + (1 + Extraversion | Class), 
                       Total)
summary(Multilevel_SN4)
# Fixed effects model
FE_SN4 <- lm(Popular ~ 0 + factor(Class) + Gender + Extraversion                
             + Extraversion:teacherExp, data=Total)
summary(FE_SN4)

# Notice that we add an interaction term between Extraversion and teacherExp 
# without also adding the main effect of teacherExp (explicitly). This is weird 
# and something you normally should not do! But...the main effect of
# teacherExp is in the model! Remember that it is part of the difference
# between the means of each class.

# Compare the individual intercepts of the multilevel model to the dummy scores
# from the fixed effect model. What do you notice?
InterceptsML <- coef(Multilevel_SN4)$Class[,1]
InterceptsFE <- as.numeric(FE_SN4$coefficients[1:12])

cor(InterceptsML, InterceptsFE)
InterceptsFE - InterceptsML

# The values from the FE model are larger because it doesn't partially pool,
# but the rank ordering is pretty similar.

######################### Part 3: Longitudinal Data ############################
#------------------------------------------------------------------------------#
# The dataset "gpa"  contains longitudinal data set on 200 college students. 
# The students' grade point average (GPA) has been recorded for six successive 
# semesters. At the same time, it was recorded whether the student held a job 
# in that semester, and for how many hours. This is recorded in a variable 
# 'job' (= hours worked). Finally, we also have the student-level variables 
# high school GPA and gender (0 = male, 1 = female).
#------------------------------------------------------------------------------#

as_tibble(gpa)

#### Assignment 1 --------------------------------------------------------------

# Now, let's approach this like we normally would. Start with figures.

# J-P and Niall plots
ggplot(data      = gpa,
       aes(x     = time,
           y     = gpa,
           col   = student,
           group = student))+ #to add the colors for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_gradientn(colours = rainbow(100))+
  geom_smooth(method = lm,                                                      
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+ # to add regression line
  labs(title    = "GPA vs. Time",
       subtitle = "add colours for different students and regression lines")


# Some variation in time evident
ggplot(data      = gpa,
       aes(x     = job,
           y     = gpa,
           col   = student,
           group = student))+ #to add the colors for different classes
  geom_point(size     = 1.2,
             alpha    = .8,
             position = "jitter")+ #to add some random noise for plotting
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_gradientn(colours = rainbow(100))+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+ # to add regression line
  labs(title    = "GPA vs. Job",
       subtitle = "add colours for different students and regression lines")

# Something is clearly weird with job. It appears to be an ordinal variable 
# more than a continuous one. There is also some evidence of different slopes.  
# We've been assuming job has a monotone effect but let's do it properly

gpa$job_cat <- as.ordered(gpa$job)

#### Assignment 2 --------------------------------------------------------------
# Maximum approach: Run the full model in one go (using the ordinal variable    
# job_cat). Start with checking the assumptions. 


# This model might take a long time to run, so the output is already added to
# the workspace
#MaximumModel_Bayes_GPA <- brm(gpa ~ 1 + time + mo(job_cat) + sex + highgpa + 
#                            admitted +(1 + time + mo(job_cat) | student), 
#                          iter = 5000,
#                          control = list(adapt_delta = 0.8), gpa)

# Check level 1 Residuals
hist(residuals(MaximumModel_Bayes_GPA)[,1])
qqnorm(residuals(MaximumModel_Bayes_GPA)[,1])

# Check level 2 Residuals
hist(ranef(MaximumModel_Bayes_GPA)$student[,,1][,1])
hist(ranef(MaximumModel_Bayes_GPA)$student[,,2][,1])
hist(ranef(MaximumModel_Bayes_GPA)$student[,,3][,1])

qqnorm(ranef(MaximumModel_Bayes_GPA)$student[,,1][,1])
qqnorm(ranef(MaximumModel_Bayes_GPA)$student[,,2][,1])
qqnorm(ranef(MaximumModel_Bayes_GPA)$student[,,3][,1])

# Looks okay. Now, look at results.
summary(MaximumModel_Bayes_GPA)                                                     

# There really is hardly    
# any variation in effect of job. But we could now add cross-level interactions
# for time.


####################### Part 4: Growthcurve and AR #############################
#------------------------------------------------------------------------------#
## Exercise 1a Longitudinal Data------------------------------------------------
#------------------------------------------------------------------------------#
# For speed purposes, let's only focus on time and highgpa for the following
# analyses :)

# First, let's run a normal growth model
GrowthModel_Bayes <- brm(gpa ~ 1 + time + highgpa +
                           (1 + time | student), 
                         iter = 2000,
                         control = list(adapt_delta = 0.8), gpa)

summary(GrowthModel_Bayes)


# And now one with AR residuals. What do you notice?

GrowthModel_Bayes_ARRes <- brm(gpa ~ 1 + time + highgpa + ar(p = 1) +
                                 (1 + time | student), 
                               iter = 2000,
                               control = list(adapt_delta = 0.8), gpa)

summary(GrowthModel_Bayes_ARRes)                                                

# There is no mention of the AR at all!! Because it's considered a nuisance.     
# It is there though! All other results are pretty similar. This is because 
# there isn't a lot of autocorrelation. We can see that as follows:

mean(GrowthModel_Bayes_ARRes$fit@sim$samples[[4]]$`ar[1]`) # .07
sd(GrowthModel_Bayes_ARRes$fit@sim$samples[[4]]$`ar[1]`) # .08

# So, we see there is little autocorrelation in this data (which makes sense 
# given the sampling frequency in the data). For Intensive longitudinal data
# the AR will tend to be higher :).

# If we want to explicitly model the AR effect, we need to lag the dependent
# variable and add it as a predictor.

gpa <- gpa %>%
  group_by(student) %>%
  mutate(gpa_L = lag(gpa, order_by=student))


GrowthModel_Bayes_AR <- brm(gpa ~ 1 + time + highgpa + gpa_L +                  
                              (1 + time | student), 
                            iter = 5000,
                            control = list(adapt_delta = 0.8), gpa)

summary(GrowthModel_Bayes_AR)

# Note that the AR value here and in the previous model don't match....that's
# because we are fitting a very "weird" model called the ALT-model (and adding 
# level 2 predictors on top of that). In this model the interpretation of
# parameters is complicated! I wrote an article on it with Ellen Hamaker
# years ago, but the main message is: Be very careful when modeling systematic
# change and AR relationships in one model, and maybe see a statistician if
# you really want to ;).

#------------------------------------------------------------------------------#
### Exercise 1b Intensive Longitudinal Data ------------------------------------
#------------------------------------------------------------------------------#

# Data on 2 variables for 100 individuals each measured 50 times.
# The means of the 2 variables are 4 and 4.5 respectively, with SEs of .5.
# The correlation between the two variables is .3. The AR parameters are .4 and
# .3 respectively, while the two lagged effects are .2 and .1. All lagged
# parameters have SEs of .1. The residuals are standard normally distributed.

# Let's start with a growth curve model again

# This model takes a longtime to run, so the results are already in the
# R workspace. The code is the following:

# GrowthModel_Bayes_ARRes2 <- brm(Y1 ~ 1 + time + ar(p = 1) +
#                                   (1 + time | individual), 
#                                 iter = 2000,
#                                 control = list(adapt_delta = 0.8), VARData)

summary(GrowthModel_Bayes_ARRes2)

mean(GrowthModel_Bayes_ARRes2$fit@sim$samples[[4]]$`ar[1]`) # .51
sd(GrowthModel_Bayes_ARRes2$fit@sim$samples[[4]]$`ar[1]`) # .03

# Clearly there is no systematic change over time, but there is a substantial
# amount of autocorrelation. Since there is no trend, we can use AR models.
# If there was a trend, we would have to "get rid of it" first, by subtracting
# the predicted scores from the observed ones. Kind of like centering, but now  
# with predicted scores (based on the regression line) instead of simple means.
# If you don't do this, the estimated AR parameter will be biased. 

# Now, let's use a simple AR model again. We'll use variable Y1. 

AR1 <- brm(Y1 ~ Y1lag + (1 + Y1lag | individual), 
           iter = 5000, data = VARData)
summary(AR1)

# You'll see that the intercept isn't close to the actual value of 4!
# This is because the intercept in AR models can't be interpreted as a mean.    
# If we want means, we need to group-mean center.

VARData <- VARData %>%
  group_by(individual) %>%
  mutate(Y1lag_c = Y1lag - mean(Y1lag, na.rm=T),
         Y2lag_c = Y2lag - mean(Y2lag, na.rm=T))


# Let's rerun the model with group-mean centered predictor now

AR1_c <- brm(Y1 ~ Y1lag_c + (1 + Y1lag_c | individual ), 
             iter = 5000, data = VARData)
summary(AR1_c)

# Much better ;). Although using sample means to group mean center isn't ideal
# We can use estimates of the individual means, but then we have to write
# our own code...or you should come see me ;)


# Now, we typically want to model several longitudinal variables at the same 
# time. This can be done with VAR models, which are multivariate
# AR models.

# First, we create a model to model Y1 and Y2 simultaneously! We use the mvbind
# command for that. We also let the intercepts vary across individuals. We use
# the "p" notation because we want the random intercepts for both variables to
# also be correlated with each other. It's likely that if you score higher on 
# one, you score higher on the other for example.

bform1 <- 
  bf(mvbind(Y1, Y2) ~ Y1lag_c + Y2lag_c + (1|p|individual))

# This model takes some time to run, so the result is already included ;)
#VAR <- brm(bform1, data = VARData, iter = 5000, chains = 2, cores = 2)
summary(VAR)

# Finally, let's look at longitudinal network models....and let's just use 
# our VAR data. This could be seen as a network of only two nodes after 
# all. Do you see a difference between the network and the VAR model?

LongNet <- mlVAR(VARData[, c(1:3)], vars = c("Y1", "Y2"), 
                 idvar = c("individual"), 
                 lags = 1, temporal = "correlated")

summary(LongNet)
plot(LongNet)

# The network model isn't doing "true" multilevel! It's calculating person 
# means based on the sample data and subtracting those from the observations
# to person-center the data. Then, a network is fitted to the group-means to
# model level 2 (but without taking the amount of observations an individual 
# has into account. A mean of someone with 10 observations is treated the same
# as the mean of someone with 100 observations). The group-mean centered data
# is used to fit two models: a longitudinal network to lagged versions of the
# variables (think Y1lag and Y2lag) and a "within time point" or contemporaneous
# network to model relationships between variables at the same time point
# (e.g. Y1 at T1, on Y2 at T2).
# Can you tell where these 3 networks are located in the VAR output?
#
# Lagged is in the lagged effect
# Contemporaneous is the correlation between residuals
# Between is the correlation between intercepts/means


# The VAR model has some benefits over the network approach since it uses
# true multilevel analysis, and the parameters are easier to interpret (i.e.,
# we get information on "significance" etc.), but it scales less nicely to 
# larger sample sizes and larger number of variables (although there are 
# ways around that). Networks also have nicer visualization out of the box.
# Which method you choose again comes down to modeling the same data in slightly
# different (but highly similar) ways, and which methods is best depends on
# you research question etc. Even if you go for a VAR model though, networks
# are always a good place to start because of their nice visualization of the
# data (in ways relevant to VAR models).