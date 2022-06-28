# wiffle_ball_model.r
# Following an example from Chapter 8 of Robert Gramacy's excellent book on surrogate modeling using 
# Gaussian Processes (GPs). Calibtration of computer model for acceleration of falling object using 
# both field data (ball.csv) and computer model data. 
#
# Author: Andrew Roberts (code copied from Robert Gramacy's book, but with some additions and 
#                         slightly modified for my tastes). 
# Working Directory: statistical-modeling-practice

library(data.table)

# Field experiment data: how long for wiffle ball to hit ground, dropped from varying heights. 
# Note that from basic physics, when neglecting air resistance we would expect the relationship
# t = sqrt(2/g * h), where h is the initial height (taking 0 to be the ground and up to be the 
# positive direction) and we'd expect g to be about 9.8 m/s^2.
dt <- fread(file.path('data', 'ball.csv'))
plot(dt, xlab="height", ylab="time")

# Basic linear regression to estimate gravitational acceleration g. If we regress t on 
# sqrt(h) without an intercept, then we can estimate g by setting beta = sqrt(2/g) and solve to get 
# estimate g = 2/beta^2. 
reg1 <- lm(time ~ -1 + sqrt(height), data = dt)
g_estimate <- 2 / reg1$coefficients[1]^2
print(g_estimate) # Not 9.8, but in the ballpark - effect of air resistance unsurprisingly substantial for wiffle ball
