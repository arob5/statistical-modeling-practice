# stan_planetary_motion_case_study.r
# Planetary motion example for rstan from here: 
# https://mc-stan.org/users/documentation/case-studies/planetary_motion/planetary_motion.html
#
# Working Directory: statistical-modeling-practice

library(cmdstanr)
library(posterior)
library(ggplot2)
library(dplyr)
library(plyr)
library(tidyr)
library(boot)

set.seed(1954)

mod <- cmdstan_model("planetary_motion_sim.stan")
