##########################################################
##                       LIBRARIES                      ##
##########################################################

# install and load the required packages

#install.packages("brms") #For Bayesian model fitting
#install.packages("dplyr") #For data wrangling
#install.packages("plyr") #For data wrangling
#install.packages("ggplot2") #For plots
#install.packages("cowplot") #For combining multiple plots into one
#install.packages("performance") #For intraclass-correlations
#install.packages("parallel") #For automatic identification of number of cores
#install.packages("psych") #For descriptive statistics
#install.packages("bayestestR) #For highest density-intervals

library(brms)
library(dplyr)
library(plyr)
library(ggplot2)
library(cowplot)
library(performance)
library(parallel)
library(psych)
library(bayestestR)

##########################################################
##                       SETTINGS                       ##
##########################################################

# set seed for reproducible results
set.seed(123)

# plot settings
theme_set(theme_classic(base_size = 18))


##########################################################
##                      PROCESSING                      ##
##########################################################

# load data file
load("data/dt.RData")
