rm(list=ls())
# install.packages("welo")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)
# install.packages("car")
library(car)

#-----------------------------------------------------------------------------------------------------

subset_m <- as.data.table(read.csv("../data/usopen_subset_m.csv"))
subset_f <- as.data.table(read.csv("../data/usopen_subset_f.csv"))

names(subset_m)

#-----------------------------------------------------------------------------------------------------

# new models with just second serves
subset_m_second <- subset_m[ServeNumber == 2]
subset_f_second <- subset_f[ServeNumber == 2]

## speed_ratio linear
logit_model_5_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_second, family = "binomial")
summary(logit_model_5_m) # p_server_beats_returner significant *** (pos coef), speed_ratio significant *** (pos coef), importance significant *** (pos coef), Servewidth W *** (pos coef), Servedepth NCTL *** (neg coef)
logit_model_5_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_second, family = "binomial")
summary(logit_model_5_f) # p_server_beats_returner significant *** (pos coef), elapsed_time * (neg coef), speed_ratio significant *** (pos coef), importance significant *** (pos coef), Servewidth W *** (pos coef), Servedepth NCTL ** (neg coef)

vif(logit_model_5_m) # acceptable, all less than 1.3ish
vif(logit_model_5_f)

## quadratic and spline on speed_ratio
logit_model_9_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + I(speed_ratio**2) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_second, family = "binomial")
summary(logit_model_9_m) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), speed_ratio significant * (neg coef), speed_ratio^2 significant * (pos coef), Servewidth W *** (pos coef), Servedepth NCTL *** (neg coef)
logit_model_9_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + speed_ratio + I(speed_ratio**2) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_second, family = "binomial")
summary(logit_model_9_f) # p_server_beats_returner significant *** (pos coef), elapsed_time * (neg coef), importance significant *** (pos coef), Servewidth W *** (pos coef), Servedepth NCTL ** (neg coef)

logit_model_10_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                        data = subset_m_second, family = "binomial")
summary(logit_model_10_m) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), splines::bs(speed_ratio, degree = 3, df = 5)5 * (pos coef), Servewidth W *** (pos coef), Servedepth NCTL *** (neg coef)
logit_model_10_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(speed_ratio, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                        data = subset_f_second, family = "binomial")
summary(logit_model_10_f) # p_server_beats_returner significant *** (pos coef), elapsed_time * (neg coef), importance significant *** (pos coef), Servewidth W *** (pos coef), Servedepth NCTL *** (neg coef)

#-----------------------------------------------------------------------------------------------------

## use serve speed instead of serve ratio

logit_model_6_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_second, family = "binomial")
summary(logit_model_6_m) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), Speed_MPH significant *** (pos coef), Servewidth W *** (pos coef), Servedepth NCTL *** (neg coef)
logit_model_6_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_second, family = "binomial")
summary(logit_model_6_f) # p_server_beats_returner significant *** (pos coef), elapsed_time * (neg coef), importance significant *** (pos coef), Speed_MPH significant *** (pos coef), Servewidth W *** (pos coef), Servedepth NCTL ** (neg coef)

logit_model_7_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + I(Speed_MPH**2) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_second, family = "binomial")
summary(logit_model_7_m) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), Speed_MPH significant * (neg coef), Speed_MPH^2 significant * (pos coef), Servewidth W *** (pos coef), Servedepth NCTL *** (neg coef)
logit_model_7_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + Speed_MPH + I(Speed_MPH**2) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_second, family = "binomial")
summary(logit_model_7_f) # p_server_beats_returner significant *** (pos coef), elapsed_time * (neg coef), importance significant *** (pos coef), Servewidth W *** (pos coef), Servedepth NCTL ** (neg coef)

logit_model_8_m <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(Speed_MPH, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_m_second, family = "binomial")
summary(logit_model_8_m) # p_server_beats_returner significant *** (pos coef), importance significant *** (pos coef), splines::bs(Speed_MPH, degree = 3, df = 5)3 * (pos coef), splines::bs(Speed_MPH, degree = 3, df = 5)5 ** (pos coef), Servewidth W *** (pos coef), Servedepth NCTL *** (neg coef)
logit_model_8_f <- glm(serving_player_won ~ p_server_beats_returner + ElapsedSeconds_fixed + importance + splines::bs(Speed_MPH, degree = 3, df = 5) + factor(ServeWidth) + factor(ServeDepth), 
                       data = subset_f_second, family = "binomial")
summary(logit_model_8_f) 

#-----------------------------------------------------------------------------------------------------
