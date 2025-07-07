rm(list=ls())
# install.packages("fastDummies")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)
library(hms)

#-----------------------------------------------------------------------------------------------------

subset_m <- as.data.table(read.csv("out_data/wimbledon_subset_m.csv"))
subset_f <- as.data.table(read.csv("out_data/wimbledon_subset_f.csv"))

colSums(is.na(subset_m))

# model for probability of double fault:
# filter to only where ServeNumber == 2, Speed_MPH not equal to 0 if (P1DoubleFault == 1 and P2DoubleFault == 1)
subset_m_test <- subset_m %>%
  filter(ServeNumber == 2, (Speed_MPH == 0 & (P1DoubleFault == 1 | P2DoubleFault == 1)) 
         | (Speed_MPH != 0 & (P1DoubleFault == 0 & P2DoubleFault == 0))) %>%
  mutate(DoubleFault = ifelse(P1DoubleFault == 1 | P2DoubleFault == 1, 1, 0))
  