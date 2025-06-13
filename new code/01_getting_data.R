rm(list=ls()) # clear environment

# install.packages(c("rio", "data.table", "ggplot2","dplyr","tidyverse"))
library(rio)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)

#-----------------------------------------------------------------------------------------------------

## wimbledon 2019

wimbledon_2019_matches <- as.data.table(read.csv("../data/raw_data/2019-wimbledon-matches.csv"))
# names(wimbledon_2019_matches)
colSums(is.na(wimbledon_2019_matches))

wimbledon_2019_points <- as.data.table(read.csv("../data/raw_data/2019-wimbledon-points.csv"))
# names(wimbledon_2019_points)
colSums(is.na(wimbledon_2019_points))

# remove all cols with NAs
wimbledon_2019_matches <- wimbledon_2019_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2019_matches)

wimbledon_2019_points <- wimbledon_2019_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2019_points)

wimbledon_2019 <- left_join(wimbledon_2019_points, wimbledon_2019_matches, by = "match_id")
names(wimbledon_2019)
colSums(is.na(wimbledon_2019))

# binary variable: whether serving player won or lost
wimbledon_2019 <- wimbledon_2019 %>%
  filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>% 
  mutate(serving_player_won = ifelse((ServeNumber == 1 & PointWinner == 1) | (ServeNumber == 2 & PointWinner == 2), 1, 0))


write.csv(wimbledon_2019, "../data/wimbledon_2019_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## wimbledon 2021

wimbledon_2021_matches <- as.data.table(read.csv("../data/raw_data/2021-wimbledon-matches.csv"))
# names(wimbledon_2021_matches)
colSums(is.na(wimbledon_2021_matches))

wimbledon_2021_points <- as.data.table(read.csv("../data/raw_data/2021-wimbledon-points.csv"))
# names(wimbledon_2021_points)
colSums(is.na(wimbledon_2021_points))

# remove all cols with NAs
wimbledon_2021_matches <- wimbledon_2021_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2021_matches)

wimbledon_2021_points <- wimbledon_2021_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2021_points)

wimbledon_2021 <- left_join(wimbledon_2021_points, wimbledon_2021_matches, by = "match_id")
names(wimbledon_2021)
colSums(is.na(wimbledon_2021))

# binary variable: whether serving player won or lost
wimbledon_2021 <- wimbledon_2021 %>%
  filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>% 
  mutate(serving_player_won = ifelse((ServeNumber == 1 & PointWinner == 1) | (ServeNumber == 2 & PointWinner == 2), 1, 0))


write.csv(wimbledon_2021, "../data/wimbledon_2021_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## wimbledon 2022

wimbledon_2022_matches <- as.data.table(read.csv("../data/raw_data/2022-wimbledon-matches.csv"))
# names(wimbledon_2022_matches)
colSums(is.na(wimbledon_2022_matches))

wimbledon_2022_points <- as.data.table(read.csv("../data/raw_data/2022-wimbledon-points.csv"))
# names(wimbledon_2022_points)
colSums(is.na(wimbledon_2022_points))

# remove all cols with NAs
wimbledon_2022_matches <- wimbledon_2022_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2022_matches)

wimbledon_2022_points <- wimbledon_2022_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2022_points)

wimbledon_2022 <- left_join(wimbledon_2022_points, wimbledon_2022_matches, by = "match_id")
names(wimbledon_2022)
colSums(is.na(wimbledon_2022))

# binary variable: whether serving player won or lost
wimbledon_2022 <- wimbledon_2022 %>%
  filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>% 
  mutate(serving_player_won = ifelse((ServeNumber == 1 & PointWinner == 1) | (ServeNumber == 2 & PointWinner == 2), 1, 0))


write.csv(wimbledon_2022, "../data/wimbledon_2022_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## wimbledon 2023

wimbledon_2023_matches <- as.data.table(read.csv("../data/raw_data/2023-wimbledon-matches.csv"))
# names(wimbledon_2023_matches)
colSums(is.na(wimbledon_2023_matches))

wimbledon_2023_points <- as.data.table(read.csv("../data/raw_data/2023-wimbledon-points.csv"))
# names(wimbledon_2023_points)
colSums(is.na(wimbledon_2023_points))

# remove all cols with NAs
wimbledon_2023_matches <- wimbledon_2023_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2023_matches)

wimbledon_2023_points <- wimbledon_2023_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2023_points)

wimbledon_2023 <- left_join(wimbledon_2023_points, wimbledon_2023_matches, by = "match_id")
names(wimbledon_2023)
colSums(is.na(wimbledon_2023))

# binary variable: whether serving player won or lost
wimbledon_2023 <- wimbledon_2023 %>%
  filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>% 
  mutate(serving_player_won = ifelse((ServeNumber == 1 & PointWinner == 1) | (ServeNumber == 2 & PointWinner == 2), 1, 0))


write.csv(wimbledon_2023, "../data/wimbledon_2023_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------

## wimbledon 2024

wimbledon_2024_matches <- as.data.table(read.csv("../data/raw_data/2024-wimbledon-matches.csv"))
# names(wimbledon_2024_matches)
colSums(is.na(wimbledon_2024_matches))

wimbledon_2024_points <- as.data.table(read.csv("../data/raw_data/2024-wimbledon-points.csv"))
# names(wimbledon_2024_points)
colSums(is.na(wimbledon_2024_points))

# remove all cols with NAs
wimbledon_2024_matches <- wimbledon_2024_matches %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2024_matches)

wimbledon_2024_points <- wimbledon_2024_points %>%
  select(where(~ all(!is.na(.))))
names(wimbledon_2024_points)

wimbledon_2024 <- left_join(wimbledon_2024_points, wimbledon_2024_matches, by = "match_id")
names(wimbledon_2024)
colSums(is.na(wimbledon_2024))

# binary variable: whether serving player won or lost
wimbledon_2024 <- wimbledon_2024 %>%
  filter(PointServer != 0, Speed_KMH != 0, Speed_MPH != 0) %>% 
  mutate(serving_player_won = ifelse((ServeNumber == 1 & PointWinner == 1) | (ServeNumber == 2 & PointWinner == 2), 1, 0))


write.csv(wimbledon_2024, "../data/wimbledon_2024_combined.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------