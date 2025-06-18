## ideas:
# (done) wElo ratings vs. serve speed ratio and/or serve location (do better players perform differently?)
# elapsed_seconds vs. serve speed ratio and/or serve location (does strategy change as match goes on?)
# importance vs. serve speed ratio and/or serve location (does strategy change in important points?)

#-----------------------------------------------------------------------------------------------------

rm(list=ls())
# install.packages("fastDummies")
library(welo)
library(tidyverse)
library(data.table)
library(ggplot2)

#-----------------------------------------------------------------------------------------------------

subset_m <- as.data.table(read.csv("../data/wimbledon_subset_m.csv"))
subset_f <- as.data.table(read.csv("../data/wimbledon_subset_f.csv"))

names(subset_m)

subset_m <- subset_m %>%
  filter(ServeDepth != "", ServeWidth != "")

subset_f <- subset_f %>%
  filter(ServeDepth != "", ServeWidth != "")

subset_m_second <- subset_m[ServeNumber == 2]
subset_f_second <- subset_f[ServeNumber == 2]

#-----------------------------------------------------------------------------------------------------

# wElo ratings vs. serve speed ratio and/or serve location (do better players perform differently?)

m_second_welos <- subset_m_second %>% 
  mutate(server_welo = if_else(PointServer == 1, player1_avg_welo, player2_avg_welo)) %>%
  select(server_welo, speed_ratio)

cor(m_second_welos$server_welo, m_second_welos$speed_ratio, use = "complete.obs") # -0.05 ish

ggplot(m_second_welos, aes(x = server_welo, y = speed_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Server wElo vs. Second Serve Speed Ratio -- Males (corr ~ -0.05)",
       x = "Server wElo",
       y = "Second Serve Speed Ratio (compared to avg first serve speed)") +
  theme_minimal()
ggsave("../images/male_welo_vs_second_serve_speed.png", bg = "white", 
       width = 8, height = 6, units = "in")
# slight negative correlation, but very weak

f_second_welos <- subset_f_second %>% 
  mutate(server_welo = if_else(PointServer == 1, player1_avg_welo, player2_avg_welo)) %>%
  select(server_welo, speed_ratio)

cor(f_second_welos$server_welo, f_second_welos$speed_ratio, use = "complete.obs") # -0.14 ish

ggplot(f_second_welos, aes(x = server_welo, y = speed_ratio)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Server wElo vs. Second Serve Speed Ratio -- Females (corr ~ -0.14)",
       x = "Server wElo",
       y = "Second Serve Speed Ratio (compared to avg first serve speed)") +
  theme_minimal()
ggsave("../images/female_welo_vs_second_serve_speed.png", bg = "white", 
       width = 8, height = 6, units = "in")
