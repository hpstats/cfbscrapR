#install.packages('tidyverse')

#install.packages("devtools")
#devtools::install_github("meysubb/cfbscrapR")

#remotes::install_github("rstudio/gt")

library(tidyverse)
library(cfbscrapR)
library(broom)
library(glmnet)


pbp_2019 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2019, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2019 <- bind_rows(pbp_2019, df)
}

runs <- pbp_2019 %>%
  filter(play_type == 'Rush') %>%
  select(offense_play, defense_play, play_text, yards_gained) %>%
  mutate(player = substr(play_text, 1, str_locate(play_text, ' run ')[,1] - 1)) %>%
  filter(player != 'TEAM') %>%
  mutate(playerteam = paste(player, offense_play)) %>%
  mutate(offense_play = as.factor(offense_play)) %>%
  mutate(defense_play = as.factor(defense_play)) %>%
  mutate(playerteam = as.factor(playerteam))

lambdas <- 10^seq(3, -2, by = -.1)
cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)