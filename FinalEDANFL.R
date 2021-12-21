install.packages("tidyverse")
install.packages("dplyr")
install.packages("na.tools")
install.packages("ggimage")
library(tidyverse)
library(dplyr)
library(na.tools)
standings <- read_csv("http://nflgamedata.com/standings.csv")
standings %>% head()
standings %>% filter(playoff == "WonSB") %>% group_by(seed) %>% summarize(count=n())
library(ggplot2)
ggplot(standings,aes(x=scored,y=allowed)) +
  theme_minimal() +
  geom_point(aes(color=playoff)) +
  xlab("Points Scored") +
  ylab("Points Allowed") +
  labs(title="Points Scored vs. Points Allowed")
games <- read_csv("http://www.habitatring.com/games.csv")
games %>% select(season,week,location,away_team,away_score,home_team,home_score,result) %>% head()
home_games <- games %>% filter(location == "Home")
summary(home_games$result)
ggplot(home_games,aes(x=result)) +
  theme_minimal() +
  geom_histogram(binwidth=1) +
  xlab("Home Team Net Points") +
  ylab("Share of Games") +
  labs(title="Distribution of Home Team Net Points")
draft_draft_picks <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_picks.csv")
values <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_values.csv")
pbp2018 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))
pbp2018 %>% select(posteam, defteam, desc, play_type) %>% head
pbp_rp <- pbp2018 %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")
pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 
pbp_rp %>% filter(play_type=="no_play") %>% select(pass, rush, desc)  %>% head
pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)
pbp_rp %>%
  filter(posteam == "LA", rush == 1, down<=4) %>%
  group_by(rusher_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>40)
passfirst <- pbp_rp %>%
  filter(wp>.20 & wp<.80 & down<=2 & qtr<=2 & half_seconds_remaining>120) %>%
  group_by(posteam) %>%
  summarize(mean_pass=mean(pass), plays=n()) %>%
  arrange(mean_pass)
ggplot(passfirst, aes(x=reorder(posteam,-mean_pass), y=mean_pass)) +
  geom_text(aes(label=posteam))
ggsave('passfirstO.png', dpi=1000)