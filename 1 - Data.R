# FANTASY FOOTBALL FORECASTER: 1 - DATA SCRAPING

# Overview ----------------------------------------------------------------
# This forecaster aims to use historical player performance data to pick the
# highest-scoring team possible for the next game week

# It is split into four key sections:
#   - Data collection
#   - Data aggregation
#   - Model training
#   - Forecasting the ideal team (ideal team creation and graphing)

# ------------------------1 - DATA SCRAPING---------------------------------

# Basic setup -------------------------------------------------------------
library(jsonlite)
library(tidyverse)

# API Details -------------------------------------------------------------

player_url <- "https://fantasy.premierleague.com/drf/element-summary/"
current_url <- "https://fantasy.premierleague.com/drf/bootstrap-static/"

names(fromJSON(paste0(player_url,372)))
names(fromJSON(current_url))

# Example API pulls for player ID 372
player_historypast <- as.tibble(fromJSON(paste0(player_url,"372"))$history_past)  # Annual stats for a player
player_fixturessummary <- as.tibble(fromJSON(paste0(player_url,"372"))$fixtures_summary) # Next 3 fixtures
player_fixtures <- as.tibble(fromJSON(paste0(player_url,"372"))$fixtures) # Upcoming fixtures for a player
player_history <- as.tibble(fromJSON(paste0(player_url,"372"))$history) # Match by match stats for player
player_historysummary <- as.tibble(fromJSON(paste0(player_url,"372"))$history_summary)  # Last 3 games stats


# 1. Data Aggregation -------------------------------------------------------

#... Pulling data on players and teams ====

# Pulling data on teams
teams <- teams_raw %>% select(id,code,name,short_name,strength_overall_home,strength_overall_away,strength_attack_home,strength_attack_away,strength_defence_home,strength_defence_away)
write_csv(teams,"data_raw/teams.csv")

# Pulling data on current players - 600 players
current_players <- as.tibble(fromJSON(current_url)$elements)
write_csv(current_players,"data_raw/current_players.csv")
# ... Test dataset ====
# Looping through the dataset, summing the data for last three games

# Creating a dataset of key player performance data
dataset_1 <- player_history %>% 
  select(id,total_points,was_home,minutes,goals_scored,assists,clean_sheets,goals_conceded,penalties_saved,
         penalties_missed,yellow_cards,red_cards,saves,bonus,bps,
         influence,creativity,threat,ict_index,ea_index,open_play_crosses,big_chances_created,
         big_chances_created,clearances_blocks_interceptions,recoveries,key_passes,tackles,
         winning_goals,attempted_passes,completed_passes,penalties_conceded,big_chances_missed,
         errors_leading_to_goal,errors_leading_to_goal_attempt,tackled,offside,target_missed,
         fouls,dribbles)
dataset_1 <- dataset_1[order(-dataset_1$id),]
sum_colnames <- colnames(select(dataset_1,minutes,goals_scored,assists,clean_sheets,goals_conceded,
                                saves,bonus,bps))

# Creating a dataset out of the columns to be summed up
dataset_2 <- select(dataset_1,sum_colnames)

# Blank tibble to hold the aggregated 3 game data
g3_tibble <- tibble(no=NA,total_points=NA,was_home=NA,g3_minutes=NA,g3_goals_scored=NA,g3_assists=NA,g3_clean_sheets=NA,g3_goals_conceded=NA,g3_saves=NA,g3_bonus=NA,g3_bps=NA)
g3_tibble <- g3_tibble[-1,]

dataset_grouped <- for(i in 1:nrow(dataset_1)){
  x = i+1
  y = i+3
  no = tibble(no=i)
  outcome_points = dataset_1[i,] %>% select(total_points)
  home_or_away = dataset_1[i,] %>% select(was_home)
  sum_line = dataset_2[x:y,] %>% group_by %>% summarise_all(sum)
  colnames(sum_line) <- paste0("g3_",colnames(sum_line))
  
  g3_line = as.tibble(cbind(no,outcome_points,home_or_away,sum_line))
  g3_tibble <- rbind(g3_tibble,g3_line)
}

# ... Full dataset ====
# Looping through the dataset, summing the data for last three games

# Blank tibble to hold outcome data
final_tibble <- tibble(player_id=NA,game_no=NA,total_points=NA,was_home=NA,team=NA,opponent=NA,g3_minutes=NA,
                       g3_goals_scored=NA,g3_assists=NA,g3_clean_sheets=NA,
                       g3_goals_conceded=NA,g3_saves=NA,g3_bonus=NA,g3_bps=NA)
final_tibble <- final_tibble[-1,]

future_tibble <- final_tibble # Future data to be used for next week forecasts

g3_tibble <- tibble(no=NA,total_points=NA,was_home=NA,g3_minutes=NA,g3_goals_scored=NA,g3_assists=NA,g3_clean_sheets=NA,g3_goals_conceded=NA,g3_saves=NA,g3_bonus=NA,g3_bps=NA)
g3_tibble <- g3_tibble[-1,]

# Pulling all data at once

full_history <- as.tibble(fromJSON(paste0(player_url,1))$history)[0,]
full_nextgame <- as.tibble(fromJSON(paste0(player_url,1))$fixtures_summary)[0,]

for(p in 1:595){
  
  player_history <- as.tibble(fromJSON(paste0(player_url,p))$history) # Match by match stats for player
  player_nextgame <- as.tibble(fromJSON(paste0(player_url,p))$fixtures_summary)
  
  nextgame <- player_nextgame[1,] %>% select(is_home,opponent_name)
  
  player_data <- player_history %>% 
    select(id,total_points,opponent_team,was_home,minutes,goals_scored,assists,clean_sheets,goals_conceded,penalties_saved,
           penalties_missed,yellow_cards,red_cards,saves,bonus,bps,
           influence,creativity,threat,ict_index,ea_index,open_play_crosses,big_chances_created,
           clearances_blocks_interceptions,recoveries,key_passes,tackles,
           winning_goals,attempted_passes,completed_passes,penalties_conceded,big_chances_missed,
           errors_leading_to_goal,errors_leading_to_goal_attempt,tackled,offside,target_missed,
           fouls,dribbles)
  
  player_data <- player_data[order(-player_data$id),] %>% #To put most recent games at the top
    mutate(player_id=p) %>%
    select(player_id,everything())
  
  full_history <- rbind(full_history,player_data)
  
  percentage <- p / 595
  
  cat(paste0(round(percentage*100,0),"%     "))   #progress indicator
  
}

write_csv(full_history,"data_raw/full_history.csv")