# FANTASY FOOTBALL FORECASTER: 2 - DATA AGGREGATION

# Overview ----------------------------------------------------------------
# This forecaster aims to use historical player performance data to pick the
# highest-scoring team possible for the next game week

# It is split into four key sections:
#   - Data collection
#   - Data aggregation
#   - Model training
#   - Forecasting the ideal team (ideal team creation and graphing)

# ------------------------2 - DATA AGGREGATION---------------------------------

# Basic setup -------------------------------------------------------------
library(jsonlite)
library(tidyverse)


# Game aggregation
full_history_raw <- read_csv("data_raw/full_history.csv")
teams <- read_csv("data_raw/teams.csv")
current_players <- read_csv("data_raw/current_players.csv")

full_history <- full_history_raw %>%
  left_join(select(current_players,id,team_code), by=c("player_id"="id")) %>%
  left_join(select(teams,code,strength_defence_home,strength_defence_away,strength_attack_home,strength_attack_away),by=c("team_code" = "code")) %>%
  mutate(team_defence = ifelse(was_home==TRUE,strength_defence_home,strength_defence_away),
         team_attack = ifelse(was_home==TRUE,strength_attack_home,strength_attack_away)) %>%
  select(-strength_defence_home,-strength_defence_away,-strength_attack_home,-strength_attack_away) %>%
  left_join(select(teams,id,strength_defence_home,strength_defence_away,strength_attack_home,strength_attack_away),by=c("opponent_team" = "id")) %>%
  mutate(opponent_defence = ifelse(was_home==TRUE,strength_defence_home,strength_defence_away),
         opponent_attack = ifelse(was_home==TRUE,strength_attack_home,strength_attack_away)) %>%
  select(-strength_defence_home,-strength_defence_away,-strength_attack_home,-strength_attack_away) %>%
  mutate(attack_strength = team_attack - opponent_defence,
         defence_strength = team_defence - opponent_attack) %>%
  select(-team_attack,-team_defence,-opponent_attack,-opponent_defence)

sum_colnames <- colnames(select(full_history,-id,-opponent_team,-was_home,-penalties_saved,-penalties_missed,
                                -yellow_cards,-red_cards))
sum_dataset_n <- select(full_history,sum_colnames)

# Function to aggregate last games ====
# firstgame is backwards looking start of aggregation (last game played = 1), lastgame is backwards looking end of aggregation, futuregames is number of future games
# to count future points for

f_dataset_n <- function(firstgame,lastgame,futuregames){
  output = full_history[0,] %>% select(-id,-opponent_team,-was_home)

  for(p in unique(full_history$player_id)){
    input = full_history %>% filter(player_id == p)
    input_sum = sum_dataset_n %>% filter(player_id == p)
    for(i in futuregames:nrow(input)){
      a = i-futuregames+1 # Start row of forwards looking
      b = i # End row of forwards looking
      
      x = i+firstgame # Start row of backwards looking
      y = i+lastgame # End row of backwards looking
      
      player_id = p
      game_no = tibble(game_no=nrow(input)+1-i)
      team = filter(current_players,id==p) %>% select(team_code)
      opponent = input[i,] %>% select(opponent_team)
      #total_points = input[i,] %>% select(total_points)
      total_points = input_sum[a:i,] %>% select(total_points) %>% group_by %>% summarise_all(sum)
      future_total_attacking_advantage = input_sum[a:b,] %>% select(attack_strength) %>% group_by %>% summarise_all(sum)
      future_total_defending_advantage = input_sum[a:b,] %>% select(defence_strength) %>% group_by %>% summarise_all(sum)

      sum_line = input_sum[x:y,] %>% select(-player_id,-team_code) %>% group_by %>% summarise_all(sum)
      colnames(sum_line) <- paste0("n_",colnames(sum_line))
      #sum_line = sum_line %>% rename_at(vars(colnames(sum_line[,-1])),~paste0("n_",colnames(sum_line[,-1])))
      
      n_line = as.tibble(cbind(player_id,team,game_no,total_points,future_total_attacking_advantage,future_total_defending_advantage,sum_line))
      output = rbind(output,n_line)
    }
    cat(paste0(p,", "))
  }
  return(output)
}


# Function to add player and team data ====
f_add_teams <- function(tibble){
  teams_ids <- teams %>% select(id,code,name) %>% mutate(team=name)

  output <- tibble %>% 
    select(player_id,everything()) %>%
    left_join(.,select(current_players,id,web_name,element_type),by=c("player_id"="id")) %>%
    left_join(.,select(teams_ids,code,team),by=c("team_code"="code")) %>%
    select(player_id,web_name,team,element_type,everything(),-team_code)
  
  output <- mutate(output, element_type=ifelse(element_type==1,"Goalkeeper",
                                               ifelse(element_type==2,"Defender",
                                                      ifelse(element_type==3,"Midfielder",
                                                             ifelse(element_type==4,"Forward",NA)))))
  
  return(output)
}



# Writing csvs of all datasets --------------------------------------------

write_csv(f_add_teams(f_dataset_n(1,5,5)),"data_aggregated/data_final/final_dataset_5h_5f.csv")
