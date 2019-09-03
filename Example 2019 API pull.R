library(jsonlite)
library(tidyverse)

player_url <- "https://fantasy.premierleague.com/api/element-summary/"
static_url <- "https://fantasy.premierleague.com/api/bootstrap-static/"

player_number <- 342

# Checking what information is available in the API
names(fromJSON(paste0(player_url,player_number,"/")))
# [1] "fixtures"     "history"      "history_past"

names(fromJSON(static_url))
# [1] "events"        "game_settings" "phases"        "teams"         "total_players" "elements"      "element_stats"
# [8] "element_types"

# Example API pulls for player
player_historypast <- as.tibble(fromJSON(paste0(player_url,player_number,"/"))$history_past)  # Annual stats for a player
player_fixtures <- as.tibble(fromJSON(paste0(player_url,player_number,"/"))$fixtures) # Upcoming fixtures for a player
player_history <- as.tibble(fromJSON(paste0(player_url,player_number,"/"))$history) # Match by match stats for player

# Example pulling team information
team_information <- as.tibble(fromJSON(static_url)$teams)
