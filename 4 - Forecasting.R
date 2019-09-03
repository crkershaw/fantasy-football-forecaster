# FANTASY FOOTBALL FORECASTER: 4 - FORECASTING THE IDEAL TEAM

# Overview ----------------------------------------------------------------
# This forecaster aims to use historical player performance data to pick the
# highest-scoring team possible for the next game week

# It is split into four key sections:
#   - Data collection
#   - Data aggregation
#   - Model training
#   - Forecasting the ideal team (ideal team creation and graphing)

# ------------------------4 - FORECASTING THE IDEAL TEAM ---------------------------------
library(tidyverse)
library(lpSolveAPI)
library(Rglpk)
library(caret)
library(randomForest)


CustomTheme <- theme_bw()+
  theme(
    legend.text = element_text(colour="grey47"),
    legend.title = element_text(colour="grey47"),
    panel.background = element_rect("white"),
    plot.background = element_rect("white"),
    panel.border = element_rect("grey",fill=NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust=0.5,colour="grey47"),
    axis.title = element_text(colour="grey47"),
    axis.text = element_text(colour="grey47"),
    strip.background = element_rect(colour="grey70",fill="grey98"),
    strip.text = element_text(colour="grey47")
  )

c_lightblue = "#00A8C8"
c_lightgreen = "#90BE44"
c_orange = "#FBAE17"
c_pink = "#ED2C67"
c_darkgreen = "#0FB694"

# Predicting next week's scores ====

week <- 34
model_name <- "rf_5g_nolog"
model_longname <- "Random Forest"

# ... Reading in data and models ====
data_raw <- read_csv("data_aggregated/data_final/final_dataset_5h_5f.csv")
data_future <- data_raw %>%
  filter(game_no == week) %>%
  mutate(total_points = NA)

# Choosing models

f_create_model_input <- function(position, model){

  model_data = readRDS(paste0("data_model/models/model_",position, "_",model,".rds"))
  
  model_cols = colnames(as_tibble(model_data$trainingData))[-length(model_data$trainingData)]
  
  future_data = data_future %>%
    filter(element_type == position)
  
  future_data_model = future_data %>%
    select(model_cols)
  
  return(future_data_model)
}

f_create_model_output <- function(position, model, modelinput, log_or_nolog){
  
  model_data = readRDS(paste0("data_model/models/model_",position, "_",model,".rds"))
  
  forecast_outcomes = predict(model_data,newdata = data.matrix(get(modelinput)))
  
  future_data = data_future %>%
    filter(element_type == position)
  
  if(log_or_nolog == "log"){
    future_data$predicted = exp(forecast_outcomes)-points_adjustment # Points adjustment from script 3
  } else {
    future_data$predicted = forecast_outcomes
  }
  
  return(future_data)
}

input_goalkeeper <-  f_create_model_input("Goalkeeper",model_name)
input_defender <-  f_create_model_input("Defender",model_name)
input_midfielder <-  f_create_model_input("Midfielder",model_name)
input_forward <-  f_create_model_input("Forward",model_name)

output_goalkeeper <- f_create_model_output("Goalkeeper",model_name, "input_goalkeeper", "")
output_defender <- f_create_model_output("Defender",model_name, "input_defender", "")
output_midfielder <- f_create_model_output("Midfielder",model_name, "input_midfielder", "")
output_forward <- f_create_model_output("Forward",model_name, "input_forward", "")

output_all <- output_goalkeeper %>%
  rbind(output_defender) %>%
  rbind(output_midfielder) %>%
  rbind(output_forward)

# Creating ideal team --------------------------------------
# Using linear programming to create the ideal team

# ... Data manipulation ====
# Bringing in player injury status and cost
current_players <- read_csv("data_raw/current_players.csv")
output_all_2 <- left_join(output_all,select(current_players,id,status,now_cost),by=c("player_id"="id"))%>%
  select(player_id,web_name,status,now_cost,everything())

# Converting element_type to text
output_all_2$element_type <- as.factor(output_all_2$element_type)
levels(output_all_2$element_type) <- list("Goalkeeper"="1","Defender"="2","Midfielder"="3","Forward"="4")

# ... Linear programming ====
# Constraints
#   Total value < 100m
#   Total number of players = 11
#   Maximum goalkeepers = 1
#   Maximum defenders = 5
#   Maximum midfielders = 5
#   Maximum forwards = 5
# 
# Decision variables
#   Maximum total points

lp_data <- output_all_2

num_players <- nrow(lp_data)
obj <- lp_data$predicted

var.types <- rep("B",num_players) # Var types represented as booleans

# Creating matrix of all teams, and if player is in the team (used for maximum player/team criteria)
teamMatrix <- lapply(unique(lp_data$team), function(name) as.numeric(lp_data$team==name))
teamMatrix <- t(matrix(unlist(teamMatrix), ncol=n_distinct(lp_data$team)))

# Constraints
matrix <- rbind(
  as.numeric(lp_data$element_type=="Goalkeeper"),
  as.numeric(lp_data$element_type=="Defender"),
  as.numeric(lp_data$element_type=="Defender"),
  as.numeric(lp_data$element_type=="Midfielder"),
  as.numeric(lp_data$element_type=="Midfielder"),
  as.numeric(lp_data$element_type=="Forward"),
  as.numeric(lp_data$element_type=="Forward"),
  as.numeric(lp_data$element_type %in% c("Goalkeeper","Defender","Midfielder","Forward")),
  lp_data$now_cost,
  teamMatrix
)

direction <- c(
  "==",
  ">=",
  "<=",
  ">=",
  "<=",
  ">=",
  "<=",
  "==",
  "<=",
  rep("<=",nrow(teamMatrix))
)

rhs <- c(
  1,
  3,
  5,
  3,
  5,
  3,
  5,
  11,
  1000,
  rep(3,nrow(teamMatrix))
)

# Running the solver
sol <- Rglpk_solve_LP(obj = obj, mat = matrix, dir = direction, rhs = rhs,
                      types = var.types, max = TRUE)

# Tibble of the final team
final_team <- lp_data[sol$solution==1,]
sum(final_team$now_cost)
sum(final_team$predicted)


# Graphing final team -----------------------------------------------------

graph_data <- final_team %>% select(web_name,now_cost,element_type,team,predicted)

# Data manipulation for formation graph
graph_data2 <- graph_data %>%
  mutate(y = ifelse(element_type=="Goalkeeper",1,
                    ifelse(element_type=="Defender",2,
                           ifelse(element_type=="Midfielder",3,
                                  ifelse(element_type=="Forward",4,0)
                           )))) %>%
  group_by(element_type) %>%
  mutate(x_initial=row_number()) %>%
  mutate(x_max = max(x_initial)) %>%
  ungroup() %>%
  mutate(x_overallmax = max(x_initial)) %>%
  mutate(x_step=x_overallmax/(x_max+1)) %>%
  mutate(x = x_initial*x_step) %>%
  mutate(predicted_captaincy = ifelse(predicted == max(predicted),round(predicted,0)*2,round(predicted,0))) %>%
  mutate(name_captaincy = ifelse(predicted == max(predicted),paste0(web_name," (c)"),web_name))


# Team Structure Graph
team_graph <- ggplot(graph_data2,aes(x,y,colour=team))+
  geom_point(size=10)+
  geom_text(aes(label=name_captaincy),vjust=-2)+
  geom_text(aes(label=round(predicted_captaincy,0)),colour="white",size=4,fontface="bold")+
  geom_text(aes(label=paste0("£",now_cost/10,"m")),vjust=2.5)+
  annotate("text",1,1.5,label=paste0("Total cost: £",sum(graph_data2$now_cost)/10,"m"),fontface="bold")+
  annotate("text",1,1.35,label=paste0("Total points: ",sum(round(graph_data2$predicted_captaincy),0)),fontface="bold")+
  annotate("text",filter(graph_data2,element_type == "Goalkeeper")$x,0.5,label="Number in circle is predicted points. Number below is cost", fontface = "italic", colour = "grey") +
  annotate("text",filter(graph_data2,element_type == "Goalkeeper")$x,0.3,label="The captain (c) has their points doubled", fontface = "italic", colour = "grey") +
  theme_void()+
  ylim(0,max(graph_data2$y)*1.1)+
  xlim(0,max(graph_data2$x)+min(graph_data2$x_step))+
  labs(colour="Team")+
  ggtitle(paste0("Optimal FPL Team (Week ",week,") - ",model_longname))+
  theme(plot.title = element_text(hjust=0.5,colour="grey47"))

# Saving
png(paste0("outputs/","Chosen_team_week_",week," (",model_name,").png"), width=1280,height=800,res=144)
team_graph
dev.off()


# Prediction vs Reality Graph
prediction_vs_reality <- graph_data2 %>% 
  select(web_name, now_cost, element_type, team, predicted) %>%
  left_join(data_raw %>% filter(game_no == week) %>% select(web_name, element_type, team, total_points) %>% filter(web_name %in% graph_data2$web_name))

graph_data <- prediction_vs_reality %>%
  mutate(difference = predicted - total_points,
         Estimate = ifelse(difference >= 0, "Over", "Under")) %>%
  gather(type, points, -web_name, -now_cost, -element_type, -team, -difference, -Estimate) %>%
  mutate(rand_num = runif(1,0,0.00001)) %>%
  group_by(type) %>%
  mutate(captain = ifelse(points + rand_num == max(points + rand_num), "yes", "no")) %>%
  mutate(points_captain = ifelse(captain == "yes", round(points,0) * 2, round(points,0))) %>%
  ungroup() %>%
  rename("Type" = type) %>%
  mutate(Type = ifelse(Type == "predicted", "Predicted", "Actual")) %>%
  mutate(web_name = gsub("-"," ",web_name),
         web_name = gsub('\\s','\n',web_name)) # Splitting names with two parts over two lines for axis

totals <- graph_data %>% group_by(Type) %>% summarise(sum_points_captain = sum(points_captain))
total_predicted <- round(totals %>% filter(Type == "Predicted") %>% select(sum_points_captain) %>% as.numeric(),0)
total_actual <- round(totals %>% filter(Type == "Actual") %>% select(sum_points_captain) %>% as.numeric(),0)


chosen_team_performance <- ggplot(graph_data)+
  geom_linerange(
    mapping = aes(x = reorder(web_name, -Actual), ymin = Predicted, ymax = Actual, colour = Estimate),
    data = spread(select(graph_data, -captain, -points_captain), Type, points), 
    size = 2) +
  geom_point(aes(x=web_name, y = points, fill = Type), size = 8, pch = 21, colour = alpha("white", 0)) +
  scale_fill_manual(values = c(c_lightblue, c_pink)) +
  annotate("text",x=c(1,1),y=c(8,10),label = c(paste0("Total actual: ",total_actual), paste0("Total predicted: ",total_predicted)), colour = c(c_lightblue, c_pink), hjust = 0) +
  CustomTheme +
  scale_y_continuous(name = "Points") +
  scale_x_discrete(name = "Player") +
  ggtitle(paste0("Selected team predicted vs actual performance (Week ",week,") - ",model_longname))

# Saving
png(paste0("outputs/","Chosen_team_performance_week_",week," (",model_name,").png"), width=1280,height=800,res=144)
chosen_team_performance
dev.off()


# The ideal team for the selected week ------------------------------------

ideal_team_data <- output_all_2 %>%
  select(-total_points) %>%
  left_join(select(data_raw, player_id, game_no, total_points), by = c("player_id","game_no"))

num_players <- nrow(ideal_team_data)
ideal_obj <- ideal_team_data$total_points

var.types <- rep("B",num_players) # Var types represented as booleans

# Creating matrix of all teams, and if player is in the team (used for maximum player/team criteria)
ideal_teamMatrix <- lapply(unique(ideal_team_data$team), function(name) as.numeric(ideal_team_data$team==name))
ideal_teamMatrix <- t(matrix(unlist(ideal_teamMatrix), ncol=n_distinct(ideal_team_data$team)))

# Constraints
ideal_matrix <- rbind(
  as.numeric(ideal_team_data$element_type=="Goalkeeper"),
  as.numeric(ideal_team_data$element_type=="Defender"),
  as.numeric(ideal_team_data$element_type=="Defender"),
  as.numeric(ideal_team_data$element_type=="Midfielder"),
  as.numeric(ideal_team_data$element_type=="Midfielder"),
  as.numeric(ideal_team_data$element_type=="Forward"),
  as.numeric(ideal_team_data$element_type=="Forward"),
  as.numeric(ideal_team_data$element_type %in% c("Goalkeeper","Defender","Midfielder","Forward")),
  ideal_team_data$now_cost,
  ideal_teamMatrix
)

ideal_sol <- Rglpk_solve_LP(obj = ideal_obj, mat = ideal_matrix, dir = direction, rhs = rhs,
                      types = var.types, max = TRUE)

# Tibble of the final team
ideal_final_team <- ideal_team_data[ideal_sol$solution==1,]
ideal_final_team_captaincy <- ideal_final_team %>%
  mutate(rand_num = runif(1,0,0.00001)) %>%
  mutate(captain = ifelse(total_points + rand_num == max(total_points + rand_num), "yes", "no")) %>%
  mutate(total_points_captain = ifelse(captain == "yes",total_points * 2, total_points))
sum(ideal_final_team_captaincy$now_cost)
sum(ideal_final_team_captaincy$total_points_captain)



