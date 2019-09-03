# FANTASY FOOTBALL FORECASTER: 3 - TRAINING THE MODELS

# Overview ----------------------------------------------------------------
# This forecaster aims to use historical player performance data to pick the
# highest-scoring team possible for the next game week

# It is split into four key sections:
#   - Data collection
#   - Data aggregation
#   - Model training
#   - Forecasting the ideal team (ideal team creation and graphing)

# ------------------------3 - TRAINING THE MODELS ---------------------------------

library(tidyverse)
# library(ggrepel)
library(caret)
library(randomForest)
library(doParallel)
# library(data.table)
# library(Matrix)
library(xgboost)
library(gridExtra)
# library(ggraph)
library(igraph)

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

# Visualisation -----------------------------------------------------------

dataset <- read_csv("data_aggregated/data_final/final_dataset_5h_5f.csv")

# Points distribution

data_graphs <- dataset %>%
  select(player_id, element_type, game_no, total_points) %>%
  mutate(log_total_points = log(total_points)) %>%
  pivot_longer(cols = c(total_points, log_total_points), names_to = "type", values_to = "points")

ggplot(data_graphs, aes(points, fill = element_type)) +
  geom_histogram() +
  CustomTheme +
  facet_grid(type ~ element_type, scales = "free")

graph_points_hist <- ggplot(dataset, aes(total_points)) +
  geom_histogram() +
  CustomTheme

graph_points_hist_pos <- ggplot(dataset , aes(total_points, fill = element_type)) +
  geom_histogram() +
  CustomTheme +
  facet_wrap(~element_type, nrow = 1) +
  labs(
    title = "Distribution of 5-game total points by position",
    x = "Total points",
    y = "Count",
    fill = "Position"
  ) +
  theme(legend.position = "bottom")

# Log points distribution
graph_points_histlog <- ggplot(dataset, aes(log(total_points))) +
  geom_histogram() +
  CustomTheme

# Log points distribution
graph_points_histlog_pos <- ggplot(filter(dataset) , aes(log(total_points), fill = element_type)) +
  geom_histogram() +
  CustomTheme +
  facet_wrap(~element_type, nrow = 1) +
  labs(
    title = "Distribution of 5-game log total points by position",
    x = "Log total points",
    y = "Count",
    fill = "Position"
  ) +
  theme(legend.position = "none")

# Comparing logged and unlogged
grid.arrange(graph_points_hist_pos, graph_points_histlog_pos, nrow = 2)

# Points vs lastgame points scatter
ggplot(dataset,aes(n_total_points,total_points))+
  geom_point(position="jitter")+
  geom_smooth() +
  CustomTheme

# Goals vs points scatter
ggplot(subset(dataset,element_type=="Forward"),aes(n_goals_scored,total_points))+
  geom_point(position="jitter")+
  geom_smooth()+
  CustomTheme

# Points vs team scatter
ggplot(subset(dataset,element_type=="Forward"),aes(team,total_points)) +
  geom_point(position="jitter", colour = "lightblue") + 
  geom_boxplot(aes(alpha=0.5),outlier.shape=NA)+
  CustomTheme


# 2. Model creation -----------------------------------------------------------

position_list <- c("Goalkeeper", "Defender", "Midfielder", "Forward")

# Data Manipulation ====
# ... Reading in data after data aggregation  ==============================================================================

data_raw <- read_csv("data_aggregated/data_final/final_dataset_5h_5f.csv")

# Opponent and team captured by defense_strength and attack_strength, game_no not relevant
basiccols <- c("player_id","web_name","element_type","total_points")

data_output <- data_raw %>% 
  select(-game_no) %>% 
  filter(!is.na(n_total_points))

# Selecting numerical columns
numerics <- colnames(select(data_output,-team,-player_id,-web_name,-element_type,-total_points))
non_numerics <- colnames(select(data_output,-numerics))

#... Setting position and whether to log or not  ==========================================================================

# [1] "Goalkeeper" "Defender"   "Midfielder" "Forward" 

for(position in position_list){
  for(log_or_not in c("log","nolog")){
    
    chosen_position = position
    
    data_output_logged <- data_output
    # Logging selected columns if log selected
    if(log_or_not == "log"){
      points_adjustment <- -min(data_output_logged$total_points) + 1    # [1] 4
      
      data_output_logged$total_points <- log(data_output_logged$total_points+points_adjustment) # Adjusting all values to prevent negatives
      data_output_logged$n_total_points <- log(data_output_logged$n_total_points+points_adjustment) # Adjusting all values to prevent negatives
    }
    
    #... Creating training and testing data sets ==============================================================================
    
    data_model <- data_output_logged %>%
      filter(element_type == chosen_position) %>%
      mutate(element_type = as.factor(element_type))
    
    set.seed(123)
    inTrain <- createDataPartition(data_model$total_points,p=0.7,list=FALSE)
    trg <- data_model[inTrain,]
    test <- data_model[-inTrain,]
    
    dim(trg)
    dim(test)
    
    #... Scaling and centering numeric values ==============================================================================
    
    procValues <- trg %>%
      select(numerics) %>%
      preProcess(method = c("center", "scale"))
    
    f_scale_and_center <- function(data, proc_values){
      
      data_scaled_num <- predict(proc_values,data[,numerics]) #scaled and centered
      
      data_scaled <- data %>%
        select(non_numerics) %>%
        cbind(.,data_scaled_num) %>% 
        as_tibble() %>%
        mutate(element_type = as.factor(element_type))
      
      summary(data_scaled)
      
      return(data_scaled)
    }
    
    train_scaled <- trg
    test_scaled <- test
    
    # Uncomment to scale and center
    # train_scaled <- f_scale_and_center(trg, procValues)
    # test_scaled <- f_scale_and_center(test, procValues)
    
    #... Selecting input and output columns ================================================================
    
    factor_cols <- c("element_type")
    train_scaled[factor_cols] <- lapply(train_scaled[factor_cols],as.factor)
    test_scaled[factor_cols] <- lapply(test_scaled[factor_cols],as.factor)
    
    train_scaled_x <- train_scaled %>% select(-total_points, -player_id)  # select all cols except total_points & id
    test_scaled_x <- test_scaled %>% select(-total_points, -player_id)    # select all cols except total_points & id
    train_scaled_y <- train_scaled %>% select(total_points)               # select only total_points
    test_scaled_y <- test_scaled %>% select(total_points)                 # select only total_points
    
    summary(train_scaled_x) #check to ensure no NAs
    summary(test_scaled_x) #check to ensure no NAs
    
    #... Creating data matrix ==============================================================================
    
    train_matrix_x <- data.matrix(train_scaled_x)
    test_matrix_x <- data.matrix(test_scaled_x)
    
    summary(train_matrix_x) #check to ensure no NAs
    summary(test_matrix_x) #check to ensure no NAs
    
    #... Removing NZV variables ==============================================================================
    
    # Identifying NZVs
    nzv <- nearZeroVar(train_matrix_x, saveMetrics= TRUE,allowParallel=TRUE)
    head(nzv)
    
    nzv[nzv$nzv,] # A number of near zero variance variables to be removed
    nzv["n_total_points","nzv"] <- FALSE # Overwriting n_total_points, as used for comparison models)
    
    # Removing NZVs 
    f_remove_nzv <- function(data){
      data_filtered <- data[,!nzv$nzv]
      return(data_filtered)
    }
    
    train_matrix_x_filtered <- f_remove_nzv(train_matrix_x)
    test_matrix_x_filtered <- f_remove_nzv(test_matrix_x)
    
    colnames(train_matrix_x)[which(!(colnames(train_matrix_x)%in%colnames(train_matrix_x_filtered)))] # confirmed that nzv cols are removed
    summary(test_matrix_x_filtered) # no NAS
    
    # Saving files
    saveRDS(train_matrix_x_filtered, paste0("data_model/inputs/", tolower(chosen_position), "_train_x_",log_or_not,".rds"))
    saveRDS(train_scaled_y, paste0("data_model/inputs/", tolower(chosen_position), "_train_y_",log_or_not,".rds"))
    
    saveRDS(test_matrix_x_filtered, paste0("data_model/inputs/", tolower(chosen_position), "_test_x_",log_or_not,".rds"))
    saveRDS(test_scaled_y, paste0("data_model/inputs/", tolower(chosen_position), "_test_y_",log_or_not,".rds"))
    
  }
}


# Model training ----------------------------------------------------------

# ... Linear Regression ====

f_train_lm <- function(position){
  for(log_or_nolog in c("log","nolog")){
    
    print(paste0(position," - ",log_or_nolog))
    
    train_x <- readRDS(paste0("data_model/inputs/",position,"_train_x_",log_or_nolog,".rds")) %>% as.data.frame()
    train_y <- readRDS(paste0("data_model/inputs/",position,"_train_y_",log_or_nolog,".rds")) %>% as.data.frame()
    
    test_x <- readRDS(paste0("data_model/inputs/",position,"_test_x_",log_or_nolog,".rds")) %>% as.data.frame()
    test_y <- readRDS(paste0("data_model/inputs/",position,"_test_y_",log_or_nolog,".rds")) %>% as.data.frame()
    
    # Creating the model
    ctrl <- trainControl(method="repeatedcv",
                         number=10,
                         repeats=5,
                         allowParallel=TRUE)
    
    lmFit <- train(x=train_x,
                   y=train_y$total_points,
                   method="lm",
                   trControl = ctrl)
    
    # Saving and loading the model
    saveRDS(lmFit,paste0("data_model/models/model_",tolower(position),"_lm_5g_",log_or_nolog,".rds"))
    lmFit <- readRDS(paste0("data_model/models/model_",tolower(position),"_lm_5g_",log_or_nolog,".rds"))
  }
}
  
lapply(position_list, f_train_lm)


# Setting model parameters ------------------------------------------------

# Linear model
lm_ctrl <- trainControl(method="repeatedcv",
                     number=10,
                     repeats=5,
                     allowParallel=TRUE)

# Random forest model
rf_ctrl <- trainControl(method="repeatedcv",
                     number=10,
                     repeats=5,
                     classProbs=TRUE,
                     #summaryFunction = twoClassSummary,
                     allowParallel=TRUE)

# XGboost model
xg_ctrl <- trainControl(method="repeatedcv",
                        number=10,
                        repeats=5,
                        classProbs=TRUE,
                        verboseIter = FALSE) # no training log


# Function to train a model -----------------------------------------------

f_train_model <- function(position, model, both_or_just_nolog, ctrl){
  
  ctrl = get("lm_ctrl")
  set.seed(123)
  no_cores <- detectCores()-1
  registerDoParallel(no_cores) 
  
  if(both_or_just_nolog == "both"){
    loglist <- c("log", "nolog")
  } else {
    loglist = "nolog"
  }
  
  for(log_or_nolog in loglist){
  
    print(paste0("Training ", position, " ", model, " (", log_or_nolog,") ----------"))
    
    train_x <- readRDS(paste0("data_model/inputs/",position,"_train_x_",log_or_nolog,".rds")) %>% as.data.frame()
    train_y <- readRDS(paste0("data_model/inputs/",position,"_train_y_",log_or_nolog,".rds")) %>% as.data.frame()
    
    test_x <- readRDS(paste0("data_model/inputs/",position,"_test_x_",log_or_nolog,".rds")) %>% as.data.frame()
    test_y <- readRDS(paste0("data_model/inputs/",position,"_test_y_",log_or_nolog,".rds")) %>% as.data.frame()
    
    
    t1 <- Sys.time()
    print(t1)
    fit <- train(x=train_x,
                 y=train_y$total_points,
                 method=model,
                 importance=T,
                 trControl = ctrl,
                 objective = 'reg:squarederror') # For xgboost
    
    print(Sys.time())
    print(paste0("Time taken: ",Sys.time()-t1))
    
    # Saving and loading the model
    model_name <- paste0("data_model/models/model_",tolower(position),"_",model,"_5g_",log_or_nolog,".rds")
    saveRDS(fit,model_name)
    print(paste0("Model saved: ",model_name))
    
    cat("\n")
  }
}

  
# .....Function to examine the model outputs  -------------------------------
f_examine_model <- function(position, model, both_or_just_nolog){

  modeltype = substr(model,1,2)
  
  if(both_or_just_nolog == "both"){
    loglist <- c("log", "nolog")
  } else {
    loglist = "nolog"
  }
  
  for(log_or_nolog in loglist){
    
    train_x <- readRDS(paste0("data_model/inputs/",position,"_train_x_",log_or_nolog,".rds")) %>% as.data.frame()
    train_y <- readRDS(paste0("data_model/inputs/",position,"_train_y_",log_or_nolog,".rds")) %>% as.data.frame()
    
    test_x <- readRDS(paste0("data_model/inputs/",position,"_test_x_",log_or_nolog,".rds")) %>% as.data.frame()
    test_y <- readRDS(paste0("data_model/inputs/",position,"_test_y_",log_or_nolog,".rds")) %>% as.data.frame()
    
    fit <- readRDS(paste0("data_model/models/model_",tolower(position),"_",model,"_",log_or_nolog,".rds"))
    
    print(paste0(modeltype, " for ", position, " - ", log_or_nolog, " ----------"))
    summary(fit) 
    
    # Variable importance
    print("Variable importance (top 10):")
    varImp(fit)$importance %>% as.tibble(rownames = "Variable") %>% arrange(-Overall) %>% slice(1:10) %>% print()
    plot(varImp(fit))
    
    # Residuals (training data)
    residuals <- resid(fit)
    plot(train_y$total_points,residuals)
    
    # Predicted values (training data)
    predictedValues <- predict(fit, train_x)
    plot(train_y$total_points,predictedValues)
    
    # Predicted values (test data)
    predictedVal <- predict(fit,test_x)
    modelvalues <- data.frame(obs=test_y$total_points,pred=predictedVal)
    
    # Model summary
    print("Model summary:")
    print(defaultSummary(modelvalues))
    
    # If logged the data, transforming it back
    if(log_or_nolog == "log"){
      test_unlogged_y <- test_y %>%
        mutate(total_points = exp(total_points)-points_adjustment)
      predicted_unlogged <- exp(predictedVal)-points_adjustment
      # Original transformation for reference: log(data_output$total_points+points_adjustment)
    } else {
      test_unlogged_y <- test_y
      predicted_unlogged <- predictedVal
    }
    
    # Graph
    rsq <- round(as.numeric(defaultSummary(modelvalues)[2]),2)
    rsq_per <- paste0(rsq*100,"%")
    
    label_x <- max(test_unlogged_y$total_points)*0.8
    
    print(paste0("R squared: ",rsq_per))
    
    output_plot <- ggplot(tibble(actual_points=test_unlogged_y$total_points,predicted_points=predicted_unlogged),aes(actual_points,predicted_points))+
      geom_point(colour=c_lightblue,alpha=0.4) +
      annotate("text",x=label_x,y=1,label=paste0("RSq: ",rsq_per), colour = "grey47") +
      CustomTheme +
      xlab("Actual Points") +
      ylab("Predicted Points") +
      ggtitle(paste0("Model Outcomes - ", toupper(substr(position, 1, 1)), tolower(substring(position, 2)),"s"))+
      geom_abline(slope=1,intercept = 0, lty=2, colour="darkgrey")
    

    cat("\n")
    
    png(paste0("outputs/",tolower(position),"_",model,"_",log_or_nolog,"_",rsq,".png"), width=1280,height=800,res=144)
    
    print(output_plot)

    dev.off()
    
    cat("\n")
  }
}

# Running the models and examining the outputs ----------------------------

# Running varieties of this for each position, with different models and specs
# f_train_model("Goalkeeper","xgbTree","nolog","xg_ctrl")
# f_examine_model("Goalkeeper","xgbTree_5g","nolog")

pmap(list(position = position_list, model = rep("lm", 4), both_or_just_nolog = rep("both", 4), ctrl = rep("lm_ctrl", 4)), f_train_model)
pmap(list(position = position_list, model = rep("rf", 4), both_or_just_nolog = rep("nolog", 4), ctrl = rep("rf_ctrl", 4)), f_train_model)
pmap(list(position = position_list, model = rep("xgbTree", 4), both_or_just_nolog = rep("nolog", 4), ctrl = rep("xg_ctrl", 4)), f_train_model)

pmap(list(position = position_list, model = rep("lm_5g"), both_or_just_nolog = rep("both",4)), f_examine_model)
pmap(list(position = position_list, model = rep("rf_5g"), both_or_just_nolog = rep("nolog",4)), f_examine_model)
pmap(list(position = position_list, model = rep("xgbTree_5g"), both_or_just_nolog = rep("nolog",4)), f_examine_model)

# Training single variable linear model ---------------------------------------------------

# Overall model
model <- lm(total_points ~ n_total_points, data = dataset)

summary(model)

# Player model

for(position in position_list){
  model_name <- paste0("data_model/models/model_",tolower(position),"_lm_5g_nolog_onevar.rds")
  model_data <- lm(total_points ~ n_total_points, data = filter(dataset, element_type == position))
  saveRDS(model_data, model_name)
}

for(position in position_list){
  model_name <- paste0("data_model/models/model_",tolower(position),"_lm_5g_log_onevar.rds")
  minval <- -min(min(dataset$total_points, na.rm = TRUE), min(dataset$n_total_points, na.rm = TRUE)) + 1
  model_data <- lm(total_points ~ n_total_points, 
                   data = dataset %>%
                     mutate(total_points = log(total_points + minval),
                            n_total_points = log(n_total_points+ minval)) %>%
                     filter(element_type == position))
  saveRDS(model_data, model_name)
}

# Comparing models --------------------------------------------------------

# Comparing variable importance

varimp_extraction <- function(model){
  output_table <- tibble(variable=NA,Overall=NA,model=NA)[-1,]
  for(position in position_list){
    modeldata <- readRDS(paste0("data_model/models/model_",tolower(position),"_",model,".rds"))
    varimp <- as.tibble(varImp(modeldata)$importance, rownames = "variable") %>% mutate(model = tolower(position))
    output_table <- rbind(output_table, varimp) %>%
      filter(Overall > 0.1) %>%
      mutate(variable = gsub("n_","",variable))
  }
  return(output_table)
}


ggplot(varimp_extraction("lm_5g_log"), aes(reorder(variable,Overall), Overall, fill = model)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~model) +
  CustomTheme +
  xlab("Overall importance") +
  ylab("Variable")

# Comparing rsquared

rsqu_extraction <- function(model,log_or_nolog){
  output_list <- c()
  for(position in tolower(position_list)){
    
    modeldata <- readRDS(paste0("data_model/models/model_",position,"_",model,".rds"))
    modelinput_x <- readRDS(paste0("data_model/inputs/",position,"_test_x_",log_or_nolog,".rds")) %>% as.data.frame()
    modelinput_y <- readRDS(paste0("data_model/inputs/",position,"_test_y_",log_or_nolog,".rds")) %>% as.data.frame()
    # rsqu <- as.numeric(summary(modeldata)$r.squared)
    
    predictedVal <- predict(modeldata,newdata = modelinput_x)
    modelvalues <- data.frame(obs=modelinput_y$total_points,pred=predictedVal)
    rsqu <- round(as.numeric(defaultSummary(modelvalues)[2]),2)
    
    output_list <- c(output_list, rsqu)
  }
  return(output_list)
}

# Comparing MSE

rmse_extraction <- function(model,log_or_nolog){
  output_list <- c()
  for(position in tolower(position_list)){

    modeldata <- readRDS(paste0("data_model/models/model_",position,"_",model,".rds"))
    modelinput_x <- readRDS(paste0("data_model/inputs/",position,"_test_x_",log_or_nolog,".rds")) %>% as.data.frame()
    modelinput_y <- readRDS(paste0("data_model/inputs/",position,"_test_y_",log_or_nolog,".rds")) %>% as.data.frame()
    # rsqu <- as.numeric(summary(modeldata)$r.squared)
    
    predictedVal <- predict(modeldata,newdata = modelinput_x)
    modelvalues <- data.frame(obs = if(log_or_nolog == "log"){ exp(modelinput_y$total_points) } else { modelinput_y$total_points },
                              pred = if(log_or_nolog == "log"){ exp(predictedVal) } else { predictedVal})
    rmse <- round(as.numeric(defaultSummary(modelvalues)[1]),2)
    # rmse2 <- ifelse(log_or_nolog == "log", exp(rmse), rmse)
    
    output_list <- c(output_list, rmse)
  }
  return(output_list)
}

# mse_extraction("lm_5g_nolog_onevar","nolog")


# Plotting one variable model
rsquared_onevar <- tibble(element_type = position_list, rsquared = rsqu_extraction("lm_5g_nolog_onevar","nolog")) %>%
  gather(element_type, rsquared) %>%
  mutate(model = "Single Variable Linear Model")

basic_model_plot <- ggplot(dataset,aes(n_total_points,total_points, colour = element_type)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(data = rsquared_onevar, mapping = aes(x = 10, y = 58, label = paste0("Model Rsq = ",round(rsquared * 100, 0),"%")), show.legend = FALSE) +
  CustomTheme +
  facet_wrap(~element_type) +
  labs(x = "Total Points (Last 5 Games)", y = "Total Points (Next 5 Games)", colour = "Position") +
  ggtitle("Relationship between points scored over last five games and next five games")

png("outputs/basic_model_plot.png", width=1280,height=800,res=144)
basic_model_plot
dev.off()

# Comparing Rsquared and RMSE of different models

rsq_tibble <- tibble("element_type" = position_list, 
                    "Single-Variable Linear Model (with log)" = rsqu_extraction("lm_5g_log_onevar", "log"),
                    "Single-Variable Linear Model (without log)" = rsqu_extraction("lm_5g_nolog_onevar","nolog"),
                    "Multi-Variable Linear Model (with log)" = rsqu_extraction("lm_5g_log","log"),
                    "Multi-Variable Linear Model (without log)" = rsqu_extraction("lm_5g_nolog","nolog"),
                    "Random Forest Model" = rsqu_extraction("rf_5g_nolog","nolog"),
                    "XGBoost Model" = rsqu_extraction("xgbTree_5g_nolog", "nolog")
                    ) %>%
  gather(model, rsquared, -element_type)

rmse_tibble <- tibble("element_type" = position_list, 
                     "Single-Variable Linear Model (with log)" = rmse_extraction("lm_5g_log_onevar", "log"),
                     "Single-Variable Linear Model (without log)" = rmse_extraction("lm_5g_nolog_onevar","nolog"),
                     "Multi-Variable Linear Model (with log)" = rmse_extraction("lm_5g_log","log"),
                     "Multi-Variable Linear Model (without log)" = rmse_extraction("lm_5g_nolog","nolog"),
                     "Random Forest Model" = rmse_extraction("rf_5g_nolog","nolog"),
                     "XGBoost Model" = rmse_extraction("xgbTree_5g_nolog", "nolog")
                     ) %>%
  gather(model, rmse, -element_type)

c_lightblue = "#00A8C8"
c_lightgreen = "#90BE44"
c_orange = "#FBAE17"
c_pink = "#ED2C67"
c_darkgreen = "#0FB694"

model_comparison_rsq <- ggplot(rsq_tibble, aes(reorder(element_type, rsquared), rsquared, fill = reorder(model, rsquared))) +
  geom_col(position = "dodge") +
  CustomTheme +
  labs(y = "R-squared", x = "Position", fill = "Model", title = "Model Accuracy Comparison") +
  scale_fill_manual(values = c(c_lightblue, c_pink, c_lightgreen, c_orange, c_darkgreen, c_lightgreen))

png("outputs/model_accuracy_comparison_rsq.png", width = 1280, height = 800, res = 144)
model_comparison_rsq
dev.off()

model_comparison_mse <- ggplot(rmse_tibble, aes(reorder(element_type, rmse), rmse, fill = reorder(model, rmse))) +
  geom_col(position = "dodge") +
  CustomTheme +
  labs(y = "RMSE", x = "Position", fill = "Model", title = "Model Accuracy Comparison") +
  scale_fill_manual(values = c(c_lightblue, c_pink, c_lightgreen, c_orange, c_darkgreen, c_lightgreen))

png("outputs/model_accuracy_comparison_rmse.png", width = 1280, height = 800, res = 144)
model_comparison_mse
dev.off()


# Examining random forest tree --------------------------------------------


f_tree_graph <- function(model, tree_num, tree_label_numbers, label_prob){
  
  final_model <- model$finalModel
  
  renames <- tibble(split_var = c("n_minutes", "n_ict_index", "n_attack_strength", "attack_strength", "n_offside", "n_assists", "n_bps",
                                  "n_big_chances_created", "n_defence_strength", "n_big_chances_missed",  "n_tackles",            
                                  "n_recoveries", "n_clearances_blocks_interceptions",  "defence_strength", "n_threat",
                                  "n_creativity", "n_attempted_passes", "n_completed_passes", "n_fouls",
                                  "n_bonus", "n_goals_conceded", "n_dribbles", "n_tackled",                        
                                  "n_influence", "n_winning_goals", "n_target_missed", "n_goals_scored",
                                  "n_clean_sheets", "n_open_play_crosses", "n_key_passes", "n_total_points"),
                    split_var_2 = c("Minutes", "ICT Index", "Past atk adv", "Future atk str", "Offsides", "Assists", "Bonus Points",
                                    "Big Chances Created", "Past def adv", "Big chances missed",  "Tackles",            
                                    "Recoveries", "Clear-Block-Intercept",  "Future def adv", "Opp Threat",
                                    "Creativity", "Attempted Passes", "Completed Passes", "Fouls",
                                    "Bonuses", "Goals conceded", "Dribbles", "Tackled",                        
                                    "Influence", "Winning Goals", "Target Missed", "Goals Scored",
                                    "Clean Sheets", "Open Play Crosses", "Key Passes", "Total Points"))
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(label = ifelse(`left daughter` == 0 | `right daughter` == 0, "No", 
                          ifelse(`left daughter` <= tree_label_numbers | `right daughter` <= tree_label_numbers, "Yes", "No")),
           `split point` = ifelse(!is.na(prediction), `split point`, NA),
           `split point` = ifelse(label == "Yes", `split point`, NA),
           `split var` = as.character(`split var`),
           `split var` = ifelse(label == "Yes", `split var`, NA)
    ) %>%
    rowwise() %>%
    mutate(prediction = ifelse(label == "Yes", prediction, ifelse(runif(1,0,1) <= label_prob, prediction, NA ))) %>%
    as_tibble() %>%
    left_join(renames, by = c("split var" = "split_var")) %>%
    select(-"split var") %>%
    rename("split var" = "split_var_2") %>%
    mutate(node_label = ifelse(label == "Yes", paste0(`split var`, ": ", `split point`), NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- tree$node_label
  V(graph)$leaf_label <- as.character(round(tree$prediction, 2))
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    # geom_node_label(aes(label = split), vjust = -1.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE,
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    geom_node_label(aes(label = node_label), na.rm = TRUE, repel = TRUE, hjust = "middle", vjust = -2.5) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  plot
}

position <- "forward"
model_rf <- readRDS(paste0("data_model/models/model_", position, "_rf_5g_nolog.rds"))
smallest_tree <- which(model_rf$finalModel$forest$ndbigtree == min(model_rf$finalModel$forest$ndbigtree))

png(paste0("outputs/",tolower(position),"_rf_5g_nolog_treegraph.png"), width=2500,height=1500,res=144)

print(f_tree_graph(model_rf, smallest_tree, tree_label_numbers = 100, label_prob = 0.2))

dev.off()


# 
# position = "forward"
# model = "rf_5g_nolog"
# log_or_nolog = "nolog"
# 
# modeldata <- readRDS(paste0("data_model/models/model_",position,"_",model,".rds"))
# modelinput_x <- readRDS(paste0("data_model/inputs/",position,"_test_x_",log_or_nolog,".rds")) %>% as.data.frame()
# modelinput_y <- readRDS(paste0("data_model/inputs/",position,"_test_y_",log_or_nolog,".rds")) %>% as.data.frame()
# # rsqu <- as.numeric(summary(modeldata)$r.squared)
# 
# predictedVal <- predict(modeldata,newdata = modelinput_x)
# modelvalues <- data.frame(obs = if(log_or_nolog == "log"){exp(modelinput_y$total_points)} else {modelinput_y$total_points},
#                           pred = if(log_or_nolog == "log"){exp(predictedVal)} else {predictedVal})
# rmse <- round(as.numeric(defaultSummary(modelvalues)[1]),2)
