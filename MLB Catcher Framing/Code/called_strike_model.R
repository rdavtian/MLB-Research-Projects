library(stringr)
library(tidyverse)
require(caTools)
library(baseballr)
library(ggplot2)
library(psych)
library(lubridate)
library(zoo)
library(caret)
library(MLmetrics)
library(kableExtra)
current_season <- as.integer(substr(Sys.Date(), 1, 4)) - 1

teamnames <- baseballr::teams_lu_table %>% 
  filter(sport.name == "Major League Baseball") %>% 
  select(teamName, abbreviation) %>% distinct() %>% 
  mutate(teamName = case_when(teamName == "D-backs" ~ "Dbacks", TRUE ~ teamName)) %>% 
  rename("Team" = "abbreviation") %>% 
  add_row(teamName = 'Dbacks', Team = 'AZ') %>% 
  arrange(teamName)

create_strikezone <- function()
{
  x <- c(-.95,.95,.95,-.95,-.95)
  z <- c(1.5,1.5,3.5,3.5,1.5)
  sz <- as.data.frame(tibble(x,z)) 
  g <- ggplot() + geom_path(data = sz, aes(x=x, y=z), lwd = 1.6) +
    coord_equal() + xlab("feet from home plate") +
    ylab("feet above the ground") + xlim(-1.8,1.8) + ylim(0.6,4.4)
  return(g)
}

pitch_chart <- function(data, title)
{
  zone <- create_strikezone()
  zone + geom_point(data = data, aes(x = pitch_location_side, y = pitch_location_height, color = pitch_name)) +
    guides(colour = guide_legend(override.aes = list(size=3))) + 
    labs(color = "Pitch Type", title = title) +
    ylab("Feet Above Homeplate") +
    xlab("Feet From Homeplate (Catcher's Perspective)") +
    theme(plot.title=element_text(hjust=0.5,vjust=0,size=22,face = 'bold'),
          plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 9)) +
    theme(axis.text.x=element_text(vjust = .5, size=13,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=13,colour="#535353",face="bold")) +
    theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) +
    theme(panel.background = element_rect(fill = "white")) +
    facet_grid(~ batter_side) + 
    theme(strip.text = element_text(face="bold", size=12),
          strip.background = element_rect(fill="lightblue", colour="black",size=1))
}

pitch_chart2 <- function(data, title)
{
  zone <- create_strikezone()
  zone + geom_point(data = data, aes(x = pitch_location_side, y = pitch_location_height, color = pitch_name)) +
    guides(colour = guide_legend(override.aes = list(size=5))) + 
    labs(color = "Pitch Type", title = title) +
    ylab("Feet Above Homeplate") +
    xlab("Feet From Homeplate (Catcher's Perspective)") +
    theme(plot.title=element_text(hjust=0.5,vjust=0,size=20,face = 'bold'),
          plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 9)) +
    theme(axis.text.x=element_text(vjust = .5, size=13,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=13,colour="#535353",face="bold")) +
    theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) +
    theme(panel.background = element_rect(fill = "white")) +
    theme(strip.text = element_text(face="bold", size=12),
          strip.background = element_rect(fill="lightblue", colour="black",size=1))
}

statcast_scraper <- function(start_date, end_date) 
{
  dates <- seq.Date(as.Date(start_date), as.Date(end_date), by = 3)
  date_grid <- tibble(start_date = dates, end_date = dates + 2)
  df <- list()
  for (i in 1:nrow(date_grid))
  {
    message(paste0('Scraping days ', date_grid$start_date[i], ' through ', date_grid$end_date[i], "\n"))
    df[[i]] <- scrape_statcast_savant(as.character(date_grid$start_date[i]), as.character(date_grid$end_date[i]), player_type = "batter")
  }
  df <- purrr::keep(df, ~nrow(.) > 0)
  combined <- bind_rows(df)
  #safe_savant <- safely(scrape_statcast_savant)
  #payload <- map(.x = seq_along(date_grid$start_date), 
  #~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
  
  #payload <- safe_savant(start_date = date_grid$start_date[.x], 
  #end_date = date_grid$end_date[.x], type = 'pitcher')
  
  #return(payload)
  #})
  #payload_df <- map(payload, 'result')
  #number_rows <- map_df(.x = seq_along(payload_df), 
  #~{number_rows <- tibble(week = .x, 
  #number_rows = length(payload_df[[.x]]$game_date))}) %>%
  #filter(number_rows > 0) %>%
  #pull(week)
  #payload_df_reduced <- payload_df[number_rows]
  #combined <- payload_df_reduced %>% bind_rows()
  return(combined %>% filter(game_type == "R"))
}


clean_statcast_data_for_modeling <- function(clean_data)
{
  clean_data2 <- clean_data %>%
    filter(pitch_name != "Intentional Ball", pitch_name != "Pitch Out", strikes < 3) %>%
    mutate(balls = case_when((balls > as.integer(3)) & (events != "walk") ~ as.integer(3), TRUE ~ balls)) %>% 
    filter(balls < 4) %>% 
    mutate(plate_x = -plate_x,
           #home_team = case_when(home_team == "AZ" ~ "ARI", TRUE ~ home_team),
           #away_team = case_when(away_team == "AZ" ~ "ARI", TRUE ~ home_team),
           count = paste(balls, strikes, sep="-"),
           count = as.factor(count),
           two_strikes = case_when(strikes == 2 ~ 1, TRUE ~ 0),
           base_state = case_when(is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ "Bases Cleared",
                                  !is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ "1st Only",
                                  is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ "2nd Only",
                                  !is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ "1st and 2nd",
                                  is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ "3rd Only",
                                  !is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ "1st and 3rd",
                                  is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ "2nd and 3rd",
                                  !is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ "Bases Loaded",
                                  TRUE ~ NA_character_),
           base_state = as.factor(base_state),
           is_swing = case_when(description %in% c("hit_into_play_no_out","hit_into_play",
                                                   "swinging_strike","foul","foul_tip",
                                                   "foul_bunt","hit_into_play_score","bunt_foul_tip",
                                                   "swinging_strike_blocked","missed_bunt") ~ 1, TRUE ~ 0),
           is_called_strike = case_when(description == "called_strike" ~ 1, TRUE ~ 0),
           in_zone = case_when((plate_z <= sz_top & plate_z >= sz_bot & abs(plate_x) <= 0.95) ~ 1, TRUE ~ 0),
           zone_length = 3.5 - 1.5,
           attack_zone = case_when((abs(plate_x) <= 0.9 * (20/10)) &
                                     (plate_z <= (zone_length / 2)*(9/3) + 1.5) &
                                     (plate_z >= 1.5 - (12/12)) ~ "Chase", TRUE ~ ""),
           attack_zone = case_when((abs(plate_x) <= 0.9 * (13.3/10)) &
                                     (plate_z <= (zone_length / 2)*(7/3) + 1.5) &
                                     (plate_z >= 1.5 - (4/12)) ~ "Shadow", TRUE ~ attack_zone),
           attack_zone = case_when(in_zone == 1 ~ "Strike Zone", TRUE ~ attack_zone),
           attack_zone = case_when((abs(plate_x) <= 0.9 * (6.7/10)) &
                                     (plate_z <= (zone_length / 2)*(5/3) + 1.5) &
                                     (plate_z >= (zone_length / 2)*(1/3) + 1.5) ~ "Heart", TRUE ~ attack_zone),
           attack_zone = case_when((abs(plate_x) <= 0.9 * (3.3/10)) &
                                     (plate_z <= (zone_length / 2)*(4/3) + 1.5) &
                                     (plate_z >= (zone_length / 2)*(2/3) + 1.5) ~ "Meat", TRUE ~ attack_zone),
           attack_zone = case_when(attack_zone == "" ~ "Waste", TRUE ~ attack_zone),
           attack_zone = factor(attack_zone, levels=c("Meat","Heart","Strike Zone","Shadow","Chase","Waste")),
           pitch_name2 = case_when(pitch_name %in% c("4-Seam Fastball","2-Seam Fastball") ~ 'Fastball',
                                   pitch_name %in% c("Curveball","Knuckle Curve","Slow Curve") ~ 'Curveball',
                                   pitch_name == 'Split-Finger' ~ 'Splitter',
                                   TRUE ~ pitch_name),
           pitch_type2 = case_when(pitch_type %in% c("FF","FT") ~ 'FB',
                                   pitch_type %in% c("CU","KC","CS") ~ 'CU',
                                   pitch_type == 'ST' ~ 'SW',
                                   TRUE ~ pitch_type),
           release_speed2 = case_when( (release_speed >= 70) & (release_speed <= 75) ~ '70-75',
                                       (release_speed >= 75) & (release_speed <= 80) ~ '75-80',
                                       (release_speed >= 80) & (release_speed <= 85) ~ '80-85',
                                       (release_speed >= 85) & (release_speed <= 90) ~ '85-90',
                                       (release_speed >= 90) & (release_speed <= 95) ~ '90-95',
                                       (release_speed >= 95) & (release_speed <= 100) ~ '95-100',
                                       release_speed < 70 ~ '< 70',
                                       release_speed > 100 ~ '> 100'),
           events = case_when(events %in% c("grounded_into_double_play","double_play") ~ 'Double Play',
                              events == 'strikeout' ~ 'Strike Out',
                              events == 'walk' ~ 'Walk',
                              events == 'single' ~ 'Single',
                              events == 'double' ~ 'Double',
                              events == 'triple' ~ 'Triple',
                              events == 'home_run' ~ 'Home Run',
                              events == 'sac_fly' ~ 'Sac Fly',
                              events == 'sac_bunt' ~ 'Sac Bunt',
                              events == 'field_out' ~ 'Field Out',
                              events == 'force_out' ~ 'Force Out',
                              events == 'hit_by_pitch' ~ 'HBP',
                              events == 'field_error' ~ 'Error'),
           events = case_when(grepl("reaches on a fielder's choice", des) ~ 'Force Out',
                              TRUE ~ events),
           events = case_when(grepl("flies into a sacrifice double play", des) ~ 'Sac Fly',
                              TRUE ~ events),
           events = case_when(grepl("ground bunts into a sacrifice double play", des) ~ 'Sac Bunt',
                              TRUE ~ events),
           events2 = case_when(events %in% c("Double","Triple","Home Run") ~ "XBH", 
                               events %in% c("Double Play","Field Out","Force Out","Sac Bunt","Sac Fly") ~ "Field Out",
                               TRUE ~ events),
           p_throws = case_when(p_throws == 'R' ~ 'RHP',
                                p_throws == 'L' ~ 'LHP'),
           stand = case_when(stand == 'R' ~ 'RHB',
                             stand == 'L' ~ 'LHB'),
           is_platoon = case_when((stand == "RHB" & p_throws == "LHP") ~ 1,
                                  (stand == "LHB" & p_throws == "RHP") ~ 1,
                                  TRUE ~ 0),
           game_month = lubridate::month(game_date, label = TRUE),
           pfx_x = pfx_x * -12,
           pfx_z = pfx_z * 12) %>%
    filter(is_swing == 0, pitch_name != "Other") %>% 
    filter(!pitch_name2 %in% c("","Screwball","Eephus","Knuckleball")) %>% 
    filter(!is.na(pfx_z), !is.na(pfx_x), !is.na(plate_x), !is.na(plate_z),
           !is.na(release_speed), !is.na(release_spin_rate)) %>%
    mutate(pitch_name3 = case_when(pitch_name2 %in% c("Fastball","Sinker") ~ "Fastball",
                                   pitch_name2 %in% c("Cutter","Slider","Curveball","Slurve","Sweeper") ~ "Breaking Ball",
                                   pitch_name2 %in% c("Changeup","Splitter","Split-Finger") ~ "Offspeed")) %>% 
    select(where(~!all(is.na(.x)))) %>% 
    mutate(events3 = case_when(events %in% c("Field Out","Force Out","Sac Fly","Sac Bunt") ~ "Out",
                               TRUE ~ events),
           events3 = case_when(description %in% c("ball","blocked_ball","pitchout") ~ "Ball",
                               description %in% c("called_strike","bunt_foul_tip","foul","foul_bunt",
                                                  "foul_tip","missed_bunt","swinging_strike",
                                                  "swinging_strike_blocked") ~ "Strike",
                               TRUE ~ events3)) %>% 
    filter(!is.na(events3)) %>%
    left_join(teamnames, by = c("home_team" = "Team")) %>% rename("home_team_name" = "teamName") %>% 
    left_join(teamnames, by = c("away_team" = "Team")) %>% rename("away_team_name" = "teamName") %>%
    mutate(pitcher_team = case_when(inning_topbot == "Top" ~ home_team_name, 
                                    TRUE ~ away_team_name),
           batter_team = case_when(inning_topbot == "Bot" ~ home_team_name,
                                   TRUE ~ away_team_name))
  
  return(clean_data2)
}

train_test_split <- function(data, y_var, split_perc)
{
  set.seed(100)
  trainRowNumbers <- createDataPartition(data %>% select(all_of(y_var)) %>% pull(), p = split_perc, list = FALSE)
  train <- data[trainRowNumbers,]
  test <- data[-trainRowNumbers,]
  return(list(train, test))
}

run_model <- function(data, x_vars, y_var, model_type, sampling_method = NA, sd = FALSE, tuneLength, plot = FALSE)
{
  set.seed(100)
  train_test <- train_test_split(data, y_var, 0.8)
  train <- train_test[[1]] %>% select(all_of(x_vars), all_of(y_var))
  test <- train_test[[2]] %>% select(all_of(x_vars), all_of(y_var))
  if (sd == TRUE)
  {
    impute <- preProcess(train[, y_var], method = c("center","scale"))
    train <- predict(impute, newdata = train)
  }
  dummy <- dummyVars(" ~ .", data = train)
  train <- data.frame(predict(dummy, newdata = train))
  train <- train %>% mutate(is_called_strike = case_when(is_called_strike == 1 ~ "Yes", TRUE ~ "No"))
  myFolds <- createFolds(train$is_called_strike, k = 3)
  
  mod_weights <- ifelse(train$is_called_strike == "Yes", 1 / (table(train$is_called_strike) / nrow(train))[2],
                        1 / (table(train$is_called_strike) / nrow(train))[1])
  
  if (!is.na(sampling_method))
  {
    control <- trainControl(method = "cv", number = 3, index = myFolds, savePredictions = TRUE, 
                            classProbs = T, summaryFunction = mnLogLoss, sampling = sampling_method)
  } else {
    control <- trainControl(method = "cv", number = 3, index = myFolds, savePredictions = TRUE, 
                            classProbs = T, summaryFunction = mnLogLoss)
  }
  if (model_type == "glm")
  {
    model <- train(as.formula(paste0("is_called_strike ~ .")), data = train, method = model_type, 
                   family = 'binomial', trControl = control, metric="logLoss")
  } else if (model_type == "glmnet") {
    set.seed(100)
    model <- train(as.formula(paste0("is_called_strike ~ .")), data = train, method = model_type, 
                   tuneLength = tuneLength, family = 'binomial', trControl = control, metric="logLoss")
  } else if (model_type == "gbm") {
    set.seed(100)
    model <- train(as.formula(paste0("is_called_strike ~ .")), data = train, method = model_type, 
                   tuneLength = tuneLength, verbose = F, trControl=control, metric="logLoss")
  } else if (model_type == "ranger") {
    set.seed(100)
    model <- train(as.formula(paste0("is_called_strike ~ .")), data = train, method = model_type,
                   tuneLength = tuneLength, verbose = F, importance = "impurity", #permutation 
                   trControl=control, metric="logLoss")
  } else {
    set.seed(100)
    model <- train(as.formula(paste0("is_called_strike ~ .")), data = train, method = model_type,
                   tuneLength = tuneLength, trControl=control, metric="logLoss")
  }
  if ((!model_type %in% c('gbm',"ctree","C5.0")) & (plot == TRUE))
  {
    if (model_type == "glm")
    {
      model_type2 <- "Logistic Regression"
    } else if (model_type == "glmnet") {
      model_type2 <- "Regularized Logistic Regression"
    } else if (model_type %in% c("rf","ranger")) {
      model_type2 <- "Random Forest"
    } else if (model_type == "xgbTree") {
      model_type2 <- "XGBoost"
    } 
    imp2 <- varImp(model)
    imp_plot <- barchart(sort(rowMeans(imp2$importance), decreasing = T), main = paste0(model_type2, " Variable Importance Chart"), xlab = "Average Level of Importance", ylab = "Variables", fill="cyan2")
  }
  else if ((model_type == 'gbm') & (plot == TRUE))
  {
    var_imp <- summary(model)[2]
    labels <- row.names(var_imp)
    var_imp <- var_imp[1:30,]
    labels <- labels[1:30]
    df <- data.frame(labels, var_imp)
    imp_plot <- ggplot(df, aes(x = reorder(labels, var_imp), y = var_imp)) +
      geom_bar(stat = "identity", fill = "black") +
      ggtitle(paste0("GBM Variable Importance Predicting Called Strike")) + 
      coord_flip() + scale_y_continuous(name="Variable Important (0-100)") +
      scale_x_discrete(name="") +
      geom_label(aes(label = round(var_imp, 1)), size = 4, alpha = 1) + 
      geom_label(aes(label = round(var_imp, 1)), size = 4.1, alpha = 1) + 
      theme(axis.text.x = element_text(face="bold", color="#008000",
                                       size=8, angle=0),
            axis.text.y = element_text(face="bold", color="#008000",
                                       size=8, angle=0)) + 
      theme(plot.title=element_text(hjust=0.5,vjust=0,size=18,face = 'bold'),
            plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 9)) +
      theme(axis.text.x=element_text(vjust = .5, size=15,colour="#535353",face="bold")) +
      theme(axis.text.y=element_text(size=15,colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0)) +
      theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
      theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) +
      theme(panel.background = element_rect(fill = "white")) +
      theme(strip.text = element_text(face="bold", size=13),
            strip.background = element_rect(fill="cyan", colour="black",size=1))
  } else if ((model_type == "C5.0") & (plot == TRUE)) {
    var_imp <- C5imp(model, metric = "usage", pct = TRUE)
  }
  if (sd == TRUE)
  {
    test <- predict(impute, test)
  }
  test <- data.frame(predict(dummy, newdata = test))
  test <- test %>% mutate(is_called_strike = case_when(is_called_strike == 1 ~ "Yes", TRUE ~ "No"))
  test$is_called_strike <- as.factor(test$is_called_strike)
  test$is_called_strike <- relevel(test$is_called_strike, ref = "Yes")
  test$is_called_strike_Pred_Prob <- predict(model, newdata = test, type = "prob")[,2]
  test$is_called_strike_Pred_Raw <- predict(model, newdata = test, type="raw")
  test$is_called_strike_Pred_Raw <- relevel(test$is_called_strike_Pred_Raw, ref = "Yes")
  #test$Whiff_Pred_Raw <- predict(model, newdata = test, type = 'raw')
  cM <- confusionMatrix(test$is_called_strike_Pred_Raw, test$is_called_strike, positive = "Yes")
  colnames(cM$table) <- c("Called Strike (True)","Called Ball (True)")
  rownames(cM$table) <- c("Called Strike (Pred)","Called Ball (Pred)")
  preds_vs_actual <- as.data.frame(cbind(as.character(test$is_called_strike_Pred_Raw), as.character(test$is_called_strike)))
  names(preds_vs_actual) <- c("Called_Strike_Preds","Called_Strike")
  if (plot == TRUE)
  {
    return(list(model, preds_vs_actual, cM, test, imp_plot, dummy))
  } else {
    return(list(model, preds_vs_actual, cM, test, dummy))
  }
}

plot_prediction_density <- function(data, pred, title)
{
  iqr <- quantile(data[, pred], 0.75) - quantile(data[, pred], 0.25)
  h <- 0.9*min(sd(data[, pred]), (iqr / 1.34)) * nrow(data)^(-0.2)
  plot <- ggplot(data = data, aes(x = !! rlang::sym(pred))) + 
    geom_density(stat = "density", alpha = 1, kernel = 'gaussian', fill = "cyan2", bw = h) + 
    labs(x = "Predicted Probability of Swing & Miss", y="Density", title = title) + 
    theme(plot.title=element_text(hjust=0.5,vjust=0,size=20,face = 'bold'), 
          plot.subtitle=element_text(face="bold", hjust= 0.5, vjust= .09, colour="#3C3C3C", size = 10)) +
    theme(axis.text.x=element_text(vjust = .5, size=17,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=17,colour="#535353",face="bold")) +
    theme(axis.title.y=element_text(size=17,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=17,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) + 
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) + 
    geom_vline(xintercept = mean(data[, pred]), color="red", size = 1.3) + 
    annotate(geom="text", x=mean(data[, pred]) + 0.05, y=2, label="Mean",color="red", size = 5)
  return(plot)
}

#####################################################################################
# Scrape pitch by pitch statcast data
sc <- statcast_scraper("2022-04-07", "2022-10-05")

# Clean data for modeling
sc_clean <- clean_statcast_data_for_modeling(sc)
sc_clean2 <- sc_clean %>% filter((events2 != "Field Out") %>% replace_na(TRUE))

# balance of classes
tab <- data.frame(round(prop.table(sort(table(sc_clean2$is_called_strike))) * 100,1))
tab <- tab %>% sjmisc::rotate_df(cn = T) 
colnames(tab) = c("Called Strike %", "Called Ball %")
tab <- tab %>% select(`Called Strike %`, `Called Ball %`) %>% 
  mutate(`Called Strike %` = paste0(`Called Strike %`, " %"),
         `Called Ball %` = paste0(`Called Ball %`, " %"))
kable(tab, row.names = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, position = "center", fixed_thead = T) %>% 
  footnote(symbol = "Distribution of Called Strike vs Called Ball")


# select input and target variables
y_var <- "is_called_strike"
x_vars <- c("pitch_name2","count","release_speed","stand","pfx_x","pfx_z",
            "plate_x","plate_z","sz_top","sz_bot","strikes","release_spin_rate","is_platoon")
vars_to_keep <- c("pitch_name2","count","release_speed","zone","stand","pfx_x","pfx_z",
                  "plate_x","plate_z","sz_top","sz_bot","strikes","release_spin_rate","is_platoon",
                  "is_called_strike","player_name","batter","pitcher","fielder_2","game_date",
                  "in_zone","attack_zone")

df <- sc_clean2 %>% select(all_of(vars_to_keep)) %>% na.omit()
df_train <- df %>% filter(game_date <= "2022-08-31")
df_validate <- df %>% filter(game_date >= "2022-09-01")
#####################################################################################
mod <- run_model(df_train, x_vars, y_var, "xgbTree", NA, FALSE, 4, TRUE)
mod[[6]]
mod[[3]]; mod[[5]]

#saveRDS(mod[[1]], "C:/Users/rusla/OneDrive/MLBAnalyticsJobs/MLB Catcher Framing/Data/called_strike_model_object_22_xgb.rds")
saveRDS(mod[[6]],"C:/Users/rusla/OneDrive/MLBAnalyticsJobs/MLB Catcher Framing/Data/dummy.rds")
df_validate <- df_validate %>% 
  mutate(is_called_strike_raw = case_when(is_called_strike == 1 ~ "Yes", TRUE ~ "No"),
         is_called_strike_raw = as.factor(is_called_strike_raw))
df_validate$xcalled_strike <- predict(mod[[1]], data.frame(predict(mod[[6]], newdata = df_validate)), type = "prob")[,2]
df_validate$called_strike_raw_pred <- predict(mod[[1]], data.frame(predict(mod[[6]], newdata = df_validate)), type = "raw")

#cM <- confusionMatrix(df_validate$called_strike_raw_pred, df_validate$is_called_strike_raw, positive = "Yes")
#colnames(cM$table) <- c("Called Strike (True)","Called Ball (True)")
#rownames(cM$table) <- c("Called Strike (Pred)","Called Ball (Pred)")

# 0.565 glmnet, 0.154 ranger, 0.147 gbm, 0.142 xgbtree
# gbm, n.trees = 250, interaction.depth = 5, shrinkage = 0.1 and n.minobsinnode = 10. 
# xgbtree,  nrounds = 100, max_depth = 4, eta = 0.3, gamma = 0, colsample_bytree = 0.6, min_child_weight = 1 and subsample = 1

ll <- round(MLmetrics::LogLoss(y_pred = df_validate$xcalled_strike, y_true = df_validate$is_called_strike),3); ll
