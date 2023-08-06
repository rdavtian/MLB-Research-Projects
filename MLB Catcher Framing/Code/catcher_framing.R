library(stringr)
library(tidyverse)
require(caTools)
library(baseballr)
library(ggplot2)
library(psych)
library(lubridate)
library(zoo)
library(caret)
current_season <- as.integer(substr(Sys.Date(), 1, 4)) - 1

teamnames <- baseballr::teams_lu_table %>% 
  filter(sport.name == "Major League Baseball") %>% 
  select(teamName, abbreviation) %>% distinct() %>% 
  mutate(teamName = case_when(teamName == "D-backs" ~ "Dbacks", TRUE ~ teamName)) %>% 
  rename("Team" = "abbreviation") %>% 
  add_row(teamName = 'Dbacks', Team = 'AZ') %>%
  mutate(teamName = case_when(Team == "CLE" ~ "Guardians", TRUE ~ teamName)) %>% 
  arrange(teamName)

player_name_player_id <- function(season)
{
  team_ids <- baseballr::teams_lu_table %>% 
    filter(sport.name == "Major League Baseball", active == TRUE) %>% 
    select(id) %>% pull()
  
  df <- data.frame()
  for (id in team_ids)
  { 
    i <- 1
    roster <- baseballr::mlb_rosters(team_id = id, season = season, roster_type = '40Man')
    df <- bind_rows(roster, df)
  }
  
  df <- df %>% select(person_id, person_full_name, team_id) %>% distinct() %>% 
    inner_join(teams_lu_table %>% filter(sport.name == "Major League Baseball") %>% 
                 select(id, teamName) %>% distinct(),
               by = c("team_id" = "id")) %>% rename("Team" = "teamName") %>% 
    select(person_id, person_full_name) %>% distinct()
  return(df)
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

clean_statcast_data_for_rv <- function(data)
{
  data2 <- data %>% 
    mutate(events = case_when(events %in% c("grounded_into_double_play","double_play") ~ 'Double Play',
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
           is_out = case_when(events %in% c("Field Out","Strike Out","Sac Fly",
                                            "Sac Bunt", "Force Out") ~ 1, TRUE ~ 0),
           is_out = case_when(events %in% c("Double Play") ~ 2, TRUE ~ is_out))
  
  a = str_count(data2$des, "score")
  b = str_count(data2$des, "homer")
  c = str_count(data2$des, "grand slam")
  a[is.na(a)] = 0
  b[is.na(b)] = 0
  c[is.na(c)] = 0
  data2$runs_scored = a + b + c
  return(data2)
}

find_run_value <- function(data, run_exp)
{
  data$on_1b = ifelse(!is.na(data$on_1b), 1, 0)
  data$on_2b = ifelse(!is.na(data$on_2b), 1, 0)
  data$on_3b = ifelse(!is.na(data$on_3b), 1, 0)
  
  data2 <- data %>%
    arrange(pitcher, game_date, home_team, away_team, inning, desc(inning_topbot), 
            at_bat_number, pitch_number) %>%
    inner_join(run_exp, by = c("balls","strikes","on_1b","on_2b","on_3b","outs_when_up"))
  
  data2$RE_after = ifelse(data2$inning_topbot == lead(data2$inning_topbot), lead(data2$RE, 1), 0)
  data2 <- data2 %>%
    mutate(RE_after = case_when(outs_when_up + is_out == 3 ~ 0, TRUE ~ RE_after),
           RE_diff = RE_after - RE, 
           playRE = RE_diff + runs_scored)
  data2$RV <- ifelse(!is.na(data2$events == 0), data2$playRE, data2$RE_diff)
  return(data2)
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
           #events = case_when(events %in% c("grounded_into_double_play","double_play") ~ 'Double Play',
                              #events == 'strikeout' ~ 'Strike Out',
                              #events == 'walk' ~ 'Walk',
                              #events == 'single' ~ 'Single',
                              #events == 'double' ~ 'Double',
                              #events == 'triple' ~ 'Triple',
                              #events == 'home_run' ~ 'Home Run',
                              #events == 'sac_fly' ~ 'Sac Fly',
                              #events == 'sac_bunt' ~ 'Sac Bunt',
                              #events == 'field_out' ~ 'Field Out',
                              #events == 'force_out' ~ 'Force Out',
                              #events == 'hit_by_pitch' ~ 'HBP',
                              #events == 'field_error' ~ 'Error'),
           #events = case_when(grepl("reaches on a fielder's choice", des) ~ 'Force Out',
                              #TRUE ~ events),
           #events = case_when(grepl("flies into a sacrifice double play", des) ~ 'Sac Fly',
                              #TRUE ~ events),
           #events = case_when(grepl("ground bunts into a sacrifice double play", des) ~ 'Sac Bunt',
                              #TRUE ~ events),
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

#####################################################################################
ids <- player_name_player_id(2023)
sc <- statcast_scraper("2023-03-30", Sys.Date())
sc_clean <- clean_statcast_data_for_rv(sc)#; rm(sc)

run_expectancy_values <- read.csv(paste0("C:/Users/rusla/OneDrive/MLBAnalyticsJobs/MLB Catcher Framing/Data/run_expectancy_values_", current_season, ".csv"))
sc_clean2 <- find_run_value(sc_clean, run_expectancy_values)

df <- clean_statcast_data_for_modeling(sc_clean2)#; rm(sc_clean2)
dummy <- readRDS("C:/Users/rusla/OneDrive/MLBAnalyticsJobs/MLB Catcher Framing/Data/dummy.rds")
mod <- readRDS("C:/Users/rusla/OneDrive/MLBAnalyticsJobs/MLB Catcher Framing/Data/called_strike_model_object_22_xgb.rds")
df$xcalled_strike <- predict(mod, data.frame(predict(dummy, newdata = df)), type = "prob")[,2]
df <- df %>% 
  mutate(CFR = case_when(is_called_strike == 1 ~ (1 - xcalled_strike)*RV*-1, TRUE ~ xcalled_strike*RV*-1),
         FR = case_when(is_called_strike == 1 ~ (1 - xcalled_strike), TRUE ~ xcalled_strike*-1),
         Season = as.integer(substr(game_date, 1, 4)))

cfr <- df %>% 
  group_by(Season, pitcher_team, fielder_2) %>% 
  summarise(`Total CFR` = round(sum(CFR, na.rm = T),2),
            N = n(), 
            .groups = "drop") %>% 
  mutate(`CFR/100` = round((`Total CFR` / N) * 100,3)) %>% 
  arrange(desc(`CFR/100`)) %>% 
  inner_join(ids, by = c("fielder_2" = "person_id")) %>% 
  rename("Team" = "pitcher_team", "Catcher" = "person_full_name", "MLBam ID" = "fielder_2") %>% 
  select(Season, Team, Catcher, `MLBam ID` ,`Total CFR`, `CFR/100`, N) %>% 
  filter(N > 700)


fr <- df %>% 
  group_by(Season, pitcher_team, fielder_2) %>% 
  summarise(`Total FR` = round(sum(FR, na.rm = T),2),
            N = n(), 
            .groups = "drop") %>% 
  mutate(`FR/100` = round((`Total FR` / N) * 100,3)) %>% 
  arrange(desc(`FR/100`)) %>% 
  inner_join(ids, by = c("fielder_2" = "person_id")) %>% 
  rename("Team" = "pitcher_team", "Catcher" = "person_full_name", "MLBam ID" = "fielder_2") %>% 
  select(Season, Team, Catcher, `MLBam ID` ,`Total FR`, `FR/100`, N) %>% 
  filter(N > 700)

df$called_strike_raw_pred <- ifelse(df$xcalled_strike >= 0.5, "Yes", "No")
df$called_strike_raw_pred <- as.factor(df$called_strike_raw_pred)
df$called_strike_raw_pred <- relevel(df$called_strike_raw_pred, ref = "Yes")
df$is_called_strike_raw <- ifelse(df$is_called_strike == 1, "Yes", "No")
df$is_called_strike_raw <- as.factor(df$is_called_strike_raw)
df$is_called_strike_raw <- relevel(df$is_called_strike_raw, ref = "Yes")

cM <- confusionMatrix(df$called_strike_raw_pred, df$is_called_strike_raw, positive = "Yes")
colnames(cM$table) <- c("Called Strike (True)","Called Ball (True)")
rownames(cM$table) <- c("Called Strike (Pred)","Called Ball (Pred)")
