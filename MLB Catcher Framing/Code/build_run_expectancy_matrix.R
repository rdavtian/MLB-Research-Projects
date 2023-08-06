library(stringr)
library(tidyverse)
require(caTools)
library(baseballr)
library(psych)
library(lubridate)
library(zoo)

current_season <- as.integer(substr(Sys.Date(), 1, 4)) - 1

run_expectancy_code2 <- function(df, level = "plate appearance")
{
  single_outs <- c("strikeout", "caught_stealing_2b", "pickoff_caught_stealing_2b",
                   "other_out", "caught_stealing_3b", "caught_stealing_home",
                   "field_out", "force_out", "pickoff_1b", "batter_interference",
                   "fielders_choice", "pickoff_2b", "pickoff_caught_stealing_3b",
                   "pickoff_caught_stealing_home")
  df <- df %>%
    dplyr::arrange(.data$game_pk, .data$at_bat_number, .data$pitch_number) %>%
    dplyr::group_by(.data$game_pk) %>%
    dplyr::mutate(
      final_pitch_game = ifelse(.data$pitch_number == max(.data$pitch_number), 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$game_pk, .data$at_bat_number, .data$inning_topbot) %>%
    dplyr::mutate(
      final_pitch_at_bat = ifelse(.data$pitch_number == max(.data$pitch_number), 1, 0)) %>%
    dplyr::ungroup()
  
  df <- df %>%
    dplyr::arrange(.data$game_pk, .data$inning_topbot, .data$at_bat_number, .data$pitch_number) %>%
    dplyr::mutate(
      runs_scored_on_pitch = stringr::str_count(.data$des, "scores"),
      runs_scored_on_pitch = ifelse(.data$events == "home_run", .data$runs_scored_on_pitch + 1, .data$runs_scored_on_pitch), 
      bat_score_after = .data$bat_score + .data$runs_scored_on_pitch) %>%
    dplyr::arrange(.data$game_pk, .data$at_bat_number,.data$ pitch_number) %>%
    dplyr::mutate(
      final_pitch_inning = ifelse(.data$final_pitch_at_bat == 1 & .data$inning_topbot != lead(.data$inning_topbot), 1, 0), 
      final_pitch_inning = ifelse(is.na(.data$final_pitch_inning), 1, .data$final_pitch_inning))
  
  if (level == "plate appearance") {
    
    df <- df %>%
      dplyr::group_by(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::mutate(
        bat_score_start_inning = min(.data$bat_score),
        bat_score_end_inning = max(.data$bat_score), 
        cum_runs_in_inning = cumsum(.data$runs_scored_on_pitch), 
        runs_to_end_inning = .data$bat_score_end_inning - .data$bat_score) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        base_out_state = paste(.data$outs_when_up, " outs, ", 
                               ifelse(!is.na(.data$on_1b), "1b", "_"), 
                               ifelse(!is.na(.data$on_2b),  "2b", "_"), 
                               ifelse(!is.na(.data$on_3b), "3b", "_")))
    
    re_table <- run_expectancy_table(df)
    
    df <- df %>% 
      dplyr::left_join(re_table, by = "base_out_state")
    df <- df %>% dplyr::filter(.data$final_pitch_at_bat == 1) %>%
      dplyr::arrange(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::group_by(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::mutate(next_base_out_state = dplyr::lead(.data$base_out_state)) %>%
      dplyr::ungroup() %>% 
      dplyr::left_join(re_table, by = c("next_base_out_state" = "base_out_state")) %>%
      dplyr::rename(
        next_avg_re = .data$avg_re.y, 
        avg_re = .data$avg_re.x) %>%
      dplyr::mutate(
        next_avg_re = ifelse(is.na(.data$next_avg_re), 0, .data$next_avg_re),
        change_re = .data$next_avg_re - .data$avg_re,
        re24 = .data$change_re + .data$runs_scored_on_pitch) %>%
      dplyr::arrange(.data$game_pk, .data$inning, .data$inning_topbot)
  }
  else {
    df <- df %>%
      dplyr::group_by(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::mutate(
        bat_score_start_inning = min(.data$bat_score),
        bat_score_end_inning = max(.data$bat_score),
        cum_runs_in_inning = cumsum(.data$runs_scored_on_pitch),
        runs_to_end_inning = .data$bat_score_end_inning - .data$bat_score) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        count_base_out_state = paste(.data$balls, "-", .data$strikes, ", ", .data$outs_when_up, " outs, ", 
                                     ifelse(!is.na(.data$on_1b), "1b", "_"), 
                                     ifelse(!is.na(.data$on_2b), "2b", "_"), 
                                     ifelse(!is.na(.data$on_3b), "3b", "_")))
    
    re_table <- run_expectancy_table(df, level = "pitch")
    
    df <- df %>% 
      dplyr::left_join(re_table, by = "count_base_out_state")
    
    df <- df %>% 
      dplyr::arrange(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::group_by(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::mutate(
        next_count_base_out_state = dplyr::lead(.data$count_base_out_state)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(re_table, by = c("next_count_base_out_state" = "count_base_out_state")) %>%
      dplyr::rename(
        next_avg_re = .data$avg_re.y, 
        avg_re = .data$avg_re.x) %>%
      dplyr::mutate(
        next_avg_re = ifelse(is.na(.data$next_avg_re),
                             0, .data$next_avg_re),
        change_re = .data$next_avg_re - .data$avg_re,
        runs_scored_on_pitch = ifelse(is.na(.data$runs_scored_on_pitch), 0, .data$runs_scored_on_pitch),
        re24 = .data$change_re + .data$runs_scored_on_pitch) %>%
      dplyr::arrange(.data$game_pk, .data$inning, .data$inning_topbot)
  }
  
  return(df)
}

run_expectancy_table <- function(df, level = "plate appearance") 
{
  
  if (level == "plate appearance") {
    
    df <- df %>%
      dplyr::filter(.data$final_pitch_at_bat == 1, .data$inning < 9) %>%
      dplyr::group_by(.data$base_out_state) %>%
      dplyr::summarise(avg_re = mean(.data$runs_to_end_inning, na.rm = TRUE)) %>%
      dplyr::arrange(desc(.data$avg_re))
  } else {
    df <- df %>%
      dplyr::filter(.data$inning < 9) %>%
      dplyr::group_by(.data$count_base_out_state) %>%
      dplyr::summarise(avg_re = mean(.data$runs_to_end_inning, na.rm = TRUE)) %>%
      dplyr::arrange(desc(.data$avg_re))
  }
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
    filter(pitch_name != "Intentional Ball", pitch_name != "Pitch Out", strikes < 3) %>%
    mutate(balls = case_when((balls > as.integer(3)) & (events != "walk") ~ as.integer(3),
                             TRUE ~ balls),
           plate_x = -plate_x, 
           woba_value = as.numeric(woba_value),
           woba_denom = as.numeric(woba_denom),
           babip_value = as.numeric(babip_value),
           iso_value = as.numeric(iso_value),
           bb_type = case_when(bb_type == "fly_ball" ~ "Fly Ball",
                               bb_type == "ground_ball" ~ "Ground Ball",
                               bb_type == "line_drive" ~ "Line Drive",
                               bb_type == "popup" ~ "Popup",
                               TRUE ~ bb_type)) %>%
    mutate(count = paste(balls, strikes, sep="-"),
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
           is_hit = case_when(events %in% c("single","double","triple","home_run") ~ 1,
                              TRUE ~ 0),
           is_hr = case_when(events == "home_run" ~ 1, TRUE ~ 0),
           is_swing = case_when(description %in% c("hit_into_play_no_out","hit_into_play",
                                                   "swinging_strike","foul","foul_tip",
                                                   "foul_bunt","hit_into_play_score",
                                                   "swinging_strike_blocked") ~ 1,
                                TRUE ~ 0),
           is_bip = case_when(description %in% c("hit_into_play_no_out","hit_into_play","hit_into_play_score") ~ 1,
                              TRUE ~ 0),
           is_contact = case_when(description %in% c("hit_into_play_no_out","hit_into_play",
                                                     "foul","foul_tip","foul_bunt",
                                                     "hit_into_play_score") ~ 1,
                                  TRUE ~ 0),
           is_whiff = case_when(description %in% c("swinging_strike_blocked","swinging_strike") ~ 1,
                                TRUE ~ 0),
           is_barrel = case_when(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124 ~ 1, TRUE ~ 0),
           in_zone = case_when((plate_z <= sz_top & plate_z >= sz_bot & abs(plate_x) <= 0.95) ~ 1, TRUE ~ 0),
           made_contact = case_when(description %in% c("hit_into_play","hit_into_play_no_out",
                                                       "hit_into_play_score","foul","foul_tip","foul_bunt") ~ 1,
                                    TRUE ~ 0),
           launch_speed_angle = case_when(launch_speed_angle == 1 ~ 'Weak',
                                          launch_speed_angle == 2 ~ 'Topped',
                                          launch_speed_angle == 3 ~ 'Under',
                                          launch_speed_angle == 4 ~ 'Flare',
                                          launch_speed_angle == 5 ~ 'Solid',
                                          launch_speed_angle == 6 ~ 'Barrel',
                                          TRUE ~ as.character(launch_speed_angle))) %>%
    mutate(pitch_name2 = case_when(pitch_name %in% c("4-Seam Fastball","2-Seam Fastball") ~ 'Fastball',
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
           is_out = case_when(events %in% c("Field Out","Strike Out","Sac Fly",
                                            "Sac Bunt", "Force Out") ~ 1, TRUE ~ 0),
           is_out = case_when(events %in% c("Double Play") ~ 2, TRUE ~ is_out),
           game_month = lubridate::month(game_date, label = TRUE),
           pfx_x = pfx_x * -12,
           pfx_z = pfx_z * 12,
           #home_team = case_when(home_team == "AZ" ~ "ARI", TRUE ~ home_team),
           #away_team = case_when(away_team == "AZ" ~ "ARI", TRUE ~ home_team),
           estimated_ba_using_speedangle = as.numeric(estimated_ba_using_speedangle),
           estimated_woba_using_speedangle = as.numeric(estimated_woba_using_speedangle),
           hard_hit = case_when(launch_speed >= 95 ~ 1,
                                launch_speed < 95 ~ 0,
                                TRUE ~ launch_speed),
           hard_hit = as.numeric(hard_hit)) %>%
    mutate(new_balls = balls + (type == "B"),
           new_strikes = pmin(2, strikes + (type == "S")),
           new_count = ifelse(is.na(events) == FALSE, events, paste(new_balls, new_strikes, sep="-")),
           new_count = as.factor(new_count),
           hc_x_ = 2.495671 * (hc_x - 125),
           hc_y_ = 2.495671 * (199 - hc_y),
           x = hc_x - 125.42, 
           y = 198.27 - hc_y,
           phi = (180 * atan(x/y)) / pi, # 100 * atan(x/y),
           spray_angle = case_when(stand == "RHB" ~ phi, TRUE ~ -phi),
           spray_angle_bin = case_when((phi <= -30) ~ "(-45,-30]",
                                       (phi > -30 & phi <= -15) ~ "(-30,-15]",
                                       (phi > -15 & phi <= 0) ~ "(-15,0]",
                                       (phi > 0 & phi <= 15) ~ "(0,15]",
                                       (phi > 15 & phi <= 30) ~ "(15,30]",
                                       (phi > 30) ~ "(30,45]"),
           spray_direction = case_when(spray_angle < -15 ~ "Pull",
                                       spray_angle > 15 ~ "Oppo",
                                       spray_angle >= -15 & spray_angle <= 15 ~ "Center",
                                       TRUE ~ NA_character_),
           zone_length = sz_top - sz_bot,
           attack_zone = case_when((abs(plate_x) <= 0.9 * (20/10)) &
                                     (plate_z <= (zone_length / 2)*(9/3) + sz_bot) &
                                     (plate_z >= sz_bot - (12/12)) ~ "Chase", TRUE ~ ""),
           attack_zone = case_when((abs(plate_x) <= 0.9 * (13.3/10)) &
                                     (plate_z <= (zone_length / 2)*(7/3) + sz_bot) &
                                     (plate_z >= sz_bot - (4/12)) ~ "Shadow", TRUE ~ attack_zone),
           attack_zone = case_when(in_zone == 1 ~ "Strike Zone", TRUE ~ attack_zone),
           attack_zone = case_when((abs(plate_x) <= 0.9 * (6.7/10)) &
                                     (plate_z <= (zone_length / 2)*(5/3) + sz_bot) &
                                     (plate_z >= (zone_length / 2)*(1/3) + sz_bot) ~ "Heart", TRUE ~ attack_zone),
           attack_zone = case_when((abs(plate_x) <= 0.9 * (3.3/10)) &
                                     (plate_z <= (zone_length / 2)*(4/3) + sz_bot) &
                                     (plate_z >= (zone_length / 2)*(2/3) + sz_bot) ~ "Meat", TRUE ~ attack_zone),
           attack_zone = case_when(attack_zone == "" ~ "Waste", TRUE ~ attack_zone),
           attack_zone = factor(attack_zone, levels=c("Meat","Heart","Strike Zone","Shadow","Chase","Waste"))) #%>%
  #mutate_at(vars(estimated_ba_using_speedangle, estimated_woba_using_speedangle, 
  #woba_value, woba_denom, babip_value, iso_value), ~replace(., is.na(.), 0))
  
  a = str_count(data2$des, "score")
  b = str_count(data2$des, "homer")
  c = str_count(data2$des, "grand slam")
  a[is.na(a)] = 0
  b[is.na(b)] = 0
  c[is.na(c)] = 0
  data2$runs_scored = a + b + c
  return(data2)
}

get_run_expectancy_values <- function(season)
{
  if (season == 2022)
  {
    mlbraw <- statcast_scraper(paste0(season, "-04-07"), paste0(season, "-10-05"))
    
  } else if (season != as.integer(substr(Sys.Date(), 1, 4))) {
    mlbraw <- statcast_scraper(paste0(season, "-04-01"), paste0(season, "-10-03"))
  }
  mlbraw <- clean_statcast_data_for_rv(mlbraw)
  
  mlbraw$des2 = NA
  mlbraw$des2[grepl("single", mlbraw$des)] = "single"
  mlbraw$des2[grepl("doubles", mlbraw$des)] = "double"
  mlbraw$des2[grepl("ground-rule double", mlbraw$des)] = "double"
  mlbraw$des2[grepl("triple", mlbraw$des)] = "triple"
  mlbraw$des2[grepl("homer", mlbraw$des)] = "home_run"
  mlbraw$des2[grepl("grand slam", mlbraw$des)] = "home_run"
  mlbraw$des2[grepl("home run", mlbraw$des)] = "home_run"
  mlbraw$des2[grepl("reaches on a throwing error", mlbraw$des)] = "field_error"
  mlbraw$des2[grepl("reaches on a fielding error", mlbraw$des)] = "field_error"
  mlbraw$des2[grepl("reaches on a missed catch error", mlbraw$des)] = "field_error"
  mlbraw$des2[grepl("hit by pitch", mlbraw$des)] = "hit_by_pitch"
  mlbraw$des2[grepl("walk", mlbraw$des)] = "walk"
  mlbraw$des2[grepl("strikes out", mlbraw$des)] = "strikeout"
  mlbraw$des2[grepl("on strikes", mlbraw$des)] = "strikeout"
  mlbraw$des2[grepl("sacrifice fly", mlbraw$des)] = "sac_fly"
  mlbraw$des2[grepl("fielder's choice", mlbraw$des)] = "fielders_choice"
  mlbraw$des2[grepl("force out", mlbraw$des)] = "fielders_choice"
  mlbraw$des2[grepl("double play", mlbraw$des)] = "double_play"
  mlbraw$des2[grepl("flies out", mlbraw$des) | 
                grepl("grounds out", mlbraw$des) |
                grepl("lines out", mlbraw$des) |
                grepl("pops out", mlbraw$des) |
                grepl("out on a sacrifice bunt", mlbraw$des)] = "field_out"
  
  des_subset = mlbraw[!is.na(mlbraw$des2),]
  
  des_subset$on_1b = ifelse(!is.na(des_subset$on_1b), 1, 0)
  des_subset$on_2b = ifelse(!is.na(des_subset$on_2b), 1, 0)
  des_subset$on_3b = ifelse(!is.na(des_subset$on_3b), 1, 0)
  
  a = str_count(des_subset$des, "score")
  b = str_count(des_subset$des, "homer")
  c = str_count(des_subset$des, "grand slam")
  a[is.na(a)] = 0
  b[is.na(b)] = 0
  c[is.na(c)] = 0
  
  des_subset$runs_scored = a + b + c
  
  des_subset$date1 = as.Date(des_subset$game_date)
  des_subset = des_subset%>%
    arrange(date1, home_team, away_team, inning, desc(inning_topbot), at_bat_number, pitch_number)
  
  mlbraw2 <- run_expectancy_code2(mlbraw, level = "pitch level")
  re_table <- run_expectancy_table(mlbraw2, level = "pitch level")
  
  Season_RE = data.frame(matrix(ncol = 7, nrow = nrow(re_table)))
  names(Season_RE) = c("balls", "strikes", "outs_when_up", "on_1b", "on_2b", "on_3b", "RE")
  
  for (i in 1:nrow(re_table))
  {
    Season_RE$outs_when_up[i] = str_split(
      re_table$count_base_out_state, " ")[[i]][[6]]
    
    Season_RE$balls[i] = str_split(
      re_table$count_base_out_state, " ")[[i]][[1]]
    
    Season_RE$strikes[i] = str_split(
      re_table$count_base_out_state, " ")[[i]][[3]]
    
    Season_RE$on_1b[i] = ifelse(str_split(
      re_table$count_base_out_state, " ")[[i]][[10]] == "1b", 1, 0)
    
    Season_RE$on_2b[i] = ifelse(str_split(
      re_table$count_base_out_state, " ")[[i]][[11]] == "2b", 1, 0)
    
    Season_RE$on_3b[i] = ifelse(str_split(
      re_table$count_base_out_state, " ")[[i]][[12]] == "3b", 1, 0)
  }
  
  Season_RE$RE = re_table$avg_re
  Season_RE$outs_when_up = as.integer(Season_RE$outs_when_up)
  Season_RE$balls = as.integer(Season_RE$balls)
  Season_RE$strikes = as.integer(Season_RE$strikes)
  return(Season_RE)
}

#####################################################################################
run_expectancy_values <- get_run_expectancy_values(current_season)
write.csv(run_expectancy_values, file = paste0("C:/Users/rusla/OneDrive/MLBAnalyticsJobs/MLB Catcher Framing/Data/run_expectancy_values_", current_season, ".csv"), row.names = FALSE)
