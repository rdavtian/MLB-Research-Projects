library(stringr)
library(tidyverse)
require(caTools)
library(baseballr)
library(ggplot2)
library(psych)
library(lubridate)
library(zoo)
library(caret)

teamnames <- baseballr::teams_lu_table %>% 
  filter(sport.name == "Major League Baseball") %>% 
  select(teamName, abbreviation) %>% distinct() %>% 
  mutate(teamName = case_when(teamName == "D-backs" ~ "Dbacks",
                              TRUE ~ teamName)) %>% 
  rename("Team" = "abbreviation")

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
    filter(pitch_name != "Intentional Ball", pitch_name != "Pitch Out") %>%
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
                                   pitch_name %in% c("Curveball","Knuckle Curve") ~ 'Curveball',
                                   pitch_name == 'Slider' ~ 'Slider',
                                   pitch_name == 'Cutter' ~ 'Cutter',
                                   pitch_name == 'Sinker' ~ 'Sinker',
                                   pitch_name == 'Split-Finger' ~ 'Split-Finger',
                                   pitch_name == 'Changeup' ~ 'Changeup',
                                   TRUE ~ pitch_name),
           pitch_type2 = case_when(pitch_type %in% c("FF","FT") ~ 'FB',
                                   pitch_type %in% c("CU","KC") ~ 'CU',
                                   pitch_type == 'FS' ~ 'FS',
                                   pitch_type == 'SL' ~ 'SL',
                                   pitch_type == 'CU' ~ 'CU',
                                   pitch_type == 'SI' ~ 'SI',
                                   pitch_type == 'CH' ~ 'CH',
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
  if (season == as.integer(substr(Sys.Date(), 1, 4)))
  {
    mlbraw <- statcast_scraper(paste0(season, "-04-07"), Sys.Date())
    
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

clean_statcast_data_for_pitch_mod <- function(clean_data)
{
  clean_data2 <- clean_data %>% 
    filter(!pitch_name2 %in% c("","Screwball","Eephus","Knuckleball")) %>% 
    filter(!is.na(pfx_z), !is.na(pfx_x), !is.na(plate_x), !is.na(plate_z), !is.na(RV),
           !is.na(release_speed), !is.na(release_pos_x), !is.na(release_pos_z),
           !is.na(release_pos_y), !is.na(release_spin_rate), !is.na(release_extension)) %>%
    mutate(pitch_name3 = case_when(pitch_name2 %in% c("Fastball","Sinker") ~ "Fastball",
                                   pitch_name2 %in% c("Cutter","Slider","Curveball") ~ "Breaking Ball",
                                   pitch_name2 %in% c("Changeup","Split-Finger") ~ "Offspeed")) %>% 
    select(where(~!all(is.na(.x)))) %>% 
    mutate(events3 = case_when(events %in% c("Field Out","Force Out","Sac Fly","Sac Bunt") ~ "Out",
                               TRUE ~ events),
           events3 = case_when(description %in% c("ball","blocked_ball","pitchout") ~ "Ball",
                               description %in% c("called_strike","bunt_foul_tip","foul","foul_bunt",
                                                  "foul_tip","missed_bunt","swinging_strike",
                                                  "swinging_strike_blocked") ~ "Strike",
                               TRUE ~ events3),
           events3 = case_when(new_count == "Walk" ~ "Walk",
                               new_count == "Strike Out" ~ "Strike Out",
                               TRUE ~ events3)) %>% 
    filter(!is.na(events3)) %>% 
    inner_join(teamnames, by = c("home_team" = "Team")) %>% rename("home_team_name" = "teamName") %>% 
    inner_join(teamnames, by = c("away_team" = "Team")) %>% rename("away_team_name" = "teamName") %>%
    mutate(pitcher_team = case_when(inning_topbot == "Top" ~ home_team_name, 
                                    TRUE ~ away_team_name),
           batter_team = case_when(inning_topbot == "Bot" ~ home_team_name,
                                   TRUE ~ away_team_name))
  
  clean_data2 <- clean_data2 %>%
    group_by(pitcher, game_date) %>% 
    mutate(avg_fb_velo = mean(release_speed[pitch_name3 == "Fastball"], na.rm = T),
           avg_fb_pfx_x = mean(pfx_x[pitch_name3 == "Fastball"], na.rm = T),
           avg_fb_pfx_z = mean(pfx_z[pitch_name3 == "Fastball"], na.rm = T)) %>% 
    mutate(release_speed_diff = release_speed - avg_fb_velo, 
           pfx_x_diff = pfx_x - avg_fb_pfx_x,
           pfx_z_diff = pfx_z - avg_fb_pfx_z) %>% ungroup() %>% 
    filter(!is.na(release_speed_diff), !is.na(pfx_x_diff), !is.na(pfx_z_diff))
  
  return(clean_data2)
}

train_test_split <- function(data, nrows, train_split)
{
  set.seed(0)
  data <- sample(data)[1:nrows,]
  index <- sort(sample(nrow(data), nrow(data)*train_split))
  train <- data[index,]
  test <- data[-index,]
  return(list(train, test))
}

run_model <- function(data, x_vars, y_var, model_type, tuneLength, plot = TRUE)
{
  set.seed(0)
  train_test <- train_test_split(data, nrow(data), 0.7)
  train <- train_test[[1]]
  test <- train_test[[2]]
  myFolds <- createFolds(train[, y_var] %>% select(all_of(y_var)) %>% pull(), k = 5)
  control <- trainControl(number = 5,index = myFolds, savePredictions = TRUE)
  
  if (model_type %in% c("glmnet"))
  {
    #grid <- expand.grid(.alpha = seq(0, 1, 0.1), .lambda = seq(0, 1, 0.1))
    set.seed(0)
    model <- train(as.formula(paste0(y_var, " ~ ", paste0(x_vars, collapse = " + "))), 
                   data = train, method = model_type, tuneLength = tuneLength,
                   trControl=control, metric = 'MAE', preProcess = c('center','scale'))
  } else if (model_type == "gbm") {
    set.seed(0)
    model <- train(as.formula(paste0(y_var, " ~ ", paste0(x_vars, collapse = " + "))),  
                   data = train, method = model_type, tuneLength = tuneLength, verbose = F,
                   trControl=control, metric = 'MAE')
  } else if (model_type == "ranger") {
    set.seed(0)
    model <- train(as.formula(paste0(y_var, " ~ ", paste0(x_vars, collapse = " + "))),  
                   data = train, method = model_type, tuneLength = tuneLength,
                   trControl=control, metric = 'MAE', importance = "impurity")
  } else {
    set.seed(0)
    model <- train(as.formula(paste0(y_var, " ~ ", paste0(x_vars, collapse = " + "))), 
                   data = train, method = model_type, tuneLength = tuneLength,
                   trControl=control, metric = 'MAE')
  }
  if ((model_type != 'gbm') & (plot == TRUE))
  {
    if (model_type == "glmnet")
    {
      model_type2 <- "Reg. Regression"
    } else if (model_type %in%  c("rf","ranger")) {
      model_type2 <- "Random Forest"
    } else if (model_type == "xgbTree") {
      model_type2 <- "XGBoost"
    }
    imp2 <- varImp(model)
    imp_plot <- barchart(sort(rowMeans(imp2$importance), decreasing = F), main = paste0(model_type2, " Variable Importance Chart"), xlab = "Average Level of Importance", ylab = "Variables", fill="cyan2")
  }
  else if ((model_type == 'gbm') & (plot == TRUE))
  {
    var_imp <- summary(model)[2]
    labels <- row.names(var_imp)
    var_imp <- var_imp[1:length(model$finalModel$xNames),]
    labels <- labels[1:length(model$finalModel$xNames)]
    df <- data.frame(labels, var_imp)
    imp_plot <- ggplot(df, aes(x = reorder(labels, var_imp), y = var_imp)) +
      geom_bar(stat = "identity", fill = "cyan2") +
      ggtitle(paste0("GBM Variable Importance Chart")) + 
      coord_flip() + scale_y_continuous(name="Variable Important (0-100)") +
      scale_x_discrete(name="") +
      geom_label(aes(label = round(var_imp, 1)), size = 5, alpha = 0.7) +
      geom_label(aes(label = round(var_imp, 1)), size = 5.1, alpha = 0.7) +
      theme(plot.title=element_text(hjust=0.5,vjust=0,size=17,face = 'bold'),
            plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 9)) +
      theme(axis.text.x=element_text(vjust = .5, size=13,colour="#535353",face="bold")) +
      theme(axis.text.y=element_text(size=13,colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0)) +
      theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
      theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) +
      theme(panel.background = element_rect(fill = "white"),
            legend.title = element_text(face = "bold", size = 15),
            legend.text = element_text(face = "bold", size = 12),
            legend.key.height= unit(0.7, 'cm'),
            legend.key.width= unit(0.7, 'cm')) +
      theme(strip.text = element_text(face="bold", size=13),
            strip.background = element_rect(fill="lightblue", colour="black",size=1))
  }
  if (model_type == "glmnet")
  {
    test2 <- test %>%
      select(all_of(x_vars)) %>% 
      mutate(across(where(is.numeric), scale, center = TRUE)) %>% 
      mutate_at(all_of(x_vars), as.numeric)
  } else {
    test2 <- test
  }
  test$y_var_pred <- predict(model, newdata = test2)
  rmse <- round(sqrt(mean((test[,y_var] %>% pull() - test$y_var_pred)^2)),2)
  print(paste0(model_type," Test Set RMSE: ", rmse))
  if (plot == TRUE)
  {
    return(list(model, test, imp_plot))
  } else {
    return(list(model, test))
  }
}

pitch_quality_leaders <- function(data, pitch_type, min_pitches = 100)
{
  if (pitch_type != "all")
  {
    data2 <- data %>% 
      filter(pitch_name3 == !!pitch_type) %>% 
      group_by(pitcher, person_full_name, pitch_name2) %>% 
      summarize(`Total RV` = round(sum(RV, na.rm = T),2),
                `RV/100` = round(mean(RV, na.rm = T) * 100,2),
                `Total xRV` = round(sum(xRV, na.rm = T),2),
                `xRV/100` = round(mean(xRV, na.rm = T) * 100,2),
                `Num Pitches` = n(), 
                Team = max(pitcher_team), .groups = "drop") %>% 
      filter(`Num Pitches` >= min_pitches) %>% 
      arrange(`xRV/100`) %>% 
      mutate(`xRV/100+` = (`xRV/100` - min(`xRV/100`, na.rm = T)) / (min(`xRV/100`, na.rm = T) - max(`xRV/100`, na.rm = T)),
             `Pitch Quality Score` = round((`xRV/100+` * 100) + 100),
             `Pitch Quality Percentile` = rank(`Pitch Quality Score`)/length(`Pitch Quality Score`),
             `Pitch Quality Percentile` = round(`Pitch Quality Percentile` * 100),
             Season = unique(data$game_year)) %>% 
      select(Season,Team,person_full_name,pitch_name2,`Total RV`,`RV/100`,`Total xRV`,`xRV/100`,
             `Num Pitches`, `Pitch Quality Score`, `Pitch Quality Percentile`) %>% 
      rename("Pitcher" = "person_full_name","Pitch Type" = "pitch_name2")
  } else {
    data2 <- data %>%
      group_by(pitcher, person_full_name) %>% 
      summarize(`Total RV` = round(sum(RV, na.rm = T),2),
                `RV/100` = round(mean(RV, na.rm = T) * 100,2),
                `Total xRV` = round(sum(xRV, na.rm = T),2),
                `xRV/100` = round(mean(xRV, na.rm = T) * 100,2),
                `Num Pitches` = n(), 
                Team = max(pitcher_team), .groups = "drop") %>% 
      filter(`Num Pitches` >= min_pitches) %>% 
      arrange(`xRV/100`) %>% 
      mutate(`xRV/100+` = (`xRV/100` - min(`xRV/100`, na.rm = T)) / (min(`xRV/100`, na.rm = T) - max(`xRV/100`, na.rm = T)),
             `Pitcher Quality Score` = round((`xRV/100+` * 100) + 100),
             `Pitcher Quality Percentile` = rank(`Pitcher Quality Score`)/length(`Pitcher Quality Score`),
             `Pitcher Quality Percentile` = round(`Pitcher Quality Percentile` * 100),
             Season = unique(data$game_year)) %>% 
      select(Season,Team,person_full_name,`Total RV`,`RV/100`,`Total xRV`,`xRV/100`,
             `Num Pitches`, `Pitcher Quality Score`, `Pitcher Quality Percentile`) %>% 
      rename("Pitcher" = "person_full_name")
  }
  return(data2)
}

team_pitch_quality_leaders <- function(data, pitch_type)
{
  if (pitch_type != "all")
  {
    data2 <- data %>%
      filter(pitch_name3 == !!pitch_type) %>% 
      group_by(pitcher_team) %>% 
      summarize(`Total RV` = round(sum(RV, na.rm = T),2),
                `RV/100` = round(mean(RV, na.rm = T) * 100,2),
                `Total xRV` = round(sum(xRV, na.rm = T),2),
                `xRV/100` = round(mean(xRV, na.rm = T) * 100,2),
                `Num Pitches` = n(), 
                Team = max(pitcher_team), .groups = "drop") %>% 
      arrange(`xRV/100`) %>% 
      mutate(`xRV/100+` = (`xRV/100` - min(`xRV/100`, na.rm = T)) / (min(`xRV/100`, na.rm = T) - max(`xRV/100`, na.rm = T)),
             `Team Pitch Quality Score` = round((`xRV/100+` * 100) + 100),
             `Team Pitch Quality Percentile` = rank(`Team Pitch Quality Score`)/length(`Team Pitch Quality Score`),
             `Team Pitch Quality Percentile` = round(`Team Pitch Quality Percentile` * 100),
             Season = unique(data$game_year)) %>% 
      select(Season,Team,`Total RV`,`RV/100`,`Total xRV`,`xRV/100`,
             `Team Pitch Quality Score`, `Team Pitch Quality Percentile`)
  } else {
    data2 <- data %>%
      group_by(pitcher_team) %>% 
      summarize(`Total RV` = round(sum(RV, na.rm = T),2),
                `RV/100` = round(mean(RV, na.rm = T) * 100,2),
                `Total xRV` = round(sum(xRV, na.rm = T),2),
                `xRV/100` = round(mean(xRV, na.rm = T) * 100,2),
                `Num Pitches` = n(), 
                Team = max(pitcher_team), .groups = "drop") %>% 
      arrange(`xRV/100`) %>% 
      mutate(`xRV/100+` = (`xRV/100` - min(`xRV/100`, na.rm = T)) / (min(`xRV/100`, na.rm = T) - max(`xRV/100`, na.rm = T)),
             `Team Pitch Quality Score` = round((`xRV/100+` * 100) + 100),
             `Team Pitch Quality Percentile` = rank(`Team Pitch Quality Score`)/length(`Team Pitch Quality Score`),
             `Team Pitch Quality Percentile` = round(`Team Pitch Quality Percentile` * 100),
             Season = unique(data$game_year)) %>% 
      select(Season,Team,`Total RV`,`RV/100`,`Total xRV`,`xRV/100`,
             `Team Pitch Quality Score`, `Team Pitch Quality Percentile`)
  }
  return(data2)
}

pitch_quality_individual_pitches <- function(data, pitcher_name)
{
  df <- data %>% 
    mutate(game_date = as.Date(game_date),
           player_name = sub("(^.*),\\s(.*$)","\\2 \\1", player_name)) %>% 
    group_by(pitch_name3) %>% 
    mutate(`xRV Norm` = (xRV - min(xRV, na.rm = T)) / (min(xRV, na.rm = T) - max(xRV, na.rm = T)),
           `Pitch Quality Score` = round((`xRV Norm` * 100) + 100),
           `Pitch Quality Percentile` = rank(`Pitch Quality Score`)/length(`Pitch Quality Score`),
           `Pitch Quality Percentile` = round(`Pitch Quality Percentile` * 100)) %>% ungroup() %>%
    mutate(game_date = format(game_date, "%m/%d/%Y"), Inning = paste0(inning_topbot, " ", inning)) %>% 
    select(game_year, game_date, game_pk, at_bat_number, pitch_number, pitcher_team, batter_team, 
           person_full_name, player_name, Inning, outs_when_up, count, events, description, 
           runs_scored, pitch_name2, release_speed, pfx_x, pfx_z, release_spin_rate, attack_zone, 
           bb_type, launch_speed, launch_angle, hit_distance_sc, is_barrel, 
           estimated_ba_using_speedangle, estimated_woba_using_speedangle, lin_weight, 
           wOBA_weight, woba_value, playRE, RV, xRV, `xRV Norm`, `Pitch Quality Score`, 
           `Pitch Quality Percentile`) %>% 
    filter(person_full_name == pitcher_name) %>% 
    rename("Year" = "game_year", "Date" = "game_date", "Pitcher Team" = "pitcher_team", 
           "Batter Team" = "batter_team", "Pitcher" = "person_full_name", "Runs Scored" = "runs_scored",
           "Batter" = "player_name", "Outs" = "outs_when_up", "Count" = "count", "Event" = "events",
           "Pitch Type" = "pitch_name2", "Batted Ball Type" = "bb_type", "wOBA Weight" = "wOBA_weight",
           "Pitch Speed" = "release_speed", "Exit Velo" = "launch_speed", "Zone" = "attack_zone",
           "Launch Angle" = "launch_angle", "Hit Distance" = "hit_distance_sc", "Linear Weight" = "lin_weight",
           "xBA" = "estimated_ba_using_speedangle", "xwOBA" = "estimated_woba_using_speedangle", 
           "Horz. Mov." = "pfx_x", "Vert. Mov." = "pfx_z", "Spin Rate" = "release_spin_rate",
           "Description" = "description", "Is Barrel" = "is_barrel", "wOBA Value" = "woba_value") %>% 
    mutate(`xRV Norm` = `xRV Norm` + 1)
  return(df %>% arrange(-`Pitch Quality Score`))
}

get_video_links <- function(data, num_games)
{
  games <- data %>% select(game_pk, Date) %>% 
    mutate(game_date = as.Date(Date, "%m/%d/%Y")) %>% distinct()
  
  df <- data.frame()
  for (i in 1:length(games$game_date))
  {
    game_id <- mlb_game_pks(games$game_date[i]) %>% 
      filter(game_pk %in% games$game_pk) %>% select(game_pk) %>% pull()
    
    pbp <- mlb_pbp(game_id) %>% 
      select(game_pk,at_bat_number = atBatIndex, pitch_number = pitchNumber,play_id = playId) %>%
      mutate(at_bat_number = as.double(at_bat_number),
             at_bat_number = at_bat_number + 1,
             pitch_number = as.double(pitch_number))
    
    both <- left_join(data, pbp, by = c('game_pk','at_bat_number','pitch_number')) %>%
      mutate(video_url = ifelse(!is.na(play_id),paste0('https://baseballsavant.mlb.com/sporty-videos?playId=',play_id),play_id)) %>% 
      filter(!is.na(video_url)) %>% rename("Video Link" = "video_url")
    
    df <- bind_rows(both, df)
    if (i == num_games)
    {
      break
    }
  }
  return(df %>% select(-game_pk, -at_bat_number, -pitch_number, -play_id) %>% 
           arrange(-`Pitch Quality Score`))
}

get_sds_weights <- function(data)
{
  weights <- data %>%
    group_by(attack_zone, is_swing, two_strikes) %>% 
    summarize(`RV/100` = mean(RV, na.rm = T) * 100, 
              `xRV/100` = mean(xRV, na.rm = T) * 100, .groups = "drop")
  data2 <- data %>% 
    inner_join(weights, by = c("attack_zone","is_swing","two_strikes"))
  return(data2)
}

sds_individual_leaders <- function(data, min_pitches)
{
  if (unique(data$game_year) == 2021)
  {
    avg_pitchPA <- 3.908334
  } else if (unique(data$game_year) == 2022) {
    avg_pitchPA <- 3.895207
  }
  #avg_pitchPA <- data %>% 
  #arrange(pitcher, game_date, home_team, away_team, inning, desc(inning_topbot), 
  #at_bat_number, pitch_number) %>% 
  #group_by(pitcher, game_date, inning, inning_topbot, at_bat_number) %>% 
  #summarize(`Num Pitches` = max(pitch_number), .groups = "drop") %>% ungroup() %>% 
  #select(`Num Pitches`) %>% pull() %>% mean()
  
  wOBA_scale <- fg_guts() %>% 
    filter(season == unique(data$game_year)) %>% 
    select(woba_scale) %>% pull() 
  
  data2 <- data %>% 
    group_by(player_name) %>%
    mutate(`Num Pitches` = n()) %>% 
    filter(`Num Pitches` >= min_pitches) %>% 
    summarise(`RV/100` = round(mean(`RV/100`),3),
              `xRV/100` = round(mean(`xRV/100`),3),
              `wOBA/Swing Decision` = round(`RV/100` / 100 * avg_pitchPA * wOBA_scale,4),
              `Num Pitches` = n(),
              Team = max(batter_team)) %>%
    mutate(`SDS Score` = (`xRV/100` - min(`xRV/100`, na.rm = T)) / (max(`xRV/100`, na.rm = T) - min(`xRV/100`, na.rm = T)),
           `SDS Score` = round((`SDS Score` * 100)),
           `SDS Percentile` = rank(`SDS Score`)/length(`SDS Score`),
           `SDS Percentile` = round(`SDS Percentile` * 100),
           Season = unique(data$game_year)) %>% rename("Batter" = "player_name") %>% 
    select(Season, Team, Batter, `RV/100`, `xRV/100`, `wOBA/Swing Decision`, `SDS Score`, 
           `SDS Percentile`, `Num Pitches`) %>% arrange(-`xRV/100`, -`SDS Percentile`) %>% 
    mutate(Batter = sub("(^.*),\\s(.*$)","\\2 \\1", Batter))
  return(data2)
} 

sds_team_leaders <- function(data)
{
  if (unique(data$game_year) == 2021)
  {
    avg_pitchPA <- 3.908334
  } else if (unique(data$game_year) == 2022) {
    avg_pitchPA <- 3.895207
  }
  #avg_pitchPA <- data %>% 
  #arrange(pitcher, game_date, home_team, away_team, inning, desc(inning_topbot), 
  #at_bat_number, pitch_number) %>% 
  #group_by(pitcher, game_date, inning, inning_topbot, at_bat_number) %>% 
  #summarize(`Num Pitches` = max(pitch_number), .groups = "drop") %>% ungroup() %>% 
  #select(`Num Pitches`) %>% pull() %>% mean()
  
  wOBA_scale <- fg_guts() %>% 
    filter(season == unique(data$game_year)) %>% 
    select(woba_scale) %>% pull() 
  
  data2 <- data %>% 
    group_by(batter_team) %>%
    summarise(`RV/100` = round(mean(`RV/100`),3),
              `xRV/100` = round(mean(`xRV/100`),3),
              `wOBA/Swing Decision` = round(`RV/100` / 100 * avg_pitchPA * wOBA_scale,4)) %>%
    mutate(`SDS Score` = (`xRV/100` - min(`xRV/100`, na.rm = T)) / (max(`xRV/100`, na.rm = T) - min(`xRV/100`, na.rm = T)),
           `SDS Score` = round((`SDS Score` * 100)),
           `SDS Percentile` = rank(`SDS Score`)/length(`SDS Score`),
           `SDS Percentile` = round(`SDS Percentile` * 100),
           Season = unique(data$game_year)) %>% rename("Team" = "batter_team") %>% 
    select(Season, Team, `RV/100`, `xRV/100`, `wOBA/Swing Decision`, `SDS Score`, `SDS Percentile`) %>% 
    arrange(-`xRV/100`, -`SDS Percentile`)
  return(data2)
} 
#########################################################################################
sc <- statcast_scraper("2022-04-07", Sys.Date())
sc_clean <- clean_statcast_data_for_rv(sc)
run_expectancy_values <- get_run_expectancy_values(2022)
sc_clean2 <- find_run_value(sc_clean, run_expectancy_values)
df <- clean_statcast_data_for_pitch_mod(sc_clean2)
lw <- df %>% 
  filter(!is.na(events3)) %>% 
  group_by(events3) %>% 
  summarize(lin_weight = mean(RV, na.rm = T)) %>% 
  mutate(wOBA_weight = (lin_weight + abs(lin_weight[events3 == "Out"])) * fg_guts() %>% filter(season == 2022) %>% select(woba_scale) %>% pull())
df <- df %>% inner_join(lw, by = "events3")

fastball <- df %>% filter(pitch_name3 == "Fastball")
offspeed <- df %>% filter(pitch_name3 == "Offspeed")
breaking <- df %>% filter(pitch_name3 == "Breaking Ball")
fastball2 <- fastball %>% filter(game_date < "2022-07-01")
breaking2 <- breaking %>% filter(game_date < "2022-07-01")
offspeed2 <- offspeed %>% filter(game_date < "2022-07-01")
ids <- player_name_player_id(2022)

x_vars <- c("release_speed","pfx_x","pfx_z","release_pos_x","release_pos_z", 
            "release_extension","release_spin_rate","spin_axis","plate_x","plate_z",
            "release_speed_diff","pfx_x_diff","pfx_z_diff")
x_vars_fb <- c("release_speed","pfx_x","pfx_z","release_pos_x","release_pos_z", 
               "release_extension","release_spin_rate","spin_axis","plate_x","plate_z")
y_var <- "lin_weight"
##########################################################################################
# Fastballs
fb <- run_model(fastball2, x_vars_fb, y_var, "ranger", 2, TRUE)
fb[[3]]
fastball$xRV <- predict(fb[[1]], fastball)

# Plot xRV kernel density estimate of test set predictions
iqr <- quantile(fb[[2]]$y_var_pred, prob = 0.75, na.rm = T) - quantile(fb[[2]]$y_var_pred, prob = 0.25, na.rm = T)
h = 0.9*min(sd(fb[[2]]$y_var_pred, na.rm = T), (iqr / 1.34)) * nrow(fb[[2]])^(-0.2)
ggplot(data = fb[[2]], aes(x = y_var_pred)) + 
  geom_density(stat = "density", alpha = 1, kernel = 'gaussian', fill = "cyan2", bw = h) + 
  labs(x="xRV", y="Density",
       title="Kernel Density Estimate of xRV") + 
  theme(plot.title=element_text(hjust=0.5,vjust=0,size=20,face = 'bold'), 
        plot.subtitle=element_text(face="bold", hjust= 0.5, vjust= .09, colour="#3C3C3C", size = 15)) +
  theme(axis.text.x=element_text(vjust = .5, size=17,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=17,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=17,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=17,colour="#535353",face="bold",vjust=0)) +
  theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
  theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) + 
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) 

# Plot actual RV from test data set
y_var2 <- rlang::sym(quo_name(enquo(y_var)))
iqr <- quantile(fb[[2]][, y_var] %>% pull(), prob = 0.75, na.rm = T) - quantile(fb[[2]][, y_var] %>% pull(), prob = 0.25, na.rm = T)
h = 0.9*min(sd(fb[[2]][, y_var] %>% pull(), na.rm = T), (iqr / 1.34)) * nrow(fb[[2]])^(-0.2)
ggplot(data = fb[[2]], aes(x = !! y_var2)) + 
  geom_density(stat = "density", alpha = 1, kernel = 'gaussian', fill = "cyan2", bw = h) + 
  labs(x="RV", y="Density",
       title="Kernel Density Estimate of RV") + 
  theme(plot.title=element_text(hjust=0.5,vjust=0,size=20,face = 'bold'), 
        plot.subtitle=element_text(face="bold", hjust= 0.5, vjust= .09, colour="#3C3C3C", size = 15)) +
  theme(axis.text.x=element_text(vjust = .5, size=17,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=17,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=17,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=17,colour="#535353",face="bold",vjust=0)) +
  theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
  theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) + 
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) 

###########################################################################################
# Breaking Balls
br <- run_model(breaking2, x_vars, y_var, "ranger", 2, TRUE)
br[[3]]
breaking$xRV <- predict(br[[1]], breaking)

# Plot xRV kernel density estimate of test set predictions
iqr <- quantile(br[[2]]$y_var_pred, prob = 0.75, na.rm = T) - quantile(br[[2]]$y_var_pred, prob = 0.25, na.rm = T)
h = 0.9*min(sd(br[[2]]$y_var_pred, na.rm = T), (iqr / 1.34)) * nrow(br[[2]])^(-0.2)
ggplot(data = br[[2]], aes(x = y_var_pred)) + 
  geom_density(stat = "density", alpha = 1, kernel = 'gaussian', fill = "cyan2", bw = h) + 
  labs(x="xRV", y="Density",
       title="Kernel Density Estimate of xRV") + 
  theme(plot.title=element_text(hjust=0.5,vjust=0,size=20,face = 'bold'), 
        plot.subtitle=element_text(face="bold", hjust= 0.5, vjust= .09, colour="#3C3C3C", size = 15)) +
  theme(axis.text.x=element_text(vjust = .5, size=17,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=17,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=17,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=17,colour="#535353",face="bold",vjust=0)) +
  theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
  theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) + 
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) 

# Plot actual RV from test data set
y_var2 <- rlang::sym(quo_name(enquo(y_var)))
iqr <- quantile(br[[2]][, y_var] %>% pull(), prob = 0.75, na.rm = T) - quantile(br[[2]][, y_var] %>% pull(), prob = 0.25, na.rm = T)
h = 0.9*min(sd(br[[2]][, y_var] %>% pull(), na.rm = T), (iqr / 1.34)) * nrow(br[[2]])^(-0.2)
ggplot(data = br[[2]], aes(x = !! y_var2)) + 
  geom_density(stat = "density", alpha = 1, kernel = 'gaussian', fill = "cyan2", bw = h) + 
  labs(x="RV", y="Density",
       title="Kernel Density Estimate of RV") + 
  theme(plot.title=element_text(hjust=0.5,vjust=0,size=20,face = 'bold'), 
        plot.subtitle=element_text(face="bold", hjust= 0.5, vjust= .09, colour="#3C3C3C", size = 15)) +
  theme(axis.text.x=element_text(vjust = .5, size=17,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=17,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=17,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=17,colour="#535353",face="bold",vjust=0)) +
  theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
  theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) + 
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) 
###########################################################################################
# Offspeed
off <- run_model(offspeed2, x_vars, y_var, "ranger", 2, TRUE)
off[[3]]
offspeed$xRV <- predict(off[[1]], offspeed)

# Plot xRV kernel density estimate of test set predictions
iqr <- quantile(off[[2]]$y_var_pred, prob = 0.75, na.rm = T) - quantile(off[[2]]$y_var_pred, prob = 0.25, na.rm = T)
h = 0.9*min(sd(off[[2]]$y_var_pred, na.rm = T), (iqr / 1.34)) * nrow(off[[2]])^(-0.2)
ggplot(data = off[[2]], aes(x = y_var_pred)) + 
  geom_density(stat = "density", alpha = 1, kernel = 'gaussian', fill = "cyan2", bw = h) + 
  labs(x="xRV", y="Density",
       title="Kernel Density Estimate of xRV") + 
  theme(plot.title=element_text(hjust=0.5,vjust=0,size=20,face = 'bold'), 
        plot.subtitle=element_text(face="bold", hjust= 0.5, vjust= .09, colour="#3C3C3C", size = 15)) +
  theme(axis.text.x=element_text(vjust = .5, size=17,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=17,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=17,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=17,colour="#535353",face="bold",vjust=0)) +
  theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
  theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) + 
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) 

# Plot actual RV from test data set
y_var2 <- rlang::sym(quo_name(enquo(y_var)))
iqr <- quantile(off[[2]][, y_var] %>% pull(), prob = 0.75, na.rm = T) - quantile(off[[2]][, y_var] %>% pull(), prob = 0.25, na.rm = T)
h = 0.9*min(sd(off[[2]][, y_var] %>% pull(), na.rm = T), (iqr / 1.34)) * nrow(off[[2]])^(-0.2)
ggplot(data = off[[2]], aes(x = !! y_var2)) + 
  geom_density(stat = "density", alpha = 1, kernel = 'gaussian', fill = "cyan2", bw = h) + 
  labs(x="RV", y="Density",
       title="Kernel Density Estimate of RV") + 
  theme(plot.title=element_text(hjust=0.5,vjust=0,size=20,face = 'bold'), 
        plot.subtitle=element_text(face="bold", hjust= 0.5, vjust= .09, colour="#3C3C3C", size = 15)) +
  theme(axis.text.x=element_text(vjust = .5, size=17,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=17,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=17,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=17,colour="#535353",face="bold",vjust=0)) +
  theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
  theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) + 
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) 
####################################################################################
full_preds <- rbind(fastball, breaking, offspeed) %>% inner_join(ids, by = c("pitcher" = "person_id"))
full_preds <- get_sds_weights(full_preds)
write.csv(full_preds, "C:\\Users\\rusla\\OneDrive\\MLBAnalyticsJobs\\MLB Swing Decision & Pitch Quality Leaderboards\\R Shiny\\pitch_by_pitch22.csv", row.names = FALSE)


round(sqrt(mean((full_preds[,y_var] %>% pull() - full_preds$xRV)^2)),3)
round(sd(full_preds$lin_weight),3)

team_pitch_quality_leaders(full_preds, "all") %>% View()
pitch_quality_leaders(full_preds, "Fastball", 300) %>% View() filter(Team == "Giants") %>% View()
pitch_quality_leaders(full_preds, "Breaking Ball", 200) %>% filter(Team == "Giants") %>% View()
pitch_quality_leaders(full_preds, "Offspeed", 300) %>% filter(Team == "Giants") %>% View()
pitch_quality_leaders(full_preds, "all", 200) %>% View()
sds_individual_leaders(full_preds, 500) %>% View()
sds_team_leaders(full_preds) %>% View()


sort(unique(full_preds22$person_full_name))
test <- pitch_quality_individual_pitches(full_preds22, "Sam Long")
test2 <- get_video_links(test, length(unique(test$game_pk)))
##############################################################################################
