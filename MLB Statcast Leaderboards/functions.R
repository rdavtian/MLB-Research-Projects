statcast_exit_velo_barrels <- function(year, player_type, min_pa = 25)
{
  if (player_type %in% c("batter","pitcher"))
  {
    df <- statcast_leaderboards(leaderboard = "exit_velocity_barrels", year = year,
                                player_type = player_type, min_pa = min_pa)
    df <- df %>% 
      mutate(`Player Name` = paste0(first_name, " ", last_name),
             anglesweetspotpercent = anglesweetspotpercent/ 100,
             brl_percent = brl_percent / 100,
             brl_pa = brl_pa / 100,
             ev95percent = ev95percent / 100) %>% 
      rename("Year" = "year", "BIP" = "attempts", "Avg LA" = "avg_hit_angle", 
             "LA Sweet Spot%" = "anglesweetspotpercent", "Max EV" = "max_hit_speed",
             "Avg EV" = "avg_hit_speed", "Max Dist" = "max_distance", 
             "Avg Dist" = "avg_distance", "Avg HR Dist" = "avg_hr_distance",
             "EV 95+" = "ev95plus", "EV 95+ %" = "ev95percent", "Barrels" = "barrels",
             "Barrel%" = "brl_percent", "Barrel/PA%" = "brl_pa", "Avg EV GB" = "gb",
             "Avg EV FB/LD" = "fbld") %>% 
      select(Year, `Player Name`, BIP, `Avg LA`, `LA Sweet Spot%`, `Max EV`, `Avg EV`, `Avg EV FB/LD`,
             `Avg EV GB`,`Max Dist`, `Avg Dist`, `Avg HR Dist`, `EV 95+`, `EV 95+ %`, 
             Barrels, `Barrel%`, `Barrel/PA%`) %>% arrange(`Player Name`)
    
  } else {
    df <- statcast_leaderboards(leaderboard = "exit_velocity_barrels", year = year,
                                player_type = player_type)
    df <- df %>%
      mutate(anglesweetspotpercent = anglesweetspotpercent / 100,
             brl_percent = brl_percent / 100,
             brl_pa = brl_pa / 100,
             ev95percent = ev95percent / 100) %>%
      rename("Team" = "team", "BIP" = "attempts", "Avg LA" = "avg_hit_angle", 
             "LA Sweet Spot%" = "anglesweetspotpercent", "Max EV" = "max_hit_speed",
             "Avg EV" = "avg_hit_speed", "Max Dist" = "max_distance", 
             "Avg Dist" = "avg_distance", "Avg HR Dist" = "avg_hr_distance",
             "EV 95+" = "ev95plus", "EV 95+ %" = "ev95percent", "Barrels" = "barrels",
             "Barrel%" = "brl_percent", "Barrel/PA%" = "brl_pa", "Avg EV GB" = "gb",
             "Avg EV FB/LD" = "fbld", "Year" = "year") %>% 
      select(Year, Team, BIP, `Avg LA`, `LA Sweet Spot%`, `Max EV`, `Avg EV`, `Avg EV FB/LD`,
             `Avg EV GB`,`Max Dist`, `Avg Dist`, `Avg HR Dist`, `EV 95+`, `EV 95+ %`, 
             Barrels, `Barrel%`, `Barrel/PA%`) %>% arrange(Team)
  }
  return(df)
}

statcast_xstat <- function(year, player_type, min_pa = 25)
{
  if (player_type %in% c("batter","pitcher"))
  {
    df <- statcast_leaderboards(leaderboard = "expected_statistics", year = year,
                                player_type = player_type, min_pa = min_pa)
    df <- df %>% 
      mutate(`Player Name` = paste0(first_name, " ", last_name),
             est_ba_minus_ba_diff = round(est_ba_minus_ba_diff, 5),
             est_slg_minus_slg_diff = round(est_slg_minus_slg_diff, 5),
             est_woba_minus_woba_diff = round(est_woba_minus_woba_diff, 5)) %>% 
      rename("Season" = "year", "PA" = "pa", "BA" = "ba", "xBA" = "est_ba",
            "BA - xBA" = "est_ba_minus_ba_diff", "SLG" = "slg", "xSLG" = "est_slg",
            "SLG - xSLG" = "est_slg_minus_slg_diff", "wOBA" = "woba", "xwOBA" = "est_woba",
            "wOBA - xwoBA" = "est_woba_minus_woba_diff", "BIP" = "bip") %>% 
      select(Season, `Player Name`, PA, BIP, BA, xBA, `BA - xBA`, SLG, xSLG, `SLG - xSLG`, 
              wOBA, xwOBA, `wOBA - xwoBA`) %>% arrange(`Player Name`)
  } else {
    df <- statcast_leaderboards(leaderboard = "expected_statistics", year = year,
                                player_type = player_type)
    df <- df %>%
      mutate(est_ba_minus_ba_diff = round(est_ba_minus_ba_diff, 5),
             est_slg_minus_slg_diff = round(est_slg_minus_slg_diff, 5),
             est_woba_minus_woba_diff = round(est_woba_minus_woba_diff, 5)) %>% 
      rename("Team" = "team", "PA" = "pa", "BA" = "ba", "xBA" = "est_ba",
             "BA - xBA" = "est_ba_minus_ba_diff", "SLG" = "slg", "xSLG" = "est_slg",
             "SLG - xSLG" = "est_slg_minus_slg_diff", "wOBA" = "woba", "xwOBA" = "est_woba",
             "wOBA - xwoBA" = "est_woba_minus_woba_diff", "Season" = "year") %>% 
      select(Season, Team, PA, BA, xBA, `BA - xBA`, SLG, xSLG, `SLG - xSLG`, 
             wOBA, xwOBA, `wOBA - xwoBA`) %>% arrange(Team)
  }
  return(df)
}

statcast_pitch_arsenal <- function(year, arsenal_type)
{
  df <- statcast_leaderboards(leaderboard = "pitch_arsenal", year = year, 
                              arsenal_type = arsenal_type) %>% 
    mutate(`Player Name` = paste0(first_name, " ", last_name)) %>% 
    select(-last_name, -first_name, -pitcher) %>% rename("Season" = "year")
  
  if (arsenal_type == "n_")
  {
    df <- df %>%
      mutate(n_ff = n_ff / 100,
             n_si = n_si / 100,
             n_fc = n_fc / 100,
             n_sl = n_sl / 100,
             n_ch = n_ch / 100,
             n_cu = n_cu / 100,
             n_fs = n_fs / 100, 
             n_st = n_st / 100, 
             n_sv = n_sv / 100) %>% 
      select(-n_kn) %>% 
      rename("4-Seam FB%" = "n_ff", "Sinker%" = "n_si", "Cutter%" = "n_fc", 
             "Slider%" = "n_sl", "Changeup%" = "n_ch", "Curveball%" = "n_cu", 
             "Splitter%" = "n_fs", "Sweeper%" = "n_st", "Slurve%" = "n_sv") %>% 
      select(Season, `Player Name`, everything()) %>% arrange(`Player Name`)
  } else if (arsenal_type == "avg_spin") {
    df <- df %>%
      select(-kn_avg_spin) %>% 
      rename("4-Seam FB" = "ff_avg_spin", "Sinker" = "si_avg_spin", "Cutter" = "fc_avg_spin", 
             "Slider" = "sl_avg_spin", "Changeup" = "ch_avg_spin", "Curveball" = "cu_avg_spin", 
             "Splitter" = "fs_avg_spin", "Sweeper" = "st_avg_spin", "Slurve" = "sv_avg_spin") %>% 
      select(Season, `Player Name`, everything()) %>% arrange(`Player Name`)
  } else {
    df <- df %>%
      select(-kn_avg_speed) %>% 
      rename("4-Seam FB" = "ff_avg_speed", "Sinker" = "si_avg_speed", "Cutter" = "fc_avg_speed", 
             "Slider" = "sl_avg_speed", "Changeup" = "ch_avg_speed", "Curveball" = "cu_avg_speed", 
             "Splitter" = "fs_avg_speed", "Sweeper" = "st_avg_speed", "Slurve" = "sv_avg_speed") %>% 
      select(Season, `Player Name`, everything()) %>% arrange(`Player Name`)
  }
  return(df)
}

statcast_outs_above_average <- function(year, fielding_type, min_field = 0)
{
  season <- year
  df <- statcast_leaderboards(leaderboard = "outs_above_average", year = year, min_field = min_field)
  
  if (fielding_type == "player")
  {
    df <- df %>% 
      mutate(Season = season,
             `Player Name` = paste0(first_name, " ", last_name),
             actual_success_rate_formatted = as.numeric(str_replace(actual_success_rate_formatted,"%","")) / 100,
             adj_estimated_success_rate_formatted = as.numeric(str_replace(adj_estimated_success_rate_formatted,"%","")) / 100) %>% 
      select(-year, -first_name, -last_name, -player_id) %>% 
      rename("Team" = "display_team_name", "Primary Pos" = "primary_pos_formatted",
             "DRS" = "fielding_runs_prevented", "OAA" = "outs_above_average",
             "Success%" = "actual_success_rate_formatted", "Estimated Success%" = 
              "adj_estimated_success_rate_formatted") %>% 
      select(Season, `Player Name`, Team, `Primary Pos`, DRS, OAA, `Success%`, 
             `Estimated Success%`) %>% arrange(-DRS, -OAA) %>% 
      mutate(`Primary Pos` = as.factor(`Primary Pos`), 
             Team = as.factor(Team))
  } else {
    df <- df %>% filter(display_team_name != "---") %>% 
      group_by(display_team_name) %>% 
      summarise(`Total DRS` = sum(fielding_runs_prevented, na.rm = T),
                `Total OAA` = sum(outs_above_average, na.rm = T)) %>%
      mutate(Season = season) %>% 
      rename("Team" = "display_team_name") %>%
      select(Season, Team, `Total DRS`, `Total OAA`) %>% arrange(-`Total DRS`, `Total OAA`)
  }
  return(df)
}

#statcast_xstat(year = 2022, player_type = "batter-team")
#statcast_exit_velo_barrels(year = 2022, player_type = "pitcher-team")
#df <- statcast_pitch_arsenal(2022, "n_")
#df <- statcast_outs_above_average(2022, "player", 0)
