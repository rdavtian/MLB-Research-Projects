add_team_names <- function(data)
{
  data$team <- ifelse(str_extract(data$game_date, "20[0-9][0-9]") == 2019, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == 2019], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == 2019])), decreasing = T)[1]), 
                      ifelse(str_extract(data$game_date, "20[0-9][0-9]") == 2020, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == 2020], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == 2020])), decreasing = T)[1]),
                             ifelse(str_extract(data$game_date, "20[0-9][0-9]") == 2021, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == 2021], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == 2021])), decreasing = T)[1]), NA_character_)))
  #data <- data %>% 
  #mutate(team = case_when(str_extract(data$game_date, "20[0-9][0-9]") == 2019 ~  
  #names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == 2019], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == 2019])), decreasing = T)[1]),
  #str_extract(data$game_date, "20[0-9][0-9]") == 2020 ~ 
  #names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == 2020], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == 2020])), decreasing = T)[1]),
  #TRUE ~ NA_character_)
  #) %>%
  data <- data %>%
    mutate(team_name = case_when(team == "LAA" ~ "angels", team == "HOU" ~ "astros",
                                 team == "OAK" ~ "athletics", team == "TOR" ~ "blue_jays",
                                 team == "ATL" ~ "braves", team == "MIL" ~ "brewers",
                                 team == "STL" ~ "cardinals", team == "CHC" ~ "cubs",
                                 team == "ARI" ~ "diamondbacks", team == "LAD" ~ "dodgers",
                                 team == "SF" ~ "giants", team == "CLE" ~ "indians",
                                 team == "SEA" ~ "mariners", team == "MIA" ~ "marlins",
                                 team == "NYM" ~ "mets", team == "WSH" ~ "nationals",
                                 team == "BAL" ~ "orioles", team == "SD" ~ "padres",
                                 team == "PHI" ~ "phillies", team == "PIT" ~ "pirates",
                                 team == "TEX" ~ "rangers", team == "TB" ~ "rays",
                                 team == "BOS" ~ "red_sox", team == "CIN" ~ "reds",
                                 team == "COL" ~ "rockies", team == "KC" ~ "royals",
                                 team == "DET" ~ "tigers", team == "MIN" ~ "twins",
                                 team == "CWS" ~ "white_sox", TRUE ~ "yankees"),
           home_team_name = case_when(home_team == "LAA" ~ "angels", home_team == "HOU" ~ "astros",
                                      home_team == "OAK" ~ "athletics", home_team == "TOR" ~ "blue_jays",
                                      home_team == "ATL" ~ "braves", home_team == "MIL" ~ "brewers",
                                      home_team == "STL" ~ "cardinals", home_team == "CHC" ~ "cubs",
                                      home_team == "ARI" ~ "diamondbacks", home_team == "LAD" ~ "dodgers",
                                      home_team == "SF" ~ "giants", home_team == "CLE" ~ "indians",
                                      home_team == "SEA" ~ "mariners", home_team == "MIA" ~ "marlins",
                                      home_team == "NYM" ~ "mets", home_team == "WSH" ~ "nationals",
                                      home_team == "BAL" ~ "orioles", home_team == "SD" ~ "padres",
                                      home_team == "PHI" ~ "phillies", home_team == "PIT" ~ "pirates",
                                      home_team == "TEX" ~ "rangers", home_team == "TB" ~ "rays",
                                      home_team == "BOS" ~ "red_sox", home_team == "CIN" ~ "reds",
                                      home_team == "COL" ~ "rockies", home_team == "KC" ~ "royals",
                                      home_team == "DET" ~ "tigers", home_team == "MIN" ~ "twins",
                                      home_team == "CWS" ~ "white_sox", TRUE ~ "yankees"))
  return(data)
}

spray_chart_advanced_home <- function(data, title)
{
  chart_list <- list()
  for (i in 1:length(unique(data$team_name)))
  {
    plot <- data %>% filter(home_team_name == team_name & 
                              team_name == unique(data$team_name)[i]) %>%
      filter(bb_type != "null") %>%
      mutate(bb_type = as.factor(bb_type)) %>% 
      mutate(bb_type = forcats::fct_relevel(bb_type, "Ground Ball", "Line Drive", "Fly Ball","Popup")) %>%
      ggplot(aes(x = hc_x_, y = hc_y_, color = events, size = bb_type)) + 
      geom_spraychart(stadium_ids = unique(data$team_name)[i],
                      stadium_transform_coords = TRUE, 
                      stadium_segments = "all") +
      theme_void() + 
      coord_fixed(ratio = 0.9) + 
      scale_size_discrete(range = c(1,3,6,10)) +
      labs(size = "Batted Ball Type",
           color = "Events") +
      ggtitle(paste(strsplit(unique(data$player_name), " ")[[1]][2],title, "-" ,unique(data$team)[i])) + 
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
            axis.ticks = element_blank()) + 
      theme(plot.title = element_text(hjust = 0.5, vjust=0,size=16,face = 'bold'))
    if (i == 1 & length(unique(data$team_name)) == 2)
    {
      plot <- plot + theme(legend.position = "none") 
    }
    if ((i == 1 | i == 2) & length(unique(data$team_name)) == 3)
    {
      plot <- plot + theme(legend.position = "none") 
    }
    
    chart_list[[i]] <- plot
    
  }
  if (length(unique(data$team_name)) == 1)
  {
    return(chart_list[[1]])
  }
  if (length(unique(data$team_name)) == 2)
  {
    return((chart_list[[1]] | chart_list[[2]]))
  }
  if (length(unique(data$team_name)) == 3)
  {
    return((chart_list[[1]] | chart_list[[2]] | chart_list[[3]]))
  }
}

spray_chart_advanced_generic <- function(data, title)
{
  data %>% filter(bb_type != "null") %>%
    mutate(bb_type = as.factor(bb_type)) %>% 
    mutate(bb_type = forcats::fct_relevel(bb_type, "Ground Ball", "Line Drive", "Fly Ball","Popup")) %>%
    ggplot(aes(x = hc_x_, y = hc_y_, color = events, size = bb_type)) + 
    geom_spraychart(stadium_ids = "generic",
                    stadium_transform_coords = TRUE, 
                    stadium_segments = "all") +
    theme_void() + 
    coord_fixed(ratio = 0.9) + 
    scale_size_discrete(range = c(1,3,6,10)) +
    labs(size = "Batted Ball Type",
         color = "Events") +
    ggtitle(paste(unique(data$player_name),title)) + 
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
          axis.ticks = element_blank()) + 
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=16,face = 'bold'))
}

create_strikezone <- function()
{
  x <- c(-.95,.95,.95,-.95,-.95)
  z <- c(1.5,1.5,3.5,3.5,1.5)
  sz <- as.data.frame(tibble(x,z)) 
  g <- ggplot() + geom_path(data = sz, aes(x=x, y=z), lwd = 1.2) +
    coord_equal() + xlab("feet from home plate") +
    ylab("feet above the ground") + xlim(-1.5,1.5) + ylim(0.5,4.5)
  return(g)
}

pitch_chart_batter <- function(data, title)
{
  zone <- create_strikezone()
  data <- data %>% filter(pitch_name2 != 'null')
  zone + geom_point(data = data, aes(x = plate_x, y = plate_z, size = release_speed, color = pitch_name2)) +
    scale_size(range = c(0.5,3.5)) +
    viridis::scale_color_viridis(discrete = TRUE, option = "C") +
    labs(size = "Pitch Speed",
         color = "Pitch Type",
         title = paste(unique(data$player_name), title)) +
    ylab("Feet Above Homeplate") +
    xlab("Feet From Homeplate (Catcher's Perspective)") +
    theme(plot.title=element_text(hjust=0.5,vjust=0,size=16,face = 'bold'),
          plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 8)) +
    theme(axis.text.x=element_text(vjust = .5, size=11,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=12,colour="#535353",face="bold")) +
    theme(axis.title.y=element_text(size=12,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=12,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) +
    theme(panel.background = element_rect(fill = "white")) +
    facet_grid(~ p_throws)
}

pitch_chart_pitcher <- function(data, title)
{
  zone <- create_strikezone()
  data <- data %>% filter(pitch_name2 != 'null')
  zone + geom_point(data = data, aes(x = plate_x, y = plate_z, size = release_speed, color = pitch_name2)) +
    scale_size(range = c(0.5,3.5)) +
    viridis::scale_color_viridis(discrete = TRUE, option = "C") +
    labs(size = "Pitch Speed",
         color = "Pitch Type",
         title = paste(unique(data$player_name), title)) +
    ylab("Feet Above Homeplate") +
    xlab("Feet From Homeplate (Catcher's Perspective)") +
    theme(plot.title=element_text(hjust=0.5,vjust=0,size=16,face = 'bold'),
          plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 8)) +
    theme(axis.text.x=element_text(vjust = .5, size=12,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=12,colour="#535353",face="bold")) +
    theme(axis.title.y=element_text(size=12,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=12,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) +
    theme(panel.background = element_rect(fill = "white")) +
    facet_grid(~ stand)
}

if_shift_usage <- function(data, title)
{
  shift <- data %>% filter(!is.na(events)) %>% 
    filter(!is.na(if_fielding_alignment) & if_fielding_alignment != 'null') %>%
    group_by(if_fielding_alignment) %>%
    summarize(wOBA = round(mean(as.numeric(woba_value), na.rm = T),3),
              ISO = round(mean(as.numeric(iso_value), na.rm = T),3),
              N = n(), .groups = 'drop') %>%
    mutate(total = sum(N)) %>%
    mutate(prop = round(N / total, 3) * 100) %>%
    select(-total, -N) %>%
    select(if_fielding_alignment, prop, wOBA, ISO) %>%
    rename("IF Defense" = "if_fielding_alignment",
           "% Faced" = "prop")
  kable(shift, row.names = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
}
of_shift_usage <- function(data, title)
{
  shift <- data %>% filter(!is.na(events)) %>%
    filter(!is.na(of_fielding_alignment) & of_fielding_alignment != 'null') %>%
    group_by(of_fielding_alignment) %>%
    summarize(wOBA = round(mean(as.numeric(woba_value), na.rm = T),3),
              ISO = round(mean(as.numeric(iso_value), na.rm = T),3),
              N = n(), .groups = 'drop') %>%
    mutate(total = sum(N)) %>%
    mutate(prop = round(N / total, 3) * 100) %>%
    select(-total, -N) %>%
    select(of_fielding_alignment, prop, wOBA, ISO) %>%
    rename("OF Defense" = "of_fielding_alignment",
           "% Faced" = "prop")
  kable(shift, row.names = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
}

query_hitter <- function(Full_Name)
{
  First_Name <- scan(text = Full_Name, what = "", quiet = TRUE)[1]
  First_Name_Clean <- stringr::str_remove_all(First_Name, "[.]")
  Last_Name <- scan(text = Full_Name, what = "", quiet = TRUE)[2]
  Last_Name_Clean <- stringr::str_remove_all(Last_Name, "[.]")
  #index <- as.numeric(rownames(hitters_list[(hitters_list$Last == Last_Name) & hitters_list$First == First_Name,]))
  player_id <- playerid_lookup(last_name = Last_Name_Clean, first_name = First_Name_Clean) %>% 
    filter(birth_year > 1975 & !is.na(mlb_played_first)) %>% pull(mlbam_id)
  hitter_19 <- scrape_statcast_savant(start_date = "2019-03-25", end_date = "2019-09-30", playerid = 	player_id)
  hitter_20 <- scrape_statcast_savant(start_date = "2020-03-25", end_date = "2020-09-30", playerid = 	player_id)
  hitter_21 <- scrape_statcast_savant(start_date = "2021-03-25", end_date = "2021-09-30", playerid = 	player_id)
  hitter <- rbind(hitter_19, hitter_20, hitter_21)
  return(hitter)
}

query_pitcher <- function(Full_Name)
{
  First_Name <- scan(text = Full_Name, what = "", quiet = TRUE)[1]
  First_Name_Clean <- stringr::str_remove_all(First_Name, "[.]")
  Last_Name <- scan(text = Full_Name, what = "", quiet = TRUE)[2]
  Last_Name_Clean <- stringr::str_remove_all(Last_Name, "[.]")
  #index <- as.numeric(rownames(pitchers_list[(pitchers_list$Last == Last_Name) & pitchers_list$First == First_Name,]))
  player_id <- playerid_lookup(last_name = Last_Name_Clean, first_name = First_Name_Clean) %>% 
    filter(birth_year > 1975 & !is.na(mlb_played_first)) %>% pull(mlbam_id)
  pitcher_19 <- scrape_statcast_savant(start_date = "2019-03-25", end_date = "2019-09-30", playerid = player_id, player_type = "pitcher")
  pitcher_20 <- scrape_statcast_savant(start_date = "2020-03-25", end_date = "2020-09-30", playerid = player_id, player_type = "pitcher")
  pitcher_21 <- scrape_statcast_savant(start_date = "2021-03-25", end_date = "2021-09-30", playerid = player_id, player_type = "pitcher")
  pitcher <- rbind(pitcher_19, pitcher_20, pitcher_21)
  return(pitcher)
}

spray_chart <- function(data, title)
{
  ggplot(data = data, aes(x = hc_x, y = -hc_y, color = events, size = bb_type)) +
    geom_point() +
    geom_curve(x = 33, xend = 223, y = -100, yend = -100, curvature = -0.65, size = 1.1, color = 'black') + 
    geom_segment(x = 128, xend = 33, y = -208, yend = -100, size = 1.1, color = "black") + 
    geom_segment(x = 128, xend = 223, y = -208, yend = -100, size = 1.1, color = "black") + 
    geom_curve(x = 83, xend = 173, y = -155, yend = -156, curvature = -0.65, size = 1.1, color = "black") + 
    coord_fixed() + 
    scale_size_discrete(range = c(1,3,6,10)) +
    labs(size = "Batted Ball Type",
         color = "Events") + 
    scale_x_continuous(NULL, limits = c(5, 230)) +
    scale_y_continuous(NULL, limits = c(-230, -5)) + 
    ggtitle(paste(unique(data$player_name), title)) + 
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
          axis.ticks = element_blank()) + 
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=16,face = 'bold'))
}

pitch_arsenal <- function(data, title)
{
  pitch_distr <- data %>% filter(pitch_name2 != 'null') %>%
    group_by(pitch_name2) %>% 
    summarize(n = n(), .groups = 'drop') %>%
    mutate(total = sum(n)) %>%
    mutate(prop = round(n / total, 3) * 100) %>%
    arrange(-prop) %>%
    select(-n) %>%
    mutate(ypos = cumsum(prop) - 0.5*prop) %>%
    mutate(ypos = ypos)
  
  ggplot(pitch_distr, aes(x = "", y = prop, fill = pitch_name2)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(prop, "%")), position = position_stack(vjust = 0.5),
              color = "black", size = 4.3) +
    geom_text(aes(label = paste0(prop, "%")), position = position_stack(vjust = 0.5),
              color = "black", size = 4.4) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=16,face = 'bold')) + 
    labs(color = "Pitch Type",
         title = paste0(unique(data$player_name), title, " (", pitch_distr$total[1] ," Pitches)")) + 
    guides(fill=guide_legend(title="Pitch Type"))
}

contact_chart <- function(data, title)
{
  limits <- c("Weak","Topped","Under","Flare","Solid","Barrel")
  ggplot(data %>% filter(launch_speed_angle != 'null'), aes(x = launch_speed_angle)) +  
    geom_bar(aes(y = (..count..)/sum(..count..)), fill = 'red') + 
    scale_y_continuous(labels=scales::percent) + 
    labs(y = "Percentage", x = "Contact Type") + 
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y= (..count..)/sum(..count..)), stat= "count", vjust = -0.1, size = 6.1) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y= (..count..)/sum(..count..)), stat= "count", vjust = -0.1, size = 6.15) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y= (..count..)/sum(..count..)), stat= "count", vjust = -0.1, size = 6.2) +
    ggtitle(paste(unique(data$player_name), title)) + 
    scale_x_discrete(limits = limits) + 
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=16,face = 'bold')) + 
    theme(axis.text.x=element_text(vjust = .5, size=14,colour="#535353",face="bold")) + 
    theme(axis.text.y=element_text(size=14,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=14,colour="#535353",face="bold",vjust=1.5)) + 
    theme(axis.title.x=element_text(size=14,colour="#535353",face="bold",vjust=0))
}

pitch_velocity <- function(data, title)
{
  mean_speed_by_pitchtype <- data %>% 
    filter(pitch_name2 != 'null') %>%
    group_by(pitch_name2) %>%
    mutate(mean_speed_pitch = mean(release_speed, na.rm = T))
  min <- as.numeric(floor(quantile(mean_speed_by_pitchtype$release_speed, na.rm = T, c(0.01, 0.99))[1] / 1) * 1)
  max <- as.numeric(ceiling(quantile(mean_speed_by_pitchtype$release_speed, na.rm = T, c(0.01, 0.99))[2]))
  ggplot(mean_speed_by_pitchtype, aes(x = release_speed)) + 
    geom_density(aes(fill = pitch_name2)) + ggtitle("Distribution of Pitch Velocity by Pitch Type") +
    facet_grid(pitch_name2 ~ ., scales = 'fixed') + 
    xlab("Velocity (MPH)") + ylab("Density") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=16,face = 'bold')) +  
    labs(color = "Pitch Type",
         title = paste(unique(data$player_name), title)) + 
    guides(fill=guide_legend(title="Pitch Type")) + 
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) + 
    geom_vline(aes(xintercept = mean_speed_pitch)) + 
    coord_cartesian(xlim=c(min, max)) + 
    scale_x_continuous(breaks = seq(min, max, 2))
  
}

pitch_spinrate <- function(data, title)
{
  mean_spin_by_pitchtype <- data %>% 
    filter(pitch_name2 != 'null') %>%
    group_by(pitch_name2) %>%
    mutate(mean_spin_pitch = mean(release_spin_rate, na.rm = T))
  min <- as.numeric(floor(quantile(mean_spin_by_pitchtype$release_spin_rate, na.rm = T, c(0.01, 0.99))[1] / 100) * 100)
  max <- ceiling(as.numeric(quantile(mean_spin_by_pitchtype$release_spin_rate, na.rm = T, c(0.01, 0.99))[2]) / 100) * 100
  
  ggplot(mean_spin_by_pitchtype, aes(x = release_spin_rate)) + 
    geom_density(aes(fill = pitch_name2)) + ggtitle("Distribution of Spin Rate by Pitch Type") +
    facet_grid(pitch_name2 ~ ., scales = 'fixed') + 
    xlab("Spin Rate (RPM)") + ylab("Density") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=16,face = 'bold')) +  
    labs(color = "Pitch Type",
         title = paste(unique(data$player_name), title)) + 
    guides(fill=guide_legend(title="Pitch Type")) + 
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) + 
    geom_vline(aes(xintercept = mean_spin_pitch)) +
    coord_cartesian(xlim=c(min - 50, max + 50)) + 
    scale_x_continuous(breaks = seq(min - 100, max + 100, 200))
}

pitch_movement <- function(data, title)
{
  data2 <- data %>% filter(pitch_name2 != 'null') %>%
    filter(pfx_x < 25 & pfx_x > -25) %>%
    filter(pfx_z < 30 & pfx_z > -30)
  ggplot(data = data2) + 
    geom_point(aes(x = pfx_x, y = pfx_z, color = pitch_name2)) +
    scale_x_continuous(breaks = round(seq(round(min(data$pfx_x, na.rm = T), -1), round(max(data$pfx_x, na.rm = T), -1), by = 5),1)) +
    scale_y_continuous(breaks = round(seq(round(min(data$pfx_z, na.rm = T), -1), round(max(data$pfx_z, na.rm = T), -1), by = 5),1)) +
    xlab("Horizontal Break (Inches) Catcher's Perspective") + ylab("Vertical Break (Inches)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=16,face = 'bold')) + 
    labs(color = "Pitch Type",
         title = paste(unique(data$player_name), title)) + 
    guides(fill=guide_legend(title="Pitch Type")) #+ 
  #facet_grid(~stand)
}

exit_velocity <- function(data, title)
{
  ggplot(data, aes(x = launch_speed)) + 
    geom_density(fill = 'cyan') + 
    ggtitle("Distribution of Exit Velocity 2019-2021") +
    xlab("Velocity (MPH)") + ylab("Density") + 
    labs(color = "Pitch Type",
         title = paste(unique(data$player_name), title),
         subtitle = "* Vertical Line = Avg. Exit Velocity") + 
    guides(fill=guide_legend(title="Pitch Type")) + 
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) + 
    geom_vline(aes(xintercept = mean(launch_speed, na.rm = T))) + 
    theme(plot.title=element_text(hjust=0.5,vjust=0,size=16, face = 'bold'),
          plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 9))
}

whiff_by_pitch_type <- function(data, title)
{
  whiff <- data %>% filter(is_swing == 1) %>%
    filter(pitch_name2 != 'null') %>%
    group_by(pitch_name2) %>%
    summarise(whiffs = sum(description == "swinging_strike" | description == "swinging_strike_blocked"),
              N = n(), 
              perc_seen = round(N / nrow(data %>% filter(is_swing == 1)),3)*100,
              .groups = 'drop') %>%
    mutate(whiff_rate = round(whiffs / N, 3) * 100) %>%
    select(-whiffs, -N) %>%
    arrange(-perc_seen) %>%
    rename("Pitch Type" = "pitch_name2", "Whiff %" = "whiff_rate",
           "Pitch Distribution" = "perc_seen")
  
  kable(whiff, row.names = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
  
  
}

batted_ball_type <- function(data, title)
{
  hit_distr <- data %>% filter(bb_type != 'null' & !is.na(bb_type)) %>%
    mutate(bb_type = case_when(bb_type == "Fly Ball" ~ "FB %",
                               bb_type == "Ground Ball" ~ "GB %",
                               bb_type == "Line Drive" ~ "LD %",
                               bb_type == "Popup" ~ "PU %")) %>%
    group_by(bb_type) %>% tally() %>%
    mutate(total = sum(n)) %>%
    mutate(rate = round(n / total, 3) * 100) %>% 
    select(bb_type, rate) %>%
    tidyr::spread(bb_type, rate)
  
  kable(hit_distr, row.names = F, format = "html") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
}

batted_ball_by_pitch_type <- function(data, title)
{
  pitch_distr <- data %>% filter(bb_type != 'null' & !is.na(bb_type)) %>%
    filter(pitch_name2 != 'null' & !is.na(pitch_type2)) %>%
    group_by(pitch_name2) %>%
    summarize(N = n(), 
              perc_seen = round(N / nrow(data %>% filter(bb_type != 'null' & pitch_name2 != 'null' & !is.na(pitch_type2) & !is.na(bb_type))),3)*100,
              .groups = 'drop') %>% select(-N)
  
  hit_distr <- data %>% filter(bb_type != 'null' & !is.na(bb_type)) %>%
    filter(pitch_name2 != 'null' & !is.na(pitch_type2)) %>%
    mutate(bb_type = case_when(bb_type == "Fly Ball" ~ "FB %",
                               bb_type == "Ground Ball" ~ "GB %",
                               bb_type == "Line Drive" ~ "LD %",
                               bb_type == "Popup" ~ "PU %")) %>%
    group_by(pitch_name2) %>% 
    mutate(total = n()) %>% 
    ungroup() %>%
    group_by(pitch_name2, bb_type) %>%
    mutate(count = n()) %>%
    summarize(rate = round(count / total, 3) * 100, .groups = 'drop') %>% 
    select(bb_type, pitch_name2, rate) %>% distinct() %>%
    tidyr::spread(bb_type, rate) %>%  
    mutate_all(~ tidyr::replace_na(., 0)) %>%
    inner_join(pitch_distr, by = "pitch_name2") %>%
    select(pitch_name2, perc_seen, `FB %`, `GB %`, `LD %`, `PU %`) %>%
    arrange(-perc_seen) %>%
    rename("Pitch Type" = "pitch_name2", "Pitch Distribution" = "perc_seen")
  
  kable(hit_distr, row.names = F, format = "html") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
}

batter_stats <- function(data, title)
{
  tab <- data %>% filter(!is.na(events)) %>%
    group_by(p_throws) %>%
    summarize(PA = n(),
              prop = round(PA / nrow(data %>% filter(!is.na(events))),3) * 100,
              BB = sum(events == 'Walk'),
              K = sum(events == "Strike Out"),
              BB. = round(BB / PA, 3) * 100,
              K. = round(K / PA, 3) * 100,
              H = sum(events %in% c("Single","Double","Triple","Home Run")),
              BIP = sum(description %in% c("hit_into_play_no_out","hit_into_play","hit_into_play_score")),
              BABIP = round(H / BIP, 3),
              xwOBA = round(mean(estimated_woba_using_speedangle),3),
              xBA = round(mean(estimated_ba_using_speedangle),3),
              hard_hit_rate = round(mean(hard_hit, na.rm = T), 3) * 100,
              .groups = 'drop') %>%
    select(-BB, - K, -PA, -H, - BIP) %>%
    rename("BB%" = "BB.", "K%" = "K.", "Pitcher" = "p_throws", 
           "% Faced" = "prop", "Hard Hit %" = "hard_hit_rate")
  
  kable(tab, row.names = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
}

pitcher_stats <- function(data, title)
{
  tab <- data %>% filter(!is.na(events)) %>%
    group_by(stand) %>%
    summarize(PA = n(),
              prop = round(PA / nrow(data %>% filter(!is.na(events))),3) * 100,
              BB = sum(events == 'Walk'),
              K = sum(events == "Strike Out"),
              BB. = round(BB / PA, 3) * 100,
              K. = round(K / PA, 3) * 100,
              H = sum(events %in% c("Single","Double","Triple","Home Run")),
              BIP = sum(description %in% c("hit_into_play_no_out","hit_into_play","hit_into_play_score")),
              BABIP = round(H / BIP, 3),
              xwOBA = round(mean(estimated_woba_using_speedangle),3),
              xBA = round(mean(estimated_ba_using_speedangle),3),
              hard_hit_rate = round(mean(hard_hit, na.rm = T), 3) * 100,
              .groups = 'drop') %>%
    select(-BB, - K, -PA, -H, -BIP) %>%
    rename("BB%" = "BB.", "K%" = "K.", "Batter" = "stand", 
           "% Faced" = "prop", "Hard Hit %" = "hard_hit_rate")
  
  kable(tab, row.names = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
}

stats_by_type <- function(data, title)
{
  tab <- data %>% filter(!is.na(events) & pitch_name2 != 'null') %>%
    group_by(pitch_name2) %>%
    summarize(wOBA = round(mean(as.numeric(woba_value), na.rm = T),3),
              ISO = round(mean(as.numeric(iso_value), na.rm = T),3),
              xwOBA = round(mean(estimated_woba_using_speedangle),3),
              xBA = round(mean(estimated_ba_using_speedangle),3),
              hard_hit_rate = round(mean(hard_hit, na.rm = T), 3) * 100,
              N = n(), 
              perc_seen = round(N / nrow(data %>% filter(!is.na(events) & pitch_name2 != 'null')),3)*100,
              .groups = 'drop') %>%
    select(pitch_name2, perc_seen, wOBA, xwOBA, xBA, ISO, hard_hit_rate) %>%
    arrange(-perc_seen) %>%
    rename("Pitch Type" = "pitch_name2", "Hard Hit %" = "hard_hit_rate",
           "Pitch Distribution" = "perc_seen") %>%
    mutate_all(~ tidyr::replace_na(., 0))
  
  kable(tab, row.names = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
}

pitch_usage_by_count <- function(data, title)
{
  usage <- data %>% filter(pitch_type2 != 'null' & !is.na(pitch_type2)) %>%
    select(count, pitch_type2) %>%
    group_by(count, pitch_type2) %>% summarize(n = n(), .groups = 'drop') %>%
    group_by(count) %>%
    mutate(freq = round(n / sum(n), 3)) %>% ungroup() %>%
    group_by(count) %>% 
    mutate(color = ifelse(freq == max(freq), "black", "red"))
  
  ggplot(data = usage, aes(x = pitch_type2, y= freq)) +
    geom_bar(position="dodge", stat="identity", fill = usage$color) + 
    scale_fill_manual(values = c("red2","black")) + 
    facet_wrap( ~ count, ncol = 3) + xlab("Pitch Type") + 
    ggtitle(paste(unique(data$player_name), title)) + ylab('Pitch Usage Rate') + 
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=16,face = 'bold')) + 
    scale_y_continuous(labels=scales::percent) + 
    geom_text(aes(label = paste0(freq * 100, "%"), y = freq + 0.15), size = 3.5) + 
    geom_text(aes(label = paste0(freq * 100, "%"), y = freq + 0.16), size = 3.5)
}

clean_statcast_data <- function(data)
{
  data <- data %>% add_team_names() %>%
    mutate(woba_value = as.numeric(woba_value),
           woba_denom = as.numeric(woba_denom),
           babip_value = as.numeric(babip_value),
           iso_value = as.numeric(iso_value),
           bb_type = case_when(bb_type == "fly_ball" ~ "Fly Ball",
                               bb_type == "ground_ball" ~ "Ground Ball",
                               bb_type == "line_drive" ~ "Line Drive",
                               bb_type == "popup" ~ "Popup",
                               TRUE ~ bb_type)) %>%
    mutate(count = case_when((balls == 0 & strikes == 0) ~ "0-0",
                             (balls == 0 & strikes == 1) ~ "0-1",
                             (balls == 0 & strikes == 2) ~ "0-2",
                             (balls == 1 & strikes == 0) ~ "1-0",
                             (balls == 1 & strikes == 1) ~ "1-1",
                             (balls == 1 & strikes == 2) ~ "1-2",
                             (balls == 2 & strikes == 0) ~ "2-0",
                             (balls == 2 & strikes == 1) ~ "2-1",
                             (balls == 2 & strikes == 2) ~ "2-2",
                             (balls == 3 & strikes == 0) ~ "3-0",
                             (balls == 3 & strikes == 1) ~ "3-1",
                             (balls == 3 & strikes == 2) ~ "3-2"),
           count = as.factor(count),
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
           is_whiff = case_when(description %in% c("swinging_strike_blocked","swinging_strike") ~ 1,
                                TRUE ~ 0),
           in_zone = case_when((plate_z <= 3.5 & plate_z >= 1.5 & abs(plate_x) <= 0.95) ~ 1, TRUE ~ 0),
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
    mutate(pitch_name2 = case_when(pitch_name %in% c("4-Seam Fastball","2-Seam Fastball","Split-Finger") ~ 'Fastball',
                                   pitch_name %in% c("Curveball","Knuckle Curve") ~ 'Curveball',
                                   pitch_name == 'Slider' ~ 'Slider',
                                   pitch_name == 'Cutter' ~ 'Cutter',
                                   pitch_name == 'Sinker' ~ 'Sinker',
                                   pitch_name == 'Changeup' ~ 'Changeup',
                                   TRUE ~ pitch_name),
           pitch_type2 = case_when(pitch_type %in% c("FF","FT","FS") ~ 'FB',
                                   pitch_type %in% c("CU","KC") ~ 'CU',
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
           events2 = case_when(events %in% c("Double","Triple","Home Run") ~ "XBH", 
                               events %in% c("Double Play","Field Out","Force Out","Sac Bunt","Sac Fly") ~ "Field Out",
                               TRUE ~ events),
           p_throws = case_when(p_throws == 'R' ~ 'RHP',
                                p_throws == 'L' ~ 'LHP'),
           stand = case_when(stand == 'R' ~ 'RHB',
                             stand == 'L' ~ 'LHB'),
           pfx_x = pfx_x * 12,
           pfx_z = pfx_z * 12,
           estimated_ba_using_speedangle = as.numeric(estimated_ba_using_speedangle),
           estimated_woba_using_speedangle = as.numeric(estimated_woba_using_speedangle),
           hard_hit = case_when(launch_speed >= 95 ~ 1,
                                launch_speed < 95 ~ 0,
                                TRUE ~ launch_speed),
           hard_hit = as.numeric(hard_hit)) %>%
    mutate(hc_x_ = 2.495671 * (hc_x - 125),
           hc_y_ = 2.495671 * (199 - hc_y)) %>%
    mutate(first_name = str_squish(strsplit(player_name, split = ",")[[1]][2]),
           last_name = str_squish(strsplit(player_name, split = ",")[[1]][1])) %>% 
    tidyr::unite(player_name, c("first_name", "last_name"), sep = " ") %>%
    mutate(x = hc_x - 125.42, 
           y = 198.27 - hc_y,
           phi = (180 * atan(x/y)) / pi, # 100 * atan(x/y),
           spray_angle = case_when(stand == "RHB" ~ phi, TRUE ~ -phi)) %>%
    mutate_at(vars(estimated_ba_using_speedangle, estimated_woba_using_speedangle, 
                   woba_value, woba_denom, babip_value, iso_value), ~replace(., is.na(.), 0))
}

heat_map <- function(data, var, title, binary, legend_title)
{
  topKzone <- 3.5
  botKzone <- 1.5
  inKzone <- -0.95
  outKzone <- 0.95
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone))
  
  if (binary)
  {
    fit <- gam(as.formula(paste0(var, " ~ ", "s(plate_x, plate_z)")), family = binomial, data=data)
    x <- seq(-1.5, 1.5, length.out=50)
    y <- seq(0.5, 5, length.out=50)
    data.predict <- data.frame(plate_x = c(outer(x, y * 0 + 1)),
                               plate_z = c(outer(x * 0 + 1, y)))
    lp <- predict(fit, data.predict)
    data.predict$Prob <- exp(lp) / (1 + exp(lp))
  }
  else{
    fit <- gam(as.formula(paste0(var, " ~ ", "s(plate_x, plate_z)")), data=data)
    x <- seq(-1.5, 1.5, length.out=50)
    y <- seq(0.5, 5, length.out=50)
    data.predict <- data.frame(plate_x = c(outer(x, y * 0 + 1)),
                               plate_z = c(outer(x * 0 + 1, y)))
    lp <- predict(fit, data.predict)
    data.predict$Prob <- lp
    
  }
  ggplot(kZone, aes(x, y)) +
    geom_tile(data=data.predict, 
              aes(plate_x, plate_z, fill = Prob)) +
    scale_fill_distiller(palette = "Spectral") +
    #scale_fill_distiller(palette = "Spectral", limits = c(min, max)) +
    #geom_path(lwd=1.5, col="black") +
    add_zone("black") + 
    coord_fixed() + ylim(1,4) + labs(fill = legend_title) + 
    ggtitle(paste(unique(data$player_name), title)) + 
    # unlist(strsplit(unique(data$player_name), " "))[2]
    xlab("Feet From Homeplate (Catcher's Perspective)") + 
    ylab("Feet Above Homeplate") + xlim(-1.5, 1.5) + centertitle() + 
    theme(axis.text.x=element_text(vjust = .5, size=12,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=12,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=12,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=12,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5))
}

woba_heat_map_batter <- function(data, title)
{
  data2 <- split(data, data$p_throws)
  woba_plot(data2, title = paste(unique(data$player_name), title))
}
xba_heat_map_batter <- function(data, title)
{
  data2 <- split(data, data$p_throws)
  ehit_plot(data2, title = paste(unique(data$player_name), title))
}
xwoba_heat_map_batter <- function(data, title)
{
  data2 <- split(data, data$p_throws)
  ewoba_plot(data2, title = paste(unique(data$player_name), title))
}

woba_heat_map_pitcher <- function(data, title)
{
  data2 <- split(data, data$stand)
  woba_plot(data2, title = paste(unique(data$player_name), title))
}
xba_heat_map_pitcher <- function(data, title)
{
  data2 <- split(data, data$stand)
  ehit_plot(data2, title = paste(unique(data$player_name), title))
}
xwoba_heat_map_pitcher <- function(data, title)
{
  data2 <- split(data, data$stand)
  ewoba_plot(data2, title = paste(unique(data$player_name), title))
}
