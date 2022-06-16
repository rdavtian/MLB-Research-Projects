player_zones <- read.csv("https://raw.githubusercontent.com/rdavtian/MLB-Research-Projects/master/MLB%20Advanced%20Scouting%20Reports/player_zones.csv", fileEncoding = 'UTF-8-BOM')
park_dim <- read.csv("C:/Users/rusla/OneDrive/MLBAnalyticsJobs/MLB Advanced Scouting Reports/park_dimensions.csv", fileEncoding = 'UTF-8-BOM')
season <- substr(Sys.Date(), 1, 4)
fip_c <- 3.133
current_year <- substr(Sys.Date(), 1, 4)

rv_non_bip <- CalledStrike::count_values[1:12,]
names(rv_non_bip) <- c("count","rvnon_bip")

rv_bip <- CalledStrike::count_values[13:nrow(CalledStrike::count_values),]
names(rv_bip) <- c("events","rvbip")
rv_bip <- rv_bip %>% 
  mutate(events = stringr::str_replace_all(events, "_", " "),
         events = stringr::str_to_title(events),
         events = case_when(events == "Hit By Pitch" ~ "HBP",
                            events == "Field Error" ~ "Error",
                            events == "Strikeout" ~ "Strike Out",
                            TRUE ~ events))

add_team_names <- function(data, start_year, end_year)
{
  
  if (end_year - start_year == 0)
  {
    data$team <- ifelse(str_extract(data$game_date, "20[0-9][0-9]") == end_year, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == end_year], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == end_year])), decreasing = T)[1]), NA_character_)
  }
  else if (end_year - start_year == 1)
  {
    data$team <- ifelse(str_extract(data$game_date, "20[0-9][0-9]") == start_year, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year])), decreasing = T)[1]),
                        ifelse(str_extract(data$game_date, "20[0-9][0-9]") == end_year, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == end_year], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == end_year])), decreasing = T)[1]), NA_character_))
  }
  else if (end_year - start_year == 2) {
    data$team <- ifelse(str_extract(data$game_date, "20[0-9][0-9]") == start_year, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year])), decreasing = T)[1]), 
                        ifelse(str_extract(data$game_date, "20[0-9][0-9]") == start_year + 1, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year + 1], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year + 1])), decreasing = T)[1]),
                               ifelse(str_extract(data$game_date, "20[0-9][0-9]") == end_year, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == end_year], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == end_year])), decreasing = T)[1]), NA_character_)))
  }
  else if (end_year - start_year == 3) {
    data$team <- ifelse(str_extract(data$game_date, "20[0-9][0-9]") == start_year, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year])), decreasing = T)[1]), 
                        ifelse(str_extract(data$game_date, "20[0-9][0-9]") == start_year + 1, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year + 1], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year + 1])), decreasing = T)[1]),
                               ifelse(str_extract(data$game_date, "20[0-9][0-9]") == start_year + 2, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year + 2], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year + 2])), decreasing = T)[1]),
                                      ifelse(str_extract(data$game_date, "20[0-9][0-9]") == end_year, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == end_year], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == end_year])), decreasing = T)[1]), NA_character_))))
  } else {
    
    data$team <- ifelse(str_extract(data$game_date, "20[0-9][0-9]") == start_year, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year])), decreasing = T)[1]), 
                        ifelse(str_extract(data$game_date, "20[0-9][0-9]") == start_year + 1, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year + 1], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year + 1])), decreasing = T)[1]),
                               ifelse(str_extract(data$game_date, "20[0-9][0-9]") == start_year + 2, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year + 2], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year + 2])), decreasing = T)[1]),
                                      ifelse(str_extract(data$game_date, "20[0-9][0-9]") == start_year + 3, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year + 3], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == start_year + 3])), decreasing = T)[1]),
                                             ifelse(str_extract(data$game_date, "20[0-9][0-9]") == end_year, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == end_year], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == end_year])), decreasing = T)[1]), NA_character_)))))
    
  }
  
  #data$team <- ifelse(str_extract(data$game_date, "20[0-9][0-9]") == 2021, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == 2021], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == 2021])), decreasing = T)[1]))
  
  #data$team <- ifelse(str_extract(data$game_date, "20[0-9][0-9]") == 2020, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == 2020], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == 2020])), decreasing = T)[1]),
  #ifelse(str_extract(data$game_date, "20[0-9][0-9]") == 2021, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == 2021], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == 2021])), decreasing = T)[1]), NA_character_))
  #data$team <- ifelse(str_extract(data$game_date, "20[0-9][0-9]") == 2019, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == 2019], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == 2019])), decreasing = T)[1]), 
  #ifelse(str_extract(data$game_date, "20[0-9][0-9]") == 2020, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == 2020], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == 2020])), decreasing = T)[1]),
  #ifelse(str_extract(data$game_date, "20[0-9][0-9]") == 2021, names(sort(table(c(data$home_team[str_extract(data$game_date, "20[0-9][0-9]") == 2021], data$away_team[str_extract(data$game_date, "20[0-9][0-9]") == 2021])), decreasing = T)[1]), NA_character_)))
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
      filter(bb_type != "null", bb_type != "", !is.na(bb_type), events != "Strike Out") %>%
      mutate(bb_type = as.factor(bb_type)) %>% 
      mutate(bb_type = forcats::fct_relevel(bb_type, "Ground Ball", "Line Drive", "Fly Ball","Popup")) %>%
      ggplot(aes(x = hc_x_, y = hc_y_, color = events, size = bb_type)) + 
      #geom_hex() +
      #scale_fill_distiller(palette = "Reds", direction = 1) +
      #geom_mlb_stadium(stadium_ids = unique(data$team_name)[i],
      #stadium_transform_coords = TRUE, stadium_segments = "all") +
      geom_spraychart(stadium_ids = unique(data$team_name)[i],
                      stadium_transform_coords = TRUE, stadium_segments = "all") +
      theme_void() + 
      coord_fixed(ratio = 0.9) + 
      scale_size_discrete(range = c(2,5,9,14)) +
      labs(size = "Batted Ball Type",
           color = "Events") +
      ggtitle(paste(strsplit(unique(data$player_name), " ")[[1]][2],title, "-" ,unique(data$team)[i])) + 
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
            axis.ticks = element_blank()) + 
      theme(plot.title = element_text(hjust = 0.5, vjust=0,size=17,face = 'bold'),
            legend.title = element_text(face = "bold", size = 15),
            legend.text = element_text(face = "bold", size = 12),
            legend.key.height= unit(0.7, 'cm'),
            legend.key.width= unit(0.7, 'cm'))
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
  data %>% filter(bb_type != "null", bb_type != "", !is.na(bb_type), events != "Strike Out") %>%
    mutate(bb_type = as.factor(bb_type)) %>% 
    mutate(bb_type = forcats::fct_relevel(bb_type, "Ground Ball", "Line Drive", "Fly Ball","Popup")) %>%
    ggplot(aes(x = hc_x, y = -hc_y, color = events, size = bb_type)) +
    geom_point() + 
    xlim(0,250) +
    ylim(-250, 0) +
    geom_curve(x = 33, xend = 223, y = -100, yend = -100,
               curvature = -.65, colour = "black", size = 1.3) +
    geom_segment(x=128, xend = 33, y=-208, yend = -100, colour = "black", size = 1.3) +
    geom_segment(x=128, xend = 223, y=-208, yend = -100, colour = "black", size = 1.3) +
    geom_curve(x = 83, xend = 173, y = -155, yend = -156,
               curvature = -.65, colour = "black", size = 1.3) +
    theme_void() + 
    coord_fixed(ratio = 0.9) + 
    scale_size_discrete(range = c(2,5,9,14)) +
    labs(size = "Batted Ball Type",
         color = "Events") +
    ggtitle(paste(unique(data$player_name),title)) + 
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
          axis.ticks = element_blank()) + 
    theme(aspect.ratio = 0.9) + 
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=17,face = 'bold'),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm'))
}
create_strikezone <- function(data)
{
  x <- c(-.9,.9,.9,-.9,-.9)
  if (!is.na(unique(data$person_strike_zone_top)) & !is.na(unique(data$person_strike_zone_bottom)))
  {
    if (length(unique(data$batter)) == 1)
    {
      z <- c(unique(data$person_strike_zone_bottom),unique(data$person_strike_zone_bottom),
             unique(data$person_strike_zone_top), unique(data$person_strike_zone_top),
             unique(data$person_strike_zone_bottom))
    } else {
      z <- c(1.55,1.55,3.5,3.5,1.55)
    }  
  }
  else 
  {
    z <- c(1.55,1.55,3.5,3.5,1.55)
  }
  if (unique(data$player_name) == "Jose Altuve")
  {
    z <- c(1.2,1.2,3.15,3.15,1.2)
  }
  sz <- as.data.frame(tibble(x,z)) 
  g <- ggplot() + geom_path(data = sz, aes(x=x, y=z), lwd = 1.5) +
    coord_equal() + xlab("feet from home plate") +
    ylab("feet above the ground") + xlim(-1.5,1.5) + ylim(0.5,4.5)
  return(g)
}

pitch_chart_batter <- function(data, title, hits = FALSE, outs = FALSE)
{
  update_geom_defaults("point",list(size=4))
  zone <- create_strikezone(data)
  data <- data %>% filter(pitch_name2 != 'null', pitch_name2 != '', !is.na(pitch_name2))
  
  if (hits == TRUE)
  {
    data <- data %>% 
      mutate(events = forcats::fct_relevel(events,"Single","Double","Triple","Home Run"))
    
    plot <- zone + geom_point(data = data, aes(x = plate_x, y = plate_z, size = as.factor(events), color = pitch_name2)) + 
      labs(size = "Event", color = "Pitch Type",
           title = paste(unique(data$player_name), title)) + 
      scale_size_discrete(range = c(2,5,9,14))
    
  } else if (outs == TRUE) {
    data2 <- data %>% filter(events != "Strike Out")
    #mutate(events = forcats::fct_relevel(events,"Single","Double","Triple","Home Run"))
    
    plot <- zone + geom_point(data = data2, aes(x = plate_x, y = plate_z, size = as.factor(events), color = pitch_name2)) + 
      labs(size = "Event", color = "Pitch Type",
           title = paste(unique(data$player_name), title)) + 
      scale_size_discrete(range = c(2,5,9,14))
    
  } else{
    plot <- zone + geom_point(data = data, aes(x = plate_x, y = plate_z, size = release_speed, color = pitch_name2)) + 
      labs(size = "Pitch Speed", color = "Pitch Type",
           title = paste(unique(data$player_name), title)) + 
      scale_size(range = c(2,4.8))
  }
  plot +
    #viridis::scale_color_viridis(discrete = TRUE, option = "C") + 
    guides(colour = guide_legend(override.aes = list(size=3))) +
    ylab("Feet Above Homeplate") +
    xlab("Feet From Homeplate (Pitcher's Perspective)") +
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
    facet_grid(~ p_throws) + 
    theme(strip.text = element_text(face="bold", size=13),
          strip.background = element_rect(fill="lightblue", colour="black",size=1))
}

pitch_chart_pitcher <- function(data, title, hits = FALSE, outs = FALSE)
{
  update_geom_defaults("point",list(size=4))
  zone <- create_strikezone(data)
  data <- data %>% filter(pitch_name2 != 'null', pitch_name2 != '', !is.na(pitch_name2))
  
  if (hits == TRUE)
  {
    data <- data %>% 
      mutate(events = forcats::fct_relevel(events,"Single","Double","Triple","Home Run"))
    
    plot <- zone + geom_point(data = data, aes(x = plate_x, y = plate_z, size = as.factor(events), color = pitch_name2)) + 
      labs(size = "Event", color = "Pitch Type",
           title = paste(unique(data$player_name), title)) + 
      scale_size_discrete(range = c(2,5,9,14))
    
  } else if (outs == TRUE) {
    data2 <- data %>% filter(events != "Strike Out")
    #mutate(events = forcats::fct_relevel(events,"Single","Double","Triple","Home Run"))
    
    plot <- zone + geom_point(data = data2, aes(x = plate_x, y = plate_z, size = as.factor(events), color = pitch_name2)) + 
      labs(size = "Event", color = "Pitch Type",
           title = paste(unique(data$player_name), title)) + 
      scale_size_discrete(range = c(2,5,9,14))
    
  } else {
    plot <- zone + geom_point(data = data, aes(x = plate_x, y = plate_z, size = release_speed, color = pitch_name2)) + 
      labs(size = "Pitch Speed", color = "Pitch Type",
           title = paste(unique(data$player_name), title)) + 
      scale_size(range = c(2,4.8))
  }
  plot +
    #viridis::scale_color_viridis(discrete = TRUE, option = "C") + 
    guides(colour = guide_legend(override.aes = list(size=3))) +
    ylab("Feet Above Homeplate") +
    xlab("Feet From Homeplate (Pitcher's Perspective)") +
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
    facet_grid(~ stand) + 
    theme(strip.text = element_text(face="bold", size=13),
          strip.background = element_rect(fill="lightblue", colour="black",size=1))
}
pitch_chart_pitcher_first_pitch <- function(data, title)
{
  update_geom_defaults("point",list(size=4))
  zone <- create_strikezone(data)
  data2 <- data %>% filter(pitch_name2 != 'null', pitch_name2 != '', !is.na(pitch_name2), count == "0-0") %>% 
    mutate(events3 = case_when(events %in% c("Single","Double","Triple","Home Run") ~ "Hit",
                               description %in% c("ball","blocked_ball") ~ "Ball", 
                               events %in% c("Field Out","Sac Bunt","Double Play","Force Out") ~ "Out",
                               description %in% c("foul_tip","foul_bunt","foul","swinging_stike","swinging_strike_blocked","called_strike") ~ "Called/Swinging Strike/Foul")) %>%
    mutate(events3 = forcats::fct_relevel(events3,"Out","Called/Swinging Strike/Foul","Ball","Hit")) %>% 
    filter(!is.na(events3))
  zone + geom_point(data = data2, aes(x = plate_x, y = plate_z, size = as.factor(events3), color = pitch_name2)) +
    #viridis::scale_color_viridis(discrete = TRUE, option = "C") +
    guides(colour = guide_legend(override.aes = list(size=3))) + 
    labs(size = "Event", color = "Pitch Type",
         title = paste(unique(data$player_name), title)) + 
    scale_size_discrete(range = c(2,5,9,14)) + 
    ylab("Feet Above Homeplate") +
    xlab("Feet From Homeplate (Pitcher's Perspective)") +
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
    facet_grid(~ stand) + 
    theme(strip.text = element_text(face="bold", size=13),
          strip.background = element_rect(fill="lightblue", colour="black",size=1))
}
pitch_chart_pitcher_behind <- function(data, title)
{
  update_geom_defaults("point",list(size=4))
  zone <- create_strikezone(data)
  data2 <- data %>% filter(pitch_name2 != 'null', pitch_name2 != '', !is.na(pitch_name2), count %in% c("1-0","2-0","3-0","2-1","3-1")) %>% 
    mutate(events3 = case_when(events %in% c("Single","Double","Triple","Home Run") ~ "Hit",
                               description %in% c("ball","blocked_ball") ~ "Ball", 
                               events %in% c("Field Out","Sac Bunt","Double Play","Force Out") ~ "Out",
                               description %in% c("foul_tip","foul_bunt","foul","swinging_stike","swinging_strike_blocked","called_strike") ~ "Called/Swinging Strike/Foul")) %>%
    mutate(events3 = forcats::fct_relevel(events3,"Out","Called/Swinging Strike/Foul","Ball","Hit")) %>% 
    filter(!is.na(events3))
  zone + geom_point(data = data2, aes(x = plate_x, y = plate_z, size = as.factor(events3), color = pitch_name2)) +
    #viridis::scale_color_viridis(discrete = TRUE, option = "C") +
    guides(colour = guide_legend(override.aes = list(size=3))) + 
    labs(size = "Event", color = "Pitch Type",
         title = paste(unique(data$player_name), title)) + 
    scale_size_discrete(range = c(2,5,9,14)) + 
    ylab("Feet Above Homeplate") +
    xlab("Feet From Homeplate (Pitcher's Perspective)") +
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
    facet_grid(~ stand) + 
    theme(strip.text = element_text(face="bold", size=13),
          strip.background = element_rect(fill="lightblue", colour="black",size=1))
}

pitch_chart_density <- function(data, title)
{
  update_geom_defaults("point",list(size=2))
  zone <- create_strikezone(data)
  data2 <- data %>% filter(pitch_name2 != 'null', pitch_name2 != "", !is.na(pitch_name2)) %>%
    group_by(pitch_name2) %>% 
    mutate(n = n(),
           label = paste0(unique(pitch_name2), " (N = ", unique(n), ")")) %>% 
  filter(n >= 5)
  
  zone + geom_density_2d_filled(data = data2, aes(x = plate_x, y = plate_z), alpha = 0.6, contour_var = "ndensity", bins = 9) +
    scale_fill_brewer(type = "seq", palette = "YlOrRd", direction = 1) + 
    labs(color = "Density",
         title = paste(unique(data2$player_name), title)) + 
    ylab("Feet Above Homeplate") +
    xlab("Feet From Homeplate (Pitcher's Perspective)") +
    theme(plot.title=element_text(hjust=0.5,vjust=0,size=20,face = 'bold'),
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
    facet_wrap(~ label, nrow = 1) + theme(legend.position = "none") + 
    theme(strip.text = element_text(face="bold", size=13),
          strip.background = element_rect(fill="lightblue", colour="black",size=1))
}

pitch_chart_pitch_type <- function(data, title)
{
  myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")))
  update_geom_defaults("point",list(size=3.7, alpha = 0.6))
  ranks <- quantile(data$launch_speed, na.rm = T, c(0, 0.5, 1))
  
  zone <- create_strikezone(data)
  data2 <- data %>% filter(pitch_name2 != 'null', pitch_name2 != '', !is.na(pitch_name2)) %>% 
    mutate(Type = case_when(launch_speed > 95 ~ "Exit Velo > 95",
                            (launch_speed >= 70 & launch_speed < 95) ~ "70 < Exit Velo < 95", 
                            launch_speed < 70 ~ "Exit Velo < 70", 
                            description == "swinging_strike" ~ "Swinging Strike", 
                            description == "ball" ~ "Called Ball", 
                            description == "called_strike" ~ "Called Strike", 
                            TRUE ~ NA_character_), 
           Type = forcats::fct_relevel(Type,"Exit Velo > 95","70 < Exit Velo < 95", "Exit Velo < 70",
                                       "Swinging Strike","Called Strike", "Called Ball")) 
  zone + geom_point(data = data2 %>% filter(!is.na(Type), Type != "70 < Exit Velo < 95"), 
                    aes(x = plate_x, y = plate_z, color = Type)) +
    scale_colour_manual(values = c("Exit Velo > 95" = "red","Exit Velo < 70" = "blue", 
                                   "70 < Exit Velo < 95" = "gold", "Called Ball" = "black", 
                                   "Called Strike" = "green", "Swinging Strike" = "cyan")) +
    guides(colour = guide_legend(override.aes = list(size=3))) +
    labs(color = "Event Type",
         title = paste(unique(data2$player_name), title)) +
    ylab("Feet Above Homeplate") +
    xlab("Feet From Homeplate (Pitcher's Perspective)") +
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
    facet_wrap(~ pitch_name2, nrow = 1) + 
    theme(strip.text = element_text(face="bold", size=13),
          strip.background = element_rect(fill="lightblue", colour="black",size=1))
}

pitch_chart_pitcher_ahead <- function(data, title)
{
  update_geom_defaults("point",list(size=4))
  zone <- create_strikezone(data)
  data2 <- data %>% filter(pitch_name2 != 'null', pitch_name2 != '', !is.na(pitch_name2), count %in% c("0-1","0-2","1-2","2-2")) %>% 
    mutate(events3 = case_when(events %in% c("Single","Double","Triple","Home Run") ~ "Hit",
                               description %in% c("ball","blocked_ball") ~ "Ball", 
                               events %in% c("Field Out","Sac Bunt","Double Play","Force Out") ~ "Out",
                               description %in% c("foul_tip","foul_bunt","foul","swinging_stike","swinging_strike_blocked","called_strike") ~ "Called/Swinging Strike/Foul")) %>%
    mutate(events3 = forcats::fct_relevel(events3,"Out","Called/Swinging Strike/Foul","Ball","Hit")) %>% 
    filter(!is.na(events3))
  zone + geom_point(data = data2, aes(x = plate_x, y = plate_z, size = as.factor(events3), color = pitch_name2)) +
    #viridis::scale_color_viridis(discrete = TRUE, option = "C") +
    guides(colour = guide_legend(override.aes = list(size=3))) + 
    labs(size = "Event", color = "Pitch Type",
         title = paste(unique(data$player_name), title)) + 
    scale_size_discrete(range = c(2,5,9,14)) + 
    ylab("Feet Above Homeplate") +
    xlab("Feet From Homeplate (Pitcher's Perspective)") +
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
    facet_grid(~ stand) + 
    theme(strip.text = element_text(face="bold", size=13),
          strip.background = element_rect(fill="lightblue", colour="black",size=1))
}

if_shift_usage <- function(data, title)
{
  shift <- data %>% filter(!is.na(events)) %>% 
    filter(!is.na(if_fielding_alignment) & if_fielding_alignment != 'null') %>%
    group_by(if_fielding_alignment) %>%
    summarize(PA = n(),
              BB = sum(events == 'Walk'),
              HBP = sum(events == "HBP"),
              SF = sum(events == "Sac Fly"),
              SacBunt = sum(events == "Sac Bunt"),
              Error = sum(events == "Error"),
              Single = sum(events == "Single"),
              Double = sum(events == "Double"),
              Triple = sum(events == "Triple"),
              HR = sum(events == "Home Run"),
              AB = PA - BB - HBP - SF - SacBunt,
              BA = round(mean(is_hit[!events %in% c("Walk","Sac Fly","HBP","Sac Bunt")]),3),
              xBA = round(mean(estimated_ba_using_speedangle[!events %in% c("Walk","HBP")]),3),
              BABIP = round(mean(is_hit[(is_bip == 1) & (!events %in% c("Home Run"))], na.rm = T),3),
              wOBA = round((0.69*BB + 0.72*HBP + 0.89*Single + 1.27*Double + 1.62*Triple + 2.1*HR) / (AB + BB + SF + HBP),3),
              xwOBAcon = round(mean(estimated_woba_using_speedangle[is_bip == 1]),3),
              ISO = round(mean(as.numeric(iso_value[!events %in% c("Walk","Sac Bunt", "HBP", "Sac Fly")]), na.rm = T),3),
              N = n(), .groups = 'drop') %>%
    mutate(total = sum(N)) %>%
    mutate(prop = round(N / total, 3) * 100) %>%
    select(-total, -N) %>%
    arrange(-prop) %>%
    select(if_fielding_alignment, prop, BA, xBA, BABIP, wOBA, xwOBAcon, ISO) %>%
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
    summarize(PA = n(),
              BB = sum(events == 'Walk'),
              HBP = sum(events == "HBP"),
              SF = sum(events == "Sac Fly"),
              SacBunt = sum(events == "Sac Bunt"),
              Error = sum(events == "Error"),
              Single = sum(events == "Single"),
              Double = sum(events == "Double"),
              Triple = sum(events == "Triple"),
              HR = sum(events == "Home Run"),
              AB = PA - BB - HBP - SF - SacBunt,
              BA = round(mean(is_hit[!events %in% c("Walk","Sac Fly","HBP","Sac Bunt")]),3),
              xBA = round(mean(estimated_ba_using_speedangle[!events %in% c("Walk","HBP")]),3),
              BABIP = round(mean(is_hit[(is_bip == 1) & (!events %in% c("Home Run"))], na.rm = T),3),
              wOBA = round((0.69*BB + 0.72*HBP + 0.89*Single + 1.27*Double + 1.62*Triple + 2.1*HR) / (AB + BB + SF + HBP),3),
              xwOBAcon = round(mean(estimated_woba_using_speedangle[is_bip == 1]),3),
              ISO = round(mean(as.numeric(iso_value[!events %in% c("Walk","Sac Bunt", "HBP", "Sac Fly")]), na.rm = T),3),
              N = n(), .groups = 'drop') %>%
    mutate(total = sum(N)) %>%
    mutate(prop = round(N / total, 3) * 100) %>%
    select(-total, -N) %>%
    arrange(-prop) %>%
    select(of_fielding_alignment, prop, BA, xBA, BABIP, wOBA, xwOBAcon, ISO) %>%
    rename("OF Defense" = "of_fielding_alignment",
           "% Faced" = "prop")
  kable(shift, row.names = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
}

query_hitter <- function(Full_Name, start_year, end_year, start_date, end_date)
{
  if (grepl('II', Full_Name) == TRUE)
  {
    Full_Name <- strsplit(Full_Name, split = " ")[[1]][1:2]
  } 
  if ((length(strsplit(Full_Name, split = " ")[[1]]) == 3) & (grepl(".", Full_Name, fixed = T) == FALSE))
  {
    First_Name <- str_c(strsplit(Full_Name, split = " ")[[1]][1:2], collapse = " ")
    First_Name_Clean <- stringr::str_remove_all(First_Name, "[.]")
    Last_Name <- strsplit(Full_Name, split = " ")[[1]][3]
    Last_Name_Clean <- stringr::str_remove_all(Last_Name, "[.]")
  } else if (Full_Name %in% c("J.D. Martinez","J.D. Davis","J.P. Crawford","J.T. Realmuto"))
  {
    First_Name_Clean <- paste0(substr(Full_Name, 1, 2), " ", substr(Full_Name, 3, 4))
    Last_Name_Clean <- trimws(paste0(substr(Full_Name, 5, nchar(Full_Name))))
  } else {
    First_Name <- scan(text = Full_Name, what = "", quiet = TRUE)[1]
    First_Name_Clean <- stringr::str_remove_all(First_Name, "[.]")
    Last_Name <- scan(text = Full_Name, what = "", quiet = TRUE)[2]
    Last_Name_Clean <- stringr::str_remove_all(Last_Name, "[.]")
  }
  
  if (Full_Name == "Tommy La Stella")
  {
    First_Name_Clean <- "Tommy"
    Last_Name_Clean <- "La Stella"
  }
  player_id <- playerid_lookup(last_name = Last_Name_Clean, first_name = First_Name_Clean) %>% 
    filter(birth_year > 1979 & !is.na(mlb_played_first)) %>% pull(mlbam_id)
  if (Full_Name == "JT Chargois")
  {
    player_id <- 608638
  }
  if (Full_Name == "Michael A. Taylor")
  {
    player_id <- 572191
  } else if (Full_Name == "C.J. Cron")
  {
    player_id <- 543068
  }
  hitter_year <- list()
  if (end_year - start_year == 0)
  {
    hitter_year[[end_year - start_year + 1]] <- scrape_statcast_savant(start_date = start_date, end_date = end_date, playerid = player_id) 
  } else if (end_year - start_year == 1)
  {
    hitter_year[[end_year - start_year]] <- scrape_statcast_savant(start_date = start_date, end_date = paste0(start_year, "-12-31"), playerid = player_id)
    hitter_year[[end_year - start_year + 1]] <- scrape_statcast_savant(start_date = paste0(end_year, "-04-07"), end_date = end_date, playerid = player_id) 
  } else 
  {
    hitter_year[[end_year - start_year - 1]] <- scrape_statcast_savant(start_date = start_date, end_date = paste0(start_year, "-12-31"), playerid = player_id)
    hitter_year[[end_year - start_year]] <- scrape_statcast_savant(start_date = paste0(start_year + 1, "-04-01"), end_date = paste0(start_year + 1, "-12-31"), playerid = player_id) 
    hitter_year[[end_year - start_year + 1]] <- scrape_statcast_savant(start_date = paste0(end_year, "-04-01"), end_date = end_date, playerid = player_id) 
  }
  #for (year in start_year:end_year)
  #{
  #hitter_year[[year - start_year + 1]] <- scrape_statcast_savant(start_date = start_date, end_date = end_date, playerid = player_id)
  #}
  hitter_sc <- do.call(plyr::rbind.fill, hitter_year)
  hitter_sc$game_date <- as.Date(hitter_sc$game_date, origin = "1970-01-01")
  return(list(hitter_sc, player_id))
}

query_pitcher <- function(Full_Name, start_year, end_year, start_date, end_date)
{
  if (Full_Name == "J. D. Hammer")
  {
    Full_Name == "J.D. Hammer"
  }
  if (grepl('II', Full_Name) == TRUE)
  {
    Full_Name <- strsplit(Full_Name, split = " ")[[1]][1:2]
  }
  
  if (Full_Name == "Daniel Ponce de")
  {
    First_Name_Clean <- "Daniel"
    Last_Name_Clean <- "Ponce de Leon"
  } else if (Full_Name == "Hyun-Jin Ryu")
  {
    First_Name_Clean <- "Hyun Jin"
    Last_Name_Clean <- "Ryu"
  } else if (Full_Name == "Kwang-hyun Kim")
  {
    First_Name_Clean <- "Kwang Hyun"
    Last_Name_Clean <- "Kim"
  } else if ((length(strsplit(Full_Name, split = " ")[[1]]) == 3) & (grepl(".", Full_Name, fixed = T) == FALSE))
  {
    First_Name <- str_c(strsplit(Full_Name, split = " ")[[1]][1:2], collapse = " ")
    First_Name_Clean <- stringr::str_remove_all(First_Name, "[.]")
    Last_Name <- strsplit(Full_Name, split = " ")[[1]][3]
    Last_Name_Clean <- stringr::str_remove_all(Last_Name, "[.]")
  } else if (Full_Name %in% c("A.J. Alexy","A.J. Minter","J.P. Feyereisen","J.B. Wendelken","J.A. Happ","J.D. Hammer",
                              "T.J. McFarland"))
  {
    First_Name_Clean <- paste0(substr(Full_Name, 1, 2), " ", substr(Full_Name, 3, 4))
    Last_Name_Clean <- trimws(paste0(substr(Full_Name, 5, nchar(Full_Name))))
  } else {
    First_Name <- scan(text = Full_Name, what = "", quiet = TRUE)[1]
    First_Name_Clean <- stringr::str_remove_all(First_Name, "[.]")
    Last_Name <- scan(text = Full_Name, what = "", quiet = TRUE)[2]
    Last_Name_Clean <- stringr::str_remove_all(Last_Name, "[.]")
  }
  
  player_id <- playerid_lookup(last_name = Last_Name_Clean, first_name = First_Name_Clean) %>% 
    filter(birth_year > 1979 & !is.na(mlb_played_first)) %>% pull(mlbam_id)
  if (Full_Name == "JT Chargois")
  {
    player_id <- 608638
  }
  if (Full_Name == "Vladimir Gutierrez")
  {
    player_id <- 661269
  }
  if (Full_Name == "JC Mejia")
  {
    player_id <- 650496
  }
  if (player_id[1] == 671277)
  {
    player_id <- player_id[2]
  }
  #if (Full_Name == "Luis Garcia")
  #{
  #player_id <- 472610
  #}
  pitcher_year <- list()
  if (end_year - start_year == 0)
  {
    pitcher_year[[end_year - start_year + 1]] <- scrape_statcast_savant(start_date = start_date, end_date = end_date, playerid = 	player_id, player_type = "pitcher") 
  } else if (end_year - start_year == 1)
  {
    pitcher_year[[end_year - start_year]] <- scrape_statcast_savant(start_date = start_date, end_date = paste0(start_year, "-12-31"), playerid = 	player_id, player_type = "pitcher")
    pitcher_year[[end_year - start_year + 1]] <- scrape_statcast_savant(start_date = paste0(end_year, "-04-07"), end_date = end_date, playerid = 	player_id, player_type = "pitcher") 
  } else 
  {
    pitcher_year[[end_year - start_year - 1]] <- scrape_statcast_savant(start_date = start_date, end_date = paste0(start_year, "-12-31"), playerid = 	player_id, player_type = "pitcher")
    pitcher_year[[end_year - start_year]] <- scrape_statcast_savant(start_date = paste0(start_year + 1, "-04-01"), end_date = paste0(start_year + 1, "-12-31"), playerid = 	player_id, player_type = "pitcher") 
    pitcher_year[[end_year - start_year + 1]] <- scrape_statcast_savant(start_date = paste0(end_year, "-04-01"), end_date = end_date, playerid = 	player_id, player_type = "pitcher") 
  }
  #for (year in start_year:end_year)
  #{
  #pitcher_year[[year - start_year + 1]] <- scrape_statcast_savant(start_date = start_date, end_date = end_date, playerid = 	player_id, player_type = "pitcher")
  #}
  pitcher_sc <- do.call(plyr::rbind.fill, pitcher_year)
  pitcher_sc$game_date <- as.Date(pitcher_sc$game_date, origin = "1970-01-01")
  return(list(pitcher_sc, player_id))
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
    scale_size_discrete(range = c(2,5,9,14)) +
    labs(size = "Batted Ball Type",
         color = "Events") + 
    scale_x_continuous(NULL, limits = c(5, 230)) +
    scale_y_continuous(NULL, limits = c(-230, -5)) + 
    ggtitle(paste(unique(data$player_name), title)) + 
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
          axis.ticks = element_blank()) + 
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=17,face = 'bold'),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm'))
}

pitch_arsenal <- function(data, title)
{
  pitch_distr <- data %>% filter(pitch_name2 != 'null', pitch_name2 != "", !is.na(pitch_name2)) %>%
    group_by(pitch_name2) %>% 
    summarize(n = n(), .groups = 'drop') %>%
    filter(n >= 5) %>% 
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
              color = "black", size = 6) +
    geom_text(aes(label = paste0(prop, "%")), position = position_stack(vjust = 0.5),
              color = "black", size = 6.1) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=17,face = 'bold'),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm')) + 
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
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=17,face = 'bold')) + 
    theme(axis.text.x=element_text(vjust = .5, size=14,colour="#535353",face="bold")) + 
    theme(axis.text.y=element_text(size=15,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) + 
    theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm'))
}

pitch_velocity <- function(data, title)
{
  mean_speed_by_pitchtype <- data %>% 
    filter(pitch_name2 != 'null', pitch_name2 != "", !is.na(pitch_name2)) %>%
    group_by(pitch_name2) %>%
    mutate(mean_speed_pitch = round(mean(release_speed, na.rm = T), 1))
  min <- as.numeric(floor(quantile(mean_speed_by_pitchtype$release_speed, na.rm = T, c(0.01, 0.99))[1] / 1) * 1)
  max <- as.numeric(ceiling(quantile(mean_speed_by_pitchtype$release_speed, na.rm = T, c(0.01, 0.99))[2]))
  ggplot(mean_speed_by_pitchtype, aes(x = release_speed)) + 
    geom_density(aes(fill = pitch_name2)) + ggtitle("Distribution of Pitch Velocity by Pitch Type") +
    facet_grid(pitch_name2 ~ ., scales = 'fixed') + 
    xlab("Velocity (MPH)") + ylab("Density") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=17,face = 'bold')) +  
    labs(color = "Pitch Type",
         title = paste(unique(data$player_name), title)) + 
    guides(fill=guide_legend(title="Pitch Type")) + 
    geom_label(data=mean_speed_by_pitchtype, aes(x=mean_speed_pitch, y=0.3, label=mean_speed_pitch), colour = "black", nudge_x = 0.7, fontface = 2) + 
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) + 
    geom_vline(aes(xintercept = mean_speed_pitch), size = 1.3) + 
    coord_cartesian(xlim=c(min, max)) + 
    scale_x_continuous(breaks = seq(min, max, 2)) + 
    theme(axis.text.x=element_text(vjust = .5, size=15,colour="#535353",face="bold")) + 
    theme(axis.text.y=element_text(size=15,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) + 
    theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm'))
}

pitch_spinrate <- function(data, title)
{
  mean_spin_by_pitchtype <- data %>% 
    filter(pitch_name2 != 'null', !is.na(release_spin_rate)) %>%
    group_by(pitch_name2) %>%
    mutate(mean_spin_pitch = round(mean(release_spin_rate, na.rm = T)))
  min <- as.numeric(floor(quantile(mean_spin_by_pitchtype$release_spin_rate, na.rm = T, c(0.01, 0.99))[1] / 100) * 100)
  max <- ceiling(as.numeric(quantile(mean_spin_by_pitchtype$release_spin_rate, na.rm = T, c(0.01, 0.99))[2]) / 100) * 100
  
  ggplot(mean_spin_by_pitchtype, aes(x = release_spin_rate)) + 
    geom_density(aes(fill = pitch_name2)) + ggtitle("Distribution of Spin Rate by Pitch Type") +
    facet_grid(pitch_name2 ~ ., scales = 'fixed') +
    xlab("Spin Rate (RPM)") + ylab("Density") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=17,face = 'bold')) +  
    labs(color = "Pitch Type",
         title = paste(unique(data$player_name), title)) + 
    guides(fill=guide_legend(title="Pitch Type")) +
    geom_label(data=mean_spin_by_pitchtype, aes(x=mean_spin_pitch, y=0.003, label=mean_spin_pitch), colour = "black", nudge_x = 40, fontface = 2) + 
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) + 
    geom_vline(aes(xintercept = mean_spin_pitch), size = 1.3) +
    coord_cartesian(xlim=c(min - 50, max + 50)) + 
    scale_x_continuous(breaks = seq(min - 100, max + 100, 200)) + 
    theme(axis.text.x=element_text(vjust = .5, size=15,colour="#535353",face="bold")) + 
    theme(axis.text.y=element_text(size=15,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) + 
    theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm'))
}

pitch_movement <- function(data, title)
{
  update_geom_defaults("point",list(size=3))
  data2 <- data %>% filter(pitch_name2 != 'null') %>%
    filter(pfx_x < 25 & pfx_x > -25) %>%
    filter(pfx_z < 30 & pfx_z > -30) %>%
    group_by(pitch_name2) %>% 
    mutate(`1st percentile pfx_x` = quantile(pfx_x, 0.01),
           `99th percentile pfx_x` = quantile(pfx_x, 0.99),
           `1st percentile pfx_z` = quantile(pfx_z, 0.01),
           `99th percentile pfx_z` = quantile(pfx_z, 0.99)) %>% 
    filter(pfx_x > `1st percentile pfx_x`, pfx_x < `99th percentile pfx_x`,
           pfx_z > `1st percentile pfx_z`, pfx_z < `99th percentile pfx_z`)
  
  ggplot(data = data2) + 
    geom_point(aes(x = pfx_x, y = pfx_z, color = pitch_name2)) +
    scale_x_continuous(limits = c(-25, 25), breaks = seq(-25, 25, by = 5)) + 
    scale_y_continuous(limits = c(-25, 25), breaks = seq(-25, 25, by = 5)) +
    geom_encircle(aes(x = pfx_x, y = pfx_z, group = pitch_name2, fill = pitch_name2), alpha = 0.4) + 
    #scale_x_continuous(breaks = round(seq(round(min(data$pfx_x, na.rm = T), -1), round(max(data$pfx_x, na.rm = T), -1), by = 5),1)) +
    #scale_y_continuous(breaks = round(seq(round(min(data$pfx_z, na.rm = T), -1), round(max(data$pfx_z, na.rm = T), -1), by = 5),1)) +
    xlab("Horizontal Break (Inches) Pitcher's Perspective") + ylab("Vertical Break (Inches)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=17,face = 'bold')) + 
    theme(axis.text.x=element_text(vjust = .5, size=15,colour="#535353",face="bold")) + 
    theme(axis.text.y=element_text(size=15,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) + 
    theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0),
          legend.title = element_text(face = "bold", size = 18),
          legend.text = element_text(face = "bold", size = 15),
          legend.key.height= unit(0.8, 'cm'),
          legend.key.width= unit(0.8, 'cm')) + 
    labs(color = "Pitch Type",
         title = paste(unique(data$player_name), title)) + 
    guides(fill=guide_legend(title="Pitch Type")) + 
    guides(fill = "none") + 
    geom_vline(aes(xintercept = 0), size = 1.6) + 
    geom_hline(aes(yintercept = 0), size = 1.6)
}

exit_velocity <- function(data, title, start_year, end_year)
{
  min_ls <- round(min(data$launch_speed, na.rm = T), -1)
  max_ls <- max(data$launch_speed, na.rm = T)
  ggplot(data, aes(x = launch_speed)) + 
    geom_density(fill = 'cyan') + 
    ggtitle(paste0("Distribution of Exit Velocity ", start_year, "-", end_year)) +
    xlab("Velocity (MPH)") + ylab("Density") + 
    labs(color = "Pitch Type",
         title = paste(unique(data$player_name), title),
         subtitle = "* Vertical Line = Avg. Exit Velocity") + 
    geom_label(data = data, aes(x=mean(launch_speed, na.rm = T), y=0.025, label=round(mean(launch_speed, na.rm = T),1)), colour = "black", nudge_x = 1.5, fontface = 2) + 
    guides(fill=guide_legend(title="Pitch Type")) + 
    scale_x_continuous(breaks = seq(min_ls, max_ls, 10)) + 
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) + 
    geom_vline(aes(xintercept = mean(launch_speed, na.rm = T)), size = 1.3) +
    theme(plot.title=element_text(hjust=0.5,vjust=0,size=17, face = 'bold'),
          plot.subtitle=element_text(face="bold", hjust= 0.5, vjust= .0, colour="#3C3C3C", size = 13)) + 
    theme(axis.text.x=element_text(vjust = .5, size=14,colour="#535353",face="bold")) + 
    theme(axis.text.y=element_text(size=15,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) + 
    theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm'))
}

whiff_by_pitch_type <- function(data, title)
{
  whiff <- data %>% filter(is_swing == 1) %>%
    filter(pitch_name2 != 'null', pitch_name2 != "", !is.na(pitch_name2)) %>%
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
  
  spray_dir <- data %>% filter(spray_angle != 'null' & !is.na(spray_angle) & is_bip == 1) %>%
    mutate(spray_dir = case_when(spray_direction == "Center" ~ "Center %",
                                 spray_direction == "Oppo" ~ "Oppo %",
                                 spray_direction == "Pull" ~ "Pull %")) %>%
    group_by(spray_dir) %>% tally() %>%
    mutate(total = sum(n)) %>%
    mutate(rate = round(n / total, 3) * 100) %>% 
    select(spray_dir, rate) %>%
    tidyr::spread(spray_dir, rate) %>%
    select(`Pull %`, `Center %`, `Oppo %`)
  
  df <- cbind(hit_distr, spray_dir)
  
  kable(df, row.names = F, format = "html") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
}

batted_ball_by_pitch_type <- function(data, title)
{
  pitch_distr <- data %>% filter(bb_type != 'null' & !is.na(bb_type) & bb_type != "NA" & bb_type != "") %>%
    filter(pitch_name != "" & pitch_name2 != 'null' & !is.na(pitch_type2)) %>%
    group_by(pitch_name2) %>%
    summarize(N = n(), 
              perc_seen = round(N / nrow(data %>% filter(bb_type != "NA" & bb_type != "" & bb_type != 'null' & pitch_name2 != "" & pitch_name2 != 'null' & !is.na(pitch_type2) & !is.na(bb_type))),3)*100,
              .groups = 'drop') %>% select(-N)
  
  hit_distr <- data %>% filter(bb_type != 'null' & !is.na(bb_type) & bb_type != "NA" & bb_type != "") %>%
    filter(pitch_name != "" & pitch_name2 != 'null' & !is.na(pitch_type2)) %>%
    mutate(bb_type = case_when(bb_type == "Fly Ball" ~ "FB %",
                               bb_type == "Ground Ball" ~ "GB %",
                               bb_type == "Line Drive" ~ "LD %",
                               bb_type == "Popup" ~ "PU %")) %>%
    group_by(pitch_name2) %>% 
    mutate(total = n()) %>% 
    ungroup() %>%
    group_by(pitch_name2, bb_type) %>%
    mutate(count2 = n()) %>%
    summarize(rate = round(count2 / total, 3) * 100, .groups = 'drop') %>% 
    select(bb_type, pitch_name2, rate) %>% distinct() %>%
    tidyr::spread(bb_type, rate) %>%  
    mutate_if(is.numeric, ~ tidyr::replace_na(., 0)) %>% 
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

batter_basic_stats <- function(data, title, start_year, end_year)
{
  team_batting <- baseball_reference_scraper(level = "team", stat_type = "standard", 
                                             area = "batting", start_year = start_year, 
                                             end_year = end_year) %>% 
    mutate(OBP = as.numeric(OBP),
           SLG = as.numeric(SLG)) %>% 
    summarise(lgOBP = mean(OBP),
              lgSLG = mean(SLG))
  lgOBP <- team_batting$lgOBP
  lgSLG <- team_batting$lgSLG
  
  tab <- data %>% filter(!is.na(events)) %>%
    mutate(on_base = case_when(events %in% c("Single","Double","Triple","Home Run","Walk","HBP") ~ 1,
                               TRUE ~ 0),
           total_bases = case_when(events == "Single" ~ 1,
                                   events == "Double" ~ 2,
                                   events == "Triple" ~ 3,
                                   events == "Home Run" ~ 4,
                                   TRUE ~ NA_real_)) %>%
    group_by(1) %>% 
    summarize(PA = n(),
              BB = sum(events == 'Walk'),
              HBP = sum(events == "HBP"),
              SF = sum(events == "Sac Fly"),
              SacBunt = sum(events == "Sac Bunt"),
              Error = sum(events == "Error"),
              Single = sum(events == "Single"),
              Double = sum(events == "Double"),
              Triple = sum(events == "Triple"),
              HR = sum(events == "Home Run"),
              AB = PA - BB - HBP - SF - SacBunt,
              BA = round(sum(events %in% c("Single","Double","Triple","Home Run")) / AB, 3),
              #BA = round(mean(is_hit[!events %in% c("Walk","Sac Fly","HBP","Sac Bunt","Error")]),3),
              OBP = round(sum(on_base, na.rm = T) / PA, 3),
              SLG = round(sum(total_bases, na.rm = T) / AB, 3),
              OPS = OBP + SLG, 
              OPS_plus = round((((OBP / lgOBP) + (SLG / lgSLG)) - 1) * 100),
              wOBA = round((0.69*BB + 0.72*HBP + 0.89*Single + 1.27*Double + 1.62*Triple + 2.1*HR) / (AB + BB + SF + HBP),3),
              xwOBAcon = round(mean(estimated_woba_using_speedangle[is_bip == 1]),3),
              BABIP = round(mean(is_hit[(is_bip == 1) & (!events %in% c("Home Run"))], na.rm = T),3),
              K = sum(events == "Strike Out"),
              H = sum(events %in% c("Single","Double","Triple","Home Run")),
              K. = round(K / PA, 3) * 100,
              BB. = round(BB / PA, 3) * 100,
              xBA = round(mean(estimated_ba_using_speedangle[!events %in% c("Walk","HBP")]),3),
              `Hard Hit %` = round(mean(hard_hit[!events %in% c("Walk","Strike Out","HBP")], na.rm = T), 3) * 100,
              `Barrel %` = round(mean(is_barrel[!events %in% c("Walk","Strike Out","HBP")], na.rm = T), 3) * 100,
              .groups = 'drop') %>%
    select(PA, AB, BA, OBP, SLG, OPS, OPS_plus, wOBA, xBA, xwOBAcon, BABIP, HR, H, K, K., BB, BB.,
           `Hard Hit %`,`Barrel %`) %>%
    distinct() %>% 
    rename("BB%" = "BB.", "K%" = "K.", "OPS+" = "OPS_plus")
  
  kable(tab, row.names = F) %>%
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
              HBP = sum(events == "HBP"),
              K = sum(events == "Strike Out"),
              H = sum(events %in% c("Single","Double","Triple","Home Run")),
              HR = sum(events == "Home Run"),
              Single = sum(events == "Single"),
              Double = sum(events == "Double"),
              Triple = sum(events == "Triple"),
              SF = sum(events == "Sac Fly"),
              Error = sum(events == "Error"),
              SacBunt = sum(events == "Sac Bunt"),
              AB = PA - BB - HBP - SF - SacBunt,
              BIP = sum(description %in% c("hit_into_play_no_out","hit_into_play","hit_into_play_score")),
              BA = round(mean(is_hit[!events %in% c("Walk","Sac Fly","HBP","Sac Bunt")]),3),
              BABIP = round(mean(is_hit[(is_bip == 1) & (!events %in% c("Home Run"))], na.rm = T),3),
              xBA = round(mean(estimated_ba_using_speedangle[!events %in% c("Walk","HBP")]),3),
              wOBA = round((0.69*BB + 0.72*HBP + 0.89*Single + 1.27*Double + 1.62*Triple + 2.1*HR) / (AB + BB + SF + HBP),3),
              xwOBAcon = round(mean(estimated_woba_using_speedangle[is_bip == 1]),3),
              K. = round(K / PA, 3) * 100,
              BB. = round(BB / PA, 3) * 100,
              hard_hit_rate = round(mean(hard_hit[!events %in% c("Walk","Strike Out","HBP")], na.rm = T), 3) * 100,
              `Barrel %` = round(mean(is_barrel[!events %in% c("Walk","Strike Out","HBP")], na.rm = T), 3) * 100,
              .groups = 'drop') %>%
    select(p_throws, prop, BA, xBA, wOBA, xwOBAcon, BABIP, K., BB., hard_hit_rate, `Barrel %`) %>%
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
              HBP = sum(events == "HBP"),
              K = sum(events == "Strike Out"),
              H = sum(events %in% c("Single","Double","Triple","Home Run")),
              Single = sum(events == "Single"),
              Double = sum(events == "Double"),
              Triple = sum(events == "Triple"),
              HR = sum(events == "Home Run"),
              SF = sum(events == "Sac Fly"),
              Error = sum(events == "Error"),
              SacBunt = sum(events == "Sac Bunt"),
              AB = PA - BB - HBP - SF - SacBunt,
              BIP = sum(description %in% c("hit_into_play_no_out","hit_into_play","hit_into_play_score")),
              BA = round(mean(is_hit[!events %in% c("Walk","Sac Fly","HBP","Sac Bunt")]),3),
              BABIP = round(mean(is_hit[(is_bip == 1) & (!events %in% c("Home Run"))], na.rm = T),3),
              wOBA = round((0.69*BB + 0.72*HBP + 0.89*Single + 1.27*Double + 1.62*Triple + 2.1*HR) / (AB + BB + SF + HBP),3),
              xBA = round(mean(estimated_ba_using_speedangle[!events %in% c("Walk","HBP")]),3),
              xwOBAcon = round(mean(estimated_woba_using_speedangle[is_bip == 1]),3),
              K. = round(K / PA, 3) * 100,
              BB. = round(BB / PA, 3) * 100,
              hard_hit_rate = round(mean(hard_hit[!events %in% c("Walk","Strike Out","HBP")], na.rm = T), 3) * 100,
              `Barrel %` = round(mean(is_barrel[!events %in% c("Walk","Strike Out","HBP")], na.rm = T), 3) * 100,
              .groups = 'drop') %>%
    select(stand, prop, BA, xBA, wOBA, xwOBAcon, BABIP, K., BB., hard_hit_rate, `Barrel %`) %>%
    rename("BB%" = "BB.", "K%" = "K.", "Batter" = "stand", 
           "% Faced" = "prop", "Hard Hit %" = "hard_hit_rate")
  
  kable(tab, row.names = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
}

pitcher_basic_stats <- function(data, title)
{
  #IP <- data %>%
  #select(game_date, outs_when_up, inning) %>% distinct() %>%
  #group_by(game_date) %>% 
  #summarise(n = n() / 3, .groups = "drop") %>% select(n) %>% sum()
  
  IP <- round(sum(data$is_out) / 3,2)
  
  #IP <- data %>% dplyr::select(game_date, outs_when_up, inning) %>% distinct() %>% nrow() / 3
  #IP <- data %>%
  #mutate(inning2 = case_when(outs_when_up == 0 ~ inning - 1,
  #outs_when_up == 1 ~ (inning - 1) + (1/3),
  #TRUE ~ inning)) %>%
  #tidyr::unite(inning3, inning, outs_when_up, sep = ".", remove = FALSE) %>%
  #group_by(game_date) %>% 
  #summarise(IP = n_distinct(inning3) / 3, .groups = "drop") %>% 
  #ungroup() %>% select(IP) %>% pull() %>% sum()
  
  ERA <- data %>%
    group_by(game_date) %>% 
    mutate(runs_scored_home = abs(home_score - lag(home_score)),
           runs_scored_away = abs(away_score - lag(away_score))) %>%
    #mutate(runs_scored_home = tidyr::replace_na(runs_scored_home, 0),
    #runs_scored_away = tidyr::replace_na(runs_scored_away, 0)) %>% 
    mutate(runs_allowed = case_when(team_name != home_team_name ~ runs_scored_home, 
                                    TRUE ~ runs_scored_away)) %>%
    #filter(events2 != "Error") %>% 
    group_by(1) %>% 
    summarise(ERA = round(9 * (sum(runs_allowed, na.rm = T) / IP),2), .groups = "drop") %>% 
    select(ERA) %>% pull()
  
  tab <- data %>% filter(!is.na(events)) %>%
    summarize(PA = n(),
              IP = round(IP,2), 
              ERA = ERA,
              BB = sum(events == 'Walk'),
              HR = sum(events == "Home Run"),
              K = sum(events == "Strike Out"),
              SF = sum(events == "Sac Fly"),
              HBP = sum(events == "HBP"),
              SB = sum(events == "Sac Bunt"),
              AB = PA - BB - HBP - SF - SB,
              Single = sum(events %in% c("Single")),
              Double = sum(events %in% c("Double")),
              Triple = sum(events %in% c("Triple")),
              H = sum(events %in% c("Single","Double","Triple","Home Run")),
              WHIP = round((BB + H) / IP,3),
              BIP = sum(description %in% c("hit_into_play_no_out","hit_into_play","hit_into_play_score")),
              BA = round(mean(is_hit[!events %in% c("Walk","Sac Fly","HBP","Sac Bunt")]),3),
              xBA = round(mean(estimated_ba_using_speedangle[!events %in% c("Walk","HBP")]),3),
              BABIP = round(mean(is_hit[(is_bip == 1) & (!events %in% c("Home Run"))], na.rm = T),3),
              wOBA = round((0.69*BB + 0.72*HBP + 0.89*Single + 1.27*Double + 1.62*Triple + 2.1*HR) / (AB + BB + SF + HBP),3),
              xwOBAcon = round(mean(estimated_woba_using_speedangle[is_bip == 1]),3),
              #xwOBA = round(4.745763 * (xwOBAcon + 0.69*BB + 0.72*HBP) / (AB + BB + SF + HBP),3),
              #xwOBA2 = round((0.69*BB + 0.72*HBP + 0.89*Single + 1.27*Double + 1.62*Triple + 2.1*HR) / (AB + BB + SF + HBP),3),
              xBA = round(mean(estimated_ba_using_speedangle[!events %in% c("Walk","HBP")]),3),
              FIP = round(((13*HR + 3*(BB + HBP) - 2*K) / (IP)) + fip_c,2),
              FIPer = round(((13*HR + 3*(BB + HBP) - 2*K) / (HR + BB + K)) + fip_c,2),
              K. = round(K / PA, 3) * 100,
              BB. = round(BB / PA, 3) * 100,
              K_per9 = round((K / IP) * 9, 2),
              BB_per9 = round((BB / IP) * 9,2),
              HR_per9 = round((HR / IP) * 9,2),
              hard_hit_rate = round(mean(hard_hit[!events %in% c("Walk","Strike Out","HBP")], na.rm = T), 3) * 100,
              `Barrel %` = round(mean(is_barrel[!events %in% c("Walk","Strike Out","HBP")], na.rm = T), 3) * 100,
              .groups = 'drop') %>%
    select(IP, ERA, FIP, BA, xBA, wOBA, xwOBAcon, BABIP, K_per9, K., BB_per9, BB., HR_per9, WHIP, hard_hit_rate,`Barrel %`) %>%
    rename("BB%" = "BB.", "K%" = "K.","Hard Hit %" = "hard_hit_rate", "K/9" = "K_per9", 
           "BB/9" = "BB_per9", "HR/9" = "HR_per9") 
  
  kable(tab, row.names = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
}

stats_by_pitch_type <- function(data, title)
{
  tab <- data %>% filter(!is.na(events) & pitch_name2 != 'null', !is.na(pitch_name2), pitch_name2 != "") %>% 
    group_by(pitch_name2) %>%
    summarize(PA = n(),
              Single = sum(events %in% c("Single")),
              Double = sum(events %in% c("Double")),
              Triple = sum(events %in% c("Triple")),
              HR = sum(events %in% c("Home Run")),
              xBH = sum(events %in% c("Double","Triple","Home Run")),
              BB = sum(events == "Walk"),
              K = sum(events == "Strike Out"),
              H = sum(events %in% c("Single","Double","Triple","Home Run")),
              HBP = sum(events == "HBP"),
              Error = sum(events == "Error"),
              SF = sum(events == "Sac Fly"),
              SB = sum(events == "Sac Bunt"),
              AB = PA - BB - HBP - SF - SB,
              wOBA = round((0.69*BB + 0.72*HBP + 0.89*Single + 1.27*Double + 1.62*Triple + 2.1*HR) / (AB + BB + SF + HBP),3),
              ISO = round(mean(as.numeric(iso_value[!events %in% c("Walk","Sac Bunt", "HBP", "Sac Fly")]), na.rm = T),3),
              SLG = round((1*Single + 2*Double + 3*Triple + 4*HR) / AB,3),
              xwOBAcon = round(mean(estimated_woba_using_speedangle[is_bip == 1]),3),
              xBA = round(mean(estimated_ba_using_speedangle[!events %in% c("Walk","HBP")]),3),
              BA = round(mean(is_hit[!events %in% c("Walk","Sac Fly","HBP","Sac Bunt")]),3),
              hard_hit_rate = round(mean(hard_hit[!events %in% c("Walk","Strike Out","HBP")], na.rm = T), 3) * 100,
              `Barrel %` = round(mean(is_barrel[!events %in% c("Walk","Strike Out","HBP")], na.rm = T), 3) * 100,
              N = n(), 
              perc_seen = round(N / nrow(data %>% filter(!is.na(events) & pitch_name2 != 'null', !is.na(pitch_name2), pitch_name2 != "")),3)*100,
              .groups = 'drop') %>%
    select(pitch_name2, perc_seen, BA, xBA, SLG, ISO, wOBA, xwOBAcon, hard_hit_rate,`Barrel %`,K, H, xBH, HR) %>%
    arrange(-perc_seen) %>%
    rename("Pitch Type" = "pitch_name2", "Hard Hit %" = "hard_hit_rate",
           "Pitch Distribution" = "perc_seen") %>%
    mutate_if(is.numeric , tidyr::replace_na, replace = 0)
  
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
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=17,face = 'bold')) + 
    scale_y_continuous(labels=scales::percent) + 
    geom_text(aes(label = paste0(freq * 100, "%"), y = freq + 0.11), size = 4.4) + 
    geom_text(aes(label = paste0(freq * 100, "%"), y = freq + 0.12), size = 4.41) + 
    theme(axis.text.x=element_text(vjust = .5, size=12,colour="#535353",face="bold")) + 
    theme(axis.text.y=element_text(size=12,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) + 
    theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0)) + 
    theme(strip.text = element_text(face="bold", size=11),
          strip.background = element_rect(fill="lightblue", colour="black"),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm'))
}

#Acceptable team names for the argument#
#BAL,BOS,NYY,TB,TOR,ATL,MIA,NYM,PHI,WSH
#CLE,CWS,DET,KC,MIN,CHC,CIN,MIL,PIT,STL
#HOU,LAA,OAK,SEA,TEX,ARI,COL,LAD,SD,SF

#Acceptable throwing_hand arguments#
#L,R

#Acceptable pitch_type arguments#
#ALL,FF,SI,CH,CU,FC,SL
spin_direction_leaderboard <- function(data, num_pitches, pitch_type = "ALL",throwing_hand="All",team) {
  if(missing(team)) {
    if (throwing_hand != "All") {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=",season,"&min=",num_pitches,"&sort=9&sortDir=asc&pitch_type=",pitch_type,"&throws=",throwing_hand,"&playerName=&team=&csv=true")
    } else {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=",season,"&min=",num_pitches,"&sort=9&sortDir=asc&pitch_type=",pitch_type,"&throws=&playerName=&team=&csv=true")
    } 
  }
  else {
    if (throwing_hand != "All") {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=",season,"&min=",num_pitches,"&sort=9&sortDir=asc&pitch_type=",pitch_type,"&throws=",throwing_hand,"&playerName=&team=",team,"&csv=true")
    } else {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=",season,"&min=",num_pitches,"&sort=9&sortDir=asc&pitch_type=",pitch_type,"&throws=&playerName=&team=",team,"&csv=true")
    } 
  }
  payload <- read_csv(url)
  payload <- payload %>% dplyr::filter(player_id == unique(data$pitcher)) %>% 
    dplyr::mutate(year = 2021)
  return(payload)
}

plot_spin_axis <- function(data, date1, date2)
{
  spin_dir <- spin_direction_leaderboard(data, num_pitches = 1, team = unique(data$team))
  spin_dir <- spin_dir %>% 
    mutate(hawkeye_measured_clock_minutes = hawkeye_measured_clock_minutes / 60, 
           movement_inferred_clock_minutes = movement_inferred_clock_minutes / 60, 
           hawkeye_measured_clock_minutes = case_when((hawkeye_measured_clock_minutes > 12 & hawkeye_measured_clock_minutes < 13) ~ hawkeye_measured_clock_minutes - 12, 
                                                      TRUE ~ hawkeye_measured_clock_minutes), 
           movement_inferred_clock_minutes = case_when((movement_inferred_clock_minutes > 12 & movement_inferred_clock_minutes < 13) ~ movement_inferred_clock_minutes - 12, 
                                                       TRUE ~ movement_inferred_clock_minutes))
  
  spin1 <- format(as.POSIXct(spin_dir$hawkeye_measured_clock_label, format = '%H:%M'), "%H:%M")
  spin2 <- format(as.POSIXct('00:00', format = '%H:%M'), "%H:%M")
  
  spin_dir$spin_axis <- difftime(as.POSIXct(spin1, format = "%H:%M"), as.POSIXct(spin2, format = "%H:%M"), units = "min")
  spin_dir$spin_axis <- ((spin_dir$spin_axis / (60*12)) * 360) + 180
  spin_dir$spin_axis <- ifelse(spin_dir$spin_axis >= 360, spin_dir$spin_axis - 360, spin_dir$spin_axis)
  
  plot1 <- ggplot(spin_dir, aes(x = hawkeye_measured_clock_minutes, y = n_pitches, fill = api_pitch_name)) +
    geom_bar(stat = "identity", width = 0.5)
  if (unique(spin_dir$pitch_hand) == "R")
  {
    plot1 <- plot1 + coord_polar()
  }else 
  {
    plot1 <- plot1 + coord_polar()
  }
  plot1 <- plot1 + scale_x_continuous(limits = c(0,12),
                                      breaks = seq(0, 12, by = 1),
                                      minor_breaks = seq(0, 12, by = .50)) + 
    ylab("Frequency") + xlab("Spin Direction") + scale_fill_discrete(name = "Pitch Type") + 
    ggtitle(paste0(unique(data$player_name), " Measured Spin Direction ", date1, "-", date2)) + 
    theme(plot.title=element_text(hjust=0.5,vjust=0,size=15,face = 'bold'),
          plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 9)) +
    theme(axis.text.x=element_text(vjust = .5, size=15,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=15,colour="#535353",face="bold")) +
    theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) +
    theme(panel.background = element_rect(fill = "white")) +
    theme(strip.text = element_text(face="bold", size=13),
          strip.background = element_rect(fill="black", colour="black",size=1),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm'))
  
  plot2 <- ggplot(spin_dir, aes(x = movement_inferred_clock_minutes, y = n_pitches, fill = api_pitch_name)) +
    geom_bar(stat = "identity", width = 0.5)
  if (unique(spin_dir$pitch_hand) == "R")
  {
    plot2 <- plot2 + coord_polar()
  }else 
  {
    plot2 <- plot2 + coord_polar()
  }
  plot2 <- plot2 + scale_x_continuous(limits = c(0,12),
                                      breaks = seq(0, 12, by = 1),
                                      minor_breaks = seq(0, 12, by = .5)) + 
    ylab("Frequency") + xlab("Spin Direction") + scale_fill_discrete(name = "Pitch Type") + 
    ggtitle(paste0(unique(data$player_name), " Observed Spin Direction ", date1, "-", date2)) + 
    theme(plot.title=element_text(hjust=0.5,vjust=0,size=15,face = 'bold'),
          plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 9)) +
    theme(axis.text.x=element_text(vjust = .5, size=15,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=15,colour="#535353",face="bold")) +
    theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) +
    theme(panel.background = element_rect(fill = "white")) +
    theme(strip.text = element_text(face="bold", size=13),
          strip.background = element_rect(fill="black", colour="black",size=1),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm'))
  
  plot3 <- grid.arrange(plot1, plot2, nrow = 1, ncol = 2)
  return(plot3)
}

release_position <- function(data, title)
{
  lowerx <- round(min(data$release_pos_x, na.rm = T), 0) - 0.5
  upperx <- round(max(data$release_pos_x, na.rm = T), 0) + 0.5
  lowerz <- round(min(data$release_pos_z, na.rm = T), 0) - 0.5
  upperz <- round(max(data$release_pos_z, na.rm = T), 0) + 0.5
  plot1 <- ggplot(data = data) + 
    geom_point(aes(x = release_pos_x, y = release_pos_z, color = pitch_name2)) + 
    labs(color = "Pitch Type",
         title = paste(unique(str_split(data$player_name, " "))[[1]][-1], title)) + 
    xlab("Release Side (feet)") + 
    ylab("Release Height (feet)") + labs(color = "Pitch Type") + 
    theme(plot.title = element_text(hjust = 0.5, size=12, face = "bold")) + 
    scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, by = 1)) + 
    scale_y_continuous(limits = c(0, 6.5), breaks = seq(0, 6.5, by = 1)) + 
    theme(legend.position = "none") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=17,face = 'bold')) + 
    theme(axis.text.x=element_text(vjust = .5, size=15,colour="#535353",face="bold")) + 
    theme(axis.text.y=element_text(size=15,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) + 
    theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm'))
  plot2 <- ggplot(data = data) + 
    geom_point(aes(x = release_pos_x, y = release_pos_z, color = pitch_name2)) + 
    labs(color = "Pitch Type",
         title = paste(unique(data$player_name), title)) + 
    xlab("Release Side (feet)") + 
    ylab("Release Height (feet)") + labs(color = "Pitch Type") +
    scale_x_continuous(limits = c(lowerx, upperx), breaks = seq(lowerx, upperx, 0.5)) +
    scale_y_continuous(limits = c(lowerz, upperz), breaks = seq(lowerz, upperz, 0.25)) +
    theme(plot.title = element_text(hjust = 0.5, size=12, face = "bold")) + 
    theme(plot.title = element_text(hjust = 0.5, vjust=0,size=17,face = 'bold')) + 
    theme(axis.text.x=element_text(vjust = .5, size=15,colour="#535353",face="bold")) + 
    theme(axis.text.y=element_text(size=15,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=15,colour="#535353",face="bold",vjust=1.5)) + 
    theme(axis.title.x=element_text(size=15,colour="#535353",face="bold",vjust=0),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm'))
  return(grid.arrange(plot1, plot2, nrow = 1, ncol = 2))
}

clean_statcast_data <- function(data, start_year, end_year)
{
  data2 <- data[[1]] %>% 
    filter(pitch_name != "Intentional Ball", pitch_name != "Pitch Out") %>%
    add_team_names(start_year = start_year, end_year = end_year) %>%
    mutate(mlbam_id = data[[2]],
           player_name = case_when(pitcher == 607074 & endsWith(player_name, "n, Carlos") ~ "Rodon, Carlos",
                                   pitcher == 527048 & grepl("rez, Mart", unique(player_name)) ~ "Perez, Martin",
                                   batter == 643289 & endsWith(player_name, "n, Mauricio") ~ "Dubon, Mauricio",
                                   batter == 670768 & endsWith(player_name, "lez, Luis") ~ "Gonzalez, Luis",
                                   batter == 553993 & endsWith(player_name, "rez, Eugenio") ~ "Suarez, Eugenio",
                                   TRUE ~ player_name),
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
           events2 = case_when(events %in% c("Double","Triple","Home Run") ~ "XBH", 
                               events %in% c("Double Play","Field Out","Force Out","Sac Bunt","Sac Fly") ~ "Field Out",
                               TRUE ~ events),
           p_throws = case_when(p_throws == 'R' ~ 'RHP',
                                p_throws == 'L' ~ 'LHP'),
           stand = case_when(stand == 'R' ~ 'RHB',
                             stand == 'L' ~ 'LHB'),
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
    mutate(hc_x_ = 2.495671 * (hc_x - 125),
           hc_y_ = 2.495671 * (199 - hc_y)) %>%
    mutate(first_name = str_squish(strsplit(player_name, split = ",")[[1]][2]),
           last_name = str_squish(strsplit(player_name, split = ",")[[1]][1])) %>% 
    tidyr::unite(player_name, c("first_name", "last_name"), sep = " ") %>%
    mutate(x = hc_x - 125.42, 
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
                                       TRUE ~ NA_character_)) %>%
    mutate_at(vars(estimated_ba_using_speedangle, estimated_woba_using_speedangle, 
                   woba_value, woba_denom, babip_value, iso_value), ~replace(., is.na(.), 0)) %>% 
    left_join(player_zones, by = "mlbam_id") %>% 
    mutate(person_strike_zone_top = case_when(!is.na(person_strike_zone_top) ~ person_strike_zone_top, TRUE ~ 3.5),
           person_strike_zone_bottom = case_when(!is.na(person_strike_zone_bottom) ~ person_strike_zone_bottom, TRUE ~ 1.55),
           in_zone = case_when((plate_z <= person_strike_zone_top & plate_z >= person_strike_zone_bottom & abs(plate_x) <= 0.95) ~ 1, TRUE ~ 0))
}

heat_map <- function(data, var, title, binary, legend_title)
{
  if (!is.na(unique(data$person_strike_zone_top)) & !is.na(unique(data$person_strike_zone_bottom)))
  {
    if (length(unique(data$batter)) == 1)
    {
      topKzone <- unique(data$person_strike_zone_top)
      botKzone <- unique(data$person_strike_zone_bottom)
    } else {
      topKzone <- 3.5
      botKzone <- 1.55
    }
  } else {
    topKzone <- 3.5
    botKzone <- 1.55
  }
  if (unique(data$player_name) == "Jose Altuve")
  {
    topKzone <- 3.15
    botKzone <- 1.2
  }
  inKzone <- -0.9
  outKzone <- 0.9
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone))
  
  if (binary)
  {
    fit <- gam(as.formula(paste0(var, " ~ ", "s(plate_x, plate_z)")), method = "REML", family = binomial, data=data)
    x <- seq(-1.7, 1.7, length.out=50)
    y <- seq(0.5, 4.5, length.out=50)
    data.predict <- data.frame(plate_x = c(outer(x, y * 0 + 1)),
                               plate_z = c(outer(x * 0 + 1, y)))
    lp <- predict(fit, data.predict)
    data.predict$Prob <- exp(lp) / (1 + exp(lp))
  }
  else {
    fit <- gam(as.formula(paste0(var, " ~ ", "s(plate_x, plate_z)")), method = "REML", data=data)
    x <- seq(-1.7, 1.7, length.out=50)
    y <- seq(0.5, 4.5, length.out=50)
    data.predict <- data.frame(plate_x = c(outer(x, y * 0 + 1)),
                               plate_z = c(outer(x * 0 + 1, y)))
    lp <- predict(fit, data.predict)
    data.predict$Prob <- lp
    
  }
  min <- min(data.predict$Prob)
  max <- max(data.predict$Prob)
  ggplot(kZone, aes(x, y)) +
    geom_tile(data=data.predict, 
              aes(plate_x, plate_z, fill = Prob)) +
    scale_fill_distiller(palette = "Spectral", limits = c(min,max), breaks=seq(round(min,1), round(max,1), (round(max,1) - round(min,1)) / 5)) +
    #geom_path(lwd=1.5, col="black") +
    #add_zone("black") + 
    geom_path(aes(.data$x, .data$y), data=kZone, lwd=1.5, col="black") + 
    coord_fixed() + ylim(0.5, 4.5) + labs(fill = legend_title) + 
    ggtitle(paste(unique(data$player_name), title)) + 
    # unlist(strsplit(unique(data$player_name), " "))[2]
    xlab("Feet From Homeplate (Pitcher's Perspective)") + 
    ylab("Feet Above Homeplate") + xlim(-1.7, 1.7) + centertitle() + 
    theme(panel.background = element_rect(fill = "white")) +
    theme(plot.title = element_text(color = "black", face = "bold", size = 18)) + 
    theme(axis.text.x=element_text(vjust = .5, size=12,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=12,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=12,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=12,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) + 
    theme(strip.text = element_text(face="bold", size=13),
          strip.background = element_rect(fill="lightblue", colour="black",size=1),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm'))
}

woba_heat_map_batter <- function(data, title)
{
  data2 <- split(data, data$p_throws)
  woba_plot(data2, title = paste(unique(data$player_name), title))
}
xba_heat_map_batter <- function(data, title)
{
  data2 <- data #split(data, data$p_throws)
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

heat_map_rv <- function(data, title, legend_title)
{
  data <- find_run_value(data)
  if (!is.na(unique(data$person_strike_zone_top)) & !is.na(unique(data$person_strike_zone_bottom)))
  {
    if (length(unique(data$batter)) == 1)
    {
      topKzone <- unique(data$person_strike_zone_top)
      botKzone <- unique(data$person_strike_zone_bottom)
    } else {
      topKzone <- 3.5
      botKzone <- 1.55
    }
  } else {
    topKzone <- 3.5
    botKzone <- 1.55
  }
  if (unique(data$player_name) == "Jose Altuve")
  {
    topKzone <- 3.15
    botKzone <- 1.2
  }
  inKzone <- -0.9
  outKzone <- 0.9
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone))
  control <- caret::trainControl(method = "cv", number = 5)
  fit <- caret::train(rv ~ plate_x + plate_z + pitch_name2, data = data, method = "knn", 
                      metric = "RMSE", trControl = control, tuneLength = 25)
  #print(fit)
  pitch_types = unique(data$pitch_name2)
  predict.data <- list()
  x <- seq(-1.7, 1.7, length.out=30)
  y <- seq(0.5, 4.5, length.out=30)
  for (i in 1:length(pitch_types))
  {
    predict.data[[i]] <- data.predict <- data.frame(plate_x = c(outer(x, y * 0 + 1)),
                                                    plate_z = c(outer(x * 0 + 1, y)),
                                                    pitch_name2 = pitch_types[i])
  }
  data.predict <- do.call(rbind, predict.data)
  lp <- predict(fit, data.predict)
  data.predict$RV <- lp
  
  
  ggplot(kZone, aes(x, y)) +
    geom_tile(data=data.predict, 
              aes(plate_x, plate_z, fill = RV)) +
    scale_fill_distiller(palette = "Spectral") +
    geom_path(aes(.data$x, .data$y), data=kZone, lwd=1.5, col="black") + 
    #add_zone("black") + 
    coord_fixed() + ylim(0.5, 4.5) + labs(fill = legend_title) + 
    ggtitle(paste(unique(data$player_name), title)) + 
    xlab("Feet From Homeplate (Pitcher's Perspective)") + 
    ylab("Feet Above Homeplate") + xlim(-1.7, 1.7) + centertitle() + 
    theme(panel.background = element_rect(fill = "white")) +
    theme(plot.title = element_text(color = "black", face = "bold", size = 18)) + 
    theme(axis.text.x=element_text(vjust = .5, size=12,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=12,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=12,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=12,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) + 
    theme(strip.text = element_text(face="bold", size=13),
          strip.background = element_rect(fill="lightblue", colour="black",size=1),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm')) + 
    facet_grid(~ pitch_name2)
}

heat_map_rv2 <- function(data, title, legend_title)
{
  data2 <- find_run_value(data)
  data2 <- data2 %>% 
    group_by(pitch_name2) %>% 
    mutate(num_pitches = n()) %>% 
    filter(num_pitches > 60) %>% 
    ungroup()
  unique_pitches <- unique(data2$pitch_name2)
  
  if (!is.na(unique(data$person_strike_zone_top)) & !is.na(unique(data$person_strike_zone_bottom)))
  {
    if (length(unique(data$batter)) == 1)
    {
      topKzone <- unique(data$person_strike_zone_top)
      botKzone <- unique(data$person_strike_zone_bottom)
    } else {
      topKzone <- 3.5
      botKzone <- 1.55
    }
  } else {
    topKzone <- 3.5
    botKzone <- 1.55
  }
  if (unique(data$player_name) == "Jose Altuve")
  {
    topKzone <- 3.15
    botKzone <- 1.2
  }
  inKzone <- -0.9
  outKzone <- 0.9
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone))
  x <- seq(-1.7, 1.7, length.out=50)
  y <- seq(0.5, 4.5, length.out=50)
  
  full_predict_df <- list()
  for (i in 1:length(unique_pitches))
  {
    data3 <- data2 %>% filter(pitch_name2 == unique_pitches[i])
    fit <- gam(rv ~ s(plate_x, plate_z), data=data3)
    data.predict <- data.frame(plate_x = c(outer(x, y * 0 + 1)),
                               plate_z = c(outer(x * 0 + 1, y)))
    data.predict$RV <- predict(fit, data.predict)
    data.predict$pitch_name2 <- unique_pitches[i]
    full_predict_df[[i]] <- data.predict
  }
  df <- do.call(rbind, full_predict_df)
  
  ggplot(kZone, aes(x, y)) +
    geom_tile(data=df, 
              aes(plate_x, plate_z, fill = RV)) +
    scale_fill_distiller(palette = "Spectral") +
    geom_path(aes(.data$x, .data$y), data=kZone, lwd=1.5, col="black") + 
    #add_zone("black") + 
    coord_fixed() + ylim(0.5, 4.5) + labs(fill = legend_title) + 
    ggtitle(paste(unique(data$player_name), title)) + 
    xlab("Feet From Homeplate (Pitcher's Perspective)") + 
    ylab("Feet Above Homeplate") + xlim(-1.7, 1.7) + centertitle() + 
    theme(panel.background = element_rect(fill = "white")) +
    theme(plot.title = element_text(color = "black", face = "bold", size = 18)) + 
    theme(axis.text.x=element_text(vjust = .5, size=12,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=12,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=12,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=12,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) + 
    theme(strip.text = element_text(face="bold", size=13),
          strip.background = element_rect(fill="lightblue", colour="black",size=1),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm')) +
    facet_grid(~ pitch_name2)
}

find_run_value <- function(data)
{
  data2 <- data %>%
    filter(pitch_name2 != "null", pitch_name2 != "", !is.na(pitch_name2)) %>%
    mutate(count_after = lag(count)) %>% 
    inner_join(rv_non_bip, by = "count") %>%
    rename("rvnon_bip_before" = "rvnon_bip") %>% 
    inner_join(rv_non_bip, by = c("count_after" = "count")) %>%
    rename("rvnon_bip_after" = "rvnon_bip") %>% 
    left_join(rv_bip, by = "events") %>% 
    mutate(rvnon_bip = rvnon_bip_after - rvnon_bip_before) %>% 
    mutate(rv = coalesce(rvbip, rvnon_bip))
  return(data2)
}

run_value_table <- function(data, title)
{
  data2 <- find_run_value(data)
  tab <- data2 %>% 
    group_by(pitch_name2) %>% 
    summarise(total_rv = round(sum(rv, na.rm = T),1),
              Pitches = n(),
              Perc_Seen = round((Pitches / nrow(data)) * 100,1),
              rv_per_100 = round((total_rv / Pitches) * 100,1), 
              .groups = "drop") %>%
    dplyr::select(pitch_name2, Pitches, Perc_Seen, total_rv, rv_per_100) %>%
    rename("RV" = "total_rv", "RV Per 100" = "rv_per_100", "Pitch Type" = "pitch_name2",
           "%" = "Perc_Seen") %>%
    arrange(-Pitches)
  
  kable(tab, row.names = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
}

pitch_chart_batter_chase <- function(data, title)
{
  update_geom_defaults("point",list(size=4))
  zone <- create_strikezone(data)
  data2 <- data %>% filter(pitch_name2 != 'null', in_zone == 0, is_swing == 1) 
  zone + geom_point(data = data2, aes(x = plate_x, y = plate_z, color = pitch_name2)) +
    guides(colour = guide_legend(override.aes = list(size=3))) + 
    labs(color = "Pitch Type",
         title = paste(unique(data$player_name), title)) + 
    ylab("Feet Above Homeplate") +
    xlab("Feet From Homeplate (Pitcher's Perspective)") +
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
    facet_grid(~ p_throws) + 
    theme(strip.text = element_text(face="bold", size=13),
          strip.background = element_rect(fill="lightblue", colour="black",size=1))
}

# level: team, player
# stat_type (batting): standard, advanced, value, sabermetric, ratio, pitches
# stat_type (pitching): standard, value, advanced, ratio
# stat_type (fielding): standard
# area: batting, pitching
baseball_reference_scraper <- function(level, stat_type, area, start_year, end_year)
{
  final_df <- data.frame()
  for (season in start_year:end_year)
  {
    url <- paste0("https://www.baseball-reference.com/leagues/majors/",season,"-", stat_type,"-" , area,".shtml")
    
    if (level == "team")
    {
      df <- url %>% 
        xml2::read_html() %>% 
        html_nodes("table") %>% 
        html_table()
      df <- df[[1]]
      if (stat_type %in% c("advanced"))
      {
        colnames(df) <- df[1, ]
      }
      df <- df %>% 
        filter(!Tm %in% c("Tm","League Average","")) %>% 
        mutate(Season = season)
    }
    else {
      parser <- xml2::read_html(url)
      b <- readLines(url)
      c <- paste0(b, collapse = "")
      d <- as.character(unlist(stringi::stri_extract_all_regex(c, '<table(.*?)/table>', omit_no_match = T, simplify = T)))
      df <- html_table(xml2::read_html(d[2]))[[1]]
      rm(d)
      rm(c)
      rm(b)
      if (stat_type == "advanced")
      {
        colnames(df) <- df[1, ]
      }
      df <- df %>% filter(Name != "Name", Name != "LgAvg per 180 IP", Name != "LgAvg per 600 PA") %>% 
        mutate(Name = str_replace_all(Name, "[^[:alnum:]]", " "),
               Season = season)
    }
    final_df <- rbind(final_df, df)
  }
  return(final_df)
}

plate_discipline_by_pitch_type <- function(data, title)
{
  tab <- data %>%
    filter(pitch_name2 != "" & pitch_name2 != "null" & !is.na(pitch_name2)) %>% 
    group_by(pitch_name2) %>% 
    summarise(`Pitch %` = round(n() / nrow(data) * 100,1),
              `O-Swing%` = round(sum(is_swing == 1 & in_zone == 0) /  sum(in_zone == 0) * 100,1),
              `Z-Swing%` = round(sum(is_swing == 1 & in_zone == 1) /  sum(in_zone == 1) * 100,1),
              `Swing%` = round(sum(is_swing == 1) /  n() * 100,1),
              `O-Contact%` = round(sum(is_contact == 1 & in_zone == 0) /  sum(in_zone == 0 & is_swing == 1) * 100,1),
              `Z-Contact%` = round(sum(is_contact == 1 & in_zone == 1) /  sum(in_zone == 1 & is_swing == 1) * 100,1),
              `Contact%` = round(sum(is_contact == 1) /  sum(is_swing == 1) * 100,1),
              `Zone%` = round(sum(in_zone == 1) /  n() * 100,1),
              `SwStr%` = round(sum(is_whiff == 1) /  sum(in_zone == 1 | description %in% c("swinging_strike","called_strike","swinging_strike_blocked","foul","foul_bunt","foul_tip","missed_bunt")) * 100,1),
              `CStr%`= round(sum(description %in% c("called_strike")) /  n() * 100,1),
              `CSW%` = round(sum(description %in% c("called_strike","swinging_strike", "swinging_strike_blocked"," missed_bunt")) /  n() * 100,1)) %>% 
    rename(`Pitch Type` = "pitch_name2") %>% arrange(-`Pitch %`) %>% 
    mutate_if(is.numeric , tidyr::replace_na, replace = 0)
  
  kable(tab, row.names = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
}

plate_discipline <- function(data, title)
{
  tab <- data %>% 
    summarise(`O-Swing%` = round(sum(is_swing == 1 & in_zone == 0) /  sum(in_zone == 0) * 100,1),
              `Z-Swing%` = round(sum(is_swing == 1 & in_zone == 1) /  sum(in_zone == 1) * 100,1),
              `Swing%` = round(sum(is_swing == 1) /  n() * 100,1),
              `O-Contact%` = round(sum(is_contact == 1 & in_zone == 0) /  sum(in_zone == 0 & is_swing == 1) * 100,1),
              `Z-Contact%` = round(sum(is_contact == 1 & in_zone == 1) /  sum(in_zone == 1 & is_swing == 1) * 100,1),
              `Contact%` = round(sum(is_contact == 1) /  sum(is_swing == 1) * 100,1),
              `Zone%` = round(sum(in_zone == 1) /  n() * 100,1),
              `SwStr%` = round(sum(is_whiff == 1) /  sum(in_zone == 1 | description %in% c("swinging_strike", "called_strike", "swinging_strike_blocked","foul","foul_bunt","foul_tip","missed_bunt")) * 100,1),
              `CStr%`= round(sum(description %in% c("called_strike")) /  n() * 100,1),
              `CSW%` = round(sum(description %in% c("called_strike","swinging_strike", "swinging_strike_blocked"," missed_bunt")) /  n() * 100,1)) %>% 
    mutate_if(is.numeric , tidyr::replace_na, replace = 0)
  
  kable(tab, row.names = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                        "responsive"), full_width = F, 
                  position = "left", fixed_thead = T) %>%
    footnote(symbol = paste(unique(data$player_name), title), title_format = c("bold","underline"))
}

heat_map_by_pitch_type <- function(data, var, title, binary, legend_title)
{
  data <- data %>% filter(!is.na(pitch_name2), pitch_name2 != "", pitch_name2 != "null") %>%
    ungroup() %>% group_by(pitch_name2) %>% 
    mutate(`Pitch Number` = n()) %>% 
    filter(`Pitch Number` >= 20) %>% select(-`Pitch Number`) %>% 
    mutate(pitch_name2 = as.factor(pitch_name2))
  
  if (!is.na(unique(data$person_strike_zone_top)) & !is.na(unique(data$person_strike_zone_bottom)))
  {
    if (length(unique(data$batter)) == 1)
    {
      topKzone <- unique(data$person_strike_zone_top)
      botKzone <- unique(data$person_strike_zone_bottom)
    } else {
      topKzone <- 3.5
      botKzone <- 1.55
    }
  } else {
    topKzone <- 3.5
    botKzone <- 1.55
  }
  if (unique(data$player_name) == "Jose Altuve")
  {
    topKzone <- 3.15
    botKzone <- 1.2
  }
  inKzone <- -0.9
  outKzone <- 0.9
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone))
  
  if (binary)
  {
    fit <- gam(as.formula(paste0(var, " ~ ", "pitch_name2 + s(plate_x, plate_z, by = pitch_name2)")), method = "REML", family = binomial, data=data)
    x <- seq(-1.7, 1.7, length.out=50)
    y <- seq(0.5, 4.5, length.out=50)
    data.predict <- data.frame(plate_x = c(outer(x, y * 0 + 1)),
                               plate_z = c(outer(x * 0 + 1, y)))
    data.predict <- data.predict[rep(seq_len(nrow(data.predict)), length(unique(data$pitch_name2))), ]
    data.predict$pitch_name2 <- sort(rep(unique(data$pitch_name2), 2500))
    lp <- predict(fit, data.predict)
    data.predict$Prob <- exp(lp) / (1 + exp(lp))
  }
  else {
    fit <- gam(as.formula(paste0(var, " ~ ", "pitch_name2 + s(plate_x, plate_z, by = pitch_name2)")),  method = "REML", data=data)
    x <- seq(-1.7, 1.7, length.out=50)
    y <- seq(0.5, 4.5, length.out=50)
    data.predict <- data.frame(plate_x = c(outer(x, y * 0 + 1)),
                               plate_z = c(outer(x * 0 + 1, y)))
    data.predict <- data.predict[rep(seq_len(nrow(data.predict)), length(unique(data$pitch_name2))), ]
    data.predict$pitch_name2 <- sort(rep(unique(data$pitch_name2), 2500))
    lp <- predict(fit, data.predict)
    data.predict$Prob <- lp
  }
  min <- min(data.predict$Prob)
  max <- max(data.predict$Prob)
  ggplot(kZone, aes(x, y)) +
    geom_tile(data=data.predict, 
              aes(plate_x, plate_z, fill = Prob, group = pitch_name2)) +
    scale_fill_distiller(palette = "Spectral", limits = c(min,max), breaks=seq(round(min,1), round(max,1), (round(max,1) - round(min,1)) / 5)) +
    #geom_path(lwd=1.5, col="black") +
    #add_zone("black") + 
    geom_path(aes(.data$x, .data$y), data=kZone, lwd=1.5, col="black") + 
    coord_fixed() + ylim(0.5, 4.5) + labs(fill = legend_title) + 
    ggtitle(paste(unique(data$player_name), title)) + 
    # unlist(strsplit(unique(data$player_name), " "))[2]
    xlab("Feet From Homeplate (Pitcher's Perspective)") + 
    ylab("Feet Above Homeplate") + xlim(-1.7, 1.7) + centertitle() + 
    theme(panel.background = element_rect(fill = "white")) +
    theme(plot.title = element_text(color = "black", face = "bold", size = 18)) + 
    theme(axis.text.x=element_text(vjust = .5, size=12,colour="#535353",face="bold")) +
    theme(axis.text.y=element_text(size=12,colour="#535353",face="bold")) + 
    theme(axis.title.y=element_text(size=12,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=12,colour="#535353",face="bold",vjust=0)) +
    theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
    theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)) + 
    theme(strip.text = element_text(face="bold", size=13),
          strip.background = element_rect(fill="lightblue", colour="black",size=1),
          legend.title = element_text(face = "bold", size = 15),
          legend.text = element_text(face = "bold", size = 12),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.7, 'cm')) + 
    facet_grid(~ pitch_name2)
}

spray_chart_distributions <- function(data, title)
{
  data2 <- data %>% filter(!is.na(spray_angle_bin), is_bip == 1) %>%
    group_by(player_name, spray_angle_bin) %>% 
    summarise(Count = n(),
              Year = paste0(unique(game_year), collapse = "-"),
              .groups = "drop") %>% 
    ungroup() %>% 
    mutate(Total = sum(Count),
           `Spray %` = Count / Total) %>% 
    mutate_if(is.numeric, list(~tidyr::replace_na(., 0))) %>% 
    mutate(`Spray %` = round(`Spray %`, 3) * 100)
  
  lines <- data.frame(x_start = c(0, 0, sqrt(90^2 / 2), -sqrt(90^2 / 2)),
                      x_end = c(-250, 250, 0, 0),
                      y_start = c(0, 0, sqrt(90^2 / 2), sqrt(90^2 / 2)),
                      y_end = c(250, 250, sqrt(90^2 / 2) * 2, sqrt(90^2 / 2) * 2))
  infield <- data.frame(x = c(0, -sqrt(70^2 / 2), 0, sqrt(70^2 / 2)),
                        y = c(10, sqrt(90^2 / 2), sqrt(80^2 / 2) * 2, sqrt(90^2 / 2)))
  dirt <- data.frame(x = c(0, -sqrt(120^2 / 2), 0, sqrt(120^2 / 2)),
                     y = c(0, sqrt(120^2 / 2), sqrt(120^2 / 2) * 2, sqrt(120^2 / 2)))
  spray_lines <- data.frame(x_end = c(-175, -100, 0, 100, 175),
                            x_start = rep(0, 5),
                            y_start = rep(0, 5), 
                            y_end = c(305,370,404,370,305))
  
  ggplot(data = data2) + geom_polygon(data = park_dim, aes(x = X_START, y = Y_START), fill = '#31a354', alpha = 0.3) +
    geom_polygon(data = dirt, aes(x = x, y = y), fill = 'orange') +
    geom_polygon(data = infield, aes(x = x, y = y), fill = '#31a354') +
    geom_segment(data = lines, aes(x = x_start, y = y_start, xend = x_end, yend = y_end), color = "white", size = 1.2) +
    geom_segment(data = spray_lines, aes(x = x_start, y = y_start, xend = x_end, yend = y_end), color = "black") + 
    labs(title = paste0(unique(data2$player_name), " Spray Chart ", " (" ,unique(data2$Total)[1], " BIP) ", title)) + 
    geom_text(x=-160, y=200, label=if_else(paste0(data2 %>% filter(spray_angle_bin == "(-45,-30]") %>% select(`Spray %`) %>% pull(), "%") == "%","0%",paste0(data2 %>% filter(spray_angle_bin == "(-45,-30]") %>% select(`Spray %`) %>% pull(), "%")), size = 8.2) + 
    geom_text(x=-105, y=250, label=if_else(paste0(data2 %>% filter(spray_angle_bin == "(-30,-15]") %>% select(`Spray %`) %>% pull(), "%") == "%", "0%",paste0(data2 %>% filter(spray_angle_bin == "(-30,-15]") %>% select(`Spray %`) %>% pull(), "%")), size = 8.2) + 
    geom_text(x=-40, y=280, label=if_else(paste0(data2 %>% filter(spray_angle_bin == "(-15,0]") %>% select(`Spray %`) %>% pull(), "%") == "%","0%",paste0(data2 %>% filter(spray_angle_bin == "(-15,0]") %>% select(`Spray %`) %>% pull(), "%")), size = 8.2) + 
    geom_text(x=40, y=280, label=if_else(paste0(data2 %>% filter(spray_angle_bin == "(0,15]") %>% select(`Spray %`) %>% pull(), "%") == "%", "0%",paste0(data2 %>% filter(spray_angle_bin == "(0,15]") %>% select(`Spray %`) %>% pull(), "%")), size = 8.2) + 
    geom_text(x=105, y=250, label=if_else(paste0(data2 %>% filter(spray_angle_bin == "(15,30]") %>% select(`Spray %`) %>% pull(), "%") == "%","0%",paste0(data2 %>% filter(spray_angle_bin == "(15,30]") %>% select(`Spray %`) %>% pull(), "%")), size = 8.2) + 
    geom_text(x=160, y=200, label=if_else(paste0(data2 %>% filter(spray_angle_bin == "(30,45]") %>% select(`Spray %`) %>% pull(), "%") == "%","0%",paste0(data2 %>% filter(spray_angle_bin == "(30,45]") %>% select(`Spray %`) %>% pull(), "%")), size = 8.2) + 
    xlim(c(-300, 300)) +
    ylim(c(-60, 500)) +
    coord_fixed() +
    theme_void() + 
    theme(plot.title=element_text(hjust=0.5,vjust=0,size=20,face = 'bold'),
          plot.subtitle=element_text(face="plain", hjust= -.015, vjust= .09, colour="#3C3C3C", size = 10))
  
}
