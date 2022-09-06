library(stringr)
library(tidyverse)
library(baseballr)
library(lubridate)
library(ggplot2)
library(shiny)
library(kableExtra)
library(reactable)
library(DT)

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
    summarise(`RV/100` = round(mean(`RV.100`),3),
              `xRV/100` = round(mean(`xRV.100`),3),
              `wOBA/Swing Decision` = round(`RV/100` / 100 * avg_pitchPA * wOBA_scale,4),
              `Num Pitches` = n(),
              Team = max(batter_team)) %>%
    mutate(`SDS Score` = (`RV/100` - min(`RV/100`, na.rm = T)) / (max(`RV/100`, na.rm = T) - min(`RV/100`, na.rm = T)),
           `SDS Score` = round((`SDS Score` * 100)),
           `SDS Percentile` = rank(`SDS Score`)/length(`SDS Score`),
           `SDS Percentile` = round(`SDS Percentile` * 100),
           Season = unique(data$game_year)) %>% rename("Batter" = "player_name") %>% 
    select(Season, Team, Batter, `RV/100`, `xRV/100`, `wOBA/Swing Decision`, `SDS Score`, 
           `SDS Percentile`, `Num Pitches`) %>% arrange(-`RV/100`, -`SDS Percentile`) %>% 
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
    summarise(`RV/100` = round(mean(`RV.100`),3),
              `xRV/100` = round(mean(`xRV.100`),3),
              `wOBA/Swing Decision` = round(`RV/100` / 100 * avg_pitchPA * wOBA_scale,4)) %>%
    mutate(`SDS Score` = (`RV/100` - min(`RV/100`, na.rm = T)) / (max(`RV/100`, na.rm = T) - min(`RV/100`, na.rm = T)),
           `SDS Score` = round((`SDS Score` * 100)),
           `SDS Percentile` = rank(`SDS Score`)/length(`SDS Score`),
           `SDS Percentile` = round(`SDS Percentile` * 100),
           Season = unique(data$game_year)) %>% rename("Team" = "batter_team") %>% 
    select(Season, Team, `RV/100`, `xRV/100`, `wOBA/Swing Decision`, `SDS Score`, `SDS Percentile`) %>% 
    arrange(-`RV/100`, -`SDS Percentile`)
  return(data2)
} 