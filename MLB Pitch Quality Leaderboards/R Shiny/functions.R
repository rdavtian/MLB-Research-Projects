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