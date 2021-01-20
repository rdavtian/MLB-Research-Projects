library('baseballr')
library('ggplot2')
library('dplyr')
library('kableExtra')
library("CalledStrike")
library('shiny')
require('mgcv')

setwd("C:/Users/rusla/OneDrive/MLBAnalyticsJobs/Trackman_Shiny_App")
source("functions.R")

fg_hitter <- fg_bat_leaders(x = 2019, y = 2020, league = "all", qual = 70, ind = 1)
hitters_list <- fg_hitter %>% tidyr::separate(col = Name, into = c("First", "Last"), sep = "\\s") %>% select(First, Last) %>% distinct()
hitters_list$name <- as.character(interaction(hitters_list,sep=" "))

fg_pitchers <- fg_pitch_leaders(x = 2019, y = 2020, league = "all", qual = 35, ind = 1)
pitchers_list <- fg_pitchers %>% tidyr::separate(col = Name, into = c("First", "Last"), sep = "\\s") %>% select(First, Last) %>% distinct()
pitchers_list$name <- as.character(interaction(pitchers_list,sep=" "))

source("set_up_shiny.R")
shiny <- set_up_shiny()
shinyApp(ui = shiny[[1]], server = shiny[[2]])
