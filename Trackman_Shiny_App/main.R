library('baseballr')
library('ggplot2')
library('dplyr')
library('stringr')
library('kableExtra')
library("CalledStrike")
library('shiny')
library('GeomMLBStadiums')
library('patchwork')
library('readr')
library('gridExtra')
library('ggpointdensity')
library('caret')
require('mgcv')
library('lubridate')
`%!in%` = Negate(`%in%`)
start_year <- 2021
end_year <- 2021

setwd("C:/Users/RuslanDavtian/Downloads")
source("functions.R")

fg_hitters <- fg_bat_leaders(x = start_year, y = end_year, league = "all", qual = 30, ind = 1)
hitters_list <- fg_hitters %>% tidyr::separate(col = Name, into = c("First","Middle", "Last"), sep = "\\s") %>% select(First, Middle, Last) %>% distinct()
hitters_list$name <- ifelse(is.na(hitters_list$Last) == FALSE, as.character(interaction(hitters_list, sep=" ")), 
                            as.character(interaction(hitters_list[, c("First","Middle")], sep=" ")))


fg_pitchers <- fg_pitch_leaders(x = start_year, y = end_year, league = "all", qual = 20, ind = 1)
pitchers_list <- fg_pitchers %>% tidyr::separate(col = Name, into = c("First", "Middle", "Last"), sep = "\\s") %>% select(First, Middle, Last) %>% distinct()
pitchers_list$name <- ifelse(is.na(pitchers_list$Last) == FALSE, as.character(interaction(pitchers_list, sep=" ")), 
                             as.character(interaction(pitchers_list[, c("First","Middle")], sep=" ")))

source("set_up_shiny.R")
shiny <- set_up_shiny()
shinyApp(ui = shiny[[1]], server = shiny[[2]])
