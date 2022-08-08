set_up_shiny <- function()
{
  ui <- fluidPage(
    titlePanel(h1("Ruslan's MLB Statcast Leaderboards", align = "center", 
                  h3("", align = "center", style = "font-size:16px;"))),
    sidebarLayout(
      sidebarPanel(width = 3, 
                   selectInput("user_area_of_game", "Area of Game", c("Choose...", Offense = "user_offense", Defense = "user_defense", Pitching = "user_pitching")),
                   conditionalPanel(
                     condition = "input.user_area_of_game == 'user_offense'",
                     selectInput("user_player_type_off", "Player Type", choices = c("Choose...", "Player", "Team")),
                     selectInput("user_leaderboard_type_off", "Leaderboard Type", choices = c("Choose...", "Exit Velo & Barrels", "Expected Stats"))
                     ),
                   conditionalPanel(
                     condition = "input.user_area_of_game == 'user_defense'",
                     selectInput("user_player_type_def", "Player Type", choices = c("Choose...", "Player", "Team")),
                     selectInput("user_leaderboard_type_def", "Leaderboard Type", choices = c("Choose...", "DRS & OAA"))
                   ),
                   conditionalPanel(
                     condition = "input.user_area_of_game == 'user_pitching'",
                     selectInput("user_player_type_pit", "Player Type", choices = c("Choose...", "Player")),
                     selectInput("user_leaderboard_type_pit", "Leaderboard Type", choices = c("Choose...", "Pitch Arsenal")),
                     conditionalPanel(
                       condition = "input.user_leaderboard_type_pit == 'Pitch Arsenal'",
                       selectInput("user_pitch_arsenal_type", "Pitch Arsenal Type", choices = c("Choose...", "Distribution", "Avg Speed", "Avg Spin"))
                       
                     ),
                     
                   ),
                   selectInput("user_season", "Season", choices = c("Choose...",2015:as.numeric(substr(Sys.Date(), 1, 4))))
      ), mainPanel(uiOutput('plot'))
    )
  )
  
  server <- function(input, output, session) 
  {
    output$plot <- renderUI({
      season <- input$user_season
      if ((input$user_area_of_game == "user_offense") & (season != "Choose..."))
      {
        if (input$user_player_type_off == "Player")
        {
          if (input$user_leaderboard_type_off == "Exit Velo & Barrels")
          {
            output$tbl <- renderDT({datatable(statcast_exit_velo_barrels(year = season, player_type = "batter"), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE)) %>% DT::formatPercentage(c("LA Sweet Spot%","Barrel%"), digits = 1)})
          } else if (input$user_leaderboard_type_off == "Expected Stats") {
            output$tbl <- renderDT({datatable(statcast_xstat(year = season, player_type = "batter"), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
          }
        } else if (input$user_player_type_off == "Team") {
          if (input$user_leaderboard_type_off == "Exit Velo & Barrels")
          {
            output$tbl <- renderDT({datatable(statcast_exit_velo_barrels(year = season, player_type = "batter-team"), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
          } else if (input$user_leaderboard_type_off == "Expected Stats") {
            output$tbl <- renderDT({datatable(statcast_xstat(year = season, player_type = "batter-team"), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
          }
        }
      } else if ((input$user_area_of_game == "user_defense") & (season != "Choose...")) {
        if (input$user_player_type_def == "Player")
        {
          if (input$user_leaderboard_type_def == "DRS & OAA")
          {
            output$tbl <- renderDT({datatable(statcast_outs_above_average(year = season, fielding_type = "player", min_field = 0), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE)) %>% DT::formatPercentage(c("Success%","Estimated Success%"), digits = 0)})
          } 
        } else if (input$user_player_type_def == "Team") {
          if (input$user_leaderboard_type_def == "DRS & OAA")
          {
            output$tbl <- renderDT({datatable(statcast_outs_above_average(year = season, fielding_type = "team", min_field = 0), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE)) %>% DT::formatPercentage(c("Success%","Estimated Success%"), digits = 0)})
          } 
        }
      }
      else if ((input$user_area_of_game == "user_pitching") & (season != "Choose...")) {
        if (input$user_player_type_pit == "Player")
        {
          if (input$user_leaderboard_type_pit == "Pitch Arsenal")
          {
            if (input$user_pitch_arsenal_type == "Distribution")
            {
              output$tbl <- renderDT({datatable(statcast_pitch_arsenal(year = season, arsenal_type = "n_"), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE)) %>% DT::formatPercentage(c("4-Seam FB%","Sinker%","Cutter%","Slider%","Changeup%","Curveball%","Splitter%"), digits = 1)})
            } else if (input$user_pitch_arsenal_type == "Avg Speed") {
              output$tbl <- renderDT({datatable(statcast_pitch_arsenal(year = season, arsenal_type = "avg_speed"), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
            } else if (input$user_pitch_arsenal_type == "Avg Spin") {
              output$tbl <- renderDT({datatable(statcast_pitch_arsenal(year = season, arsenal_type = "avg_spin"), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
            }
          }
        }
      }
    })
  }
  return(list(ui, server))
}