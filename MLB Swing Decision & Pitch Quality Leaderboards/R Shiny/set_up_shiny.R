set_up_shiny <- function(full_preds21, full_preds22)
{
  ui <- fluidPage(
    titlePanel(h1("Ruslan's MLB Swing Decision & Pitch Quality Leaderboards", align = "center", 
                  h3("", align = "center", style = "font-size:16px;"))),
    sidebarLayout(
      sidebarPanel(width = 3, 
                   selectInput("user_stat_type", "Statistic", c("Choose...", `Swing Decisions` = "user_sds", `Pitch Quality` = "user_pitch_quality")),
                   selectInput("user_season", "Season", choices = c("Choose...",2021,2022)),
                   conditionalPanel(
                     condition = "input.user_stat_type == 'user_sds'",
                     selectInput("user_player_type_sds", "Player Type", choices = c("Choose...", Batter = "user_sds_battters", Team = "user_sds_teams")),
                     conditionalPanel(
                       condition = "input.user_player_type_sds == 'user_sds_battters'",
                       sliderInput("num_pitches_sds", "Min. Pitches:", min = 0, max = 1600, value = 0, step=100),
                     ),
                   ),
                   conditionalPanel(
                     condition = "input.user_stat_type == 'user_pitch_quality'",
                     selectInput("user_player_type_pitch_quality", "Player Type", c("Choose...", Pitcher = "user_pitch_quality_pitchers", Team = "user_pitch_quality_teams")),
                     conditionalPanel(
                       condition = "input.user_player_type_pitch_quality == 'user_pitch_quality_teams'",
                       selectInput("user_pitch_type_teams", "Pitch Type", choices = c("Choose...","All","Fastball","Breaking Ball","Offspeed"))
                       ),
                       conditionalPanel(
                         condition = "input.user_player_type_pitch_quality == 'user_pitch_quality_pitchers'",
                         selectInput("user_pitch_type_pitchers", "Pitch Type", choices = c("Choose...","All","Fastball","Breaking Ball","Offspeed")),
                       ),
                       conditionalPanel(
                         condition = "input.user_player_type_pitch_quality == 'user_pitch_quality_pitchers'",
                         sliderInput("num_pitches_pitch_quality", "Min. Pitches:", min = 0, max = 1600, value = 0, step=100),
                       )
                     )
      ), mainPanel(uiOutput('plot'))
    )
  )
  
  server <- function(input, output, session) 
  {
    output$plot <- renderUI({
      season <- input$user_season
      if ((input$user_stat_type == "user_sds") & (input$user_stat_type != "Choose..."))
      {
        if ((input$user_player_type_sds == "user_sds_battters") & (season != "Choose..."))
        {
          N <- input$num_pitches_sds
          if (season == 2021)
          {
            output$tbl <- renderDT({datatable(sds_individual_leaders(data = full_preds21, min_pitches = N), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
          } else if (season == 2022) {
            output$tbl <- renderDT({datatable(sds_individual_leaders(data = full_preds22, min_pitches = N), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
          }
        } else if ((input$user_player_type_sds == "user_sds_teams") & (season != "Choose...")) {
          if (season == 2021)
          {
            output$tbl <- renderDT({datatable(sds_team_leaders(data = full_preds21), rownames= FALSE, filter = "top", options = list(pageLength = 30, autoWidth = TRUE))})
          } else if (season == 2022) {
            output$tbl <- renderDT({datatable(sds_team_leaders(data = full_preds22), rownames= FALSE, filter = "top", options = list(pageLength = 30, autoWidth = TRUE))})
          }
        }
      } else if ((input$user_stat_type == "user_pitch_quality") & (input$user_stat_type != "Choose..."))
      {
        if ((input$user_player_type_pitch_quality == "user_pitch_quality_pitchers") & (season != "Choose..."))
        {
          N <- input$num_pitches_pitch_quality
          if (season == 2021)
          {
            if ((input$user_pitch_type_pitchers == "All") & !is.na(N) & (input$user_pitch_type_pitchers != "Choose..."))
            {
              output$tbl <- renderDT({datatable(pitch_quality_leaders(data = full_preds21, pitch_type = "all", min_pitches = N), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
            } else if ((input$user_pitch_type_pitchers == "Fastball") & !is.na(N)) {
              output$tbl <- renderDT({datatable(pitch_quality_leaders(data = full_preds21, pitch_type = "Fastball", min_pitches = N), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
            } else if ((input$user_pitch_type_pitchers == "Breaking Ball") & !is.na(N)) {
              output$tbl <- renderDT({datatable(pitch_quality_leaders(data = full_preds21, pitch_type = "Breaking Ball", min_pitches = N), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
            } else if ((input$user_pitch_type_pitchers == "Offspeed") & !is.na(N)) {
              output$tbl <- renderDT({datatable(pitch_quality_leaders(data = full_preds21, pitch_type = "Offspeed", min_pitches = N), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
            }
          } else if (season == 2022) {
            if ((input$user_pitch_type_pitchers == "All") & !is.na(N) & (input$user_pitch_type_pitchers != "Choose..."))
            {
              output$tbl <- renderDT({datatable(pitch_quality_leaders(data = full_preds22, pitch_type = "all", min_pitches = N), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
            } else if ((input$user_pitch_type_pitchers == "Fastball") & !is.na(N)) {
              output$tbl <- renderDT({datatable(pitch_quality_leaders(data = full_preds22, pitch_type = "Fastball", min_pitches = N), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
            } else if ((input$user_pitch_type_pitchers == "Breaking Ball") & !is.na(N)) {
              output$tbl <- renderDT({datatable(pitch_quality_leaders(data = full_preds22, pitch_type = "Breaking Ball", min_pitches = N), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
            } else if ((input$user_pitch_type_pitchers == "Offspeed") & !is.na(N)) {
              output$tbl <- renderDT({datatable(pitch_quality_leaders(data = full_preds22, pitch_type = "Offspeed", min_pitches = N), rownames= FALSE, filter = "top", options = list(pageLength = 25, autoWidth = TRUE))})
            }
          }
        } else if ((input$user_player_type_pitch_quality == "user_pitch_quality_teams") & (input$user_pitch_type_teams != "Choose...") & (season != "Choose...")) {
          if (season == 2021) {
            if ((input$user_pitch_type_teams == "All"))
            {
              output$tbl <- renderDT({datatable(team_pitch_quality_leaders(data = full_preds21, pitch_type = "all"), rownames = FALSE, filter = "top", options = list(pageLength = 30, autoWidth = TRUE))})
            } else if (input$user_pitch_type_teams == "Fastball") {
              output$tbl <- renderDT({datatable(team_pitch_quality_leaders(data = full_preds21, pitch_type = "Fastball"), rownames = FALSE, filter = "top", options = list(pageLength = 30, autoWidth = TRUE))})
            } else if (input$user_pitch_type_teams == "Breaking Ball") {
              output$tbl <- renderDT({datatable(team_pitch_quality_leaders(data = full_preds21, pitch_type = "Breaking Ball"), rownames = FALSE, filter = "top", options = list(pageLength = 30, autoWidth = TRUE))})
            } else if (input$user_pitch_type_teams == "Offspeed") {
              output$tbl <- renderDT({datatable(team_pitch_quality_leaders(data = full_preds21, pitch_type = "Offspeed"), rownames = FALSE, filter = "top", options = list(pageLength = 30, autoWidth = TRUE))})
            }
          } else if (season == 2022) {
            if ((input$user_pitch_type_teams == "All"))
            {
              output$tbl <- renderDT({datatable(team_pitch_quality_leaders(data = full_preds22, pitch_type = "all"), rownames = FALSE, filter = "top", options = list(pageLength = 30, autoWidth = TRUE))})
            } else if (input$user_pitch_type_teams == "Fastball") {
              output$tbl <- renderDT({datatable(team_pitch_quality_leaders(data = full_preds22, pitch_type = "Fastball"), rownames = FALSE, filter = "top", options = list(pageLength = 30, autoWidth = TRUE))})
            } else if (input$user_pitch_type_teams == "Breaking Ball") {
              output$tbl <- renderDT({datatable(team_pitch_quality_leaders(data = full_preds22, pitch_type = "Breaking Ball"), rownames = FALSE, filter = "top", options = list(pageLength = 30, autoWidth = TRUE))})
            } else if (input$user_pitch_type_teams == "Offspeed") {
              output$tbl <- renderDT({datatable(team_pitch_quality_leaders(data = full_preds22, pitch_type = "Offspeed"), rownames = FALSE, filter = "top", options = list(pageLength = 30, autoWidth = TRUE))})
            }
          }
        }
      }
    })
  }
  return(list(ui, server))
}