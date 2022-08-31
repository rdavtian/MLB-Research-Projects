set_up_shiny <- function(full_preds21, full_preds22)
{
  ui <- fluidPage(
    titlePanel(h1("Ruslan's MLB Pitch Quality Leaderboards", align = "center", 
                  h3("", align = "center", style = "font-size:16px;"))),
    sidebarLayout(
      sidebarPanel(width = 3, 
                   selectInput("user_player_type", "Player Type", c("Choose...", Pitcher = "user_pitchers", Team = "user_teams")),
                   selectInput("user_season", "Season", choices = c("Choose...",2021,2022)),
                   conditionalPanel(
                     condition = "input.user_player_type == 'user_pitchers'",
                     selectInput("user_pitch_type_pitchers", "Pitch Type", choices = c("Choose...","All","Fastball","Breaking Ball","Offspeed")),
                   ),
                   conditionalPanel(
                     condition = "input.user_player_type == 'user_pitchers'",
                     sliderInput("num_pitches", "Min. Pitches:", min = 0, max = 1000, value = 0),
                   ),
                   conditionalPanel(
                     condition = "input.user_player_type == 'user_teams'",
                     selectInput("user_pitch_type_teams", "Pitch Type", choices = c("Choose...","All","Fastball","Breaking Ball","Offspeed")))
      ), mainPanel(uiOutput('plot'))
    )
  )
  
  server <- function(input, output, session) 
  {
    output$plot <- renderUI({
      season <- input$user_season
      if ((input$user_player_type == "user_pitchers") & (season != "Choose"))
      {
        N <- input$num_pitches
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
      } else if ((input$user_player_type == "user_teams") & (input$user_pitch_type_teams != "Choose...") & (season != "Choose")) {
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
    })
  }
  return(list(ui, server))
}