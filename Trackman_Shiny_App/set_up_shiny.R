set_up_shiny <- function()
{
  ui <- fluidPage(
    titlePanel(h1("Ruslan's MLB Advanced/Pro Scouting Reports", align = "center")),
      sidebarLayout(
      sidebarPanel(width=3, 
      selectInput("user_type_input","Player Type", choices = c("Choose...","Batter","Pitcher")),
      conditionalPanel(condition = "input.user_type_input == 'Batter'",
                       selectInput("user_name_input","Select Name",  c("Choose...", sort(hitters_list$name)))
                       ),
      conditionalPanel(condition = "input.user_type_input == 'Pitcher'",
                       selectInput("user_name_input2","Select Name", c("Choose...", sort(pitchers_list$name)))
                       ),
      conditionalPanel(condition = "input.user_type_input == 'Batter'",
                       selectInput("user_visual_type","Charts & Tables", choices = c("Choose...","Batter Basic Stats","Spray Chart - All Hits","Spray Chart - All Outs",
                                                                      "Spray Chart - Home Hits","Spray Chart - Home Outs",
                                                                      "Pitch Chart - Hits","Pitch Chart - Outs","Pitch Chart - SO",
                                                                      "Pitch Chart - Whiffs", "Pitch Chart - Pitch Type", 
                                                                      "Contact Type", "Exit Velocity",
                                                                      "Batted Ball Type", "Batted Ball Type by Pitch",
                                                                      "IF Shift","OF Shift","Batter Metrics",
                                                                      "Batter Metrics by Pitch Type","Whiff Rates"))
                      ),
      conditionalPanel(condition = "input.user_type_input == 'Pitcher'",
                       selectInput("user_visual_type2","Charts & Tables", choices = c("Choose...","Pitcher Basic Stats","Pitch Arsenal",
                                                                       "Spray Chart - All Hits","Spray Chart - All Outs",
                                                                       "Spray Chart - Home Hits","Spray Chart - Home Outs","Pitch Chart - Hits",
                                                                       "Pitch Chart - Outs","Pitch Chart - SO","Pitch Chart - Whiffs", 
                                                                       "Pitch Chart - Tendencies", "Pitch Chart - Pitch Type","Pitch Chart - First Pitch",
                                                                       "Pitch Chart - Ahead",  "Pitch Chart - Behind", "Contact Type","Release Points",
                                                                       "Pitch Movement", "Pitch Velocity","Pitch Spin Rate","Pitch Spin Axis","Pitch Usage by Count",
                                                                       "Exit Velocity", "Batted Ball Type","Batted Ball Type by Pitch", "IF Shift","OF Shift",
                                                                       "Pitcher Metrics by Batter Side","Pitcher Metrics by Pitch Type","Whiff Rates"))
                      ),
      conditionalPanel(condition = "input.user_type_input == 'Batter'",
                       selectInput("user_heat_input","Heat Maps", choices = c("Choose...","Called Strike Probability","Swing Probability",
                                                               "Contact Probability","Whiff Probability","Ball in Play Probability",
                                                               "Home Run Probability","Hit Probability","Exit Velocity","Launch Angle","Spray Angle","WOBA","xBA","xWOBA"))),
      conditionalPanel(condition = "input.user_type_input == 'Pitcher'",
                       selectInput("user_heat_input2","Heat Maps", choices = c("Choose...","Called Strike Probability","Swing Probability",
                                                                "Contact Probability","Whiff Probability","Ball in Play Probability",
                                                                "Home Run Probability","Hit Probability","Exit Velocity","Launch Angle","Spray Angle","WOBA","xBA","xWOBA"))
                      ),
      dateRangeInput(
        inputId = "daterange",
        label = "Select Date Range",
        start = as.Date("2021-03-25"),
        end = as.Date("2021-07-01"),
        min = as.Date("2021-03-25"),
        max = as.Date("2021-07-01"),
        format = "mm/dd/yyyy",
        separator = "to"),
    ),
     mainPanel(uiOutput("plot"))
     ))
  
  
  server <- function(input, output, session)
  {
    output$startdate <- renderText({
      as.character(input$daterange[1])
    })
    output$range <- renderText({
      paste("Selected date range is ", input$daterange[1], "to", input$daterange[2])
    })
    output$enddate <- renderText({
      as.character(input$daterange[2])
    })

    output$plot <- renderUI({
      if (input$user_type_input == "Batter" & input$user_name_input != "Choose...")
      {
        hitter <- query_hitter(input$user_name_input)
        hitter <- clean_statcast_data(hitter)
        hitter <- hitter %>% filter(game_date >= input$daterange[1], game_date <= input$daterange[2])
        hitter_hits <- hitter %>% filter(events %in% c("Home Run","Triple","Double","Single"))
        hitter_outs <- hitter %>% filter(events %in% c("Strike Out","Force Out","Double Play","Field Out","Sac Fly","Sac Bunt"))
        hitter_batted_balls <- hitter %>% filter(description %in% c("hit_into_play_no_out","hit_into_play","hit_into_play_score"))
        hitter_k <- hitter %>% filter(events == 'Strike Out')
        hitter_whiffs <- hitter %>% filter(description %in% c("swinging_strike_blocked","swinging_strike"))
        hitter_abs <- hitter %>% filter(events %in% c("Strike Out","Field Out","Single","Double",
                                                      "Triple","Home Run","Force Out","Double Play"))
      }
      else if (input$user_type_input == "Pitcher" & input$user_name_input2 != "Choose...") {
        pitcher <- query_pitcher(input$user_name_input2)
        pitcher <- clean_statcast_data(pitcher) 
        pitcher <- pitcher %>% filter(game_date >= input$daterange[1], game_date <= input$daterange[2])
        pitcher_hits <- pitcher %>% filter(events %in% c("Home Run","Triple","Double","Single"))
        pitcher_outs <- pitcher %>% filter(events %in% c("Strike Out","Force Out","Double Play","Field Out","Sac Fly","Sac Bunt"))
        pitcher_batted_balls <- pitcher %>% filter(description %in% c("hit_into_play_no_out","hit_into_play","hit_into_play_score"))
        pitcher_k <- pitcher %>% filter(events == 'Strike Out')
        pitcher_whiffs <- pitcher %>% filter(description %in% c("swinging_strike_blocked","swinging_strike"))
        pitcher_abs <- pitcher %>% filter(events %in% c("Strike Out","Field Out","Single","Double",
                                                        "Triple","Home Run","Force Out","Double Play"))
      }
      if (input$user_type_input == "Batter")
      {
        if (input$user_visual_type == "Spray Chart - All Hits" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot1 <- renderPlot({spray_chart_advanced_generic(hitter_hits, title = "Hits Spray Chart 2021")})
          plotOutput("plot1")
        }
        else if (input$user_visual_type == "Spray Chart - All Outs" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot2 <- renderPlot({spray_chart_advanced_generic(hitter_outs %>% filter(bb_type != 'null'), title = "Outs Spray Chart 2021")})
          plotOutput("plot2")
        }
        else if (input$user_visual_type == "Spray Chart - Home Hits" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot2 <- renderPlot({spray_chart_advanced_home(hitter_hits %>% filter(bb_type != 'null'), title = "Hits Spray Chart 2021")})
          plotOutput("plot2")
        }
        else if (input$user_visual_type == "Spray Chart - Home Outs" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot2 <- renderPlot({spray_chart_advanced_home(hitter_outs %>% filter(bb_type != 'null'), title = "Outs Spray Chart 2021")})
          plotOutput("plot2")
        }
        else if (input$user_visual_type == "Pitch Chart - Hits" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot3 <- renderPlot({pitch_chart_batter(hitter_hits, title = "Hits Location 2021", TRUE)})
          plotOutput("plot3")
        }
        else if (input$user_visual_type == "Pitch Chart - Outs" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot4 <- renderPlot({pitch_chart_batter(hitter_outs, title = "Outs Achieved by Location 2021")})
          plotOutput("plot4")
        }
        else if (input$user_visual_type == "Pitch Chart - SO" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot5 <- renderPlot({pitch_chart_batter(hitter_k, title = "Strikeouts by Location 2021")})
          plotOutput("plot5")
        }
        else if (input$user_visual_type == "Pitch Chart - Whiffs" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot6 <- renderPlot({pitch_chart_batter(hitter_whiffs, title = "Whiffs by Location 2021")})
          plotOutput("plot6")
        }
        else if (input$user_visual_type == "Pitch Chart - Pitch Type" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot1234 <- renderPlot({pitch_chart_pitch_type(hitter, title = "Location by Pitch Type 2021")})
          plotOutput("plot1234")
        }
        else if (input$user_visual_type == "Contact Type" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot7 <- renderPlot({contact_chart(hitter, title = "Batted Balls Contact Type % 2021")})
          plotOutput("plot7")
        }
        else if (input$user_visual_type == "Exit Velocity" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot8 <- renderPlot({exit_velocity(hitter_batted_balls, title = "Distribution of Exit Velocity")})
          plotOutput("plot8")
        }
        else if (input$user_visual_type == "Batted Ball Type" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({batted_ball_type(hitter_batted_balls, title = "Batted Ball Rates 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "IF Shift" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({if_shift_usage(hitter, title = " Metrics by IF Positioning 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "OF Shift" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({of_shift_usage(hitter, title = " Metrics by OF Positioning 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Batter Basic Stats" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({batter_basic_stats(hitter, title = "Overall Stats 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Batter Metrics" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({batter_stats(hitter, title = "Metrics 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Batter Metrics by Pitch Type" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({stats_by_type(hitter, title = "Metrics by Pitch Type 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Whiff Rates" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({whiff_by_pitch_type(hitter, "Whiff % 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Batted Ball Type by Pitch" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({batted_ball_by_pitch_type(hitter, "Batted Ball Rates by Pitch Type 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_heat_input == "Called Strike Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_called(hitter)
          output$plot22 <- renderPlot({heat_map(hitter2, var = "Strike", binary = T, legend_title = "Probability", title = "Probability of Strike 2021")})
          plotOutput("plot22")
        }
        else if (input$user_heat_input == "Swing Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_all(hitter)
          output$plot23 <- renderPlot(heat_map(hitter2, var = "Swing", binary = T, legend_title = "Probability", title = "Probability of Swing 2021"))
          plotOutput("plot23")
        }
        else if (input$user_heat_input == "Contact Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_swing(hitter)
          output$plot24 <- renderPlot(heat_map(hitter2, var = "Contact", binary = T, legend_title = "Probability", title = "Probability of Contact Given Swing 2021"))
          plotOutput("plot24")
        }
        else if (input$user_heat_input == "Whiff Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_swing(hitter)
          output$plot25 <- renderPlot(heat_map(hitter2, var = "Miss", binary = T, legend_title = "Probability", title = "Probability of Whiff Given Swing 2021"))
          plotOutput("plot25")
        }
        else if (input$user_heat_input == "Ball in Play Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_swing(hitter)
          output$plot26 <- renderPlot(heat_map(hitter2, var = "InPlay", binary = T, legend_title = "Probability", title = "Probability of Ball in Play Given Swing 2021"))
          plotOutput("plot26")
        }
        else if (input$user_heat_input == "Exit Velocity" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_inplay(hitter)
          output$plot27 <- renderPlot(heat_map(hitter2, var = "launch_speed", binary = F, legend_title = "Exit Velocity", title = "Exit Velocity 2021"))
          plotOutput("plot27")
        }
        else if (input$user_heat_input == "Launch Angle" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_inplay(hitter)
          output$plot28 <- renderPlot(heat_map(hitter2, var = "launch_angle", binary = F, legend_title = "Launch Angle", title = "Launch Angle 2021"))
          plotOutput("plot28")
        }
        else if (input$user_heat_input == "Spray Angle" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_inplay(hitter)
          output$plot29 <- renderPlot(heat_map(hitter2, var = "spray_angle", binary = F, legend_title = "Spray Angle", title = "Spray Angle 2021"))
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "WOBA" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          output$plot29 <- renderPlot(woba_heat_map_batter(hitter, title = "WOBA by Pitcher Side 2021"))
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "xBA" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          output$plot29 <- renderPlot(xba_heat_map_batter(hitter, "xBA by Pitcher Side 2021"))
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "xWOBA" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          output$plot29 <- renderPlot(xwoba_heat_map_batter(hitter, "xWOBA by Pitcher Side 2021"))
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "Home Run Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          output$plot29 <- renderPlot(heat_map(hitter_batted_balls, var = "is_hr", binary = T, legend_title = "Probability", title = "Probability of HR 2021"))
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "Hit Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          output$plot29 <- renderPlot(heat_map(hitter_abs, var = "is_hit", binary = T, legend_title = "Probability", title = "Probability of Hit 2021"))
          plotOutput("plot29")
        }
      }
      
      
      
      
      else if (input$user_type_input == "Pitcher")
      {
        if (input$user_visual_type2 == "Spray Chart - All Hits" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot9 <- renderPlot({spray_chart_advanced_generic(pitcher_hits, title = "Hits Given Up 2021")})
          plotOutput("plot9")
        }
        else if (input$user_visual_type2 == "Spray Chart - All Outs" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot10 <- renderPlot({spray_chart_advanced_generic(pitcher_outs %>% filter(bb_type != 'null'), title = "Outs Achieved 2021")})
          plotOutput("plot10")
        }
        else if (input$user_visual_type2 == "Spray Chart - Home Hits" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot10 <- renderPlot({spray_chart_advanced_home(pitcher_hits %>% filter(bb_type != 'null'), title = "Hits Given Up 2021")})
          plotOutput("plot10")
        }
        else if (input$user_visual_type2 == "Spray Chart - Home Outs" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot10 <- renderPlot({spray_chart_advanced_home(pitcher_outs %>% filter(bb_type != 'null'), title = "Outs Achieved 2021")})
          plotOutput("plot10")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Hits" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot11 <- renderPlot({pitch_chart_pitcher(pitcher_hits, title = "Hits Given Up by Location 2021", TRUE)})
          plotOutput("plot11")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Outs" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot12 <- renderPlot({pitch_chart_pitcher(pitcher_outs, title = "Outs Achieved by Location 2021")})
          plotOutput("plot12")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Pitch Type" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot1234 <- renderPlot({pitch_chart_pitch_type(pitcher, title = "Location by Pitch Type 2021")})
          plotOutput("plot1234")
        }
        else if (input$user_visual_type2 == "Pitch Chart - SO" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot13 <- renderPlot({pitch_chart_pitcher(pitcher_k, title = "Strikeouts by Location 2021")})
          plotOutput("plot13")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Whiffs" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot14 <- renderPlot({pitch_chart_pitcher(pitcher_whiffs, title = "Whiffs by Location 2021")})
          plotOutput("plot14")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Tendencies" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot1002 <- renderPlot({pitch_chart_density(pitcher, title = "Pitch Location Tendencies 2021")})
          plotOutput("plot1002")
        }
        else if (input$user_visual_type2 == "Pitch Chart - First Pitch" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot1001 <- renderPlot({pitch_chart_pitcher_first_pitch(pitcher, title = "First Pitch Location 2021")})
          plotOutput("plot1001")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Ahead" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot1002 <- renderPlot({pitch_chart_pitcher_ahead(pitcher, title = "Ahead in Count Location 2021")})
          plotOutput("plot1002")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Behind" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot1003 <- renderPlot({pitch_chart_pitcher_behind(pitcher, title = "Behind in Count Location 2021")})
          plotOutput("plot1003")
        }
        else if (input$user_visual_type2 == "Pitch Arsenal" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot15 <- renderPlot({pitch_arsenal(pitcher, " Pitch Distribution 2021")})
          plotOutput("plot15")
        }
        else if (input$user_visual_type2 == "Pitch Movement" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot16 <- renderPlot({pitch_movement(pitcher, title = "Pitch Movement 2021")})
          plotOutput("plot16")
        }
        else if (input$user_visual_type2 == "Pitch Velocity" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot17 <- renderPlot({pitch_velocity(pitcher, "Velocity by Pitch Type 2021")})
          plotOutput("plot17")
        }
        else if (input$user_visual_type2 == "Pitch Spin Rate" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot18 <- renderPlot({pitch_spinrate(pitcher, "Spin Rate by Pitch Type 2021")})
          plotOutput("plot18")
        }
        else if (input$user_visual_type2 == "Pitch Spin Axis" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot100 <- renderPlot({plot_spin_axis(pitcher)})
          plotOutput("plot100")
        }
        else if (input$user_visual_type2 == "Contact Type" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot19 <- renderPlot({contact_chart(pitcher, title = "Batted Balls Contact Type % 2021")})
          plotOutput("plot19")
        }
        else if (input$user_visual_type2 == "Release Points" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot123 <- renderPlot({release_position(pitcher, title = "2021 Release Position")})
          plotOutput("plot123")
        }
        else if (input$user_visual_type2 == "Exit Velocity" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot20 <- renderPlot({exit_velocity(pitcher_batted_balls, title = "Distribution of Exit Velocity Against")})
          plotOutput("plot20")
        }
        else if (input$user_visual_type2 == "Batted Ball Type" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({batted_ball_type(pitcher_batted_balls, title = "Batted Ball Rates")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "IF Shift" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({if_shift_usage(pitcher, title = " Metrics by IF Positioning 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "OF Shift" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({of_shift_usage(pitcher, title = " Metrics by OF Positioning 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Pitcher Metrics by Batter Side" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({pitcher_stats(pitcher, title = "Metrics Against 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Pitcher Basic Stats" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({pitcher_basic_stats(pitcher, title = "Overall Stats 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Pitcher Metrics by Pitch Type" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({stats_by_type(pitcher, title = "Metrics by Pitch Type 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Whiff Rates" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({whiff_by_pitch_type(pitcher, "Whiff % 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Batted Ball Type by Pitch" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({batted_ball_by_pitch_type(pitcher, "Batted Ball Rates by Pitch Type 2021")})
          htmlOutput("tbl")
        }
        else if (input$user_heat_input2 == "Called Strike Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_called(pitcher)
          output$plot30 <- renderPlot({heat_map(pitcher2, var = "Strike", binary = T, legend_title = "Probability", title = "Probability of Called Strike 2021")})
          plotOutput("plot30")
        }
        else if (input$user_heat_input2 == "Swing Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_all(pitcher)
          output$plot31 <- renderPlot(heat_map(pitcher2, var = "Swing", binary = T, legend_title = "Probability", title = "Probability of Swing 2021"))
          plotOutput("plot31")
        }
        else if (input$user_heat_input2 == "Contact Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_swing(pitcher)
          output$plot32 <- renderPlot(heat_map(pitcher2, var = "Contact", binary = T, legend_title = "Probability", title = "Probability of Contact Given Swing 2021"))
          plotOutput("plot32")
        }
        else if (input$user_heat_input2 == "Whiff Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_swing(pitcher)
          output$plot33 <- renderPlot(heat_map(pitcher2, var = "Miss", binary = T, legend_title = "Probability", title = "Probability of Whiff Given Swing 2021"))
          plotOutput("plot33")
        }
        else if (input$user_heat_input2 == "Ball in Play Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_swing(pitcher)
          output$plot34 <- renderPlot(heat_map(pitcher2, var = "InPlay", binary = T, legend_title = "Probability", title = "Probability of Ball in Play Given Swing 2021"))
          plotOutput("plot34")
        }
        else if (input$user_heat_input2 == "Exit Velocity" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_inplay(pitcher)
          output$plot35 <- renderPlot(heat_map(pitcher2, var = "launch_speed", binary = F, legend_title = "Exit Velocity", title = "Exit Velocity 2021"))
          plotOutput("plot35")
        }
        else if (input$user_heat_input2 == "Launch Angle" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_inplay(pitcher)
          output$plot36 <- renderPlot(heat_map(pitcher2, var = "launch_angle", binary = F, legend_title = "Launch Angle", title = "Launch Angle 2021"))
          plotOutput("plot36")
        }
        else if (input$user_heat_input2 == "Spray Angle" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_inplay(pitcher)
          output$plot37 <- renderPlot(heat_map(pitcher2, var = "spray_angle", binary = F, legend_title = "Spray Angle", title = "Spray Angle 2021"))
          plotOutput("plot37")
        }
        else if (input$user_heat_input2 == "WOBA" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          output$plot38 <- renderPlot(woba_heat_map_pitcher(pitcher, title = "WOBA by Batter Side 2021"))
          plotOutput("plot38")
        }
        else if (input$user_heat_input2 == "xBA" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          output$plot39 <- renderPlot(xba_heat_map_pitcher(pitcher, "xBA by Batter Side 2021"))
          plotOutput("plot39")
        }
        else if (input$user_heat_input2 == "xWOBA" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          output$plot40 <- renderPlot(xwoba_heat_map_pitcher(pitcher, "xWOBA by Batter Side 2021"))
          plotOutput("plot40")
        }
        else if (input$user_visual_type2 == "Pitch Usage by Count" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot41 <- renderPlot(pitch_usage_by_count(pitcher, "Pitch Usage by Count 2021"))
          plotOutput("plot41")
        }
        else if (input$user_heat_input2 == "Home Run Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          output$plot42 <- renderPlot(heat_map(pitcher_batted_balls, var = "is_hr", binary = T, legend_title = "Probability", title = "Probability of HR 2021"))
          plotOutput("plot42")
        }
        else if (input$user_heat_input2 == "Hit Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          output$plot43 <- renderPlot(heat_map(pitcher_abs, var = "is_hit", binary = T, legend_title = "Probability", title = "Probability of Hit 2021"))
          plotOutput("plot43")
        }
        
      }
      
    })
  }
  return(list(ui, server))
}