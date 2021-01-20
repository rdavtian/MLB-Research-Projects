set_up_shiny <- function()
{
  ui <- fluidPage(
    sidebarPanel(
      selectInput("user_type_input","Player Type", choices = c("Choose...","Batter","Pitcher")),
      conditionalPanel(
        condition = "input.user_type_input == 'Batter'",
        selectInput("user_name_input","Select Name",  c("Choose...", sort(hitters_list$name))
        )
      ),
      conditionalPanel(
        condition = "input.user_type_input == 'Pitcher'",
        selectInput("user_name_input2","Select Name", c("Choose...", sort(pitchers_list$name))
        )
      ),
      conditionalPanel(
        condition = "input.user_type_input == 'Batter'",
        selectInput("user_visual_type","Charts & Tables", choices = c("Choose...","Spray Chart - Hits","Spray Chart - Outs",
                                                                      "Pitch Chart - Hits","Pitch Chart - Outs",
                                                                      "Pitch Chart - SO","Pitch Chart - Whiffs", "Contact Type",
                                                                      "Exit Velocity","Batted Ball Type", "Batted Ball Type by Pitch",
                                                                      "IF Shift","OF Shift",
                                                                      "Batter Metrics","Batter Metrics by Pitch Type","Whiff Rates")
        )
      ),
      conditionalPanel(
        condition = "input.user_type_input == 'Pitcher'",
        selectInput("user_visual_type2","Charts & Tables", choices = c("Choose...","Pitch Arsenal",
                                                                       "Spray Chart - Hits","Spray Chart - Outs",
                                                                       "Pitch Chart - Hits","Pitch Chart - Outs",
                                                                       "Pitch Chart - SO","Pitch Chart - Whiffs", "Contact Type",
                                                                       "Pitch Movement", "Pitch Velocity","Pitch Spin Rate","Pitch Usage by Count",
                                                                       "Exit Velocity", "Batted Ball Type","Batted Ball Type by Pitch", "IF Shift","OF Shift",
                                                                       "Pitcher Metrics","Pitcher Metrics by Pitch Type","Whiff Rates")
        )
      ),
      conditionalPanel(
        condition = "input.user_type_input == 'Batter'",
        selectInput("user_heat_input","Heat Maps", choices = c("Choose...","Called Strike Probability","Swing Probability",
                                                               "Contact Probability","Whiff Probability","Ball in Play Probability",
                                                               "Home Run Probability","Hit Probability","Exit Velocity","Launch Angle","Spray Angle","WOBA","xBA","xWOBA"))
      ),
      conditionalPanel(
        condition = "input.user_type_input == 'Pitcher'",
        selectInput("user_heat_input2","Heat Maps", choices = c("Choose...","Called Strike Probability","Swing Probability",
                                                                "Contact Probability","Whiff Probability","Ball in Play Probability",
                                                                "Home Run Probability","Hit Probability","Exit Velocity","Launch Angle","Spray Angle","WOBA","xBA","xWOBA"))
      )
    ),
    mainPanel(
      uiOutput("plot")
    ))
  
  
  server <- function(input, output, session)
  {
    
    output$plot <- renderUI({
      
      if (input$user_type_input == "Batter" & input$user_name_input != "Choose...")
      {
        hitter <- query_hitter(input$user_name_input)
        hitter <- clean_statcast_data(hitter)
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
        if (input$user_visual_type == "Spray Chart - Hits" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot1 <- renderPlot({spray_chart(hitter_hits, title = "Hits Spray Chart 2019-2020")})
          plotOutput("plot1")
        }
        else if (input$user_visual_type == "Spray Chart - Outs" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot2 <- renderPlot({spray_chart(hitter_outs %>% filter(bb_type != 'null'), title = "Outs Spray Chart 2019-2020")})
          plotOutput("plot2")
        }
        else if (input$user_visual_type == "Pitch Chart - Hits" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot3 <- renderPlot({pitch_chart_batter(hitter_hits, title = "Hits Location 2019-2020")})
          plotOutput("plot3")
        }
        else if (input$user_visual_type == "Pitch Chart - Outs" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot4 <- renderPlot({pitch_chart_batter(hitter_outs, title = "Outs Achieved by Location 2019-2020")})
          plotOutput("plot4")
        }
        else if (input$user_visual_type == "Pitch Chart - SO" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot5 <- renderPlot({pitch_chart_batter(hitter_k, title = "Strikeouts by Location 2019-2020")})
          plotOutput("plot5")
        }
        else if (input$user_visual_type == "Pitch Chart - Whiffs" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot6 <- renderPlot({pitch_chart_batter(hitter_whiffs, title = "Whiffs by Location 2019-2020")})
          plotOutput("plot6")
        }
        else if (input$user_visual_type == "Contact Type" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot7 <- renderPlot({contact_chart(hitter, title = "Batted Balls Contact Type % 2019-2020")})
          plotOutput("plot7")
        }
        else if (input$user_visual_type == "Exit Velocity" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot8 <- renderPlot({exit_velocity(hitter_batted_balls, title = "Distribution of Exit Velocity")})
          plotOutput("plot8")
        }
        else if (input$user_visual_type == "Batted Ball Type" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({batted_ball_type(hitter_batted_balls, title = "Batted Ball Rates 2019-2020")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "IF Shift" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({if_shift_usage(hitter, title = " Metrics by IF Positioning 2019-2020")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "OF Shift" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({of_shift_usage(hitter, title = " Metrics by OF Positioning 2019-2020")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Batter Metrics" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({batter_stats(hitter, title = "Metrics Against 2019-2020")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Batter Metrics by Pitch Type" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({stats_by_type(hitter, title = "Metrics by Pitch Type 2019-2020")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Whiff Rates" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({whiff_by_pitch_type(hitter, "Whiff % 2019-2020")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Batted Ball Type by Pitch" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({batted_ball_by_pitch_type(hitter, "Batted Ball Rates by Pitch Type 2019-2020")})
          htmlOutput("tbl")
        }
        else if (input$user_heat_input == "Called Strike Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_called(hitter)
          output$plot22 <- renderPlot({heat_map(hitter2, var = "Strike", binary = T, legend_title = "Probability", title = "Probability of Strike 2019-2020")})
          plotOutput("plot22")
        }
        else if (input$user_heat_input == "Swing Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_all(hitter)
          output$plot23 <- renderPlot(heat_map(hitter2, var = "Swing", binary = T, legend_title = "Probability", title = "Probability of Swing 2019-2020"))
          plotOutput("plot23")
        }
        else if (input$user_heat_input == "Contact Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_swing(hitter)
          output$plot24 <- renderPlot(heat_map(hitter2, var = "Contact", binary = T, legend_title = "Probability", title = "Probability of Contact Given Swing 2019-2020"))
          plotOutput("plot24")
        }
        else if (input$user_heat_input == "Whiff Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_swing(hitter)
          output$plot25 <- renderPlot(heat_map(hitter2, var = "Miss", binary = T, legend_title = "Probability", title = "Probability of Whiff Given Swing 2019-2020"))
          plotOutput("plot25")
        }
        else if (input$user_heat_input == "Ball in Play Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_swing(hitter)
          output$plot26 <- renderPlot(heat_map(hitter2, var = "InPlay", binary = T, legend_title = "Probability", title = "Probability of Ball in Play Given Swing 2019-2020"))
          plotOutput("plot26")
        }
        else if (input$user_heat_input == "Exit Velocity" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_inplay(hitter)
          output$plot27 <- renderPlot(heat_map(hitter2, var = "launch_speed", binary = F, legend_title = "Exit Velocity", title = "Exit Velocity 2019-2020"))
          plotOutput("plot27")
        }
        else if (input$user_heat_input == "Launch Angle" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_inplay(hitter)
          output$plot28 <- renderPlot(heat_map(hitter2, var = "launch_angle", binary = F, legend_title = "Launch Angle", title = "Launch Angle 2019-2020"))
          plotOutput("plot28")
        }
        else if (input$user_heat_input == "Spray Angle" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_inplay(hitter)
          output$plot29 <- renderPlot(heat_map(hitter2, var = "spray_angle", binary = F, legend_title = "Spray Angle", title = "Spray Angle 2019-2020"))
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "WOBA" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          output$plot29 <- renderPlot(woba_heat_map_batter(hitter, title = "WOBA by Pitcher Side 2019-2020"))
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "xBA" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          output$plot29 <- renderPlot(xba_heat_map_batter(hitter, "xBA by Pitcher Side 2019-2020"))
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "xWOBA" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          output$plot29 <- renderPlot(xwoba_heat_map_batter(hitter, "xWOBA by Pitcher Side 2019-2020"))
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "Home Run Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          output$plot29 <- renderPlot(heat_map(hitter_batted_balls, var = "is_hr", binary = T, legend_title = "Probability", title = "Probability of HR 2019-2020"))
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "Hit Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          output$plot29 <- renderPlot(heat_map(hitter_abs, var = "is_hit", binary = T, legend_title = "Probability", title = "Probability of Hit 2019-2020"))
          plotOutput("plot29")
        }
      }
      
      
      
      
      else if (input$user_type_input == "Pitcher")
      {
        if (input$user_visual_type2 == "Spray Chart - Hits" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot9 <- renderPlot({spray_chart(pitcher_hits, title = "Hits Given Up 2019-2020")})
          plotOutput("plot9")
        }
        else if (input$user_visual_type2 == "Spray Chart - Outs" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot10 <- renderPlot({spray_chart(pitcher_outs %>% filter(bb_type != 'null'), title = "Outs Achieved 2019-2020")})
          plotOutput("plot10")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Hits" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot11 <- renderPlot({pitch_chart_pitcher(pitcher_hits, title = "Hits Given Up by Location 2019-2020")})
          plotOutput("plot11")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Outs" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot12 <- renderPlot({pitch_chart_pitcher(pitcher_outs, title = "Outs Achieved by Location 2019-2020")})
          plotOutput("plot12")
        }
        else if (input$user_visual_type2 == "Pitch Chart - SO" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot13 <- renderPlot({pitch_chart_pitcher(pitcher_k, title = "Strikeouts by Location 2019-2020")})
          plotOutput("plot13")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Whiffs" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot14 <- renderPlot({pitch_chart_pitcher(pitcher_whiffs, title = "Whiffs by Location 2019-2020")})
          plotOutput("plot14")
        }
        else if (input$user_visual_type2 == "Pitch Arsenal" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot15 <- renderPlot({pitch_arsenal(pitcher, " Pitch Distribution 2019-2020")})
          plotOutput("plot15")
        }
        else if (input$user_visual_type2 == "Pitch Movement" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot16 <- renderPlot({pitch_movement(pitcher, title = "Pitch Movement 2019-2020")})
          plotOutput("plot16")
        }
        else if (input$user_visual_type2 == "Pitch Velocity" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot17 <- renderPlot({pitch_velocity(pitcher, "Velocity by Pitch Type 2019-2020")})
          plotOutput("plot17")
        }
        else if (input$user_visual_type2 == "Pitch Spin Rate" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot18 <- renderPlot({pitch_spinrate(pitcher, "Spin Rate by Pitch Type 2019-2020")})
          plotOutput("plot18")
        }
        else if (input$user_visual_type2 == "Contact Type" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot19 <- renderPlot({contact_chart(pitcher, title = "Batted Balls Contact Type % 2019-2020")})
          plotOutput("plot19")
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
          output$tbl <- renderText({if_shift_usage(pitcher, title = " Metrics by IF Positioning 2019-2020")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "OF Shift" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({of_shift_usage(pitcher, title = " Metrics by OF Positioning 2019-2020")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Pitcher Metrics" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({pitcher_stats(pitcher, title = "Metrics Against 2019-2020")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Pitcher Metrics by Pitch Type" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({stats_by_type(pitcher, title = "Metrics by Pitch Type 2019-2020")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Whiff Rates" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({whiff_by_pitch_type(pitcher, "Whiff % 2019-2020")})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Batted Ball Type by Pitch" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({batted_ball_by_pitch_type(pitcher, "Batted Ball Rates by Pitch Type 2019-2020")})
          htmlOutput("tbl")
        }
        else if (input$user_heat_input2 == "Called Strike Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_called(pitcher)
          output$plot30 <- renderPlot({heat_map(pitcher2, var = "Strike", binary = T, legend_title = "Probability", title = "Probability of Called Strike 2019-2020")})
          plotOutput("plot30")
        }
        else if (input$user_heat_input2 == "Swing Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_all(pitcher)
          output$plot31 <- renderPlot(heat_map(pitcher2, var = "Swing", binary = T, legend_title = "Probability", title = "Probability of Swing 2019-2020"))
          plotOutput("plot31")
        }
        else if (input$user_heat_input2 == "Contact Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_swing(pitcher)
          output$plot32 <- renderPlot(heat_map(pitcher2, var = "Contact", binary = T, legend_title = "Probability", title = "Probability of Contact Given Swing 2019-2020"))
          plotOutput("plot32")
        }
        else if (input$user_heat_input2 == "Whiff Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_swing(pitcher)
          output$plot33 <- renderPlot(heat_map(pitcher2, var = "Miss", binary = T, legend_title = "Probability", title = "Probability of Whiff Given Swing 2019-2020"))
          plotOutput("plot33")
        }
        else if (input$user_heat_input2 == "Ball in Play Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_swing(pitcher)
          output$plot34 <- renderPlot(heat_map(pitcher2, var = "InPlay", binary = T, legend_title = "Probability", title = "Probability of Ball in Play Given Swing 2019-2020"))
          plotOutput("plot34")
        }
        else if (input$user_heat_input2 == "Exit Velocity" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_inplay(pitcher)
          output$plot35 <- renderPlot(heat_map(pitcher2, var = "launch_speed", binary = F, legend_title = "Exit Velocity", title = "Exit Velocity 2019-2020"))
          plotOutput("plot35")
        }
        else if (input$user_heat_input2 == "Launch Angle" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_inplay(pitcher)
          output$plot36 <- renderPlot(heat_map(pitcher2, var = "launch_angle", binary = F, legend_title = "Launch Angle", title = "Launch Angle 2019-2020"))
          plotOutput("plot36")
        }
        else if (input$user_heat_input2 == "Spray Angle" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_inplay(pitcher)
          output$plot37 <- renderPlot(heat_map(pitcher2, var = "spray_angle", binary = F, legend_title = "Spray Angle", title = "Spray Angle 2019-2020"))
          plotOutput("plot37")
        }
        else if (input$user_heat_input2 == "WOBA" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          output$plot38 <- renderPlot(woba_heat_map_pitcher(pitcher, title = "WOBA by Batter Side 2019-2020"))
          plotOutput("plot38")
        }
        else if (input$user_heat_input2 == "xBA" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          output$plot39 <- renderPlot(xba_heat_map_pitcher(pitcher, "xBA by Batter Side 2019-2020"))
          plotOutput("plot39")
        }
        else if (input$user_heat_input2 == "xWOBA" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          output$plot40 <- renderPlot(xwoba_heat_map_pitcher(pitcher, "xWOBA by Batter Side 2019-2020"))
          plotOutput("plot40")
        }
        else if (input$user_visual_type2 == "Pitch Usage by Count" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot41 <- renderPlot(pitch_usage_by_count(pitcher, "Pitch Usage by Count 2019-2020"))
          plotOutput("plot41")
        }
        else if (input$user_heat_input2 == "Home Run Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          output$plot42 <- renderPlot(heat_map(pitcher_batted_balls, var = "is_hr", binary = T, legend_title = "Probability", title = "Probability of HR 2019-2020"))
          plotOutput("plot42")
        }
        else if (input$user_heat_input2 == "Hit Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          output$plot43 <- renderPlot(heat_map(pitcher_abs, var = "is_hit", binary = T, legend_title = "Probability", title = "Probability of Hit 2019-2020"))
          plotOutput("plot43")
        }
        
      }
      
    })
  }
  return(list(ui, server))
}