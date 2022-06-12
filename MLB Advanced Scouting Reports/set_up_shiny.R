set_up_shiny <- function(hitters_list, pitchers_list, start_year, end_year)
{
  ui <- fluidPage(
    titlePanel(h1("Ruslan's MLB Advanced Scouting Reports", align = "center", 
               h3("Keep Either Charts & Tables or Heat Maps as Choose...", 
                  align = "center", style = "font-size:14px;"))),
    #useWaiter(), # include dependencies
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
                                                                                                  "Spray Chart - Distributions",
                                                                                                  "Pitch Chart - Hits","Pitch Chart - Outs","Pitch Chart - SO",
                                                                                                  "Pitch Chart - Whiffs", "Pitch Chart - Outside Swings",
                                                                                                  "Pitch Chart - Pitch Type", "Contact Type", "Exit Velocity",
                                                                                                  "Batted Ball Type", "Batted Ball Type by Pitch",
                                                                                                  "IF Shift","OF Shift","Batter Metrics by Pitcher Side",
                                                                                                  "Batter Metrics by Pitch Type","Plate Discipline","Plate Discipline by Pitch Type",
                                                                                                  "Whiff Rates"))
                   ),
                   conditionalPanel(condition = "input.user_type_input == 'Pitcher'",
                                    selectInput("user_visual_type2","Charts & Tables", choices = c("Choose...","Pitcher Basic Stats","Pitch Arsenal",
                                                                                                   "Spray Chart - All Hits","Spray Chart - All Outs",
                                                                                                   "Spray Chart - Home Hits","Spray Chart - Home Outs",
                                                                                                   "Spray Chart - Distributions", 
                                                                                                   "Pitch Chart - Hits",
                                                                                                   "Pitch Chart - Outs","Pitch Chart - SO","Pitch Chart - Whiffs", 
                                                                                                   "Pitch Chart - Pitch Type","Pitch Chart - First Pitch",
                                                                                                   "Pitch Chart - Ahead",  "Pitch Chart - Behind", "Contact Type","Release Points",
                                                                                                   "Pitch Movement", "Pitch Velocity","Pitch Spin Rate","Pitch Spin Axis","Pitch Usage by Count",
                                                                                                   "Exit Velocity", "Batted Ball Type","Batted Ball Type by Pitch", "IF Shift","OF Shift",
                                                                                                   "Pitcher Metrics by Batter Side","Pitcher Metrics by Pitch Type",
                                                                                                   "Plate Discipline","Plate Discipline by Pitch Type","Whiff Rates","Run Value"))
                   ),
                   conditionalPanel(condition = "input.user_type_input == 'Batter'",
                                    selectInput("user_heat_input","Heat Maps", choices = c("Choose...","Called Strike Probability","Swing Probability","Swing Probability by Pitch Type",
                                                                                           "Contact Probability","Contact Probability by Pitch Type","Whiff Probability",
                                                                                           "Whiff Probability by Pitch Type","Ball in Play Probability","Ball in Play Probability by Pitch Type",
                                                                                           "Hit Probability","Hit Probability by Pitch Type","Home Run Probability","Home Run Probability by Pitch Type",
                                                                                           "Exit Velocity","Exit Velocity by Pitch Type","Launch Angle","Spray Angle","wOBA","xBA","xwOBA"))),
                   conditionalPanel(condition = "input.user_type_input == 'Pitcher'",
                                    selectInput("user_heat_input2","Heat Maps", choices = c("Choose...","Pitch Chart - Tendencies","Called Strike Probability","Swing Probability",
                                                                                            "Swing Probability by Pitch Type","Contact Probability","Contact Probability by Pitch Type",
                                                                                            "Whiff Probability","Whiff Probability by Pitch Type", "Ball in Play Probability",
                                                                                            "Ball in Play Probability by Pitch Type","Hit Probability",
                                                                                            "Hit Probability by Pitch Type","Home Run Probability","Home Run Probability by Pitch Type",
                                                                                            "Exit Velocity","Exit Velocity by Pitch Type","Launch Angle","Spray Angle",
                                                                                            "wOBA","xBA","xwOBA","Run Value", "Run Value 2"))
                   ),
                   dateRangeInput(
                     inputId = "daterange",
                     label = "Select Date Range",
                     start = as.Date(paste0(substr(Sys.Date(), 1, 4), "-04-07")),
                     end = Sys.Date(),
                     min = as.Date(paste0(start_year, "-04-07")),
                     max = Sys.Date(),
                     format = "mm/dd/yyyy",
                     separator = "To"),
      ),
      mainPanel(uiOutput("plot"))
    ))
  
  
  server <- function(input, output, session)
  {
    #w <- Waiter$new()
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
        hitter <- query_hitter(input$user_name_input, as.integer(substr(input$daterange[1], start = 1, stop = 4)), as.integer(substr(input$daterange[2], start = 1, stop = 4)), input$daterange[1], input$daterange[2])
        hitter <- clean_statcast_data(hitter, start_year, end_year)
        #hitter <- hitter %>% filter(game_date >= input$daterange[1], game_date <= input$daterange[2])
        hitter_hits <- hitter %>% filter(events %in% c("Home Run","Triple","Double","Single"))
        hitter_outs <- hitter %>% filter(events %in% c("Strike Out","Force Out","Double Play","Field Out","Sac Fly","Sac Bunt"))
        hitter_batted_balls <- hitter %>% filter(description %in% c("hit_into_play_no_out","hit_into_play","hit_into_play_score"))
        hitter_k <- hitter %>% filter(events == 'Strike Out')
        hitter_whiffs <- hitter %>% filter(description %in% c("swinging_strike_blocked","swinging_strike"))
        hitter_abs <- hitter %>% filter(events %in% c("Strike Out","Field Out","Single","Double",
                                                      "Triple","Home Run","Force Out","Double Play"))
      } else if (input$user_type_input == "Pitcher" & input$user_name_input2 != "Choose...") {
        pitcher <- query_pitcher(input$user_name_input2, as.integer(substr(input$daterange[1], start = 1, stop = 4)), as.integer(substr(input$daterange[2], start = 1, stop = 4)), input$daterange[1], input$daterange[2])
        pitcher <- clean_statcast_data(pitcher, start_year, end_year) 
        #pitcher <- pitcher %>% filter(game_date >= input$daterange[1], game_date <= input$daterange[2])
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
          output$plot1 <- renderPlot({spray_chart_advanced_generic(hitter_hits, title = paste0("Hits Spray Chart ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot1")
        }
        else if (input$user_visual_type == "Spray Chart - All Outs" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot2 <- renderPlot({spray_chart_advanced_generic(hitter_outs %>% filter(bb_type != 'null'), title = paste0("Outs Spray Chart ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot2")
        }
        else if (input$user_visual_type == "Spray Chart - Home Hits" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot2 <- renderPlot({spray_chart_advanced_home(hitter_hits %>% filter(bb_type != 'null'), title = paste0("Hits Spray Chart ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot2")
        }
        else if (input$user_visual_type == "Spray Chart - Home Outs" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot2 <- renderPlot({spray_chart_advanced_home(hitter_outs %>% filter(bb_type != 'null'), title = paste0("Outs Spray Chart ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot2")
        }
        else if (input$user_visual_type == "Spray Chart - Distributions" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot2 <- renderPlot({spray_chart_distributions(hitter, title = paste0(format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          div(plotOutput("plot2", height=575, width = 575), align="center")
        }
        else if (input$user_visual_type == "Pitch Chart - Hits" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot3 <- renderPlot({pitch_chart_batter(hitter_hits, title = paste0("Hits Location ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")), TRUE)})
          plotOutput("plot3")
        }
        else if (input$user_visual_type == "Pitch Chart - Outs" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot4 <- renderPlot({pitch_chart_batter(hitter_outs, title = paste0("Outs Achieved by Location ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")), FALSE, TRUE)})
          plotOutput("plot4")
        }
        else if (input$user_visual_type == "Pitch Chart - SO" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot5 <- renderPlot({pitch_chart_batter(hitter_k, title = paste0("Strikeouts by Location ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot5")
        }
        else if (input$user_visual_type == "Pitch Chart - Whiffs" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot6 <- renderPlot({pitch_chart_batter(hitter_whiffs, title = paste0("Whiffs by Location ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot6")
        }
        else if (input$user_visual_type == "Pitch Chart - Outside Swings" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot7 <- renderPlot({pitch_chart_batter_chase(hitter, title = paste0("Out of Zone Swings by Location ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot7")
        }
        else if (input$user_visual_type == "Pitch Chart - Pitch Type" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot1234 <- renderPlot({pitch_chart_pitch_type(hitter, title = paste0("Location by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot1234")
        }
        else if (input$user_visual_type == "Contact Type" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot7 <- renderPlot({contact_chart(hitter, title = paste0("Batted Balls Contact % ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot7")
        }
        else if (input$user_visual_type == "Exit Velocity" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$plot8 <- renderPlot({exit_velocity(hitter_batted_balls, start_year = as.integer(substr(input$daterange[1], start = 1, stop = 4)), end_year = as.integer(substr(input$daterange[2], start = 1, stop = 4)), title = paste0("Distribution of Exit Velocity ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot8")
        }
        else if (input$user_visual_type == "Batted Ball Type" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({batted_ball_type(hitter_batted_balls, title = paste0("Batted Ball Rates ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "IF Shift" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({if_shift_usage(hitter, title = paste0("Metrics by IF Positioning ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "OF Shift" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({of_shift_usage(hitter, title = paste0("Metrics by OF Positioning ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Batter Basic Stats" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({batter_basic_stats(hitter, start_year = as.integer(substr(input$daterange[1], start = 1, stop = 4)), end_year = as.integer(substr(input$daterange[2], start = 1, stop = 4)),title = paste0("Overall Stats ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Batter Metrics by Pitcher Side" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({batter_stats(hitter, title = paste0("Metrics ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Batter Metrics by Pitch Type" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({stats_by_pitch_type(hitter, title = paste0("Metrics by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Plate Discipline" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({plate_discipline(hitter, title = paste0("Plate Discipline Metrics ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Plate Discipline by Pitch Type" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({plate_discipline_by_pitch_type(hitter, title = paste0("Plate Discipline Metrics by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Whiff Rates" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({whiff_by_pitch_type(hitter, paste0("Whiff % ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type == "Batted Ball Type by Pitch" & input$user_name_input != "Choose..." & input$user_heat_input == "Choose...")
        {
          output$tbl <- renderText({batted_ball_by_pitch_type(hitter, paste0("Batted Ball Rates by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_heat_input == "Called Strike Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_called(hitter)
          output$plot22 <- renderPlot({heat_map(hitter2, var = "Strike", binary = T, legend_title = "Probability", title = paste0("Probability of Strike ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot22")
        }
        else if (input$user_heat_input == "Swing Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_all(hitter)
          output$plot23 <- renderPlot({heat_map(hitter2, var = "Swing", binary = T, legend_title = "Probability", title = paste0("Probability of Swing ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot23")
        }
        else if (input$user_heat_input == "Swing Probability by Pitch Type" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_all(hitter)
          output$plot23 <- renderPlot({heat_map_by_pitch_type(hitter2, var = "Swing", binary = T, legend_title = "Probability", title = paste0("Probability of Swing by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot23")
        }
        else if (input$user_heat_input == "Contact Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_swing(hitter)
          output$plot24 <- renderPlot({heat_map(hitter2, var = "Contact", binary = T, legend_title = "Probability", title = paste0("Probability of Contact Given Swing ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot24")
        }
        else if (input$user_heat_input == "Contact Probability by Pitch Type" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_swing(hitter)
          output$plot24 <- renderPlot({heat_map_by_pitch_type(hitter2, var = "Contact", binary = T, legend_title = "Probability", title = paste0("Probability of Contact Given Swing by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot24")
        }
        else if (input$user_heat_input == "Whiff Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_swing(hitter)
          output$plot25 <- renderPlot({heat_map(hitter2, var = "Miss", binary = T, legend_title = "Probability", title = paste0("Probability of Whiff Given Swing ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot25")
        }
        else if (input$user_heat_input == "Whiff Probability by Pitch Type" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_swing(hitter)
          output$plot25 <- renderPlot({heat_map_by_pitch_type(hitter2, var = "Miss", binary = T, legend_title = "Probability", title = paste0("Probability of Whiff Given Swing by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot25")
        }
        else if (input$user_heat_input == "Ball in Play Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_swing(hitter)
          output$plot26 <- renderPlot({heat_map(hitter2, var = "InPlay", binary = T, legend_title = "Probability", title = paste0("Probability of Ball in Play Given Swing ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot26")
        }
        else if (input$user_heat_input == "Ball in Play Probability by Pitch Type" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_swing(hitter)
          output$plot26 <- renderPlot({heat_map_by_pitch_type(hitter2, var = "InPlay", binary = T, legend_title = "Probability", title = paste0("Probability of Ball in Play Given Swing by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot26")
        }
        else if (input$user_heat_input == "Exit Velocity" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_inplay(hitter)
          output$plot27 <- renderPlot({heat_map(hitter2, var = "launch_speed", binary = F, legend_title = "Exit Velocity", title = paste0("Exit Velocity ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot27")
        }
        else if (input$user_heat_input == "Exit Velocity by Pitch Type" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_inplay(hitter)
          output$plot27 <- renderPlot({heat_map_by_pitch_type(hitter2, var = "launch_speed", binary = F, legend_title = "Exit Velocity", title = paste0("Exit Velocity by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot27")
        }
        else if (input$user_heat_input == "Launch Angle" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_inplay(hitter)
          output$plot28 <- renderPlot({heat_map(hitter2, var = "launch_angle", binary = F, legend_title = "Launch Angle", title = paste0("Launch Angle ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot28")
        }
        else if (input$user_heat_input == "Spray Angle" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_inplay(hitter)
          output$plot29 <- renderPlot({heat_map(hitter2, var = "phi", binary = F, legend_title = "Spray Angle", title = paste0("Spray Angle ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "wOBA" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_inplay(hitter)
          output$plot29 <- renderPlot({heat_map(hitter2, var = "woba_value", binary = FALSE, legend_title = "wOBA", title = paste0("wOBA ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "xBA" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_inplay(hitter)
          output$plot29 <- renderPlot({heat_map(hitter2, var = "estimated_ba_using_speedangle", binary = FALSE, legend_title = "xBA", title = paste0("xBA ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "xwOBA" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_inplay(hitter)
          output$plot29 <- renderPlot({heat_map(hitter2, var = "estimated_woba_using_speedangle", binary = FALSE, legend_title = "xwOBA", title = paste0("xwOBA ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "Home Run Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_all(hitter)
          output$plot29 <- renderPlot({heat_map(hitter2, var = "is_hr", binary = T, legend_title = "Probability", title = paste0("Probability of HR ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "Home Run Probability by Pitch Type" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_all(hitter)
          output$plot29 <- renderPlot({heat_map_by_pitch_type(hitter2, var = "is_hr", binary = T, legend_title = "Probability", title = paste0("Probability of HR by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot29")
        }
        else if (input$user_heat_input == "Hit Probability" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_all(hitter)
          output$plot29 <- renderPlot({heat_map(hitter2, var = "is_hit", binary = T, legend_title = "Probability", title = paste0("Probability of Hit ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot29")
        } 
        else if (input$user_heat_input == "Hit Probability by Pitch Type" & input$user_name_input != "Choose..." & input$user_visual_type == "Choose...")
        {
          hitter2 <- setup_all(hitter)
          output$plot29 <- renderPlot({heat_map_by_pitch_type(hitter2, var = "is_hit", binary = T, legend_title = "Probability", title = paste0("Probability of Hit by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot29")
        } 
      }
      
      
      
      
      else if (input$user_type_input == "Pitcher")
      {
        if (input$user_visual_type2 == "Spray Chart - All Hits" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot9 <- renderPlot({spray_chart_advanced_generic(pitcher_hits, title = paste0("Hits Given Up ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot9")
        }
        else if (input$user_visual_type2 == "Spray Chart - All Outs" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot10 <- renderPlot({spray_chart_advanced_generic(pitcher_outs %>% filter(bb_type != 'null'), title = paste0("Outs Achieved ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot10")
        }
        else if (input$user_visual_type2 == "Spray Chart - Home Hits" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot10 <- renderPlot({spray_chart_advanced_home(pitcher_hits %>% filter(bb_type != 'null'), title = paste0("Hits Given Up ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot10")
        }
        else if (input$user_visual_type2 == "Spray Chart - Home Outs" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot10 <- renderPlot({spray_chart_advanced_home(pitcher_outs %>% filter(bb_type != 'null'), title = paste0("Outs Achieved ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot10")
        }
        else if (input$user_visual_type2 == "Spray Chart - Distributions" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot10 <- renderPlot({spray_chart_distributions(pitcher, title = paste0(format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          div(plotOutput("plot10", height=575, width = 575), align="center")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Hits" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot11 <- renderPlot({pitch_chart_pitcher(pitcher_hits, title = paste0("Hits Given Up by Location ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")), TRUE)})
          plotOutput("plot11")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Outs" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot12 <- renderPlot({pitch_chart_pitcher(pitcher_outs, title = paste0("Outs Achieved by Location ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")), FALSE, TRUE)})
          plotOutput("plot12")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Pitch Type" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot1234 <- renderPlot({pitch_chart_pitch_type(pitcher, title = paste0("Location by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot1234")
        }
        else if (input$user_visual_type2 == "Pitch Chart - SO" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot13 <- renderPlot({pitch_chart_pitcher(pitcher_k, title = paste0("Strikeouts by Location ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot13")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Whiffs" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot14 <- renderPlot({pitch_chart_pitcher(pitcher_whiffs, title = paste0("Whiffs by Location ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot14")
        }
        else if (input$user_visual_type2 == "Pitch Chart - First Pitch" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot1001 <- renderPlot({pitch_chart_pitcher_first_pitch(pitcher, title = paste0("First Pitch Location ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot1001")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Ahead" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot1002 <- renderPlot({pitch_chart_pitcher_ahead(pitcher, title = paste0("Ahead in Count Location ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot1002")
        }
        else if (input$user_visual_type2 == "Pitch Chart - Behind" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot1003 <- renderPlot({pitch_chart_pitcher_behind(pitcher, title = paste0("Behind in Count Location ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot1003")
        }
        else if (input$user_visual_type2 == "Pitch Arsenal" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot15 <- renderPlot({pitch_arsenal(pitcher, title = paste0(" Pitch Distribution ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot15")
        }
        else if (input$user_visual_type2 == "Pitch Movement" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot16 <- renderPlot({pitch_movement(pitcher, title = paste0("Pitch Movement ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot16")
        }
        else if (input$user_visual_type2 == "Pitch Velocity" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot17 <- renderPlot({pitch_velocity(pitcher, title = paste0("Velocity by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot17")
        }
        else if (input$user_visual_type2 == "Pitch Spin Rate" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot18 <- renderPlot({pitch_spinrate(pitcher, title = paste0("Spin Rate by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot18")
        }
        else if (input$user_visual_type2 == "Pitch Spin Axis" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot100 <- renderPlot({plot_spin_axis(pitcher, date1 = substr(format(as.Date(input$daterange[1]), "%m/%d/%Y"),9,10), date2 = substr(format(as.Date(input$daterange[2]), "%m/%d/%Y"),9,10))})
          plotOutput("plot100")
        }
        else if (input$user_visual_type2 == "Contact Type" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot19 <- renderPlot({contact_chart(pitcher, title = paste0("Batted Balls Contact Type % ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot19")
        }
        else if (input$user_visual_type2 == "Release Points" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot123 <- renderPlot({release_position(pitcher, title = paste0("Release Position ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot123")
        }
        else if (input$user_visual_type2 == "Exit Velocity" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot20 <- renderPlot({exit_velocity(pitcher_batted_balls, start_year = as.integer(substr(input$daterange[1], start = 1, stop = 4)), end_year = as.integer(substr(input$daterange[2], start = 1, stop = 4)), title = paste0("Distribution of Exit Velocity ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot20")
        }
        else if (input$user_visual_type2 == "Batted Ball Type" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({batted_ball_type(pitcher_batted_balls, title = paste0("Batted Ball Rates ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "IF Shift" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({if_shift_usage(pitcher, title = paste0("Metrics by IF Positioning ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "OF Shift" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({of_shift_usage(pitcher, title = paste0("Metrics by OF Positioning ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Pitcher Metrics by Batter Side" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({pitcher_stats(pitcher, title = paste0("Metrics Against ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Pitcher Basic Stats" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          #w$show()
          output$tbl <- renderText({pitcher_basic_stats(pitcher, title = paste0("Overall Stats ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
          #w$hide()
        }
        else if (input$user_visual_type2 == "Pitcher Metrics by Pitch Type" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({stats_by_pitch_type(pitcher, title = paste0("Metrics by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Plate Discipline" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({plate_discipline(pitcher, title = paste0("Plate Discipline Metrics ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Plate Discipline by Pitch Type" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({plate_discipline_by_pitch_type(pitcher, title = paste0("Plate Discipline Metrics by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Whiff Rates" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({whiff_by_pitch_type(pitcher, title = paste0("Whiff % ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Batted Ball Type by Pitch" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({batted_ball_by_pitch_type(pitcher, title = paste0("Batted Ball Rates by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_visual_type2 == "Run Value" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$tbl <- renderText({run_value_table(pitcher, title = paste0("Run Value by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%Y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%Y")))})
          htmlOutput("tbl")
        }
        else if (input$user_heat_input2 == "Pitch Chart - Tendencies" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          output$plot1002 <- renderPlot({pitch_chart_density(pitcher, title = paste0("Pitch Location Tendencies ",  format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot1002")
        }
        else if (input$user_heat_input2 == "Called Strike Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_called(pitcher)
          output$plot30 <- renderPlot({heat_map(pitcher2, var = "Strike", binary = T, legend_title = "Probability", title = paste0("Probability of Called Strike ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot30")
        }
        else if (input$user_heat_input2 == "Swing Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_all(pitcher)
          output$plot31 <- renderPlot({heat_map(pitcher2, var = "Swing", binary = T, legend_title = "Probability", title = paste0("Probability of Swing ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot31")
        }
        else if (input$user_heat_input2 == "Swing Probability by Pitch Type" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_all(pitcher)
          output$plot31 <- renderPlot({heat_map_by_pitch_type(pitcher2, var = "Swing", binary = T, legend_title = "Probability", title = paste0("Probability of Swing by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot31")
        }
        else if (input$user_heat_input2 == "Contact Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_swing(pitcher)
          output$plot32 <- renderPlot({heat_map(pitcher2, var = "Contact", binary = T, legend_title = "Probability", title = paste0("Probability of Contact Given Swing ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot32")
        }
        else if (input$user_heat_input2 == "Contact Probability by Pitch Type" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_swing(pitcher)
          output$plot32 <- renderPlot({heat_map_by_pitch_type(pitcher2, var = "Contact", binary = T, legend_title = "Probability", title = paste0("Probability of Contact Given Swing by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot32")
        }
        else if (input$user_heat_input2 == "Whiff Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_swing(pitcher)
          output$plot33 <- renderPlot({heat_map(pitcher2, var = "Miss", binary = T, legend_title = "Probability", title = paste0("Probability of Whiff Given Swing ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot33")
        }
        else if (input$user_heat_input2 == "Whiff Probability by Pitch Type" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_swing(pitcher)
          output$plot33 <- renderPlot({heat_map_by_pitch_type(pitcher2, var = "Miss", binary = T, legend_title = "Probability", title = paste0("Probability of Whiff Given Swing by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot33")
        }
        else if (input$user_heat_input2 == "Ball in Play Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_swing(pitcher)
          output$plot34 <- renderPlot({heat_map(pitcher2, var = "InPlay", binary = T, legend_title = "Probability", title = paste0("Probability of Ball in Play Given Swing ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot34")
        }
        else if (input$user_heat_input2 == "Ball in Play Probability by Pitch Type" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_swing(pitcher)
          output$plot34 <- renderPlot({heat_map_by_pitch_type(pitcher2, var = "InPlay", binary = T, legend_title = "Probability", title = paste0("Probability of Ball in Play Given Swing by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot34")
        }
        else if (input$user_heat_input2 == "Exit Velocity" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_inplay(pitcher)
          output$plot35 <- renderPlot({heat_map(pitcher2, var = "launch_speed", binary = F, legend_title = "Exit Velocity", title = paste0("Exit Velocity ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot35")
        }
        else if (input$user_heat_input2 == "Exit Velocity by Pitch Type" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_inplay(pitcher)
          output$plot35 <- renderPlot({heat_map_by_pitch_type(pitcher2, var = "launch_speed", binary = F, legend_title = "Exit Velocity", title = paste0("Exit Velocity by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot35")
        }
        else if (input$user_heat_input2 == "Launch Angle" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_inplay(pitcher)
          output$plot36 <- renderPlot({heat_map(pitcher2, var = "launch_angle", binary = F, legend_title = "Launch Angle", title = paste0("Launch Angle ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot36")
        }
        else if (input$user_heat_input2 == "Spray Angle" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_inplay(pitcher)
          output$plot37 <- renderPlot({heat_map(pitcher2, var = "phi", binary = F, legend_title = "Spray Angle", title =paste0("Spray Angle ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot37")
        }
        else if (input$user_heat_input2 == "wOBA" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_inplay(pitcher)
          output$plot38 <- renderPlot({heat_map(pitcher2, var = "woba_value", binary = FALSE, legend_title = "wOBA", title = paste0("wOBA ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot38")
        }
        else if (input$user_heat_input2 == "xBA" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_inplay(pitcher)
          output$plot39 <- renderPlot({heat_map(pitcher2, var = "estimated_ba_using_speedangle", binary = FALSE, legend_title = "xBA", title = paste0("xBA ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot39")
        }
        else if (input$user_heat_input2 == "xwOBA" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_inplay(pitcher)
          output$plot40 <- renderPlot({heat_map(pitcher2, var = "estimated_woba_using_speedangle", binary = FALSE, legend_title = "xwOBA", title = paste0("xwOBA ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot40")
        }
        else if (input$user_visual_type2 == "Pitch Usage by Count" & input$user_name_input2 != "Choose..." & input$user_heat_input2 == "Choose...")
        {
          output$plot41 <- renderPlot({pitch_usage_by_count(pitcher, title = paste0("Pitch Usage by Count ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot41")
        }
        else if (input$user_heat_input2 == "Home Run Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_all(pitcher)
          output$plot42 <- renderPlot({heat_map(pitcher2, var = "is_hr", binary = T, legend_title = "Probability", title = paste0("Probability of HR ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot42")
        }
        else if (input$user_heat_input2 == "Home Run Probability by Pitch Type" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_all(pitcher)
          output$plot42 <- renderPlot({heat_map_by_pitch_type(pitcher2, var = "is_hr", binary = T, legend_title = "Probability", title = paste0("Probability of HR by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot42")
        }
        else if (input$user_heat_input2 == "Hit Probability" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_all(pitcher)
          output$plot43 <- renderPlot({heat_map(pitcher2, var = "is_hit", binary = T, legend_title = "Probability", title = paste0("Probability of Hit ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot43")
        }
        else if (input$user_heat_input2 == "Hit Probability by Pitch Type" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          pitcher2 <- setup_all(pitcher)
          output$plot43 <- renderPlot({heat_map_by_pitch_type(pitcher2, var = "is_hit", binary = T, legend_title = "Probability", title = paste0("Probability of Hit by Pitch Type ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot43")
        }
        else if (input$user_heat_input2 == "Run Value" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          output$plot43 <- renderPlot({heat_map_rv(pitcher, legend_title = "Run Value", title = paste0("Run Value by Pitch ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot43")
        }
        else if (input$user_heat_input2 == "Run Value 2" & input$user_name_input2 != "Choose..." & input$user_visual_type2 == "Choose...")
        {
          output$plot43 <- renderPlot({heat_map_rv2(pitcher, legend_title = "Run Value", title = paste0("Run Value by Pitch ", format(as.Date(input$daterange[1]), "%m/%d/%y"), " to ", format(as.Date(input$daterange[2]), "%m/%d/%y")))})
          plotOutput("plot43")
        }
      }
    })
  }
  return(list(ui, server))
}