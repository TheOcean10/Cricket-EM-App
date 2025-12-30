library(shiny)
library(bslib)
library(ggplot2)
library(stringr)
library(readxl)
library(reshape2)
library(dplyr)
source("EM_Algorithm.R")

ui <- page_fluid(
  h1("Cricket Performance"),
  navset_pill( 
    
#=======================Tab 1======================================== 
    nav_panel(
      "Problem Statement",
      fluidPage(
        h2("Why Standard Batting Averages Can Mislead"),
        p("In cricket, a batsman's average is calculated as total runs divided by number of times dismissed. However, when a player remains 'not out', that inning is excluded from the denominator."),
        p("This inflates batting averages for players with many 'not out' innings, giving an inaccurate measure of true performance."),
        br(),
        h3("What is the EM Algorithm and How Does it Help?"),
        p("The Expectation-Maximization (EM) algorithm estimates player performance by adjusting for 'right-censored' data, innings where the player wasn't dismissed."),
        p("It produces more accurate batting averages that reflect the true skill of the batsman."),
        br(),
        h3("Why This Matters"),
        tags$ul(
          tags$li("Fairer player rankings and selections."),
          tags$li("Better contract decisions for players."),
          tags$li("Accurate historical and current performance analysis.")
        ),
        br(),
        h3("Using This App"),
        p("Navigate through the tabs to explore player statistics, advanced analyses, and see how the EM algorithm adjusts traditional cricket stats."),
        br(),
        img(src = "tab_1_pic.png", height = "800px"),
        p(em("Figure: Illustration of how 'not out' innings affect batting averages."))
      )
    ), 
    
# =======================Tab 2======================================== 
    nav_panel(
      "General Player Statistics",
      sidebarLayout(
        sidebarPanel(
          selectInput("player_b", "Select Player", choices = NULL),
          checkboxGroupInput("metrics_b", "Select Metrics:",
                             choices = c(
                             "Runs" = "runs",
                             "Balls Faced" = "balls_faced",
                             "Strike Rate" = "SR"
                             ),
                            selected = "runs")
      ),
      mainPanel(
        plotOutput("lineplot_b"),
        tableOutput("summary_b")
      )
      )
    ),
    
#  =======================Tab 3========================================    
    nav_panel(
      "Detailed Statistical Graphs",
      sidebarLayout(
        sidebarPanel(
          selectInput("player_c", "Select Player", choices = NULL),
          selectInput("metric_c", "Select Metric:",
                      choices = c("Runs" = "runs",
                                  "Balls Faced" = "balls_faced",
                                  "Strike Rate" = "SR")),
          checkboxGroupInput("plot_types_c", "Select Plot Types:",
                             choices = c("Box Plot" = "box",
                                         "Histogram" = "hist",
                                         "Time Series" = "time"),
                             selected = c("box", "hist", "time"))
        ),
        mainPanel(
          uiOutput("plotsUI_c")
        )
      )
      ),
    
# =======================Tab 4========================================   
    nav_panel(
      "EM Algorithm",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            inputId = "metric",
            label = "Choose statistics to display",
            choices = list(
              "Runs (original)" = "runs",
              "Runs (EM adjusted)" = "runs_updated",
              "Balls Faced (original)" = "balls_faced",
              "Balls Faced (EM adjusted)" = "balls_faced_updated"
            ),
            selected = "runs"
          ),
          selectInput(
            inputId = "player",
            label = "Select Player:",
            choices = NULL
          )
        ),
        mainPanel(
          uiOutput("plotsUI"),
          br(),
          h4("Comparison of Regular vs EM-Adjusted Statistics"),
          tableOutput("em_table")
        )
        )
    ),
    id = "tab"
  )
)
  





#======================================================================================
# ===================SERVER============================================================
# =====================================================================================


server <- function(input, output, session) {
  
  results <- cricket_em_seq(path = "Dataset.xlsx")
  summary_df <- results[[1]]   
  all_results <- results[[2]]
  
  my_data <- do.call(rbind, lapply(all_results, function(df) {
    df$Player <- df$batter_names.j1.
    df
  }))
  
  my_data$SR <- with(my_data, ifelse(balls_faced > 0, (runs / balls_faced) * 100, NA))
  
  observe({
    updateSelectInput(session, "player", choices = unique(my_data$Player))
  })
  
# =======================Tab 2=====================================================
observe({
  updateSelectInput(session, "player_b", choices = unique(my_data$Player))
})
  
output$lineplot_b <- renderPlot({
  req(input$player_b, input$metrics_b)
  player_data <- subset(my_data, Player == input$player_b)
  
  plot_df <- player_data[, c("Player", input$metrics_b), drop = FALSE]
  plot_df$Inning <- seq_len(nrow(plot_df))
  plot_df_long <- reshape2::melt(plot_df, id.vars = c("Player", "Inning"),
                                 variable.name = "Metric", value.name = "Value")
  
  ggplot(plot_df_long, aes(x = Inning, y = Value, color = Metric)) +
    geom_line(size = 1) +
    geom_point() +
    labs(title = paste("Performance Metrics for", input$player_b),
         x = "Innings", y = "Value") +
    theme_minimal()
})

output$summary_b <- renderTable({
  req(input$player_b, input$metrics_b)
  player_data <- subset(my_data, Player == input$player_b)
  summary_stats <- sapply(player_data[, input$metrics_b, drop = FALSE], summary)
  as.data.frame(t(summary_stats))
}, rownames = TRUE)
  

# =======================Tab 3=====================================================

observe({
  updateSelectInput(session, "player_c", choices = unique(my_data$Player))
})

output$plotsUI_c <- renderUI({
  req(input$plot_types_c)
  plot_output_list <- lapply(seq_along(input$plot_types_c), function(i) {
    plotname <- paste0("plot_c_", i)
    plotOutput(plotname, height = 400)
  })
  do.call(tagList, plot_output_list)
})

observe({
  req(input$player_c, input$metric_c, input$plot_types_c)
  player_data <- subset(my_data, Player == input$player_c)
  
  lapply(seq_along(input$plot_types_c), function(i) {
    local({
      plot_type <- input$plot_types_c[i]
      plotname <- paste0("plot_c_", i)
      metric <- input$metric_c
      
      output[[plotname]] <- renderPlot({
        df <- data.frame(Inning = seq_len(nrow(player_data)),
                         Value = as.numeric(player_data[[metric]]))
        if (plot_type == "box") {
          ggplot(df, aes(y = Value)) +
            geom_boxplot(fill = "skyblue") +
            labs(title = paste("Box Plot of", metric, "for", input$player_c),
                 y = metric) +
            theme_minimal()
          
        } else if (plot_type == "hist") {
          ggplot(df, aes(x = Value)) +
            geom_histogram(bins = 20, fill = "orange", color = "black") +
            labs(title = paste("Histogram of", metric, "for", input$player_c),
                 x = metric, y = "Frequency") +
            theme_minimal()
          
        } else if (plot_type == "time") {
          ggplot(df, aes(x = Inning, y = Value)) +
            geom_line(color = "steelblue", size = 1) +
            geom_point(color = "steelblue") +
            labs(title = paste("Time Series of", metric, "for", input$player_c),
                 x = "Innings", y = metric) +
            theme_minimal()
        }
      })
    })
  })
})



# =======================Tab 4=====================================================
  
observe({
  req(input$player, input$metric)
  player_data <- subset(my_data, Player == input$player)
  player_data$Inning <- seq_len(nrow(player_data))
  
  output$plotsUI <- renderUI({
    plotOutput("em_plot", height = 500)
  })
  
  output$em_plot <- 
    renderPlot({
    # Filter only metrics present in data
    available_metrics <- input$metric[input$metric %in% colnames(player_data)]
    
    # Prepare long format for ggplot
    plot_df <- reshape2::melt(
      player_data[, c("Inning", available_metrics)],
      id.vars = "Inning",
      variable.name = "Metric",
      value.name = "Value"
    )
    
    ggplot(plot_df, aes(x = Inning, y = Value, color = Metric, group = Metric)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = paste("EM vs Original for", input$player),
        x = "Innings",
        y = "Value"
      ) +
      theme_minimal()
  })
  
  output$em_table <- renderTable({
    req(input$player)
    player_data <- subset(my_data, Player == input$player)
    
    comparison <- data.frame(
      Metric = c("Runs", "Balls Faced"),
      Original = c(mean(player_data$runs, na.rm = TRUE),
                   mean(player_data$balls_faced, na.rm = TRUE)),
      EM_Adjusted = c(mean(player_data$runs_updated, na.rm = TRUE),
                      mean(player_data$balls_faced_updated, na.rm = TRUE))
    )
    
    comparison <- data.frame(
      Metric = c("Runs", "Balls Faced"),
      Original = c(mean(player_data$runs, na.rm = TRUE),
                   mean(player_data$balls_faced, na.rm = TRUE)),
      EM_Adjusted = c(mean(player_data$runs_updated, na.rm = TRUE),
                      mean(player_data$balls_faced_updated, na.rm = TRUE))
    )
    
    comparison <- comparison |>
      dplyr::mutate(
        Difference = EM_Adjusted - Original,
        dplyr::across(c(Original, EM_Adjusted, Difference), ~ round(.x, 2))
      )
  })
})
}


shinyApp(ui = ui, server = server)
