library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Law of Large Numbers: Convergence of the Sample Mean"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Sample size (n):",
                  min = 10, max = 1000, value = 10, step = 1)
    ),
    mainPanel(
      plotOutput("p_density", height = 300),
      plotOutput("p_convergence", height = 300)
    )
  )
)

server <- function(input, output, session) {
  
  # ---- FIXED POPULATION ----
  #set.seed(123)
  N_pop <- 100000
  Pop <- rnorm(N_pop, mean = 0, sd = 1)
  true_mean <- mean(Pop)
  
  output$p_density <- renderPlot({
    n <- input$n
    set.seed(n)
    sample_data <- sample(Pop, n)
    
    ggplot() +
      geom_density(aes(x = Pop), color = "black", linetype = "dotted", linewidth = 1) +
      geom_histogram(aes(x = sample_data, y = after_stat(density)), 
                     bins = 30, fill = "steelblue", alpha = 0.5, color = "white") +
      geom_vline(xintercept = true_mean, color = "red", linetype = "dashed") +
      xlim(-3, 3) + 
      ylim(0, 0.8) +
      labs(
        title = paste("Sample vs. Population (n =", n, ")"),
        subtitle = "Small samples may not reflect the population distribution",
        x = "Value",
        y = "Density"
      ) +
      theme_minimal(base_size = 14)
  })
  
  output$p_convergence <- renderPlot({
    n <- input$n
    set.seed(n)
    sample_data <- sample(Pop, n)
    
    running_mean <- cumsum(sample_data) / seq_along(sample_data)
    
    df <- data.frame(
      n = seq_along(running_mean),
      mean = running_mean
    )
    
    ggplot(df, aes(x = n, y = mean)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_hline(yintercept = true_mean, color = "red", linetype = "dotted", linewidth = 1) +
      labs(
        title = "Convergence of the Sample Mean",
        subtitle = "As n increases, the sample mean approaches the true mean",
        x = "Number of observations",
        y = "Running mean"
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)