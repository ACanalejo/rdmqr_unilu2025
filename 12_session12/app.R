library(shiny)
library(ggplot2)
library(viridis) # For Viridis color palette

# UI
ui <- fluidPage(
  titlePanel("Interactive p-Value and t-Distribution Visualization"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("b1", "Estimated Coefficient (β1):", min = -5, max = 5, value = 0, step = 0.1),
      sliderInput("se_b1", "Standard Error (SE(β1)):", min = 0.1, max = 2, value = 0.5, step = 0.1),
      sliderInput("df", "Degrees of Freedom (df):", min = 1, max = 60, value = 30, step = 1),
      hr(),
      h4("Key Outputs:"),
      verbatimTextOutput("t_stat_output"),
      verbatimTextOutput("p_val_output"),
      hr(),
      h4("Formulas:"),
      tags$div(
        style = "font-size: 16px; background-color: #f7f7f7; padding: 10px; border-radius: 5px; margin-top: 10px;",
        withMathJax("$$\\hat{\\beta} = \\frac{\\text{Cov}(X, Y)}{\\text{Var}(X)}$$"),
        withMathJax("$$SE(\\hat{\\beta}) = \\sqrt{\\frac{s_{\\text{residual}}^2}{\\text{Var}(X)}}$$"),
        withMathJax("$$t = \\frac{\\hat{\\beta}}{SE(\\hat{\\beta})}$$"),
        withMathJax("$$df = N - k - 1$$")
      )
    ),
    mainPanel(
      plotOutput("p_value_plot"),
      plotOutput("t_dist_plot")
    )
  )
)

# Server
server <- function(input, output) {
  # Reactive calculation of t-statistic and p-value
  reactive_t_stat <- reactive({
    input$b1 / input$se_b1
  })
  
  reactive_p_value <- reactive({
    2 * (1 - pt(abs(reactive_t_stat()), df = input$df))
  })
  
  
# Plot of p-values as a function of t-values
output$p_value_plot <- renderPlot({
  # Range of t-values
  t_values <- seq(-4, 4, length.out = 1000)
  p_values <- 2 * (1 - pt(abs(t_values), df = input$df))
  
  # Critical t-values for the two-tailed test
  critical_t <- qt(0.975, df = input$df)
  
  # Observed values
  t_stat <- reactive_t_stat()
  p_val <- reactive_p_value()
  
  # Determine stars for observed p-value
  stars <- if (!is.na(p_val)) {
    if (p_val < 0.001) {
      "***"
    } else if (p_val < 0.01) {
      "**"
    } else if (p_val < 0.05) {
      "*"
    } else {
      ""
    }
  } else {
    ""
  }
  
  # Format p-value for readability
  formatted_p_val <- if (p_val < 0.0001) {
    format(p_val, scientific = TRUE, digits = 5) # Use scientific notation for small values
  } else {
    round(p_val, 5) # Use standard rounding for larger values
  }
  
  # Data for plotting
  plot_data <- data.frame(t_values = t_values, p_values = p_values)
  
  ggplot(plot_data, aes(x = t_values, y = p_values)) +
    # Line for p-value curve
    geom_line(color = viridis(1, option = "C"), size = 1) +
    
    # Shaded areas in the tails (beyond critical t-values)
    geom_ribbon(data = subset(plot_data, t_values > critical_t), 
                aes(ymin = 0, ymax = p_values), fill = viridis(1, option = "D"), alpha = 0.3) +
    geom_ribbon(data = subset(plot_data, t_values < -critical_t), 
                aes(ymin = 0, ymax = p_values), fill = viridis(1, option = "D"), alpha = 0.3) +
    
    # Vertical dashed threshold lines for critical t-values
    geom_vline(xintercept = critical_t, color = viridis(1, option = "E"), linetype = "dashed", size = 1) +
    geom_vline(xintercept = -critical_t, color = viridis(1, option = "E"), linetype = "dashed", size = 1) +
    
    # Observed point for t-stat and p-val
    geom_point(aes(x = t_stat, y = p_val), color = viridis(1, option = "C"), size = 3) +
    
    # Labels and annotations
    labs(
      title = "p-Value as a Function of t-Values",
      x = "t-Value",
      y = "p-Value"
    ) +
    annotate(
      "text",
      x = 2.8,
      y = 0.8, 
      label = paste0("Observed p = ", formatted_p_val, " ", stars),
      color = viridis(1, option = "C"),
      size = 5,
      hjust = 0
    ) +
    
    # Theme adjustments
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16),
      axis.title = element_text(size = 12)
    )
})
  
  # Plot of the t-distribution
output$t_dist_plot <- renderPlot({
  # Range of t-values
  t_values <- seq(-4, 4, length.out = 1000)
  density <- dt(t_values, df = input$df)
  
  # Critical t-values for the two-tailed test
  critical_t <- qt(0.975, df = input$df)
  
  # Observed t-statistic
  t_stat <- reactive_t_stat()
  
  # Data for plotting
  plot_data <- data.frame(t_values = t_values, density = density)
  
  ggplot(plot_data, aes(x = t_values, y = density)) +
    # Line for t-distribution
    geom_line(color = viridis(1, option = "C"), size = 1) +
    
    # Symmetric shading for the observed t-statistic region
    geom_ribbon(data = subset(plot_data, abs(t_values) <= abs(t_stat)), 
                aes(ymin = 0, ymax = density), fill = viridis(1, option = "A"), alpha = 0.4) +
    
    
    # Vertical lines for critical and observed t-values
    geom_vline(xintercept = critical_t, color = viridis(1, option = "E"), linetype = "dashed", size = 0.5) +
    geom_vline(xintercept = -critical_t, color = viridis(1, option = "E"), linetype = "dashed", size = 0.5) +
    geom_vline(xintercept = t_stat, color = viridis(1, option = "C"), linetype = "solid", size = 1) +
    
    # Labels and annotations
    labs(
      title = "t-Distribution with Symmetric Observed Region Shading",
      x = "t-Value",
      y = "Density"
    ) +
    annotate(
      "text",
      x = critical_t + 0.5,
      y = max(density) * 0.15,
      label = paste0("Critical t = ±", round(critical_t, 2)),
      color = viridis(1, option = "E")
    ) +
    annotate(
      "text",
      x = 3, # Adjust text position based on sign of t_stat
      y = 0.35,
      label = paste0("Observed t = ", round(t_stat, 2)),
      size = 5,
      color = viridis(1, option = "C")
    ) +
    
    # Theme adjustments
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16),
      axis.title = element_text(size = 12)
    )
})

  
  
}

# Run the app
shinyApp(ui = ui, server = server)
