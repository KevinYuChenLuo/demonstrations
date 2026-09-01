library(shiny)
library(ggplot2)
library(bslib)

# 1. UI Definition
ui <- page_fillable(
  # Use bslib to create a dark theme that matches the reference image
  theme = bs_theme(
    bg = "#121212", 
    fg = "#FFFFFF", 
    primary = "#4793ff",
    base_font = font_google("Inter")
  ),
  
  div(
    style = "max-width: 900px; margin: 0 auto; padding: 20px;",
    
    h3("Derivative: Secant to Tangent Limit", style = "margin-bottom: 20px;"),
    
    # The Plot
    card(
      full_screen = TRUE,
      style = "background-color: #1a1a1a; border: none;",
      plotOutput("calcPlot", height = "450px")
    ),
    
    br(),
    
    # Readouts for Slopes
    layout_columns(
      col_widths = c(6, 6),
      div(
        style = "text-align: center; border-right: 1px solid #333;", 
        h6("SECANT SLOPE", style = "color: #b3b3b3; letter-spacing: 1px; font-size: 0.8rem;"), 
        h3(textOutput("secant_slope"), style = "font-weight: bold;")
      ),
      div(
        style = "text-align: center;", 
        h6("INSTANTANEOUS SLOPE", style = "color: #b3b3b3; letter-spacing: 1px; font-size: 0.8rem;"), 
        h3(textOutput("tangent_slope"), style = "font-weight: bold; color: #a6c8ff;")
      )
    ),
    
    br(), hr(style = "border-color: #333;"), br(),
    
    # Interactive Controls
    layout_columns(
      col_widths = c(6, 6),
      sliderInput("dx", "Distance (Δx)", 
                  min = 0.001, max = 2.0, value = 2.0, step = 0.01, width = "100%"),
      sliderInput("x_base", "Point (x)", 
                  min = -1.0, max = 2.0, value = 1.0, step = 0.1, width = "100%")
    )
  )
)

# 2. Server Logic
server <- function(input, output, session) {
  
  # Define the core function and its derivative
  f <- function(x) x^2
  f_prime <- function(x) 2*x
  
  # Calculate Secant Slope dynamically
  output$secant_slope <- renderText({
    x <- input$x_base
    dx <- input$dx
    slope <- (f(x + dx) - f(x)) / dx
    sprintf("%.3f", slope)
  })
  
  # Calculate Tangent (Instantaneous) Slope dynamically
  output$tangent_slope <- renderText({
    sprintf("%.3f", f_prime(input$x_base))
  })
  
  # Render the ggplot
  output$calcPlot <- renderPlot({
    x0 <- input$x_base
    dx <- input$dx
    x1 <- x0 + dx
    
    y0 <- f(x0)
    y1 <- f(x1)
    
    # Calculate geometric lines (y = mx + b)
    sec_slope <- (y1 - y0) / dx
    sec_intercept <- y0 - sec_slope * x0
    
    tan_slope <- f_prime(x0)
    tan_intercept <- y0 - tan_slope * x0
    
    # Build the base plot
    p <- ggplot(data.frame(x = c(-2.5, 3.5)), aes(x)) +
      # The main curve (Parabola)
      stat_function(fun = f, color = "#a6c8ff", linewidth = 1.2) +
      
      # The base Point P
      geom_point(aes(x = x0, y = y0), color = "#4793ff", size = 4) +
      annotate("text", x = x0 - 0.4, y = y0 + 0.3, 
               label = sprintf("P (%.1f, %.1f)", x0, y0), 
               color = "white", face = "bold") +
      
      # The Tangent Line (Yellow Dashed)
      geom_abline(intercept = tan_intercept, slope = tan_slope, 
                  color = "#ffb347", linetype = "dashed", linewidth = 1) +
      
      # Theme and grid styling to match reference
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1a1a1a", color = NA),
        panel.background = element_rect(fill = "#1a1a1a", color = NA),
        panel.grid.major = element_line(color = "#333333", linewidth = 0.5),
        panel.grid.minor = element_line(color = "#222222", linewidth = 0.5),
        axis.text = element_text(color = "#888888", face = "bold"),
        axis.title = element_blank() # Hiding standard axis titles to match image
      ) +
      coord_cartesian(ylim = c(-0.5, 6), xlim = c(-2, 3.5))
    
    # Add Secant line and Delta indicators if dx is large enough
    if(dx > 0.05) {
      p <- p + 
        # Secondary Point
        geom_point(aes(x = x1, y = y1), color = "#4793ff", size = 3) +
        # Secant Line (Blue Solid/Dashed)
        geom_abline(intercept = sec_intercept, slope = sec_slope, 
                    color = "#a6c8ff", linewidth = 0.8) +
        # Horizontal Delta x line
        geom_segment(aes(x = x0, y = y0, xend = x1, yend = y0), 
                     color = "#4793ff", linetype = "dotted", linewidth = 0.8) +
        # Vertical Delta y line
        geom_segment(aes(x = x1, y = y0, xend = x1, yend = y1), 
                     color = "#4793ff", linetype = "dotted", linewidth = 0.8) +
        # Delta x Label
        annotate("text", x = x0 + dx/2, y = y0 - 0.4, 
                 label = sprintf("Δx = %.2f", dx), color = "#4793ff")
    }
    
    p
  })
}

# 3. Run the application 
shinyApp(ui = ui, server = server)