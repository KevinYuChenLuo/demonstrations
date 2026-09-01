library(shiny)
library(ggplot2)
library(bslib)

# 1. UI Definition
ui <- page_fillable(
  # Clean, modern light theme
  theme = bs_theme(
    version = 5,
    bg = "#F8F9FA",
    fg = "#212529",
    primary = "#0D6EFD",
    base_font = font_google("Inter")
  ),
  
  div(
    style = "max-width: 950px; margin: 0 auto; padding: 24px;",
    
    h3("Derivative: Secant to Tangent Limit", 
       style = "font-weight: 700; color: #1E293B; margin-bottom: 24px; text-align: center;"),
    
    # Plot Card
    card(
      full_screen = TRUE,
      style = "background-color: #FFFFFF; border: 1px solid #E2E8F0; border-radius: 12px; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05);",
      plotOutput("calcPlot", height = "480px")
    ),
    
    br(),
    
    # Readouts for Slopes and Distance h
    layout_columns(
      col_widths = c(4, 4, 4),
      
      # Distance h Readout
      div(
        style = "text-align: center; border-right: 1px solid #E2E8F0;",
        h6("DISTANCE (h = x_Q - x_P)", style = "color: #64748B; letter-spacing: 0.5px; font-size: 0.75rem; font-weight: 600;"),
        h3(textOutput("h_val"), style = "font-weight: 700; color: #0EA5E9;")
      ),
      
      # Secant Slope Readout
      div(
        style = "text-align: center; border-right: 1px solid #E2E8F0;",
        h6("SECANT SLOPE (Δy / h)", style = "color: #64748B; letter-spacing: 0.5px; font-size: 0.75rem; font-weight: 600;"),
        h3(textOutput("secant_slope"), style = "font-weight: 700; color: #2563EB;")
      ),
      
      # Instantaneous Slope at P
      div(
        style = "text-align: center;",
        h6("INSTANTANEOUS SLOPE AT P", style = "color: #64748B; letter-spacing: 0.5px; font-size: 0.75rem; font-weight: 600;"),
        h3(textOutput("tangent_slope"), style = "font-weight: 700; color: #F59E0B;")
      )
    ),
    
    br(), hr(style = "border-color: #E2E8F0;"), br(),
    
    # Interactive Controls: Point P and Point Q
    layout_columns(
      col_widths = c(6, 6),
      sliderInput("x_p", "Point P (x₁)", 
                  min = -10, max = 10, value = 1.0, step = 0.1, width = "100%"),
      sliderInput("x_q", "Point Q (x₂)", 
                  min = -10, max = 10, value = 3.0, step = 0.1, width = "100%")
    )
  )
)

# 2. Server Logic
server <- function(input, output, session) {
  
  f <- function(x) x^2
  f_prime <- function(x) 2 * x
  
  # Distance h
  output$h_val <- renderText({
    h <- input$x_q - input$x_p
    sprintf("%.2f", h)
  })
  
  # Calculate Secant Slope dynamically (handles division by zero when h = 0)
  output$secant_slope <- renderText({
    x_p <- input$x_p
    x_q <- input$x_q
    h <- x_q - x_p
    
    if (abs(h) < 1e-4) {
      "Undefined (h = 0)"
    } else {
      slope <- (f(x_q) - f(x_p)) / h
      sprintf("%.3f", slope)
    }
  })
  
  # Calculate Instantaneous Slope at P
  output$tangent_slope <- renderText({
    sprintf("%.3f", f_prime(input$x_p))
  })
  
  # Render the light-themed ggplot
  output$calcPlot <- renderPlot({
    xp <- input$x_p
    xq <- input$x_q
    yp <- f(xp)
    yq <- f(xq)
    h <- xq - xp
    
    # Tangent line at P (y = mx + b)
    tan_slope <- f_prime(xp)
    tan_intercept <- yp - tan_slope * xp
    
    # Base Plot
    p <- ggplot(data.frame(x = c(-10.5, 10.5)), aes(x)) +
      # Function Curve
      stat_function(fun = f, color = "#2563EB", linewidth = 1.2) +
      
      # Tangent Line at P (Dashed Amber)
      geom_abline(intercept = tan_intercept, slope = tan_slope, 
                  color = "#F59E0B", linetype = "dashed", linewidth = 1) +
      
      # Base Point P
      geom_point(aes(x = xp, y = yp), color = "#F59E0B", size = 4) +
      annotate("text", x = xp, y = yp + 6, 
               label = sprintf("P (%.1f, %.1f)", xp, yp), 
               color = "#B45309", fontface = "bold", size = 4.5) +
      
      # Clean Light Theme
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.grid.major = element_line(color = "#F1F5F9", linewidth = 0.8),
        panel.grid.minor = element_line(color = "#F8FAFC", linewidth = 0.5),
        axis.text = element_text(color = "#64748B", face = "bold"),
        axis.title = element_text(color = "#334155", face = "bold")
      ) +
      labs(x = "x", y = "f(x) = x²") +
      coord_cartesian(xlim = c(-10.5, 10.5), ylim = c(-5, 105))
    
    # Draw Point Q, Secant Line, and Distance Indicators when P and Q are distinct
    if (abs(h) >= 0.05) {
      sec_slope <- (yq - yp) / h
      sec_intercept <- yp - sec_slope * xp
      
      p <- p +
        # Secant Line (Blue Solid)
        geom_abline(intercept = sec_intercept, slope = sec_slope, 
                    color = "#38BDF8", linewidth = 0.9) +
        # Horizontal Δx (h) line
        geom_segment(aes(x = xp, y = yp, xend = xq, yend = yp), 
                     color = "#0284C7", linetype = "dotted", linewidth = 0.8) +
        # Vertical Δy line
        geom_segment(aes(x = xq, y = yp, xend = xq, yend = yq), 
                     color = "#0284C7", linetype = "dotted", linewidth = 0.8) +
        # Point Q
        geom_point(aes(x = xq, y = yq), color = "#0284C7", size = 3.5) +
        annotate("text", x = xq, y = yq + 6, 
                 label = sprintf("Q (%.1f, %.1f)", xq, yq), 
                 color = "#0369A1", fontface = "bold", size = 4.5) +
        # h Label
        annotate("text", x = (xp + xq) / 2, y = yp - 4, 
                 label = sprintf("h = %.2f", h), 
                 color = "#0284C7", fontface = "bold", size = 4)
    }
    
    p
  })
}

# 3. Run application
shinyApp(ui = ui, server = server)