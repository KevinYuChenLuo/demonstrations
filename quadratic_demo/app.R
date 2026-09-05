library(shiny)
library(ggplot2)
library(bslib)
library(plotly) # Added for interactive zooming and panning

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
    style = "max-width: 1200px; margin: 0 auto; padding: 24px;",
    
    h2("Quadratic Function Translator", 
       style = "font-weight: 700; color: #1E293B; margin-bottom: 24px; text-align: center;"),
    
    # Top section: Equations on the left, Graph on the right
    layout_columns(
      col_widths = c(4, 8),
      
      # Left Column: Equation Formulas
      card(
        style = "background-color: #FFFFFF; border: 1px solid #E2E8F0; border-radius: 12px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
        card_header("Live Mathematical Translations", style = "font-weight: 700; background-color: #F1F5F9; color: #334155;"),
        card_body(
          withMathJax(), # Enables LaTeX rendering
          uiOutput("dynamic_equations")
        )
      ),
      
      # Right Column: The Plotly Graph
      card(
        style = "background-color: #FFFFFF; border: 1px solid #E2E8F0; border-radius: 12px; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05); padding: 5px;",
        plotlyOutput("quadPlot", height = "450px") # Upgraded to plotlyOutput
      )
    ),
    
    br(),
    
    # Bottom section: The Driver Controls
    card(
      style = "background-color: #FFFFFF; border: 1px solid #E2E8F0; border-radius: 12px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
      card_header(
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span("Variable Controls", style = "font-weight: 700; color: #334155;"),
          radioButtons("driver", "Select Active Driver:", 
                       choices = c("Standard Form", "Factored Form", "Vertex Form"), 
                       selected = "Standard Form", inline = TRUE)
        ),
        style = "background-color: #F1F5F9;"
      ),
      card_body(
        
        # Panel 1: Standard Form Inputs
        conditionalPanel(
          condition = "input.driver == 'Standard Form'",
          div(style = "border-left: 4px solid #2563EB; padding-left: 20px; background-color: #EFF6FF; padding-top: 10px; padding-bottom: 1px; border-radius: 0 8px 8px 0;",
              h6("STANDARD FORM (y = ax² + bx + c)", style = "color: #1D4ED8; font-weight: bold; margin-bottom: 15px;"),
              layout_columns(
                col_widths = c(4, 4, 4),
                numericInput("a_std", "a", value = 1, step = 0.1),
                numericInput("b_std", "b", value = 0, step = 0.1),
                numericInput("c_std", "c", value = -4, step = 0.1)
              )
          )
        ),
        
        # Panel 2: Factored Form Inputs
        conditionalPanel(
          condition = "input.driver == 'Factored Form'",
          div(style = "border-left: 4px solid #059669; padding-left: 20px; background-color: #ECFDF5; padding-top: 10px; padding-bottom: 1px; border-radius: 0 8px 8px 0;",
              h6("FACTORED FORM (y = a(x - r)(x - s))", style = "color: #047857; font-weight: bold; margin-bottom: 15px;"),
              layout_columns(
                col_widths = c(4, 4, 4),
                numericInput("a_fac", "a", value = 1, step = 0.1),
                numericInput("r_fac", "r (root 1)", value = -2, step = 0.1),
                numericInput("s_fac", "s (root 2)", value = 2, step = 0.1)
              )
          )
        ),
        
        # Panel 3: Vertex Form Inputs
        conditionalPanel(
          condition = "input.driver == 'Vertex Form'",
          div(style = "border-left: 4px solid #D97706; padding-left: 20px; background-color: #FFFBEB; padding-top: 10px; padding-bottom: 1px; border-radius: 0 8px 8px 0;",
              h6("VERTEX FORM (y = a(x - h)² + k)", style = "color: #B45309; font-weight: bold; margin-bottom: 15px;"),
              layout_columns(
                col_widths = c(4, 4, 4),
                numericInput("a_ver", "a", value = 1, step = 0.1),
                numericInput("h_ver", "h (x-vertex)", value = 0, step = 0.1),
                numericInput("k_ver", "k (y-vertex)", value = -4, step = 0.1)
              )
          )
        )
      )
    )
  )
)

# 2. Server Logic
server <- function(input, output, session) {
  
  # Reactive function to calculate standard coefficients (a, b, c)
  params <- reactive({
    if (input$driver == "Standard Form") {
      a <- input$a_std; b <- input$b_std; c <- input$c_std
    } else if (input$driver == "Factored Form") {
      a <- input$a_fac; r <- input$r_fac; s <- input$s_fac
      b <- -a * (r + s)
      c <- a * r * s
    } else {
      a <- input$a_ver; h <- input$h_ver; k <- input$k_ver
      b <- -2 * a * h
      c <- a * h^2 + k
    }
    
    # Handle NA inputs if a student clears a field temporarily
    if (is.na(a) || a == 0) a <- 0.001 
    if (is.na(b)) b <- 0
    if (is.na(c)) c <- 0
    
    list(a = a, b = b, c = c)
  })
  
  # Dynamically render the equations
  output$dynamic_equations <- renderUI({
    p <- params()
    a <- p$a; b <- p$b; c <- p$c
    
    std_eq <- sprintf("$$y = %.2fx^2 %s %.2fx %s %.2f$$", 
                      a, ifelse(b>=0, "+", "-"), abs(b), ifelse(c>=0, "+", "-"), abs(c))
    
    h_v <- -b / (2*a)
    k_v <- c - (b^2) / (4*a)
    ver_eq <- sprintf("$$y = %.2f(x %s %.2f)^2 %s %.2f$$",
                      a, ifelse(h_v<=0, "+", "-"), abs(h_v), ifelse(k_v>=0, "+", "-"), abs(k_v))
    
    disc <- b^2 - 4*a*c
    if (disc < 0) {
      fac_eq <- "<p style='text-align: center; color: #ef4444; font-weight: bold; margin-top: 15px;'>No Real Roots</p>"
    } else {
      r1 <- (-b + sqrt(disc)) / (2*a)
      r2 <- (-b - sqrt(disc)) / (2*a)
      fac_eq <- sprintf("$$y = %.2f(x %s %.2f)(x %s %.2f)$$",
                        a, ifelse(r1<=0, "+", "-"), abs(r1), ifelse(r2<=0, "+", "-"), abs(r2))
    }
    
    withMathJax(HTML(paste0(
      "<div style='color: #2563EB; margin-bottom: 25px;'><strong>Standard Form: </strong>", std_eq, "</div>",
      "<div style='color: #059669; margin-bottom: 25px;'><strong>Factored Form: </strong>", fac_eq, "</div>",
      "<div style='color: #D97706;'><strong>Vertex Form: </strong>", ver_eq, "</div>"
    )))
  })
  
  # Render the Plotly interactive graph
  output$quadPlot <- renderPlotly({
    p_params <- params()
    
    # Calculate vertex to center the graph initially
    v_x <- -p_params$b / (2 * p_params$a)
    v_y <- p_params$c - (p_params$b^2) / (4 * p_params$a)
    
    # Generate a wide range of data points to allow for zooming out
    x_vals <- seq(v_x - 200, v_x + 200, length.out = 1500)
    y_vals <- p_params$a * x_vals^2 + p_params$b * x_vals + p_params$c
    line_df <- data.frame(x = x_vals, y = y_vals)
    
    # Identify roots for visual dots
    disc <- p_params$b^2 - 4 * p_params$a * p_params$c
    roots_df <- data.frame(x = numeric(0), y = numeric(0))
    if (disc >= 0) {
      r1 <- (-p_params$b + sqrt(disc)) / (2 * p_params$a)
      r2 <- (-p_params$b - sqrt(disc)) / (2 * p_params$a)
      roots_df <- data.frame(x = c(r1, r2), y = c(0, 0))
    }
    vertex_df <- data.frame(x = v_x, y = v_y)
    
    # Define an initial dynamic view window based on the vertex
    x_lims <- c(v_x - 12, v_x + 12)
    if (p_params$a > 0) {
      y_lims <- c(v_y - 5, v_y + 35) # Opening up
    } else {
      y_lims <- c(v_y - 35, v_y + 5) # Opening down
    }
    
    # Build the ggplot
    p <- ggplot() +
      geom_hline(yintercept = 0, color = "#94A3B8", linewidth = 0.8) +
      geom_vline(xintercept = 0, color = "#94A3B8", linewidth = 0.8) +
      
      # The Parabola (Using geom_line instead of stat_function for Plotly compatibility)
      geom_line(data = line_df, aes(x = x, y = y), color = "#3B82F6", linewidth = 1.2) +
      
      # Draw Vertex (Orange Dot)
      geom_point(data = vertex_df, aes(x = x, y = y, text = sprintf("Vertex: (%.2f, %.2f)", x, y)), 
                 color = "#D97706", size = 3) +
      
      # Draw Roots (Green Dots)
      geom_point(data = roots_df, aes(x = x, y = y, text = sprintf("Root: (%.2f, 0)", x)), 
                 color = "#059669", size = 3) +
      
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.grid.major = element_line(color = "#F1F5F9", linewidth = 1),
        panel.grid.minor = element_line(color = "#F8FAFC", linewidth = 0.5),
        axis.title = element_blank()
      ) +
      coord_cartesian(xlim = x_lims, ylim = y_lims) # Sets the initial camera frame
    
    # Convert ggplot to interactive Plotly
    ggplotly(p, tooltip = "text") %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
  })
}

# 3. Run application
shinyApp(ui = ui, server = server)