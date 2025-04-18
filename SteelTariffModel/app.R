library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Steel Industry Cost Curve Under Tariff Protection"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("tariff", "Tariff Rate (%)", min = 0, max = 100, value = 25),
      numericInput("fixedCost", "Fixed Cost ($)", value = 1000000),
      numericInput("variableCost", "Initial Variable Cost per Ton ($)", value = 500),
      numericInput("learningRate", "Learning Rate (λ)", value = 0.1, step = 0.01),
      numericInput("output", "Output (Tons)", value = 10000),
      numericInput("worldPrice", "World Price ($)", value = 500),
      numericInput("years", "Number of Years", value = 10, min = 1, max = 50),
      numericInput("demandIntercept", "Demand Intercept (a)", value = 20000),
      numericInput("demandSlope", "Demand Slope (b)", value = 20)
    ),
    
    mainPanel(
      plotOutput("costPlot"),
      plotOutput("demandPlot"),
      tableOutput("econSummary"),
      tableOutput("costTable")
    )
  )
)

server <- function(input, output) {
  
  costData <- reactive({
    t <- 0:input$years
    e_decay <- exp(-input$learningRate * t)
    VC <- input$variableCost * e_decay
    AC <- (input$fixedCost / input$output) + VC
    tariffPrice <- input$worldPrice * (1 + input$tariff / 100)
    
    data.frame(
      Year = t,
      AvgCost = AC,
      TariffAdjustedPrice = rep(tariffPrice, length(t)),
      WorldPrice = rep(input$worldPrice, length(t))
    )
  })
  
  demandData <- reactive({
    prices <- seq(0, input$worldPrice * 1.5, length.out = 100)
    quantities <- input$demandIntercept - input$demandSlope * prices
    data.frame(Price = prices, Quantity = quantities)
  })
  
  econData <- reactive({
    # Demand parameters
    a <- input$demandIntercept
    b <- input$demandSlope
    
    # Prices
    Pw <- input$worldPrice
    Pt <- Pw * (1 + input$tariff / 100)
    
    # Average Cost from cost curve
    t <- input$years
    AC <- (input$fixedCost / input$output) + input$variableCost * exp(-input$learningRate * t)
    
    # Quantities at each price
    Qd_w <- a - b * Pw
    Qd_t <- a - b * Pt
    Qs <- input$output  # Assume perfectly elastic supply at AC
    
    # Surplus calculations
    # Area of triangle: (1/2) * base * height
    CS_w <- 0.5 * Qd_w * (a / b - Pw)      # Without tariff
    CS_t <- 0.5 * Qd_t * (a / b - Pt)      # With tariff
    PS_t <- (Pt - AC) * Qs                 # Producer surplus only exists with tariff
    DWL <- max(0, CS_w - CS_t - PS_t)      # Deadweight loss
    
    list(
      AC = AC,
      Pw = Pw,
      Pt = Pt,
      Qd_w = Qd_w,
      Qd_t = Qd_t,
      Qs = Qs,
      CS_w = CS_w,
      CS_t = CS_t,
      PS_t = PS_t,
      DWL = DWL
    )
  })
  
  
  
  output$costPlot <- renderPlot({
    df <- costData()
    ggplot(df, aes(x = Year)) +
      geom_line(aes(y = AvgCost, color = "Average Cost")) +
      geom_line(aes(y = TariffAdjustedPrice, color = "Tariff-Adjusted Price")) +
      geom_line(aes(y = WorldPrice, color = "World Price")) +
      labs(y = "Price per Ton ($)", color = "Legend") +
      theme_minimal()
  })
  
  output$demandPlot <- renderPlot({
    e <- econData()
    df <- demandData()
    
    # Define demand choke price (price at zero quantity)
    choke_price <- input$demandIntercept / input$demandSlope
    
    # Ensure we have valid equilibrium quantities
    Qd_t <- max(0, e$Qd_t)
    Qs <- max(0, e$Qs)
    
    # 1. Consumer Surplus Polygon
    # Consumer Loss Polygon: area between demand curve and price lines from Q_t to Q_w
    Qd_t <- max(0, e$Qd_t)
    Qd_w <- max(0, e$Qd_w)
    
    # Build a polygon only if Qd_w > Qd_t
    cs_loss_poly <- if (Qd_w > Qd_t) {
      data.frame(
        Quantity = c(Qd_t, Qd_w, Qd_w, Qd_t),
        Price = c(e$Pt, e$Pt, e$Pw, e$Pw)
      )
    } else {
      data.frame(Quantity = numeric(0), Price = numeric(0))
    }
    
    
    # 2. Producer Surplus Polygon
    ps_poly <- data.frame(
      Quantity = c(0, Qs, Qs, 0),
      Price = c(e$AC, e$AC, e$Pt, e$Pt)
    )
    
    # 3. Deadweight Loss Polygon (triangle between Qs and Qd_t at tariff price)
    # Only create this if Qd_t > Qs
    dwl_poly <- if (Qd_t > Qs) {
      data.frame(
        Quantity = c(Qs, Qd_t, Qs),
        Price = c(e$Pt, e$Pt, (input$demandIntercept - Qs) / input$demandSlope)
      )
    } else {
      data.frame(Quantity = numeric(0), Price = numeric(0))
    }
    
    ggplot(df, aes(x = Quantity, y = Price)) +
      # Demand curve
      geom_line(color = "blue", size = 1.2) +
      
      # Add polygons if non-empty
      {if (nrow(cs_loss_poly) > 0) geom_polygon(data = cs_loss_poly, aes(x = Quantity, y = Price), fill = "skyblue", alpha = 0.4) else NULL} +
      {if (nrow(ps_poly) > 0) geom_polygon(data = ps_poly, aes(x = Quantity, y = Price), fill = "palegreen4", alpha = 0.5) else NULL} +
      {if (nrow(dwl_poly) > 0) geom_polygon(data = dwl_poly, aes(x = Quantity, y = Price), fill = "orangered", alpha = 0.4) else NULL} +
      
      # Supply line (AC)
      geom_hline(yintercept = e$AC, linetype = "solid", color = "darkgreen", size = 1) +
      
      # Price lines
      geom_hline(yintercept = e$Pw, linetype = "dashed", color = "black") +
      geom_hline(yintercept = e$Pt, linetype = "dotted", color = "red") +
      
      # Equilibrium points
      geom_point(aes(x = e$Qd_w, y = e$Pw), color = "black", size = 3) +
      geom_point(aes(x = Qd_t, y = e$Pt), color = "red", size = 3) +
      
      # Annotations
      annotate("text", x = Qd_t + 300, y = e$Pt + 10, label = "With Tariff", color = "red") +
      annotate("text", x = e$Qd_w + 300, y = e$Pw + 10, label = "No Tariff", color = "black") +
      annotate("text", x = Qs / 2, y = e$Pt + 40, label = "Consumer Loss", color = "blue") +
      annotate("text", x = Qs / 2, y = e$AC - 20, label = "Producer Surplus", color = "darkgreen") +
      annotate("text", x = Qs + 200, y = e$Pt + 20, label = "Deadweight Loss", color = "orangered") +
      
      # Final plot setup
      labs(
        title = "Surplus Analysis with Tariff",
        x = "Quantity",
        y = "Price ($)"
      ) +
      coord_cartesian(xlim = c(0, max(df$Quantity, Qd_t + 1000)), ylim = c(0, max(df$Price, e$Pt + 50))) +
      theme_minimal()
  })
  
  
  
  output$econSummary <- renderTable({
    e <- econData()
    data.frame(
      `Avg Cost` = round(e$AC, 2),
      `World Price` = e$Pw,
      `Tariff Price` = e$Pt,
      `Consumer Loss` = round(e$CS_w - e$CS_t, 0),
      `Producer Surplus` = round(e$PS_t, 0),
      `Deadweight Loss` = round(e$DWL, 0)
    )
  })
  
  
  output$costTable <- renderTable({
    df <- costData()
    df
  })
}

shinyApp(ui = ui, server = server)

