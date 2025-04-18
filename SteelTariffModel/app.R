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
      numericInput("years", "Number of Years", value = 10, min = 1, max = 50)
    ),
    
    mainPanel(
      plotOutput("costPlot"),
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
  
  output$costPlot <- renderPlot({
    df <- costData()
    ggplot(df, aes(x = Year)) +
      geom_line(aes(y = AvgCost, color = "Average Cost")) +
      geom_line(aes(y = TariffAdjustedPrice, color = "Tariff-Adjusted Price")) +
      geom_line(aes(y = WorldPrice, color = "World Price")) +
      labs(y = "Price per Ton ($)", color = "Legend") +
      theme_minimal()
  })
  
  output$costTable <- renderTable({
    df <- costData()
    df
  })
}

shinyApp(ui = ui, server = server)

