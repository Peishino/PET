library(shiny)
ui <- fluidPage(
  titlePanel("Histograma de uma distribuição normal"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Número de observações:", min = 1, max = 1000, value = 500)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)
server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
}
shinyApp(ui = ui, server = server)
