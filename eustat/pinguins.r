library(shiny)
library(palmerpenguins)


ui <- fluidPage(
  titlePanel("Penguins Scatter Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select Species:",
        choices = unique(penguins$species)
      ),
      selectInput("xvar", "Select X-axis Variable:",
        choices = names(penguins)[3:6]
      ),
      selectInput("yvar", "Select Y-axis Variable:",
        choices = names(penguins)[3:6],
        selected = names(penguins)[4]
      )
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output) {
  filteredData <- reactive({
    penguins[penguins$species == input$species, ]
  })

  output$scatterPlot <- renderPlot({
    data <- filteredData()
    x <- data[[input$xvar]]
    y <- data[[input$yvar]]

    plot(x, y,
      xlab = input$xvar,
      ylab = input$yvar,
      main = paste("Scatter Plot of", input$species),
      pch = 19,
      col = "blue"
    )
  })
}

shinyApp(ui = ui, server = server)
