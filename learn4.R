

#Create an app that greets the user by name.
library(shiny)

ui <- fluidPage(
  sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", label = "If y is", min = 1, max = 50, value = 30),
  "then x times y is",
  textOutput("result")
)

server <- function(input, output, session) {
  output$result <- renderText({ 
    input$x * input$y
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
