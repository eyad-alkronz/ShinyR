library(shiny)

ui <- fluidPage(
  titlePanel("Basic Shiny App"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Enter your name:"),
      selectInput("gender", "Select your gender:",
                  c("Male", "Female")),
      numericInput("age", "Enter your age:", value = 18),
      dateInput("birthdate", "Enter your birthdate:")
    ),
    mainPanel(
      verbatimTextOutput("results")
    )
  )
)

server <- function(input, output) {
  output$results <- renderPrint({
    paste("Name:", input$name, "\n",
          "Gender:", input$gender, "\n",
          "Age:", input$age, "\n",
          "Birthdate:", input$birthdate)
  })
}

shinyApp(ui, server)