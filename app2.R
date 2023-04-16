library(shiny)

ui <- navbarPage(
  "My Shiny App",
  tabPanel("Data", dataTableOutput("data")),
  tabPanel("Plot", plotOutput("plot")),
  tabPanel("Summary", tableOutput("summary")))


server <- function(input, output) {
  output$plot <- renderPlot({
    plot(mtcars)
  })
  
  output$summary <- renderTable({
    summary(mtcars)
  })
  
  output$data <- renderDataTable({
    mtcars
  })
}

shinyApp(ui, server)