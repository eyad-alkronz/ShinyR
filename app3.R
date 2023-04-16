library(shiny)
library(readxl)
library(tidyverse)

plotTab <- tabPanel(
  "Plot",
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "plot_type",
        label = "Select plot type:",
        choices = c("Histogram", "Bar plot", "Scatter plot"),
        selected = "Histogram"
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Histogram' || input.plot_type == 'Bar plot'",
        selectInput(
          inputId = "variable",
          label = "Select a variable:",
          choices = NULL
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Scatter plot'",
        selectInput(
          inputId = "x_var_select",
          label = "Select the X variable:",
          choices = NULL
        ),
        selectInput(
          inputId = "y_var_select",
          label = "Select the Y variable:",
          choices = NULL
        )
      ),
      downloadButton("downloadPlot", "Download plot")
    ),
    mainPanel(
      conditionalPanel(condition = "input.plot_type != 'Scatter plot'", plotOutput("plot") ),
      conditionalPanel(condition = "input.plot_type == 'Scatter plot'", plotOutput("scatter_output") ),
      
       
    )
  )
)

ui <- navbarPage(
  "My Shiny App",
  tabPanel("Data", 
           sidebarLayout(
             sidebarPanel(
               fileInput("file", "Choose a file", accept = c(".csv", ".xlsx")),
               verbatimTextOutput("rowcount"),
               verbatimTextOutput("variablesCounts"),
               tableOutput("var_summary") 
             ),
             mainPanel(
               dataTableOutput("data")
             )
           )
  ),
  tabPanel("Plot",plotTab ),
  
  
  tabPanel("Summary", 
             column(6, 
                    h3("Summary Table"),
                    tableOutput("summary")),
             column(6, 
                    h3("Summary Stats"),
                    verbatimTextOutput("summary_stats"))
           )
  )
 


server <- function(input, output) {

  
  data <- reactive({
    req(input$file)
    infile <- input$file
    
    # Define the dat object before the if statement
    dat <- NULL
    
    if (grepl(".csv$", infile$name)) {
      dat <- tryCatch(
        read.csv(infile$datapath , sep = ","),
        error = function(e) {
          print(e)
          return(NULL)
        }
      )
    } else if (grepl(".xls$|.xlsx$", infile$name)) {
      dat <- tryCatch(
        read_excel(infile$datapath),
        error = function(e) {
          print(e)
          return(NULL)
        }
      )
    }
    
    # If dat is NULL, return an empty data frame instead
    if (is.null(dat)) {
      dat <- data.frame()
    }
    
    # Update the rowcount output element with the number of rows
    output$rowcount <- renderText(paste0("Total rows: ", nrow(dat)))
    # Update the rowcount output element with the number of rows
    output$variablesCounts <- renderText(paste0("Total Variables: ", ncol(dat)))
    
    # Return the dat object
    return(dat)
  })
  
  output$data <- renderDataTable({
    data()
  })
  # Create a reactive function to extract the variable names and types
  var_summary <- reactive({
    req(data())
    cols <- colnames(data())
    types <- sapply(data(), class)
    summary_df <- data.frame(variable = cols, type = types)
    return(summary_df)
  })
  
  
  # Create a reactive function to extract the variable names and types
  var_summary <- reactive({
    req(data())
    cols <- colnames(data())
    types <- sapply(data(), class)
    summary_df <- data.frame(variable = cols, type = types)
    return(summary_df)
  })
  
  # Output the summary table
  output$var_summary <- renderTable({
    var_summary()
  })
  
  
  # Create a reactive function to update the variable selection based on the plot type
  variable_choices <- reactive({
    if (input$plot_type == "Histogram" || input$plot_type == "Bar plot") {
      choices <- colnames(data())
    } else if (input$plot_type == "Scatter plot") {
      choices <- c("", colnames(data()))
    }
    return(choices)
  })
  
  # # Update the variable selection
  observe({
    updateSelectInput(getDefaultReactiveDomain() , "variable", choices = variable_choices())
    updateSelectInput(getDefaultReactiveDomain(), "x_var_select", choices = variable_choices())
    updateSelectInput(getDefaultReactiveDomain(), "y_var_select", choices = variable_choices())
  })
  
  # Output the plot data
  output$plot <- renderPlot({
    plot()
  })
  
  output$scatter_output <- renderPlot({
    scatterPlot()
  })
  
  # Create a reactive function to generate the plot
  plot <- reactive({
    req(plot_data())
    if (get_var_type(selected_var()) == "numeric") {
      create_hist(plot_data(), selected_var())
    } else if (get_var_type(selected_var()) == "factor") {
      create_bar_plot(plot_data(), selected_var())
    } else {
      return(NULL)
    }
  })
  
  scatterPlot <- reactive({
    req(scatter_data())
    create_scatter_plot(scatter_data(), selected_x_var() , selected_y_var())
  
  })
  
  
  # Create a reactive function to filter the data for plotting
  plot_data <- reactive({
    req(data())
    if (!is.null(selected_var())) {
      df <- data() %>%
        filter(!is.na(!!sym(selected_var())))
      return(df)
    } else {
      return(NULL)
    }
  })
  # 
  # Create a reactive function to filter the data for scatter plot
  scatter_data <- reactive({
    req(data())
    if (!is.null(selected_x_var()) & !is.null(selected_y_var())) {
      df <- data() %>%
        filter(!is.na(!!sym(selected_x_var())) & !is.na(!!sym(selected_y_var())))
      return(df)
    } else {
      return(NULL)
    }
  })
  
  # Define a reactive function to get the selected variable for plotting
  selected_var <- reactive({
    req(input$variable)
    return(input$variable)
  })
  # 
  # Define a reactive function to get the selected x variable for plotting
  selected_x_var <- reactive({
    req(input$x_var_select)
    return(input$x_var_select)
  })
  # 
  # Define a reactive function to get the selected y variable for plotting
  selected_y_var <- reactive({
    req(input$y_var_select)
    return(input$y_var_select)
  })
  
  
  # Define a function to create the histogram
  create_hist <- function(df, var_name) {
    p <- ggplot(df, aes_string(x = var_name)) +
      geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
      labs(title = paste0("Histogram of ", var_name))
    return(p)
  }
  
  # Define a function to create the bar plot
  create_bar_plot <- function(df, var_name) {
    p <- ggplot(df, aes_string(x = var_name)) +
      geom_bar(color = "black", fill = "lightblue") +
      labs(title = paste0("Bar plot of ", var_name))
    return(p)
  }
  
  # Define a function to create the scatter plot
  create_scatter_plot <- function(df, x_var, y_var) {
    p <- ggplot(df, aes_string(x = x_var, y = y_var)) +
      geom_point(color = "blue") +
      labs(title = paste0("Scatter plot of ", x_var, " vs. ", y_var))
    return(p)
  }
  
  
  
  
  

  # Create a reactive function to generate the plot data
  plotData <- reactive({
    if (input$plot_type == "Histogram") {
      xvar <- input$variable
      if (!is.null(xvar)) {
        if (is.numeric(data()[[xvar]])) {
          return(list(xvar = xvar, data = data()))
        }
      }
    } else if (input$plot_type == "Bar plot") {
      xvar <- input$variable
      if (!is.null(xvar)) {
        if (is.factor(data()[[xvar]]) || is.character(data()[[xvar]])) {
          counts <- table(data()[[xvar]])
          return(list(xvar = xvar, counts = counts))
        }
      }
    } else if (input$plot_type == "Scatter plot") {
      xvar <- input$xvar
      yvar <- input$yvar
      if (!is.null(xvar) && !is.null(yvar)) {
        if (is.numeric(data()[[xvar]]) && is.numeric(data()[[yvar]])) {
          return(list(xvar = xvar, yvar = yvar, data = data()))
        }
      }
    }
    return(NULL)
  })
  # 
  # Define a function to get the variable type
  get_var_type <- function(var_name) {
    var_type <- var_summary() %>%
      filter(variable == var_name) %>%
      pull(type)
    return(var_type)
  }
  


  
  # output$summary <- renderTable({
  #   data() %>%
  #     select_if(is.numeric) %>%
  #     summary() %>%
  #     t() %>%
  #     as.data.frame() %>%
  #     set_names("Stats") %>%
  #     rownames_to_column("Variable") %>%
  #     bind_cols(data.frame(Variable = names(data()), 
  #                          Type = sapply(data(), class))) %>%
  #     arrange(desc(Stats[,"Mean"])) %>%
  #     select(Variable, Type, everything())
  # })
  # 
  # output$summary_stats <- renderPrint({
  #   stats <- sapply(data(), function(x) c(mean = mean(x), sd = sd(x), median = median(x)))
  #   colnames(stats) <- paste0(colnames(data()), c(" (mean)", " (sd)", " (median)"))
  #   stats
  # })
  # 

  
 
}

shinyApp(ui, server)
