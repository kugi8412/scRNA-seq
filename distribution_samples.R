library(shiny)
library(DT)

# User interface for choosing distribution
ui_samples <- fluidPage(
  titlePanel("Sample Generator from Selected Distribution"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("distribution", "Choose distribution:",
                  choices = c("Normal" = "norm",
                              "Exponential" = "exp",
                              "Binomial" = "binom",
                              "Custom samples" = "custom")),
      
      conditionalPanel(
        condition = "input.distribution == 'norm'",
        numericInput("mean", "Mean:", value = 0),
        numericInput("sd", "Standard Deviation:", value = 1)
      ),
      
      conditionalPanel(
        condition = "input.distribution == 'exp'",
        numericInput("rate", "Lambda:", value = 1)
      ),
      
      conditionalPanel(
        condition = "input.distribution == 'binom'",
        numericInput("size", "Number of trials:", value = 10),
        numericInput("prob", "Probability of success:", value = 0.5)
      ),
      
      conditionalPanel(
        condition = "input.distribution == 'custom'",
        fileInput("file1", "Upload CSV file", accept = ".csv"),
        textInput("custom_values", "Or enter values separated by semicolons")
      ),
      
      conditionalPanel(
        condition = "input.distribution != 'custom'",
        numericInput("n", "Number of samples to generate:", value = 100)
      ),
      
      actionButton("generate", "Generate samples"),
      actionButton("remove", "Remove selected samples"),
      actionButton("clear", "Clear all samples"),
      downloadButton("downloadData", "Download results.csv")
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      dataTableOutput("dataTable")
    )
  )
)

# Server for selecting distributions
server_samples <- function(input, output, session) {
  sample_data <- reactiveVal(data.frame(Value = numeric()))
  
  observeEvent(input$generate, {
    n <- input$n
    dist <- input$distribution
    new_data <- NULL
    
    if (dist == "norm") {
      new_data <- rnorm(n, mean = input$mean, sd = input$sd)
    } else if (dist == "exp") {
      new_data <- rexp(n, rate = input$rate)
    } else if (dist == "binom") {
      new_data <- rbinom(n, size = input$size, prob = input$prob)
    } else if (dist == "custom") {
      if (input$custom_values != "") {
        new_data <- as.numeric(unlist(strsplit(input$custom_values, ";")))
      } else if (!is.null(input$file1)) {
        new_data <- as.numeric(unlist(strsplit(readLines(input$file1$datapath),
                                               ";")))
      }
    }
    
    if (!is.null(new_data)) {
      current_data <- sample_data()
      updated_data <- rbind(current_data, data.frame(Value = new_data))
      sample_data(updated_data)
    }
  })
  
  observeEvent(input$remove, {
    selected_rows <- input$dataTable_rows_selected
    if (length(selected_rows) > 0) {
      current_data <- sample_data()
      updated_data <- current_data[-selected_rows, , drop = FALSE]
      sample_data(updated_data)
    }
  })
  
  observeEvent(input$clear, {
    sample_data(data.frame(Value = numeric()))
  })
  
  output$dataTable <- renderDataTable({
    current_data <- sample_data()
    mean_value <- mean(current_data$Value, na.rm = TRUE)
    data <- cbind(current_data, Above_Mean =
                    ifelse(current_data$Value > mean_value, "Yes", "No"))
    datatable(data, options = list(pageLength = 10,
                                   order = list(list(0, 'asc'))))
  }, server = FALSE)
  
  output$distPlot <- renderPlot({
    current_data <- sample_data()
    if (nrow(current_data) > 0) {
      hist(current_data$Value, probability = TRUE,
           main = "Histogram with Density",
           xlab = "Values", col = "lightblue")
      lines(density(current_data$Value), col = "red", lwd = 2)
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "results.csv"
    },
    content = function(file) {
      write.csv(sample_data(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui_samples, server = server_samples)
