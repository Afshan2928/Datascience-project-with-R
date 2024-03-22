library(shiny)
library(shinydashboard)
library(survival)
library(DT)  # For interactive DataTable
library(caret)
library(ggplot2)

# Load the cancer survival dataset
data(lung)

# Define the UI
ui <- dashboardPage(
  
  dashboardHeader(
    title = "Cancer Survival Data Analysis",
    titleWidth = 250
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Analysis", tabName = "data_analysis", icon = icon("database")),
      menuItem("Age Analysis", tabName = "age_analysis", icon = icon("chart-bar")),
      menuItem("Gender Analysis", tabName = "gender_analysis", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data_analysis",
        fluidRow(
          box(
            title = "Lung Cancer Dataset",
            DTOutput("lung_table")
          )
        )
      ),
      tabItem(
        tabName = "age_analysis",
        fluidRow(
          box(
            title = "Age Distribution",
            plotOutput("age_distribution_plot")
          ),
          box(
            title = "Model Accuracy Comparison (Age)",
            plotOutput("accuracy_plot_age")
          ),
          box(
            title = "Survival Probability vs. Age",
            plotOutput("survival_age_plot")
          )
        )
      ),
      tabItem(
        tabName = "gender_analysis",
        fluidRow(
          box(
            title = "Gender Distribution",
            plotOutput("gender_distribution_plot")
          ),
          box(
            title = "Model Accuracy Comparison (Gender)",
            plotOutput("accuracy_plot_gender")
          ),
          box(
            title = "Survival Probability by Gender",
            plotOutput("survival_gender_plot")
          )
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Render the DataTable
  output$lung_table <- renderDT({
    datatable(lung, options = list(pageLength = 10))  # Displaying only 10 rows per page
  })
  
  # Render the age distribution plot
  output$age_distribution_plot <- renderPlot({
    ggplot(lung, aes(x = age)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
      labs(title = "Age Distribution",
           x = "Age", y = "Frequency") +
      theme_minimal()
  })
  
  # Train and evaluate models for age
  results_age <- reactive({
    train_data <- na.omit(lung)
    train_data$sex <- as.factor(train_data$sex)
    train_data$status <- factor(train_data$status)  # Convert to factor with two levels
    levels(train_data$status) <- c("0", "1")  # Rename levels if necessary
    
    # Define classification algorithms
    algorithms <- c("Decision Tree", "Random Forest", "SVM", "k-NN", "GBM")
    
    # Train and evaluate models
    results <- list()
    for (algo in algorithms) {
      model <- NULL
      if (algo == "Decision Tree") {
        model <- train(status ~ age, data = train_data, method = "rpart")
      } else if (algo == "Random Forest") {
        model <- train(status ~ age, data = train_data, method = "rf")
      } else if (algo == "SVM") {
        model <- train(status ~ age, data = train_data, method = "svmRadial")
      } else if (algo == "k-NN") {
        model <- train(status ~ age, data = train_data, method = "knn")
      } else if (algo == "GBM") {
        model <- train(status ~ age, data = train_data, method = "gbm")
      }
      predictions <- predict(model, newdata = train_data)
      accuracy <- confusionMatrix(predictions, train_data$status)$overall["Accuracy"]
      results[[algo]] <- accuracy
    }
    
    # Convert results to data frame
    results_df <- data.frame(Algorithm = names(results), Accuracy = unlist(results))
    
    # Return results
    results_df
  })
  
  # Render the accuracy comparison plot for age
  output$accuracy_plot_age <- renderPlot({
    ggplot(results_age(), aes(x = Algorithm, y = Accuracy)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Accuracy of Different Classification Algorithms (Age)",
           x = "Algorithm", y = "Accuracy") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the survival probability vs. age plot
  output$survival_age_plot <- renderPlot({
    lung$status <- factor(lung$status, levels = c("0", "1"))
    lung$time <- with(lung, ifelse(status == "1", time, max(time)))
    lung$status <- with(lung, ifelse(status == "1", 1, 0))
    
    fit <- survfit(Surv(time, status) ~ age, data = lung)
    plot(fit, col = c("blue", "red"), xlab = "Age", ylab = "Survival Probability", main = "Survival Probability vs. Age")
    legend("topright", legend = c("0", "1"), col = c("blue", "red"), lty = 1, title = "Status")
  })
  
  # Render the gender distribution plot
  output$gender_distribution_plot <- renderPlot({
    ggplot(lung, aes(x = factor(sex, labels = c("Female", "Male")), fill = factor(sex, labels = c("Female", "Male")))) +
      geom_bar() +
      labs(title = "Gender Distribution",
           x = "Gender", y = "Count") +
      scale_fill_manual(values = c("Female" = "red", "Male" = "blue")) + # Set colors
      theme_minimal()
  })
  
  # Train and evaluate models for gender
  results_gender <- reactive({
    train_data <- na.omit(lung)
    train_data$sex <- as.factor(train_data$sex)
    train_data$status <- factor(train_data$status)  # Convert to factor with two levels
    levels(train_data$status) <- c("0", "1")  # Rename levels if necessary
    
    # Define classification algorithms
    algorithms <- c("Decision Tree", "Random Forest", "SVM", "k-NN", "GBM")
    
    # Train and evaluate models
    results <- list()
    for (algo in algorithms) {
      model <- NULL
      if (algo == "Decision Tree") {
        model <- train(status ~ sex, data = train_data, method = "rpart")
      } else if (algo == "Random Forest") {
        model <- train(status ~ sex, data = train_data, method = "rf")
      } else if (algo == "SVM") {
        model <- train(status ~ sex, data = train_data, method = "svmRadial")
      } else if (algo == "k-NN") {
        model <- train(status ~ sex, data = train_data, method = "knn")
      } else if (algo == "GBM") {
        model <- train(status ~ sex, data = train_data, method = "gbm")
      }
      predictions <- predict(model, newdata = train_data)
      accuracy <- confusionMatrix(predictions, train_data$status)$overall["Accuracy"]
      results[[algo]] <- accuracy
    }
    
    # Convert results to data frame
    results_df <- data.frame(Algorithm = names(results), Accuracy = unlist(results))
    
    # Return results
    results_df
  })
  
  # Render the accuracy comparison plot for gender
  output$accuracy_plot_gender <- renderPlot({
    ggplot(results_gender(), aes(x = Algorithm, y = Accuracy)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Accuracy of Different Classification Algorithms (Gender)",
           x = "Algorithm", y = "Accuracy") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the survival probability by gender plot
  output$survival_gender_plot <- renderPlot({
    fit <- survfit(Surv(time, status) ~ sex, data = lung)
    plot(fit, col = c("blue", "red"), xlab = "Time", ylab = "Survival Probability", main = "Survival Probability by Gender")
    legend("topright", legend = c("Female", "Male"), col = c("red", "blue"), lty = 1, title = "Gender")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
