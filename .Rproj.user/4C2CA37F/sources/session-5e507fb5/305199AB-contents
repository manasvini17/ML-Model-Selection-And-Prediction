library(shiny)
library(shinythemes)
library(rpart)
library(class)
library(e1071)
library(nnet)
library(dplyr)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("ML Model Selection and Prediction App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      uiOutput("feature_select"),
      uiOutput("target_select"),
      selectInput("model", "Select Model:",
                  choices = c("Linear Regression", "Logistic Regression", "Naive Bayes", "k-NN", "Decision Tree", "SVM")),
      actionButton("predict", "Predict"),
      uiOutput("inputs_ui")
    ),
    
    mainPanel(
      verbatimTextOutput("prediction_result")
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  
  output$feature_select <- renderUI({
    req(dataset())
    data <- dataset()
    checkboxGroupInput("features", "Select Features:", choices = names(data), selected = names(data)[1:4])
  })

  output$target_select <- renderUI({
    req(dataset())
    data <- dataset()
    selectInput("target", "Select Target Variable:", choices = names(data), selected = names(data)[5])
  })

  output$inputs_ui <- renderUI({
    req(input$features)
    inputs <- lapply(input$features, function(feature) {
      numericInput(feature, feature, value = 0)
    })
    do.call(tagList, inputs)
  })

  model <- reactive({
    req(dataset(), input$features, input$target)
    data <- dataset() %>% select(all_of(c(input$features, input$target)))
    
    if (input$model == "Linear Regression") {
      lm(as.formula(paste(input$target, "~ .")), data = data)
    } else if (input$model == "Logistic Regression") {
      multinom(as.formula(paste(input$target, "~ .")), data = data)
    } else if (input$model == "Naive Bayes") {
      naiveBayes(as.formula(paste(input$target, "~ .")), data = data)
    } else if (input$model == "k-NN") {
      list(data = data[, input$features], labels = data[[input$target]])
    } else if (input$model == "Decision Tree") {
      rpart(as.formula(paste(input$target, "~ .")), data = data)
    } else if (input$model == "SVM") {
      svm(as.formula(paste(input$target, "~ .")), data = data)
    }
  })

  output$prediction_result <- renderPrint({
    req(input$predict, input$features)
    new_data <- as.data.frame(lapply(input$features, function(feature) input[[feature]]))
    colnames(new_data) <- input$features
    
    if (input$model == "Linear Regression") {
      prediction <- predict(model(), newdata = new_data)
    } else if (input$model == "Logistic Regression") {
      prediction <- predict(model(), newdata = new_data, type = "class")
    } else if (input$model == "Naive Bayes") {
      prediction <- predict(model(), newdata = new_data)
    } else if (input$model == "k-NN") {
      knn_model <- model()
      prediction <- knn(train = knn_model$data, test = new_data, cl = knn_model$labels, k = 3)
    } else if (input$model == "Decision Tree") {
      prediction <- predict(model(), newdata = new_data, type = "class")
    } else if (input$model == "SVM") {
      prediction <- predict(model(), newdata = new_data)
    }
    
    paste("Predicted Outcome:", prediction)
  })
}

shinyApp(ui = ui, server = server)
