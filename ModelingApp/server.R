#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(tidyverse)
library(caret)

function(input, output, session) {
#function for modeling
  modeling_function <- function(method, variables){
    variable_list <- paste(variables, collapse = " + ")
    func_model <- as.formula(paste("diagnosis ~", variable_list))
    if (method == "kNN"){
      kNNFit_func <- train(func_model, 
                           data = train,
                           method = "knn",
                           preProcess = c("center", "scale"),
                           trControl = trainControl(method = "repeatedcv", 
                                                    number = 10,
                                                    repeats = 3),
                           tuneGrid = expand.grid(k = seq(from = 1, to = 10, by = 1)))
      return(kNNFit_func)
    }
    if (method == "tree"){
      treeFit_func <- train(func_model, 
                            data = train,
                            method = "rpart",
                            preProcess = c("center", "scale"),
                            trControl = trainControl(method = "repeatedcv", 
                                                     number = 10,
                                                     repeats = 3),
                            tuneGrid = expand.grid(cp = seq(from = 0, to = 0.1, by = 0.01)))    
      return(treeFit_func)
    }
    if (method == "logistic"){
      logFit_func <- train(func_model, 
                           data = train,
                           preProcess = c("center", "scale"),
                           method = "glm",
                           family = "binomial",
                           trControl = trainControl(method = "repeatedcv", 
                                                    number = 10,
                                                    repeats = 3))
      return(logFit_func)
    }
  }

# reading in data 
  data <- read_csv("../breast-cancer.csv") |>
    rename("concave_points_mean" = "concave points_mean",
           "concave_points_se" = "concave points_se",
           "concave_points_worst" = "concave points_worst")
  set.seed(426)
  # Creating an 80/20 split
  split <- createDataPartition(y = data$diagnosis, p = 0.8, list = FALSE)
  train <- data[split, ]
  test <- data[-split, ]
  
  # Creating interactive columns
  output$columns <- renderUI({
    checkboxGroupInput("variables", "Select Predictor Variables", 
                       choices = colnames(data))})
  selected_variables <- reactive(input$variables)
  selected_method <- reactive(input$method)
  
  # creating inputs for 4 predictors
  output$pred1 <- renderUI({
    if (length(selected_variables()) > 0){
    vars <- selected_variables()
    numericInput("pred1", vars[1], value = round(mean(data[[vars[1]]]),3))
    }
    })
  output$pred2 <- renderUI({
    if (length(selected_variables()) > 1){
    vars <- selected_variables()
    numericInput("pred2", vars[2], value = round(mean(data[[vars[2]]]),3))
    }
  })
  output$pred3 <- renderUI({
    if (length(selected_variables()) > 2){
    vars <- selected_variables()
    numericInput("pred3", vars[3], value = round(mean(data[[vars[3]]]),3))
    }
  })
  output$pred4 <- renderUI({
    if (length(selected_variables()) > 3){
    vars <- selected_variables()
    numericInput("pred4", vars[4], value = round(mean(data[[vars[4]]]),3))
    }
  })
  
  output$summary <- DT::renderDataTable({
      data
    })

  # Creating the reactive fit
  fit <- reactive(modeling_function(method = selected_method(), variables = selected_variables()))
  
  # printing fit if 1-4 variables are chosen
  output$formula <- renderPrint({
    if (length(selected_variables()) > 0 & length(selected_variables()) < 5){
    print(fit())
    print(confusionMatrix(fit(), newdata = test)) }
    else {
      print("Please select 1 - 4 variables as predictors")} })
  
  # if 4 variables are chosen, the interactive numerical inputs allow the user to predict if the tumor is bengin,
  # based on the inputs and the fit that was created above
  output$prediction <- renderPrint({
    vars <- selected_variables()
    values <- c(input$pred1,input$pred2,input$pred3,input$pred4)
    accuracy <- reactive((round(max(fit()$results$Accuracy),2) * 100))
    if (length(vars) == 4){
      names <- c(vars[1],vars[2],vars[3],vars[4])
      pred_obs <- data.frame(setNames(as.list(values), names))
      pred_obs
      prediction <- predict(fit(), newdata = pred_obs)
      prediction
    if (as.character(prediction) == "B"){
        cat("Using the", accuracy(), "% accurate fit, we predict this tumor to be Benign")}
      else {
        cat("Using the", accuracy(), "% accurate fit, we predict this tumor to be Malignant")}
      
}
  })
  }


