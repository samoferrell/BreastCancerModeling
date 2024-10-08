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
  modeling_function <- function(method, variables, 
                                num_folds, num_repeats, 
                                k_low = 1, k_high = 10, k_inc = 1,
                                cp_low = 0, cp_high = 0.1, cp_inc = 0.01){
    variable_list <- paste(variables, collapse = " + ")
    func_model <- as.formula(paste("diagnosis ~", variable_list))
    if (method == "kNN"){
      kNNFit_func <- train(func_model, 
                           data = train,
                           method = "knn",
                           preProcess = c("center", "scale"),
                           trControl = trainControl(method = "repeatedcv", 
                                                    number = num_folds,
                                                    repeats = num_repeats),
                           tuneGrid = expand.grid(k = seq(from = k_low, to = k_high, by = k_inc)))
      return(kNNFit_func)
    }
    if (method == "tree"){
      treeFit_func <- train(func_model, 
                            data = train,
                            method = "rpart",
                            preProcess = c("center", "scale"),
                            trControl = trainControl(method = "repeatedcv", 
                                                     number = num_folds,
                                                     repeats = num_repeats),
                            tuneGrid = expand.grid(cp = seq(from = cp_low, to = cp_high, by = cp_inc)))    
      return(treeFit_func)
    }
    if (method == "logistic"){
      logFit_func <- train(func_model, 
                           data = train,
                           preProcess = c("center", "scale"),
                           method = "glm",
                           family = "binomial",
                           trControl = trainControl(method = "repeatedcv", 
                                                    number = num_folds,
                                                    repeats = num_repeats))
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
  
  # reactive function parameters
  selected_variables <- reactive(input$variables)
  selected_method <- reactive(input$method)
  selected_folds <- reactive(input$folds)
  selected_repeats <- reactive(input$repeats)
  selected_k_low <- reactive(input$k_low)
  selected_k_high <- reactive(input$k_high)
  selected_k_inc <- reactive(input$k_inc)
  selected_cp_low <- reactive(input$cp_low)
  selected_cp_high <- reactive(input$cp_high)
  selected_cp_inc <- reactive(input$cp_inc)
  
  
  
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
  output$pred5 <- renderUI({
    if (length(selected_variables()) > 4) {
      vars <- selected_variables()
      numericInput("pred5", vars[5], value = round(mean(data[[vars[5]]]), 3))
    }
  })
  output$pred6 <- renderUI({
    if (length(selected_variables()) > 5) {
      vars <- selected_variables()
      numericInput("pred6", vars[6], value = round(mean(data[[vars[6]]]), 3))
    }
  })
  output$pred7 <- renderUI({
    if (length(selected_variables()) > 6) {
      vars <- selected_variables()
      numericInput("pred7", vars[7], value = round(mean(data[[vars[7]]]), 3))
    }
  })
  output$pred8 <- renderUI({
    if (length(selected_variables()) > 7) {
      vars <- selected_variables()
      numericInput("pred8", vars[8], value = round(mean(data[[vars[8]]]), 3))
    }
  })
  output$pred9 <- renderUI({
    if (length(selected_variables()) > 8) {
      vars <- selected_variables()
      numericInput("pred9", vars[9], value = round(mean(data[[vars[9]]]), 3))
    }
  })
  output$pred10 <- renderUI({
    if (length(selected_variables()) > 9) {
      vars <- selected_variables()
      numericInput("pred10", vars[10], value = round(mean(data[[vars[10]]]), 3))
    }
  })
  # output$pred11 <- renderUI({
  #   if (length(selected_variables()) > 10) {
  #     vars <- selected_variables()
  #     numericInput("pred11", vars[11], value = round(mean(data[[vars[11]]]), 3))
  #   }
  # })
  # output$pred12 <- renderUI({
  #   if (length(selected_variables()) > 11) {
  #     vars <- selected_variables()
  #     numericInput("pred12", vars[12], value = round(mean(data[[vars[12]]]), 3))
  #   }
  # })
  # output$pred13 <- renderUI({
  #   if (length(selected_variables()) > 12) {
  #     vars <- selected_variables()
  #     numericInput("pred13", vars[13], value = round(mean(data[[vars[13]]]), 3))
  #   }
  # })
  # output$pred14 <- renderUI({
  #   if (length(selected_variables()) > 13) {
  #     vars <- selected_variables()
  #     numericInput("pred14", vars[14], value = round(mean(data[[vars[14]]]), 3))
  #   }
  # })
  # output$pred15 <- renderUI({
  #   if (length(selected_variables()) > 14) {
  #     vars <- selected_variables()
  #     numericInput("pred15", vars[15], value = round(mean(data[[vars[15]]]), 3))
  #   }
  # })
  
  output$summary <- DT::renderDataTable({
      data
    })

  # Creating the reactive fit
  fit <- reactive(modeling_function(method = selected_method(), 
                                    variables = selected_variables(), 
                                    num_folds = selected_folds(),
                                    num_repeats = selected_repeats(),
                                    k_low = selected_k_low(), 
                                    k_high = selected_k_high(), 
                                    k_inc = selected_k_inc(),
                                    cp_low = selected_cp_low(), 
                                    cp_high = selected_cp_high(), 
                                    cp_inc = selected_cp_inc()))
  
  # printing fit if 1-4 variables are chosen
  output$formula <- renderPrint({
    if (length(selected_variables()) > 0){
    print(fit())
    print(confusionMatrix(fit(), newdata = test)) }
    else {
      print("Please select 1 - 10 variables as predictors")} })
  
  # if 4 variables are chosen, the interactive numerical inputs allow the user to predict if the tumor is bengin,
  # based on the inputs and the fit that was created above
  output$prediction <- renderPrint({
    vars <- selected_variables()
    #subset values to length of vars
    values <- c(input$pred1, input$pred2, input$pred3, input$pred4, 
                input$pred5, input$pred6, input$pred7, input$pred8, 
                input$pred9, input$pred10, input$pred11, input$pred12, 
                input$pred13, input$pred14, input$pred15)[1:length(vars)]
    
    accuracy <- reactive((round(max(fit()$results$Accuracy),2) * 100))
      names <- vars[1:length(vars)]
      pred_obs <- data.frame(setNames(as.list(values), names))
      pred_obs
      prediction <- predict(fit(), newdata = pred_obs)
      prediction
    if (as.character(prediction) == "B"){
        cat("Using the", accuracy(), "% accurate fit, we predict this tumor to be Benign")}
      else {
        cat("Using the", accuracy(), "% accurate fit, we predict this tumor to be Malignant")}
      

  })
  }


