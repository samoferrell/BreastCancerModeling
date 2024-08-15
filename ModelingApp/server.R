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

# Define server logic required to draw a histogram
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
                           tuneGrid = expand.grid(k = seq(from = 1, to = 40, by = 1)))
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
                            tuneGrid = expand.grid(cp = seq(from = 0, to = 0.1, by = 0.001)))    
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
  
  output$summary <- DT::renderDataTable({

      output$columns <- renderUI({
        checkboxGroupInput("variables", "Select Predictor Variables", 
                           choices = colnames(data))})
      data
    })
  output$formula <- renderPrint({
    selected_variables <- input$variables
    fit <- modeling_function(method = "logistic", variables = selected_variables)
    print(fit)
  })

}
