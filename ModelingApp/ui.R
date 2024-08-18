#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

fluidPage(

    # Application title
    titlePanel("Breast Cancer Modeling App"),

    sidebarLayout(
        sidebarPanel(
          selectInput("method", label = "Modeling Method", 
                      choices = c("logistic", "kNN", "tree"),
                      selected = "logistic"),
          uiOutput("columns"),
          sliderInput("folds", "Number of Folds", min = 0, max = 15, step = 1, value = 10),
          sliderInput("repeats", "Repeats", min = 0, max = 5, step = 1, value = 3),
          conditionalPanel(condition = "input.method == 'kNN'",
                           sliderInput("k_low", "Minimum K Value", min = 1, max = 9, step = 1, value = 1),
                           sliderInput("k_high", "Max K Value", min = 10, max = 40, step = 1, value = 10),
                           sliderInput("k_inc", "Increment", min = 1, max = 3, step = 1, value = 1)),
          conditionalPanel(condition = "input.method == 'tree'",
                           sliderInput("cp_low", "Minimum CP Value", min = 0, max = 0.04, step = 0.01, value = 0),
                           sliderInput("cp_high", "Max CP Value", min = 0.05, max = 0.1, step = 0.01, value = 0.1),
                           sliderInput("cp_inc", "Increment", min = 0.001, max = 0.01, step = 0.001, value = 0.01))
                           
          )
        ,
        
        mainPanel(
          dataTableOutput("summary"),
          verbatimTextOutput("formula"),
          h3("Please select 4 inputs to make a prediction"),
          p("Each numerical input automatically takes the value of the mean for that variable."),
          uiOutput("pred1"),
          uiOutput("pred2"),
          uiOutput("pred3"),
          uiOutput("pred4"),
          verbatimTextOutput("prediction")
        )
    )
)
