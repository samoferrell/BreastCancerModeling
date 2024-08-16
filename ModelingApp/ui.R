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
          uiOutput("columns")
        ),
        
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
