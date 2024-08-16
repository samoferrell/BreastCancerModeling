#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Breast Cancer Modeling App"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          selectInput("method", label = "Modeling Method", 
                      choices = c("logistic", "kNN", "tree"),
                      selected = "logistic"),
          uiOutput("columns")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          dataTableOutput("summary"),
          verbatimTextOutput("formula"),
          uiOutput("pred1"),
          uiOutput("pred2"),
          uiOutput("pred3"),
          uiOutput("pred4")
          
        )
    )
)
