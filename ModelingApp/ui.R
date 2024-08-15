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
          uiOutput("columns")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          dataTableOutput("summary"),
          verbatimTextOutput("formula")
          
        )
    )
)
