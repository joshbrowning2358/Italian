library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    
    dashboardHeader(title = "Learn Italian!"),
    
    dashboardSidebar(disable = TRUE),
    
    dashboardBody(
        sidebarPanel(width = 12, 
            # title = "Inputs",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "General",
            h3("Please input your name:"),
            h3(textInput(inputId = "userName", label = "", "Josh")),
            h3(textOutput("question")),
            h3(textInput(inputId = "answer", label = "", "")),
            h3(textOutput("response")),
            actionButton("submitButton", "Submit")
#             tabPanel(title = "Plots",
#                      checkboxInput(inputId = "fillBox",
#                                    label = "Fill Box Plot?",
#                                    value = TRUE)
#             )
        )
    )
)
