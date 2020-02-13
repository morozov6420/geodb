library(shiny)

shinyUI(
  fluidPage(
    titlePanel(
      h3("first exanple")
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "select",
          label = "Choose the table name",
          choices = c(
            "ny_parts", 
            "ny_crimes"
          ),
          selected = "ny_parts"
        ),
        numericInput(
          inputId = "num",
          label = "Number of observations",
          value = 5
        ),
        actionButton(
          inputId = "action",
          label = "SELECT",
          icon = icon("angellist")
        )
      ),
      mainPanel(
        h3("Executed request"),
        textOutput("req"),
        tableOutput("tab")
      )
    )
  )
)
