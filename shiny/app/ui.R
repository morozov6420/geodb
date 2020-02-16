library(shiny)
library(leaflet)

shinyUI(
  pageWithSidebar(
    headerPanel(
      "app with leaflet"
    ),
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabselected == 1",
        textAreaInput(
          inputId = "r1_req",
          label = "Request", 
          value = request_1,
          # placeholder = "write your request here", 
          rows = 15
        ),
        selectInput(
          inputId = "r1_choice",
          label = "Choose an option",
          choices = c(
            "map" = 2,
            "table" = 1,
            "request" = 3
          ), 
          selected = 2
        ),
        actionButton(
          inputId = "r1_submit",
          label = "Submit",
          icon = icon("share-square"),
          style = "color: #fff; 
          background-color: #337ab7; 
          border-color: #2e6da4"
        ),
        actionButton(
          inputId = "r1_clean",
          label = "Clean",
          icon = icon("broom"),
          style = "color: #fff; 
          background-color: #9ea8b0; 
          border-color: #7c8287"
        )
      ),
      conditionalPanel(
        condition = "input.tabselected == 2",
        textInput(
          inputId = "r2_lat",
          label = "Latitude",
          value = "40.7"
        ),
        textInput(
          inputId = "r2_lng",
          label = "Longitude",
          value = "-73.9"
        ),
        numericInput(
          inputId = "r2_nn",
          label = "k-nearest neighbors", 
          value = 10, 
          min = 1
        ),
        selectInput(
          inputId = "r2_choice",
          label = "Choose an option",
          choices = c(
            "map" = 2,
            "table" = 1,
            "request" = 3
          ), 
          selected = 2
        ),
        actionButton(
          inputId = "r2_submit",
          label = "Submit",
          icon = icon("share-square"),
          style = "color: #fff; 
          background-color: #337ab7; 
          border-color: #2e6da4"
        ),
        actionButton(
          inputId = "r2_clean",
          label = "Clean",
          icon = icon("broom"),
          style = "color: #fff; 
          background-color: #9ea8b0; 
          border-color: #7c8287"
        )
      ),
      conditionalPanel(
        condition = "input.tabselected == 3",
        selectInput(
          inputId = "r3_choice",
          label = "Choose an option",
          choices = c(
            "map" = 2,
            "table" = 1,
            "request" = 3
          ), 
          selected = 2
        ),
        checkboxInput(
          inputId = "r3_check",
          label = "Convex Hull", 
          value = F
        ),
        actionButton(
          inputId = "r3_submit",
          label = "Submit",
          icon = icon("share-square"),
          style = "color: #fff; 
          background-color: #337ab7; 
          border-color: #2e6da4"
        ),
        actionButton(
          inputId = "r3_clean",
          label = "Clean",
          icon = icon("broom"),
          style = "color: #fff; 
          background-color: #9ea8b0; 
          border-color: #7c8287"
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "req1", 
          value = 1,
          conditionalPanel(
            condition = "input.r1_choice == 1",
            tableOutput("r1_table")
          ),
          conditionalPanel(
            condition = "input.r1_choice == 2",
            leafletOutput("r1_map", height = "500px")
          ),
          conditionalPanel(
            condition = "input.r1_choice == 3",
            verbatimTextOutput("r1_request")
          )
        ),
        tabPanel(
          title = "req2",
          value = 2,
          conditionalPanel(
            condition = "input.r2_choice == 1",
            tableOutput("r2_table")
          ),
          conditionalPanel(
            condition = "input.r2_choice == 2",
            leafletOutput("r2_map", height = "500px")
          ),
          conditionalPanel(
            condition = "input.r2_choice == 3",
            verbatimTextOutput("r2_request")
          )
        ),
        tabPanel(
          title = "req3",
          value = 3,
          conditionalPanel(
            condition = "input.r3_choice == 2",
            leafletOutput("r3_map", height = "500px")
          ),
          conditionalPanel(
            condition = "input.r3_choice == 1",
            tableOutput("r3_table")
          ),
          conditionalPanel(
            condition = "input.r3_choice == 3",
            verbatimTextOutput("r3_request")
          )
        ),
        id = "tabselected"
      )
    )
  )
)
