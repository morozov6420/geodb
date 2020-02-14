library(shiny)

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
          placeholder = "write your request here", 
          rows = 15
        ),
        radioButtons(
          inputId = "r1_choice",
          label = "Choose an option",
          choices = c(
            "table" = 1,
            "map" = 2
          ),
          inline = T
        ),
        actionButton(
          inputId = "r1_submit",
          label = "Submit",
          icon = icon("share-square"),
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )
      ),
      conditionalPanel(
        condition = "input.tabselected == 2",
        # "req2"
        dateRangeInput(
          inputId = "daterange",
          label = "Data range",
          weekstart = 1,
          format = "mm/dd/yy",
          separator = " - "
        ),
        helpText(
          "Здесь мы можем напихать всё что угодно, нужно просто придумать запросы"
        ),
        actionButton(
          inputId = "r2_submit",
          label = "Submit",
          icon = icon("share-square"),
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
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
          )
        ),
        tabPanel(
          title = "req2",
          value = 2
          
        ),
        id = "tabselected"
      )
    )
  )
)
