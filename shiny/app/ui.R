library(shiny)
library(leaflet)
library(DBI)
library(RPostgreSQL)
library(dplyr)
library(leafem)
library(stringr)
library(DT)

shinyUI(
  navbarPage(
    title = "Layout v2",
    tabPanel(
      "req1",
      value = "r1",
      sidebarLayout(
        sidebarPanel(
          textAreaInput(
            inputId = "r1_req",
            label = "Request",
            value = request_1,
            placeholder = "write your request here",
            rows = 15,
            resize = "both"
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
        mainPanel(
          tabsetPanel(
            tabPanel(
              title = "map", 
              value = "map1",
              leafletOutput("r1_map", height = "500px")
            ),
            tabPanel(
              title = "table",
              value = "table1",
              dataTableOutput("r1_table")
            ),
            tabPanel(
              title = "request",
              value = "request1",
              verbatimTextOutput("r1_request")
            ),
            id = "req1"
          )
        )
      )
    ),
    tabPanel(
      "req2",
      value = "r2",
      sidebarLayout(
        sidebarPanel(
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
        mainPanel(
          tabsetPanel(
            tabPanel(
              title = "map",
              value = "map2",
              leafletOutput("r2_map", height = "500px")
            ),
            tabPanel(
              title = "table",
              value = "table2",
              dataTableOutput("r2_table")
            ),
            tabPanel(
              title = "request",
              value = "request2",
              verbatimTextOutput("r2_request")
            ),
            id = "req2"
          )
        )
      )
    ),
    tabPanel(
      "req3",
      value = "r3",
      sidebarLayout(
        sidebarPanel(
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
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              title = "map",
              value = "map3",
              leafletOutput("r3_map", height = "500px")
            ),
            tabPanel(
              title = "table",
              value = "table3",
              dataTableOutput("r3_table")
            ),
            tabPanel(
              title = "request",
              value = "request3",
              verbatimTextOutput("r3_request")
            ),
            id = "req3"
          )
        )
      )
    ),
    tabPanel(
      "req4",
      value = "r4",
      sidebarLayout(
        sidebarPanel(
          uiOutput(
            outputId = "r4_param"
          ),
          selectInput(
            inputId = "r4_param_level",
            label = "Choose a level",
            choices = ""
          ),
          actionButton(
            inputId = "r4_submit",
            label = "Submit",
            icon = icon("share-square"),
            style = "color: #fff; 
            background-color: #337ab7; 
            border-color: #2e6da4"
          ),
          actionButton(
            inputId = "r4_clean",
            label = "Clean",
            icon = icon("broom"),
            style = "color: #fff; 
            background-color: #9ea8b0; 
            border-color: #7c8287"
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              title = "map",
              value = "map4",
              leafletOutput("r4_map", height = "500px")
            ),
            tabPanel(
              title = "table",
              value = "table4",
              dataTableOutput("r4_table")
            ),
            tabPanel(
              title = "request",
              value = "request4",
              verbatimTextOutput("r4_request")
            ),
            id = "req4"
          )
        )
      )
    ),
    id = "reqselected"
  )
)