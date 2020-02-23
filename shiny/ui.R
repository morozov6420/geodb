library(shiny)
library(leaflet)
library(DBI)
library(RPostgreSQL)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(shinyTime)

shinyUI(
  navbarPage(
    title = "GeoDB",
    tabPanel(
      "Select",
      value = "r1",
      sidebarLayout(#position = "right",
        sidebarPanel(#width = 5,
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
            width = "49%",
            style = "color: #fff;
          background-color: #337ab7;
          border-color: #2e6da4"
          ),
          actionButton(
            inputId = "r1_clean",
            label = "Clean",
            icon = icon("broom"),
            width = "49%",
            style = "color: #fff;
            background-color: #9ea8b0;
            border-color: #7c8287"
          )
        ),
        mainPanel(#width = 7,
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
    navbarMenu(
      "Insert",
      tabPanel(
        "Ponit",
        value = "r6_1",
        sidebarLayout(
          sidebarPanel(
            textInput(
              inputId = "r6_1_age",
              label = "age_group",
              # value = ""
              value = "MMM"
            ),
            textInput(
              inputId = "r6_1_sex",
              label = "vic_sex",
              # value = ""
              value = "MMM"
            ),
            textInput(
              inputId = "r6_1_type",
              label = "cr_type",
              # value = ""
              value = "MMM"
            ),
            textInput(
              inputId = "r6_1_lat",
              label = "Latitude",
              value = "40.7"
            ),
            textInput(
              inputId = "r6_1_lng",
              label = "Longitude",
              value = "-73.9"
            ),
            dateInput(
              inputId = "r6_1_date",
              label = "date",
              value = "2020-02-18", 
              width = "49%",
              autoclose = TRUE
            ),
            timeInput(
              inputId = "r6_1_time",
              label = "time",
              value = strptime("12:34:56", "%T")
            ),
            actionButton(
              inputId = "r6_1_submit",
              label = "Submit",
              icon = icon("share-square"),
              width = "49%",
              style = "color: #fff;
              background-color: #337ab7;
              border-color: #2e6da4"
            ),
            actionButton(
              inputId = "r6_1_clean",
              label = "Clean",
              icon = icon("broom"),
              width = "49%",
              style = "color: #fff;
              background-color: #9ea8b0;
              border-color: #7c8287"
            )
          ),
          mainPanel(
            tabsetPanel(
              tabPanel(
                title = "map",
                value = "map6_1",
                leafletOutput("r6_1_map", height = "500px")
              ),
              tabPanel(
                title = "table",
                value = "table6_1",
                dataTableOutput("r6_1_table")
              ),
              tabPanel(
                title = "request",
                value = "request6_1",
                verbatimTextOutput("r6_1_request")
              ),
              id = "req6_1"
            )
          )
        )
      ),
      tabPanel(
        "Polygon",
        value = "r6_2",
        sidebarLayout(
          sidebarPanel(
            textInput(
              inputId = "r6_2_region",
              label = "region",
              # value = "MMM", 
              placeholder = "new value"
            ),
            checkboxInput(
              inputId = "r6_2_check",
              label = "Convex Hull", 
              value = F
            ),
            actionButton(
              inputId = "r6_2_submit",
              label = "Submit",
              icon = icon("share-square"),
              width = "49%",
              style = "color: #fff;
              background-color: #337ab7;
              border-color: #2e6da4"
            ),
            actionButton(
              inputId = "r6_2_clean",
              label = "Clean",
              icon = icon("broom"),
              width = "49%",
              style = "color: #fff;
              background-color: #9ea8b0;
              border-color: #7c8287"
            )
          ),
          mainPanel(
            tabsetPanel(
              tabPanel(
                title = "map",
                value = "map6_2",
                leafletOutput("r6_2_map", height = "500px")
              ),
              tabPanel(
                title = "table",
                value = "table6_2",
                dataTableOutput("r6_2_table")
              ),
              tabPanel(
                title = "request",
                value = "request6_2",
                verbatimTextOutput("r6_2_request")
              ),
              id = "req6_2"
            )
          )
        )
      )
    ),
    tabPanel(
      "k-NN",
      value = "r2",
      sidebarLayout(
        sidebarPanel(
          textInput(
            inputId = "r2_lat",
            label = "Latitude",
            value = "40.7",
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
            width = "49%",
            style = "color: #fff;
            background-color: #337ab7;
            border-color: #2e6da4"
          ),
          actionButton(
            inputId = "r2_clean",
            label = "Clean",
            icon = icon("broom"),
            width = "49%",
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
      "Search in the vicinity",
      value = "r5",
      sidebarLayout(
        sidebarPanel(
          textInput(
            inputId = "r5_lat",
            label = "Latitude",
            # width = "49%",
            value = "40.7",
          ),
          textInput(
            inputId = "r5_lng",
            label = "Longitude",
            # width = "49%",
            value = "-73.9"
          ),
          numericInput(
            inputId = "r5_dist",
            label = "Distance, m",
            value = 5000,
            min = 1
          ),
          actionButton(
            inputId = "r5_submit",
            label = "Submit",
            icon = icon("share-square"),
            width = "49%",
            style = "color: #fff;
            background-color: #337ab7;
            border-color: #2e6da4"
          ),
          actionButton(
            inputId = "r5_clean",
            label = "Clean",
            icon = icon("broom"),
            width = "49%",
            style = "color: #fff;
            background-color: #9ea8b0;
            border-color: #7c8287"
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              title = "map",
              value = "map5",
              leafletOutput("r5_map", height = "500px")
            ),
            tabPanel(
              title = "table",
              value = "table5",
              dataTableOutput("r5_table")
            ),
            tabPanel(
              title = "request",
              value = "request5",
              verbatimTextOutput("r5_request")
            ),
            id = "req5"
          )
        )
      )
    ),
    tabPanel(
      "Search in the polygon",
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
            width = "49%",
            style = "color: #fff; 
            background-color: #337ab7; 
            border-color: #2e6da4"
          ),
          actionButton(
            inputId = "r3_clean",
            label = "Clean",
            icon = icon("broom"),
            width = "49%",
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
      "Categorical features",
      value = "r4",
      sidebarLayout(
        sidebarPanel(
          uiOutput(
            outputId = "r4_param"
          ),
          selectInput(
            inputId = "r4_param_level",
            label = "Choose a level",
            # width = "49%",
            choices = ""
          ),
          actionButton(
            inputId = "r4_submit",
            label = "Submit",
            icon = icon("share-square"),
            width = "49%",
            style = "color: #fff; 
            background-color: #337ab7; 
            border-color: #2e6da4"
          ),
          actionButton(
            inputId = "r4_clean",
            label = "Clean",
            icon = icon("broom"),
            width = "49%",
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
    tabPanel(
      "Linkages",
      value = "r7",
      sidebarLayout(
        sidebarPanel(
          uiOutput(
            outputId = "r7_param"
          ),
          selectInput(
            inputId = "r7_param_level_1",
            label = "Choose a level 1",
            # width = "49%",
            choices = ""
          ),
          selectInput(
            inputId = "r7_param_level_2",
            label = "Choose a level 2",
            # width = "49%",
            choices = ""
          ),
          radioButtons(
            inputId = "r7_link",
            label = NULL,
            choices = c(
              "UPGMA",
              "Centroid", 
              "Complete", 
              "Single"
            )
          ),
          actionButton(
            inputId = "r7_submit",
            label = "Submit",
            icon = icon("share-square"),
            width = "49%",
            style = "color: #fff; 
            background-color: #337ab7; 
            border-color: #2e6da4"
          ),
          actionButton(
            inputId = "r7_clean",
            label = "Clean",
            icon = icon("broom"),
            width = "49%",
            style = "color: #fff; 
            background-color: #9ea8b0; 
            border-color: #7c8287"
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              title = "map",
              value = "map7",
              leafletOutput("r7_map", height = "500px")
            ),
            tabPanel(
              title = "table",
              value = "table7",
              dataTableOutput("r7_table")
            ),
            tabPanel(
              title = "request",
              value = "request7",
              verbatimTextOutput("r7_request")
            ),
            id = "req7"
          )
        )
      )
    ),
    id = "reqselected"
  )
)