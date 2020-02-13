library(shiny)

shinyUI(
  pageWithSidebar(
    titlePanel(
      "my example with tabsets"
    ),
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabselected == 1",
        selectInput(
          inputId = "iris_x",
          label = "Select iris X",
          choices = names(iris)
        ),
        selectInput(
          inputId = "iris_y",
          label = "Select iris Y",
          choices = names(iris)
        )
      ),
      conditionalPanel(
        condition = "input.tabselected == 2",
        selectInput(
          inputId = "mtcars_x",
          label = "Select mtcars X",
          choices = names(mtcars)
        ),
        radioButtons(
          inputId = "mtcars_y",
          label = "Select mtcars Y",
          choices = names(mtcars)
        )
      ),
      conditionalPanel(
        condition = "input.tabselected == 3",
        radioButtons(
          inputId = "trees_x",
          label = "Select trees X",
          choices = names(trees),
          inline = T
        ),
        radioButtons(
          inputId = "trees_y",
          label = "Select trees Y",
          choices = names(trees),
          inline = T
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "iris",
          value = 1,
          plotOutput("irisPlot")
        ),
        tabPanel(
          title = "mtcars",
          value = 2,
          plotOutput("mtcarsPlot")
        ),
        tabPanel(
          title = "trees",
          value = 3,
          plotOutput("treesPlot")
        ),
        id = "tabselected"
      )
    )
  )
)
