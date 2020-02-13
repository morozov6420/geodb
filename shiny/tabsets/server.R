library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  output$irisPlot <- renderPlot({
    ggplot(
      iris,
      aes(get(input$iris_x),
          get(input$iris_y))
      ) +
        geom_point() + 
        xlab(input$iris_x) + 
        ylab(input$iris_y)
  })
  output$mtcarsPlot <- renderPlot({
    ggplot(
      mtcars,
      aes(get(input$mtcars_x),
          get(input$mtcars_y))
      ) +
        geom_point() +
        xlab(input$mtcars_x) +
        ylab(input$mtcars_y)
  })
  output$treesPlot <- renderPlot({
    ggplot(
      trees,
      aes(get(input$trees_x),
          get(input$trees_y))
      ) +
        geom_point() + 
        xlab(input$trees_x) +
        ylab(input$trees_y)
  })
})