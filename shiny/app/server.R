library(shiny)
library(leaflet)
library(DBI)
library(RPostgreSQL)
library(dplyr)

args <- list(
  drv = dbDriver("PostgreSQL"),
  dbname ="geo",
  host = "localhost",
  user = "postgres",
  password = "a"
)


# disconnect all connections
# for (i in dbListConnections(db_drv)){
#   dbDisconnect(i)
# }

shinyServer(
  function(input, output){
    
    output$r1_table <- renderTable(
      {
        if(input$r1_submit == 0)
          return()
        isolate(
          {
            con <- do.call(DBI::dbConnect, args)
            on.exit(dbDisconnect(con))
            dbGetQuery(con, input$r1_req)
          }
        )
      }
    )
    output$r1_map <- renderLeaflet(
      {
        if(input$r1_submit == 0)
          return()
        isolate(
          {
            con <- do.call(DBI::dbConnect, args)
            on.exit(dbDisconnect(con))
            dat <- dbGetQuery(con, input$r1_req)
            leaflet(data = dat) %>%
              addTiles() %>%
              clearShapes() %>%
              addCircles(
                lat = ~lat,
                lng = ~lng,
                weight = 1,
                color = "#cf0c0c",
                # fillColor = ~ region,
                fillOpacity = 0.7, 
                radius = ~ 100,
                popup = ~ paste(
                  "Region: ", get("region"), "<br>",
                  "Age: ", get("age"), "<br>",
                  "Type: ", get("type"), "<br>",
                  "Date: ", get("date")
                )
              )
          }
        )
      }
    )
  }
)
