# setwd("D:/Учёба/МАГА/Германия/GeoDB/geodb")
library(shiny)
library(DBI)
library(RPostgreSQL)

db_drv <- dbDriver("PostgreSQL")
db_name <- "geo"
db_host <- "localhost"
db_port <- "5432"
db_user <- "postgres"
db_password <- "a"
con <- dbConnect(db_drv,
                 dbname = db_name,
                 host = db_host,
                 port = db_port,
                 user = db_user,
                 password = db_password)
# dbListTables(con)


shinyServer(
  function(input, output){
    output$req <- renderText(
      {
        input$action
        isolate(paste0("SELECT * FROM ", input$select))
      }
    )
    output$tab <- renderTable(
      {
        input$action
        isolate(
          {
            df_postgres <- dbGetQuery(
              con, 
              paste0("SELECT * FROM ", input$select)
            )
            head(df_postgres, n = input$num)
          }
        )
      }
    )
  }
)