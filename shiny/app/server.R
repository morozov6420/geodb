# setwd('D:/Учёба/МАГА/Германия/GeoDB/experiments')
library(shiny)
library(leaflet)
library(DBI)
library(RPostgreSQL)
library(dplyr)
library(leafem)
library(stringr)
rm(list = ls())

args <- list(
  drv = dbDriver("PostgreSQL"),
  dbname = "geo",
  host = "localhost",
  user = "postgres",
  password = "a"
)


# disconnect all connections
# for (i in dbListConnections(db_drv)){
#   dbDisconnect(i)
# }

shinyServer(function(session, input, output) {
  output$r1_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = 40.7, lng = -73.9, zoom = 10) %>% 
      clearShapes()
  })
  output$r1_table <- renderTable({
    if (input$r1_submit == 0)
      return()
    isolate({
      ifelse(
        grepl("^select", tolower(input$r1_req)),
        {
          con <- do.call(DBI::dbConnect, args)
          on.exit(dbDisconnect(con))
          dat <- dbGetQuery(con, input$r1_req)
        },
        ifelse(
          grepl("drop", tolower(input$r1_req)) | 
            grepl("alter table", tolower(input$r1_req)),
          dat <- data.frame(message = "Don't do it"),
          ifelse(
            grepl("insert", tolower(input$r1_req)),
            {
              con <- do.call(DBI::dbConnect, args)
              on.exit(dbDisconnect(con))
              dat <- dbExecute(con, input$r1_req)
            },
            dat <- data.frame(message = "Use SELECT or INSERT")
          )
        )
      )
      dat
    })
  })
  output$r1_request <- renderPrint({
    if (input$r1_submit == 0)
      return()
    isolate({
      cat(input$r1_req, sep = "")
    })
  })
  observeEvent(input$r1_submit, {
    ifelse(
      grepl("^select", tolower(input$r1_req)) &&
        grepl("lat", tolower(input$r1_req)) &&
        grepl("lng", tolower(input$r1_req)),
      {
        con <- do.call(DBI::dbConnect, args)
        on.exit(dbDisconnect(con))
        dat <- dbGetQuery(con, input$r1_req)
        leafletProxy('r1_map', data = dat) %>% 
          clearGroup("crimes") %>% 
          addCircles(
            group = "crimes",
            lat = ~ lat,
            lng = ~ lng,
            weight = 1,
            color = 'black',
            fillColor = 'orange',
            fillOpacity = 0.5,
            opacity = 1,
            radius = 100,
            popup = ~ paste(
              "Age: ", age, "<br>",
              "Type: ", type, "<br>",
              "Date: ", date
            )
          )
      },
      ifelse(
        grepl("drop", tolower(input$r1_req)) ||
          grepl("alter table", tolower(input$r1_req)),
        return(),
        {
          con <- do.call(DBI::dbConnect, args)
          on.exit(dbDisconnect(con))
          dbExecute(con, input$r1_req)
        }
      )
    )
  })
  observeEvent(input$r1_clean, {
    leafletProxy('r1_map') %>% 
      clearGroup(
        group = "crimes"
      )
  })
  
  
  output$r2_map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lat = 40.7, lng = -73.9, zoom = 10) %>% 
      clearShapes()
  })
  output$r2_table <- renderTable({
    if (input$r2_submit == 0 || 
          (suppressWarnings(
            is.na(
              as.numeric(
                input$r2_lng
              )
            )
          ) ||
            suppressWarnings(
              is.na(
                as.numeric(
                  input$r2_lat
                )
              )
            ))
      )
      return()
    isolate({
      request_2[2] <- input$r2_lng
      request_2[4] <- input$r2_lat
      request_2[6] <- input$r2_nn
      req_2 <- paste0(request_2, collapse = "")
      con <- do.call(DBI::dbConnect, args)
      on.exit(dbDisconnect(con))
      dbGetQuery(con, req_2)
    })
  })
  output$r2_request <- renderPrint({
    if (input$r2_submit == 0 ||
        (suppressWarnings(
          is.na(
            as.numeric(
              input$r2_lng
            )
          )
        ) ||
        suppressWarnings(
          is.na(
            as.numeric(
              input$r2_lat
            )
          )
        ))
    )
      return()
    isolate({
      request_2[2] <- input$r2_lng
      request_2[4] <- input$r2_lat
      request_2[6] <- input$r2_nn
      # req_2 <- paste0(request_2, collapse = "")
      cat(request_2, sep = "")
    })
  })
  observeEvent(input$r2_map_click, {
    click <- input$r2_map_click
    clat <- click$lat
    clng <- click$lng
    leafletProxy("r2_map") %>% 
      clearGroup(group = "circles") %>%
      addAwesomeMarkers(
        group = 'circles',
        lng = clng,
        lat = clat,
        icon = awesomeIcons(markerColor = "red"),
        popup = paste0("lat: ", round(clat, 4), "<br>",
                       "lng: ", round(clng, 4))
      )
    updateTextInput(
      session = session, 
      inputId = "r2_lat", 
      label = "Latitude",
      value = clat
    )
    updateTextInput(
      session = session, 
      inputId = "r2_lng", 
      label = "Longitude",
      value = clng
    )
  })
  observeEvent(input$r2_submit, {
    if (suppressWarnings(
      is.na(
        as.numeric(
          input$r2_lng
        )
      )
    ) || 
    suppressWarnings(
      is.na(
        as.numeric(
          input$r2_lat
        )
      )
    )) {
      return()
    } else {
      con <- do.call(DBI::dbConnect, args)
      on.exit(dbDisconnect(con))
      request_2[2] <- input$r2_lng
      request_2[4] <- input$r2_lat
      request_2[6] <- input$r2_nn
      req_2 <- paste0(request_2, collapse = "")
      dat <- dbGetQuery(con, req_2)
      leafletProxy('r2_map', data = dat) %>% 
        clearGroup(group = "crimes") %>%
        clearGroup(group = "circles") %>% 
        addCircles(
          group = "crimes",
          lat = ~ lat,
          lng = ~ lng,
          weight = 1,
          color = 'black',
          fillColor = 'orange',
          fillOpacity = 0.5,
          opacity = 1,
          radius = 100,
          popup = ~ paste(
            "Age: ", age, "<br>",
            "Type: ", type, "<br>",
            "Date: ", date, "<br>",
            "Time :", time, "<br>",
            "lng: ", lng, "<br>",
            "lat: ", lat
          )
        ) %>% 
        addMarkers(
          group = "crimes",
          lng = as.numeric(input$r2_lng),
          lat = as.numeric(input$r2_lat),
          popup = paste0(
            "lat: ", round(as.numeric(input$r2_lng), 4), "<br>",
            "lng: ", round(as.numeric(input$r2_lat), 4)
          )
        )
    }
  })
  observeEvent(input$r2_clean, {
    leafletProxy('r2_map') %>% 
      clearGroup(
        group = "crimes"
      ) %>% 
      clearGroup(
        group = "circles"
      )
  })
  
  output$r3_map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lat = 40.7, lng = -73.9, zoom = 10) %>% 
      clearShapes()
  })
  output$r3_table <- renderTable({
    if (input$r3_submit == 0)
      return()
    isolate({
      request_3__2 <- paste0(request_3_2, collapse = "")
      con <- do.call(DBI::dbConnect, args)
      on.exit(dbDisconnect(con))
      dbGetQuery(con, request_3__2)
    })
  })
  output$r3_request <- renderPrint({
    if (input$r3_submit == 0)
      return()
    isolate({
      cat(request_3_1, sep = "")
      cat("\n")
      cat(request_3_2, sep = "")
    })
  })
  observeEvent(input$r3_map_click, {
    click <- input$r3_map_click
    clat <- click$lat
    clng <- click$lng
    if (nrow(r3_coord) < 4){
      r3_coord <<- rbind(r3_coord, c(clng, clat))
      r3_coord <<- rbind(r3_coord, c(clng, clat))
      r3_coord <<- rbind(r3_coord, c(clng, clat))
    }
    r3_coord <<- rbind(r3_coord, c(clng, clat))
    r3_coord_copy <- rbind(r3_coord, r3_coord[1,])
    colnames(r3_coord_copy) <- c("lng", "lat")
    if (input$r3_check){
      request_3_1[2] <- "st_convexhull"
    } else {
      request_3_1[2] <- "st_makepolygon"
    }
    request_3_1[4] <- paste(
      r3_coord_copy$lng, 
      r3_coord_copy$lat, 
      sep = " ", 
      collapse = ", "
    )
    request_3_1 <- paste0(request_3_1, collapse = "")
    con <- do.call(DBI::dbConnect, args)
    on.exit(dbDisconnect(con))
    dat_1 <- dbGetQuery(con, request_3_1)
    r3_cooord <<- str_sub(dat_1[1,1], 16, -4) %>%
      str_split(",", simplify = T) %>%
      str_split(" ", simplify = T) %>%
      as.data.frame() %>%
      `colnames<-`(c("lng", "lat")) %>%
      mutate(
        lat = as.numeric(as.character(lat)),
        lng = as.numeric(as.character(lng))
      )
    leafletProxy("r3_map") %>%
      clearGroup("poly") %>%
      clearGroup("circles") %>%
      clearGroup("crimes") %>%
      clearGroup("line") %>% 
      addPolygons(
        group = "poly",
        lat = r3_cooord$lat,
        lng = r3_cooord$lng,
        color = "green"
      ) %>%
      addAwesomeMarkers(
        group = 'circles',
        lng = clng,
        lat = clat,
        icon = awesomeIcons(markerColor = "red"),
        popup = paste0("lat: ", round(clat, 4), "<br>",
                       "lng: ", round(clng, 4))
      ) %>% 
      addPolylines(
        group = "line",
        lng = r3_coord_copy[-nrow(r3_coord_copy),1],
        lat = r3_coord_copy[-nrow(r3_coord_copy),2],
        color = "red"
      )
  })
  observeEvent(input$r3_submit, {
    r3_coord_copy <- rbind(r3_coord, r3_coord[1,])
    colnames(r3_coord_copy) <- c("lng", "lat")
    if (input$r3_check){
      request_3_1[2] <<- "st_convexhull"
      request_3_2[2] <<- "st_convexhull"
    } else {
      request_3_1[2] <<- "st_makepolygon"
      request_3_2[2] <<- "st_makepolygon"
    }
    request_3_1[4] <<- paste(
      r3_coord_copy$lng, 
      r3_coord_copy$lat, 
      sep = " ", 
      collapse = ", "
    )
    request_3_2[4] <<- paste(
      r3_coord_copy$lng,
      r3_coord_copy$lat,
      sep = " ",
      collapse = ", "
    )
    request_3__1 <- paste0(request_3_1, collapse = "")
    request_3__2 <- paste0(request_3_2, collapse = "")
    con <- do.call(DBI::dbConnect, args)
    on.exit(dbDisconnect(con))
    dat_1 <- dbGetQuery(con, request_3__1)
    dat_2 <- dbGetQuery(con, request_3__2)
    r3_cooord <<- str_sub(dat_1[1,1], 16, -4) %>%
      str_split(",", simplify = T) %>%
      str_split(" ", simplify = T) %>%
      as.data.frame() %>%
      `colnames<-`(c("lng", "lat")) %>%
      mutate(
        lat = as.numeric(as.character(lat)),
        lng = as.numeric(as.character(lng))
      )
    
    if (nrow(dat_2) > 0){
      leafletProxy("r3_map", data = dat_2) %>%
        clearGroup("poly") %>%
        clearGroup("crimes") %>%
        addPolygons(
          group = "crimes",
          lat = r3_cooord$lat,
          lng = r3_cooord$lng,
          color = "#03F"
        ) %>% 
        addPolylines(
          group = "line",
          lng = r3_coord_copy[-nrow(r3_coord_copy),1],
          lat = r3_coord_copy[-nrow(r3_coord_copy),2],
          color = "red"
        ) %>% 
        addCircles(
          group = "crimes",
          lat = ~ lat,
          lng = ~ lng,
          weight = 1,
          color = 'black',
          fillColor = 'orange',
          fillOpacity = 0.5,
          opacity = 1,
          radius = 100,
          popup = ~ paste(
            "Age: ", age, "<br>",
            "Type: ", type, "<br>",
            "Date: ", date, "<br>",
            "Time :", time, "<br>",
            "lng: ", lng, "<br>",
            "lat: ", lat
          )
        )
    }
  })
  observeEvent(input$r3_clean, {
    r3_coord <<- data.frame(numeric(), numeric())
    r3_cooord <<- data.frame(numeric(), numeric())
    leafletProxy('r3_map') %>% 
      clearGroup(
        group = "crimes"
      ) %>% 
      clearGroup(
        group = "circles"
      ) %>% 
      clearGroup(
        group = "poly"
      ) %>% 
      clearGroup(
        group = "line"
      )
  })
  
  params <- reactive({
    con <- do.call(DBI::dbConnect, args)
    on.exit(dbDisconnect(con))
    dbGetQuery(con, request_4_1)
  })
  output$r4_map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lat = 40.7, lng = -73.9, zoom = 10) %>% 
      clearShapes()
  })
  output$r4_table <- renderTable({
    if (input$r4_submit == 0)
      return()
    isolate({
      request_4_3[2] <- input$r4_par
      request_4_3[4] <- input$r4_param_level
      request_4__3 <- paste0(request_4_3, collapse = "")
      con <- do.call(DBI::dbConnect, args)
      on.exit(dbDisconnect(con))
      dbGetQuery(con, request_4__3)
    })
  })
  output$r4_request <- renderPrint({
    if (input$r4_submit == 0)
      return()
    isolate({
      request_4_2[2] <- input$r4_par
      request_4_3[2] <- input$r4_par
      request_4_3[4] <- input$r4_param_level
      cat(request_4_1, sep = "")
      cat("\n")
      cat(request_4_2, sep = "")
      cat("\n")
      cat(request_4_3, sep = "")
    })
  })
  output$r4_param <- renderUI({
    selectInput(
      inputId = "r4_par",
      label = "Choose a param", 
      choices = params()$params
    )
  })
  observeEvent(input$r4_par, {
    request_4_2[2] <- input$r4_par
    request_4__2 <- paste0(request_4_2, collapse = "")
    con <- do.call(DBI::dbConnect, args)
    on.exit(dbDisconnect(con))
    dat <- dbGetQuery(con, request_4__2)
    updateSelectInput(
      session = session,
      inputId = "r4_param_level",
      label = "Choose a level",
      choices = dat$levels
    )
  })
  observeEvent(input$r4_submit, {
    request_4_3[2] <- input$r4_par
    request_4_3[4] <- input$r4_param_level
    request_4__3 <- paste0(request_4_3, collapse = "")
    con <- do.call(DBI::dbConnect, args)
    on.exit(dbDisconnect(con))
    dat <- dbGetQuery(con, request_4__3)
    leafletProxy("r4_map", data = dat) %>%
      clearGroup("crimes") %>% 
      addCircles(
        group = "crimes",
        lat = ~ lat,
        lng = ~ lng,
        weight = 1,
        color = 'black',
        fillColor = 'orange',
        fillOpacity = 0.5,
        opacity = 1,
        radius = 100,
        popup = ~ paste(
          "Age: ", age, "<br>",
          "Type: ", type, "<br>",
          "Date: ", date, "<br>",
          "Time :", time, "<br>",
          "Region :", region, "<br>",
          "lng: ", lng, "<br>",
          "lat: ", lat
        )
      )
  })
  observeEvent(input$r4_clean, {
    leafletProxy('r4_map') %>% 
      clearGroup(
        group = "crimes"
      )
  })
  
})
