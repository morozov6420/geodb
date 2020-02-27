library(shiny)
library(leaflet)
library(DBI)
library(RPostgreSQL)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(shinyTime)

# Create a database connection parameters
args <- list(
  drv = dbDriver("PostgreSQL"),
  dbname = "geo",
  host = "localhost",
  user = "postgres",
  password = "a"
)

# disconnect all connections
# for (i in dbListConnections(dbDriver("PostgreSQL"))){
#   dbDisconnect(i)
# }

shinyServer(
  function(input, output, session) {
    # select query ---- 
    # create a map
    output$r1_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lat = 40.7, lng = -73.9, zoom = 10) %>% 
        clearShapes()
    })
    
    # create a table
    # observe clicking event
    # After clicking on the button submit, 
    # the query will be executed
    output$r1_table <- renderDataTable({
      if (input$r1_submit == 0)
        return()
      isolate({
        ifelse(
          grepl("^select", tolower(input$r1_req)), # if query starts with select
          {
            con <- do.call(DBI::dbConnect, args)
            on.exit(dbDisconnect(con))
            dat <- dbGetQuery(con, input$r1_req)
          },
          dat <- data.frame(message = "Something wrong")
        )
        dat
      })
    }, 
    options = list(searching = FALSE))
    
    # create query-viewing field
    output$r1_request <- renderPrint({ 
      if (input$r1_submit == 0)
        return()
      isolate({
        cat(input$r1_req, sep = "")
      })
    })
    
    # After clicking on the button submit, 
    # the map will be updated
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
                "lng: ", round(lng, 4), "<br>",
                "lat: ", round(lat, 4)
              )
            )
        },
        return()
      )
    })
    # After clicking on the button clean, 
    # the map will be cleaned off
    observeEvent(input$r1_clean, {
      leafletProxy('r1_map') %>%
        clearGroup(
          group = "crimes"
        )
    })
    
    # k-NN ----
    # create a map
    output$r2_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lat = 40.7, lng = -73.9, zoom = 10) %>%
        clearShapes() %>% 
        addAwesomeMarkers(
          group = 'circles',
          lng = -73.9,
          lat = 40.7,
          icon = awesomeIcons(markerColor = "red"),
          popup = paste0(
            "lat: ", -73.9, "<br>",
            "lng: ", 40.7
          )
        )
    })
    
    # create a table
    # After clicking on the button submit, 
    # the query will be executed
    output$r2_table <- renderDataTable({
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
    }, 
    options = list(searching = FALSE))
    
    # create query-viewing field
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
        request__2 <- paste0(request_2, collapse = "")
        cat(request__2, sep = "")
      })
    })
    
    # After clicking on the map, 
    # lat and lng will be updated
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
    
    # After clicking on the button submit, 
    # the map will be updated
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
              "lng: ", round(lng, 4), "<br>",
              "lat: ", round(lat, 4)
            )
          ) %>%
          addMarkers(
            group = "circles",
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
        )
    })
    
    # Search in the polygon ----
    # create a map
    output$r3_map <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>% 
        setView(lat = 40.7, lng = -73.9, zoom = 10) %>% 
        clearShapes()
    })
    
    # create a table
    # observe clicking event
    # After clicking on the button submit, 
    # the query will be executed
    output$r3_table <- renderDataTable({
      if (input$r3_submit == 0)
        return()
      isolate({
        request_3__2 <- paste0(request_3_2, collapse = "")
        con <- do.call(DBI::dbConnect, args)
        on.exit(dbDisconnect(con))
        dbGetQuery(con, request_3__2)
      })
    }, 
    options = list(searching = FALSE))
    
    # create query-viewing field
    output$r3_request <- renderPrint({
      if (input$r3_submit == 0)
        return()
      isolate({
        request_3__1 <- paste0(request_3_1, collapse = "")
        request_3__2 <- paste0(request_3_2, collapse = "")
        request_3__1 <- gsub(", ", ", \n        ", request_3__1)
        request_3__2 <- gsub(", ", ", \n      ", request_3__2)
        cat(request_3__1, sep = "")
        cat(request_3__2, sep = "")
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
      if (nrow(r3_coord) > 0){
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
                "lng: ", round(lng, 4), "<br>",
                "lat: ", round(lat, 4)
              )
            )
        }
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
    
    # Categorical features ----
    params_4 <- reactive({
      con <- do.call(DBI::dbConnect, args)
      on.exit(dbDisconnect(con))
      dbGetQuery(con, request_4_1)
    })
    # create a map
    output$r4_map <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>% 
        setView(lat = 40.7, lng = -73.9, zoom = 10) %>% 
        clearShapes()
    })
    
    # create a table
    # After clicking on the button submit, 
    # the query will be executed
    output$r4_table <- renderDataTable({
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
    }, 
    options = list(searching = FALSE))
    
    # create query-viewing field
    output$r4_request <- renderPrint({
      if (input$r4_submit == 0)
        return()
      isolate({
        request_4_2[2] <- input$r4_par
        request_4_3[2] <- input$r4_par
        if(input$r4_par == 'region' & input$r4_param_level == "NA"){
          request_4_3[3] <- " is "
          request_4_3[4] <- "NULL"
          request_4_3[5] <- ""
        } else {
          request_4_3[4] <- input$r4_param_level
        }
        cat(request_4_1, sep = "")
        cat(request_4_2, sep = "")
        cat(request_4_3, sep = "")
      })
    })
    output$r4_param <- renderUI({
      selectInput(
        inputId = "r4_par",
        label = "Choose a parameter",
        choices = params_4()$params
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
        label = "Choose a value",
        choices = dat$levels
      )
    })
    
    # After clicking on the button submit, 
    # the map will be updated
    observeEvent(input$r4_submit, {
      if(input$r4_par == 'region' & input$r4_param_level == "NA"){
        request_4_3[3] <- " is "
        request_4_3[4] <- "NULL"
        request_4_3[5] <- ""
      } else {
        request_4_3[4] <- input$r4_param_level
      }
        request_4_3[2] <- input$r4_par
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
            "lng: ", round(lng, 4), "<br>",
            "lat: ", round(lat, 4)
          )
        )
    })
    
    # After clicking on the button clean, 
    # the map will be cleaned off
    observeEvent(input$r4_clean, {
      leafletProxy('r4_map') %>% 
        clearGroup(
          group = "crimes"
        )
    })
    
    # Search in the vicinity ----
    # create a map
    output$r5_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lat = 40.7, lng = -73.9, zoom = 10) %>%
        clearShapes() %>% 
        addAwesomeMarkers(
          group = 'circles',
          lng = -73.9,
          lat = 40.7,
          icon = awesomeIcons(markerColor = "red"),
          popup = paste0(
            "lat: ", -73.9, "<br>",
            "lng: ", 40.7
          )
        )
    })
    
    # create a table
    # After clicking on the button submit and check input format, 
    # the query will be executed
    output$r5_table <- renderDataTable({
      if (input$r5_submit == 0 ||
          (suppressWarnings(
            is.na(
              as.numeric(
                input$r5_lng
              )
            )
          ) ||
          suppressWarnings(
            is.na(
              as.numeric(
                input$r5_lat
              )
            )
          ))
      )
      return()
      isolate({
        request_5[2] <- input$r5_lng
        request_5[4] <- input$r5_lat
        request_5[6] <- input$r5_dist
        req_5 <- paste0(request_5, collapse = "")
        con <- do.call(DBI::dbConnect, args)
        on.exit(dbDisconnect(con))
        dbGetQuery(con, req_5)
      })
    }, 
    options = list(searching = FALSE))
    
    # create query-viewing field
    output$r5_request <- renderPrint({
      if (input$r5_submit == 0 ||
          (suppressWarnings(
            is.na(
              as.numeric(
                input$r5_lng
              )
            )
          ) ||
          suppressWarnings(
            is.na(
              as.numeric(
                input$r5_lat
              )
            )
          ))
      )
      return()
      isolate({
        request_5[2] <- input$r5_lng
        request_5[4] <- input$r5_lat
        request_5[6] <- input$r5_dist
        cat(request_5, sep = "")
      })
    })
    observeEvent(input$r5_map_click, {
      click <- input$r5_map_click
      clat <- click$lat
      clng <- click$lng
      leafletProxy("r5_map") %>%
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
        inputId = "r5_lat",
        label = "Latitude",
        value = clat
      )
      updateTextInput(
        session = session,
        inputId = "r5_lng",
        label = "Longitude",
        value = clng
      )
    })
    
    # After clicking on the button submit, 
    # the map will be updated
    observeEvent(input$r5_submit, {
      if (suppressWarnings(
        is.na(
          as.numeric(
            input$r5_lng
          )
        )
      ) ||
      suppressWarnings(
        is.na(
          as.numeric(
            input$r5_lat
          )
        )
      )) {
        return()
      } else {
        con <- do.call(DBI::dbConnect, args)
        on.exit(dbDisconnect(con))
        request_5[2] <- input$r5_lng
        request_5[4] <- input$r5_lat
        request_5[6] <- input$r5_dist
        req_5 <- paste0(request_5, collapse = "")
        dat <- dbGetQuery(con, req_5)
        leafletProxy('r5_map') %>%
          clearGroup(group = "crimes") %>%
          clearGroup(group = "circles") %>% 
          addCircles(
            group = "crimes",
            lat = as.numeric(input$r5_lat),
            lng = as.numeric(input$r5_lng),
            radius = input$r5_dist
          ) %>%
          addMarkers(
            group = "circles",
            lng = as.numeric(input$r5_lng),
            lat = as.numeric(input$r5_lat),
            popup = paste0(
              "lat: ", round(as.numeric(input$r5_lng), 4), "<br>",
              "lng: ", round(as.numeric(input$r5_lat), 4)
            )
          )
        if (nrow(dat) > 0){
          leafletProxy('r5_map', data = dat) %>%
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
                "lng: ", round(lng, 4), "<br>",
                "lat: ", round(lat, 4)
              )
            )
        }
      }
    })
    
    # After clicking on the button clean, 
    # the map will be cleaned off
    observeEvent(input$r5_clean, {
      leafletProxy('r5_map') %>%
        clearGroup(
          group = "crimes"
        )
    })
    
    # input point ----
    # create a map
    output$r6_1_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lat = 40.7, lng = -73.9, zoom = 10) %>%
        clearShapes() %>% 
        addAwesomeMarkers(
          group = "circles",
          lng = -73.9,
          lat = 40.7,
          icon = awesomeIcons(markerColor = "red"),
          popup = paste0(
            "lat: ", -73.9, "<br>",
            "lng: ", 40.7
          )
        )
    })
    
    # create a table
    # After clicking on the button submit and check input format, 
    # the query will be executed
    output$r6_1_table <- renderDataTable({
      if (input$r6_1_submit == 0 ||
          (suppressWarnings(
            is.na(
              as.numeric(
                input$r6_1_lng
              )
            )
          ) ||
          suppressWarnings(
            is.na(
              as.numeric(
                input$r6_1_lat
              )
            )
          ))
      )
      return()
      isolate({
        insert_6_1
      })
    }, 
    options = list(searching = FALSE))
    
    # create query-viewing field
    output$r6_1_request <- renderPrint({
      if (input$r6_1_submit == 0 ||
          (suppressWarnings(
            is.na(
              as.numeric(
                input$r6_1_lng
              )
            )
          ) ||
          suppressWarnings(
            is.na(
              as.numeric(
                input$r6_1_lat
              )
            )
          ))
      )
      return()
      isolate({
        request_6__1_1 <- paste0(request_6_1_1, collapse = "")
        cat(request_6__1_1, sep = "")
      })
    })
    observeEvent(input$r6_1_map_click, {
      click <- input$r6_1_map_click
      clat <- click$lat
      clng <- click$lng
      leafletProxy("r6_1_map") %>%
        clearGroup(group = "circles") %>%
        addAwesomeMarkers(
          group = "circles",
          lng = clng,
          lat = clat,
          icon = awesomeIcons(markerColor = "red"),
          popup = paste0("lat: ", round(clat, 4), "<br>",
                         "lng: ", round(clng, 4))
        )
      updateTextInput(
        session = session,
        inputId = "r6_1_lat",
        label = "Latitude",
        value = clat
      )
      updateTextInput(
        session = session,
        inputId = "r6_1_lng",
        label = "Longitude",
        value = clng
      )
    })
    
    # After clicking on the button submit, 
    # the map will be updated
    observeEvent(input$r6_1_submit, {
      if (suppressWarnings(
        is.na(
          as.numeric(
            input$r6_1_lng
          )
        )
      ) ||
      suppressWarnings(
        is.na(
          as.numeric(
            input$r6_1_lat
          )
        )
      )) {
        return()
      } else {
        con <- do.call(DBI::dbConnect, args)
        on.exit(dbDisconnect(con))
        request_6_1_1[2] <<- input$r6_1_age
        request_6_1_1[4] <<- input$r6_1_sex
        request_6_1_1[6] <<- input$r6_1_type
        request_6_1_1[8] <<- input$r6_1_lng
        request_6_1_1[10] <<- input$r6_1_lat
        request_6_1_1[12] <<- as.character(input$r6_1_date)
        request_6_1_1[14] <<- str_split(input$r6_1_time, " ", simplify = T)[2]
        request_6_1__1 <- paste0(request_6_1_1, collapse = "")
        insert_6_1 <<- dbGetQuery(con, request_6_1__1)
        dbCommit(con)
        leafletProxy('r6_1_map', data = insert_6_1) %>%
          clearGroup(group = "crimes") %>%
          clearGroup(group = "circles") %>%
          addMarkers(
            group = "crimes",
            lng = ~ lng,
            lat = ~ lat,
            popup = ~ paste0(
              "Age: ", age_group, "<br>",
              "Type: ", cr_type, "<br>",
              "Date: ", date, "<br>",
              "Time :", time, "<br>",
              "lng: ", round(lng, 4), "<br>",
              "lat: ", round(lat, 4)
            )
          )
      }
    })
    
    # After clicking on the button clean, 
    # the map will be cleaned off
    observeEvent(input$r6_1_clean, {
      leafletProxy('r6_1_map') %>%
        clearGroup(group = "crimes") %>%
        clearGroup(group = "circles")
    })
    
    # input polygon ----
    # create a map
    output$r6_2_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lat = 40.7, lng = -73.9, zoom = 10) %>%
        clearShapes() %>% 
        addAwesomeMarkers(
          group = "circles",
          lng = -73.9,
          lat = 40.7,
          icon = awesomeIcons(markerColor = "red"),
          popup = paste0(
            "lat: ", -73.9, "<br>",
            "lng: ", 40.7
          )
        )
    })
    
    # create a table
    # After clicking on the button submit, 
    # the query will be executed
    output$r6_2_table <- renderDataTable({
      if (input$r6_2_submit == 0)
        return()
      isolate({
        insert_poly
      })
    }, 
    options = list(searching = FALSE))
    
    # create query-viewing field
    output$r6_2_request <- renderPrint({
      if (input$r6_2_submit == 0)
        return()
      isolate({
        request_6__2_1 <- paste0(request_6_2_1, collapse = "")
        request_6__2_2 <- paste0(request_6_2_2, collapse = "")
        request_6__2_1 <- gsub(", ", ", \n      ", request_6__2_1)
        request_6__2_2 <- gsub(", ", ", \n      ", request_6__2_2)
        cat(request_6__2_1, sep = "")
        cat(request_6__2_2, sep = "")
      })
    })
    observeEvent(input$r6_2_map_click, {
      click <- input$r6_2_map_click
      clat <- click$lat
      clng <- click$lng
      if (nrow(r6_2_coord) < 4){
        r6_2_coord <<- rbind(r6_2_coord, c(clng, clat))
        r6_2_coord <<- rbind(r6_2_coord, c(clng, clat))
        r6_2_coord <<- rbind(r6_2_coord, c(clng, clat))
      }
      r6_2_coord <<- rbind(r6_2_coord, c(clng, clat))
      r6_2_coord_copy <- rbind(r6_2_coord, r6_2_coord[1,])
      colnames(r6_2_coord_copy) <- c("lng", "lat")
      if (input$r6_2_check){
        request_6_2_1[2] <- "st_convexhull"
      } else {
        request_6_2_1[2] <- "st_makepolygon"
      }
      request_6_2_1[4] <- paste(
        r6_2_coord_copy$lng, 
        r6_2_coord_copy$lat, 
        sep = " ", 
        collapse = ", "
      )
      request_6_2_1 <- paste0(request_6_2_1, collapse = "")
      con <- do.call(DBI::dbConnect, args)
      on.exit(dbDisconnect(con))
      dat_1 <- dbGetQuery(con, request_6_2_1)
      r6_2_cooord <<- str_sub(dat_1[1,1], 16, -4) %>%
        str_split(",", simplify = T) %>%
        str_split(" ", simplify = T) %>%
        as.data.frame() %>%
        `colnames<-`(c("lng", "lat")) %>%
        mutate(
          lat = as.numeric(as.character(lat)),
          lng = as.numeric(as.character(lng))
        )
      leafletProxy("r6_2_map") %>%
        clearGroup("poly") %>%
        clearGroup("circles") %>%
        clearGroup("crimes") %>%
        clearGroup("line") %>% 
        addPolygons(
          group = "poly",
          lat = r6_2_cooord$lat,
          lng = r6_2_cooord$lng,
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
          lng = r6_2_coord_copy[-nrow(r6_2_coord_copy),1],
          lat = r6_2_coord_copy[-nrow(r6_2_coord_copy),2],
          color = "red"
        )
    })
    
    # After clicking on the button submit, 
    # the map will be updated
    observeEvent(input$r6_2_submit, {
      if (nrow(r6_2_coord) > 0){
        request_6_2_2[2] <<- input$r6_2_region
        r6_2_coord_copy <- rbind(r6_2_coord, r6_2_coord[1,])
        colnames(r6_2_coord_copy) <- c("lng", "lat")
        if (input$r6_2_check){
          request_6_2_1[2] <<- "st_convexhull"
          request_6_2_2[4] <<- "st_convexhull"
        } else {
          request_6_2_1[2] <<- "st_makepolygon"
          request_6_2_2[4] <<- "st_makepolygon"
        }
        request_6_2_1[4] <<- paste(
          r6_2_coord_copy$lng, 
          r6_2_coord_copy$lat, 
          sep = " ", 
          collapse = ", "
        )
        request_6_2_2[6] <<- paste(
          r6_2_coord_copy$lng,
          r6_2_coord_copy$lat,
          sep = " ",
          collapse = ", "
        )
        request_6_2_2 <- paste0(request_6_2_2, collapse = "")
        con <- do.call(DBI::dbConnect, args)
        on.exit(dbDisconnect(con))
        insert_poly <<- dbGetQuery(con, request_6_2_2)
        request_6_2_1 <- paste0(request_6_2_1, collapse = "")
        con <- do.call(DBI::dbConnect, args)
        on.exit(dbDisconnect(con))
        dat_1 <- dbGetQuery(con, request_6_2_1)
        r6_2_cooord <<- str_sub(dat_1[1,1], 16, -4) %>%
          str_split(",", simplify = T) %>%
          str_split(" ", simplify = T) %>%
          as.data.frame() %>%
          `colnames<-`(c("lng", "lat")) %>%
          mutate(
            lat = as.numeric(as.character(lat)),
            lng = as.numeric(as.character(lng))
          )
        leafletProxy("r6_2_map", data = r6_2_cooord) %>%
          clearGroup("poly") %>%
          clearGroup("crimes") %>%
          addPolygons(
            group = "crimes",
            lat = r6_2_cooord$lat,
            lng = r6_2_cooord$lng,
            color = "#03F"
          ) %>% 
          addPolylines(
            group = "line",
            lng = r6_2_coord_copy[-nrow(r6_2_coord_copy),1],
            lat = r6_2_coord_copy[-nrow(r6_2_coord_copy),2],
            color = "red"
          ) 
        r6_2_coord <<- data.frame(numeric(), numeric())
        r6_2_cooord <<- data.frame(numeric(), numeric())  
      }
    })
    
    # After clicking on the button clean, 
    # the map will be cleaned off
    observeEvent(input$r6_2_clean, {
      r6_2_coord <<- data.frame(numeric(), numeric())
      r6_2_cooord <<- data.frame(numeric(), numeric())
      leafletProxy('r6_2_map') %>% 
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
    
    # linkages ----
    
    params_7 <- reactive({
      con <- do.call(DBI::dbConnect, args)
      on.exit(dbDisconnect(con))
      dbGetQuery(con, request_4_1)
    })
    # create a map
    output$r7_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lat = 40.7, lng = -73.9, zoom = 10) %>%
        clearShapes()
    })
    
    # create a table
    # After clicking on the button submit, 
    # the query will be executed
    output$r7_table <- renderDataTable({
      if (input$r7_submit == 0)
        return()
      request_7_4[2] <- input$r7_par
      if(input$r7_par == 'region' & input$r7_param_level_1 == "NA"){
        request_7_4[3] <- "_1 is "
        request_7_4[4] <- "NULL"
        request_7_4[5] <- ""
      } else {
        request_7_4[4] <- input$r7_param_level_1
      }
      request_7_4[7] <- input$r7_par
      if(input$r7_par == 'region' & input$r7_param_level_2 == "NA"){
        request_7_4[8] <- " is "
        request_7_4[9] <- "NULL"
        request_7_4[10] <- ""
      } else {
        request_7_4[9] <- input$r7_param_level_2
      }
      request_7__4 <- paste0(request_7_4, collapse = "")
      con <- do.call(DBI::dbConnect, args)
      on.exit(dbDisconnect(con))
      dat <- dbGetQuery(con, request_7__4)
    },
    options = list(searching = FALSE))
    
    # create query-viewing field
    output$r7_request <- renderPrint({
      if (input$r7_submit == 0)
        return()
      isolate({
        request_7_2[2] <- input$r7_par
        request_7_3[2] <- input$r7_par
        if(input$r7_par == "region" & input$r7_param_level_1 == "NA"){
          request_7_3[3] <- "_1 is "
          request_7_3[4] <- "NULL"
          request_7_3[5] <- ""
        } else {
          request_7_3[4] <- input$r7_param_level_1
        }
        request_7_3[7] <- input$r7_par
        if(input$r7_par == "region" & input$r7_param_level_2 == "NA"){
          request_7_3[8] <- " is "
          request_7_3[9] <- "NULL"
          request_7_3[10] <- ""
        } else {
          request_7_3[9] <- input$r7_param_level_2
        }
        request_7_4[2] <- input$r7_par
        if(input$r7_par == 'region' & input$r7_param_level_1 == "NA"){
          request_7_4[3] <- "_1 is "
          request_7_4[4] <- "NULL"
          request_7_4[5] <- ""
        } else {
          request_7_4[4] <- input$r7_param_level_1
        }
        request_7_4[7] <- input$r7_par
        if(input$r7_par == 'region' & input$r7_param_level_2 == "NA"){
          request_7_4[8] <- " is "
          request_7_4[9] <- "NULL"
          request_7_4[10] <- ""
        } else {
          request_7_4[9] <- input$r7_param_level_2
        }
        cat(request_7_1, sep = "")
        cat(request_7_2, sep = "")
        cat(request_7_3, sep = "")
        cat(request_7_4, sep = "")
      })
    })
    output$r7_param <- renderUI({
      selectInput(
        inputId = "r7_par",
        label = "Choose a parameter",
        choices = params_7()$params
      )
    })
    observeEvent(input$r7_par, {
      request_7_2[2] <- input$r7_par
      request_7__2 <- paste0(request_7_2, collapse = "")
      con <- do.call(DBI::dbConnect, args)
      on.exit(dbDisconnect(con))
      dat <- dbGetQuery(con, request_7__2)
      updateSelectInput(
        session = session,
        inputId = "r7_param_level_1",
        label = "Choose a value",
        choices = dat$levels
      )
      updateSelectInput(
        session = session,
        inputId = "r7_param_level_2",
        label = "Choose a value",
        choices = dat$levels
      )
    })

    # After clicking on the button submit, 
    # the map will be updated
    observeEvent(input$r7_submit, {
      request_7_3[2] <- input$r7_par
      if(input$r7_par == "region" & input$r7_param_level_1 == "NA"){
        request_7_3[3] <- "_1 is "
        request_7_3[4] <- "NULL"
        request_7_3[5] <- ""
      } else {
        request_7_3[4] <- input$r7_param_level_1
      }
      request_7_3[7] <- input$r7_par
      if(input$r7_par == "region" & input$r7_param_level_2 == "NA"){
        request_7_3[8] <- " is "
        request_7_3[9] <- "NULL"
        request_7_3[10] <- ""
      } else {
        request_7_3[9] <- input$r7_param_level_2
      }
      request_7__3 <- paste0(request_7_3, collapse = "")
      con <- do.call(DBI::dbConnect, args)
      on.exit(dbDisconnect(con))
      dat <- dbGetQuery(con, request_7__3) %>% 
        mutate(row_num = as.numeric(rownames(.)))
      leafletProxy("r7_map", data = dat) %>%
        clearGroup("points") %>%
        addCircles(
          group = "points",
          lat = ~ lat_1,
          lng = ~ lng_1,
          stroke = F,
          fillColor = 'red',
          fillOpacity = 1,
          radius = 100,
          popup = ~ paste(
            "Age: ", age_group_1, "<br>",
            "Type: ", cr_type_1, "<br>",
            "Sex: ", vic_sex_1, "<br>",
            "Date: ", date_1, "<br>",
            "Time :", time_1, "<br>",
            "Region :", region_1, "<br>",
            "lng: ", round(lng_1, 4), "<br>",
            "lat: ", round(lat_1, 4)
          )
        ) %>% 
        addCircles(
          group = "points",
          lat = ~ lat_2,
          lng = ~ lng_2,
          stroke = F,
          fillColor = 'green',
          fillOpacity = 1,
          radius = 100,
          popup = ~ paste(
            "Age: ", age_group_2, "<br>",
            "Type: ", cr_type_2, "<br>",
            "Sex: ", vic_sex_2, "<br>",
            "Date: ", date_2, "<br>",
            "Time :", time_2, "<br>",
            "Region :", region_2, "<br>",
            "lng: ", round(lng_2, 4), "<br>",
            "lat: ", round(lat_2, 4)
          )
        )
      
      ifelse(
        input$r7_link == "UPGMA",
        dat %>% 
          select(lat_1, lat_2, lng_1, lng_2) %>% 
          unite("coord_1", lat_1, lng_1, sep = "_", remove = T) %>% 
          unite("coord_2", lat_2, lng_2, sep = "_", remove = T) %>% 
          pivot_longer(cols = c(coord_1, coord_2), 
                       names_to = "coord_num", 
                       values_to = "coord") %>% 
          separate(coord, c("lat", "lng"), "_") %>% 
          mutate(coord_num = NULL,
                 lat = as.numeric(lat),
                 lng = as.numeric(lng)) %>% 
          leafletProxy("r7_map", data = .) %>% 
          clearGroup("lines") %>%
          addPolylines(
            group = "lines",
            lat = ~ lat,
            lng = ~ lng,
            weight = 2
          ),
        ifelse(
          input$r7_link == "Centroid",
          leafletProxy("r7_map") %>%
            clearGroup("lines") %>%
            addPolylines(
              group = "lines",
              lng = c(mean(dat[, 8]), mean(dat[, 17])),
              lat = c(mean(dat[, 9]), mean(dat[, 18])),
              weight = 2,
              popup = "Centroid"
            ),
          ifelse(
            input$r7_link == "Complete",
            {
              temp <- which.max((dat[, 8] - dat[, 17]) ** 2 + (dat[, 9] - dat[, 18]) ** 2)
              leafletProxy("r7_map") %>%
                clearGroup("lines") %>%
                addPolylines(
                  group = "lines",
                  lng = c(dat[temp, 8], dat[temp, 17]),
                  lat = c(dat[temp, 9], dat[temp, 18]),
                  weight = 2,
                  popup = "Complete"
                )
            },
            ifelse(
              input$r7_link == "Single",
              {
                temp <- which.min((dat[, 8] - dat[, 17]) ** 2 + (dat[, 9] - dat[, 18]) ** 2)
                leafletProxy("r7_map") %>%
                  clearGroup("lines") %>%
                  addPolylines(
                    group = "lines",
                    lng = c(dat[temp, 8], dat[temp, 17]),
                    lat = c(dat[temp, 9], dat[temp, 18]),
                    weight = 2,
                    popup = "Single"
                  )
              },
              print("nothing")
            )
          )
        )
      )
    })
    
    # After clicking on the button clean, 
    # the map will be cleaned off
    observeEvent(input$r7_clean, {
      leafletProxy('r7_map') %>%
        clearGroup(
          group = "lines"
        ) %>% 
        clearGroup(
          group = "points"
        )
    })
  }
)