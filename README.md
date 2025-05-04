# NYC Uber 2014 Dashboard

## Live App
https://cpriyanjana.shinyapps.io/ShinyUber/

---

## Loading and Wrangling Data
```r
master_url <- "https://raw.githubusercontent.com/cpriyanjana/Uber/main/master_data.rds"
tf <- tempfile(fileext = ".rds")
download.file(master_url, tf, mode = "wb")
uber_raw <- readRDS(tf)
unlink(tf)

uber_raw <- uber_raw %>% mutate(WeekOfMonth = ceiling(Day/7))

month_levels <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep")
wday_levels  <- c("Monday", "Tuesday", "Wednesday", "Thursday",
                  "Friday", "Saturday", "Sunday")
```

---

## Pivot & Plots
```r
#Trips Every Hour
  output$plot_hour <- renderPlot({
    ggplot(uber_raw %>% count(Hour), aes(Hour, n)) +
      geom_col(fill = "#1f77b4") +
      scale_x_continuous(breaks = 0:23) +
      scale_y_continuous(labels = comma, breaks = pretty_breaks(6)) +
      labs(x = "Hour", y = "Trips") +
      theme_minimal(base_size = 13)
  })
  
  #Hourly patterns by Month
  output$plot_hour_month <- renderPlot({
    ggplot(uber_raw %>% count(Month, Hour) %>%
             mutate(Month = factor(Month, levels = month_levels)),
           aes(Hour, n, colour = Month)) +
      geom_line(linewidth = 1) + geom_point(size = 1.4) +
      scale_colour_viridis_d(option = "C") +
      scale_x_continuous(breaks = 0:23) +
      labs(x = "Hour", y = "Trips") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank())
  })
  
  #Trips by Day of Month
  output$plot_day <- renderPlot({
    ggplot(uber_raw %>% count(Day), aes(Day, n)) +
      geom_col(fill = "#e07a5f") +
      scale_x_continuous(breaks = 1:31) +
      scale_y_continuous(labels = comma, breaks = pretty_breaks(6)) +
      labs(x = "Day", y = "Trips") +
      theme_minimal(base_size = 13)
  })
  
  #Weekday & Month facet plot
  output$plot_wday_month <- renderPlot({
    d <- uber_raw %>% count(Wday, Month) %>%
      mutate(Wday = factor(Wday, levels = wday_levels),
             Month = factor(Month, levels = month_levels))
    ggplot(d, aes(Wday, n, fill = Wday)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ Month, ncol = 3) +
      scale_fill_viridis_d(option = "B") +
      scale_y_continuous(labels = comma, breaks = pretty_breaks(6)) +
      labs(x = "Weekday", y = "Trips") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #Trips by Month
  output$plot_month <- renderPlot({
    ggplot(uber_raw %>% count(Month) %>%
             mutate(Month = factor(Month, levels = month_levels)),
           aes(Month, n)) +
      geom_col(fill = "#43aa8b") +
      scale_y_continuous(labels = comma, breaks = pretty_breaks(6)) +
      labs(x = "Month", y = "Trips") +
      theme_minimal(base_size = 13)
  })
  
  #Base & Month faceted
  output$plot_base_month <- renderPlot({
    d <- uber_raw %>% count(Base, Month) %>%
      mutate(Month = factor(Month, levels = month_levels))
    ggplot(d, aes(Base, n, fill = Base)) +
      geom_col(show.legend = FALSE) +
      scale_fill_viridis_d(option = "D") +
      facet_wrap(~ Month, ncol = 3) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
```

---

## Heat Maps
```r

#Heatmap Hour vs Weekday
  output$heat1 <- renderPlot({
    d <- uber_raw %>% count(Hour, Wday) %>%
      mutate(Wday = factor(Wday, levels = rev(wday_levels)))
    ggplot(d, aes(Hour, Wday, fill = n)) +
      geom_tile() +
      scale_fill_viridis_c(option = "magma", trans = "sqrt") +
      scale_x_continuous(breaks = seq(0, 23, 2)) +
      labs(x = "Hour", y = "Weekday", fill = "Trips") +
      theme_minimal(base_size = 12)
  })
  
  #Heatmap Day vs Month
  output$heat2 <- renderPlot({
    d <- uber_raw %>% count(Day, Month) %>%
      mutate(Month = factor(Month, levels = rev(month_levels)))
    ggplot(d, aes(Day, Month, fill = n)) +
      geom_tile() +
      scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
      scale_x_continuous(breaks = seq(1, 31, 5)) +
      labs(x = "Day", y = "Month", fill = "Trips") +
      theme_minimal(base_size = 12)
  })
  
  #Heatmap Week‑of‑Month vs Month
  output$heat3 <- renderPlot({
    d <- uber_raw %>% mutate(WoM = factor(WeekOfMonth, levels = 1:5),
                             Month = factor(Month, levels = rev(month_levels))) %>%
      count(WoM, Month)
    ggplot(d, aes(WoM, Month, fill = n)) +
      geom_tile() +
      scale_fill_viridis_c(option = "inferno", trans = "sqrt") +
      labs(x = "Week of Month", y = "Month", fill = "Trips") +
      theme_minimal(base_size = 12)
  })
  
  #Heatmap Base vs Weekday
  output$heat4 <- renderPlot({
    d <- uber_raw %>% count(Base, Wday) %>%
      mutate(Base = factor(Base, levels = rev(sort(unique(Base)))),
             Wday = factor(Wday, levels = wday_levels))
    ggplot(d, aes(Wday, Base, fill = n)) +
      geom_tile() +
      scale_fill_viridis_c(option = "cividis", trans = "sqrt") +
      labs(x = "Weekday", y = "Dispatch Base", fill = "Trips") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
```

---

## Leaflet Map
```r
output$map <- renderLeaflet({
    samp <- sample_n(uber_raw, 50000)
    leaflet(samp) %>% addTiles() %>%
      addCircleMarkers(lng = ~Lon, lat = ~Lat, radius = 4,
                       color = "#2b83ba",
                       stroke = FALSE, fillOpacity = 0.35,
                       clusterOptions = markerClusterOptions()) %>%
      setView(lng = mean(samp$Lon, na.rm = TRUE),
              lat = mean(samp$Lat, na.rm = TRUE), zoom = 12)
  })
```

---

## Prediction Model
```r
  ride_pred <- eventReactive(input$goPred, {
    hr <- as.numeric(input$pred_hour)
    wd <- as.integer(factor(input$pred_wday, levels = wday_levels))
    mo <- as.integer(factor(input$pred_month, levels = month_levels))
    predict(pred_model, newdata = data.frame(
      Hour = hr,
      Wday_num = wd,
      Month_num = mo
    ))
  })
  
  output$pred_text <- renderText({
    req(input$goPred)
    paste0("Estimated rides: ",
           formatC(round(ride_pred()), format = "d", big.mark = ","))
  })
}
```
