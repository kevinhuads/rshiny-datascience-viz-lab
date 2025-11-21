skills   <- c("R","Python","SQL","MLOps","Machine Learning")
skills_y <- c(90, 90, 95, 80, 90)
cols     <- c("#2ecc71",  "#f1c40f", "#2980b9", "#d35400", "#2c3e50")

output$about_it <- renderHighchart({
  df <- data.frame(
    name  = skills,
    y     = skills_y,
    color = cols
  )
  
  highchart() %>% 
    hc_chart(polar = TRUE, type = "line") %>% 
    hc_xAxis(
      categories = df$name,
      tickmarkPlacement = "on",
      lineWidth = 0
    ) %>% 
    hc_yAxis(
      gridLineInterpolation = "polygon",
      lineWidth = 0,
      min = 0,
      max = 100
    ) %>% 
    hc_add_series(
      data           = list_parse(df),
      name           = "Skill level",
      type           = "area",
      pointPlacement = "on"
    ) %>%
    hc_legend(enabled = FALSE)
})
