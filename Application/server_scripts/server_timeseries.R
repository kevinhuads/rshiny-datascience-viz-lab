output$sales_table = DT::renderDataTable({
  datatable(sales_train, extensions = 'Scroller', options = list(
    deferRender = TRUE,
    scrollY = 500,
    scroller = TRUE,
    scrollX = TRUE,
    autoWidth = TRUE
  ), 
  rownames = FALSE)
})

sales_db = reactive({
  x1 = input$sales_ts_store
  x2 = input$sales_ts_item
  db = sales_train[(sales_train$store ==x1)&(sales_train$item ==x2),c('date','sales')]
})

sales_themout = reactive({
  if (input$sales_them == 'Elementary') y = hc_theme_elementary()
  if (input$sales_them == 'Dota Buff') y = hc_theme_db()
  if (input$sales_them == 'Economist') y = hc_theme_economist()
  if (input$sales_them == 'FiveThirtyEight') y = hc_theme_538()
  if (input$sales_them == 'DarkUnica') y = hc_theme_darkunica()
  if (input$sales_them == 'Google') y = hc_theme_google()
  return(y)
})

sales_text_reac = reactive({
  if(input$sales_metric == 'MAE') text = 'Mean Absolute Error' else text = 'Root Mean Square Error'
  return(text)
})

output$sales_timeseries = renderHighchart({
  db = sales_db()
  sales_them_const <<- sales_themout()
  highchart(type = "stock") %>% 
    hc_add_series(xts::xts(db$sales, order.by=as.Date(db$date)), id = "sales",name = 'Sales')%>% 
    hc_chart(polar = FALSE)  %>% 
    hc_add_theme(sales_them_const)
})

sales_dbtrain = reactive({
  db = sales_db()
  db = db[db$date<"2017-01-01",]
})

sales_prophet_reac = reactive({
  ind_item = which((sales_itemgrid$item_list == input$sales_ts_item)&
                     (sales_itemgrid$store_list == input$sales_ts_store))
  
  ind_proph = which((sales_prophgrid$yearly_list == tolower(input$sales_proph_year))&
                      (sales_prophgrid$monthly_list == tolower(input$sales_proph_month))&
                      (sales_prophgrid$weekly_list == tolower(input$sales_proph_week)))
  
  return(sales_prophet[[ind_item]][[ind_proph]])
})

output$sales_prophpred = renderHighchart({
  sales_them_const <<- sales_themout()
  pred = sales_db()
  pred$pred = sales_prophet_reac()$trend$yhat
  pred = pred[pred$date>="2017-01-01",]
  pred$resid = pred$sales - pred$pred
  
  highchart(type = "stock") %>% 
    # create axis :)
    hc_yAxis_multiples(
      create_yaxis(2, height = c(4, 1), turnopposite = TRUE)
    ) %>% 
    hc_add_series(xts::xts(pred$sales,yAxis = 0, order.by=as.Date(pred$date)), id = "sales",name = 'True Sales') %>% 
    hc_add_series(xts::xts(pred$pred ,yAxis = 0, order.by=as.Date(pred$date)), id = "sales",name = 'Predictions')%>% 
    hc_add_series(xts::xts(pred$resid ,yAxis = 0, order.by=as.Date(pred$date)), yAxis = 1, name = "Residuals")  %>% 
    hc_add_theme(sales_them_const)
})

output$sales_mean = renderValueBox({
  pred = sales_db()
  y = mean(pred$sales[pred$date<="2016-12-31"])
  pred = pred$sales[pred$date>="2017-01-01"]
  if(input$sales_metric == 'MAE') z = mean(abs(pred-y)) else z = sqrt(mean((pred-y)^2))
  
  valueBox(round(z,2),'Error for the Mean model',icon = icon('list'),color = 'blue')
})

output$sales_naive = renderValueBox({
  pred = sales_db()
  y = pred$sales[pred$date=="2016-12-31"]
  pred = pred$sales[pred$date>="2017-01-01"]
  if(input$sales_metric == 'MAE') z = mean(abs(pred-y)) else z = sqrt(mean((pred-y)^2))
  
  valueBox(round(z,2),'Error for the Naive model',icon = icon('list'),color = 'black')
})

output$sales_prophtrend = renderHighchart({
  sales_them_const <<- sales_themout()
  trend = data.frame(ds = sales_db()$date,trend = sales_prophet_reac()$trend$trend)
  
  highchart() %>% 
    hc_title(text = "Trend") %>% 
    hc_xAxis(categories = as.Date(trend$ds),tickInterval = 365) %>% 
    hc_add_series(trend$trend,showInLegend = FALSE,name = 'Trend')  %>%
    hc_tooltip(crosshairs = TRUE, borderWidth = 2, sort = TRUE, table = TRUE)  %>% 
    hc_add_theme(sales_them_const)
})

output$sales_prophyearui = renderUI({
  if (input$sales_proph_year != 'None') highchartOutput('sales_prophyear', height = "250px")
})

output$sales_prophmonthui = renderUI({
  if (input$sales_proph_month != 'None') highchartOutput('sales_prophmonth', height = "250px")
})

output$sales_prophweekui = renderUI({
  if (input$sales_proph_week != 'None') highchartOutput('sales_prophweek', height = "250px")
})

output$sales_prophyear = renderHighchart({
  sales_them_const <<- sales_themout()
  year = sales_prophet_reac()$year
  year$month = month.abb[month(as.Date("2014-12-31") +year$day)]
  
  if (input$sales_proph_year == 'Multiplicative'){
    highchart() %>% 
      hc_title(text = "Yearly Seasonality") %>% 
      hc_xAxis(categories = year$month,tickInterval = 32) %>% 
      hc_yAxis(labels = list(format = "{value}%")) %>% 
      hc_add_series(100*year$value,showInLegend = FALSE) %>% 
      hc_add_theme(sales_them_const)
  } else {
    highchart() %>% 
      hc_title(text = "Yearly Seasonality")  %>% 
      hc_xAxis(categories = year$month,tickInterval = 32) %>% 
      hc_add_series(year$value,showInLegend = FALSE) %>% 
      hc_add_theme(sales_them_const)
  }
})

output$sales_prophmonth = renderHighchart({
  sales_them_const <<- sales_themout()
  month = sales_prophet_reac()$month
  
  if (input$sales_proph_month == 'Multiplicative'){
    highchart() %>% 
      hc_title(text = "Monthly Seasonality")  %>% 
      hc_xAxis(categories = month$day) %>% 
      hc_yAxis(labels = list(format = "{value}%")) %>% 
      hc_add_series(100*month$value,showInLegend = FALSE) %>% 
      hc_add_theme(sales_them_const)
  } else {
    highchart() %>% 
      hc_title(text = "Monthly Seasonality")  %>% 
      hc_xAxis(categories = month$day) %>% 
      hc_add_series(month$value,showInLegend = FALSE) %>% 
      hc_add_theme(sales_them_const)
  }
})

output$sales_prophweek = renderHighchart({
  sales_them_const <<- sales_themout()
  week = sales_prophet_reac()$week
  if (input$sales_proph_week == 'Multiplicative'){
    highchart()  %>% 
      hc_title(text = "Weekly Seasonality") %>% 
      hc_xAxis(categories = week$day) %>% 
      hc_yAxis(labels = list(format = "{value}%")) %>% 
      hc_add_series(100*week$value,showInLegend = FALSE)  %>% 
      hc_add_theme(sales_them_const)
  } else {
    highchart()  %>% 
      hc_title(text = "Weekly Seasonality") %>% 
      hc_xAxis(categories = week$day) %>% 
      hc_add_series(week$value,showInLegend = FALSE)  %>% 
      hc_add_theme(sales_them_const)
  }
})


output$sales_prophformula <- renderUI({ 
  y = ''
  
  if (any(c(input$sales_proph_week=='Multiplicative',
            input$sales_proph_month=='Multiplicative',
            input$sales_proph_year=='Multiplicative'))){
    for(i in c('Week','Month','Year')){
      x = tolower(i)
      if (input[[paste0('sales_proph_',x)]]=='Multiplicative') y = paste0(y,'*(1+t_{mut.',i,'})')
    }
    
    #y = paste0('*(1 ',y,')')
  }
  
  for(i in c('Week','Month','Year')){
    x = tolower(i)
    if (input[[paste0('sales_proph_',x)]]=='Additive') y = paste0(y,'+t_{add.',i,'}')
  }
  
  text = paste0("$$ \\hat{\\mathbf{y}} = trend",y,"$$")
  withMathJax(text)
})

output$sales_prophresults <- renderValueBox({
  pred = sales_db()
  pred$pred = sales_prophet_reac()$trend$yhat
  pred = pred[pred$date>="2017-01-01",]
  
  if(input$sales_metric == 'MAE') z = mean(abs(pred$sales - pred$pred)) else z = sqrt(mean((pred$sales - pred$pred)^2))
  valueBox(
    round(z,2), 
    sales_text_reac(), 
    icon = icon("search",lib = 'glyphicon'),
    color = "light-blue",
    width = 12
  )
})

output$sales_proph_col_ui = renderUI({
  vec = c('Weekly','Monthly','Yearly')
  vec = vec[!vec%in%input$sales_proph_fix]
  selectInput('sales_proph_col','Color By',choices = vec,selected = vec[1])
})

sales_dt_reac = reactive({
  vec = c('Yearly','Monthly','Weekly')
  sales_x <- vec[!vec%in%c(input$sales_proph_fix,input$sales_proph_col)]
  sales_col <- input$sales_proph_col
  sales_fix <- input$sales_proph_fix
  sales_to <- input$sales_proph_to
  
  x1 <- input$sales_ts_store
  x2 <- input$sales_ts_item
  k <- which(sales_itemgrid$store_list%in%x1&sales_itemgrid$item_list%in%x2)
  dt <- cbind(sales_prophgrid,round(sales_prophet_err[[input$sales_metric]][[k]],2))
  colnames(dt) <- c('Yearly','Monthly','Weekly','Error')
  dt <- dt[dt[,sales_fix]==tolower(sales_to),]
  for (i in 1:3) levels(dt[,i]) <- paste0(colnames(dt)[i],' : ',levels(dt[,i]))
  dt <- dt[,c(sales_x,sales_col,sales_fix,'Error')]
  return(dt)
})

output$sales_proph_dt = renderHighchart({
  dt_reac <<- sales_dt_reac()
  sales_them_const <<- sales_themout()
  shiny::validate(
    need(input$sales_proph_col,message = FALSE),
    need(input$sales_proph_col!=input$sales_proph_fix,message = FALSE)
  )
  
  dt_reac %>% hchart('column', hcaes(x = dt_reac[,1], y = 'Error', group = dt_reac[,2])) %>% 
    hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(title = list(text = sales_text_reac())) %>% 
    hc_add_theme(sales_them_const)
})


# output$sales_arimadiff = renderHighchart({
#   db = sales_db()
#   sales_them_const <<- sales_themout()
#   
#   db_diff = data.frame(date = db$date[-1],sales = diff(db$sales))
#   highchart(type = "stock") %>% 
#     hc_add_series(xts::xts(db_diff$sales, order.by=as.Date(db_diff$date)), 
#                   id = "sales",name = 'Sales')%>% 
#     hc_chart(polar = FALSE)  %>% 
#     hc_add_theme(sales_them_const)
# })