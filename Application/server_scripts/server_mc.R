mc_rv <- reactiveValues(i = 0,count_red = 0,
                        df = data.frame(x = -1,y = -1,color = 'blue'))
mc_err <- reactiveValues(value = 0,iter = 0)

observeEvent(input$mc_launch,{
  mc_rv$i = 0
  mc_rv$count_red = 0
  mc_rv$df = data.frame(x = runif(10000, min = -1,max = 1),
                        y = runif(10000, min = -1,max = 1)) %>% 
    mutate(color =  ifelse(x^2 + y^2>=1, 'blue','red'))
  
  mc_err$value = 0
  mc_err$iter = 0
})

observe({
  isolate({
    mc_rv$count_red = mc_rv$count_red + sum(mc_rv$df$color[mc_rv$i+1:as.numeric(input$mc_display)]%in%'red')
    mc_rv$i = mc_rv$i + as.numeric(input$mc_display)
    mc_err$value <- c(mc_err$value,4*mc_rv$count_red/mc_rv$i)
    mc_err$iter <-  c(mc_err$iter,mc_rv$i)
  })

  if ((mc_rv$i < input$mc_iter)&input$mc_launch){
    invalidateLater(100)
  }
})

output$mc_circle <- renderPlotly({
  df = data.frame(x = 0,y = -1, color ='blue')
  df = rbind(df,mc_rv$df)
  
  plot_ly(df[1:(mc_rv$i + 1),], x = ~x, y = ~y,color = ~color,
          colors = c(rgb(0, 0, 152/255),rgb(212/255, 60/255, 60/255)),
          type = 'scatter', mode = 'markers', opacity = 0.7,
          marker = list(size = 4), hoverinfo="none") %>%
    layout(shapes = list(
      list(type = "rect",
           line = list(color = "black"), opacity = 0.3,
           x0 = -1, x1 = 1, xref = "x",
           y0 = -1, y1 = 1, yref = "y"),
      list(type = 'circle',
           xref = 'x', x0 = -1, x1 = 1,
           yref = 'y', y0 = -1, y1 = 1,
           line = list(color = 'rgba(212, 60, 60, .8)'),
           opacity = 1)),showlegend = FALSE,
      xaxis = list(title ='',zeroline = FALSE, range = c(-1,1)),
      yaxis = list(title ='',zeroline = FALSE, range = c(-1,1)))%>%
    config(displayModeBar = FALSE)
})
  

output$mc_pi_iterbox = renderValueBox({
  valueBox(mc_rv$i, 'Iterations', icon = icon('list'),color = 'blue')
})

output$mc_pi_redbox = renderValueBox({
  valueBox(mc_rv$count_red, 'Red Points', icon = icon('list'),color = 'red')
})

output$mc_pi_valuebox = renderValueBox({
  if (input$mc_choice =='Estimated Value'){
    valueBox(round(4*mc_rv$count_red/mc_rv$i,4),'Estimated Value',  icon = icon('list'),color = 'black')
  } else {
    valueBox(formatC(abs(pi - 4*mc_rv$count_red/mc_rv$i), format = "e", digits = 2),
             'Error',  icon = icon('list'),color = 'black')
  }
})

output$mc_pi_value <- renderPlotly({
  df = data.frame(Iteration = mc_err$iter,Value = mc_err$value)
  range_dinam = c(2.3,4)
  
  if (input$mc_choice =='Error'){
    df$Value = abs(pi - df$Value)
    range_dinam = c(0,0.8)
  } 
  
  plot_ly(df, x = ~Iteration, y = ~Value,type = 'scatter', mode = 'lines+markers',
          marker = list(size = 3,color ='rgba(0, 0, 152, .8)',
                        line = list(color = 'rgba(0, 0, 152, .8)',width = 1)))%>%
    add_annotations(
      x = input$mc_iter,y = pi,
      text = 'Value of pi',xref = "x",yref = "y",
      showarrow = TRUE,arrowhead = 7,ax = 40,ay = -20,
      font = list(color = 'rgba(212, 60, 60, .8)')
    ) %>%
    layout(shapes = list(
      list(line = list(color = "rgba(212, 60, 60, .8)"),
           x0 = 0, x1 = input$mc_iter+10, xref = "x",
           y0 = pi, y1 = pi, yref = "y")),
      xaxis = list(title = 'Iterations',range = c(0,input$mc_iter+10),zeroline = TRUE),
      yaxis = list(title = 'Estimated Value',range = range_dinam,zeroline = TRUE))
})


mc_novisu <- reactiveValues(df = data.frame(x = runif(10000, min = -1,max = 1),
                                            y = runif(10000, min = -1,max = 1)) %>% 
                              mutate(color =  ifelse(x^2 + y^2>=1, 'blue','red')),
                            count_red = 7848)

observeEvent(input$mc_novisu,{
  mc_novisu$df = data.frame(x = runif(as.numeric(input$mc_novisu), min = -1,max = 1),
                            y = runif(as.numeric(input$mc_novisu), min = -1,max = 1)) %>% 
    mutate(color =  ifelse(x^2 + y^2>=1, 'blue','red'))
  mc_novisu$count_red = sum(mc_novisu$df$color == 'red')
})

observeEvent(input$mc_novisugo,{
  mc_novisu$df = data.frame(x = runif(as.numeric(input$mc_novisu), min = -1,max = 1),
                            y = runif(as.numeric(input$mc_novisu), min = -1,max = 1)) %>% 
    mutate(color =  ifelse(x^2 + y^2>=1, 'blue','red'))
  mc_novisu$count_red = sum(mc_novisu$df$color == 'red')
})


output$mc_simul_box = renderUI({
  iter = as.numeric(input$mc_novisu)
  reds = mc_novisu$count_red
  estim_ = 4*reds/iter
  column(12,style='padding:0px;',
    column(6,offset = 0, style='padding:0px;',
           boxPad(
             color = "black",
             descriptionBlock(header = iter , text = "Iterations"),
             descriptionBlock(header = reds , text = "Red Points")
           )),
    column(6,offset = 0, style='padding:0px;',
           boxPad(
             color = "blue",
             descriptionBlock(header = round(estim_,6), text = "Estimated Value"),
             descriptionBlock(header = formatC(abs(pi-estim_), format = "e", digits = 2), text = "Error")
           )))
})

output$mc_complexity = renderHighchart({
  pi_mean = data.frame(
    log_it = seq(1,7,0.25),
    log_error = log10(sapply(pi_rand,function(x) mean(abs(x))))) %>%
    mutate(it = 10^log_it, error = 10^log_error)
  
  highchart() %>% 
    hc_title(text = "Evolution of the error given the number of iterations") %>% 
    hc_subtitle(text = "log10(err) ~ log10(n)")%>%
    hc_xAxis(title = list(text = "Number of Iterations (log)")) %>%
    hc_yAxis(title = list(text = "Error (log)")) %>%
    hc_add_series(pi_mean, "scatter", hcaes(log_it, log_error),showInLegend = FALSE)%>%
    hc_add_theme(hc_theme_elementary()) %>%
    hc_tooltip(formatter = JS("function () {
      return 'Iterations (log): <b>' +this.point.log_it + '</b><br>' +
      'Error (log): <b>' +  Highcharts.numberFormat(Math.abs(this.point.log_error), 4) + '</b><br>' +
      'Iterations: <b>' +Highcharts.numberFormat(Math.abs(this.point.it), 0)  + '</b><br>' +
      'Error: <b>' +  Highcharts.numberFormat(Math.abs(this.point.error), 4) + '</b>';}"))
})