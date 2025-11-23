# Probability and statistics section - server outputs and reactives

# Plot of posterior probability of having chosen the biased coin
output$coins_throws = renderPlotly({
  p = as.numeric(input$coins_p)
  m = 1/as.numeric(input$coins_m)
  k = as.numeric(input$coins_k)
  X = 0:k
  Y = 1 / (1 + ((1-m)/m)*(0.5/p)^X)
  
  dtick_ = 2
  size_ = 10
  width_ = 2
  if (k > 20){
    dtick_ = 5
    size_ = 8
    width_ = 1
  } 
  if (k > 40){
    dtick_ = 5
    size_ = 6
    width_ = 1
  } 
  if (k > 60){
    size_ = 4
    width_ = 0
  } 
  if (k > 80){
    dtick_ = 10
  } 
  if (k > 160){
    dtick_ = 20
  } 
  tickformat_ = '.0%'
  hoverformat_ = '.2%'
  if (p != 0.5){
    if (max(Y) < 0.1) {
      n = abs(round(log10(max(Y))))
      tickformat_ = paste0('.',n,'%')
      hoverformat_ = paste0('.',n+2,'%')
    }
  }
  plot_ly(x = X, y = Y, type = 'scatter',mode = 'lines+markers',
          line = list(color = '#aaaaaa'),
          marker = list(size = size_,color = rep(c('#1e72b2')),
                        line = list(color = rep(c('rgba(0, 0, 152, .8)')),width = width_))) %>%
    layout(
      title = "",
      yaxis = list(title = "Probability of having chosen the biased coin", 
                   tickformat = tickformat_ ,hoverformat = hoverformat_  ),
      xaxis = list(title = "Number of heads in a row",dtick = dtick_))
})

# One shot simulation of coin tosses and counts of heads and tails
coins_randVals_1 <- eventReactive(input$coins_simul_1, {
  rand_ = rbinom(n = as.numeric(input$coins_simul_n_1) ,size = 1,prob = as.numeric(input$coins_simul_p_1))
  if (all(rand_ == 0)){
    tails_ = as.numeric(input$coins_simul_n_1)
    heads_ = 0
  } else {
    if (all(rand_ == 1)){
      tails_ = 0
      heads_ = as.numeric(input$coins_simul_n_1)
    } else {
      tab_ = table(rand_)
      tails_ = tab_[1]
      heads_ = tab_[2]
    }
  }
  rand_ = list('head' = heads_,'tail' = tails_)
},ignoreNULL = FALSE)

# UI box summarising single simulation results (heads, tails, proportion)
output$coins_simul_box_1 = renderUI({
  rand_ = coins_randVals_1()
  tails_ = rand_$tail
  heads_ = rand_$head
  
  boxPad(
    color = "blue",
    descriptionBlock(header = heads_ , text = "Heads"),
    descriptionBlock(header = tails_ , text = "Tails"),
    descriptionBlock(header = paste(100*heads_/as.numeric(input$coins_simul_n_1),"%"),text = "Success")
  )
})

# Event based simulation of coin tosses for repeated sampling
coins_randVals <- eventReactive(input$coins_simul, {
  rand_ = rbinom(n = as.numeric(input$coins_simul_n) ,size = 1,prob = as.numeric(input$coins_simul_p))
  if (all(rand_ == 0)){
    tails_ = as.numeric(input$coins_simul_n)
    heads_ = 0
  } else {
    if (all(rand_ == 1)){
      tails_ = 0
      heads_ = as.numeric(input$coins_simul_n)
    } else {
      tab_ = table(rand_)
      tails_ = tab_[1]
      heads_ = tab_[2]
    }
  }
  rand_ = list('head' = heads_,'tail' = tails_)
},ignoreNULL = FALSE)

# Reactive sample of average number of heads over N experiments
coins_samp = reactive({
  rand_ = coins_randVals()
  N = as.numeric(input$coins_simul_N)
  n = as.numeric(input$coins_simul_n)
  p = as.numeric(input$coins_simul_p)
  
  withProgress(message = 'Throwing...', value = 0, {
    sapply(1:N , function(i){
      y = rbinom(n = n ,size = 1,prob = p)
      if (i%%(N/100) == 0) incProgress(0.01, detail = paste(i*100/N,"%"))
      ytab = table(y)[2]/n
    })
  })
})

# Shapiro-Wilk test box on the sampling distribution of the average
output$coins_simul_box = renderUI({
  test_ = shapiro.test(coins_samp()[1:min(length(coins_samp()),5000)])
  if (test_$p.value < 0.05){
    col_ = "red" 
    text_ = "Rejected"
  } else {
    col_ = "blue"
    text_ = "Can not be rejected"
  }
  
  boxPad(
    color = col_,
    descriptionBlock(header = round(test_$statistic,4) , text = "Statistic W", right_border = FALSE,margin_bottom = TRUE),
    descriptionBlock(header = format.pval(test_$p.value, digits = max(1, getOption("digits") - 2),
                                          eps = .Machine$double.eps, na.form = "NA") , text = "p-Value", 
                     right_border = FALSE,margin_bottom = TRUE),
    descriptionBlock(header = "Null Hypothesis",text = text_, right_border = FALSE,margin_bottom = TRUE)
  )
})

# Histogram or QQ plot of the sampling distribution of the average of heads
output$coins_hist = renderHighchart({
  samp_tab = coins_samp()
  if (input$coins_simul_plot == 'bar'){
    hchart(samp_tab, color = '#1e72b2')%>% 
      hc_title(text = "Distribution of the average of heads obtained",
               margin = 20,style = list(useHTML = TRUE))%>% 
      hc_legend(enabled = F) 
  } else {
    qq_data = as.data.frame(bind_rows(qqnorm(samp_tab,plot.it = FALSE)))
    highchart() %>% 
      hc_add_series(qq_data,'scatter', color = '#1e72b2') %>% 
      hc_title(text = "Distribution of the average of heads obtained",
               margin = 20,style = list(useHTML = TRUE))%>% 
      hc_legend(enabled = F) 
  }
})

# Event based simulation of coin tosses for tail test example
coins_randVals_2 <- eventReactive(c(input$coins_tail_simul, input$coins_tail_n) , {
  rand_ = rbinom(n = as.numeric(input$coins_tail_n) ,size = 1,prob = as.numeric(input$coins_tail_p))
  if (all(rand_ == 0)){
    tails_ = as.numeric(input$coins_tail_n)
    heads_ = 0
  } else {
    if (all(rand_ == 1)){
      tails_ = 0
      heads_ = as.numeric(input$coins_tail_n)
    } else {
      tab_ = table(rand_)
      tails_ = tab_[1]
      heads_ = tab_[2]
    }
  }
  rand_ = list('head' = heads_,'tail' = tails_)
},ignoreNULL = FALSE)

# Binomial distribution and p-value visualisation for one or two tailed tests
output$coins_tail = renderHighchart({
  N = as.numeric(input$coins_tail_n)
  x = 0:N
  y = choose(N,x)*(0.5^N)
  df = data.frame(x,y)
  
  heads_ = unname(coins_randVals_2()$head)
  probs = sum(y[heads_:length(y)])
  
  m2 = min(max(which(y>0.001),unname(coins_randVals_2()$head)) + 5,nrow(df))
  m1 = N+1 - m2
  df = df[m1:m2,]
  p_val = as.numeric(input$coins_tail_threshold)
  
  
  col_ = 'rgba(192,192,192,0.35)'
  if (input$coins_tail_test == 'One tail'){
    plotband = list(from = N - min(which(cumsum(y)>p_val)),to = N,color = col_)
  } else {
    y2 = 2*y[1:(length(y)/2)]
    plotband = list(
      list(from = 0, to = min(which(cumsum(y2)>p_val)), color = col_),
      list(from = N - min(which(cumsum(y2)>p_val)),to = N,color = col_)
    )
  }
  
  hchart(df, color = "rgba(55, 128, 191, 0.7)", "column", hcaes(x = x, y = y))%>% 
    hc_title(text = "",margin = 20,style = list(useHTML = TRUE))%>% 
    hc_legend(enabled = F) %>%
    hc_tooltip(formatter = JS("function(){return  '<b>' + this.x + ' heads </b><br/> Probability: '
                              +Highcharts.numberFormat(100*this.y)+'%'}"),
               useHTML = FALSE) %>%
    hc_yAxis(title = NULL,
             labels = list(formatter =  JS("function () {return Highcharts.numberFormat(100*this.value,0) + '%';}")))%>%
    hc_xAxis(title = NULL,plotLines = list(
      list(value = heads_, 
           color = '#ff5555',width = 2.5,zIndex = 1,
           label = list(text = paste(heads_,"Heads Obtained /",
                                     "p-value: ",format.pval(probs, digits = max(1, getOption("digits") - 2),
                                                             eps = .Machine$double.eps, na.form = "NA")),
                        style = list()))),
      plotBands = plotband
    )
})

# Trade off plot between p-value threshold and false negatives for different sample sizes
output$coins_prob = renderHighchart({
  hchart(as_tibble(coins_prob[[paste0('N',input$coins_prob_n)]]), "line", hcaes(x = 100*prob, y = 100*negative, group = p_value),
         color = brewer.pal(n = 7, name = "Spectral")) %>%
    hc_yAxis(title = "Flagged Negatives",labels = list(formatter =  JS("function () {return this.value + '%';}")))%>%
    hc_xAxis(title = "Probability",labels = list(formatter =  JS("function () {return this.value + '%';}")))  %>%
    hc_tooltip(useHTML= TRUE,table= TRUE,sort= TRUE)
})
