output$bikes_table = DT::renderDataTable({
  datatable(bikes, extensions = 'Scroller', options = list(
    deferRender = TRUE,
    scrollY = 500,
    scroller = TRUE,
    scrollX = TRUE,
    autoWidth = TRUE
  ), 
  rownames = FALSE)
})

output$bikes_dist = renderHighchart({
  y = bikes$count
  if (input$bikes_histdist == 'Density') y= density(y)
  hchart(y, color = "rgba(55, 128, 191, 0.7)")%>% 
    hc_title(text = "Distribution of the count of rents",
             margin = 20,style = list(useHTML = TRUE))%>% 
    hc_legend(enabled = F)
})


output$bikes_hour = renderHighchart({
  bikes_hour = as.data.frame(aggregate(bikes_ml$count,
                                       list(bikes_ml$hour),
                                       mean)) 
  colnames(bikes_hour) = c('Hour','Count') #On renomme les colonnes
  highchart() %>% 
    hc_xAxis(categories = bikes_hour$Hour) %>% 
    hc_add_series(name = "Count", data = round(bikes_hour$Count,2))  %>% 
    hc_colors('rgba(219, 64, 82, 0.7)') %>% 
    hc_chart(polar = input$bikes_polar) %>% 
    hc_title(text = "Average count per hour",
             margin = 20,style = list(useHTML = TRUE)) %>% 
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5) %>% 
    hc_exporting(enabled = F)%>% 
    hc_legend(enabled = F) # enable exporting option
})



output$selectbikesUI = renderUI({
  if (input$bikes_rad == "quali"){
    choices_ = bikes_quali
    names(choices_) = c("Season","Holiday","Working Day","Weather")
  } else {
    choices_ = bikes_quanti
    names(choices_) = c("Temperature","Temperature Felt Like","Humidity","Wind Speed")
  }
  
  selectInput(inputId = 'selectbikes', label = 'Feature to study', 
              choices = choices_ ,selected = 1)
})

# hcboxplot(var = bikes$season, x = bikes$count,name = "Length") %>% 
#   hc_chart(type = "column")  %>% 
#   hc_colors(c("#203d7d","#a0a0ed","#203d7e","#a0a0ad")) 

output$bikes_chosen = renderPlotly({
  shiny::validate(need(input$selectbikes,message = 'Loading...'))
  
  if (input$selectbikes %in% bikes_quali) {
    bikes_temp = bikes
    bikes_temp[,input$selectbikes] = as.factor(bikes_temp[,input$selectbikes])
    if (input$selectbikes %in% 'season') 
      levels(bikes_temp[,input$selectbikes]) = c('Spring','Summer','Fall','Winter')  
    if (input$selectbikes %in% 'weather'){
      bikes_temp = bikes_temp[bikes_temp[,input$selectbikes]!=4,]
      bikes_temp[,input$selectbikes] = droplevels(bikes_temp[,input$selectbikes])
      levels(bikes_temp[,input$selectbikes]) = c('Clear or Few Clouds','Mist and Cloudy','Light Snow or Light Rain') 
    }  
    
    # custom_colors_with_alpha_in_hex<- c("#87CEFF80","#EE3B3B80","#B0306080","#79CDCD80")
    # make_highchart_boxplot_with_colored_factors(value = bikes_temp$count, by=bikes_temp$season, chart_title = "",
    #                                             chart_x_axis_label = "",show_outliers = FALSE, 
    #                                             boxcolors = custom_colors_with_alpha_in_hex)
    plot_ly(bikes_temp, y = ~count, color = ~bikes_temp[,input$selectbikes], type = "box",showlegend = FALSE)  %>%
      layout(title = "",xaxis = list(title = ""))
    
  } else {
    sel = which(bikes_quanti == input$selectbikes)
    n = rep(c(2,5),each = 2)[sel]
    span = rep(c(1/3,2/3),each = 2)[sel]
    
    x = n*round(bikes[,input$selectbikes]/n)
    
    bikes_agg = as.data.frame(aggregate(bikes$count,list(x),mean)) 
    bikes_agg$weight = as.numeric(table(x))
    colnames(bikes_agg) = c(input$selectbikes,'Count','Weight') 
    
    bikes_ay = list(title = 'Count' , overlaying = "y", side ='right',
                    range = c(0, max(bikes_agg$Count)*1.05),zeroline=TRUE,showline=FALSE,showgrid = FALSE) 
    plot_ly(bikes_agg, x = ~bikes_agg[,input$selectbikes], 
            y = ~Weight, type = 'bar', name = 'Number of observations')  %>%
      add_lines(y = ~Count, yaxis = "y2", color = I('grey'),mode = "lines+markers",name = 'Observed Count',  
                line = list(dash = 'dot'), marker = list(size = 8,width =3)) %>%
      add_lines(y = ~fitted(loess(Count ~ as.numeric(bikes_agg[,input$selectbikes]),
                                  span = span,weights = Weight)), yaxis = "y3", mode = 'marker',
                line = list(color = 'rgba(0, 0, 0, 1)', width = 3),
                name = "Smoothed Curve of the Count")%>%
      layout(xaxis = list(title = input$selectbikes),
             yaxis = list(title = 'Number of observations'),
             yaxis2 = bikes_ay,
             yaxis3 = bikes_ay,
             margin = list(l = 50,r = 100,b = 80,t = 50,pad = 4),
             legend=list(orientation="h",y=1.1,x=0.25))
  }
})

output$bikescor <- renderHighchart({
  colnames(bikes_corr$pearson)  = c("Hour","Temperature","Felt Like","Humidity","Wind Speed")
  rownames(bikes_corr$pearson)  = colnames(bikes_corr$pearson) 
  #corrplot.mixed(bikes_corr$pearson,lower.col = 'black', upper = 'square',tl.cex = 0.9)
  hchart.cor(bikes_corr$pearson)
})

bikes_tree_reac <- reactive({
  bikes_temp = bikes_train
  for (i in bikes_quali) bikes_temp[,i] = as.factor(bikes_temp[,i])
  var = unlist(input$bikes_treefeatures)
  tree1 <-rpart(bikes_temp[,'count']~.,data=bikes_temp[,var],cp=-1,
                maxdepth=as.numeric(input$bikes_treeslide))
})

output$bikestree <- renderPlot({
  shiny::validate(
    need(length(input$bikes_treefeatures)>1, message = "Choose at least 2 features")
  )
  heat.tree(bikes_tree_reac(),type=4,extra=101,varlen=0,faclen=0,fallen.leaves=TRUE,roundint=FALSE)
})

output$bikes_tree_val <- renderText({
  if (length(input$bikes_treefeatures)>1){
    paste("The current tree has an error reduction ratio of", 
          percent(1-er_gamma(pred = predict(bikes_tree_reac(), newdata = bikes_val), obs = bikes_val$count)),
          "on the validation set.")
  }
  
})

output$bikes_glm_summary <- renderPrint({
  reg = bikes_lm[[input$bikes_lmchoice]]
  summary(reg)
})

output$bikes_lm_results <- err_box(bikes_lm_dev[input$bikes_lmchoice])
output$bikes_knn_results <- err_box(max(bikes_err$knn$dev))
output$bikes_tree_results <- err_box(max(bikes_err$tree$dev))
output$bikes_gbm_results <- err_box(max(bikes_err$gbm$dev))
output$bikes_rf_results <- err_box(max(bikes_err$rf$dev))
output$bikes_svm_results  <- err_box(max(sapply(c('nu','eps'),function(y) {
  sapply(bikes_err$svm[[y]], function(x) {
    x[which.max(x$dev),'dev']
  })
})))

output$bikes_tree <- renderPlotly({
  plot_ly(data = as.data.frame(bikes_err$tree), x = ~depth , y = ~dev, 
          type = 'scatter',mode = 'lines + markers',marker = list(size = 10,line = list(width = 2))) %>%
    add_annotations(~percent(dev)) %>%
    layout(yaxis = list(title = 'Error Reduction Ratio',tickformat= ',.0%',hoverformat = '.2%'),
           xaxis = list(title = 'Depth'))
})

output$bikes_knn <- renderPlotly({
  plot_ly(data = as.data.frame(bikes_err$knn)[bikes_err$knn$k<16,], x = ~k , y = ~dev, 
          type = 'scatter',color = ~algorithm, mode = 'line') %>%
    layout(yaxis = list(title = 'Error Reduction Ratio',tickformat= ',.0%',hoverformat = '.2%'),
           xaxis = list(dtick = 1))
})

bikes_svm_df <- reactive({
  bikes_err$svm[[input$bikes_svm_type]][[input$bikes_svm_kernel]]
})

output$bikes_svm_ui <- renderUI({
  y = colnames(bikes_svm_df())
  y = y[-length(y)]
  selectInput("bikes_err_svm",label = 'Parameter',choices = y)
})

output$bikes_svm <- renderPlotly({
  choice = input$bikes_err_svm 
  new_df = aggregate(bikes_svm_df()$dev,by = list(bikes_svm_df()[[choice]]),FUN = max)
  
  if (input$bikes_err_svm == 'cost'){
    new_df[,'Group.1'] = log(new_df[,'Group.1'], base = 2)
    choice = paste(choice, '(base 2 logarithm)')
  } 
  plot_ly(new_df, x = ~Group.1, y = ~x,type = 'scatter',mode = 'lines + markers',
          marker = list(size = 10,line = list(width = 2)))%>%
    add_annotations(~percent(x)) %>%
    layout(yaxis = list(title = 'Error Reduction Ratio',tickformat= ',.0%',hoverformat = '.2%'),
           xaxis = list(title = choice))
})

output$bikes_rf <- renderPlotly({
  new_df = aggregate(bikes_err$rf$dev,by = list(bikes_err$rf[[input$bikes_err_rf]]),FUN = max)
  plot_ly(new_df, x = ~Group.1, y = ~x,type = 'scatter',mode = 'lines + markers',
          marker = list(size = 10,line = list(width = 2)))%>%
    add_annotations(~percent(x)) %>%
    layout(yaxis = list(title = 'Error Reduction Ratio',tickformat= ',.0%',hoverformat = '.2%'),
           xaxis = list(title = input$bikes_err_rf))
})

output$bikes_gbm <- renderPlotly({
  new_df = aggregate(bikes_err$gbm$dev,by = list(bikes_err$gbm[[input$bikes_err_gbm]]),FUN = max)
  plot_ly(new_df, x = ~Group.1, y = ~x,type = 'scatter',mode = 'lines + markers',
          marker = list(size = 10,line = list(width = 2)))%>%
    add_annotations(~percent(x)) %>%
    layout(yaxis = list(title = 'Error Reduction Ratio',tickformat= ',.0%',hoverformat = '.2%'),
           xaxis = list(title = input$bikes_err_gbm))
})

# output$bikes_svmdf <- renderPlotly({
#   plot_ly(bikes_df_svm,y = ~ kernel, x = ~performance, color = ~type, type = 'bar',
#           colors = c(rgb(159, 255, 128, maxColorValue = 255),rgb(255, 217, 179, maxColorValue = 255)),
#           marker = list(line = list(color = 'rgb(0,0,0)',width = 1.5))) %>%
#     layout(margin = list(l = 120),
#            xaxis = list(title = '',tickformat= ',.0%',hoverformat = '.2%'),
#            yaxis = list(title = ''),
#            legend = list(orientation = 'h'))
# })

output$bikes_svmdf <- renderDataTable({
  
  brks <- seq(0.75,0.8,length.out = 8)
  clrs <- brewer.pal(9,'Blues')
  
  datatable(bikes_df_svm_wide, options = list(dom = 't')) %>% 
    formatStyle(names(bikes_df_svm_wide), backgroundColor = styleInterval(brks, clrs))%>% 
    formatPercentage(column = 1:4,digits = 3)
})




output$bikes_testset <- renderHighchart({
  df_temp = bikes_perf
  colnames(df_temp) = c("y","name")
  df_temp = df_temp[rev(1:nrow(df_temp)),]
  df_temp$color = ifelse(df_temp$y> 0.8,'rgba(219, 64, 82, 0.7)','rgba(55, 128, 191, 0.7)')
  df_temp$borderColor = ifelse(df_temp$y> 0.8,'rgba(249, 64, 82, 1.0)','rgb(8,48,107,1.0)')
  df_temp$borderWidth = 1.5
  highchart() %>%
    hc_chart(type = "bar") %>% 
    hc_xAxis(categories = df_temp$name) %>% 
    hc_yAxis(labels = list(formatter =   JS("function () {return Highcharts.numberFormat(Math.abs(100*this.value),0) + '%';}"))) %>% 
    hc_add_series(df_temp, showInLegend = FALSE) %>%
    hc_tooltip(formatter = JS("function () {
                              return '<b>' + this.point.category + '</b><br/>' +
                              'Performance: ' + Highcharts.numberFormat(Math.abs(100*this.point.y), 2) + '%';}")) %>%
    hc_plotOptions(bar = list(
      dataLabels = list(enabled = TRUE,inside = TRUE,align = "right",
                        formatter =  JS("function() {return  Highcharts.numberFormat(Math.abs(100*this.point.y), 2) + '%'; }"))))
})


output$bikes_varimp <- renderHighchart({
  df_temp = bikes_gbm_varimp
  colnames(df_temp) = c("name","y")
  df_temp$color = ifelse(df_temp$y> 0.6,'rgba(219, 64, 82, 0.7)','rgba(55, 128, 191, 0.7)')
  df_temp$borderColor = ifelse(df_temp$y> 0.6,'rgba(249, 64, 82, 1.0)','rgb(8,48,107,1.0)')
  df_temp$borderWidth = 1.5
  highchart() %>%
    hc_chart(type = "bar") %>% 
    hc_xAxis(categories = df_temp$name) %>% 
    hc_yAxis(labels = list(formatter =   JS("function () {return Highcharts.numberFormat(Math.abs(100*this.value),0) + '%';}"))) %>% 
    hc_add_series(df_temp, showInLegend = FALSE) %>%
    hc_tooltip(formatter = JS("function () {
                              return '<b>' + this.point.category + '</b><br/>' +
                              'Performance: ' + Highcharts.numberFormat(Math.abs(100*this.point.y), 2) + '%';}")) %>%
    hc_plotOptions(bar = list(
      dataLabels = list(enabled = TRUE,color = "black",
                        formatter =  JS("function() {return  Highcharts.numberFormat(Math.abs(100*this.point.y), 2) + '%'; }"))))
})

output$bikes_predict <- renderValueBox({
  vec =  bikes_test[1,]
  vec$hour[1] = input$bikes_hourinp
  vec$temp = input$bikes_tempinp
  vec$humidity = input$bikes_huminp
  vec$windspeed = input$bikes_windinp
  vec$season[1] = as.numeric(input$bikes_seasoninp)
  vec$holiday[1] = as.numeric(input$bikes_holinp)
  vec$workingday[1] = as.numeric(input$bikes_workinp)
  vec$weather[1] = as.numeric(input$bikes_weatherinp)
  
  valueBox(
    round(predict(bikes_lm$square,newdata = vec,type = 'response'),2), 
    'Estimated bikes rent', 
    icon = icon("search",lib = 'glyphicon'),
    color = "red",
    width = 12
  )
})
