output$spam = DT::renderDataTable({
  options(DT.options = list(
    pageLength = 5,
    ordering=F,
    dom = 'ltpi'
  ))
  datatable(spam_df, rownames = FALSE)
})

output$spam_pie = renderPlotly({
  plot_ly(spam_table, labels = ~Var1, values = ~Freq, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent+value',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'label+percent+value',
          marker = list(colors = c('rgb(211,94,96)', 'rgb(128,133,133)'),
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE) %>%
    layout(title = '',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           font = list(size = 16,type = 'bold'))
})

output$spam_wcham = renderPlot({
  wordcloud(spam_corpus$ham, scale = c(2,0.5) ,max.words = 400, random.order = FALSE,colors =brewer.pal(8,'Set1'))
})

output$spam_compcl = renderPlot({
  comparison.cloud(spam_conc,max.words=200,scale=c(6,.2),random.order=FALSE,title.size = 3)
})

output$spam_wcloud2 <- renderWordcloud2({
  par(mar = rep(0, 4))
  df = as.data.frame(spam_conc)
  df$word = rownames(df)
  df = df[,c('word',input$spam_choice)]
  colnames(df)[2] = 'freq'
  df = df[df$freq>0,]
  wordcloud2(df, size=input$spam_size)
})
