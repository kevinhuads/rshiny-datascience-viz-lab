# NLP section - server outputs and reactives

# Interactive data table for hotel reviews
output$hotel_table = renderDataTable({
  datatable(hotel_df, extensions = 'Scroller', options = list(
    deferRender = TRUE,
    scrollY = 500,
    scroller = TRUE,
    scrollX = TRUE,
    autoWidth = TRUE
  ), 
  rownames = FALSE)
})

# Leaflet map of hotels with clustered markers
output$hotel_map <- renderLeaflet({
  
  leaflet(data = hotel_byhot) %>%
    addProviderTiles("Esri.WorldStreetMap", group = "World Street Map") %>%
    addProviderTiles("CartoDB.Positron",    group = "Light") %>%
    addLayersControl(
      baseGroups = c("World Street Map", "Light"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addMarkers(
      popup = ~Hotel_Address,
      lat   = ~lat,
      lng   = ~lng,
      clusterOptions = markerClusterOptions()
    )
})

# Reactive selection of sentiment time series (overall or country specific)
hotel_yVar <- reactive({
  if(is.null(input$hotel_sentcountry)) return('Overall')
  input$hotel_sentcountry
})

# Reactive data frame for sentiment time series with mean and quantile bands
hotel_ggvisdf <- reactive({
  gdf <- hotel_sent[, c('date', hotel_yVar())]
  names(gdf) <- c("x", "y")
  gdf$mean = hotel_quant[10]
  
  if (input$hotel_sentquart == '5 %') {
    gdf$q1 = hotel_quant[2]
    gdf$q3 = hotel_quant[20]
  } else {
    if (input$hotel_sentquart == '10 %') {
      gdf$q1 = hotel_quant[3]
      gdf$q3 = hotel_quant[19]
    } else {
      gdf$q1 = hotel_quant[5]
      gdf$q3 = hotel_quant[15]
    }
  }
  
  gdf
})  

# Reactive colour palette for sentiment points
hotel_pal <- reactive({colorNumeric('Spectral', hotel_sent$overall)(hotel_sent[,hotel_yVar()])})

# Tooltip content for sentiment time series points
hotel_all_values <- function(x) {
  if(as.Date(as.POSIXct(x[,1]/1000, origin="1970-01-01"))== "1970-01-01"){
    return(NULL)
  } else {
    return(paste0("Date :", as.Date(as.POSIXct(x[,1]/1000, origin="1970-01-01")), "<br />",
                  "Average Sentiment : ",round(x[,2],2)))
  }
}

# ggvis pipeline for interactive sentiment time series
hotel_ggvisdf %>%
  ggvis(~x, ~y) %>%
  set_options(width = "auto", resizable=FALSE) %>%    
  ggvis::add_axis("x", title = 'Date')  %>% ggvis::add_axis("y", title = 'Average Sentiment')  %>% 
  layer_points(size := 50,fill := ~hotel_pal(), stroke := "black",strokeWidth := 0.2) %>% 
  layer_lines(y = ~mean,opacity := 0.6, strokeDash:=0, strokeWidth := 4,stroke := "red") %>%
  layer_lines(y = ~q1,  opacity := 0.5, strokeDash:=6, strokeWidth := 3,stroke := "black") %>%
  layer_lines(y = ~q3,  opacity := 0.5, strokeDash:=6, strokeWidth := 3,stroke := "black") %>%
  #scale_numeric("y", domain = c(7.5, 9.5), nice = FALSE) %>%
  add_tooltip(hotel_all_values, "hover") %>%
  layer_smooths(span = input_slider(0.1, 1, value = 0.3,step = 0.05, label = 'smooth'),
                opacity := 0.4,se = TRUE,fill:= "dodgerblue3", strokeWidth := 5)%>% 
  bind_shiny("hotel_sentvis", "hotel_ggsent")

# Conditional UI to switch between wordcloud and frequency bar chart
output$hotel_wordbar = renderUI({
  if(input$hotel_wordbar_choose == 'Wordcloud'){
    plotOutput('hotel_wordc',height = '400px')
  } else {
    highchartOutput('hotel_wordfreq',height = '400px')
  }
})

# Wordcloud of most frequent terms in positive or negative reviews
output$hotel_wordc = renderPlot({
  if(input$hotel_negpos == 'Positive') col_hot = 'Blues' else col_hot = 'Reds'
  
  d = data.frame(sort(hotel_tdm[[input$hotel_negpos]][[input$hotel_wc_ngram]],decreasing = TRUE))
  colnames(d) = 'freq'
  d$word = rownames(d)
  d = d[d$freq>2,]
  par(mar = rep(0, 4))
  wordcloud(words = d$word, freq = d$freq, min.freq = 3, scale = (1/as.numeric(input$hotel_wc_ngram))^0.3*c(4,0.6),
            max.words=400/as.numeric(input$hotel_wc_ngram), random.order=FALSE, 
            colors=brewer.pal(9, col_hot)[4:9]) %>%
    suppressWarnings()
})

# Highcharter bar chart of word frequencies in positive or negative reviews
output$hotel_wordfreq = renderHighchart({
  hotel_negpos <- input$hotel_negpos
  hotel_wc_gram <- input$hotel_wc_ngram
  
  if(hotel_negpos == 'Positive') col_hot <- rgb(61/255,89/255,171/255,0.7) else col_hot <<- rgb(238/255,59/255,59/255,0.4)
  #if(input$hotel_negpos == 'Positive') col_hot2 = 'blue' else col_hot2 = 'red'
  
  d <- data.frame(sort(hotel_tdm[[hotel_negpos]][[hotel_wc_gram]],decreasing = TRUE))
  colnames(d) = 'Count'
  d$Word = factor(rownames(d),levels = rev(rownames(d)))
  d$Frequency = d$Count/nrow(hotel_filter)
  d = d[1:20,]
  
  number_ = 0
  if (max(d$Frequency) < 0.1) number_ = 1
  if (max(d$Frequency) < 0.01) number_ = 2
  
  hchart(d, "bar", hcaes(x = Word, label=Word,y = Frequency), color = col_hot) %>% 
    hc_xAxis(title = list(text = '')) %>% 
    hc_yAxis(title = list(text = 'Frequency'),
             labels = list(formatter =   JS(paste0("function () {return Highcharts.numberFormat(Math.abs(100*this.value),",
                                                   number_,") + '%';}"))))%>%
    hc_tooltip(formatter = JS("function () {
                              return '<b>' + this.point.label + '</b><br/>' +
                              'Frequency: ' + Highcharts.numberFormat(Math.abs(100*this.point.y), 2) + '%';
}")) 
})

# Column chart summarising average sentiment metrics by rating range
output$hotel_rangescore = renderHighchart({
  i = 1
  
  df = hotel_range[[i]][[as.character(input$hotel_rangebreak)]]
  
  vec = c('Average Positive Words' = 'Num_Pos',
          'Average Negative Words' = 'Num_Neg',
          'Number of Reviews' = 'Num_Reviews')
  
  df = df[,c('Review_Range',input$hotel_rangedisp)]
  colnames(df) = c('name','y')
  
  df = df  %>% 
    mutate(
      z = y,
      e = y/10,
      value = y,
      color = hue_pal()(nrow(df)),
      segmentColor = hue_pal()(nrow(df))
    )
  df[,c("from",'to')] = df$name %>% 
    as.character() %>% 
    strsplit(",")%>%
    sapply(function(x) {
      temp = str_extract(x,"\\d+\\.\\d+")
      ifelse(is.na(temp), str_extract(x,"\\d+"),temp) %>%
        as.numeric()
    }) %>%
    t()
  
  round_ = ifelse(input$hotel_rangedisp == "Num_Reviews",0,4)
  
  highchart() %>%
    hc_chart(type ='column') %>% 
    hc_xAxis(categories = df$name) %>% 
    hc_add_series(df, showInLegend = FALSE) %>%
    hc_tooltip(formatter = JS(paste0("function () {
                              return '<b>Notes from ' + this.point.from + ' to ' + this.point.to + ' (included)</b><br/>' +'",
                                     names(vec)[vec == input$hotel_rangedisp]," : ' + Highcharts.numberFormat(Math.abs(this.point.y), ",round_,");
}")))
  
  
})

# Word co-occurrence graph for chosen pair statistics and layout
output$hotel_graph <- renderHighchart({
  df <- hotel_pairs[[input$hotel_graphchoice]][1:input$hotel_graphlength, ]
  
  vert <- df %>%
    tidyr::gather(item, word, word1, word2) %>%
    dplyr::group_by(word) %>%
    dplyr::summarise(n = sum(n), .groups = "drop")
  
  g <- graph_from_data_frame(
    df[, c("word1", "word2", "n")],
    vertices = vert,
    directed = TRUE
  )
  
  wc <- cluster_walktrap(g, steps = 10)
  
  igraph::V(g)$label <- sort(unique(c(df$word1, df$word2)))
  igraph::V(g)$name  <- sort(unique(c(df$word1, df$word2)))
  igraph::V(g)$size  <- igraph::degree(g)
  igraph::V(g)$color <- colorize(membership(wc))
  
  layout_fun <- hotel_layouts[[input$hotel_graphlayout]]
  
  hchart(g, layout = layout_fun) %>%
    hc_boost(enabled = FALSE)
})

# UI controls for LDA word display (table, wordcloud, or graph)
output$hotel_ldawordshow = renderUI({
  if(input$hotel_ldadisp == 'Table'){
    sliderInput('hotel_ldashow',label = 'Number of Words',min = 15, max = 35, step =5, value = 15)
  } else {
    if (input$hotel_ldadisp == 'Graph'){
      column(12, style='padding:0px;',
             selectInput('hotel_graphlayout_lda',label = 'Layout', selected = 'fr',choices = hotel_layouts_labels),
             radioButtons("hotel_graphlength_lda",label = "Number of words to show",c(250,500,1000),selected = 250,inline = TRUE)
      )
    }
  }
})

# LDA topic graph based on word co-occurrences and topic assignments
output$hotel_ldagraph = renderHighchart({
  df = hotel_pairs[[input$hotel_ldachoice]]
  
  k = input$hotel_ldak - 1
  pairs_temp = sapply(1:(k+1),function(i){
    df_temp = hotel_lda[[input$hotel_ldachoice]][[k]]
    terms_lda = df_temp[df_temp$topic == i,]
    temp = as.numeric(df$word1%in%terms_lda$term & 
                        df$word2%in%terms_lda$term)
    
    temp[temp == 1] = terms_lda$beta[match(df$word1[temp == 1],terms_lda$term)] +
      terms_lda$beta[match(df$word2[temp == 1],terms_lda$term)]
    return(temp)
  })
  df$Topic = as.factor(apply(pairs_temp,1,which.max))
  df = df[apply(pairs_temp,1,sum)>0,]
  df = df[1:input$hotel_graphlength_lda,]
  
  vert = df %>% gather(item, word, word1, word2) %>%
    group_by(word) %>% dplyr::summarise(n = sum(n))
  
  vert_temp = sapply(1:(k+1),function(i){
    df_temp = hotel_lda[[input$hotel_ldachoice]][[k]]
    terms_lda = df_temp[df_temp$topic == i,]
    temp = as.numeric(vert$word%in%terms_lda$term)
    temp[temp == 1] = terms_lda$beta[match(vert$word[temp == 1],terms_lda$term)] 
    return(temp)
  })
  vert$Topic = as.factor(apply(vert_temp,1,which.max))
  vert$Topic[apply(vert_temp,1,sum) == 0] = NA
  
  y = df %>% graph_from_data_frame(vertices = vert,directed = TRUE) 
  col_temp = vert$Topic 
  levels(col_temp) = hue_pal()(input$hotel_ldak)
  
  igraph::V(y)$label <- sort(unique(c(df$word1,df$word2)))
  igraph::V(y)$name <-  sort(unique(c(df$word1,df$word2)))
  igraph::V(y)$size <- igraph::degree(y)
  igraph::V(y)$color <- as.character(col_temp)
  
  hchart(y, layout = hotel_layouts[[input$hotel_graphlayout_lda]])%>% hc_boost(enabled = FALSE)
})

# Conditional UI for LDA visualisation output type
output$hotel_ldachart = renderUI({
  if(input$hotel_ldadisp == 'Table'){
    plotOutput('hotel_ldawords',height = '500px')
  } else {
    if(input$hotel_ldadisp == 'Wordcloud'){
      plotOutput('hotel_ldacloud',height= '500px')
    } else {
      highchartOutput('hotel_ldagraph',height= '700px')
    }
    
  }
})

# Plot of top LDA terms per topic using label repulsion
output$hotel_ldawords = renderPlot({
  s = 100/input$hotel_ldashow
  if (input$hotel_ldashow==5)  s = 8
  if (input$hotel_ldashow==10) s = 8
  hotel_lda[[input$hotel_ldachoice]][[input$hotel_ldak-1]]%>%
    dplyr::slice(seq_len(input$hotel_ldashow)) %>%
    arrange(topic, beta) %>%
    dplyr::mutate(row = row_number()) %>%
    ungroup() %>%
    mutate(topic = paste("Topic", topic, sep = " ")) %>%
    ggplot(aes(as.factor(row), 1, 
               label = term, 
               fill = factor(topic))) +
    geom_point(color = "transparent") +
    geom_label_repel(force = 0.1,nudge_x = .02, direction = "y",box.padding = 0.01,segment.color = "transparent",
                     size = s) +
    facet_grid(~topic) +
    theme(axis.text = element_blank(), 
          plot.title = element_text(hjust = 0.5), #center the title
          axis.ticks = element_blank(), #set axis ticks to on or off
          panel.grid.minor = element_blank(), #turn on or off the minor grid lines
          legend.position = 'none', #turn on or off the legend
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    labs(x = NULL, y = NULL, title = paste("LDA Top Terms for", input$hotel_ldak, "Topics")) +
    coord_flip()
})

# LDA comparison wordcloud showing topic specific term weights
output$hotel_ldacloud = renderPlot({
  top_term_wide = reshape2::dcast(hotel_lda[[input$hotel_ldachoice]][[input$hotel_ldak-1]], 
                                  term ~ topic, value.var="beta")
  rownames(top_term_wide) = top_term_wide$term
  top_term_wide$term = NULL
  colnames(top_term_wide) = paste0('Topic',colnames(top_term_wide))
  top_term_wide = as.matrix(top_term_wide)
  top_term_wide = round(top_term_wide/min(top_term_wide,na.rm = TRUE))
  top_term_wide[is.na(top_term_wide)] = 0
  par(mar = rep(0, 4))
  comparison.cloud(top_term_wide,random.order=FALSE, title.size=1.8,colors = hue_pal()(input$hotel_ldak))
})
