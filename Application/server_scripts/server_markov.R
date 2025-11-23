# Markov section - server outputs

# Table of most frequent n-character words per language
output$markov_table = DT::renderDataTable({
  datatable(markov_top[[paste0("nchar",input$markov_tableind)]], 
            extensions = 'Scroller', options = list(
              deferRender = TRUE,
              scrollY = 500,
              scroller = TRUE,
              scrollX = TRUE,
              autoWidth = TRUE
            ), 
            rownames = FALSE)
})

# Distribution of word lengths across languages
output$ma_length = renderHighchart({
  mat = sapply(markov_length, '[', 1:max(sapply(markov_length, length)))
  mat = as.data.frame(mat[1:min(24,nrow(mat)),]) 
  mat = mat %>% 
    mutate(length = 1:nrow(mat))%>%
    gather(lang,frequency,English:Dutch,factor_key = TRUE)
  
  highchart() %>% 
    hc_chart(type = 'column')  %>%
    hc_add_series(name = 'English',data = mat$frequency[mat$lang== "English"]) %>%
    hc_add_series(name = 'German',data = mat$frequency[mat$lang== "German"]) %>%
    hc_add_series(name = 'French',data = mat$frequency[mat$lang== "French"],visible = FALSE) %>%
    hc_add_series(name = 'Portuguese',data = mat$frequency[mat$lang== "Portuguese"],visible = FALSE) %>%
    hc_add_series(name = 'Spanish',data = mat$frequency[mat$lang== "Spanish"],visible = FALSE) %>%
    hc_add_series(name = 'Italian',data = mat$frequency[mat$lang== "Italian"],visible = FALSE) %>%
    hc_add_series(name = 'Dutch',data = mat$frequency[mat$lang== "Dutch"],visible = FALSE)  %>%
    hc_yAxis(title = "",labels = list(formatter =   JS("function () {return Highcharts.numberFormat(Math.abs(100*this.value),1) + '%';}")))%>% 
    hc_xAxis(lineWidth = 1,tickWidth = 1,tickInterval = 1,gridLineWidth = 0)  %>%
    hc_tooltip(formatter = JS("function () {
                              return '<b>' + this.series.name + ', Words with ' + this.point.category + ' letters </b><br/>' +
                              'Percentage: ' + Highcharts.numberFormat(Math.abs(100*this.point.y), 2) + '%';
    }"))  
})

# Single-character frequency distribution for a selected language
output$ma_occur = renderHighchart({
  df = data.frame(markov_gram[[input$ma_occurlang]],stringsAsFactors = FALSE) %>%
    dplyr::rename(name = Var1, y = Freq)  %>% 
    arrange(desc(y)) %>%
    mutate(
      z = y, e = y/10,value = y,
      color = rev(colorRampPalette(brewer.pal(9,input$ma_occurcol))(length(y))),
      segmentColor = color
    )
  highchart() %>%
    hc_chart(type = input$ma_occurtype) %>% 
    hc_xAxis(categories = df$name) %>% 
    hc_add_series(df, showInLegend = FALSE)  %>%
    hc_yAxis(title = "",labels = list(formatter =   JS("function () {return Highcharts.numberFormat(Math.abs(100*this.value),1) + '%';}")))%>%
    hc_tooltip(formatter = JS("function () {
                              return '<b>' + 'Letter ' + this.point.name + '</b><br/>' +
                              'Percentage: ' + Highcharts.numberFormat(Math.abs(100*this.point.y), 2) + '%';
    }"))  
})

# Bigram transition probability heatmap for the selected language
output$ma_heatmap = renderHighchart({
  pat_ = '[a-z]'
  pat_ad = " "
  x = markov_prob[[input$ma_heatlang]]
  
  pat_ad = c(pat_ad,markov_accents[[input$ma_heatlang]])
  pat_ = paste0(c(pat_,pat_ad),collapse = "|")
  
  aggregate(x$freq,by = list(x$let2,x$let3),FUN = sum) %>%
    dplyr::rename(let1 = Group.1,let2 = Group.2, freq = x) %>%
    filter(grepl(pat_,let1)&grepl(pat_,let2)) %>%
    filter(!(let1 == " "&let2 == " ")) %>%
    mutate(freq = freq/ave(freq,let1,FUN = sum)) %>% 
    tidyr::spread(let2, freq) %>%
    replace(., is.na(.), 0) %>% 
    tidyr::gather(let2, freq, -let1, factor_key = TRUE)%>%
    filter(!(let1 == " "&let2 == " ")) %>%
    mutate(freq = round(100*freq,2)) %>%
    hchart("heatmap", hcaes(x = let2, y = let1, value = freq))  %>% 
    hc_colorAxis(stops = color_stops(10, colorRampPalette(brewer.pal(9,input$ma_heatcol))(100)))%>%
    hc_tooltip(headerFormat = " ",
               pointFormat = tooltip_table(
                 x = c("Letter","Following Letter","Probability"),
                 y = c(" {point.let1}", " {point.let2}"," {point.freq} %"), 
                 style = "text-align: center;"),
               useHTML = TRUE,shadow = FALSE,
               style = list(fontWeight = "normal"))%>% 
    hc_xAxis(title = list(text = "Following letter"),opposite = TRUE,labels = list(step = 1,style = list(fontSize = 8)))%>% 
    hc_yAxis(title = list(text = "Letter"),reversed = TRUE,labels = list(step = 1,style = list(fontSize = 8))) %>%
    hc_plotOptions(column = list(dataLabels = list(allowOverlap = TRUE)))
  
})

# Generated word table from Markov chain model for selected language and length
output$ma_gentable = DT::renderDataTable({
  input$ma_gennew
  withProgress(message = 'Calculation in progress',value = 0, {
    for (i in 1:20) {
      incProgress(1/20)
      Sys.sleep(0.025)
    }
  })
  
  x = markov_new[[input$ma_genlang]]
  n = as.numeric(input$ma_genlength)
  sapply(1:5, function(i) sample(x[nchar(x) == n],5)) %>%
    datatable(rownames = FALSE, colnames = rep("", 5),
              options = list(dom = '', ordering=FALSE))
})
