# output$euromap <- renderLeaflet({
#   pal <- colorNumeric(input$eurocolors, NULL)
#   
#   fill_euro = euro[,input$eurochoice]
#   leaflet(euro_map) %>%
#     addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
#                 fillColor = ~pal(fill_euro)) %>%
#     addLegend(pal = pal, values = ~fill_euro, opacity = 1.0, title = input$eurochoice)%>%
#     addTiles()%>%
#     addTopoJSON(euro_topo, weight = 1, color = "#444444", fill = FALSE)
# })

#https://github.com/leakyMirror/map-of-europe
#https://www.destatis.de/Europa/EN/Country/Comparison/GER_EU_Compared.html
#https://www.europeandataportal.eu/en/dashboard#tab-detailed

output$usa_table = DT::renderDataTable({
  datatable(usa_df, extensions = 'Scroller', options = list(
    deferRender = TRUE,
    scrollY = 500,
    scroller = TRUE,
    scrollX = TRUE,
    autoWidth = TRUE
  ), 
  rownames = FALSE)
})


output$usa_quantnum <- renderUI({
  if (input$usa_quantcol){
    sliderInput('usa_quantnum2', 'Number of Bins', max = 10,min = 2, value = 5, step = 1)
  } 
})

output$usamap <- renderLeaflet({
  usa_fill = usa_df[,input$usachoice]
  
  if (input$usa_quantcol){
    shiny::validate(need(input$usa_quantnum2,message = FALSE))
    
    pals_ <- leaflet::colorBin(input$usacolors, domain = usa_fill,reverse = input$usa_revcol, bins =input$usa_quantnum2,right = TRUE,pretty = FALSE)
    cols_ <- pals_(usa_fill)
  } else {
    cols_ <- leaflet::colorNumeric(input$usacolors, NULL,reverse = input$usa_revcol)(usa_fill)
    pals_ <- leaflet::colorNumeric(input$usacolors, domain = usa_fill,reverse = input$usa_revcol)
  }
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g millions exports",
    usa_leaf$name, usa_fill
  ) %>% lapply(htmltools::HTML)
  
  
  usa_leaf <- spTransform(usa_leaf, CRS("+proj=longlat +datum=WGS84"))
  leaflet(usa_leaf) %>%
    setView(-96, 38, 4) %>% 
    addPolygons(
      fillColor = cols_,
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
      )  %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Gray Scale") %>%
    addProviderTiles(providers$Esri.WorldStreetMap, group = 'World Street Map') %>%
    addLayersControl(
      baseGroups = c("Gray Scale",'World Street Map'),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegend("bottomright", pal = pals_, values = usa_fill,
              title = "Millions Dollars",
              labFormat = labelFormat(digits = 0, transform = pers_pretty),
              opacity = 1
    )
})

#http://www.ipl.org/div/stateknow/popchart.html#

output$usa_corrui = renderUI({
  if (input$usa_corrorder == 'hclust'){
    numericInput('usa_corrrect',label = "Number of Squares",min = 2,max = 8,value = 4)
  }
})

output$usa_corr = renderPlot({
  col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                             "cyan", "#007FFF", "blue", "#00007F"))
  
  corrplot(cor(usa_df[,usa_export]),method = input$usa_corrmeth, order = input$usa_corrorder,
           addrect = input$usa_corrrect, number.cex = .8, col = col1(100))
})

output$usa_pca<- renderPlotly({
  shiny::validate(need(
    input$usa_pcanumb,'Loading...'
  ))
  
  if(input$usa_pcachoice == 'numb'){
    usa_n1 = input$usa_pcanumb
    usa_n = usa_n1+1
    y1 = seq(0,1,by = 0.1)
    x1 = rep(usa_n1,11)
  } else {
    shiny::validate(need(input$usa_pcapart,message = FALSE))
    usa_thres= input$usa_pcapart
    usa_n = min(which(usa_eig$Variance_Explained>usa_thres))
    x1 = 0:(length(usa_pca$eig))
    y1 = rep(usa_thres,length(usa_pca$eig)+1)
  }
  
  usa_marker = list(size = c(0,rep(12,nrow(usa_eig)-1)),
                    color = rep(c('rgba(255, 182, 193, .9)','rgba(193, 182, 255, .9)'),time = c(usa_n,nrow(usa_eig)-usa_n)),
                    line = list(color = rep(c('rgba(152, 0, 0 .8)','rgba(0, 0, 152, .8)'),time =  c(usa_n,nrow(usa_eig)-usa_n)),
                                width = 2))
  
  plot_ly(usa_eig, 
          y = ~Variance_Explained, 
          x = ~Eigenvalues, 
          type = 'scatter', 
          mode  = 'lines + markers',
          marker = usa_marker,
          line = list(color = 'rgba(0,0, 0 .3)', width = 1.5),
          name = 'Eigenvalues')  %>%
    add_trace(x=x1, 
              y=y1, 
              mode = 'lines+markers',
              line = list(color = 'rgba(152,50, 50 .6)', dash = 'dash', width = 3),
              name = 'threshold',
              marker = list(size = 0.5))  %>%
    layout(xaxis = list(title = 'Number of Components',dtick = 1),
           yaxis = list(title = 'Part of variance explained',dtick = 0.1, tickformat = "%"),
           showlegend = FALSE)
})

# highchart() %>% 
#   hc_chart(polar = FALSE) %>% 
#   hc_title(text = "Principal Component Analysis Eigenvalues") %>% 
#   hc_xAxis(categories = usa_eig$Eigenvalues[-1],
#            tickmarkPlacement = "on",
#            crosshair = TRUE) %>% 
#   hc_yAxis(gridLineInterpolation = "polygon") %>% 
#   hc_series(
#     list(
#       name = "Spend",
#       data = usa_eig$Variance_Explained[-1],
#       colorByPoint = TRUE,
#       type = "column",
#       colors = usa_marker$color[-1]
#     ),
#     list(
#       name = "Budget",
#       data = diff(usa_eig$Variance_Explained),
#       type = "line",
#       color = 'grey'
#     )
#   )


output$usa_corrplot = renderPlot({
  if(input$usa_pcachoice == 'numb'){
    usa_n = input$usa_pcanumb
  } else {
    usa_thres= input$usa_pcapart
    usa_n = min(which(usa_eig$Variance_Explained>usa_thres))-1
  }
  
  corrplot(t(as.matrix(get_pca_var(usa_pca)$cos2))[1:usa_n,])
})

output$usa_pcachoicenum = renderUI({
  if(input$usa_pcachoice == 'numb'){
    sliderInput('usa_pcanumb','Number of Components',min = 2, max = 10,value = 4, step = 1)
  } else {
    sliderInput('usa_pcapart','Part of Variance Explained',min = 0.55, max = 0.95, value = 0.75, step = 0.05)
  }
})

output$pca_box <- renderValueBox({
  shiny::validate(need(
    input$usa_pcanumb,'Loading...'
  ))
  if(input$usa_pcachoice == 'numb'){
    usa_n = input$usa_pcanumb
    val = percent(usa_eig$Variance_Explained[usa_eig$Eigenvalues == usa_n])
    sub_title = paste('of the variance is explained for',usa_n,'principal components')
  } else {
    usa_thres= input$usa_pcapart
    val = min(which(usa_eig$Variance_Explained>usa_thres))-1
    sub_title = paste('components are enough to have more than',percent(usa_thres),'of the variance explained')
  }
  valueBox(
    val, sub_title, icon = icon("percent"),
    color = "blue",
    width = 12
  )
})

output$usa_pcaplot <- renderPlot({
  
  if(input$usa_flat1) {
    temp = usa_pca_sqrt
  } else {
    temp = usa_pca
  }
  
  if (input$usa_pcadisp == 'States'){ 
    
    fviz_pca_ind(temp,
                 axes = as.numeric(strsplit(input$usa_pcacomp,',')[[1]]),
                 col.ind = "cos2", 
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = input$usa_repel1  
    )
  } else {
    if (input$usa_pcadisp == 'Variables'){
      fviz_pca_var(temp,
                   axes = as.numeric(strsplit(input$usa_pcacomp,',')[[1]]),
                   col.var = "contrib", # Color by contributions to the PC
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = input$usa_repel1 
      )
    } else {
      fviz_pca_biplot(temp,
                      axes = as.numeric(strsplit(input$usa_pcacomp,',')[[1]]),
                      col.var = "#2E9FDF", 
                      col.ind = "#696969" , 
                      repel = input$usa_repel1
      )
    }
  } 
})


usa_pcadb = reactive({
  if (input$usa_flat2) {
    usa_pca_sqrt
  } else {
    usa_pca
  }
})

usa_hclust = reactive({
  temp = usa_pcadb()$li[,as.numeric(input$usa_compchoice)] 
  if(length(as.numeric(input$usa_compchoice))==1) names(temp) = rownames(usa_pcadb()$li)
  hclust(dist(temp))
})

output$usa_pcahdendr <- renderPlot({
  
  shiny::validate(need(
    !is.null(input$usa_compchoice),'Select at least one component'
  ))
  
  temp = usa_pcadb()
  h = usa_hclust()
  
  k_groups = input$usa_pcak
  groups <- cutree(h, k = k_groups)
  usa_dend = as.dendrogram(h)
  colors <- brewer.pal(k_groups,"Set1")[unique(groups[h$order])]
  usa_dend %>% 
    set("branches_k_color", colors, k = k_groups) %>%
    set("labels_col",colors, k=k_groups) %>%
    plot
  usa_dend %>% rect.dendrogram(k=k_groups, border = 8, lty = 5, lwd = 2)
})

output$usa_pcahclust <- renderPlot({
  shiny::validate(need(input$usa_pcacomp2,message = 'Loading...'))
  
  shiny::validate(need(
    !is.null(input$usa_compchoice),'Select at least one component'
  ))
  
  temp = usa_pcadb()
  h = usa_hclust()
  
  k_groups = input$usa_pcak
  groups <- cutree(h, k = k_groups)
  fviz_pca_ind(temp, 
               axes = as.numeric(strsplit(input$usa_pcacomp2,',')[[1]]),
               palette = 'Set1',
               col.ind = groups,
               habillage = groups,
               addEllipses = TRUE,
               alpha.var ="cos2",
               repel = FALSE
  )
})

output$usa_pcacond = renderUI({
  if (input$tab_usa == 'Scatterplot') 
    selectInput('usa_pcacomp2',label = 'Plot Axis',
                choices = c('Components 1 and 2' = '1,2',
                            'Components 1 and 3' = '1,3',
                            'Components 2 and 3' = '2,3'))
})

output$usamap2 <- renderLeaflet({
  shiny::validate(need(
    !is.null(input$usa_compchoice),'Select at least one component'
  ))
  
  temp = usa_pcadb()
  
  h = usa_hclust()
  k_groups = input$usa_pcak
  groups <- cutree(h, k = k_groups)
  
  cols2 <- colorFactor('Set1', domain = NULL,n=k_groups)(groups)
  pals2 <- colorFactor('Set1', domain = groups,n=k_groups)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>Group %g",
    usa_leaf$name, groups
  ) %>% lapply(htmltools::HTML)
  
  leaflet(usa_leaf) %>%
    setView(-96, 38, 4) %>% 
    addPolygons(
      fillColor = cols2,
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.6,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))  %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Gray Scale") %>%
    addProviderTiles(providers$Esri.WorldStreetMap, group = 'World Street Map') %>%
    addLayersControl(
      baseGroups = c("Gray Scale",'World Street Map'),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegend("bottomright", pal = pals2, values = groups,
              title = "Clusters",
              # labFormat = labelFormat(prefix = "$"),
              opacity = 1
    )
})