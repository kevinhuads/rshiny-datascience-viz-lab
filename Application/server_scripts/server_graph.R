#https://perrystephenson.me/2018/09/29/the-r-twitter-network/

output$graph_edgebet = renderPlot({
  g1 = stack_graph
  g.b <- betweenness(g1, directed = T)
  
  if(input$graph_value == 'Names') Val = V(g1)$value else Val = V(g1)$group
  
  eb <- edge.betweenness.community(g1)
  membership <- cut_at(eb, no = 15)
  par(mar=c(0,0,0,0))
  plot(g1,
       vertex.color= colorRampPalette(brewer.pal(9,input$graph_colors))(length(unique(V(g1)$group)))[V(g1)$group],
       vertex.size=8,
       vertex.label=Val,
       vertex.label.cex=1,
       layout=stack_layouts[[input$graph_layout]],
       edge.arrow.size=.3)
}, height = 800, width = 800)