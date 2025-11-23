# Optimisation section - random, greedy and simulated annealing TSP

# Randomly select one of the precomputed city configurations
optim_randn = reactive({
  input$optim_gencities
  return(sample(1:optim_set,1))
})

# Retrieve the current set of cities for the selected configuration
optim_v = reactive({
  nv= input$optim_nv
  optim_dist = 100
  villes = optim_villes[[paste0('cities',nv)]][[optim_randn()]]
  return(villes)
})

# Plot the initial cities inside the bounding square
output$optim_cities <- renderPlotly({
  v_ini = optim_v()
  
  plot_ly(v_ini, x = ~x, y = ~y, text = ~names,
          type = 'scatter', mode = 'markers', 
          marker = list(size = 10,color = 'rgba(212, 60, 60, .8)')) %>%
    layout(shapes = list(
      list(type = "rect",
           line = list(color = "black"), opacity = 0.3,
           x0 = 0, x1 = optim_dist, xref = "x",
           y0 = 0, y1 = optim_dist, yref = "y")),
      xaxis = list(title ='',zeroline = FALSE),yaxis = list(title ='',zeroline = FALSE))%>%
    config(displayModeBar = FALSE)
})

# Reactive values controlling animation of random route construction
optim_rvrand <- reactiveValues(j = 0, launch = FALSE)

# Increment animation step for random route while launch flag is TRUE
observe({
  isolate({optim_rvrand$j = optim_rvrand$j+1})
  if ((optim_rvrand$j < as.numeric(input$optim_nv) +1)&optim_rvrand$launch){
    invalidateLater(100)
  }
})

# Reset random route animation when the city set changes
observeEvent(optim_v(),{
  optim_rvrand$j = 0
  optim_rvrand$launch = FALSE
})

# Start random route animation when button is pressed
observeEvent(input$optim_genrand,{
  optim_rvrand$j = 0
  optim_rvrand$launch = TRUE
})

# Generate one random TSP solution (used as a route template)
sol_rand=reactive({
  input$optim_genrand
  c(1,1+sample(as.numeric(input$optim_nv)-1),1)
})

# Animate construction of a random TSP route and display its distance
output$optim_random <- renderPlotly({
  v_ini = optim_v()
  
  sol_rand=c(1,1+sample(as.numeric(input$optim_nv)-1),1)
  v_rand=v_ini[sol_rand(),]
  if (optim_rvrand$j>1) dist_rand = calc_dist(v_rand[1:optim_rvrand$j,1:2]) else dist_rand=0
  
  plot_ly(v_rand, x = ~x, y = ~y, text = ~names,type = 'scatter', mode = 'markers', 
          marker = list(size = 10,color = 'rgba(212, 60, 60, .8)')) %>%
    add_trace(x = v_rand[1:optim_rvrand$j,'x'], y = v_rand[1:optim_rvrand$j,'y'],mode = 'lines', type = 'scatter',
              line = list(color = 'rgb(22, 96, 167)', width = 1),inherit = FALSE) %>%
    layout(title = paste0(round(dist_rand,2),' km for ',optim_rvrand$j-1, ' cities'),showlegend = FALSE,
           shapes = list(
             list(type = "rect",
                  line = list(color = "black"), opacity = 0.3,
                  x0 = 0, x1 = optim_dist, xref = "x",
                  y0 = 0, y1 = optim_dist, yref = "y")),
           xaxis = list(title ='',zeroline = FALSE),yaxis = list(title ='',zeroline = FALSE))%>%
    config(displayModeBar = FALSE)
})

# Text summary of the distribution of random route distances
output$optim_randmean <- renderText({ 
  x = optim_rands[[paste0('cities',input$optim_nv)]][,optim_randn()]
  paste0('If we generate 50 000 random solutions, we get the following distribution for the distance travelled.
  It follows a normal distribution with an average of ',round(mean(x)),' km with a standar deviation of ',round(sd(x)),' km.')
})

# Histogram and density of distances over many random routes
output$optim_distrand <- renderPlotly({
  df = optim_rands[[paste0('cities',input$optim_nv)]]
  colnames(df)[optim_randn()] = 'Distance'
  p = ggplot(df,aes(Distance)) + 
    geom_histogram(aes(y =after_stat(density)), alpha = 0.7, fill = "#333333",binwidth = 20) + 
    geom_density(fill = "#ff4d4d", alpha = 0.5) + 
    xlab('Distance Travelled') + ylab('') +
    theme(panel.background = element_rect(fill = '#ffffff')) + 
    ggtitle("") 
  withr::with_options(list(digits = 4), ggplotly(p,tooltip = c("density","Distance")))
})



# Reactive values controlling animation for the greedy algorithm
optim_rvgreed <- reactiveValues(j = 0, launch = FALSE)

# Increment animation step for greedy route while launch flag is TRUE
observe({
  isolate({optim_rvgreed$j = optim_rvgreed$j+1})
  if ((optim_rvgreed$j < as.numeric(input$optim_nv) +1)&optim_rvgreed$launch){
    invalidateLater(100)
  }
})

# Reset greedy animation when the city set changes
observeEvent(optim_v(),{
  optim_rvgreed$j = 0
  optim_rvgreed$launch = FALSE
})

# Start greedy animation when button is pressed
observeEvent(input$optim_lgreed,{
  optim_rvgreed$j = 0
  optim_rvgreed$launch = TRUE
})

# Compute greedy TSP route (nearest neighbour heuristic)
optim_vgreed <- reactive({
  v_rest=optim_v()[,1:2]
  nv = nrow(v_rest)
  Trajet=1
  for (i in 1:(nv-2)) {
    cible=Trajet[i]
    villes2 = v_rest[-which(rownames(v_rest)%in%as.character(cible)),]
    x = v_rest[which(rownames(v_rest)%in%as.character(cible)),]
    distance = sqrt(apply((unlist(c(x))-t(as.matrix(villes2)))^2,2,sum))
    Trajet = c(Trajet,as.numeric(names(which.min(distance))))
    v_rest = villes2
  }
  Trajet = c(Trajet,which(!1:nv%in%Trajet),1)
  v_traj = optim_v()[Trajet,]
  return(v_traj)
})

# Text comparing greedy route distance to average random route distance
output$optim_greedtext <- renderText({ 
  dist_glout = calc_dist(optim_vgreed()[,1:2])
  x = mean(optim_rands[[paste0('cities',input$optim_nv)]][,optim_randn()])
  paste0('We have a final distance travelled of ',round(dist_glout), ' km which is already ',round(x/dist_glout,1),
         ' better than the average random solution.')
})

# Animate greedy route construction and show current distance
output$optim_greedy <- renderPlotly({
  if (optim_rvgreed$j>1) dist_glout = calc_dist(optim_vgreed()[1:optim_rvgreed$j,1:2]) else dist_glout=0
  
  plot_ly(optim_vgreed(), x = ~x, y = ~y, text = ~names,type = 'scatter', mode = 'markers', 
          marker = list(size = 10,color = 'rgba(212, 60, 60, .8)')) %>%
    add_trace(x = optim_vgreed()[1:optim_rvgreed$j,'x'], y = optim_vgreed()[1:optim_rvgreed$j,'y'],mode = 'lines', type = 'scatter',
              line = list(color = 'rgb(22, 96, 167)', width = 1),inherit = FALSE) %>%
    layout(title = paste0(round(dist_glout,2),' km for ',optim_rvgreed$j-1, ' cities'),showlegend = FALSE,
           shapes = list(
             list(type = "rect",
                  line = list(color = "black"), opacity = 0.3,
                  x0 = 0, x1 = optim_dist, xref = "x",
                  y0 = 0, y1 = optim_dist, yref = "y")),
           xaxis = list(title ='',zeroline = FALSE),
           yaxis = list(title ='',zeroline = FALSE))%>%
    config(displayModeBar = FALSE)
})

# Reactive values driving the simulated annealing process
optim_rv <- reactiveValues(T=optim_Tinit,
                           i=0,
                           traj = 0,
                           villes_RC = 0,
                           J1 = 0,
                           traj_opt = 0,
                           J_opt = 0,
                           launch = FALSE)

# Store best distances over iterations for error plot
optim_err <- reactiveValues(distance = 0,iter = 0)

# Plot current best simulated annealing route and its distance
output$optim_anneal <- renderPlotly({
  villes = optim_v()
  nv = nrow(villes)
  
  if (optim_rv$i == 0) villes_RC = villes else villes_RC = optim_rv$villes_RC
  
  villes_RC$names = paste0('City',rownames(villes_RC))
  plot_ly(villes_RC, x = ~x, y = ~y, text = ~names,
          type = 'scatter', mode = 'lines+markers',
          marker = list(size = 10,color = 'rgba(212, 60, 60, .8)'),
          line = list(color = 'rgb(22, 96, 167)', width = 1)) %>%
    layout(title = paste0("Distance travelled : ",round(optim_rv$J_opt,2),' km'),
           shapes = list(
             list(type = "rect",
                  line = list(color = "black"), opacity = 0.3,
                  x0 = 0, x1 = optim_dist, xref = "x",
                  y0 = 0, y1 = optim_dist, yref = "y")),
           xaxis = list(title = paste0(optim_rv$i," iterations"),zeroline = FALSE),
           yaxis = list(title = paste0("Temperature : ",formatC(optim_rv$T, format = "e", digits = 2)),zeroline = FALSE)) %>%
    config(displayModeBar = FALSE)
})

# Plot distance as a function of iterations and compare to greedy distance
output$optim_annealerr <- renderPlotly({
  df = data.frame(Iteration = optim_err$iter,Distance = optim_err$distance)
  dist_glout = calc_dist(optim_vgreed()[,1:2])
  
  plot_ly(df, x = ~Iteration, y = ~Distance,type = 'scatter', mode = 'lines+markers',
          marker = list(size = 6,color ='rgba(0, 0, 152, .8)',
                        line = list(color = 'rgba(0, 0, 152, .8)',width = 1.5)))%>%
    add_annotations(
      x = as.numeric(input$optim_iter)*0.8,y = dist_glout,
      text = 'Greedy Algorithm',xref = "x",yref = "y",
      showarrow = TRUE,arrowhead = 7,ax = 40,ay = -20,
      font = list(color = 'rgba(212, 60, 60, .8)')
    ) %>%
    add_annotations(
      x = 0,y = optim_err$distance[1],
      text = 'Simulated Annealing',xref = "x",yref = "y",
      showarrow = TRUE,arrowhead = 7,ax = 40,ay = -20,
      font = list(color = 'rgba(0, 0, 152, .8)')
    ) %>%
    layout(shapes = list(
      list(line = list(color = "rgba(212, 60, 60, .8)"),
           x0 = 0, x1 = input$optim_iter+10, xref = "x",
           y0 = dist_glout, y1 = dist_glout, yref = "y")),
      xaxis = list(title = 'Iterations',range = c(0,input$optim_iter+10),zeroline = FALSE),
      yaxis = list(title = 'Distance',range = c(0,optim_err$distance[1]+10),zeroline = FALSE))
})

# Initialise simulated annealing state when cities are regenerated
observeEvent(optim_v(),{
  optim_rv$launch <- FALSE
  optim_rv$T <- optim_Tinit
  optim_rv$i <- 0
  optim_rv$traj <- c(1:input$optim_nv,1)
  optim_rv$villes_RC <- optim_v()[c(1:input$optim_nv,1),1:2]
  optim_rv$J1 <- calc_dist(optim_v()[c(1:input$optim_nv,1),1:2])
  optim_rv$traj_opt <-c(1:input$optim_nv,1)
  optim_rv$J_opt <- calc_dist(optim_v()[c(1:input$optim_nv,1),1:2])
  
  optim_err$distance <- calc_dist(optim_v()[c(1:input$optim_nv,1),1:2])
  optim_err$iter <- 0
})

# Reset and launch simulated annealing when button is pressed
observeEvent(input$optim_genanneal,{
  optim_rv$T <- optim_Tinit
  optim_rv$i <- 0
  optim_rv$traj <- c(1:input$optim_nv,1)
  optim_rv$villes_RC <- optim_v()[c(1:input$optim_nv,1),1:2]
  optim_rv$J1 <- calc_dist(optim_v()[c(1:input$optim_nv,1),1:2])
  optim_rv$traj_opt <-c(1:input$optim_nv,1)
  optim_rv$J_opt <- calc_dist(optim_v()[c(1:input$optim_nv,1),1:2])
  optim_rv$launch <- TRUE  
  
  optim_err$distance <- calc_dist(optim_v()[c(1:input$optim_nv,1),1:2])
  optim_err$iter <- 0
})

# Core simulated annealing loop, updating route, best distance and temperature
observe({
  isolate({
    one_step = anneal_step(optim_rv$T,
                           optim_rv$i,
                           optim_rv$traj,
                           optim_rv$villes_RC,
                           optim_rv$J1,
                           optim_rv$traj_opt,
                           optim_rv$J_opt,
                           input$optim_display,
                           1-0.002,
                           optim_rv$launch)
    
    optim_rv$T = one_step$T
    optim_rv$i = one_step$i
    optim_rv$traj = one_step$traj
    optim_rv$villes_RC = one_step$villes_RC
    optim_rv$J1 = one_step$J1
    optim_rv$traj_opt = one_step$traj_opt
    optim_rv$J_opt = one_step$J_opt
    
    optim_err$distance <- c(optim_err$distance,one_step$J_opt)
    optim_err$iter <- c(optim_err$iter,one_step$i)
  })
  
  if ((optim_rv$i < input$optim_iter)&optim_rv$launch){
    invalidateLater(0)
  }
})
