ode_first = reactive({
  if (input$ode_intervention){
    ep_prob_inf = 0.003
    ep_inter = 40
  } else {
    ep_prob_inf = 0.01
    ep_inter = 100
  }
  if(input$ode_hop) ep_cap = 80000 else ep_cap = 0
  return(sim_sir(ep_prob_inf = ep_prob_inf, ep_inter = ep_inter, ep_cap = ep_cap,ep_cap_red = 0.2,
                 ep_vr = FALSE,ep_vs = FALSE,peak = FALSE))
})

output$ode_first_plot = renderHighchart({
  ode_first()$plot
})

output$ode_first_death = renderValueBox({
  valueBox(
    paste(round(ode_first()$deaths/10000,4),"%"),
    'Deaths at the end', 
    icon = icon("skull"),
    color = "red",
    width = 12
  )
})

output$ode_first_length = renderValueBox({
  valueBox(
    ode_first()$days, 
    'Days of Infections', 
    icon = icon("ambulance"),
    color = "blue",
    width = 12
  )
})

ode_final = reactive({
  return(sim_sir(ep_inter = input$ode_int, ep_prob_inf = input$ode_probinf,
                 ep_prob_death = input$ode_probdea, ep_dur = input$ode_dur,
                 ep_type = input$ode_type, ep_cap = input$ode_cap, ep_cap_red = input$ode_red_cap/100,
                 ep_vaccin = input$ode_vaccin/100, ep_i0 = input$ode_i0,
                 ep_vs = "Susceptible"%in%input$ode_display,
                 ep_vi = "Infected"%in%input$ode_display,
                 ep_vr = "Recovered"%in%input$ode_display,
                 ep_vd = "Dead"%in%input$ode_display))
})

output$ode_highchart = renderHighchart({
  ode_final()$plot
})

# output$ode_total = renderValueBox({
#   valueBox(
#     input$ode_s0 + input$ode_i0 + input$ode_r0 + input$ode_d0, 
#     'Initial Population', 
#     icon = icon("users"),
#     color = "green",
#     width = 12
#   )
# })

output$ode_length = renderValueBox({
  valueBox(
    ode_final()$days, 
    'Days of Infections', 
    icon = icon("ambulance"),
    color = "blue",
    width = 12
  )
})

output$ode_peak = renderValueBox({
  valueBox(
    ode_final()$peak, 
    'Day of the Peak', 
    icon = icon("ambulance"),
    color = "black",
    width = 12
  )
})

output$ode_deaths = renderValueBox({
  valueBox(
    paste(round(ode_final()$deaths/10000,4),"%"), 
    'Deaths', 
    icon = icon("skull"),
    color = "red",
    width = 12
  )
})

# output$ode_parimpact = renderHighchart({
#   vec_ep_inter = 1:60
#   test = sapply(vec_ep_inter,function(j) unlist(sim_sir(ep_inter = j)[2:4])) %>%
#     t() %>%
#     as.data.frame()
#   test$ep_inter = vec_ep_inter
#   
#   highchart() %>% 
#     hc_yAxis_multiples(
#       list(title = list(text = "Days of Infection",style = list(color = ep_cols[2])),
#            labels = list(style = list(color = ep_cols[2]))),
#       list(title = list(text = "Deaths",style = list(color = ep_cols[1])),
#            labels = list(style = list(color = ep_cols[1])),opposite = TRUE)) %>%
#     hc_chart(type = "line") %>%
#     hc_xAxis(data = test,title = list(text = "Number of Interactions")) %>%
#     hc_add_series(name = 'Days of Infection',data = test$days,color = ep_cols[2]) %>%
#     hc_add_series(name = 'Deaths',data = test$deaths,color = ep_cols[1],yAxis = 1) %>%
#     hc_tooltip(useHTML= TRUE,table= TRUE,sort= TRUE) 
# })

observeEvent(input$ode_help,{
  introjs(session, options = list(
    "nextLabel"="Next",
    "prevLabel"="Previous",
    "skipLabel"="Quit tutorial",
    steps = data.frame(
      element = c("#ode_highchart","#ode_type","#ode_display","#ode_int","#ode_dur",
                  '#ode_probinf','#ode_probdea','#ode_cap','#ode_red_cap',
                  '#ode_i0','#ode_vaccin','#ode_length','#ode_peak','#ode_deaths'),
      intro = c(
        "This is the plot displaying the evolution of each group of population over the days
        until there are no people infected anymore. 
        You can hide some groups by clicking on them on the legend below. <br> <br>
        You can navigate in this tutorial either by clicking on <em>Next</em>,<em>Previous</em> and <em>Quit tutorial</em> 
        or by using the arrows left and right of your keyboard",
        "You can change here the type of the plot: The line plot display each group independantly
        whereas the area plot stacks the areas and the sum of each group is always equal to the total population",
        "Here you can choose to hide some groups by default, which can be useful if you want to see
        the impact of some parameters below.",
        "This is the average number of interactions each people has everyday. </br>
        You can change the value either by writing manually the number or clicking on the arrows.",
        "The duration of the disease is the duration an infected people will stay infected.
        At the end of the duration, he will either recover or die.",
        "This is the probability infected people will infect susceptible people when they interact.",
        "This is the probability infected people will die.",
        "This is the capacity of the hospitals of the nation.",
        "This is the reduction of the probability of death. <br> 
        For example, if the initial probability of death is 3% and the reduction is 0.5 
        and if there are 100000 persons infected and the hospital capacity 20000, for those
        20000 people the probability of death will be 0.5*3% = 1.5% <br> <br>
        Basically, a reduction to 0% would mean that the hospital are performing extremely well since
        nobody in those hospital will die. <br>
        In opposite, if this rate is equal to 100%, it means that an infected person has as much chance
        as dying with treatment as without and thus that the hospitals will not impact the final result",
        "This is the initial number of infected people. Generally, only one person infected is 
        enough for the disease to spread. Choosing a higher initial number of infected people could 
        be useful if you want to start the simulation after the disease has already spread",
        "You also have the option to add vaccined people, which is, as it has been said before,
        equivalent as adding recovered people at the beginning.",
        "This is the number of days until the disease has totally disappeared.",
        "This is the day at which the peak of infections is reached. Generally, the number of infections
        can only increase until the peak is reached and then decreases. <br>  <br>
        Before the peak, the situation gets worse every day as the number of infections increases exponentially.
        until there are not enough susceptible people to infect. <br>  <br>
        After the peak, the number of infected people decreases every day as most people are either infected, 
        recovered or dead",
        "This is the proportion of death at the end of the disease."))))
})