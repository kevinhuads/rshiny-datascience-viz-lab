tab_contents = tabItem(
  tabName = "Content",
  box(
    column(12,
      h1('Table of Content',style = "font-family: 'Lobster', cursive;ont-weight: 500; line-height: 1.1;  color: #4d3a7d;")),
    column(12,
      tags$ol(
        h4(tags$li(tags$b("Clustering "),': Grouping the US states according to their amount of export')), 
        h4(tags$li(tags$b("Regression"),': Predicting the number of bikes rent')),
        h4(tags$li(tags$b("Natural Language Processing"),': Analysing the patterns of Hotel Reviews')),
        h4(tags$li(tags$b("Time Series"),': Forecasting items sales')),
        h4(tags$li(tags$b("Probabilities and Statistics"),': Checking whether a coin is fair')),
        h4(tags$li(tags$b("Optimization"),': Understanding the Travelling Salesman Problem')),
        h4(tags$li(tags$b("Ordinary Differential Equations (ODE)"),': Modeling the epidemiology of the Coronavirus')),
        h4(tags$li(tags$b("Monte Carlo Simulation"),': Evaluating the value of Pi')),
        h4(tags$li(tags$b("Markov Chain"),': Simulating new words in different languages'))
      )),
    width = 12
  )
)