# server.R
shinyServer(
  function(input, output,session){
    addClass(selector = "body", class = "sidebar-collapse")
    source("server_scripts/server_aboutme.R",local=TRUE)
    source("server_scripts/server_regression.R",local=TRUE)
    source("server_scripts/server_clustering.R",local=TRUE)
    source("server_scripts/server_classification.R",local=TRUE)
    source("server_scripts/server_nlp.R",local=TRUE)
    source("server_scripts/server_prob.R",local=TRUE)
    source("server_scripts/server_mc.R",local=TRUE)
    source("server_scripts/server_timeseries.R",local=TRUE)
    source("server_scripts/server_ode.R",local=TRUE)
    source("server_scripts/server_optim.R",local=TRUE)
    source("server_scripts/server_graph.R",local=TRUE)
    source("server_scripts/server_markov.R",local=TRUE)
})
