source('ui_scripts/ui_introduction.R',local = TRUE)
source('ui_scripts/ui_aboutme.R',local = TRUE)
source('ui_scripts/ui_contents.R',local = TRUE)
source("ui_scripts/ui_regression.R",local=TRUE)
source("ui_scripts/ui_clustering.R",local=TRUE)
source("ui_scripts/ui_nlp.R",local=TRUE)
source("ui_scripts/ui_timeseries.R",local=TRUE)
source("ui_scripts/ui_prob.R",local=TRUE)
source("ui_scripts/ui_optim.R",local = TRUE)
source("ui_scripts/ui_ode.R",local=TRUE)
source("ui_scripts/ui_mc.R",local=TRUE)
source("ui_scripts/ui_markov.R",local=TRUE)

ui <- dashboardPage(
  header = dashboardHeader(
    title = tagList(
      tags$span(class = "logo-mini", "KH"),
      tags$span(class = "logo-lg", "Kevin Hua")
    )
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Presentation", icon = icon("r-project")),
      menuItem("About Me", tabName = "About", icon = icon("user")),
      menuItem("Table of Content", tabName = "Content", icon = icon("list")),
      hr(),
      menuItem("Clustering", tabName = "Clustering", icon = icon("object-group")),
      menuItem("Regression", tabName = "Regression", icon = icon("bar-chart")),
      menuItem("Natural Language Processing", tabName = "NLP", icon = icon("comments")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("line-chart")),
      menuItem("Probabilities and Statistics", tabName = "PS", icon = icon("percent")),
      menuItem("Optimization", tabName = "Optimization", icon = icon("arrow-alt-circle-up")),
      menuItem("Differential Equations", tabName = "ODE", icon = icon("braille")),
      menuItem("Monte Carlo Simulation", tabName = "MonteCarlo", icon = icon("random")),
      menuItem("Markov Chain", tabName = "Markov", icon = icon("share-alt"))
    )
  ),
  body = dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');"))),
    tags$style(HTML(".introjs-tooltip {max-width: 100%;min-width: 500px;}")),
    tags$style(HTML("
      .box.box-solid.box-info>.box-header {color:#fff;background:#666666}
      .box.box-solid.box-info{
      border-bottom-color:#666666;border-left-color:#666666;
      border-right-color:#666666;border-top-color:#666666;}")),
    tags$script(HTML("$('body').addClass('sidebar-mini');")),
    tags$head(tags$style(HTML('.modal-lg {width: 95%;}'))),
    tags$head(tags$style(HTML('.modal-sm {width: 25%;}'))),
    tabItems(
      tab_intro,
      tab_aboutme,
      tab_contents,
      tab_clust,
      tab_reg,
      tab_hotel,
      tab_timeseries,
      tab_prob,
      tab_ode,
      tab_optim,
      tab_mc,
      tab_markov
    )
  ),
  title = "Kevin Hua",
  skin  = "black"
)


