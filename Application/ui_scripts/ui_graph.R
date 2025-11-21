tab_graph = shinydashboard::tabItem(tabName = "Graph",
                                    fluidPage(
                                      fluidRow(
                                        box(h2('Graph Analysis'),
                                            sidebarLayout(
                                              
                                              sidebarPanel(
                                                radioButtons('graph_layout','Graph Layout',choices = names(stack_layouts)),
                                                radioButtons('graph_value','Display', choices = c('Names' ,'Groups' )),
                                                selectInput('graph_colors', 'Color Palette', choices = c('Spectral',palettes_quali))
                                              ),
                                              
                                              mainPanel(
                                                plotOutput('graph_edgebet', width = '100%')
                                              )
                                            ),
                                            height = 900,
                                            width = 12)
                                      )))