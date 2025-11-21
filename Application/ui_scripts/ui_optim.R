tab_optim = tabItem(
  tabName = "Optimization",
  fluidPage(
    fluidRow(
      box(column(12,h1(tags$b("Optimization"),': Understanding the Travelling Salesman Problem')),
          intro('The goal of mathematical optimization is usually to minimize (or maximize) a function 
                by choosing input values from within an allowed set and computing the value of the function.'),
          column(12,hr()),
          column(12,
                 column(6,offset = 0, style='padding:0px;',
                        p('Suppose we have a list of cities in an area of 100km*100km, 
                          what is the shortest path to visit each cities and return to the original city?'),
                        p('Despite the simplicity of the statement, The Travelling Sales Problem (TSP) is a ',em('NP-Hard'),
                          ' problem for which we can not quickly find an exact solution in every case.
                          This is especially due to the fact that this problem is facing combinatorial explosion.'),
                        p('In fact, if we have n cities, we will have \\(\\frac{(n-1)!}{2}\\) solutions possible. 
                          To give you an idea of the hugeness of this number, this mean that for :'),
                        tags$ul(
                          tags$li("n = 10 cities, we have \\(\\ 181 440\\) solutions"),
                          tags$li("n = 25 cities, we have \\(\\ 3.10*10^{23}\\) solutions"),
                          tags$li("n = 50 cities, we have \\(\\ 3.04*10^{62}\\) solutions")
                        ),
                        p('With so many posibilities, it is quite obvious that with a high number of cities,
                          we can not just test all the solutions and determine which one is the best. We have find more optimal methods')),
                 column(6,
                        column(12,
                               column(6,radioButtons('optim_nv','Number of Cities',choices = c(25,50,100,150),selected = 100,inline = TRUE)),
                               column(6,actionBttn(inputId = "optim_gencities",label = "Generate New Cities", 
                                                   style = "fill", color = "danger",icon = icon("redo")))),
                        column(12,withSpinner(plotlyOutput('optim_cities'))))
                        ),
          column(12,hr()),
          subtitle_('Random Solution'),
          column(12,
                 column(6,offset = 0, style='padding:0px;',
                        column(12,br()),
                        p("Let's first look at what a random solution would give. You can click on ",
                          em('New Random Solution'), ' on the right to see a pattern of trajectory the salesman
                          would take if he chose randomly which city he would go to after the city he is right now.'),
                        column(12,br()),
                        column(12,div(actionBttn(inputId = "optim_genrand",label = "New Random Solution", 
                                                 style = "fill", color = "danger",icon = icon("redo")), 
                                      style="text-align: center;")),
                        column(12,br()),
                        p('We can visually see that the solution is far from being optimal. The postman seems to travel
                          the whole area several times.')),
                 column(6,
                        column(12,br()),
                        column(12,plotlyOutput('optim_random')))),
          column(12,br()),
          column(12,
                 column(6,withSpinner(plotlyOutput('optim_distrand'))),
                 column(6,textOutput("optim_randmean"),
                        p("Let's see if we can do better with some more optimal algorithms."))),
          column(12,hr()),
          subtitle_('Greedy Algorithm'),
          column(12,
                 column(6,offset = 0, style='padding:0px;',
                        column(12,br()),
                        p("The greedy algorithm consists in finding local optimum at each step without considering the overall global optimum.
                          In our case, that would mean that at each step, the salesman would look for the closest city of the city he is actually in.
                          Let's look at what it would look like by clicking on ", em('Launch Greedy Algorithm')),
                        column(12,br()),
                        column(12,
                               div(actionBttn(inputId = "optim_lgreed",label = "Launch Greedy Algorithm", 
                                              style = "fill", color = "danger",icon = icon("redo")), 
                                   style="text-align: center;")),
                        column(12,br()),
                        textOutput("optim_greedtext"),
                        p('However, if we look at the path the traveller is taking, we can see that we can still do better.')),
                 column(6,
                        column(12,br()),
                        column(12,plotlyOutput('optim_greedy')))
                 ),
          column(12,hr()),
          subtitle_('Simulated Annealing'),
          column(12,
                 p('The simulated annealing method is a probabilistic method for approximating the global optimum of a function
                   (which is in our case the minimum of the distance travelled) inspired by a process used in metallurgy. Basically what we will do is: '),
                 tags$ul(
                   tags$li("Start from a random solution that will evolve at each iteration"),
                   tags$li("Introduce a fictive temperature T that will decrease at each iterations with a coefficient \\(\\lambda\\) : \\(\\ T_{i+1} = \\lambda*T_i\\) "),
                   tags$li("Permute randomly some trajectories of the full path :"),
                   tags$ul(
                     tags$li("If the distance of the new path\\(\\ D_{new}\\) is smaller than the old path\\(\\ D_{old}\\), we will update the path"),
                     tags$li("Otherwise, we will decide stochastically (if\\(\\ e^{- \\frac{D_{new}-D_{old}}{T}} > \\alpha \\)) with \\(\\alpha\\) 
                             a random number between 0 and 1, we will update it anyway. 
                             This step allows to not be stuck in a local minimum.
                             As T is decreasing at each iteration, this will be less and less likely to happen.")
                     ),
                   tags$li("At each step, we will keep in memory the best solution  \\(\\ D_{optimal}\\) we had and update it if we found a better one.")),
                 p('Normally, the initial temperature \\(\\ T_0\\) and the decreasing coefficient\\(\\lambda\\)
                   should be hyperparameters that we need to tune but for this example,
                   we will just set \\(\\ T_0 = 10 000 \\) and \\(\\lambda = 0.998\\) ')
                 ),
          column(12,br()),
          column(12,br()),
          column(12,
                 column(2,
                        h3('Control Panel'),
                        boxPad(color = 'gray',
                               numericInput("optim_iter","Maximum", value = 25000,
                                            min = 5000,max = 100000, step = 5000),
                               radioButtons("optim_display","Steps", selected = 100,
                                            choices = c(1,10,100,1000),inline = FALSE),
                               br(),
                               actionBttn(inputId = "optim_genanneal",label = "Launch Simulation", 
                                          style = "fill", color = "danger",icon = icon("redo"),size = 'sm'))),
                 column(6,plotlyOutput('optim_anneal')),
                 column(4,plotlyOutput('optim_annealerr'))
          ),
          column(12,br()),
          column(12,
                 p('We can see that each time, after a good number of iterations, we manage to find a better solution than the greedy algorithm. 
                   However, most of the time, we can only approximate the global minimum.')),
          width = 12)
          )))