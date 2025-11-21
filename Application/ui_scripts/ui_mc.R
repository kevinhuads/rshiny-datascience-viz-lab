tab_mc= tabItem(
  tabName = "MonteCarlo",
  fluidPage(
    fluidRow(
      box(column(12,h1(tags$b("Monte Carlo Simulation"),': Evaluating the value of Pi')),
          intro('Monte Carlo methods are a family of computational algorithms that aims at computing a numeric result with random samplings. 
                Their essential idea is using randomness to solve problems that might be deterministic in principle.
                These methods are used by professionals in a widely disparate fields as project management, energy, 
                engineering, research and development, insurance, transportation or even in finance.'),
          column(12,
                 hr(),
                 p("The goal of this session is to evaluate \\(\\pi\\), one of the most famous mathematical constant of the world defined as
                   the ratio between the circumference of a circle and its diameter. Let's consider a circle of radius r = 1 inside a square of length l = 2, 
                   the respective areas are defined by"),
                 p(withMathJax("$$Area_{circle} = r\\pi = \\pi$$")),
                 p(withMathJax("$$Area_{square} = l*l = 2*2 = 4$$")),
                 p('This definition will allow us to evaluate the value of \\(\\pi\\) by the method of Monte Carlo.
                   We will simulate N (to be defined) points for which the coordinates x and y will follow 
                   an uniform distribution from -1 to 1 (the point (0,0) being the center of both the circle and the square).'),
                 p('All the generated points will necessarily be located in the square. If they are inside the circle, we will flag them in red, otherwise, in blue.
                   Thus, the area of the circle corresponds to the count of red points and the area of the square corresponds the total count of points (blue and red).
                   Hence for each iteration n, we have the following relation:'),
                 p(withMathJax("$$\\frac{Area_{circle}}{Area_{square}} = \\frac{Red \\, Points}{Red \\, Points + Blue \\, Points} = \\frac{\\widehat{\\pi} (n)}{4}$$")),
                 p('We put a hat on the pi to emphasize the fact that it is only an estimation.
                   We can then estimate for value of pi by '),
                 p(withMathJax("$$\\widehat{\\pi} (n) = 4 * \\frac{Red \\, Points}{Red \\, Points + Blue \\, Points}  $$")),
                 p('And the error at the iteration n will be defined by the absolute value of the difference between the estimated value and the value of pi saved by 
                   R that we will consider as the benchmark:'),
                 p(withMathJax("$$error(n) =  |\\widehat{\\pi}  (n)- \\pi|$$")),
                 hr()),
          column(12,
                 column(3,
                        h3('Control Panel'),
                        boxPad(color = 'gray',
                               numericInput("mc_iter","Stop at", value = 2500,
                                            min = 2500,max = 5000, step = 500),
                               radioGroupButtons(
                                 inputId = "mc_display",label = "Steps", 
                                 choices = c(10,25,50),
                                 selected = 25,
                                 individual = TRUE,
                                 size = 'normal',
                                 direction = 'horizontal',
                                 checkIcon = list(
                                   yes = tags$i(class = "fa fa-circle",style = "color: steelblue"),
                                   no = tags$i(class = "fa fa-circle-o",style = "color: steelblue"))
                               ),
                               radioGroupButtons(
                                 inputId = "mc_choice",label = "Display",
                                 choices = c('Estimated Value','Error'),
                                 individual = TRUE,
                                 size = 'normal',
                                 direction = 'vertical',
                                 checkIcon = list(
                                   yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
                                   no = tags$i(class = "fa fa-circle-o", style = "color: steelblue"))
                               ),
                               br(),
                               div(actionBttn(inputId = "mc_launch",label = "Simulate", size = 'md',
                                              style = "fill", color = "danger",icon = icon("redo")), 
                                   style="text-align: center;"))
                 ),
                 
                 column(4,plotlyOutput('mc_circle')),
                 column(5,plotlyOutput('mc_pi_value')),
                 hr()),
          column(12,
                 valueBoxOutput('mc_pi_iterbox',width = 4),
                 valueBoxOutput('mc_pi_redbox',width = 4),
                 valueBoxOutput('mc_pi_valuebox',width = 4)
          ),
          width = 12),
      box(column(12,
                 h2('Convergence Speed'),
                 hr(),
                 column(6,
                        p("We can't really visualise more points as it would make the plot too heavy,
                          but now that you get the idea, we can simulate without visualisation."),
                        p("Naturally, the error should decrease with the number of iterations.
                          If we had an infinite number of points, the whole circle and the whole square would be covered
                          and we would have the exact value of pi."),
                        p('The algorithm complexity is a domain that studies the amount of resource we should provide 
                          for a certain degree of performance. In other words, in our case, how many iterations do we need to get
                          an accurate value? (first, we should know : how accurate we want to model to be?)')),
                 column(2,
                        boxPad(color = "gray",
                               selectInput('mc_novisu','Number of Iterations',choices = 10^(1:6),
                                           selected = 10^5),
                               column(12,br()),
                               div(actionBttn(inputId = 'mc_novisugo',label = "Simulate", size = 'md',
                                              style = "fill", color = "danger",icon = icon("redo")), 
                                   style="text-align: center;"),
                               column(12,br()))),
                 column(4,uiOutput('mc_simul_box'))
                        ),
          column(12,
                 hr(),
                 column(6,highchartOutput('mc_complexity')),
                 column(6,p('If we simulate 1000 iterations 5000 times, we have an error average of 0.0132,
                            if we simulate 10000 iterations 5000 times, we have an error average of 0.0042.'),
                                      p('The plot on the left shows the average error given the number of iterations on the 
                                        decimal logarithm scale. (For example, for \\(10^3 = 1000\\) iterations, the average error is
                                        around \\(10^{-1.88} = 0.0132\\))'),
                                      p('What we can see is that the relation between the logarithms seems almost perfectly linear 
                                        with a slope of -0.5 and an intercept of 0.12. Thus, if the logarithm of the number of iterations 
                                        increase by 1, the average error will decrease by 0.5.'),
                                      p('In other words, it means that if we multiply the number of iterations by 100 (increase the log by 2),
                                        the average error will by divided by 10 (decrease by 1). Thus, if n is the number of iterations, we can write it by :'),
                                      p(withMathJax("$$log_{10}(error(n)) \\sim O(-0.5 * log_{10}(n))$$")),
                                      p('Which can be replaced by :'),
                                      p(withMathJax("$$error(n) \\sim O(\\frac{1}{\\sqrt(n)})$$")),
                                      p('Happily we find the result given in theory by the ',
                                        a(href="https://en.wikipedia.org/wiki/Central_limit_theorem", "Central Limit Theorem", target="_blank"),
                                        ' and the ',
                                        a(href="https://en.wikipedia.org/wiki/Berry–Esseen_theorem", "Berry–Esseen Theorem", target="_blank")))),
                        column(12,
                               hr(),
                               p('This technique is a very slow way to compute pi as it needs many iterations to get an accurate value
                                 but this usecase allows us to understand easily how Monte Carlo methods work. 
                                 The Monte Carlo methods have the advantages of being quite simple and straightforward as long as 
                                 the convergence can be guaranteed by the theory and can provide approximate solution to many mathematical problems.')),
                        width = 12)
                    )))