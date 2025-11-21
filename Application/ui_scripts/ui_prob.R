tab_prob = tabItem(tabName = "PS",
                   fluidPage(
                     fluidRow(
                           box(column(12,offset = 0, style='padding:0px;',
                                      column(12,h1(tags$b("Probabilities and Statistics"),': Checking whether a coin is fair'))
                                  # column(3,
                                  #        radioGroupButtons(
                                  #          inputId = "coins_dice",label = NULL, 
                                  #          choices = c("<i class='fa fa-coins'></i> Coins" = "coins", "<i class='fa fa-dice'></i> Dices" = "dices"),
                                  #          justified = TRUE, status = 'primary'
                                  #        ))
                                  ),
                               
                           intro("Probabilities measure of the likelihood that an event will occur. A probability is a number always between 0 and 1 
                                 where 0 indicates impossibility and 1 indicates certainly. Statistics is a branch of mathematics dealing with data collection, organization, analysis, interpretation and presentation. 
                                 In this section, we will focus on the part of statistics that studies distribution and statistical tests.
                                 We will see different approach to detect a biased coin"),
                           width = 12),
                           box(NULL,
                           column(12,
                                  # h3("Probability-Oriented Approach"),
                                  column(12,br()),
                                  p("In this example, we have 1000 coins and one of them is ",em("biased ")," (having a probability p=1 of getting a head).
                                    All the other coins will be called ",tags$em("fair "),"(or unbiased) coin.
                                    A set of n throws of the coin follow a ",
                                    a(href="https://en.wikipedia.org/wiki/Binomial_distribution", "Binomial Distribution", target="_blank"),
                                    "\\(B(n,p)\\)","where p should be equal to 0.5 for a fair coin and is equal to 1 for our biased coin here.
                                    Let's take randomly one of these coins and throw it n = 10 times and suppose we get k = 10 heads in a row, 
                                    what is the probability that we have chosen the biased coin?"),
                                  p("To compute it, let:"),
                                  tags$ul(
                                    tags$li("N be the total number of coins"), 
                                    tags$li("M be the number of biased coins"),
                                    tags$li("k be the number of times we throw the coin"),
                                    tags$li("X be the event 'The coin is biased'"),
                                    tags$li("Y be the event 'We got k consecutive times head'")
                                  ),
                                  p('The Kolmogorov definition gives:'),
                                  div(withMathJax("$$P(X \\cap Y) = P(X|Y)*P(Y) = P(Y|X)*P(X)$$"), style="text-align: center;"),
                                  p("What we want is the probability of the coin being biased given we got head k times in a row, hence P(X|Y)"),
                                  div(withMathJax("$$P(Biased Coin|k \\; heads) = P(X|Y) = \\frac{{P(Y|X)*P(X)}}{{P(Y)}} = \\frac{{P(k \\; heads|Biased Coin)*P(Biased Coin)}}{{P(k \\; heads)}}$$"), 
                                      style="text-align: center;"),
                                  p('We know that :'),
                                  tags$ul(
                                    tags$li("For the biased coin, the probability of getting a head k consecutive times is p^k"), 
                                    tags$li("The probability of getting the biased coin is m = M/N"),
                                    tags$li("The probability of getting k heads in total is the weighted sum of the probabilities of getting k heads for both the biased coins (0.5^k) and unbiased coins (p^k)")
                                  ),
                                  div(withMathJax('$$P(Biased Coin|k \\; heads) = \\frac{{m*p^k}}{{m*p^k + (1-m)*0.5^k}} = 
                                                  \\frac{{1}}{{1 + \\left(\\frac{{1-m}}{{m}}\\right)*\\left(\\frac{{0.5}}{{p}}\\right)^k}}$$'), 
                                      style="text-align: center;"),
                                  hr()
                                  ),
                           column(12,offset = 0, style='padding:0px;',
                                  column(6,
                                         p('If we replace with the values in the introduction, we have:'),
                                         withMathJax('$$P(Biased Coin|10 \\; heads) = \\frac{{1}}{{1 + \\left(\\frac{{1-0.001}}{{0.001}}\\right)*\\left(\\frac{{0.5}}{{1}}\\right)^{10}}} = 50.62 \\%$$'),
                                         p("Isn't it surprising? It means that even if we get 10 times head in a row, we only have 1 chance over 2 to have chosen the biased coin."),
                                         p('In fact, at this point, every throw matters, one head more or one head less could completely change the odds: 
                                           If we look at the figure on the right, we can see that for 9 heads, there is only 33.88% chance that we took the biased coin whereas there is 67.21% for 11 throws. 
                                           (Thus a difference of 33.33% for 2 more throws!'),
                                         p('This can be very counter intuitive as the difference is much more neglictible for small number of heads (only 1.18% difference between 2 (0.40%) and 4 (1.58%) throws) 
                                           and big numbers (only 1.12% difference between 16 (99.50%) and 18 (99.62%) throws)'),
                                         p('The widgets on the right allows you to play a little bit more with the parameters.
                                           If the probability p of having a head for the biased coin decreased, it becomes harder to detect it as it behaviour gets closer to any unbiased coin
                                           (it becomes totally unbiased for p = 0.5). Of course the trend reverses totally when p is lower than 0.5.')),
                                  column(6,
                                         column(12,
                                                column(4,sliderTextInput(
                                                       inputId = "coins_p",label = "Probability p", 
                                                       choices = seq(from = 0,to = 1,by = 0.01),
                                                       selected = 1,grid = FALSE)%>%
                                                         shinyInput_label_embed(
                                                           shiny_iconlink() %>%
                                                             bsplus::bs_embed_popover(
                                                               title =  "Probability for the biased coin to get a head", placement = "left"
                                                             )
                                                         )),
                                                column(4,sliderTextInput(
                                                  inputId = "coins_m",label = "Number N", choices = c(10,100,1000,10000,100000),
                                                  selected = 1000,grid = FALSE)%>%
                                                    shinyInput_label_embed(
                                                      shiny_iconlink() %>%
                                                        bsplus::bs_embed_popover(
                                                          title =  "Total number of coins (considering that we always have 1 biased coin)", placement = "left"
                                                        )
                                                    )),
                                                column(4,sliderTextInput(
                                                  inputId = "coins_k",label = "X Range", 
                                                  choices = seq(from = 20,to = 200,by = 20),
                                                  grid = FALSE)%>%
                                                    shinyInput_label_embed(
                                                      shiny_iconlink() %>%
                                                        bsplus::bs_embed_popover(
                                                          title =  "Maximum number of throws to analyse", placement = "left"
                                                        )
                                                    ))),
                                         column(12,plotlyOutput('coins_throws'))
                                         )),
                           width = 12),
                       box(NULL,
                           # column(12,h3("Statistics-Oriented Approach")),
                           column(12,br()),
                           column(12,offset = 0, style='padding:0px;',
                                  column(8,p("Let's take the problem the other way around : we are going to throw the coin several times and determine statistically
                                  if we had chosen a biased coin or not. For a set on",tags$em("n "),"throws, the expected value of the averages of successes (getting a head) is p (p=0.5 for a fair coin).
                                  The ",em("Law of Large Numbers")," says that the bigger is the number of throws n, the closer we will get to the expected value. (which is here the probability of the coin)
                                     (You can try some simulations with the box at the right to verify it)")),
                                  column(4,
                                         column(6,
                                                boxPad(color = 'gray',
                                                       sliderTextInput(
                                                         inputId = "coins_simul_p_1",label = "Probability p", 
                                                         choices = seq(from = 0.01,to = 0.99,by = 0.01),selected = 0.5,grid = FALSE)%>%
                                                         shinyInput_label_embed(
                                                           shiny_iconlink() %>%
                                                             bsplus::bs_embed_popover(title =  "Probability for the biased coin to get a head", placement = "left")
                                                         ),
                                                       sliderTextInput(
                                                         inputId = "coins_simul_n_1",label = "Number of throws n", choices = c(10,50,100,500,1000,5000,10000,50000,100000),
                                                         selected = 1000,grid = FALSE)%>%
                                                         shinyInput_label_embed(
                                                           shiny_iconlink() %>%
                                                             bsplus::bs_embed_popover(title =  "Total number of throws the coin in one single set", placement = "left")
                                                         ),
                                                       div(actionBttn(inputId = "coins_simul_1",label = "Simulate Again", size = 'sm' ,
                                                                      style = "fill", color = "danger",icon = icon("redo")), 
                                                           style="text-align: center;"))),
                                         column(6,uiOutput('coins_simul_box_1')))),
                                 column(12,hr()),
                                 column(12,
                                        p("In practice, we don't need to throw the coin thousands of time to see if a coin is biased or not.
                                          Let's say we want to do n throws, the probability of getting k heads follows a Binomial distribution and is given by the formula:"),
                                        div(withMathJax('$$P(k \\; heads) = \\binom{n}{k} * p^k * (1-p)^{n-k}$$'),style="text-align: center;"),
                                        p("For example, if we throw a fair coin n = 10 times, the probability of getting 5 heads (independantly of the order) will be :"),
                                        div(withMathJax('$$P(5 \\; heads) = \\binom{10}{5} * 0.5^5 * (1-0.5)^{10-5} = 252* 0.5^{10} = 24.61 \\% $$'),style="text-align: center;"),
                                        p('We want to test if the coin is biased towards heads, which means, by definition that we expect to have more heads than tails. 
                                          we usually use ',strong('Statistical Tests'),' which are different tools that allow us to quantify how close our data are to our expectations or theories.'),
                                        p('In this case, a ',strong('one tail test'),' would be the best option: We will set as null hypothesis ',
                                          strong('\\(H_0\\) : \\(\\mu\\)  = 0.5'),' which means that we should get 50% of heads when we throw the coin. 
                                          We can guess that the further we are from this value, the easier it will be to state that the coin is biased.
                                          To measure how confident we are about the status of the coin, we use the ',strong('p-value'), 'which measures the probability
                                          of having a value as extreme as the one we got, given the null hypothesis.')
                                        ),
                                 column(12,br()),
                           column(12,hr()),
                           column(12,offset = 0, style='padding:0px;',
                                 column(8,offset = 0, style='padding:0px;',
                                   column(3,
                                          boxPad(color = 'gray',
                                                 radioGroupButtons(
                                                   inputId = "coins_tail_test",
                                                   label = "Test",
                                                   choices = c("One tail","Two tails"),
                                                   justified = TRUE,
                                                   checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                                                 ),
                                                 sliderTextInput(
                                                   inputId = "coins_tail_threshold",label = "p-Value Threshold", 
                                                   choices = c(0.001,0.01,0.05,0.1),selected = 0.05,grid = FALSE)%>%
                                                   shinyInput_label_embed(
                                                     shiny_iconlink() %>%
                                                       bsplus::bs_embed_popover(title =  "If the p-value obtained is lower than this threshold, the coin will be considered as biased", placement = "left")
                                                   ),
                                                 sliderTextInput(
                                                   inputId = 'coins_tail_n', label = 'Number of throws n',  choices = c(10,25,50,100,150,200,500),
                                                   selected = 50,grid = FALSE)%>%
                                                   shinyInput_label_embed(
                                                     shiny_iconlink() %>%
                                                       bsplus::bs_embed_popover(title =  "Number of throws", placement = "left")
                                                   ),
                                                 sliderTextInput(
                                                   inputId = 'coins_tail_p', label = 'Probability p',  choices = seq(0.5,1,by = 0.01),
                                                   selected = 0.75,grid = FALSE)%>%
                                                   shinyInput_label_embed(
                                                     shiny_iconlink() %>%
                                                       bsplus::bs_embed_popover(title =  "Probability p of the biased coin", placement = "left")
                                                   ),
                                                 div(actionBttn(inputId = "coins_tail_simul",label = "Throw the coin", size = 'sm',
                                                                style = "fill", color = "danger",icon = icon("redo")), 
                                                     style="text-align: center;"),
                                                 column(12,br())),
                                          uiOutput('coins_tail_box')
                                   ),
                                   column(9,highchartOutput('coins_tail'))
                                 ),
                           column(4,
                                  column(12,br()),
                                  p('A one-tail test measures the likelihood of our coin to be biased towards heads while a two-tails test
                                      measures the likelihood towards either heads or tails and is thus less powerful in our case'),
                                  p('We generally chose a threshold \\(\\alpha\\) (usually 0.05) and we consider that the coin is biased if we obtain a p-value lower than this threshold.
                                    We represented in the plot at the left this threshold with grey plotband(s).'),
                                  p("If the threshold is too big, we don't give our coin enough flexibility as our rule becomes too strict
                                    If a fair coin is flagged as biased, we say it is a ",strong('false positive'),"(or Type I Error)"),
                                  p('On the other hand, if the threshold is too small, we might have difficulties flagging biased coins as it would need very extreme value
                                    If a biased coin is not flagged, we say it is a ',strong('false negative'),'(or Type II Error)'))
                           ),
                           column(12,hr()),
                           column(12,offset = 0, style='padding:0px;',
                                  column(6,
                                         column(12,br()),
                                         p("For each set of probabilities and p-value thresholds \\(\\alpha\\), we simulated 100000 times and computed the frequency of the coin being flagged as
                                           biased given \\(\\alpha\\) (hence the probability sum being lower than \\(\\alpha\\)). Ideally, we would like this frequency to be equal to 0 for p = 50% 
                                           and to be equal to 100% for every probability strictly greater than 0.5."),
                                         p("However, this is impossible in practice: We can easily understand that the smaller the bias is, the harder it will be to flag it.
                                           We can also intuitively guess that increasing the number of throws will also increase the confidence of the conclusion (due to the Law of Large Numbers).
                                           Thus, we can see that for 500 throws, we can really easily flag any coin with a biased greater than 64% whereas it is already 
                                           hard to flag a biased coin with a probability of 90% if we choose a \\(\\alpha\\). (only 53.2% of coins flagged for a \\(\\alpha\\) = 0.0001)."),
                                         p("Moreover, we can see that having a high \\(\\alpha\\) can help us flagging more easily biased coins, but also rises the risk of
                                           unfairly flag a fair coin as biased. In fact, for a \\(\\alpha\\) of 0.1, we have 2.175% of false positive for N = 25 throws and 8.375% for N = 500 throws.
                                           Usually, we would want to pick a smaller p-value threshold as the number of throws increases. This could be interpreted as we need to be as strict as our model allows us to.")),
                                  column(6,
                                         column(12,boxPad(color = 'gray',
                                                radioButtons(inputId = 'coins_prob_n', label = 'Number of throws n',  choices = c(10,25,50,100,150,200,500),
                                                             selected = 100,inline = TRUE))),
                                         highchartOutput('coins_prob'))),
                           width = 12)
                       # box(column(12,h1(tags$b("Going Further")),
                       #            p("Now that we have a general idea about the results for one set of n throws,what would happen if we threw several times several sets? 
                       #              This is where the ", em("Central Limit Theorem")," intervene, it basically claims that the sum of independant variables 
                       #              will tend to a normal distribution as the size (the number of sets N) grows. 
                       #              You can see with the plot below that if you increase the number of times N we execute the set, the shape looks more and more like the ", em("bell curve"),
                       #              "of the normal distribution."),
                       #            p("To see how close our distribution is close to the normal distribution, the test that will be used here is a ",strong("Normality test"),
                       #              " (or Shapiro Wilk test) for which the null hypothesis will be",
                       #              strong(" \\(H_0\\) : The sample comes from a normal distribution"), " and the statistic will be :"),
                       #            withMathJax('$$W = \\frac{{(\\sum_{i=1}^{N}a_{i}x_{i})^{2}}}{{(\\sum_{i=1}^{N}x_{i} - \\bar{x})^{2}}} $$')%>%
                       #              p('Where : '),
                       #            tags$ul(
                       #              tags$li("\\(x_i\\) are the sorted random sample values"), 
                       #              tags$li("\\(a_i\\) are constants generated from the covariances, variances and means of the sample (size N) from a normally distributed sample."),
                       #              tags$li("\\(\\bar{x}\\) is the average of the sample")
                       #            ),
                       #            p("Theoretically, if \\(H_0\\) is true and our sample is normally distributed, the statistic W will be very close to 1. But in practice, it is always smaller."),
                       #            p("Here, the p-value will be computed by the formula \\(p= P(W< W_\\alpha|H_0)\\)  
                       #              which is the probability that W is smaller than a critical value \\(W_\\alpha\\) 
                       #              (depending on N and \\(\\alpha\\)) given \\(H_0\\) such as:"),
                       #            tags$ul(
                       #              tags$li("If \\(p< \\alpha\\), the probability is too small to be due to random, we will reject \\(H_0\\) and take the alternative hypothesis",
                       #                      "\\(H_a\\) :",em("The sample does not come from a normal distribution")), 
                       #              tags$li("If \\(p> \\alpha\\), we won't be able to reject with certainty \\(H_0\\) so we will keep this hypothesis")
                       #            )),
                       #     column(12,hr()),
                       #     column(12,offset = 0, style='padding:0px;',
                       #            column(7,
                       #                   fluidRow(
                       #                     column(3,
                       #                            boxPad(color = 'gray',
                       #                                   sliderTextInput(
                       #                                     inputId = "coins_simul_p",label = "Probability p", 
                       #                                     choices = seq(from = 0.01,to = 0.99,by = 0.01),selected = 0.5,grid = FALSE)%>%
                       #                                     shinyInput_label_embed(
                       #                                       shiny_iconlink() %>%
                       #                                         bsplus::bs_embed_popover(title =  "Probability for the biased coin to get a head", placement = "left")
                       #                                     ),
                       #                                   sliderTextInput(
                       #                                     inputId = "coins_simul_n",label = "Number of throws n", choices = c(10,50,100,500,1000,5000,10000,50000,100000),
                       #                                     selected = 1000,grid = FALSE)%>%
                       #                                     shinyInput_label_embed(
                       #                                       shiny_iconlink() %>%
                       #                                         bsplus::bs_embed_popover(title =  "Total number of throws the coin in one single set", placement = "left")
                       #                                     ),
                       #                                   sliderTextInput(
                       #                                     inputId = "coins_simul_N",label = "Number of sets N", choices = c(10,50,100,500,1000,5000,10000,50000,100000),
                       #                                     selected = 1000,grid = FALSE)%>%
                       #                                     shinyInput_label_embed(
                       #                                       shiny_iconlink() %>%
                       #                                         bsplus::bs_embed_popover(title =  "Total number of sets", placement = "left")
                       #                                     )),
                       #                            column(12,br()),
                       #                            uiOutput('coins_simul_box')
                       #                     ),
                       #                     column(9,
                       #                            withSpinner(highchartOutput('coins_hist')),
                       #                            column(12,br()),
                       #                            column(5,div(actionBttn(inputId = "coins_simul",label = "Simulate Again", 
                       #                                                    style = "fill", color = "danger",icon = icon("redo")), 
                       #                                         style="text-align: center;")),
                       #                            column(6,
                       #                                   radioGroupButtons(
                       #                                     inputId = "coins_simul_plot",
                       #                                     label = NULL, 
                       #                                     choices = c("<i class='fa fa-bar-chart'></i> Histogram" = "bar", 
                       #                                                 "<i class='fa fa-line-chart'></i> QQ-plot" = "line"),
                       #                                     justified = TRUE
                       #                                   )))
                       #                   )),
                       #            column(5,
                       #                   p("We also gave the possibility to display the ", strong('QQ Plot '),
                       #                     "which can help to check the assumption of normality (or not) of our distribution
                       #                     by plotting the Quantiles of our sample with the ones of a normal distribution that would have the same mean and standard deviation."),
                       #                   p("This means, the closer our dataset will be to a normal distribution, the more the QQ plot will look like a straight line.
                       #                    Thus, it is important to note that the QQ plot is only a visual tool without any mathematic justification."))),
                       #     
                       #     width = 12)
                           )))
