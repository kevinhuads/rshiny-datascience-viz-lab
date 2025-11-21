tab_reg = tabItem(
  tabName = "Regression",
  fluidPage(
    fluidRow(
      box(
        column(12,h1(tags$b("Regression"),': Predicting the number of bikes rent')),
        intro('Regression is a branch of supervised machine learning where the goal is to predict a continuous output given a set
              of continuous or qualitative variables by training a model on data where we already know the output.'),
        column(12,hr()),
        subtitle_('Descriptive Analysis'),
        column(12,
           #https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset#
          p('The ',actionLink("bikes_tabBut", "dataset"), 'comes from',em("UC Irvine Machine Learning Repository.")," and were collected during the years of 2011 and 2012",
            'It contains the number of bikes hourly rented with the temperature, the temperature felt, the wind speed, the humidity, the season, the weather, and if it was a working day and/or a holiday.',
            'In this section, the final goal will be to predict the number of bikes that will be rent in the future given these variables.'),
          p('We want to first look at our output, its distribution and if it has any outlier. As the data are hourly collected, it might also be interesting to look at the average count of rents per hour.'),
          bsModal("bikes_modal", "Bikes Data Table", "bikes_tabBut", size = "large",
                  withSpinner(dataTableOutput("bikes_table")))),
        column(12,
          column(6,
                 highchartOutput('bikes_dist'),
                 awesomeRadio('bikes_histdist',NULL,choices = c('Histogram','Density'),selected = 'Histogram',inline = TRUE),
                 p('We notice that the outcome follows a distribution Gamma, which is very common for a variable that represents a count over a given time.')),
          column(6,
                 highchartOutput('bikes_hour'),
                 prettyCheckbox('bikes_polar', 'Polar Plot',value =FALSE,icon = icon("check"),animation = "rotate",status = 'primary'),
                 p('The average is particularly high at 8am and at 6pm which correspond to the rush hours. 
                        We can also notice that the average is especially low during the night (between 1am and 5am), which is not very surprising.'))
        ),
        column(12,hr()),
        column(12,p('The next step is to look at the impact of each feature on the outcome, it is important to differentiate the continuous features to the categorical ones as they will not
                    always be treated equally. Here, we have 4 categorical features and 4 continuous features.'),
               tags$ul(
                 tags$li("If the variable is categorical, we will look at the distribution of the count per category"), 
                 tags$li("If the variable is continuous, we will look at the average count per unit")
               ),
               br()),
        column(12,
               column(3,
                      radioButtons("bikes_rad", "Feature type:",
                                   c("Categorical" = "quali",
                                     "Continuous" = "quanti")),
                      uiOutput('selectbikesUI')),
               column(9,plotlyOutput('bikes_chosen'))),
        column(12,hr()),
        subtitle_('Correlation'),
        column(12,
               column(7,
                      br(),
                      p('Correlation is a very important notion in machine learning at it shows linear relationship between two features.
                        Basically, a coefficient of correlation between two features is high when they both increase together.'),
                      p('Before computing any machine learning algorithm, it is important to know if some features are correlated 
                        as it might affect consequently the results (even though some models are less affected than others)'),
                      
                      p('Obviously, what we observe here is that, whatever the coefficient we use, the temperature felt like is really correlated with the real temperature.
                        For the machine learning part, we will remove the temperature felt like.')),
               column(5,align="center",
                      highchartOutput('bikescor'))),
        width = 12),
      box(subtitle_('Machine Learning'),
          column(12,
                 p('In the Machine Learning part, we will try different algorithms and compare their performance.'),
                 p('We want to predict the number of bikes, which is a continuous variable, our model will be a regression model. 
                   To measure the performance of a regression model, we can compute the deviance, which tells how far our predictions are from the real values. 
                   In the case of a Gamma distribution, the deviance is given by:'),
                 p(withMathJax("$$Dev_{model} = \\frac{1}{N}\\sum_{n=1}^{N} weight_i[log(\\frac{predicted_i}{observed_i}+\\frac{observed_i}{predicted_i}-1)]$$")),
                 p('We can also use a normalized version of the deviance which is called error reduction ratio given by the expression: '),
                 p(withMathJax("$$Er = 1- \\frac{Dev_{model}}{Dev_{null}}$$")),
                 p('Thus, this error measures how better our model is compared to the null model (which predict the average all the time). If it is close to 1, our model is very efficient'),
                 p('The null model is a benchmark and will help us to see if our model does good or not.'),
                 # valueBoxOutput('bikes_null'),
                 hr(),
                 p('We will briefly present and compare the machine learning algorithms that are currently the most used for regression:'),
                 tags$ul(
                   tags$li("Generalized Linear Model"), 
                   tags$li("K Nearest Neighbors"),
                   tags$li("Support Vector Machine"), 
                   tags$li("Regression Tree"),
                   tags$li("Random Forest"), 
                   tags$li("Boosted Trees")
                 ),
                 p('For each model, we have separated the dataset into three datasets:'),
                 tags$ul(
                   tags$li("A training set (60%) : to train the model"), 
                   tags$li("A validation set (20%) : to tune the hyperparameters (if needed)"),
                   tags$li("A test set (20%) : to give the final results")
                 ),
                 hr()),
          column(12,(h3('Training on the training set and Tuning on the validation set'))),
          tabsetPanel(
            id = "bikes_tabs",
            tabPanel("Generalized Linear Model", 
                     br(),
                     column(12,
                            column(5,
                                   p('The Generalized Linear Model (GLM) is an algorithm where you try to fit the outcome \\(Y\\) with a linear predictor \\(\\beta X\\) and possibly a link function \\(g\\) :'),
                                   p("$$E(Y) = g^{-1}(\\beta X)$$"),
                                   p('Where \\(E(Y)\\) is the expected value of \\(Y\\)'),
                                   p('The GLM does not need hyperparameters to tune but the problem is that it only takes in count linear relationships between continuous features and outcome.'),
                                   p('However, as we saw in the descriptive analysis, most of the relationships are more complex than linear. 
                                     To overcome this, we can add square term for continuous features'),
                                   p('Since we noticed that the relationship of the outcome with the features "hour" is very complex, we will keep this features as categorical'),
                                   selectInput('bikes_lmchoice',label = NULL,choices = c('linear','square')),
                                   valueBoxOutput('bikes_lm_results',width = 12),
                                   p('Thus, we can notice that all features are significant since the p-value of the Student t-tests are very close to zero.
                                     Beside, Adding the square terms improves the performance by 1.8%.'),
                                   p('However, with the square terms, the speed of the wind is not significant anymore with the square terms')),
                            column(7,verbatimTextOutput("bikes_glm_summary")))
                            ),
            tabPanel("K Nearest Neighbors",
                     br(),
                     column(12,
                            column(5,
                                   p('The K-Nearest Neighbors (KNN) regression is an algorithm where the input consists of the k closest training examples 
                                     in the feature space and the output is the average of the values of its k nearest neighbors'),
                                   p('For this algorithm, we had to transform all of our data into continuous variable. 
                                     There are two parameters to tune for the KNN algorithm: The number of neighbours (K) and the algorithm: KD Tree, Cover Tree and Brute Force'),
                                   valueBoxOutput('bikes_knn_results',width = 12),
                                   p('The 3 algorithms give pretty similar results. They all reach their maximum performance for K = 3.')),
                            column(7,plotlyOutput('bikes_knn')))
                            ),
            tabPanel("Support Vector Machine",
                     br(),
                     column(12,
                            p('In classification, the Support Vector Machines (SVM) algorithm consists in constructing a (set of) hyperplane(s) 
                              in a high dimensional space that would separate the classes with the largest margin(s). 
                              As if often happens that the set to discriminate are not linearly separable in that space, 
                              the kernel functions have been introduced to map the original space into another one where the separation is easier.
                              Among these kernel functions, there are: linear, polynomial, radial and sigmoid'),
                            p('The regression version of SVM is called', em('Support Vector Regression (SVR)'),
                              'Even though the principle is the same (minimizing error, individualizing the hyperplane(s) that maximize the margins...),
                              the algorithm is quite different. In fact, there are two types of SVR:'),
                            tags$ul(
                              tags$li(p("The \\(\\epsilon\\)-regression where the goal is to find a function \\(f(x)\\) that has at most \\(\\epsilon\\) deviation 
                                        from the actually obtained targets \\(y_i\\) for all training data")), 
                              tags$li(p("The \\(\\nu\\)-regression where the \\(\\nu\\) parameter control the amount of support vectors in the resulting model"))
                              ),
                            br(),
                            column(5,
                                   p('The table below shows the best performance for both algorithms \\(\\epsilon\\) and \\(\\nu\\) and for each kernel.
                                     The figure on the right shows the gridsearch for each of them'),
                                   dataTableOutput('bikes_svmdf'),
                                   hr(),
                                   p("It seems that, for a given kernel (except the linear one), the type of SVR doesn't change much the performance.
                                     The Radial Kernel is the one which performs the best here with 78.29%. ")
                                   # valueBoxOutput('bikes_svm_results',width = 12)
                                   ),
                            column(7,
                                   column(12,
                                          column(4,radioButtons('bikes_svm_type','Type of Regression',choices = c('Nu'='nu','Epsilon' = 'eps'))),
                                          column(4,radioButtons('bikes_svm_kernel','Kernel',choices = c('Linear' = 'linear',
                                                                                                        'Polynomial' = 'polynomial',
                                                                                                        'Radial' = 'radial',
                                                                                                        'Sigmoid' = 'sigmoid'))),
                                          column(4,uiOutput('bikes_svm_ui'))),
                                   column(12,plotlyOutput("bikes_svm"))))
                            ),
            tabPanel("Regression Tree",
                     br(),
                     column(12,
                            column(6,
                                   p('The goal of the regression tree algorithm is to create a tree learnt by splitting the source set into subsets based on an attribute value test.
                                     This process ie repeated on each derived subset in a recursive manner called recursive partitioning.'),
                                   p('The regression tree is an algorithm very likely to overfit but really quick to compute. We will only test the depth of the tree as hyperparameter here.'),
                                   valueBoxOutput('bikes_tree_results',width = 12),
                                   p('We can see that the performance increases with the depth until it reaches 11 branches.')),
                            column(6,plotlyOutput("bikes_tree"))),
                     column(12,
                            p("One of the biggest advantage of the regression tree compared to the other algorithms is that it's very easy to represent, visualize, understand and interpret.
                              Basically, we build a tree where we have the whole dataset at the top with the average output then we divide our set into a subset given a binary rule."),
                            column(3,
                                   checkboxGroupButtons("bikes_treefeatures", "Features",choices = bikes_ml_feats,
                                                        selected = bikes_ml_feats,direction = "vertical",
                                                        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                                   ),
                                   sliderInput("bikes_treeslide","Depth of Tree",min = 1, max = 5, value = 3,step = 1),
                                   textOutput("bikes_tree_val")
                            ),
                            column(9,plotOutput("bikestree")),
                            p('As we can see, if we remove the feature',em('hour'),'the performance decreases drastically compared to the other features')
                            )),
            tabPanel("Random Forest",
                     br(),
                     column(12,
                            column(6,
                                   p('Random Forest is an ensemble method that constructs a multitude of regression trees at training time and output the mean regression of the individual trees.
                                     It combines the concept of', em('bagging'),'(bootstrap aggregating) and random features selections.'),
                                   p('For this algorithm, we have to tune parameters such as the number of trees to aggregate and the number of features in each tree.
                                     We did a multidimensional gridsearch but the plot in the right show, for each parameter, the maximum performance we could reach.'),
                                   valueBoxOutput('bikes_rf_results',width = 12),
                                   p('The performance increases with the number of trees but almost stagnates after 100 trees: We do not gain any more information by computing more and more trees.'),
                                   p('The performance is better when the number of features is at its maximum. This makes sense since random forest works better with fully grown trees')
                                   ),
                            column(6,
                                   selectInput("bikes_err_rf",label = 'Parameter',choices = c('ntree','features')),
                                   plotlyOutput("bikes_rf"))
                            )),
            tabPanel("Boosted Trees",
                     br(),
                     column(12,
                            column(6,
                                   p('Gradient boosting is a technique where we produce and combine a multitude of weak learners (for example decision trees) to make a strong learner.
                                     Here, we create weak learners iteratively: We initialize with a constant value then, at each step, 
                                     we fit a weak learner on the residuals computed with the gradient.'),
                                   p('For this algorithm, we have to tune parameters such as the shrinkage, the depth of each trees and the number of features in each tree.
                                     We did a multidimensional gridsearch but the plot in the right show, for each parameter, the maximum performance we could reach.'),
                                   valueBoxOutput('bikes_gbm_results',width = 12),
                                   p('The shrinkage and the number of features does not impact significantly the perfomance of the best model.'),
                                   p('On the other hand, for the depth, we have a huge difference whether we choose stump trees (trees of depth 1) or deeper trees.
                                     It might comes from the fact that on interaction between 2 features is really important for this model. 
                                     For depth that are greater than 2, the variation is negligible.')
                                   ),
                            column(6,
                                   selectInput("bikes_err_gbm",label = 'Parameter',choices = c('shrinkage','depth','features')),
                                   plotlyOutput("bikes_gbm"))
                                   ))),
          column(12,
                 hr(),
                 h3('Evaluating on the test set'),
                 column(7,highchartOutput('bikes_testset')),
                 column(5,p('Now that we have tuned our hyperparameters for each algorithm, we just want to be sure we did not overfit the tuning on the validation set. 
                            This is where the test set is important, we will test the algorithms with the hyperparameters that got the best performances on the validation set.
                            If it did not overfit, the performances should be quite similar on the test set as they were in the validation set.'),
                        p('For all the algorithms, the performances are quite similar for the test set and the validation set, which is a good sign.
                          As we can see, the Random Forest and the Boosted Trees are the algorithms who really stand out with an error reduction ratio greater than 80%.'))
                        ),
          column(12,hr()),
          column(12,h3('Variable Importance')),
          column(12,
            column(5,
                   p('Now that we have our best model, what would be interesting to study is to look at the variable importance to see which variable has the most impact on the model.
                          For Gradient Boosting, the importance of a feature is calculated for each decision tree and represents the amount that each split improves the performance measure'),
                   p('If we look at the figure on the right, we can clearly see that the feature',em('hour'),'is much more important than the other features with 68.5% of importance, 
                          which is in agreement with what we saw earlier with the regression tree.
                          Basically, the fact that whether we are in a holiday or not does not affect impactfully the number of bikes rented.')),
            column(7,highchartOutput('bikes_varimp'))),
          column(12,
           hr(),
           h3('Estimate the number of bikes rented'),
           p('Now that we have our final model, we can finally predict the outcome (the number of bikes that will be rent) 
             as soon as we provide the features.')),
          column(12,
            column(8,
              column(3,sliderInput('bikes_hourinp',label = 'Hour',min = 0,max = 23,step = 1,value = 12),
                     radioButtons('bikes_seasoninp','Season',choices = bikes_quali_names$season,selected = 1)),
              column(3,sliderInput('bikes_tempinp',label = 'Temperature',min = 0,max = 40,step = 0.2,value = 20),
                     radioButtons('bikes_holinp','Holiday',choices = 0:1,selected = 0)),
              column(3,sliderInput('bikes_huminp',label = 'Humidity',min = 0,max = 100,step = 1,value = 60),
                     radioButtons('bikes_workinp','Working Day',choices = 0:1,selected = 1)),
              column(3,sliderInput('bikes_windinp',label = 'Wind Speed',min = 0,max = 60,step = 0.2,value = 12),
                     radioButtons('bikes_weatherinp','Weather',choices = bikes_quali_names$weather[-4],selected = 1))),
            column(4,valueBoxOutput('bikes_predict',width = 12))),
          width = 12))
))