tab_timeseries = tabItem(tabName = "timeseries",
                         fluidPage(
                           fluidRow(
                             box(column(12,offset = 0, style='padding:0px;',
                                        column(9,h1(tags$b("Time Series"),': Forecasting items sales')),
                                        column(3,selectInput('sales_them','Choose a chart them',
                                                             choices = c('Elementary',
                                                                         'Dota Buff',
                                                                         'Economist',
                                                                         'FiveThirtyEight',
                                                                         'DarkUnica',
                                                                         'Google')))),
                                 column(12,offset = 0, style='padding:0px;',
                                          intro('Time series are a special type of data where the main explanatory feature is the time, 
                                                 they are mostly studied to forecast future outputs. 
                                                 The amount of randomness and noise makes it very usually difficult and a very challenging task for data scientists.')),
                                 column(12,offset = 0, style='padding:0px;',hr()),
                                 column(12,offset = 0, style='padding:0px;',
                                        column(4,
                                               p("The ",actionLink("sales_tabBut", "dataset"), " that we will study here contains the sales from 2013 to 2017 for 50 
                                                            (anonymous) items and 10 (also anonymous) stores and can be found ",
                                                 a(href="https://www.kaggle.com/c/demand-forecasting-kernels-only/data", "here", target="_blank"),'. 
                                                 Here we will only focus on the 5 first objects for the 3 first stores.'),
                                               p("Let's first look at what the data look like: It seems that every item for every store
                                                            has a yearly and a weekly seasonality. We will dig deeper into that in the next sections."),
                                               bsModal("sales_modal", "Store Sales Data Table", "sales_tabBut", size = "small",
                                                       withSpinner(dataTableOutput("sales_table"))),
                                               br(),
                                               column(12,offset = 0, style='padding:0px;',
                                                      column(5,offset = 0, style='padding:0px;',selectInput("sales_ts_item" , label = "Item to study",choices = 1:5)),
                                                      column(1),
                                                      column(5,offset = 0, style='padding:0px;',selectInput("sales_ts_store", label = "Store to study",choices = 1:3)),
                                                      column(1))
                                               
                                        ),
                                        column(8,column(12,withSpinner(highchartOutput('sales_timeseries')))
                                        )),
                                 column(12,hr()),
                                 column(12,
                                        p('In the next sections, we will evaluate several time series models and compare their results. To do so,
                                              we will train the models from the year 2013 to 2016 and test them on the year 2017 by defining an error metric. 
                                          There are many of them in the literature but we will only focus on the two most popular :'),
                                        tags$ul(
                                          tags$li("The",em("Mean Absolute Error (MAE)")," given by $$MAE = \\frac{1}{N}\\sum_{t=1}^{N}|\\widehat{y_t} - y_t|$$"), 
                                          tags$li("The",em("Root Mean Square Error (RMSE)")," given by $$RMSE = \\sqrt{\\frac{1}{N}\\sum_{t=1}^{N}(\\widehat{y_t} - y_t)^2}$$")
                                        ),
                                        p('We then need a benchmark, more precisely, we need to see what are the performance of the most basic models that
                                          we can have. For this we can use several basic models:'),
                                        tags$ul(
                                          tags$li("The",em("Average method")," : which consists in always taking the average of the training set."), 
                                          tags$li("The",em("Naive method")," : which consists in always taking the value of the last observation.")
                                        )),
                                 column(12,hr()),
                                 column(12,
                                        column(4,radioButtons('sales_metric','Choose the error metric',choices = c('MAE','RMSE'))),
                                        column(4,valueBoxOutput('sales_mean',width = 12)),
                                        column(4,valueBoxOutput('sales_naive',width = 12))),
                                 width = 12),
                             box(subtitle_('Prophet'),
                                 column(12,p('Prophet is an algorithm developped by Facebook for forecasting time series data based on an additive (or a multiplicative) 
                                                  model where non-linear trends are fit with yearly, weekly, and daily seasonality, plus holiday effects'),
                                        p('Here, we will see the effects of the different seasonalities. 
                                          We can control if we add weekly, monthly or yearly seasonality and if we add them as additive or multiplicative terms.
                                          For each case, we will compute error (chosen before) to see how far are our predictions from the reality.')),
                                 column(12,
                                        column(4,
                                               box(title = 'Inputs',status = "primary",
                                                   radioButtons('sales_proph_week','Weekly Seasonality', inline = TRUE,
                                                                choices = c('None','Additive','Multiplicative'),selected = 'Additive'),
                                                   radioButtons('sales_proph_month','Monthly Seasonality', inline = TRUE,
                                                                choices = c('None','Additive','Multiplicative'),selected = 'Additive'),
                                                   radioButtons('sales_proph_year','Yearly Seasonality', inline = TRUE,
                                                                choices = c('None','Additive','Multiplicative'),selected = 'Additive'),
                                                   width = 12),
                                               box(title = 'Prophet Formula', status = "primary",
                                                   uiOutput('sales_prophformula'), 
                                                   width = 12),
                                               valueBoxOutput('sales_prophresults',width = 12)),
                                        column(8,
                                               tabsetPanel(id = 'sales_tabprophet', 
                                                           tabPanel('Component by Component',
                                                                    column(6,withSpinner(highchartOutput('sales_prophtrend', height = "250px"))),
                                                                    column(6,uiOutput('sales_prophweekui')),
                                                                    column(6,uiOutput('sales_prophmonthui')),
                                                                    column(6,uiOutput('sales_prophyearui'))),
                                                           tabPanel('Predicted Vs Real Value',
                                                                    withSpinner(highchartOutput('sales_prophpred')))
                                               ))),
                                 column(12,hr()),
                                 column(12,
                                        column(8,
                                               column(12,
                                                      column(3,selectInput('sales_proph_fix','Fix the seasonality',
                                                                           choices = c('Weekly','Monthly','Yearly'),selected = 'Weekly')),
                                                      column(3,selectInput('sales_proph_to','To',
                                                                           choices = c('None','Additive','Multiplicative'),selected = 'None')),
                                                      column(1),
                                                      column(3,uiOutput('sales_proph_col_ui'))),
                                               column(12,highchartOutput('sales_proph_dt'))),
                                        column(4,
                                               p('The bar chart on the left shows the error for the different combinations of seasonality.
                                                 Basically, what we can see is that the mode of a seasonality (additive or multiplicative) does not change
                                                 much the performance for some case but when it does, the additive model has a better performance (a smaller error).'),
                                               p('Besides, we see is that the weekly and the yearly seasonalities reduces strongly the performance but 
                                                 monthly seasonality has a really small impact as we can confirm 
                                                 with the vertical axes of the plots above. Even worse, it seems that the monthly seasonality worsen the
                                                 performance. This might be due to an eventual overfitting.')
                                               )),
                                 
                                 width = 12)
                           )))