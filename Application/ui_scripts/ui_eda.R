#### Football --------

tab_eda_foot = tabItem(
  tabName = 'eda_foot',
  widgetUserBox(
    offset = 0, style='padding:0px;',
    title = "Football",
    subtitle = "Competitions summary",
    width = 12,collapsible = FALSE,
    src = "footballlogo.jpg",
    background = TRUE,backgroundUrl = "footballcover.jpg",
    column(12,style='padding:0px;',
      p(),
      column(12,br()),
      column(2,style='padding:0px;',
             column(12,actionBttn(inputId = "foot_help",label =  "Methodology", 
                                  color = "primary",icon = icon("question"), 
                                  style = "material-flat"),
                    column(12,br())),
             box(title = 'Filters',
                 solidHeader = TRUE,
                 status = "primary",
                 selectInput('foot_compet','Competition',
                             choices = c('World Cup' = 'wc','Champions League' = 'cl'),
                             selected = 'World Cup'),
                 width =12
             ),
             uiOutput('foot_sum')),
      conditionalPanel(
        condition = "input.foot_compet == 'wc'",
        column(10,style='padding:0px;',
               column(6,radiogroup_cust(input = 'foot_wc',
                                        choices_ = c("matches","goals","average_attendance","average_goals"),
                                        transform_name = TRUE)),
               column(6,radiogroup_cust(input = "foot_wc1_winner", choices_ = c("host","winner"),
                                        transform_name = TRUE)),
               column(12,highchartOutput('foot_wctime')),
               column(12,br()),
               column(12,radiogroup_cust(input = "foot_wc2", choices_ = c("titles","finals_reached"),
                                         transform_name = TRUE)),
               column(6,highchartOutput('foot_wctop')),
               column(6,highchartOutput('foot_wcscore')))),
      conditionalPanel(
        condition = "input.foot_compet == 'cl'",
        column(10,style='padding:0px;',
          column(5,radiogroup_cust(input = "foot_cl", 
                                   choices_ = c("matches","goals","goals_per_match"),
                                   selected_ = "goals_per_match",transform_name = TRUE)),
          column(3,radiogroup_cust(input = "foot_cl_winner", choices_ = c("club","country"),
                                   transform_name = TRUE)),
          column(3,sliderInput(inputId = "foot_cl_num", label = NULL,min = min(eda_cl$df$year), 
                               max = max(eda_cl$df$year),value = c(1992,max(eda_cl$df$year)))),
          column(12,highchartOutput('foot_cltime')),
          column(12,br()),
          column(6,radiogroup_cust(input = "foot_cl2", choices_ = c("titles","finals_reached"),
                                   transform_name = TRUE)),
          column(6,radiogroup_cust(input = "foot_cl3", choices_ = c("goals_scorers","appearances"),
                                   transform_name = TRUE)),
          column(6,highchartOutput('foot_cltop')),
          column(6,highchartOutput('foot_clscore'))
        )
      )
    )) %>% fluidRow() %>% fluidPage())


#### Olympics --------

# tab_eda_og = tabItem(
#   tabName = 'eda_og',
#   widgetUserBox(
#     title = "Olympic Games",
#     subtitle = "Winter and Summer competitions summary",
#     width = 12,collapsible = FALSE,
#     src = "oglogo.jpg",
#     background = TRUE,backgroundUrl = "ogcover.png",
#     column(12,offset = 0, style='padding:0px;',
#            p(),
#            column(12,offset = 0, style='padding:0px;',
#                   column(6,radiogroup_cust(input = 'og_var',choices_ = c("competitors","men","women","nations","events"),transform_name = TRUE)),
#                   column(6,radiogroup_cust(input = 'og_flag',choices = c("top_nation","host_country"),transform_name = TRUE)),
#                   column(6,highchartOutput('og_winter')),
#                   column(6,highchartOutput('og_summer')),
#                   column(12,radiogroup_cust(input = 'og_var2', choices = c("gold","silver","bronze","total"),transform_name = TRUE)),
#                   column(6,highchartOutput('og_winter2')),
#                   column(6,highchartOutput('og_summer2'))
#            )
#     )) %>% fluidRow() %>% fluidPage())


#### Game of Thrones --------

tab_eda_got = tabItem(
  introjsUI(),
  tabName = 'eda_got',
  widgetUserBox(
    title = "Game of Thrones",
    subtitle = "Overview",
    type = 2,
    width = 12,collapsible = FALSE,
    src = "goticon.png",
    background = TRUE,backgroundUrl = "gotbg2.jpg",
    column(12,
      column(9,radiogroup_cust(input = 'got_var',
                               choiceValues_ = c("us_millions_viewers","imdb_ratings","imdb_voters"),
                               choiceNames_ = c("Viewers","IMDB Ratings","IMDB Voters"))),
      column(3,actionBttn(inputId = "got_help",label =  "Methodology", 
                          color = "primary",icon = icon("question"), 
                          style = "material-flat")),
      highchartOutput('got_ep',height = '550px')),
    column(12,br()),
    column(12,highchartOutput('got_top',height = '550px'))
    ) %>% fluidRow() %>% fluidPage()
)

#### Boston Marathon --------

# tab_eda_bm = tabItem(
#   tabName = 'eda_bm',
#   widgetUserBox(
#     title = "Boston Marathon",
#     subtitle = "2015, 2016 and 2017",
#     width = 12,collapsible = FALSE,
#     src = "bostonlogo.jpg",
#     background = TRUE,backgroundUrl = "bostoncover.jpg",
#     column(12,offset = 0, style='padding:0px;',hr(),
#            p(),
#            p("Given the results of the Boston Marathon runners of 2015, 2016 and 2017, can we see typical patterns for the runners over the years?
#              
#              The Boston Marathon is the world's oldest annual marathon and definitely one of the most prestigious marathon in the world.
#              Unlike most marathons, you have to be an elite runner and have a qualifying time to enter. (Even if there are some other ways to participate).
#              The ",actionLink("mara_tabBut", "dataset"), " that will be studied here contains the finishing times from 2015 to 2017 for 50 and can be found ",
#              a(href="https://www.kaggle.com/rojour/boston-results", "here", target="_blank"),
#              bsModal("mara_modal", "Marathon Data Table", "mara_tabBut", size = "large",
#                      withSpinner(dataTableOutput("mara_table")))),
#            hr(),
#            column(2,offset = 0, style='padding:0px;',
#                   box(title = 'Filters',
#                       solidHeader = TRUE,
#                       status = "primary",
#                       selectInput('mara_year','Year',choices = c('All',2015:2017),selected = 'All'),
#                       selectizeInput('mara_country','Continent',
#                                      choices = list(World = 'World',
#                                                     By_Continent = c('Western Europe','Eastern Europe',
#                                                                      'North America','Central America','South America',
#                                                                      'Asia','Africa','Oceania')),
#                                      selected = 'Western Europe'),
#                       width =12),
#                   valueBoxOutput('mara_btotal',width = 12),
#                   valueBoxOutput('mara_bfast',width = 12),
#                   valueBoxOutput('mara_btime',width = 12),
#                   valueBoxOutput('mara_bage',width = 12),
#                   valueBoxOutput('mara_bfem',width = 12)),
#            column(10,
#                   column(12,offset = 0, style='padding:0px;',
#                          column(6,
#                                 radiogroup_cust(input = "mara_pyrabreak",
#                                                 label = "Age Interval Length",
#                                                 choiceNames_ = paste(c(2,5), 'Years'),
#                                                 choiceValues_ = c(2,5)),
#                                 highchartOutput('mara_pyramid',height = '500px')
#                          ),
#                          column(6,
#                                 radiogroup_cust(input = "mara_geotype",label = "Plot Type",
#                                                 choiceNames_ = unname(sapply(c('column','treemap','bubble','scatter',
#                                                                                'pie','coloredarea','bar','waterfall'),simpleCap)),
#                                                 choiceValues_ = c('column','treemap','bubble','scatter',
#                                                                   'pie','coloredarea','bar','waterfall'),selected_ = "pie"),
#                                 highchartOutput('mara_geo')
#                          )),
#                   column(12,offset = 0, style='padding:0px;',
#                          column(6,
#                                 column(12,radiogroup_cust(input = 'mara_sex',choices_ = c("Both","Men","Women"))),
#                                 column(12,plotOutput("mara_time"))),
#                          column(6,
#                                 column(12,offset = 0, style='padding:0px;',
#                                        column(4,selectizeInput('mara_choice',NULL,
#                                                                choices = list("Caps" = marathon_list_times, "Pace" = c("Pace" = "Pace")))),
#                                        column(4,radiogroup_cust(input = 'mara_distplot',choices_ = c('Density Plot','Box Plot'))),
#                                        column(4,radiogroup_cust(input = 'mara_bysex',choices_ = c("Split by Gender","Aggregated")))),
#                                 column(12,plotOutput('mara_dist')))))
#     )) %>% fluidRow() %>% fluidPage())