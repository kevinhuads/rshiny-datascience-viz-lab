tab_hotel = tabItem(
  tabName = "NLP",
  fluidPage(
    fluidRow(
      box(column(12,h1(tags$b("Natural Language Processing"),': Analysing the patterns of Hotel Reviews')),
          intro('Natural Language Processing, also known as NLP, is a field of Artificial Intelligence aiming at enabling computers to understand
                and process human languages. Among the major tasks of this field, we can quote Speech Recognition, 
                Machine Translation, Grammar Induction or even Question Answering.'),
          column(12,hr()),
          column(12,offset = 0, style='padding:0px;',
                 column(4,
                        p("The ",actionLink("hotel_tabBut", "dataset"), " we will study here comes from ",
                          a(href="https://www.kaggle.com/jiashenliu/515k-hotel-reviews-data-in-europe", "Kaggle", target="_blank"),br(),
                          'They are collected from Booking.com and gather more than 500 000 reviews for around 1500 hotels in 6 cities 
                          (Paris, Barcelona, Vienna, London, Amsterdam and Milan)'),
                        p("The advantage with this database is that for each review, we have separately the positive comments and the negative ones.
                          What we will try to do here is to see if we can analytically differentiate the positive comments from the negative ones."),
                        bsModal("hotel_modal", "Hotel Data Table", "hotel_tabBut", size = "large",
                                withSpinner(dataTableOutput("hotel_table")))),
                 column(8,withSpinner(leafletOutput('hotel_map')))),
          column(12,hr()),
          column(12,offset = 0, style='padding:0px;',
                 column(8,
                        column(12,
                               column(3,
                                      
                                      awesomeRadio('hotel_rangebreak',label = 'Interval Length', c(0.5,1,2))),
                               column(5,awesomeRadio('hotel_rangedisp',label = 'Display', 
                                                     choices = c('Average Positive Words' = 'Num_Pos',
                                                                 'Average Negative Words' = 'Num_Neg',
                                                                 'Number of Reviews' = 'Num_Reviews'
                                                     )))
                        ),
                        column(12,highchartOutput('hotel_rangescore'))
                 ),
                 column(4,
                        p("For each review, we counted the number of words in the positive comments and in the negative comments.
                          We then binned them with the score affected to the review and compare the results."),
                        p("Naturally, what we see is that:"),
                        tags$ul(
                          tags$li("the higher is the score of the review, the more words the positive comment has."), 
                          tags$li("the lower is the score of the review, the more words the negative comment has.")
                        ))),
          column(12,hr()),
          column(12,offset = 0, style='padding:0px;',
                 column(12,
                        column(2,
                               p('For each country, we computed the average rating score per day:
                                 In a general point of view, it seems that the months from December to 
                                 April have better marks than the rest of the year.'),
                               p('We can also notice that hotels from Austria, Spain and the Netherlands have generally better marks than average
                                 while hotels from London have worse marks.')),
                        column(10,
                               column(12,
                                      column(4,pickerInput(
                                        'hotel_sentcountry',label='Country',
                                        choices = list(Overall = 'Overall',Countries = colnames(hotel_sent)[-c(1,2)]), 
                                        choicesOpt = list(
                                          content = mapply(c('Overall',colnames(hotel_sent)[-c(1,2)]), 
                                                           paste0('flags/',c('earth','at','fr','it','nl','es','gb'),'.svg'), 
                                                           FUN = function(country, flagUrl) {
                                                             HTML(paste(
                                                               tags$img(src=flagUrl, width=20, height=15),
                                                               country
                                                             ))
                                                           }, SIMPLIFY = FALSE, USE.NAMES = FALSE))
                                      )),
                                      column(4,uiOutput("hotel_ggsent")),
                                      column(4,radioButtons('hotel_sentquart','Select Quantile Bounds', choices = paste(c(5,10,25),'%'),
                                                            selected = '10 %',inline = TRUE))),
                               column(12,ggvisOutput("hotel_sentvis"))))),
          
          
          width = 12),
      box(subtitle_('Text Mining'),
          column(12,p('It is obvious that with more than 500 000 positive and negative reviews, 
                      it would be very long and tedious to read all the reviews 
                      one by one. Instead, we will use text mining technics to get a general idea of what is said.')
          ),
          column(12,
                 hr(),
                 column(6,
                        column(12,offset = 0, style='padding:0px;',
                               column(4,radioButtons('hotel_wordbar_choose',label = 'Plot Type :',
                                                     choices = c('Wordcloud','Bar Chart'))),
                               column(4,radioButtons('hotel_negpos',label = 'Review :',
                                                     choices = c('Positive Reviews' = 'Positive',
                                                                 'Negative Reviews' = 'Negative'))),
                               column(4,sliderInput('hotel_wc_ngram',label = 'N-Gram Tokenizer',
                                                    min = 1, max = 4, step =1, value = 1))
                        ),
                        p('Wordcloud is a very usefull tool to analyse text data as it allows to emphasize quickly the words that are the most used in a text.
                          A word that appears many times in the texts will be bigger in the wordcloud than other words.'),
                        p('We can see that the words that appears the most in the positive reviews are most about the staff which were friendly or the location
                          which were good whereas the words in the negative ones are mostly about small rooms.'),
                        p('It is a little bit hard here to see different topics as there are a 
                          lot of redundent thems and sequence of words (small room, room small...),
                          we will try to overcome this problem in the next session.')
                        ),
                 column(6,withSpinner(uiOutput('hotel_wordbar')))
                 
                        ),
          width = 12),
      box(subtitle_('Graph Network'),
          column(12,
                 p('Graphs are a great way to display the relationships of the words between each other. 
                   A graph is made of ',strong('nodes'),' that are connected with ',strong('edges.'),
                   'In our case, the nodes will represent the words (for which the size will represent their frequency) and the edges
                   will represents their connections (for which the opacity will represent their strength).'),
                 p('To build a graph, we need what we commonly call a ',strong('layout'), 
                   'that will choose how to display horizontally and vertically each word. 
                   It is hard to know which layout will be the best as they all have their own strengths and weaknesses.
                   Some of them are really simple such as',em('star, circle, sphere, random, grid')),
                 p('There are several classifications possible in the literature. Here, we will focus on the ',em('Force-directed graph drawing'),
                   'which is a special group of layout algorithms aiming at drawing graph in an aesthetically-pleasing way using 
                   general global optimisation methods.')),
          column(12,
                 column(2,offset = 0, style='padding:0px;',br(),
                        radioButtons('hotel_graphchoice',label = "Reviews", c('Positive','Negative'),inline = TRUE),
                        radioButtons("hotel_graphlength",label = "Number of words to show",c(250,500,1000),selected = 500,inline = TRUE),
                        selectInput('hotel_graphlayout',label = 'Layout', selected = 'fr',choices = hotel_layouts_labels)
                        # checkboxInput('hotel_graphnode','Scale Nodes',value = FALSE)
                        ),
                 column(10,withSpinner(highchartOutput('hotel_graph',height = '700px')))),
          
          width = 12),
      box(subtitle_('Latent Dirichlet Allocation'),
          column(12,
                 p('The Latent Dirichlet Allocation (LDA) is an unsupervised learning algorithm made for ', em('topic modelling,'),
                   'which is a statistical model aiming at grouping both documents (here the reviews) 
                   using similar words as well as words occuring in a similar set of documents. The LDA needs in input the number
                   of topics that is arbitrary chosen by the user according to what he needs'),
                 p('Here, we will choose a number ', em('k'),' of topics and we will display a table that will arrange by order:'),
                 tags$ul(
                   tags$li("the most recurrent topics"), 
                   tags$li("within each topic, the words that are represented the most")
                 ),
                 p('We can display either the wordcloud or the graph network that will be here colored by 
                   the topic of each word')),
          column(12,br()),
          column(12,
                 column(2,offset = 0, style='padding:0px;',
                        radioButtons('hotel_ldachoice',label = 'LDA dictionnary', choices = c('Positive','Negative')),
                        numericInput('hotel_ldak',label = 'Number of Topics',min = 2, max = hotel_nlda+1 , step =1, value = 4),
                        radioButtons('hotel_ldadisp',label = 'Display', choices = c('Table','Wordcloud','Graph')),
                        withSpinner(uiOutput('hotel_ldawordshow'))
                 ),
                 column(10,uiOutput('hotel_ldachart'),align="center")
          ),
          column(12,br()),
          column(12,p("Let's look at what happens when we choose 4 topics, for the positive reviews :"),
                 tags$ul(
                   tags$li("The first topic seems to be about the overall stay and the good quality for the price (value, services..)"),
                   tags$li("The second topic seems to be about the staff (that was friendly,nice, helpful...)"),
                   tags$li("The third topic is more about the location of the hotel (close to the station...)"), 
                   tags$li("The fourth topic talks more about the room (that were clean, comfortable, quiet, with a great view...)")
                 ),
                 p("For the negative reviews :"),
                 tags$ul(
                   tags$li("The first topic seems to be about the room services in general (shower that did not have hot water 
                           and the room that were noisy during the night...)"),
                   tags$li("The second topic seems to be about the booking and the check-in check-out that had problems"),
                   tags$li("The third topic is more about the price of the service,the bar and the breakfast"),
                   tags$li("The fourth topic talks about the size of the rooms which were small")
                 )),
          width = 12)
)))