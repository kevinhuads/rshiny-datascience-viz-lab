tab_markov = tabItem(
  tabName = 'Markov',
  fluidPage(fluidRow(
    box(column(12,column(12,h1(tags$b("Markov Chain"),': Simulating new words in different languages')),
      intro("A Markov chain is a mathematical system that experiences transitions from one state to another according to certain probabilistic rules. 
            The defining characteristic of a Markov chain is that no matter how the process arrived at its present state, the possible future states are fixed. 
            In other words, the probability of transitioning to any particular state is dependent solely on the current state and time elapsed. 
            The state space, or set of all possible states, can be anything: letters, numbers, weather conditions, baseball scores, or stock performances.")
    ),
    column(12, 
           hr(),
      column(5,p("What makes one language different from another? 
        In this section, we are going to use mathematics to create new words in seven different languages (Dutch, German, English, French, Spanish, Italian and Portuguese)
        For this, we will use and analyse the data from the ",a(href="https://www.gutenberg.org", "Project Gutenberg ", target="_blank"),"which is a library of many ebooks in different languages."),
        p("First, we need to understand the different patterns of the languages. You can ",actionLink("markov_tabBut", "click here"),"to see the most used words in our target languages."),
        bsModal("markov_modal", "Gutenberg most frequent words", "markov_tabBut", size = "medium",
                sliderInput("markov_tableind", label = "Number of characters",value = 6,min = 3,max = 18),
                dataTableOutput("markov_table")),
        p('The plot on the right show the distribution of lengths for the words in our target languages. 
          We can see for example that generally, german words are longer than english words')),
      column(7,withSpinner(highchartOutput("ma_length")))),
    column(12, br(),
           column(6,
                  column(4,selectInput("ma_occurlang",label = "Language",
                                       choices = markov_languages_full,selected = "English")),
                  column(4,selectInput("ma_occurtype",label = "Plot Type", 
                                       choices = c('column','treemap','bubble','scatter','pie','coloredarea','bar','waterfall'),
                                       selected = "pie")),
                  column(4,selectInput("ma_occurcol",label = "Color Palette",
                                       choices = palettes,selected = "Blues")),
                  withSpinner(highchartOutput("ma_occur"))),
           column(6,
                  br(),
                  p("Even though the languages that we study here are all European languages with a lot of similarities, each of them are different and have their rules on their own."),
                  p("The plot on the left shows for each of our targeted languages the most used letters. (We assimilated here the accented characters to their non accented counterparts.)
                    It seems that the letter",strong('e'),"is often the most used character in French, German and Dutch (which can reach until 16% of all the letters used)
                    whereas the letter",strong('a'),"is very more used in English, Spanish, Portuguese and Italian."),
                  p("This information is great, but it's not with that that we will be able to simulate new words that
                    can sound like a language or another. To do so, we need some more complex features like how are letters used
                    one regarding to another, and that's what we are going to analyse in the next part."))),
    column(12, 
           column(6,
                  br(),
                  p("The heatmap at the right has to be read horizontically and shows the probability of a letter following another. For example, if we look at Portuguese,the letter",
                    strong('ã'),"has a probability of 89.29% of being followed by the letter", strong('o'), "(those who speak Portuguese will understand why) and the letter",
                    strong("b"),"has a probability of 32.76% of being followed by the letter", strong('r.')),
                  p("In this graphic, the probability of following a space means the probability of being at the beginning of a word and similarly the probability of being followed by a space
                    is the probability of being the last letter of a word. For example, a word in Spanish has a probability of 12.12% of beginning with a ",strong("c"),"and if there is a word in French with a",strong("z,"),
                    "it has 79.71% of being the last letter of the word."),
                  p("The idea of a Markov Chain is to use these probailities to create new entities (in our case: words). If we want to simulate a new French word, the first letter will have a probablity of 9.23% of being an", strong('a,'),
                    "a probability of 3.15% of being a",strong('b'),"and so on...Then for the second letter we will do the same but of course we will consider the first letter that we've already simulated. We will continue until
                    arriving at a ",em("space.")),
                  p("Let's see what this model gives us below.")),
           column(6,
                  column(4,selectInput("ma_heatlang",label = "Language",
                                     choices = markov_languages_full,selected = "Portuguese")),
                  column(4,selectInput("ma_heatcol",label = "Color Palette",
                                     choices = palettes,selected = "Blues")),
                  highchartOutput("ma_heatmap",height = "550px"))),
    column(12, 
           column(6,
                  column(4,selectInput("ma_genlang",label = "Language",choices = markov_languages_full,selected = "French")),
                  column(4,sliderInput("ma_genlength",label = "Word Length",min = 5, max = 15,value = 8)),
                  column(4,actionBttn("ma_gennew",label = "Generate", style = "fill", color = "danger",icon = icon("redo"),size = 'md')),
                  column(12,withSpinner(DT::dataTableOutput("ma_gentable",height = "300px")))),
           column(6,
                  br(),
                  p("The generated words reflect many of the specific patterns captured by the model: 
                   the French samples tend to resemble plausible French words, and the Portuguese samples 
                   also align well with the structure of Portuguese. For English, however, the effect is less 
                   pronounced, as its letter–transition probabilities are more homogeneous and the constraints 
                   on successive letters are more flexible."))),
    width = 12)
  )))
