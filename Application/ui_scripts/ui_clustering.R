tab_clust = tabItem(
  tabName = "Clustering",
  fluidPage(
    fluidRow(
      box(column(12,h1(strong("Clustering "),': Grouping the US states according to their amount of exports')),
          intro('Clustering is a kind of unsupervised learning where the mission is to find individuals with similar patterns
                in order to class them in groups (called clusters). Optimally, we want individuals from one same cluster to be the most
                similar possible and individuals from different clusters to be the less similar possible.'),
          #withSpinner(DT::dataTableOutput('usa_df'))),
          column(12,hr()),
          column(12,
                 column(4,
                        br(),
                        p('The United States has 50 states and each state has very different characteristics. 
                          What we will try to do here is to cluster these states in a number of groups that we 
                          will choose given the amount they exported in various field. 
                          The ',actionLink("usa_tabBut", "dataset"), ' we will study contains the total exports of the United States 
                          (in millions dollars USD) per state in 2011, it can be found on Kaggle'
                          #a(href="https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv", "here", target="_blank")
                          ),
                        p('The univariate analysis here allows us to have a quick view of the state per variable one by one. We can already see basic patterns. 
                          For instance, the Texas is far ahead the other states in term of beef export 
                          (likewise for Iowa in term of pork export or California in term of fruit export).'),
                        p('The end goal here would be to be able to cluster the states not for one variable, but for an overall view.'),
                        bsModal("usa_modal", "USA Export Data Table", "usa_tabBut", size = "large",
                                withSpinner(dataTableOutput("usa_table")))),
                 column(8,
                        column(12,
                               column(3,selectInput('usachoice','Choice',choices = colnames(usa_df)[-(1:3)])),
                               column(3,selectInput('usacolors','Color Palette',choices = palettes)),
                               column(3,
                                      prettyCheckbox(
                                        'usa_revcol',label = 'Reverse colors',value = FALSE,
                                        icon = icon("check"),animation = "rotate",status = 'primary'
                                      ),
                                      prettyCheckbox(
                                        'usa_quantcol',label = 'Color By Bins',value = FALSE,
                                        icon = icon("check"),animation = "rotate",status = 'primary'
                                      )),
                               column(3,uiOutput('usa_quantnum'))),
                        leafletOutput('usamap'))
                 ),
          width = 12),
      box(subtitle_('Dimensions Reduction'),
          column(12,
                 p('We have, in total, 14 variables, which is way too many and some of them might give very similar information. 
                   This can be problematic for many reasons:'),
                 tags$ul(
                   tags$li("On a business point of view, it can be a huge waste of time to look at several variables and to realise after that they 
                           gave the same information"), 
                   tags$li("On a machine learning point of view, not only it takes much more memory space than needed, but it can
                           bias, for example, regression and classification models.")
                   ),
                 p('To measure the similarity between variables, we usually look at', em('the coefficient of correlation,'),' 
                   which indicates the degree to which a pair of variables are linearly related:'),
                 tags$ul(
                   tags$li("If this coefficient is close to 1, it means the variables are very correlated, 
                           i.e. when one variable increases, the other increases as well"), 
                   tags$li("If this coefficient is close to 0, it means the variables are not linearly related at all."),
                   tags$li("If this coefficient is close to -1, it means they are anti-correlated (when one increases, the other decreases)")
                   ),hr()),
          
          column(12,
                 column(5,
                        column(4,selectInput('usa_corrmeth',label = 'Visualisation Method',selected = 'square',
                                             choices = c("circle", "square", "number", "pie"))),
                        column(4,selectInput('usa_corrorder',label = 'Order Algorithm',selected = 'hclust',
                                             choices = c("AOE", "FPC", "hclust", "alphabet"))),
                        column(4,uiOutput('usa_corrui')),
                        column(12,offset = 0, style='padding:0px;',plotOutput('usa_corr'))),
                 column(7,
                        p("The plot on the left shows the coefficient of correlations for each pair of variables.
                          We can see some obvious correlations such as",em("total.veggies"),"which is strongly correlated to ",
                          em("veggies.fresh"),"and",em("veggies.proc"), "since it is basically the sum of them."),
                        p("The different types of ordering algorithms allow to have a better visualisation of which pairs of variables are correlated.
                          The", em('hclust'), "option allows to order the variables giving the Hierarchical Clustering method (described in the section below)."),
                        p("Basically, we can see that there are four groups of correlated variables (the big blue squares) given the hclust method.
                          We need now to make a deeper analysis and to see how we can reduce our 14 variables to only 3 or 4 
                          with keeping the maximum of information possible."),
                        p('What we usually do in that case is a Principal Components Analysis (PCA)')
                        
                        )
                        ),
          column(12,h3('Principal Components Analysis (PCA)'),
                 p('The PCA is a statistical algorithm aiming at projecting possibly correlated features on orthogonal and unlinearly correlated new axes called',
                   strong('Principal Components'),'. Thus, it generally also allows to reduce the dimension of the variables. To do so, we can either:'),
                 tags$ul(
                   tags$li("Fix a number of components (variables) that we want"), 
                   tags$li("Fix a percentage of the variance explained we want to keep")
                 )),
          column(12,hr()),
          column(12,
                 column(4,radioButtons("usa_pcachoice", "Choice",
                                       c("Number of components" = "numb",
                                         "Part of variance explained" = "part")),
                        uiOutput('usa_pcachoicenum'),
                        valueBoxOutput("pca_box",width = 12)),
                 column(8,tabsetPanel(type = "tabs",
                                      tabPanel("Scatter Plot", plotlyOutput('usa_pca')),
                                      tabPanel("Correlation plot", plotOutput('usa_corrplot'))
                 ))),
          column(12,br()),
          column(12,
                 p("The scatter plot shows that, to have 90% of the variability (information) of all the 14 variables, we only need 4 components:
                   We could already reduce 14 variables to only 4! Isn't it beautiful?"),
                 p("The correlation plot shows from which variable each component are made. Obviously, all correlated data will be in the same component.
                   If we look closely at it, we can see that:"),
                 tags$ul(
                   tags$li("The first component is mainly composed by the exports in fruits, veggies and dairies"), 
                   tags$li("The second component is mainly composed by the exports in beef, pork and corn"),
                   tags$li("The third component is mainly composed by the exports in poultry and cotton"),
                   tags$li("The fourth component is mainly composed by the exports in beef and wheat")
                 ),
                 p('In the next parts, we will look closely at each of the 3 first components and how are each state represented with these new variables')),
          column(12,hr()),
          column(12,
                 column(8,
                   column(12,
                          column(4,selectInput('usa_pcacomp',label = 'Components',
                                               choices = c('Components 1 and 2' = '1,2',
                                                           'Components 1 and 3' = '1,3',
                                                           'Components 2 and 3' = '2,3'))),
                          column(4,selectInput('usa_pcadisp',label = 'Display', choices = c('States','Variables','Both'))),
                          column(4,
                                 prettyCheckbox(
                                   'usa_flat1',label = 'Flatten',value = TRUE,
                                   icon = icon("check"),animation = "rotate",status = 'primary'
                                 ),
                                 prettyCheckbox(
                                   'usa_repel1',label = 'Repel ',value = FALSE,
                                   icon = icon("check"),animation = "rotate",status = 'primary'
                                 ))),
                   column(12,offset = 0, style='padding:0px;',plotOutput('usa_pcaplot'))),
                 column(4,
                        p('The plot on the left shows the different States or (and) the different variables distributed on the principal components.
                          As we can see, the first component is strongly pulled by California who has a really high value of export in fruits, veggies and dairies.
                          Likewise, the second component is pulled by Iowa who has important export of beef, pork and corn.'),
                        p('If we look at the correlation circle of the variables, we can see that the contribution of the variables 
                          are in agreement with the correlation plot we studied earlier'),
                        p('As it is sometimes difficult to visualise the contribution of some states because the distribution are really uneven, 
                          the user can check the option',em('flatten'),
                          'which basically scales the components by taking the square of the square (hence power 1/4) of them.'),
                        p('The' , em('repel'),'option allows to dispatch the variates (or states) when these ones are too compacted.'))),
          width = 12),
      box(subtitle_('Clustering'),
          column(12,
                 p('Now that we have our components, we can cluster the states (either on the raw data or the flattened ones). 
                   For clustering, we usually use either',em('Hierarchical Cluster Analysis'),'or',em('K-means')),
                 tags$ul(
                   tags$li(em("K-means"), "is an interative clustering algorithm that aims to find local maxima in each iteration. 
                           With this algorithm, we start with a random choice of clusters and the alrogithm might converge into a local maximum.
                           Besides, with K-means, we have to know the number of clusters since the beginning"),
                   tags$li(em('Hierarchical Cluster Analysis (HCA)'),' is a much greedier algorithm (O(n^2)) than K-means (O(n)). 
                           It computes the distance between each observation then regroup the closest observations together.
                           The advantage of this algorithm is that we have a dendrogram that allows us to choose the optimal number of clusters since 
                           the vertical height is the variance between 2 observations (or clusters).
                           Basically, we want to have clusters that have very similar observations (minimize the intra variance) and 
                           that differ much with observations of other clusters (maximize the inter variance)')
                   ),
                 p('Here, we only have 50 observations and we do not know how many clusters we want. Hence, the HCA seems to be a much better choice.'),
                 hr()),
          sidebarPanel(
            prettyCheckbox(
              'usa_flat2',label = strong('Flatten'),value = TRUE,
              icon = icon("check"),animation = "rotate",status = 'primary'),
            checkboxGroupButtons('usa_compchoice',label = 'Components to cluster on',choices = 1:3,
                                 selected = 1:3,status = "primary",
                                 checkIcon = list(yes = icon("ok",lib = "glyphicon"),no = icon("remove",lib = "glyphicon"))
            ),
            sliderInput('usa_pcak',label = 'Number of Clusters', min = 2, max = 8, step = 1, value = 4),
            uiOutput('usa_pcacond')),
          mainPanel(
            tabsetPanel(id = 'tab_usa',
                        tabPanel('Dendrogram',plotOutput('usa_pcahdendr')),
                        tabPanel('Scatterplot',plotOutput('usa_pcahclust')),
                        tabPanel('Choropleth Map',leafletOutput('usamap2')))
          ),
          column(12,
                 hr(),
                 p('In a dendrogram, the vertical bars represent the variance between two populations. The bigger the distance between the
                   two populations is, the taller the bar will be. This can help us to choose the right number of clusters as we want the vertical
                   bars inside a group to be small and the ones outside the group to be big.'),
                 p('If we choose to cluster on all the 3 components, we can see through the dendrogram that 4 groups seems optimal 
                   since the vertical bars after 4 groups are much bigger than those before.')),
          width = 12)
      ))) 