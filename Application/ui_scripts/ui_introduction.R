tab_intro = tabItem(tabName = "Presentation",
                    fluidPage(
                      fluidRow(
                        box(title = NULL,
                            column(12,
                                   column(6,offset = 0, style='padding:10px;',
                                          shiny::img(src='Data-Science.jpg',width = '100%',align = "left")),
                                   column(6,offset = 0, style='padding:20px;',
                                          h1('Welcome to my Shiny Application', 
                                              style = "font-family: 'Lobster', cursive;ont-weight: 500; line-height: 1.1;  color: #4d3a7d;"),
                                          br(),
                                          p('Welcome to my data science portfolio. This application presents a collection of projects that use data to explore concrete questions in areas such as retail, health, travels and marketing. 
                                            Each section focuses on a specific problem, the methods used to address it and the insights that follow.'),
                                          p("The central objective here is visual and intuitive understanding. 
                                            The topics covered, such as regression, clustering, time series or optimization, are treated at an introductory level in order to highlight the role of data visualization and 
                                            interactive exploration rather than to provide exhaustive methodological tutorials. Several of these subjects are developed in greater depth in my ",a("GitHub", href = "https://github.com/kevinhuads", target = "_blank") ,"repositories, 
                                            where the modelling details and technical aspects are explored more fully."),
                                          p("Data visualization is treated as a core part of the analytical process rather than a final cosmetic step. 
                                          Robust models only become useful when their results can be communicated clearly and interpreted by different audiences. 
                                           R Shiny provides an effective framework for this purpose, 
                                            since it makes it possible to explore both the data and the models interactively directly in the browser."),
                                          p("The sections of this application are independent from one another and are intended to be accessible to both technical and non technical readers. 
                                            You are invited to navigate through the different topics using the sidebar on the left.")
                                          )),

                            width = 12)
                      )
                    )
                    
)