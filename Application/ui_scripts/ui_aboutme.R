tab_aboutme = tabItem(
  tabName = "About",
  fluidPage(
    fluidRow(
      column(
        5, offset = 0, style = "padding:0px;",
        box(
          title = strong("Overview"),
          solidHeader = TRUE,
          status = "info",
          column(
            12,
            div(
              shiny::img(
                src = "myphoto2.jpg",
                height = "250px",
                weight = "250px"
              ),
              style = "text-align: center;"
            )
          ),
          column(12, hr()),
          column(
            12,
            p("I'm a Senior Data Scientist with 6 years of experience in diversified fields (consultancy, bank, insurance, social medias, payments...) 
              dedicated to dedicated to delivering innovative and effective data solutions and assessments.
              I have strong skills in R (+RMarkdown and RShiny) and Python and I am proficient at analyzing data, building machine learning models and creating visualizations."),
            p("In recent years I have expanded my expertise toward production-grade data science, with practical experience in MLOps practices, model deployment and monitoring, cloud-based workflows and API development. 
              I work with Python, modern deep learning frameworks and orchestration tools to design solutions that integrate smoothly into business environments. 
              This evolution has strengthened my ability to bridge analytical work with operational needs and to deliver scalable, reliable and maintainable systems."),
            p("I strongly believe that we can tell stories with data and derive valuable content from them. Today’s technologies allow us to build robust models that can make very accurate predictions."),
            tagList(
              p("For more informations, you can check my socials:"),
              
              div(
                style = "text-align: center; margin-top: 10px; font-size: 4rem;",
                
                a(
                  href = "https://github.com/kevinhuads",
                  target = "_blank",
                  icon("github"),
                  style = "margin-right: 20px;"
                ),
                
                a(
                  href = "https://www.linkedin.com/in/huakevin/",
                  target = "_blank",
                  icon("linkedin")
                )
              )
            )
          ),
          width = 12
        ),
        box(
          title = strong("Skills"),
          solidHeader = TRUE,
          status = "warning",
          column(
            7,
            div(strong("Coding"), style = "text-align: center;"),
            highchartOutput("about_it")
          ),
          column(
            5,
            div(strong("Languages"), style = "text-align: center;"),
            br(),
            br(),
            shinyWidgets::progressBar(
              id = "sk_fr", value = 100,
              status = "success", title = "French", size = "xs"
            ),
            shinyWidgets::progressBar(
              id = "sk_eng", value = 96,
              status = "success", title = "English", size = "xs"
            ),
            shinyWidgets::progressBar(
              id = "sk_port", value = 88,
              status = "success", title = "Portuguese", size = "xs"
            ),
            shinyWidgets::progressBar(
              id = "sk_esp", value = 74,
              status = "primary", title = "Spanish", size = "xs"
            ),
            shinyWidgets::progressBar(
              id = "sk_ger", value = 68,
              status = "primary", title = "German", size = "xs"
            ),
            shinyWidgets::progressBar(
              id = "sk_ch", value = 25,
              status = "warning", title = "Chinese", size = "xs"
            )
          ),
          column(12,
                 h4("Frameworks and tools"),
                 
                 p(strong("Machine learning & modeling")),
                 div(
                   style = "font-size:2rem; margin-bottom:10px;",
                   framework_icon("scikit-learn", "https://scikit-learn.org", "project-diagram"),
                   framework_icon("PyTorch", "https://pytorch.org", "fire"),
                   framework_icon("XGBoost", "https://xgboost.readthedocs.io", "bolt"),
                 ),
                 
                 p(strong("Data manipulation & visualisation")),
                 div(
                   style = "font-size:2rem; margin-bottom:10px;",
                   framework_icon("pandas", "https://pandas.pydata.org", "table"),
                   framework_icon("NumPy", "https://numpy.org", "superscript"),
                   framework_icon("dplyr", "https://dplyr.tidyverse.org", "r-project"),
                   framework_icon("ggplot2", "https://ggplot2.tidyverse.org", "chart-area"),
                   framework_icon("plotly", "https://plotly.com", "chart-line"),
                   framework_icon("highchart", "https://www.highcharts.com", "chart-bar")
                 ),
                 
                 p(strong("Apps & APIs")),
                 div(
                   style = "font-size:2rem; margin-bottom:10px;",
                   framework_icon("Streamlit", "https://streamlit.io", "globe"),
                   framework_icon("R Shiny", "https://shiny.posit.co", "r-project"),
                   framework_icon("FastAPI", "https://fastapi.tiangolo.com", "code")
                 ),
                 
                 p(strong("MLOps, orchestration & experiment tracking")),
                 div(
                   style = "font-size:2rem; margin-bottom:10px;",
                   framework_icon("MLflow", "https://mlflow.org", "flask"),
                   framework_icon("DVC", "https://dvc.org", "code-branch"),
                   framework_icon("Apache Airflow", "https://airflow.apache.org", "wind"),
                   framework_icon("Docker", "https://www.docker.com", "docker"),
                   framework_icon("Kubernetes", "https://kubernetes.io", "server")
                 ),
                 
                 p(strong("Cloud & data platforms")),
                 div(
                   style = "font-size:2rem; margin-bottom:10px;",
                   framework_icon("GCP", "https://cloud.google.com", "cloud"),
                   framework_icon("BigQuery", "https://cloud.google.com/bigquery", "database")
                 ),
                 
                 p(strong("Version control, CI/CD & collaboration")),
                 div(
                   style = "font-size:2rem; margin-bottom:10px;",
                   framework_icon("Git", "https://git-scm.com", "code-branch"),
                   framework_icon("GitHub", "https://github.com", "github"),
                   framework_icon("GitLab", "https://gitlab.com", "gitlab"),
                   framework_icon("GitHub Actions", "https://github.com/features/actions", "cogs")
                 )
          ),
          width = 12
        ),
        box(
          title = "Outside the office",
          status = "success",
          solidHeader = TRUE,
          userList(
            userListItem(
              image    = "icons_about/taekwondo.png",
              title    = "Taekwondo",
              subtitle = "Taekwondo has been my passion for more than 6 years. I couldn't live without it now.
              - Black Belt First Dan"
            ),
            userListItem(
              image    = "icons_about/marathon.jpg",
              title    = "Running",
              subtitle = "I love this feeling of always getting better every year, 
              competing with my self and fighting my own limits - Runned 4 Marathons (Paris, Lyon, Berlin and Rio de Janeiro)"
            ),
            userListItem(
              image    = "icons_about/travel.jpg",
              title    = "Travel",
              subtitle = "I've been living abroad for years and visited more than 20 countries.
              Discovering new cultures made me grow so much as a person!"
            ),
            userListItem(
              image    = "icons_about/books.png",
              title    = "Reading",
              subtitle = "I love reading books about personal development and sucess stories. 
              I will always be amazed about how human beings can push their limits to reach their goals."
            )
          ),
          width = 12
        )
      ),
      column(
        7, offset = 0, style = "padding:0px;",
        box(
          title = strong("Timeline"),
          solidHeader = TRUE,
          status = "primary",


          timelineBlock(
            width = 12,
            reversed = TRUE,
            # timelineEnd(color = "danger"),
            timelineItem(
              elevation = NULL,
              title = strong("Senior Data Scientist at Market Pay"),
              icon  = icon("gears"),
              color = "blue",
              time  = "Jun 20 - Aug 23",
              border = TRUE,
              footer = div(
                HTML(paste(
                  "Paris - France",
                  tags$img(src = "flags/fr.svg", width = 20, height = 15)
                )),
                style = "text-align: right;"
              ),
              tags$ul(
                tags$li("Forecast sales by products and by stores using Time Series models (Prophet, ARIMA), optimizing inventory management."),
                tags$li("Built models aiming at optimizing pricing and net sales"),
                tags$li("Created models to monitor KPIs to improve products efficiencies")
              )
            ),
            timelineLabel("Oct 2020 - Return to France", color = "light-blue"),
            timelineItem(
              elevation = NULL,
              title = strong("Data Scientist at Carrefour Brazil"),
              icon  = icon("gears"),
              color = "blue",
              time  = "Oct 18 - Mar 20",
              border = TRUE,
              footer = div(
                HTML(paste(
                  "São Paulo - Brasil",
                  tags$img(src = "flags/br.svg", width = 20, height = 15)
                )),
                style = "text-align: right;"
              ),
              tags$ul(
                tags$li("Created flexible and interactive visualisation NLP tool to build a Barometer of Verbatims to evaluate customers satisfaction about Carrefour's stores and promotions."),
                tags$li("Developed mathematical models to optimize advertising campaign spend and maximize return on investment."),
                tags$li("Scrapped online social network data and made Sentiment Analysis model to monitor in real-time the brand's reputation"),
                tags$li("Managed huge databases (on both Oracle and Phoenix) with SQL Queries and used machine learning models to monitor the databases and ensure their quality")
              )
            ),
            timelineLabel("Oct 2018 - Departure for Brazil", color = "light-blue"),
            timelineItem(
              elevation = NULL,
              title = strong("Researcher at the Data Science Institute - Imperial College London"),
              icon  = icon("gears"),
              color = "blue",
              time  = "Oct 17 - Sep 18",
              border = TRUE,
              footer = div(
                HTML(paste(
                  "London - United Kingdom",
                  tags$img(src = "flags/gb.svg", width = 20, height = 15)
                )),
                style = "text-align: right;"
              ),
              p(
                "Developed a model predicting fake news on Twitter on the 2016 US elections tweets ",
                tags$ul(
                  tags$li("Managed the big database of all the metadata with MongoDB"),
                  tags$li("Compared the results of the different data science algorithms and the grid searches"),
                  tags$li("Improved the models with Natural Language Processing (sentiment analysis, emojis analysis, 
                          unsupervised and supervised deep learning…)")
                )
              )
            ),
            timelineItem(
              elevation = NULL,
              title = strong("Started to build my Shiny Blog"),
              icon  = icon("lightbulb"),
              color = "black",
              time  = "Feb 18 - now",
              border = TRUE,
              footer = NULL,
              p(
                "I had the ambition to create a complete interactive blog on Shiny showing all the outstanding
                things we could do only with R"
              )
            ),
            timelineLabel("Oct 2017 - Departure for the United Kingdom", color = "light-blue"),
            timelineItem(
              elevation = NULL,
              title = strong("Data Scientist Intern at AXA Global Re"),
              icon  = icon("gears"),
              color = "blue",
              time  = "Apr 17 - Sep 17",
              border = TRUE,
              footer = div(
                HTML(paste(
                  "Paris - France",
                  tags$img(src = "flags/fr.svg", width = 20, height = 15)
                )),
                style = "text-align: right;"
              ),
              tags$ul(
                tags$li("Development of a tool (on R Shiny) allowing the visualization and the monitoring of the insurance contracts. 
                        This tool was flexible and adaptable to any database of the group's entities"),
                tags$li("Study of the performance of different models predicting future claims given the past claims and the clients' characteristics"),
                tags$li("Use of NLP and OCR techniques to classify mails in claims management procedures")
              )
            ),
            timelineLabel("Mar 2017 - Last course at Ecole Centrale de Lyon", color = "red"),
            timelineItem(
              elevation = NULL,
              title = strong("Graduated From Ecole centrale de Lyon (MEng)"),
              icon  = icon("graduation-cap"),
              color = "red",
              time  = "Sep 13 - Mar 17",
              border = TRUE,
              footer = div(
                HTML(paste(
                  "Lyon - France",
                  tags$img(src = "flags/fr.svg", width = 20, height = 15)
                )),
                style = "text-align: right;"
              ),
              tags$ul(
                tags$li("Mathematics and Statistics"),
                tags$li("Big Data and Computer Science"),
                tags$li("General Engineering Courses")
              )
            ),
            timelineItem(
              elevation = NULL,
              title = strong("Graduated From Université Claude Bernard de Lyon 1 (MSc)"),
              icon  = icon("graduation-cap"),
              color = "red",
              time  = "Sep 16 - Mar 17",
              border = TRUE,
              footer = div(
                HTML(paste(
                  "Lyon - France",
                  tags$img(src = "flags/fr.svg", width = 20, height = 15)
                )),
                style = "text-align: right;"
              ),
              p("Master of Data Science - Double Degree with Ecole centrale de Lyon")
            ),
            timelineLabel("Aug 2016 - End of the Gap Year", color = "light-blue"),
            timelineItem(
              elevation = NULL,
              title = strong("Statistician - Analyst Intern at Banque de France"),
              icon  = icon("gears"),
              color = "blue",
              time  = "Mar 16 - Aug 16",
              border = TRUE,
              footer = div(
                HTML(paste(
                  "Paris - France",
                  tags$img(src = "flags/fr.svg", width = 20, height = 15)
                )),
                style = "text-align: right;"
              ),
              p(
                "I was a part of an European project (Target 2 Securities) where I was asked to provide an analysis of the participants' behavior to highlight potential areas to improve the settlement of the engine.
                4 major goals have been established and realized for my internship:",
                tags$ul(
                  tags$li("Provide reusable tools to analyze data from production"),
                  tags$li("Analyze productions data to understand the use of T2S by the financial actors"),
                  tags$li("Simulate new data from the analysis to test the settlement algorithms"),
                  tags$li("Compare generated data sets with the statistic established profile")
                )
              )
            ),
            timelineItem(
              elevation = NULL,
              title = strong("Actuary - Data scientist consulting intern at Jalma"),
              icon  = icon("gears"),
              color = "blue",
              time  = "Jun 15 - Dec 15",
              border = TRUE,
              footer = div(
                HTML(paste(
                  "Paris - France",
                  tags$img(src = "flags/fr.svg", width = 20, height = 15)
                )),
                style = "text-align: right;"
              ),
              p(
                "In a team of actuaries and data scientists, I participated in the conception of several modeling tools: ",
                tags$ul(
                  tags$li("Study by Data-mining of an optical lenses database"),
                  tags$li("Use of the Text-mining to classify disease treatments"),
                  tags$li("Redaction of functional requirements for different softwares"),
                  tags$li("Automation (under R) of data base's updates"),
                  tags$li("Conception of a spraying tool in order to predict future charges")
                )
              )
            ),
            timelineLabel("May 2015 - Beginning of the Gap Year", color = "light-blue"),
            timelineLabel("Sep 2013 - Entrance at Ecole centrale de Lyon", color = "red"),
            timelineStart()
          ),
          width = 12
        )
      )
    )
  )
)
