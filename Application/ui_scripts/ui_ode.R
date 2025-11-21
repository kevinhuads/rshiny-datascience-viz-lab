tab_ode = shinydashboard::tabItem(
  tabName = "ODE",
  fluidPage(
    fluidRow(
      box(
        column(12,h1(tags$b("Differential Equations"),': Modeling the Epidemiology of the Coronavirus')),
        column(12,hr()),
        column(12,
          column(12,
            p("With the recent 2020 coronavirus pandemic, I felt it was important to analyse the behaviour of a virus within a population.
              I wanted to show in this section that with some simple notions of mathematics, we can have an pretty good model of what is going to 
              happen, how many people are going to be infected, how fast it will spread and how many people are going to die.
              The good news is almost any virus, no matter how lethal or contagious it can be, the final outcome depends on the action of the population.
              The bad news is it requires a certain discipline and some sacrifices from everyone."),
            p("An epidemy can begin with only one person infected, it doesn't need more than that to spread. 
              We can easily understand that if that person is in interaction with nobody, he will either recover and kill the virus
              or die with the virus. The reality is much more complicated than that as it is very unlikely that a single person is totally isolated."),
            p("You might have seen it during the outbreak, but the main objective of the governement
              and all the measures taken was to", em('flatten the curve'),'but what does that mean concretely?'),
            p("Let's look at the maths behind: to understand the behaviour of a virus, we often use a model SIR but here we will more precisely use a model 
              SIRD (Susceptible, Infected, Recovered, Dead) given that:"),
            tags$ul(
              tags$li("Infected people can infect Susceptible people"),
              tags$li("Infected people can either Recover or Die")
            ),
            p("Now, let's translate this with mathematic formula:"),
            p("$$\\frac{dS}{dt} = - \\frac{\\beta * I * S}{N}$$"),
            p("$$\\frac{dI}{dt} = \\frac{\\beta * I * S}{N} - \\gamma I$$"),
            p("$$\\frac{dR}{dt} = \\gamma * \\delta I$$"),
            p("$$\\frac{dD}{dt} = \\gamma * (1-\\delta) I$$"),
            tags$ul(
              tags$li("\\(\\frac{d}{dt}\\) means variation each day (for example at the day \\(d\\) we will add \\(\\frac{dI}{dt}\\) to \\(I\\) to have the results at the day \\(d + 1\\)"),
              tags$li("\\(\\beta\\) is the coefficient that represent the contagiousness of the virus. 
                      We can break it down as the product of two parameters: \\(\\beta = P*C\\)"),
              tags$ul(
                tags$li("\\(C\\) the average number of persons that we interact with each day"),
                tags$li("\\(P\\) the probability that an Infected infect a Susceptible when he interacts with him")
              ),
              tags$li("\\(\\gamma\\) is the probability of not being infected anymore (thus of either dying or recovering),
                      in our case, we will consider it as the inverse of the durability of the virus D : \\(\\gamma = \\frac{1}{D}\\)"),
              tags$li("\\(\\delta\\) the probability of dying if you have been infected")
              )), 
          column(12,hr()),
          column(6,    
            p("Enough with the theory, let's see what the practice gives. The graphic on the right shows the 
              evolution of the groups Infected and Death (but you can show the others too by clicking on the legend on the bottom) 
              over the time and you click on the radio button to see the impact of an eventual intervention"),
            p("An intervention from the governement is basically reducing both the average number of people we interact with (for example by imposing a lockdown) 
              and the probability of infection (for example by raising awareness about washing hands, not shaking hands with each other, not touching face and so on...).
              The defaults parameters on the graphs are:"),
            tags$ul(
              tags$li("An initial population of 1M people with 10 infected people at day 0"),
              tags$li("A 14-days disease with a probability of death of 3%"),
              tags$li("100 average interactions per day and 1% of probability of infection without intervention"),
              tags$li("30 average interactions per day and 0.3% of probability of infection with intervention")
            ),
            p("We can already see the huge difference in term of number of deaths (30000 without intervention against 20552 with intervention) which can 
              be explained by the fact that less people will be infected."),
            p("This number of deaths is still huge as it doesn't take in count medical treatments. Moreover, you might have notice is that the pandemic with last longer with intervention 
              (215 days against 618 days). The main objective of the intervention is to slow down the spread of the virus so the infected people can be treated each day."),
            p("Let's consider now the hospitals of the population which have a capacity of maximum 80000 people per days and that people under treatment have a probability of dying
              reduced to 20% of the initial probability (thus 0.03*0.02 = 0.0006 = 0.6%). You can see that without intervention, the hospitals are quickly overwhelmed from the 14th day and at the 20th day, 
              they have more than 700000 they can't treat whereas with treatment, the peak reaches only 17000 above the limit which can still be bearable. The difference in term of deaths here is
              22781 against 4949. See how much we can save lives just by reducing our interactions and with social distancing !"),
            p("On the plot below, you have more freedom to play with the parameters. Another thing we can talk about is the vaccine: Vaccined people are basically like recovered people 
              since they are not infected and they can't be infected anymore. To add vaccined people, you can add initial recovered people.")),
          column(6,
                 column(4,style='padding:0px;',
                        boxPad(color = "gray",
                               materialSwitch(inputId = "ode_intervention",label = "Intervention", value = FALSE,status = "primary"),
                               materialSwitch(inputId = "ode_hop",label = "Hospital Capacity", value = FALSE,status = "warning"))
                 ),
                 column(4,style='padding:0px;',valueBoxOutput("ode_first_length",width = 12)),
                 column(4,style='padding:0px;',valueBoxOutput("ode_first_death",width = 12)),
                 column(12,style='padding:0px;',highchartOutput('ode_first_plot',height = "500px")))),
        column(12,br()),
        column(12,hr()),
        column(12,
          column(4,style='padding:0px;',
                 column(6,actionBttn(inputId = "ode_help",label =  "Tutorial",size = "lg", 
                                     color = "primary",icon = icon("question"), 
                                     style = "material-flat")),    
                 column(6,radioGroupButtons( "ode_type",label = NULL,justified = TRUE, status = "warning",size = "lg",
                                             choices = c("<i class='fa fa-area-chart'></i> Area" = "area",
                                                         "<i class='fa fa-line-chart'></i> Line" = "line"))),
                 column(12,pickerInput("ode_display",label = "Display by default", 
                                       choices = c("Susceptible","Infected","Recovered","Dead"),
                                       selected = c("Susceptible","Infected","Recovered","Dead"),
                                       multiple = TRUE)),
                 column(12,style='padding:0px;',
                        column(6,
                               numericInput('ode_int',"Average Interactions",
                                            value = 100, min = 0,max = 10000,step = 1),
                               numericInput('ode_probinf',"Probability of Infection",
                                            value = 0.005, min = 0,max = 1,step = 0.0001)),
                        column(6,
                               numericInput('ode_dur',"Duration of the Disease",
                                            value = 14, min = 1,max = 1000),
                               numericInput('ode_probdea',"Probability of Death",
                                            value = 0.04, min = 0,max = 1,step = 0.01))),
                 column(12,hr()),
                 column(6,sliderTextInput('ode_cap',"Hospital Capacity",choices = sort(c(10^(3:6), 5*10^(3:5))),selected = 5*10^4)),
                 column(6,sliderInput('ode_red_cap',"Reduction under Treatment", min = 0,max = 100,step = 5, post  = " %", value = 50)),
                 column(6,sliderTextInput('ode_i0',"Initial Infected",choices = 10^(0:6),selected = 10)),
                 column(6,sliderInput('ode_vaccin',"Percentage Vaccined", min = 0,max = 100,step = 5, post  = " %", value = 0))
                 # column(6,sliderTextInput('ode_r0',"Initial Recovered",choices = c(0,10^(0:7)),selected = 0)),
                 # column(6,sliderTextInput('ode_d0',"Initial Dead",choices = c(0,10^(0:7)),selected = 0))
                 ),
          column(8,
                 valueBoxOutput("ode_length",width = 4),
                 valueBoxOutput("ode_peak",width = 4),
                 valueBoxOutput("ode_deaths",width = 4),
                 highchartOutput('ode_highchart',height = "500px")
          )),
        width = 12)
    )))
