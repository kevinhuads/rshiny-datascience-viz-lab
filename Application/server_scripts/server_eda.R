# Marathon and Olympics ---------------------------------------------------



# output$mara_table = DT::renderDataTable({
#   datatable(mara_subtable, 
#             extensions = 'Scroller', options = list(
#     deferRender = TRUE,
#     scrollY = 500,
#     scroller = TRUE,
#     scrollX = TRUE,
#     autoWidth = TRUE
#   ), 
#   rownames = FALSE)
# })
# 
# marathon_df = reactive({
#   if (input$mara_year == 'All'){
#     marathon_df = suppressWarnings(bind_rows(marathon_list))
#   } else {
#     marathon_df = marathon_list[[paste0('year',input$mara_year)]]
#   }
#   
#   marathon_df = merge(marathon_df,marathon_country[,-2],by.x = 'Country', by.y = 'Abb', all.y = FALSE)
#   if(input$mara_country != 'World'){
#     marathon_df = marathon_df[marathon_df$Continent == input$mara_country,]
#   }
#   return(marathon_df)
# })
# 
# output$mara_geo = renderHighchart({
#   
#   if (input$mara_year == 'All'){
#     df1 = suppressWarnings(bind_rows(marathon_list_country))
#     df1 = aggregate(df1$Count, by = list(df1$Abb,df1$Country,df1$Continent),FUN = sum, simplify= FALSE)
#     df1 = df1[order(df1[,1]),c(1,4,2,3)]
#     colnames(df1) = colnames(marathon_list_country[[1]])
#     df1$Count = unlist(df1$Count)
#   } else {
#     df1 = marathon_list_country[[paste0('year',input$mara_year)]]
#   }
#   
#   if (input$mara_country == 'World'){
#     df = aggregate(df1$Count, by = list(df1$Continent),FUN = sum)
#   } else {
#     df = df1[df1$Continent%in%input$mara_country,c('Country','Count')]
#   }
#   
#   colnames(df) = c('name','y')
#   df = df[order(df$y,decreasing = TRUE)[1:min(20,nrow(df))],]
#   
#   df = df  %>% 
#     mutate(
#       z = y,
#       e = y/10,
#       value = y,
#       color = hue_pal()(nrow(df)),
#       segmentColor = hue_pal()(nrow(df))
#     )
#   
#   highchart() %>%
#     hc_chart(type =input$mara_geotype) %>% 
#     hc_xAxis(categories = df$name) %>% 
#     hc_add_series(df, showInLegend = FALSE) 
# })
# 
# output$mara_pyramid = renderHighchart({
#   x = marathon_df()
#   y = as.data.frame(table(x$Age,x[,'M.F']))
#   colnames(y) = c('Age','Gender','Population')
#   
#   inp_break = as.numeric(input$mara_pyrabreak)
#   if (inp_break == 2){
#     breaks_ = seq(1,201,by = 2)
#     width_ = 12
#   } else {
#     breaks_ = seq(0,200,by = 5)
#     width_ = 26
#   }
#   
#   y$Age = droplevels(cut(as.numeric(as.character(y$Age)),
#                          breaks = breaks_,include.lowest = TRUE))
#   levels(y$Age) = paste0('[',as.numeric(substr(levels(y$Age),2,3))+1,
#                          '-',as.numeric(substr(levels(y$Age),5,6)),']')
#   
#   if (inp_break == 5) levels(y$Age)[1] = '[18-20]'
#   
#   
#   y = aggregate(y$Population,by = list(y$Age,y$Gender),sum)
#   colnames(y) = c('Age','Gender','Population')
#   y$Population = y$Population/sum(y$Population)
#   y = spread(y, Gender, Population)
#   colnames(y)[2:3] = c('Female','Male')
#   
#   highchart() %>% 
#     hc_chart(type = 'bar')  %>% 
#     hc_xAxis(list(categories = y$Age,reversed = FALSE,labels = list(step = 1)),
#              list(categories = y$Age,reversed = FALSE,labels = list(step = 1),
#                   opposite = TRUE, linkedTo = 0)) %>%
#     hc_yAxis(title = NULL,labels = list(
#       formatter =  JS("function () {return Highcharts.numberFormat(Math.abs(100*this.value),1) + '%';}"))) %>%
#     hc_add_series(name = 'Men',color = rgb(61/255,89/255,171/255,0.6),data = -y$Male) %>%
#     hc_add_series(name = 'Women',color =rgb(238/255,59/255,59/255,0.6),data = y$Female) %>%
#     hc_plotOptions(series = list(stacking = 'normal',pointWidth = width_))  %>%
#     hc_tooltip(formatter = JS("function () {
#                               return '<b>' + this.series.name + ', age ' + this.point.category + '</b><br/>' +
#                               'Population: ' + Highcharts.numberFormat(Math.abs(100*this.point.y), 2) + '%';
#                               }"))  
# })
# 
# output$mara_dist = renderPlot({
#   df = marathon_df()
#   
#   Time = df[,input$mara_choice]
#   r = range(Time,na.rm = TRUE)
#   breaks_ = 0.25
# 
#   levels(df$M.F) = c('Women','Men')
#   if (r[2] - r[1] < 1) breaks_ = 1/12
#   if (r[2] - r[1] > 2.9) breaks_ = 1/2
#   if (r[2] - r[1] < 0.2) breaks_ = 1/60
#   r[1] = 0
#   r[2] = 20
# 
#   round_ = 5
#   x_lab = "Time to reach the Cap"
#   if (input$mara_choice == 'Pace'){
#     round_ = 1
#     x_lab = "Average Time to run 1 km"
#   }
#   if (input$mara_bysex == "Aggregated"){
#     if(input$mara_distplot == 'Density Plot'){
#       ggplot(df,aes(x=Time)) + 
#         geom_density(alpha=.4,color='white',fill = brewer.pal(9,'Set1')[3],size=1) +
#         labs(y = "",x =x_lab) +
#         scale_x_continuous(labels = function(t) to_hour(t,r =round_ ), breaks = seq(r[1],r[2],by = breaks_)) +
#         theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
#     } else {
#       ggplot(df,aes(y=Time)) +
#         geom_boxplot(fill = brewer.pal(9,'Set1')[3], alpha = 0.4) + coord_flip()+
#         labs(x = "",y =x_lab) +
#         scale_y_continuous(labels = function(t) to_hour(t,r =round_ ), breaks = seq(r[1],r[2],by = breaks_)) +
#         theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
#     }
#   } else {
#     if(input$mara_distplot == 'Density Plot'){
#       ggplot(df,aes(x=Time,fill=M.F)) + 
#         geom_density(alpha=.4,color='white',size=1) +
#         labs(y = "",x =x_lab) +scale_fill_brewer(palette='Set1') +
#         theme(legend.position='bottom',legend.direction='horizontal',legend.title=element_blank(),
#               axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
#         scale_x_continuous(labels = function(t) to_hour(t,r =round_ ), breaks = seq(r[1],r[2],by = breaks_))
#     } else {
#       ggplot(df,aes(y=Time,fill=M.F)) +
#         geom_boxplot(alpha = 0.4) + coord_flip()+
#         labs(x = "",y =x_lab) +scale_fill_brewer(palette='Set1') +
#         theme(legend.position='bottom',legend.direction='horizontal',legend.title=element_blank(),
#               axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
#         scale_y_continuous(labels = function(t) to_hour(t,r =round_ ), breaks = seq(r[1],r[2],by = breaks_))
#     }
#     
#   }
# })
# 
# output$mara_time = renderPlot({
#   marathon_sumlist = lapply(1:3,function(y){
#     
#     df = marathon_df()
#    
#     if (y == 2) df = df[df$M.F == "M",]
#     if (y == 3) df = df[df$M.F == "F",]
#     
#     marathon_sum = t(cbind(c(0,0,0,0,0,0,0),
#                            sapply(df[,marathon_list_times],
#                                   function(z) quantile(z,
#                                                        probs = c(0,0.05,0.25,0.5,0.75,0.95,1),
#                                                        na.rm = TRUE)))) %>%
#       as.data.frame()
#     colnames(marathon_sum) = c("Minimum","First_Vingtile","First_Quartile","Mean",
#                                "Third_Quartile","Last_Vingtile","Maximum")
#     marathon_sum$distance = c(0,5,10,15,20,21.097,25,30,35,40,42.195)
#     return(marathon_sum)
#   })
#   names(marathon_sumlist) = c("Both","Men","Women")
#   
#   marathon_sum = marathon_sumlist[[input$mara_sex]]
#   mcol = marathon_list_cols[input$mara_sex]
#   
#   ggplot(marathon_sum, aes(x = distance, y =Mean)) + 
#     geom_ribbon(aes(ymin = Minimum, ymax = Maximum, fill = "All Runners")) +
#     geom_ribbon(aes(ymin = First_Vingtile, ymax = Last_Vingtile, 
#                     fill = "Without the 05% slowest and fastest"), show.legend = FALSE) +
#     geom_ribbon(aes(ymin = First_Quartile, ymax = Third_Quartile, 
#                     fill = "Without the 25% slowest and fastest"), show.legend = FALSE) +
#     scale_fill_manual(name = NULL, 
#                       values = sapply(c(0.4,0.7,1),function(x) adjustcolor(mcol, alpha.f = x))) +
#     guides(fill = guide_legend(override.aes = list(alpha = c(0.4,0.7,1)))) + 
#     geom_line(aes(y = Mean)) +    
#     geom_point(size = 2) + 
#     ylab(NULL) +
#     theme(legend.position='bottom',legend.direction='horizontal') +
#     scale_x_continuous(labels = round2, breaks = marathon_sum$distance) +
#     scale_y_continuous(labels = to_hour, breaks = seq(0,20,by = 0.5)) +
#     geom_text(x=marathon_sum$distance-1*c(1,1,1,1,1,-1,1,1,1,1,-0.2), 
#               y=marathon_sum$Mean+0.3*c(1,1,1,1,1,-1,1,1,1,1,-1), label=to_hour(marathon_sum$Mean,r =1))
#   
# })
# 
# output$mara_btotal <- renderValueBox({
#   valueBox(
#     nrow(marathon_df()), 
#     'Runners', 
#     icon = icon("search",lib = 'glyphicon'),
#     color = "black",
#     width = 12
#   )
# })
# 
# output$mara_bfast <- renderValueBox({
#   valueBox(
#     to_hour(min(marathon_df()$Official.Time),r = 1), 
#     'Fastest Time', 
#     icon = icon("search",lib = 'glyphicon'),
#     color = "blue",
#     width = 12
#   )
# })
# 
# output$mara_btime <- renderValueBox({
#   valueBox(
#     to_hour(mean(marathon_df()$Official.Time),r = 1), 
#     'Average Time', 
#     icon = icon("search",lib = 'glyphicon'),
#     color = "orange",
#     width = 12
#   )
# })
# 
# output$mara_bfem <- renderValueBox({
#   valueBox(
#     paste(round(100*sum(marathon_df()$M.F == 'F')/nrow(marathon_df()),2),'%'), 
#     'Women', 
#     icon = icon("percent",lib = 'glyphicon'),
#     color = "red",
#     width = 12
#   )
# })
# 
# output$mara_bage <- renderValueBox({
#   valueBox(
#     round(mean(marathon_df()$Age),2), 
#     'Average Age', 
#     icon = icon("search",lib = 'glyphicon'),
#     color = "green",
#     width = 12
#   )
# })
# 
# output$og_winter <- renderHighchart({
#   temp = eda_winter
#   temp$df = markers_scrap(temp$df,input$og_flag)
#   highchart_colored(temp, type_ = "areaspline",x = "year",y = input$og_var,name = "host_country",
#                     title = "Winter Olympic Games per year",
#                     color = "white",orient = 15,halosize = 30)
# })
# 
# output$og_summer <- renderHighchart({
#   temp = eda_summer
#   temp$df = markers_scrap(temp$df,input$og_flag)
#   highchart_colored(temp, type_ = "areaspline",x = "year",y = input$og_var,name = "host_country",
#                     title = "Summer Olympic Games per year",
#                     color = "white",orient = 15,halosize = 30)
# })
# 
# output$og_winter2 <- renderHighchart({
#   highchart_colored(eda_winter2, type_ = "column",
#                     x = "rank",y = input$og_var2,name = "nation",
#                     title = "Winter Olympic Games all-time medal table",
#                     color = "white",orient = 300,halosize = NULL)
# })
# 
# output$og_summer2 <- renderHighchart({
#   highchart_colored(eda_summer2, type_ = "column",
#                     x = "rank",y = input$og_var2,name = "nation",
#                     title = "Summer Olympic Games all-time medal table",
#                     color = "white",orient = 300,halosize = NULL)
# })



# Football ----------------------------------------------------------------

output$foot_sum = renderUI({
  y_temp = eda_summaries[[input$foot_compet]]
  y = y_temp$y
  i = y_temp$i
  t = y_temp$t
  b = box(title = 'Summary',
      solidHeader = TRUE,
      status = "warning",
      boxProfile(
        src = i,
        title = t,
        subtitle = NULL,
        lapply(1:nrow(y),function(i){
          boxProfileItem(title = y$title[i],description = y$value[i])
        })),
      width =12)
  return(b)
})

output$foot_wctime <- renderHighchart({
  temp = eda_wc
  temp$df = markers_scrap(temp$df,input$foot_wc1_winner)
  temp$df$flagicon = temp$df$pic
  
  highchart_colored(temp, type_ = "areaspline",
                    x = "year",y = input$foot_wc,name = input$foot_wc1_winner,transp = FALSE,
                    title = paste(gsub("_"," ",simpleCap(input$foot_wc)),"per World Cup"),
                    color = "white",orient = 15,halosize = 30,
                    borderWidth = 3, borderCol = "rgba(60,155,60,0.65)")%>% 
    hc_credits(
      enabled = TRUE,
      text = "Data from Wikipedia",
      href = "https://en.wikipedia.org/wiki/FIFA_World_Cup"
    )
})

output$foot_wctop <- renderHighchart({
  foot_temp = eda_wc2
  foot_temp$df = foot_temp$df[foot_temp$df[,input$foot_wc2]>0,]
  highchart_colored(foot_temp, type_ = "column",transp = FALSE,bgheader = TRUE,
                    x = "rank",y = input$foot_wc2,name = "nation",
                    title = "Records per country",bgfont = 0.75,
                    color = "white",altitude = -30,halosize = NULL,
                    borderWidth = 3, borderCol = "rgba(60,155,60,0.65)")
})

output$foot_wcscore = renderHighchart({
  highchart_colored(eda_wc3, type_ = "column",transp = FALSE,bgheader = TRUE,
                    x = "rank",y = 'goals',name = "player",title = "World Cup Top Goal Scorers",
                    color = "white",orient = 200,halosize = NULL,
                    borderWidth = 3, borderCol = "rgba(60,155,60,0.65)") %>% 
    hc_yAxis(
      lineWidth = 1,tickWidth = 1,tickInterval = 1,
      min = 8,max = 18,gridLineWidth = 0) %>%
    hc_xAxis(labels = list(style = list(fontSize = 10)))
})

output$foot_cltime <- renderHighchart({
  cl_temp = eda_cl
  cl_temp$df = cl_temp$df[(cl_temp$df$year>=input$foot_cl_num[1])&(cl_temp$df$year<=input$foot_cl_num[2]),]
  
  if (input$foot_cl_winner == "country"){
    var = "country" 
    cl_temp$tt = gsub(100,24,cl_temp$tt)
  } else {
    var = "url"
  }
  icleng = (input$foot_cl_num[2]-input$foot_cl_num[1])/(max(eda_cl$df$year) - min(eda_cl$df$year))
  icsize = round(15*icleng + 50*(1-icleng))
  cl_temp$df = markers_scrap(cl_temp$df,var = var,w = icsize,h = icsize)
  
  highchart_colored(cl_temp, type_ = "areaspline",transp = FALSE,
                    x = "year",y = input$foot_cl,name = "winner",bgfont = 0.9,
                    title = paste(gsub("_"," ",simpleCap(input$foot_cl)),"per Champions League"),
                    color = "white",orient = 15,halosize = 30,
                    borderWidth = 3, borderCol = "rgba(25,25,210,0.45)")%>% 
    hc_credits(
      enabled = TRUE,
      text = "Data from Wikipedia",
      href = "https://en.wikipedia.org/wiki/UEFA_Champions_League"
    )
})

output$foot_cltop <- renderHighchart({
  if (input$foot_cl_winner == "country") foot_temp = eda_cl2_country else foot_temp = eda_cl2
  foot_temp$df = foot_temp$df[1:min(15,nrow(foot_temp$df)),]
  
  highchart_colored(foot_temp, type_ = "column",transp = FALSE,bgheader = TRUE,bgfont = 0.9,
                    x = "rank",y = input$foot_cl2,name = input$foot_cl_winner,
                    title = paste0("Records per ",input$foot_cl_winner),
                    color = "white",halosize = NULL,
                    borderWidth = 3, borderCol = "rgba(25,25,210,0.45)") %>% 
    hc_yAxis(lineWidth = 1,tickWidth = 1,tickInterval = 2,gridLineWidth = 0) 
})

output$foot_clscore = renderHighchart({
  if (input$foot_cl3 == "goals_scorers"){
    eda_34 = eda_cl3
    y_ = "goals"
    min_ = 0
  } else {
    eda_34 = eda_cl4
    y_ = "apps"
    min_ = 100
  } 
  
  highchart_colored(eda_34, type_ = "column",transp = FALSE,bgheader = TRUE,
                    x = "rank",y = y_,name = "player",
                    title = paste("Champions League Top",input$foot_cl3),
                    color = "white",orient = 250,altitude = -30,halosize = NULL,
                    borderWidth = 3, borderCol = "rgba(25,25,210,0.45)") %>% 
    hc_yAxis(min = min_,lineWidth = 1,tickWidth = 1,tickInterval = 10,gridLineWidth = 0) %>%
    hc_xAxis(labels = list(style = list(fontSize = 10)))
})


observeEvent(input$foot_help,{
  steps = data.frame(
    element = c(NA,"#foot_compet + .selectize-control"),
    intro = c(
      "<h4>Football Web Scraping Methodology</h4> 
      Web scrapping is a technique used to extract data from the Web. <br>
      In this section we are scraping data on Wikipedia about football competitions. <br> <br>
      You can navigate in this tutorial either by clicking on <em>Next</em>,<em>Previous</em> and <em>Quit tutorial</em> 
      or by using the arrows left and right of your keyboard",
      "<h4>Choosing the competition</h4> 
      Here, we will particularly focus on the FIFA World Cup and the UEFA Champions League.
      You can use this select input to choose which competition to display"))
  
  if (input$foot_compet == "wc"){
    steps_compet = data.frame(
      element = c("#foot_wctime","#foot_wc","#foot_wc1_winner","#foot_wctop","#foot_wc2","#foot_wcscore"),
      intro = c(
        "<h4>Building the first plot</h4> You have on this plot the winner for each edition, 
        if you hover the mouse on the plot, you will have more details about the competition. <br>
        The data have been scraped from the main Wikipedia page of the FIFA World Cup
        and we used the hyperlinks to the respective pages of each edition to complete with additional details",
        "<h4>Adding Shiny Widgets</h4> You can change here the Y axis for the plot. (by default, it shows
        the number of matches for each edition)",
        "<h4>Scraping flags for each country</h4> 
        You can choose here the flag displayed on the plot (either the winner or the host country). <br>
        To get the flags of each country, we matched each country's name with their ISO2c codes and then we pasted the code to
        complete an hyperlink to a github reposity of the user <i>tugmaks</i> that provide publicly almost all the flags 
        given their ISO2c codes.",
        "<h4>Displaying the top nations</h4>
        You have the ranking of the nations with the most World Cups, this comes from another table on the same Wikipedia page. <br>
        You can also hover each flag to get more information about the nation. These extra informations also have been extracted by
        getting the hyperlinks on each nation and taking the summary on each association's page",
        "<h4>Adding Shiny Widgets</h4> You can choose to display either the cups won or the finals reached",
        "<h4>Displaying the all time best scorers</h4>
        Here you have a ranking of the all-time top scorers (every player that has scored at least 10 goals in his career) 
        for which the data also comes from another table of the page"
    ))
  } else {
    steps_compet = data.frame(
      element = c("#foot_cltime","#foot_cl","#foot_cl_winner","#foot_cl_num","#foot_cltop","#foot_cl2","#foot_clscore","#foot_cl3"),
      intro = c(
        "<h4>Building the first plot</h4> You have on this plot the winner for each edition, 
        if you hover the mouse on the plot, you will have more details about the competition. <br>
        The data have been scraped from the main Wikipedia page of the UEFA Champions League
        and we used the hyperlinks to the respective pages of each edition to complete with additional details",
        "<h4>Adding Shiny Widgets</h4> You can change here the Y axis for the plot. (by default, it shows
        the average number of goals per match for each edition)",
        "<h4>Scraping flags and logos for each club</h4> 
        For each winner, on the table of the wikipedia page, there is a link to the page of the winner club of each edition.
        We did a loop on these links to automatically extract the logo for each club. <br>
        You can choose here the icon displayed on the plot (either the logo of the club winner or the flag of its country).",
        "<h4>Adding more flexibility </h4> You can choose the date range here, the logo size will adjust with the number of years displayed. <br>
        Note that before 1993, the <em>Champions League </em> was called the <em>European Cup</em>",
        "<h4>Displaying the top clubs</h4>
        You have the ranking of the clubs with the most Champions Leagues, this comes from another table on the same Wikipedia page. <br>
        You can also hover each logo (or flag) to get more information about which years these clubs won or reached the finals.",
        "<h4>Adding Shiny Widgets</h4> You can choose to display either the number of cups won or finals reached",
        "<h4>Displaying the all time best scorers</h4>
        Here you have a top ten of the best scorers of all time for which the data also comes from another table of the page",
        "<h4>Adding Shiny Widgets</h4> You can also choose to display the top appearances"
    ))
  }
  steps = rbind(steps,steps_compet)
  
  introjs(session, options = list(
    "nextLabel"="Next",
    "prevLabel"="Previous",
    "skipLabel"="Quit tutorial",
    steps = steps
  ))
})


# Game of Thrones ---------------------------------------------------------

output$got_ep = renderHighchart({
  highchart_colored(eda_got_ep, type_ = "areaspline",
                    x = "no_overall",y = input$got_var,name = "title",transp = FALSE,
                    title = "Details per episode",borderWidth = 3,bgheader = TRUE,
                    color = "white",orient = 15,halosize = 15) %>%
    hc_xAxis(plotBands = eda_got_ep_plotband) %>% 
    hc_credits(
      enabled = TRUE,
      text = "Data from IMDb",
      href = "https://www.imdb.com/title/tt0944947/episodes?ref_=tt_ov_epl"
    )
})

output$got_top = renderHighchart({
  highchart_colored(eda_got_summary, type_ = "column",transp = FALSE,bgheader = TRUE,bgfont = 0.9,
                    x = "rank",y = "episodes",name = "character",borderWidth = 3,
                    title = paste0("Characters with most appearances"),
                    color = "white",halosize = 15) %>% 
    hc_credits(
      enabled = TRUE,
      text = "Data from Game of Thrones Fandom",
      href = "https://gameofthrones.fandom.com/wiki/Game_of_Thrones_Wiki"
    )
})

observeEvent(input$got_help,{
  introjs(session, options = list(
    "nextLabel"="Next",
    "prevLabel"="Previous",
    "skipLabel"="Quit tutorial",
    steps = data.frame(
      element = c(NA,"#got_ep","#got_var","#got_top"),
      intro = c(
        "<h4>Game of Thrones Web Scraping Methodology</h4> 
        Web scrapping is a method is a technique used to extract data from data. <br>
        In this section we are scraping data on Wikipedia and IMDb about the television series <b>Game of Thrones</b>.<br> <br>
        You can navigate in this tutorial either by clicking on <em>Next</em>,<em>Previous</em> and <em>Quit tutorial</em> 
        or by using the arrows left and right of your keyboard",
        "<h4>Building the first plot</h4> 
        Each episode of each season page on IMDb is provided with a clickable link to the episode that provides more information and also an image. <br>
        We extracted these data to put the episodes on the horizontal axis and the audience on the vertical axis. 
        Then, we added some extra information on the tooltip of the plot when you hover your mouse on each episode.<br>
        The final touch consisted in adding a white plotband that would highlight each season when you hover your mouse on it. <br>
        We used the package <i>Highcharter</i> to make the plots. <br>",
        "<h4>Adding Shiny Widgets</h4> 
        You can choose to change vertical axis to visualise the data from a different perspective",
        "<h4>Building the second plot</h4> 
        For the characters ranking plot, we scrapped all the main characters from Wikipedia and used those names to complete 
        the last part of the URL of the Game of Thrones Wiki. <br>
        We then used the affiliation of each character and the hyperlink provided for each house to put the icons on the plot."))))
})


# c(p("We began by collecting the first data for each Game of Thrones episode from ",
#     a(href="https://en.wikipedia.org/wiki/List_of_Game_of_Thrones_episodes", "Wikipedia", target="_blank"),br(),
#     "For ",a(href="https://www.imdb.com/title/tt0944947/episodes?season=1", "IMDb,", target="_blank"),
#     "it was more complicated as there are one page per season so we had to make a loop over the seasons."
# ),
# p("Each episode on each season page on IMDb is provided with a clickable link to the episode that provides more", 
#   a(href="https://www.imdb.com/title/tt1480055/?ref_=ttep_ep1", "information", target="_blank"),"and an image.",br(),
#   "We scrapped these data to put them on the tooltip of the plot when you hover your mouse on each episode.", br(),
#   "We then added a ",
#   a(href="https://i.ibb.co/pvHWrzc/87509-game-of-thrones-daenerys-wallpaper.jpg", "background image", target="_blank")),
# p("You can choose to change vertical axis to visualise the data from a different perspective"),
# p("The plots were made with ",
#   a(href="http://jkunst.com/highcharter/index.html", "Highcharter,", target="_blank"),
#   "a R wrapper for ",
#   a(href="https://www.highcharts.com", "Highcharts.", target="_blank"),br(),
#   "The final touch consisted in adding a plotband that would highlight each season when you hover your mouse on it."),
# p("For the characters ranking plot, we scrapped all the main characters from this",
#   a(href="https://en.wikipedia.org/wiki/List_of_Game_of_Thrones_characters", "Wikipedia page", target="_blank"),
#   "and used those names to complete the last part of the URL of the",
#   a(href="https://gameofthrones.fandom.com/wiki/Daenerys_Targaryen", "Game of Thrones Wiki", target="_blank"),br(),
#   "We then used the affiliation of each character and the hyperlink provided for each house to put the icons on the plot."
# ))