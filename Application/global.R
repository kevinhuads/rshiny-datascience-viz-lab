options(shiny.sanitize.errors = FALSE)

list.of.packages <- c('rvest',
                      'ade4',
                      'factoextra',
                      "plotly",
                      'geojsonio',
                      "shinydashboard",
                      'sp',
                      "rpart.plot",
                      'maps',
                      'leaflet',
                      'shiny',
                      'corrplot',
                      'tidyr',
                      "dplyr",
                      "shinyjs",
                      "shinyBS",
                      "scales",
                      "DT",
                      "plyr",
                      "reshape",
                      "data.table",
                      "bit64",
                      "rpart",
                      "BBmisc",
                      # "gbm3",
                      'FNN',
                      'randomForest',
                      'e1071',
                      'dendextend',
                      'ape',
                      'maps',
                      'wordcloud',
                      'tm',
                      'SnowballC',
                      'RColorBrewer',
                      # 'RWeka',
                      'readr',
                      'igraph',
                      'ggraph',
                      'syuzhet',
                      'algorithmia',
                      'stringi',
                      'text2vec',
                      'glmnet',
                      'fastrtext',
                      'smooth',
                      'lubridate',
                      'zoo',
                      'dygraphs',
                      'wordcloud2',
                      'ggrepel',
                      'topicmodels',
                      'tidytext',
                      'textclean',
                      'fmsb',
                      'shinycssloaders',
                      'ggvis',
                      'treemapify',
                      'forecast',
                      'highcharter',
                      'prophet',
                      'gutenbergr',
                      'stringr',
                      'shinyWidgets',
                      'bsplus',
                      'rintrojs',
                      'pROC',
                      'shinydashboardPlus',
                      'xgboost',
                      'caret',
                      'visNetwork')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages,dependencies = TRUE)
#lapply(list.of.packages, require, character.only = TRUE)
library(igraph)
library(rvest)
library(ade4)
library(corrplot)
library(ggplot2)
library(plotly)
library(factoextra)

library(geojsonio)
library(shinydashboard)
library(sp)
library(maps)
library(shiny)
library(corrplot)
library(tidyr)
library(dplyr)
library(shinyjs)
library(shinyBS)
library(scales)
library(DT)
library(plyr)
library(reshape)
library(data.table)
library(bit64)
library(rpart)
library(BBmisc)
library(maps)
library(rpart.plot)
#library(gbm)
library(FNN)
library(randomForest)
library(e1071)
library(dendextend)
library(ape)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(SnowballC)
# library(RWeka)
library(readr)
library(ggraph)
library(syuzhet)
library(algorithmia)
library(stringi)
library(text2vec)
#library(fastrtext)
#library(glmnet)
library(smooth)
library(lubridate)
library(zoo)
library(dygraphs)
library(wordcloud2)
library(ggrepel)
library(topicmodels)
library(tidytext)
library(textclean)
library(leaflet)
library(fmsb)
library(shinycssloaders)
library(ggvis)
library(treemapify)
library(forecast)
library(highcharter)
library(prophet)
library(gutenbergr)
library(stringr)
library(shinyWidgets)
library(bsplus)
library(rintrojs)
library(shinydashboardPlus)
library(rvest)
library(countrycode)
library(pROC)
library(xgboost)
library(caret)
# 
# if (!"shinysky"%in%installed.packages()[,"Package"]) {
#   if (!"devtools"%in%installed.packages()[,"Package"]) install.packages("devtools")
#   dir.create("utils",showWarnings = FALSE)
#   download.file(url = "https://github.com/AnalytixWare/ShinySky/archive/master.zip",
#                 destfile = "utils/ShinySkyRepo.zip")
#   unzip("utils/ShinySkyRepo.zip",exdir = "utils")
#   devtools::install("utils/ShinySky-master")
# }
# library(shinysky)
# devtools::install_github('gbm-developers/gbm3')

source('global_functions.R',local = TRUE)
source('shiny_functions.R',local = TRUE)

load('Saved/bikes.RData',envir =.GlobalEnv)
load('Saved/usa.RData',envir =.GlobalEnv)
load('Saved/spam.RData',envir =.GlobalEnv)
load('Saved/hotel.RData',envir =.GlobalEnv)
load('Saved/stack.RData',envir =.GlobalEnv)
load('Saved/marathon.RData',envir =.GlobalEnv)
load('Saved/sales.RData',envir = .GlobalEnv)
load('Saved/optim.RData',envir = .GlobalEnv)
load('Saved/bitcoin.RData',envir = .GlobalEnv)
load('Saved/pi.RData',envir = .GlobalEnv)
load('Saved/coins.RData',envir = .GlobalEnv)
load('Saved/sports.RData',envir = .GlobalEnv)
load('Saved/markov.RData',envir = .GlobalEnv)


palettes = c('Spectral','RdYlGn','YlOrRd','Reds','Oranges','Greys','BuPu','Blues')
palettes_quali = c('Set1','Set2','Set3','Accent','Paired')
ep_cols = brewer.pal(n = 9, name = "Set1")
