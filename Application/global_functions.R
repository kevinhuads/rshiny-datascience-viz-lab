# Generate the colors for the tree
col_rainbow=function(y1){
  cols <- rainbow(99, end=.36)[
    ifelse(y1 >  y1[1], (y1-y1[1]) * (99-50) / (max(y1,na.rm = TRUE)-y1[1]) + 50,
           (y1-min(y1,na.rm = TRUE))  * (50-1)  / (y1[1]-min(y1,na.rm = TRUE)) + 1)]
}

heat.tree <- function(tree, low.is.green=TRUE, ...) { # dots args passed to prp
  y1 <- tree$frame$yval
  if(low.is.green) y1 <- -y1
  prp(tree, branch.col=col_rainbow(y1), box.col=col_rainbow(y1),...)
}

dev_gamma = function(pred, obs){
  y = sum(log(pred/obs + obs/pred -1))/length(obs)
  return(y)
}

er_gamma = function(pred,obs){
  y = dev_gamma(pred,obs)/dev_gamma(mean(obs),obs)
}

err_box = function(value,subtitle =  'Error Reduction Ratio (for the best model)') {
  renderValueBox({
    valueBox(
      percent(value), 
      subtitle, 
      icon = icon("search",lib = 'glyphicon'),
      color = "black",
      width = 12
    )
  })
}

power_abs = function(x,p = 0.5){
  y = x
  y[y>=0] = y[y>=0]^p
  y[y<0] = -((-y[y<0])^p)
  return(y)
}

Clean_String <- function(string){
  temp <- tolower(string) # Lowercase
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ") # Remove everything that is not a number or letter
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ") # Shrink down to just one white space 
  temp <- stringr::str_split(temp, " ")[[1]] # Split it
  indexes <- which(temp == "") # Get rid of trailing "" if necessary
  if(length(indexes) > 0) temp <- temp[-indexes]
  return(temp)
}

pers_pretty = function(x) 10^(nchar(as.integer(x))-2)*round(x/10^(nchar(as.integer(x))-2))

round2 = function(x){
  y = x
  for (i in 1:length(x)){
    if (round(x[i]) ==x[i]) x[i]=round(x[i]) else x[i]=round(x[i],1)
  }
  return(y)
}

to_hour = function(x,r = 5){
  y = rep('',length(x))
  
  for (i in 1:length(x)){
    if (is.na(x[i])) {
      y[i] = NA
    } else {
      if (floor(x[i])>0) y[i] = paste0(as.character(floor(x[i])),'h')
      if (floor(x[i])>0) frac = floor(x[i]%%floor(x[i]) * 60) else frac = floor(x[i] * 60)
      if(frac>1) y[i] = paste0(y[i],round(frac/r)*r,'m')
    }
  }
  return(y)
}

calc_dist = function(villes){
  nv = nrow(villes)-1
  sum(diag(as.matrix(dist(villes))[2:(nv+1),1:nv]))
}

anneal_step = function(T,i,traj, villes_RC,J1,traj_opt,J_opt,steps = 10,alpha = 0.9997,launch = FALSE){
  if (launch){
    nv = nrow(villes_RC) - 1
    for (step in 1:steps){
      n=1+sample(nv-1,2)
      if (max(n)<nv){
        traj2=c(1:(min(n)-1),rev(min(n):max(n)),(max(n)+1):nv,1)
      } else {
        traj2=c(1:(min(n)-1),rev(min(n):max(n)),1)
      }
      villes_test<-villes_RC[traj2,]
      J2=calc_dist(villes_test)
      if (J2<=J1){
        traj=traj2
        villes_RC=villes_test
        J1=J2
      } else { #J2>J1
        p = exp(-(J2-J1)/T)
        if (p>runif(1)){
          traj=traj2
          villes_RC=villes_RC[traj,]
          J1=calc_dist(villes_RC)
        }
      }
      if (J_opt>J1){
        traj_opt=traj
        J_opt=J1
      }
      i = i + 1
      T=alpha*T
    }
  }
  
  return(list(T = T,
              i = i,
              traj = traj, 
              villes_RC = villes_RC,
              J1 = J1,
              traj_opt = traj_opt,
              J_opt = J_opt))
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),sep="", collapse=" ")
}

add_changepoints_to_plot <- function(m, threshold = 0.01, cp_color = "red",
                                     cp_linetype = "dashed", trend = TRUE, ...) {
  layers <- list()
  if (trend) {
    trend_layer <- ggplot2::geom_line(
      ggplot2::aes_string("ds", "trend"), color = cp_color, ...)
    layers <- append(layers, trend_layer)
  }
  signif_changepoints <- m$changepoints[abs(m$params$delta) >= threshold]
  cp_layer <- ggplot2::geom_vline(
    xintercept = as.integer(signif_changepoints), color = cp_color,
    linetype = cp_linetype, ...)
  layers <- append(layers, cp_layer)
  return(layers)
}



### Highchart -----
add_variable_to_series_list<- function(x, series_list, key_vector, value_vector){
  base::stopifnot(length(key_vector) == length(value_vector))
  base::stopifnot(length(series_list) == length(key_vector))
  series_list[[x]][length(series_list[[x]])+1]<- value_vector[x]
  names(series_list[[x]])[length(series_list[[x]])]<- key_vector[x]
  return(series_list[[x]])
}


# From highcharter github pages:
hc_add_series_bwpout = function(hc, value, by, ...) {
  z = lapply(levels(by), function(x) {
    bpstats = boxplot.stats(value[by == x])$stats
    outliers = c()
    for (y in na.exclude(value[by == x])) {
      if ((y < bpstats[1]) | (y > bpstats[5]))
        outliers = c(outliers, list(which(levels(by)==x)-1, y))
    }
    outliers
  })
  hc %>%
    hc_add_series(data = z, type="scatter", ...)
}


gen_key_vector<-function(variable, num_times){
  return(rep(variable, num_times))
} 
gen_boxplot_series_from_df<- function(value, by,...){
  value<- base::as.numeric(value)
  by<- base::as.factor(by)
  box_names<- levels(by)
  z=lapply(box_names, function(x) {
    boxplot.stats(value[by==x])$stats
  })
  tmp<- lapply(seq_along(z), function(x){
    var_name_list<- list(box_names[x])
    #tmp0<- list(names(df)[x])
    names(var_name_list)<- "name"
    index<- x-1
    tmp<- list(c(index,  z[[x]]))
    tmp<- list(tmp)
    names(tmp)<- "data"
    tmp_out<- c(var_name_list, tmp)
    #tmp<- list(tmp)
    return(tmp_out)
    
  })
  return(tmp)
}
# Usage: 
#series<- gen_boxplot_series_from_df(value = df$total_value, by=df$asset_class)

#backgroundplot

highchart_colored = function(list_df, type_ = "areaspline",x = "year",y = "events",title = NULL,
                             name = "host",bgheader = FALSE, transp = TRUE,color = "white",bgfont = 0.9,
                             orient = 15,altitude = 0,halosize = 30,borderWidth = NULL, borderCol = 'rgba(0,0,0,0.65)'){
  df = list_df$df
  df = df %>% mutate(x_ = as.data.frame(df[,x])[,1],
                     y_ = as.data.frame(df[,y])[,1],
                     name_ = as.data.frame(df[,name])[,1])
  
  hc = hchart(df, type_, hcaes(x_, y_, name = name_), name = simpleCap(name)) %>% 
    hc_colors(hex_to_rgba(color, 0.8)) %>% 
    hc_title(
      text = title,
      align = "left",style = list(color = color)) 
  
  if (type_ == 'column'){
    hc = hc %>% hc_add_series(df, hcaes(x_, y_, name = name_),type = "scatter",color = color) %>%
      hc_xAxis(
        title = list(text = simpleCap(x), style = list(color = color)),
        gridLineWidth = 0,
        tickInterval = 1,
        labels = list(style = list(color = color))) 
  } else {
    hc = hc %>%
      hc_xAxis(
        title = list(text = simpleCap(x), style = list(color = color)),
        gridLineWidth = 0,
        labels = list(style = list(color = color))) 
  }
  hc = hc %>% 
    hc_yAxis(
      lineWidth = 1,
      tickWidth = 1,
      tickLength = 10,
      title = list(text = simpleCap(y), style = list(color = color)),
      gridLineWidth = 0,
      labels = list(style = list(color = color))) %>% 
    hc_chart(
      divBackgroundImage = list_df$img,
      backgroundColor = hex_to_rgba("black", 0.1)) %>% 
    hc_tooltip(
      headerFormat = switch(bgheader,as.character(tags$h4("{point.key}")),NULL),
      pointFormat = list_df$tt,
      useHTML = TRUE,
      backgroundColor = ifelse(transp,'transparent',"rgba(255,255,255,0.65)"),
      borderColor = ifelse(transp,'transparent',borderCol),
      borderWidth = borderWidth,
      shadow = FALSE,
      style = list(color = ifelse(transp,"white","black"), fontSize = paste0(bgfont,"em"), fontWeight = "normal"),
      positioner = JS(paste0("function () { return { x: this.chart.plotLeft + ",
                             orient, ", y: this.chart.plotTop + ",altitude," }; }")),
      shape = "square"
      ) %>% hc_plotOptions(series = list(states = list(hover = list(halo = list(size  = halosize))))) 
  return(hc)
}


## Boxplot function:
make_highchart_boxplot_with_colored_factors<- function(value, by, chart_title="Boxplots",
                                                       chart_x_axis_label="Values", show_outliers=FALSE,
                                                       boxcolors=NULL, box_line_colors=NULL){
  by<- as.factor(by)
  box_names_to_use<- levels(by)
  series<- gen_boxplot_series_from_df(value = value, by=by)
  if(is.null(boxcolors)){
    cols<- viridisLite::viridis(n= length(series), alpha = 0.5) # Keeping alpha in here! (COLORS FOR BOXES ARE SET HERE)
  } else {
    cols<- boxcolors
  }
  if(is.null(box_line_colors)){
    if(base::nchar(cols[[1]])==9){
      cols2<- substr(cols, 0,7) # no alpha, pure hex truth, for box lines 
    } else {
      cols2<- cols
    }
    
  } else {
    cols2<- box_line_colors
  }
  
  # Injecting value 'fillColor' into series list
  kv<- gen_key_vector(variable = "fillColor", length(series)) 
  series2<- lapply(seq_along(series), function(x){ 
    add_variable_to_series_list(x = x, series_list = series, key_vector = kv, value_vector = cols) 
  })
  
  if(show_outliers == TRUE){
    hc<- highcharter::highchart() %>%
      highcharter::hc_chart(type="boxplot", inverted=FALSE) %>%
      highcharter::hc_title(text=chart_title) %>%
      highcharter::hc_legend(enabled=FALSE) %>%
      highcharter::hc_xAxis(type="category", categories=box_names_to_use, title=list(text=chart_x_axis_label)) %>%
      highcharter::hc_add_series_list(series2) %>%
      hc_add_series_bwpout(value = value, by=by, name="Outliers") %>%
      hc_plotOptions(series = list(
        marker = list(
          symbol = "circle"
        ),
        grouping=FALSE
      )) %>%
      highcharter::hc_colors(cols2) %>%
      highcharter::hc_exporting(enabled=TRUE)
    
  } else{
    hc<- highcharter::highchart() %>%
      highcharter::hc_chart(type="boxplot", inverted=FALSE) %>%
      highcharter::hc_title(text=chart_title) %>%
      highcharter::hc_legend(enabled=FALSE) %>%
      highcharter::hc_xAxis(type="category", categories=box_names_to_use, title=list(text=chart_x_axis_label)) %>%
      highcharter::hc_add_series_list(series2) %>%
      hc_plotOptions(series = list(marker = list(symbol = "circle"),grouping=FALSE)) %>%
      highcharter::hc_colors(cols2) %>%
      highcharter::hc_exporting(enabled=TRUE)
  }
  hc
}

### Highchart -----


countrycode_cust = function(sourcevar, origin = "country.name", destination = "iso2c"){
  y = countrycode(sourcevar = sourcevar, origin = origin, destination = destination) %>%
    suppressWarnings()
  y[sourcevar %in% 'Yugoslavia'] = 'YU'
  y[sourcevar %in% 'England'] = 'EN'
  y[sourcevar %in% 'East Germany'] = 'DE'
  y[sourcevar %in% 'Unified Team'] = 'RU'
  y[sourcevar %in% 'Wales'] = 'WA'
  return(y)
}

countrycode_cust2 = function(sourcevar, origin = "iso3c", destination = "iso2c"){
  y = countrycode(sourcevar = sourcevar, origin = origin, destination = destination) %>%
    suppressWarnings()
  y[sourcevar %in% "YUG"] = "YU"
  y[sourcevar %in% "GRE"] = "GR"
  y[sourcevar %in% "POR"] = "PT"
  y[sourcevar %in% "ENG"] = "EN"
  y[sourcevar %in% "FRG"] = "DE"
  y[sourcevar %in% "NED"] = "NL"
  y[sourcevar %in% "GER"] = "DE"
  y[sourcevar %in% "SCO"] = "SC"
  return(y)
}

countrycode_rev = function(sourcevar, origin = "iso2c", destination = "country.name"){
  y = countrycode(sourcevar = sourcevar, origin = origin, destination = destination) %>%
    suppressWarnings()
  y[sourcevar %in% 'SC'] = 'Scotland'
  y[sourcevar %in% 'EN'] = 'England'
  y[sourcevar %in% 'YU'] = 'Yugoslavia'
  return(y)
}


markers_scrap = function(df,var,h = NULL,w = NULL){
  y = df
  
  if(var != "url"){
    t1 = unlist(y[,paste0(var,'_code')])
    urltemp = sprintf(eda_urlico, t1)
    
    vec_ad = c("EN","SC","WA")
    img_ad = c("https://i.ibb.co/QvfwKCT/GB-ENG-England-Flag-icon-2.png",
               "https://i.ibb.co/4tS1DmK/2560px-Flag-of-Scotland-svg.png",
               "https://i.ibb.co/QKcbpFr/images.png")
    for (i in 1:length(vec_ad)){
      y_t = t1 %in% vec_ad[i]
      if (any(y_t)){
        urltemp[y_t] = paste0("url(",img_ad[i],")")
      }
    }
  } else {
    urltemp = paste0("url(",unlist(df[,var]),")")
  }
  
  y$marker = purrr::map(urltemp, function(x) list(symbol = x))
  if (!is.null(h)) y$marker = purrr::map(urltemp, function(x) list(symbol = x,height = h, width = w))
  y$flagicon = str_replace_all(urltemp, "url\\(|\\)", "")
  return(y)
}

time_scrap = function(tab,header = FALSE,filters_year = NULL, year = NULL,clean_year = FALSE,year_season = FALSE,
                      del_col = NULL,rem_num = NULL,rem_spec = NULL,del_row = NULL,
                      col_refs = NULL,rem_coma = NULL, rem_par = NULL,coma_country = NULL,filters_pos = NULL,one_flag = TRUE,
                      new_names = NULL,col_flags = NULL,col_split = NULL,city = NULL,int_zero = FALSE,col_rank = FALSE){
  y = tab
  y = janitor::clean_names(y)
  y = tbl_df(y)
  if (!is.null(del_col)) y = y[,-del_col]
  if (!is.null(del_row)) y = y[-del_row,]
  y = mutate_if(y, is.character, str_trim)
  if (header == TRUE){
    if(all(substr(colnames(y),1,1),na.rm = TRUE)) colnames(y) = gsub(' |-','_',tolower(y[1,]))
    y = filter(y, row_number() != 1)
  } 
  
  if (year_season) y[,year] = as.numeric(substr(unlist(y[,year]),1,4)) + 1
  if (!is.null(year)){
    y = y[!is.na(y[,year]),]
    y = y[y[,year]<= as.numeric(substr(Sys.Date(),1,4)),]
  } 
  if (clean_year) y[,year] = as.numeric(str_extract(unlist(y[,year]),"[0-9]+"))
  if (!is.null(city)){
    y$city = unlist(y[,city]) 
    y = y %>% left_join(temp_scrap_countries, by = 'city')
  } 
  if (!is.null(col_split)) for (i in col_split){
    y[grepl("Japan",unlist(y[,i]))&grepl("South Korea",unlist(y[,i])),i] = "Japan"
  } 
  if (!is.null(filters_year)) y = y[!unlist(y[,year])%in%filters_year,]
  if (!is.null(rem_par)) for (i in rem_par) y[,i] = gsub("\\(.*","",unlist(y[,i]))
  if (!is.null(rem_spec)) for (i in rem_spec) y[,i] = gsub("[[:punct:]]", "", unlist(y[,i]))
  if (!is.null(rem_coma)) for (i in rem_coma)  y[,i] =  str_replace_all(unlist(y[,i]),',','')
  if (!is.null(coma_country)) y$host_country = str_trim(str_replace(str_extract(unlist(y[,coma_country]), ", .*$"), ", ", ""))
  if (!is.null(col_refs)) for (i in col_refs) y[,i] = gsub("\\[.*","",unlist(y[,i]))
  if (!is.null(rem_num)) for (i in rem_num) y[,i] = str_extract(unlist(y[,i]), "\\D+")
  
  if (!is.null(new_names)){
    for (i in 1:length(new_names)){
      name = quo_name(new_names[i])
      new_name = quo_name(names(new_names)[i])
      y = y %>% dplyr::rename(!!new_name := !!name)
    }
  }
  
  tf <- tempfile(fileext = ".csv")
  write_csv(y, tf)
  y <- read_csv(tf,col_types = cols())
  
  if (!is.null(col_flags)){
    for (i in col_flags){
      y[,i] = sub("(^\\s+)|(\\s+$)", "", unlist(y[,i]))
      y[,paste0(i,'_code')] <- countrycode_cust(unlist(y[,i]))
    }
    if(length(col_flags) == 1) if(one_flag) y = markers_scrap(y,col_flags)
  }
  if(int_zero) y[,sapply(y,is.numeric)][is.na(y[,sapply(y,is.numeric)])] = 0
  if(!is.null(filters_pos)) for (i in filters_pos) y = y[unlist(y[,i])>0,]

  if(col_rank) y$rank = 1:nrow(y)
  
  return(y)
}

eda_list = function(dgames,img,ttvars,w = NULL, h = NULL){
  y = list(
    df = dgames,
    img = img,
    ttvars = ttvars
  )
  y$tt = tooltip_table(
    unname(sapply(y$ttvars, function(x) paste0(simpleCap(gsub("_"," ",x))," "))),
    sprintf("{point.%s}", y$ttvars),
    img = tags$img(src="{point.flagicon}", style = "text-align: center;",
                   width = switch(!is.null(w),w,NULL),
                   height = switch(!is.null(h),h,NULL))
  )
  return(y)
}


skip_line_cust = function(x, n= 4){
  y = strsplit(x,', ')[[1]]
  z = c()
  for (i in (1:ceiling(length(y)/n))-1){
    z = c(z,paste(y[(1+i*n):min(n+i*n,length(y))],collapse = ', '))
  }
  return(paste(z,collapse = ' <br>'))
}


get_links = function(link_,ind_,col_1,which_1 = 1,del_clubs = FALSE,del_patterns = NULL, by = 1, sel = 1){
  tab_temp = read_html(link_) %>% 
    html_nodes('table.wikitable') %>% .[[ind_]] 
  
  links = tab_temp %>%
    html_nodes('a') %>%
    html_attr('href')
  
  if(any(grepl("cite",links))) links = links[!grepl("cite",links)]
  if (!is.null(del_patterns)){
    for (i in del_patterns){
      links = links[!grepl(i,links)]
    }
  }
  links = links[seq(from = sel,to = length(links), by = by)]
  
  if (del_clubs){
    if (col_1 == "scorer"){
      col_2 = 'Club(s) (Goals)' 
      temp_ = 1
    } else {
      col_2 = 'Club(s) (Apps)'
      temp_ = 0
    }
    to_del = sapply(strsplit((tab_temp %>% html_table(fill = TRUE))[,col_2],","),length)
    links = links[temp_ + c(1,1+cumsum(2+to_del[-length(to_del)]))]
  }
  
  urlimg = sapply(links, function(x) {
    paste0("https:",read_html(paste0("https://en.wikipedia.org",x)) %>% html_node(xpath = '//td/a/img') %>%html_attr('src')
  )})
  
  names_ = (tab_temp  %>% html_table(fill = TRUE))[,which_1]
  y = data.frame(x = names_,links = links, url = urlimg,stringsAsFactors = FALSE)
  colnames(y)[1] = col_1
  return(y)
}

innerHTML <- function(y, trim = FALSE, collapse = " <br>"){
  sapply(y ,function(x){
    paste(xml2::xml_find_all(x, ".//text()"), collapse = collapse)
  })
}

calc_perf = function(real,pred,prob = 0.5){
  pos = ifelse(pred > prob, 1,0)
  tp = sum(pos&real)
  fp = sum((pos)&(!real))
  tn = sum((!pos)&(!real))
  fn = sum((!pos)&(real))
  total = length(pos)
  auc = pROC::auc(as.factor(real),pred)
  
  accuracy = (tp + tn)/total
  precision = tp/(tp + fp)
  recall = tp/(tp + fn)
  f1 = 2/(1/precision + 1/recall)
  
  return(c(tp = tp,fp = fp,tn = tn,fn = fn,auc = auc,
           accuracy = accuracy, precision = precision,recall = recall,f1 = f1))
}


sim_sir = function(ep_pop = 10^6, ep_i0 = 10, ep_r0 = 0,ep_d0 = 0,ep_vaccin = 0, ep_inter = 100, ep_prob_inf = 0.005,
                   ep_prob_death = 0.03, ep_dur = 14, ep_dtot = 10000,
                   ep_cap = 0,ep_cap_red = 0.5,ep_type = "line", peak = TRUE,
                   ep_vs = TRUE,ep_vi = TRUE,ep_vr = TRUE,ep_vd = TRUE){

  ep_s0 = ep_pop - ep_i0
  ep_beta = ep_inter*ep_prob_inf
  ep_gamma = 1/ep_dur
  ep_cap_death = ep_cap_red*ep_prob_death

  ep_s = c(ep_s0*(1-ep_vaccin),rep(0,ep_dtot-1))
  ep_i = c(ep_i0,rep(0,ep_dtot-1))
  ep_r = c(ep_s0*ep_vaccin,rep(0,ep_dtot-1))
  ep_d = c(ep_d0,rep(0,ep_dtot-1))
  
  i = 1
  while((round(ep_i[i]) > 0)&i<ep_dtot){
    i = i +1
    ep_i[i] = ep_i[i-1] + ep_beta*ep_i[i-1]*ep_s[i-1]/ep_pop - ep_gamma*ep_i[i-1]
    
    if (ep_i[i-1]< ep_cap){
      ep_d[i] = ep_d[i-1] + ep_gamma*ep_cap_death*ep_i[i-1]
      ep_r[i] = ep_r[i-1] + ep_gamma*(1-ep_cap_death)*ep_i[i-1]
    } else {
      ep_d[i] = ep_d[i-1] + ep_gamma*ep_cap_death*ep_cap + ep_gamma*ep_prob_death*(ep_i[i-1] - ep_cap)
      ep_r[i] = ep_r[i-1] + ep_gamma*(1-ep_cap_death)*ep_cap + ep_gamma*(1-ep_prob_death)*(ep_i[i-1] - ep_cap)
    }
    ep_s[i] = ep_s[i-1] - ep_beta*ep_i[i-1]*ep_s[i-1]/ep_pop
    if (ep_s[i] < 0){
      ep_i[i] = ep_i[i] + ep_s[i]
      ep_s[i] = 0
    }
  }
  ep_dtot = i
  ep_states = c("Susceptible","Infected","Recovered","Dead")
  peak_ = which(ep_i[1:ep_dtot] == max(ep_i[1:ep_dtot])) - 1
  ep_df = data.frame(count = round(c(ep_s[1:ep_dtot],ep_i[1:ep_dtot],
                                     ep_r[1:ep_dtot],ep_d[1:ep_dtot])),
                     state = rep(ep_states,each = ep_dtot),
                     days = rep(1:ep_dtot,times = length(ep_states)))
  
  hc = highchart() %>% 
    hc_chart(type = ep_type)  %>%
    hc_xAxis(data = ep_df$days[ep_df$state == "Susceptible"])%>%
    hc_add_series(name = 'Recovered',data = ep_df$count[ep_df$state == "Recovered"],color = ep_cols[3],visible = ep_vr) %>%
    hc_add_series(name = 'Susceptible',data = ep_df$count[ep_df$state == "Susceptible"],color = ep_cols[2],visible = ep_vs) %>%
    hc_add_series(name = 'Dead',data = ep_df$count[ep_df$state == "Dead"],color = ep_cols[1],visible = ep_vd)  %>%
    hc_add_series(name = 'Infected',data = ep_df$count[ep_df$state == "Infected"],color = ep_cols[4],visible = ep_vi) %>%
    hc_plotOptions(series = list(animation = FALSE)) %>%
    hc_tooltip(useHTML= TRUE,table= TRUE,sort= TRUE) 
  
  
  if (ep_cap>0){
    if (ep_cap< max(ep_df$count[ep_df$state == "Infected"])){
      h_plotband = list(
        list(from = 0,to = ep_cap,color = toRGB("black", alpha = 0),
             label = list(text = paste('Under Hospital Capabilities'),
                          style = list(color = "rgba(255,255,255,0.7)", fontWeight = "bold")),
             events = list(mouseover = JS("function(e) {this.svgElem.attr('fill', 'rgba(255,127,0,0.3)');}"),
                           mouseout = JS("function(e) {this.svgElem.attr('fill', 'rgba(255,255,255,0)');}"))),
        list(from = ep_cap,to = max(ep_df$count[ep_df$state == "Infected"]),color = toRGB("black", alpha = 0),
             label = list(text = paste('Over Hospital Capabilities'),
                          style = list(color = "rgba(255,255,255,0.7)", fontWeight = "bold")),
             events = list(mouseover = JS("function(e){this.svgElem.attr('fill', 'rgba(255,127,0,0.3)');}"),
                           mouseout = JS("function(e){this.svgElem.attr('fill', 'rgba(255,255,255,0)');}"))))
    } else {
      h_plotband = list(
        list(from = 0,to = max(ep_df$count[ep_df$state == "Infected"]),color = toRGB("black", alpha = 0),
             label = list(text = paste('Under Hospital Capabilities'),
                          style = list(color = "rgba(255,255,255,0.7)", fontWeight = "bold")),
             events = list(mouseover = JS("function(e) {this.svgElem.attr('fill', 'rgba(255,127,0,0.3)');}"),
                           mouseout = JS("function(e) {this.svgElem.attr('fill', 'rgba(255,255,255,0)');}"))),
        list(from = ep_cap,to = max(ep_df$count[ep_df$state == "Infected"]),color = toRGB("black", alpha = 0)))
    }
    
    
    hc = hc %>%
      hc_yAxis(plotLines = list(
        list(value = ep_cap, color = "#FF7F00",width = 2.5,zIndex = 1,
             label = list(text = "Hospital Capabilities",style = list()))),
        plotBands = h_plotband)
  } 
  
  if (ep_type == "area") hc = hc %>% hc_plotOptions(area = list(stacking = "normal"))
  
  if (peak == TRUE){
    hc = hc %>%
      hc_xAxis(plotLines = list(
        list(value = peak_, color = "grey",width = 2,zIndex = 1,dashStyle= 'dash',label = list(text = "Peak",style = list()))),
        plotBands = list(
          list(from = 0,to = peak_,color = toRGB("black", alpha = 0),
               label = list(text = paste('Before the peak'),style = list(color = "rgba(127,127,127,0.7)", fontWeight = "bold")),
               events = list(mouseover = JS("function(e) {this.svgElem.attr('fill', 'rgba(255,0,0,0.3)');}"),
                             mouseout = JS("function(e) {this.svgElem.attr('fill', 'rgba(255,255,255,0)');}"))),
          list(from = peak_,to = i,color = toRGB("black", alpha = 0),
               label = list(text = paste('After the peak'),style = list(color = "rgba(127,127,127,0.7)", fontWeight = "bold")),
               events = list(mouseover = JS("function(e) {this.svgElem.attr('fill', 'rgba(0,255,0,0.3)');}"),
                             mouseout = JS("function(e) {this.svgElem.attr('fill', 'rgba(255,255,255,0)');}")))))
  }
  
  y = list(plot = hc, days = i, deaths = round(ep_d[ep_dtot]),peak = peak_)
  return(y)
}


hchart.cor <- function(object, ...) {
  
  df <- as.data.frame(object)
  is.num <- sapply(df, is.numeric)
  df[is.num] <- lapply(df[is.num], round, 2)
  dist <- NULL
  
  x <- y <- names(df)
  
  df <- tbl_df(cbind(x = y, df)) %>% 
    gather(y, dist, -x) %>% 
    mutate(x = as.character(x),
           y = as.character(y)) %>% 
    left_join(data_frame(x = y,xid = seq(length(y)) - 1), by = "x") %>% 
    left_join(data_frame(y = y,yid = seq(length(y)) - 1), by = "y")
  
  ds <- df %>% 
    select_("xid", "yid", "dist") %>% 
    list_parse2()
  
  fntltp <- JS("function(){
               return this.series.xAxis.categories[this.point.x] + ' ~ ' +
               this.series.yAxis.categories[this.point.y] + ': <b>' +
               Highcharts.numberFormat(this.point.value, 2)+'</b>';
               ; }")
  cor_colr <- list( list(0, '#FF5733'),
                    list(0.5, '#F8F5F5'),
                    list(1, '#2E86C1')
  )
  highchart() %>% 
    hc_chart(type = "heatmap") %>% 
    hc_xAxis(categories = x, title = NULL) %>% 
    hc_yAxis(categories = y, title = NULL) %>% 
    hc_add_series(data = ds) %>% 
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE)
      )) %>% 
    hc_tooltip(formatter = fntltp) %>% 
    hc_legend(align = "right", layout = "vertical",
              margin = 0, verticalAlign = "top",
              y = 25, symbolHeight = 280) %>% 
    hc_colorAxis(  stops= cor_colr,min=-1,max=1)
  
  
}


