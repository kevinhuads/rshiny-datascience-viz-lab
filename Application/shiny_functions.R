intro = function(x){
  column(12,div(x,style = 'color:blue'))
}

title_ = function(x){
  column(12,h1(x))
}

subtitle_ = function(x){
  column(12,h2(x))
}

radiogroup_cust = function(input, label_ = NULL, choices_,selected_ = NULL,transform_name = FALSE,
                           direction_ = 'vertical', choiceNames_ = NULL, choiceValues_ = NULL){
  if (is.null(choiceNames_)){
    if (is.null(selected_)) s = choices_[1] else s = selected_
    y = radioGroupButtons(
      inputId = input,
      label = label_,
      choiceNames = unname(sapply(choices_,function(x) 
        ifelse(transform_name,simpleCap(gsub("_"," ",x)),x))),
      choiceValues = choices_,
      selected = s,
      individual = TRUE,
      direction = direction_,
      checkIcon = list(
        yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
        no = tags$i(class = "fa fa-circle-o", style = "color: steelblue"))
    )
  } else {
    if (is.null(selected_)) s = choiceValues_[1] else s = selected_
    y = radioGroupButtons(
      inputId = input,
      label = label_,
      choiceNames = choiceNames_,
      choiceValues = choiceValues_,
      selected = s,
      individual = TRUE,
      direction = direction_,
      checkIcon = list(
        yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
        no = tags$i(class = "fa fa-circle-o", style = "color: steelblue"))
    )
  }
  
  return(y)
}


framework_icon = function(label, href, icon_name) {
  div(
    style = "display:inline-block; text-align:center; margin-right:15px;",
    a(
      href = href,
      target = "_blank",
      title = label,
      span(style = "display:block; font-size:1.25rem; margin-bottom:4px;", label),
      icon(icon_name)
    )
  )
}
