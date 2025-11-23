## Data preparation for the Markov section
## Build language specific corpora from Project Gutenberg and estimate
## character level trigram probabilities for synthetic word generation.

library(gutenbergr)
library(dplyr)
library(stringr)
library(tm)
library(purrr)
library(tidyr)
library(RColorBrewer)
library(magrittr)

markov_languages = c('en','fr','de','es','pt','it','nl')
markov_languages_full = c("English", "French", "German", "Spanish", "Portuguese", "Italian", "Dutch")
mark_min_char = 4
mark_max_char = 20

mark_list = lapply(markov_languages, function(lg) {
  print(lg)
  mark_dt = gutenberg_metadata %>%
    filter(has_text == TRUE) %>%
    filter(rights == 'Public domain in the USA.') %>%
    filter(!is.na(title)) %>%
    filter(language == lg)
  
  # Download and clean up to 1000 books in the target language
  mark_text = sapply(mark_dt$gutenberg_id[1:min(1000, length(mark_dt$gutenberg_id))], function(x) {
    print(x)
    y = gutenberg_download(x, strip = TRUE,
                           mirror = "http://mirrors.xmission.com/gutenberg/")$text %>% 
      sapply(iconv, "latin1//TRANSLIT", "UTF-8") %>%
      unname() %>%
      paste(collapse = " ") %>% 
      replace_names() %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_replace_all("[[å£³]]", " ") %>%
      gsub(pattern = " ?(f|ht)tp(s?)://(.*)[.][a-z]+", replacement = " ") %>%
      gsub(pattern = "'", replacement = " ") %>% 
      gsub(pattern = "-", replacement = " ") %>%
      gsub(pattern = "\\b\\w{1,2}\\s", replacement = "") %>%
      tolower() %>%
      removeNumbers() %>%
      removePunctuation()
    
    # Fix common French encoding issues after transliteration
    if (lg == "fr") {
      y = gsub("ã©", "é", y)
      y = gsub("ã´", "ô", y)
      y = gsub("\u0082", "é", y)
      y = gsub("â\u0080\u0099", " ", y)
      y = gsub("\u0092", " ", y)
      y = gsub("\u008a", "ère ", y)
      y = gsub("ã®", "ît ", y)
      y = gsub("ã¢", "â", y)
      y = gsub("ã¨", "ère ", y)
    }
    
    return(y)
  }) %>%
    paste(collapse = " ") %>%
    stripWhitespace()
})

names(mark_list) = markov_languages_full

# Word frequency tables by language
mark_tab = lapply(mark_list, function(x) sort(table(strsplit(x, " ")), decreasing = TRUE))
mark_tab = mark_tab[markov_languages_full]

n_top = 200
# For each word length, keep top frequent words (per language) after basic cleaning
markov_top = lapply(3:18, function(i) {
  sapply(mark_tab, function(x) {
    y = sort(x[nchar(names(x)) == i], decreasing = TRUE)[1:n_top]
    y = names(y[y > 2])
    y = gsub("\u0080", "fkepflsdmfk", y)
    y = gsub("\u0093", "fkepflsdmfk", y)
    y = gsub("\u0090", "fkepflsdmfk", y)
    y = y[!grepl("±|©|®|¼|¹|¬|fkepflsdmfk", y)]
    if (length(y) < n_top) y = c(y, rep(NA, n_top - length(y)))
    return(y)
  })
})
names(markov_top) = paste0("nchar", 3:18)

# Distribution of word lengths in each language
markov_length = sapply(mark_tab, function(x) { table(nchar(names(x))) / length(x) })
markov_length_sum = sapply(mark_tab, function(x) { summary(nchar(names(x))) })

# Character sets with accents by language (used for visualisation)
markov_accents = list(
  French = c("é","à","è","ù","â","ê","î","ô","û","ç","ë","ï","ü"),
  German = c("ß","ä","ö","ü"),
  Portuguese = c("à","á","â","ã","ç","é","ê","í","ó","ô","õ","ú"),
  Spanish = c("á","é","í","ó","ú","ü","ñ"),
  Italian = c("à","è","ì","ò","ù","é","ó","î")
)

# Mapping from accented characters to base ASCII equivalents
unwanted_array = list(
  "à" = "a", "á" = "a", "â" = "a", "ã" = "a", "ä" = "a", "å" = "a", "æ" = "o",
  "ç" = "c", "ñ = n", "ß = ss",
  "è" = "e", "é" = "e", "ê" = "e", "ë" = "e",
  "ì" = "i", "í" = "i", "î" = "i", "ï" = "i",
  "ð" = "o", "ò" = "o", "ó" = "o", "ô" = "o", "õ" = "o",
  "ö" = "o", "ø" = "o",
  "ù" = "u", "ú" = "u", "û" = "u",
  "ý" = "y", "þ" = "b", "ÿ" = "y"
)

# Unigram character distributions by language (after accent simplification)
markov_gram = lapply(mark_tab, function(x) {
  pat_ = "[a-z]"
  names(x) = chartr(
    paste(names(unwanted_array), collapse = ""),
    paste(unwanted_array, collapse = ""),
    names(x)
  )
  y = table(unlist(strsplit(names(x), "")))
  
  y = y[grepl(pat_, names(y))]
  y = y[!grepl("÷|×", names(y))]
  y = y / sum(y)
  return(y)
})

# Trigram frequency tables by language for words between mark_min_char and mark_max_char
markov_prob = lapply(mark_tab, function(x) {
  x = x[nchar(x) > 3]
  y = names(x[x > 10])
  
  y_list = strsplit(y, "")
  z = table(unlist(y_list))[table(unlist(y_list)) / length(y) > 0.0001]
  
  y = y[(nchar(y) <= mark_max_char) & (nchar(y) >= mark_min_char)]
  tab_freq_final = data.frame(let1 = character(), let2 = character(), let3 = character(),
                              stringsAsFactors = FALSE)
  
  # For each word length, accumulate trigram counts across all positions
  for (nc in mark_min_char:mark_max_char) {
    y_temp = y[nchar(y) == nc]
    if (length(y_temp) > 0) {
      y_temp = paste0("  ", y_temp, "  ") %>%
        strsplit("") %>% 
        do.call(what = rbind) %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        purrr::set_names(paste0("letters", 1:(nc + 4)))
      
      tab_freq_init = table(y_temp[, 1:3]) %>% as.data.frame(stringsAsFactors = FALSE)
      colnames(tab_freq_init) = c("let1", "let2", "let3", "freq1")
      for (i in 2:(nc + 2)) {
        tab_freq = table(y_temp[, (i - 1) + 1:3]) %>% as.data.frame(stringsAsFactors = FALSE)
        colnames(tab_freq) = c("let1", "let2", "let3", paste0("freq", i))
        tab_freq = tab_freq[tab_freq[, 4] > 0, ]
        tab_freq_init = merge(tab_freq_init, tab_freq,
                              by = c("let1", "let2", "let3"), all = TRUE)
      }
      
      # Keep only trigrams made of letters, apostrophes and hyphens (with accents)
      grp_ = " |^[-'a-zA-ZÀ-ÖØ-öø-ÿ]+$"
      tab_freq_init = tab_freq_init[
        grepl(grp_, tab_freq_init$let1) &
          grepl(grp_, tab_freq_init$let2) &
          grepl(grp_, tab_freq_init$let3),
      ]
      
      # Aggregate all positions for this word length
      tab_freq_init[, paste0("nc", nc)] = apply(
        tab_freq_init[, grep("freq", colnames(tab_freq_init))],
        1,
        function(x) sum(x, na.rm = TRUE)
      )
      tab_freq_init = tab_freq_init[, c(1:3, ncol(tab_freq_init))]
      tab_freq_final = merge(tab_freq_final, tab_freq_init,
                             all = TRUE, by = c("let1", "let2", "let3"))
    }
  }
  tab_freq_final$freq = apply(
    tab_freq_final[, grep("nc", colnames(tab_freq_final))],
    1,
    function(x) sum(x, na.rm = TRUE)
  )
  tab_freq_final = tab_freq_final[, c(1:3, ncol(tab_freq_final))]
  
  return(tab_freq_final)
})

max_ = 15
min_ = 5
size_ = 70000

markov_new = list()
for (lang_i in markov_languages_full) {
  x = markov_prob[[lang_i]]
  
  # Sample first two characters from contexts starting with double space
  x1 = x[x$let1 == " " & x$let2 == " ", ] %>%
    filter(let3 %in% unique(x$let2[x$let1 == " "])) %>%
    mutate(freq / sum(freq))
  
  samp1 = sample(x1$let3, size = size_, prob = x1$freq, replace = TRUE)
  samp2 = sapply(1:size_, function(i) {
    x2 = x[x$let1 == " " & x$let2 == samp1[i], ]
    x2$freq = x2$freq / sum(x2$freq)
    y = sample(x2$let3, size = 1, prob = x2$freq, replace = TRUE)
    return(y)
  })
  
  samp = data.frame(samp1 = samp1, samp2 = samp2)
  
  # Extend samples using trigram transitions until max length
  for (j in 3:(max_ + 1)) {
    print(j)
    samp[, j] = sapply(1:size_, function(i) {
      if (samp[i, j - 1] == " ") {
        y = " "
      } else {
        if (any((x$let1 == samp[i, j - 2]) & (x$let2 == samp[i, j - 1]))) {
          letterj = x[(x$let1 == samp[i, j - 2]) & (x$let2 == samp[i, j - 1]), ]
          letterj$freq = letterj$freq / sum(letterj$freq)
          y = sample(letterj$let3, size = 1, prob = letterj$freq, replace = TRUE)
        } else {
          y = " "
        }
      }
      return(y)
    })
  }
  
  # Build candidate "words" from sampled characters and filter by length
  temp = samp %>% 
    apply(1, FUN = function(x) paste(x, collapse = "")) %>%
    strsplit(" ") %>%
    lapply(`[[`, 1) %>%
    unlist()
  temp = temp[nchar(temp) >= min_ & nchar(temp) <= max_]
  
  # For each word length, keep up to 1500 unique samples
  y = c()
  for (i in min_:max_) {
    x_ = temp[nchar(temp) == i]
    y = c(y, sample(x_, min(length(x_), 1500)))
  }
  markov_new[[lang_i]] = y
}

# Save all Markov related objects for the application
save(list = ls(pattern = "markov_"), file = "Application/Saved/markov.RData")
