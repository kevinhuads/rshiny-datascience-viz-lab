########################################################################
# prep_nlp_hotel.R
#
# Preprocessing for the Hotel Reviews NLP section
# - Reads the raw Kaggle hotel review data
# - Builds hotel-level and review-level aggregates
# - Prepares text objects for word clouds, graphs and LDA
# - Computes sentiment features and summaries
# - Saves all objects whose name contains "hotel" to hotel.RData
########################################################################

# Objects saved in hotel.RData and used in ui_nlp.R / server_nlp.R
# hotel_df               : Data frame of reviews (subset for data table)
# hotel_byhot            : One row per hotel with coordinates and metadata (for leaflet map)
# hotel_sent             : Daily average reviewer scores (overall and by country)
# hotel_quant            : Quantiles of overall daily score (for ggvis bands)
# hotel_nlda             : Maximum number of topics for LDA controls
# hotel_filter           : Logical flags for presence of positive / negative reviews
# hotel_range            : Binned score ranges with counts and average word counts
# hotel_tdm              : Term frequencies by polarity and n-gram length
# hotel_pairs            : Bigram counts by polarity for graph networks
# hotel_lda              : LDA topic-term tables by polarity and number of topics
# hotel_layouts          : igraph layout functions used for graphs
# hotel_layouts_labels   : Labels/groups of layouts used in selectInput choices


# 1. Packages --------------------------

library(dplyr)
library(tidyr)
library(tm)
library(RWeka)
library(slam)
library(topicmodels)
library(broom)
library(rworldmap)
library(sp)
library(stringr)
library(tidytext)
library(algorithmia)
library(syuzhet)
library(text2vec)
library(glmnet)
library(igraph)
library(reshape2)


# 2. Configuration --------------------------

overwrite_save <- FALSE                  # Overwrite existing hotel.RData if TRUE
raw_dir        <- "Data_Prep/Rawdata/"   # Folder with raw CSV
save_dir       <- "Application/Saved/"   # Folder where hotel.RData will be written

#############################
# 3. Load raw data and hotel-level information
#############################

# Full review-level data
hot_df <- read.csv(file.path(raw_dir, "Hotel_Reviews.csv"))

# Hotel-level features to keep for the map and summaries
hotel_features <- c(
  "Hotel_Address",
  "Additional_Number_of_Scoring",
  "Average_Score",
  "Total_Number_of_Reviews",
  "lat",
  "lng"
)

# One row per hotel with static metadata
hotel_byhot <- hot_df[, c("Hotel_Name", hotel_features)]
hotel_byhot <- hotel_byhot[!duplicated(hotel_byhot$Hotel_Name), ]
hotel_byhot <- hotel_byhot[order(hotel_byhot$Hotel_Name), ]

# Number of reviews per hotel (used in the map/table)
hotel_byhot$Reviews <- as.data.frame(table(hot_df$Hotel_Name))[, 2]

# Remove hotel-level columns from the review-level data
hot_df <- hot_df[, !(colnames(hot_df) %in% hotel_features)]

# Maximum number of topics used in the LDA controls (UI uses k in 2:(hotel_nlda + 1))
hotel_nlda <- 7

# Convert review date
hot_df$Review_Date <- as.Date(hot_df$Review_Date, format = "%m/%d/%Y")



# 4. Country information and daily averages --------------------------

# Country inferred from the last token of the hotel address
hotel_byhot$Country <- sapply(
  hotel_byhot$Hotel_Address,
  function(x) tail(strsplit(as.character(x)[1], split = " ")[[1]], 1)
)
hotel_byhot$Country[hotel_byhot$Country == "Kingdom"] <- "United Kingdom"

# Attach country to each review (for daily country-level averages)
hot_df <- merge(
  hot_df,
  hotel_byhot[, c("Hotel_Name", "Country")],
  by = "Hotel_Name"
)

# Daily average reviewer score (overall)
hotel_sent <- aggregate(
  hot_df$Reviewer_Score,
  by   = list(hot_df$Review_Date),
  FUN  = mean
) %>%
  dplyr::rename(date = Group.1, Overall = x) %>%
  # Daily average reviewer score by country, reshaped to wide format
  dplyr::left_join(
    aggregate(
      hot_df$Reviewer_Score,
      by   = list(hot_df$Review_Date, hot_df$Country),
      FUN  = mean
    ) %>%
      tidyr::spread(Group.2, x) %>%
      dplyr::rename(date = Group.1),
    by = "date"
  )

# Quantiles of the overall daily score, used in ggvis bands in the Shiny app
# (server_nlp uses elements 2,3,5,10,15,19,20)
hotel_quant <- stats::quantile(
  hotel_sent$Overall,
  probs = seq(0, 1, by = 0.05),
  na.rm = TRUE
)

#############################
# 5. Reviewer nationality and per-day counts
#############################

# Share of reviews and average score by reviewer nationality
hotel_nat <- as.data.frame(
  sort(table(hot_df$Reviewer_Nationality) / nrow(hot_df), decreasing = TRUE)
)
colnames(hotel_nat) <- c("Country", "Reviews")

temp_nat <- aggregate(
  hot_df$Reviewer_Score,
  by  = list(hot_df$Reviewer_Nationality),
  FUN = mean
)
colnames(temp_nat) <- c("Country", "Average_score")

hotel_nat <- merge(hotel_nat, temp_nat)
hotel_nat <- hotel_nat[order(hotel_nat$Reviews, decreasing = TRUE), ]

# Simple colour coding by average score (kept for potential maps)
cols    <- c("#ff0000", "#f05336", "#ffff00", "#eec73a", "#00ff00")
col_len <- 20
col_br  <- stats::quantile(
  hotel_nat$Average_score,
  seq(0, 1, length.out = col_len + 1),
  na.rm = TRUE
)

hotel_nat$col <- cut(hotel_nat$Average_score, col_br, include.lowest = TRUE)
levels(hotel_nat$col) <- grDevices::colorRampPalette(cols)(col_len)
hotel_nat$col <- as.character(hotel_nat$col)

# Number of reviews per calendar day
hotel_perday <- as.data.frame(table(hot_df$Review_Date))
colnames(hotel_perday) <- c("Date", "Reviews")
hotel_perday$Date      <- as.POSIXct(hotel_perday$Date)
hotel_perday$weekdays  <- weekdays(hotel_perday$Date)
hotel_perday$weekdays  <- factor(
  hotel_perday$weekdays,
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
)

# 6. Map-based country assignment for hotels -------------------

# Overwrite hotel_byhot$Country using spatial polygons (for the leaflet map)
coords2country <- function(points) {  
  countriesSP <- rworldmap::getMap(resolution = "low")
  pointsSP    <- sp::SpatialPoints(points, proj4string = sp::CRS(sp::proj4string(countriesSP)))
  indices     <- sp::over(pointsSP, countriesSP)
  indices$ADMIN  
}

#hotel_byhot$Country <- droplevels(coords2country(hotel_byhot[, c("lng", "lat")]))
hotel_byhot_country <- as.data.frame(table(hotel_byhot$Country))


# 7. Global means and score filters -------------------

# Overall means (kept for reference)
hotel_mean <- c(
  Review  = mean(hot_df$Reviewer_Score, na.rm = TRUE),
  Country = mean(hotel_nat$Average_score, na.rm = TRUE)
)

# Flags indicating whether each review contains positive / negative comments
hotel_filter <- data.frame(
  Positive = hot_df$Positive_Review != "No Positive",
  Negative = hot_df$Negative_Review != "No Negative"
)

# Binned reviewer score ranges and associated word counts
#   hotel_range[[i]][[break_length]] is a data frame with:
#     Review_Range, Num_Reviews, Num_Pos, Num_Neg
#   i = 1: all reviews
#   i = 2: reviews with a positive comment
#   i = 3: reviews with a negative comment
#   i = 4: reviews with both positive and negative comments
hotel_range <- vector("list", length = 4)

for (i in 1:4) {
  temp <- hot_df
  if (i == 2) temp <- temp[hotel_filter$Positive, ]
  if (i == 3) temp <- temp[hotel_filter$Negative, ]
  if (i == 4) temp <- temp[hotel_filter$Positive & hotel_filter$Negative, ]
  
  hotel_range[[i]] <- lapply(c(0.5, 1, 2), function(x) {
    hotel_cut <- cut(temp$Reviewer_Score, seq(2, 10, x))
    
    y <- as.data.frame(table(hotel_cut))
    colnames(y) <- c("Review_Range", "Num_Reviews")
    
    y$Num_Pos <- aggregate(
      temp$Review_Total_Positive_Word_Counts,
      by  = list(hotel_cut),
      FUN = mean
    )[, 2]
    
    y$Num_Neg <- aggregate(
      temp$Review_Total_Negative_Word_Counts,
      by  = list(hotel_cut),
      FUN = mean
    )[, 2]
    
    y
  })
  
  names(hotel_range[[i]]) <- c(0.5, 1, 2)
}


# 8. Text cleaning helpers -------------------

replace_ap <- function(x) gsub("'", "", x)
replace_nt <- function(x) gsub("n t ", "nt ", x)
replace_s  <- function(x) gsub(" s ", " ", x)

# 9. Corpora for positive and negative reviews (tm) -------------------

# docs[["Positive"]], docs[["Negative"]] are VCorpus objects
docs <- lapply(c("Positive", "Negative"), function(x) {
  hot_df[, paste0(x, "_Review")] %>%
    as.character() %>%
    tm::VectorSource() %>%
    tm::VCorpus() %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(content_transformer(replace_ap)) %>%
    tm_map(content_transformer(replace_nt)) %>%
    tm_map(content_transformer(replace_s)) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removePunctuation)
})
names(docs) <- c("Positive", "Negative")



# 10. Term frequencies for n-grams (1 to 4) -------------------

# hotel_tdm[["Positive" or "Negative"]][[ngram_length]] is a named integer vector
# of term counts, filtered to terms with frequency > 2.
hotel_tdm <- lapply(docs, function(corpus) {
  lapply(1:4, function(i) {
    tokenizer <- function(x) {
      RWeka::NGramTokenizer(x, RWeka::Weka_control(min = i, max = i))
    }
    
    term_counts <- tm::TermDocumentMatrix(
      corpus,
      control = list(tokenize = tokenizer)
    ) %>%
      slam::row_sums(na.rm = TRUE)
    
    term_counts[term_counts > 2]
  })
})


# 11. LDA topic models -------------------

# hotel_lda[["Positive" or "Negative"]][[k_index]]:
#   k_index = 1 corresponds to k = 2 topics, up to k = hotel_nlda + 1
# Each element is a tibble with columns: topic (renumbered), term, beta
hotel_lda <- lapply(c("Positive", "Negative"), function(x) {
  
  dtm <- hot_df[hot_df[, paste0(x, "_Review")] != paste("No", x), paste0(x, "_Review")] %>%
    as.character() %>%
    tm::VectorSource() %>%
    tm::VCorpus() %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(content_transformer(replace_ap)) %>%
    tm_map(content_transformer(replace_nt)) %>%
    tm_map(content_transformer(replace_s)) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english")) %>%
    # tm_map(stemDocument, language = "english") %>%
    tm_map(removePunctuation) %>%
    tm::DocumentTermMatrix() %>%
    tm::removeSparseTerms(0.99)
  
  # Remove empty documents
  row_totals <- apply(dtm, 1, sum)
  dtm        <- dtm[row_totals > 0, ]
  
  # Fit LDA models for k = 2 to hotel_nlda + 1 topics
  lapply(2:(hotel_nlda + 1), function(k) {
    topicmodels::LDA(dtm, k = k, method = "Gibbs", control = list(seed = 1234)) %>%
      broom::tidy(matrix = "beta") %>%
      dplyr::group_by(topic) %>%
      dplyr::slice_max(beta, n = 200, with_ties = FALSE)
  })
})
names(hotel_lda) <- c("Positive", "Negative")

# Reorder topics by approximate prominence and sort terms within each topic
hotel_lda <- lapply(hotel_lda, function(models_for_polarity) {
  lapply(seq_along(models_for_polarity), function(i) {
    y <- models_for_polarity[[i]]
    
    # Reorder topics by approximate global importance
    ordering_df <- data.frame(
      topic = order(c(y[1 + 100 * 0:i, "beta"])[[1]], decreasing = TRUE),
      order = 1:(i + 1)
    )
    
    y <- merge(y, ordering_df)
    y <- y[order(y$order, rev(y$beta)), c("order", "term", "beta")]
    colnames(y)[1] <- "topic"
    
    y %>%
      dplyr::group_by(topic) %>%
      dplyr::arrange(topic, dplyr::desc(beta))
  })
})

save(list = c("hotel_lda","docs"), file = "lda_checkpoints.RData")


# 12. Sentiment scores (NRC, lexicons, VADER, GloVe) -------------------


# # Sentiments is a list with two data frames ("Positive", "Negative"),
# # each with one row per review and the following columns:
# #   - NRC emotion scores
# #   - syuzhet, bing, afinn, nrc overall sentiment
# #   - glove: glmnet model score on a tf-idf representation
# #   - vader_*: VADER-like scores from Algorithmia
# algo_token = Sys.getenv("ALGO_TOKEN")
# client <- getAlgorithmiaClient("algo_token")
# algo   <- client$algo("nlp/SocialSentimentAnalysis/0.1.4")
# 
# # Pretrained objects for the GloVe / glmnet model
# load("Performance/Word2vec_params.RData")
# # model <- load_model("Performance//wiki.en")  
# 
# Sentiments <- lapply(c("Positive", "Negative"), function(x) {
#   text <- as.character(hot_df[, paste0(x, "_Review")])
#   
#   # NRC emotion scores and several lexicon-based sentiment scores
#   df <- syuzhet::get_nrc_sentiment(text)
#   for (m in c("syuzhet", "bing", "afinn", "nrc")) {
#     df[, m] <- syuzhet::get_sentiment(text, method = m)
#   }
#   
#   # GloVe-style sentiment score using a glmnet model on a tf-idf representation
#   df$glove <- predict(
#     glmnet_classifier,
#     text2vec::itoken(
#       text,
#       preprocessor = prep_fun,
#       tokenizer    = tok_fun,
#       ids          = seq_along(text),
#       progressbar  = TRUE
#     ) %>%
#       create_dtm(vectorizer) %>%
#       fit_transform(text2vec::TfIdf$new()),
#     type = "response"
#   )[ , 1]
#   
#   # VADER-style sentiment scores via Algorithmia (batched)
#   n_part <- 50
#   parts  <- round(seq(0, length(text), length.out = n_part))
#   temp   <- vector("list", length = n_part - 1)
#   
#   for (i in 1:(n_part - 1)) {
#     message("VADER batch ", i, " / ", n_part - 1)
#     
#     temp[[i]] <- algo$pipe(
#       stringi::stri_enc_toutf8(
#         text[(parts[i] + 1):parts[i + 1]],
#         is_unknown_8bit = FALSE,
#         validate        = FALSE
#       )
#     )$result %>%
#       unlist() %>%
#       matrix(
#         nrow = length(text[(parts[i] + 1):parts[i + 1]]),
#         byrow = TRUE
#       ) %>%
#       as.data.frame() %>%
#       dplyr::select(1:4) %>%
#       stats::setNames(paste0("vader_", c("compound", "negative", "neutral", "positive")))
#   }
#   
#   vader_df <- temp %>%
#     lapply(function(x) {
#       x[] <- lapply(x, function(y) as.numeric(as.character(y)))
#       x
#     }) %>%
#     dplyr::bind_rows()
#   
#   df[, paste0("vader_", c("compound", "negative", "neutral", "positive"))] <- vader_df
#   
#   df
# })
# 
# names(Sentiments) <- c("Positive", "Negative")
# 
# 
# 
# # 13. Sentiment summaries by polarity -------------------
# 
# 
# # Sentiment.RData just saved; reload to build derived hotel_* objects
# 
# 
# # Label each sub-data frame with its review polarity
# Sentiments[[1]]$category <- "Positive Reviews"
# Sentiments[[2]]$category <- "Negative Reviews"
# 
# # Sample a subset of reviews that contain both positive and negative text
# set.seed(1)
# samp <- sample(sum(hotel_filter$Positive & hotel_filter$Negative), 30000)
# 
# for (i in 1:2) {
#   Sentiments[[i]] <- Sentiments[[i]][hotel_filter$Positive & hotel_filter$Negative, ]
#   Sentiments[[i]] <- Sentiments[[i]][sort(samp), ]
# }
# 
# # Combined sample of sentiment scores
# hotel_sentsamp <- dplyr::bind_rows(Sentiments)
# 
# # VADER summary by polarity (normalised within each category)
# hotel_vader <- aggregate(
#   hotel_sentsamp[, paste0("vader_", c("negative", "neutral", "positive"))],
#   by  = list(hotel_sentsamp$category),
#   FUN = mean
# )
# hotel_vader <- tidyr::gather(
#   hotel_vader,
#   key   = "vader",
#   value = "value",
#   vader_negative:vader_positive,
#   factor_key = TRUE
# )
# hotel_vader$vader <- substr(
#   hotel_vader$vader,
#   7,
#   nchar(as.character(hotel_vader$vader))
# )
# 
# hotel_vader$value <- hotel_vader$value /
#   ave(hotel_vader$value, hotel_vader$Group.1, FUN = sum)
# 
# # NRC emotion summary by polarity (long format)
# hotel_nrc <- t(
#   aggregate(
#     hotel_sentsamp[, 1:10],
#     by   = list(hotel_sentsamp$category),
#     FUN  = mean
#   )
# )
# 
# colnames(hotel_nrc) <- hotel_nrc[1, ]
# hotel_nrc           <- hotel_nrc[-1, ]
# 
# hotel_nrc <- as.data.frame(cbind(
#   as.numeric(hotel_nrc),
#   rep(colnames(hotel_nrc), each = nrow(hotel_nrc)),
#   rep(rownames(hotel_nrc), times = 2)
# ))
# 
# colnames(hotel_nrc) <- c("percent", "category", "sentiments")
# hotel_nrc$percent   <- as.numeric(as.character(hotel_nrc$percent))
# hotel_nrc$category  <- factor(
#   hotel_nrc$category,
#   levels = levels(hotel_nrc$category)[2:1]
# )
# 
# # Wide format NRC table (one row per polarity, one column per emotion)
# hotel_nrc_rad <- reshape2::dcast(
#   hotel_nrc,
#   category ~ sentiments,
#   value.var = "percent"
# )
# rownames(hotel_nrc_rad) <- hotel_nrc_rad[, 1]
# hotel_nrc_rad           <- hotel_nrc_rad[, -1]
# 
# # Helper list of sentiment variable names (used elsewhere in the app)
# hotel_var_list        <- list()
# hotel_var_list$nrc    <- c("overall", colnames(hotel_sentsamp)[1:10])
# colnames(hotel_sentsamp)[1:10] <- paste0("nrc_", colnames(hotel_sentsamp)[1:10])
# colnames(hotel_sentsamp)[14]   <- "nrc_overall"
# hotel_var_list$vader  <- c("compound", "negative", "neutral", "positive")
# 
# 
# 


# 14. Bigram co-occurrences for graph networks -------------------

# hot_df_text: cleaned positive / negative text columns
hot_df_text <- hot_df[, c("Positive_Review", "Negative_Review")]
colnames(hot_df_text) <- c("Positive", "Negative")

for (pol in c("Positive", "Negative")) {
  hot_df_text[, pol] <- hot_df_text[, pol] %>%
    as.character() %>%
    stringr::str_replace_all("[[:punct:]]", " ") %>%
    stringr::str_replace_all("[[å£³]]", " ") %>%
    gsub(pattern = "'", replacement = " ") %>%
    gsub(pattern = "-", replacement = " ") %>%
    replace_ap() %>%
    replace_nt() %>%
    replace_s() %>%
    tolower() %>%
    tm::removeNumbers() %>%
    tm::removePunctuation() %>%
    tm::removeWords(stopwords("eng")) %>%
    gsub(pattern = "\\b\\w{1,2}\\s", replacement = "")
}

# hotel_pairs[["Positive" or "Negative"]] is a data frame with columns:
#   word1, word2, n (bigram counts), restricted to the most frequent pairs
hotel_pairs <- lapply(c("Positive", "Negative"), function(x) {
  df <- hot_df_text %>%
    dplyr::select(all_of(x))
  colnames(df) <- "text"
  
  df <- df %>%
    dplyr::filter(gsub(" ", "", text) != tolower(x)) %>%
    dplyr::filter(gsub(" ", "", text) != "nothing")
  
  words_counts <- df %>%
    tidytext::unnest_tokens(paired_words, text, token = "ngrams", n = 2) %>%
    tidyr::separate(paired_words, c("word1", "word2"), sep = " ") %>%
    dplyr::count(word1, word2, sort = TRUE) %>%
    dplyr::filter(word1 != word2)
  
  # Keep the most frequent bigrams to limit graph size
  words_counts[1:min(20000, nrow(words_counts)), ]
})
names(hotel_pairs) <- c("Positive", "Negative")


# 15. Graph layout definitions -------------------

# hotel_layouts: mapping from layout name to igraph layout function
hotel_layouts <- list(
  "Fruchterman-Reingold"     = igraph::layout_with_fr,
  "Kamada-Kawai"             = igraph::layout_with_kk,
  "Davidson-Harel"           = igraph::layout_with_dh,
  "Multidimensional Scaling" = igraph::layout_with_mds,
  "Random"                   = igraph::layout_randomly,
  "Grid"                     = igraph::layout_on_grid,
  # "Star"                     = igraph::layout_star,
  "GEM"                      = igraph::layout_with_gem,
  "GraphOpt"                 = igraph::layout_with_graphopt
)

# hotel_layouts_labels: grouped labels used as choices in the UI
hotel_layouts_labels <- list(
  "Forced Directed" = c(
    "Fruchterman-Reingold",
    "Kamada-Kawai",
    "Davidson-Harel"
  ),
  "Others" = c(
    "Multidimensional Scaling",
    "Random",
    "Grid",
    # "Star",
    "GEM",
    "GraphOpt"
  )
)


# 16. Final trimming and sampling for UI performance -------------------

# Keep only the top 2 000 terms per n-gram and polarity in hotel_tdm
hotel_tdm <- lapply(hotel_tdm, function(polarity_list) {
  lapply(polarity_list, function(freq_vec) {
    ordered <- sort(freq_vec, decreasing = TRUE)
    freq_vec[names(freq_vec) %in% names(ordered)[1:min(2000, length(ordered))]]
  })
})

# hotel_df: subset of hot_df used in the Shiny data table (first 5 000 rows)
hotel_df <- hot_df[1:5000, ]
for (i in which(sapply(hotel_df, class) == "factor")) {
  hotel_df[, i] <- droplevels(hotel_df[, i])
}


# 17. Save all hotel_* objects -------------------

# Save every object whose name contains "hotel" into hotel.RData
rdata_path <- file.path(save_dir, "hotel.RData")
if (!file.exists(rdata_path) || overwrite_save) {
  save(list = ls(pattern = "hotel"), file = rdata_path)
}
