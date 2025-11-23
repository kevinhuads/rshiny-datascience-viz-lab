# Data preparation for the regression section

# Required packages -------------------------------------------------------

library(FNN)          # KNN regression
library(xgboost)      # XGBoost models
library(randomForest) # Random forest
library(rpart)        # CART trees
library(e1071)        # SVM models
library(doParallel)   # Parallel backend for foreach
library(foreach)      # Parallel loops
library(tidyr)        # gather()
source("Application/global_functions.R")
n_cores <- max(2, detectCores()-4) 

bikes = read.csv('Data_Prep/Rawdata/Bikes.csv')

## Feature engineering ----------------------------------------------------

# Extract hour of day as numeric feature for modelling
bikes_hour_vec = as.integer(format(
  strptime(bikes$datetime, format = "%Y-%m-%d %H:%M:%S"),
  '%H'
))

# Define qualitative and quantitative features used in the models
bikes_quali   = c('season','holiday','workingday','weather')
bikes_quanti  = c('temp','atemp','humidity','windspeed')
bikes_quanti2 = c('temp','humidity','windspeed')
bikes_ml_feats = c('hour', bikes_quali, bikes_quanti2)


# Machine Learning data sets ---------------------------------------------

# Main modelling table with engineered hour and selected predictors
bikes_ml = cbind(
  bikes_hour_vec,
  bikes[, c(bikes_quanti2, bikes_quali, 'count')]
)
colnames(bikes_ml)[1] = 'hour'

# Convert appropriate columns to factors
for (i in c('hour', bikes_quali)) {
  bikes_ml[, i] = as.factor(bikes_ml[, i])
}

# Train / validation / test split (60 / 20 / 20)
set.seed(1)
sample = sample(1:nrow(bikes_ml))
bikes_train = bikes_ml[sample[1:(0.6*nrow(bikes_ml))], ]
bikes_val   = bikes_ml[sample[(0.6*nrow(bikes_ml)+1):(0.8*nrow(bikes_ml))], ]
bikes_test  = bikes_ml[sample[(0.8*nrow(bikes_ml)+1):nrow(bikes_ml)], ]

# Target and feature set
bikes_y = 'count'
bikes_features = c('hour','season','holiday','workingday','weather',
                   'temp','humidity','windspeed')

# Base GLM formulas (linear and with squared quantitative terms)
bikes_formula = as.formula(
  paste(bikes_y, '~', paste(bikes_features, collapse = '+'))
)
bikes_formula_square = as.formula(
  paste(
    bikes_y, '~',
    paste(c(
      bikes_features,
      paste0('I(', bikes_quanti2, '^2)')
    ), collapse = '+')
  )
)

# GLM (Gamma, log link)
bikes_lm = list()
bikes_lm$linear = glm(
  data = bikes_train,
  formula = bikes_formula,
  family = Gamma(link = 'log'),
  model = FALSE
)
bikes_lm$square = glm(
  data = bikes_train,
  formula = bikes_formula_square,
  family = Gamma(link = 'log'),
  model = FALSE
)

# Validation deviance-based performance for GLMs
bikes_lm_dev = sapply(bikes_lm, function(x) {
  1 - er_gamma(
    predict(x, newdata = bikes_val, type = 'response'),
    obs = bikes_val$count
  )
})


### KNN -------------------------------------------------------------------

bikes_err = list()
err_gamma = c()
vec_knn = c(1:20, 25, 30, 40, 50, 75, 100)

# Validation performance for several KNN settings and algorithms
err_knn = as.data.frame(sapply(c("kd_tree", "cover_tree", "brute"), function(x) {
  for (k in vec_knn) {
    pred_knn = knn.reg(
      train = sapply(
        bikes_train[, c('hour', bikes_quanti2, bikes_quali)],
        function(x) as.numeric(as.character(x))
      ),
      test = sapply(
        bikes_val[, c('hour', bikes_quanti2, bikes_quali)],
        function(x) as.numeric(as.character(x))
      ),
      y = bikes_train$count,
      k = k,
      algorithm = x
    )
    err_gamma = c(
      err_gamma,
      1 - er_gamma(pred = pred_knn$pred, obs = bikes_val$count)
    )
  }
  err_gamma
}))
err_knn$k = vec_knn
err_knn = gather(err_knn, algorithm, error, kd_tree:brute, factor_key = TRUE)

bikes_err$knn = err_knn


### XGBoost -----------------------------------------------------------------

# Numeric-only copy for XGBoost (same rows as bikes_ml)
biknum_ml = bikes_ml
for (i in 1:ncol(biknum_ml)) {
  biknum_ml[, i] = as.numeric(biknum_ml[, i])
}

set.seed(1)
sample = sample(1:nrow(biknum_ml))
biknum_train = biknum_ml[sample[1:(0.6*nrow(biknum_ml))], ]
biknum_valid = biknum_ml[sample[(0.6*nrow(biknum_ml)+1):(0.8*nrow(biknum_ml))], ]
biknum_test  = biknum_ml[sample[(0.8*nrow(biknum_ml)+1):nrow(biknum_ml)], ]

train_label = as.numeric(biknum_train[, bikes_y])
valid_label = as.numeric(biknum_valid[, bikes_y])

# Feature columns for XGBoost (exclude target)
feat_cols = setdiff(colnames(biknum_train), bikes_y)

# DMatrix for XGBoost
xgb.train = xgb.DMatrix(
  data  = as.matrix(biknum_train[, feat_cols]),
  label = train_label
)
xgb.valid = xgb.DMatrix(
  data  = as.matrix(biknum_valid[, feat_cols]),
  label = valid_label
)

# More compact XGBoost hyperparameter grid (128 combos)
searchgrid <- expand.grid(
  subsample        = c(0.7, 1.0),
  colsample_bytree = c(0.7, 1.0),
  max_depth        = c(3, 6),
  eta              = c(0.3, 0.1),
  gamma            = c(0, 1),
  lambda           = c(0.1, 1),
  alpha            = c(0, 1)
)

# Container for XGBoost results
biknum_results = searchgrid
biknum_results$dev = 0

j = sum(biknum_results$dev != 0)

# XGBoost grid search on validation set (Gamma deviance based)
for (i in sample(1:nrow(searchgrid))) {
  if (biknum_results$dev[i] == 0) {
    params = list(
      booster          = "gbtree",
      eta              = searchgrid$eta[i],
      max_depth        = searchgrid$max_depth[i],
      gamma            = searchgrid$gamma[i],
      lambda           = searchgrid$lambda[i],
      alpha            = searchgrid$alpha[i],
      subsample        = searchgrid$subsample[i],
      colsample_bytree = searchgrid$colsample_bytree[i],
      objective        = "reg:gamma",
      eval_metric      = "gamma-deviance"
    )
    
    # Early stopping on training set metric (kept as in original code)
    xgb.fit = xgboost(
      params  = params,
      data    = xgb.train,
      nrounds = 20000,
      nthread = n_cores,
      early_stopping_rounds = 100,
      verbose = TRUE,
      print_every_n = 5000
    )
    
    pred = predict(xgb.fit, xgb.valid)
    pred[pred < 0] = 1
    
    biknum_results$dev[i] = 1 - er_gamma(pred = pred, obs = bikes_val$count)
    
    j = j + 1
    print(paste(
      'Grid seach', j, 'over', nrow(searchgrid),
      ". Error Reduction", biknum_results$dev[i]
    ))
  }
}

# Store XGBoost validation performance
bikes_err$xgboost = biknum_results[biknum_results$dev != 0, ]

# Select best XGBoost hyperparameters (highest error reduction)
best_xgb_row = bikes_err$xgboost[which.max(bikes_err$xgboost$dev), ]

best_xgb_params = list(
  booster          = "gbtree",
  eta              = best_xgb_row$eta,
  max_depth        = best_xgb_row$max_depth,
  gamma            = best_xgb_row$gamma,
  lambda           = best_xgb_row$lambda,
  alpha            = best_xgb_row$alpha,
  subsample        = best_xgb_row$subsample,
  colsample_bytree = best_xgb_row$colsample_bytree,
  objective        = "reg:gamma",
  eval_metric      = "gamma-deviance"
)

# Final XGBoost model (retrained on train set with best hyperparameters)
xgb.final = xgboost(
  params  = best_xgb_params,
  data    = xgb.train,
  nrounds = 20000,
  nthread = n_cores,
  early_stopping_rounds = 100,
  verbose = TRUE,
  print_every_n = 5000
)

# Variable importance from XGBoost, kept under previous object name
xgb_imp = xgb.importance(
  feature_names = feat_cols,
  model         = xgb.final
)
xgb_imp$rel_inf = xgb_imp$Gain / sum(xgb_imp$Gain)
xgb_imp$var     = paste0(xgb_imp$Feature, ' ')
xgb_imp$var     = factor(xgb_imp$var, levels = rev(xgb_imp$var))

bikes_gbm_varimp = xgb_imp




### Tree ------------------------------------------------------------------

max_depth = 13
df_gamma = as.data.frame(matrix(0, ncol = 2, nrow = max_depth))
colnames(df_gamma) = c('depth','dev')
df_gamma$depth = 1:max_depth

# Validation deviance across tree depths
for (i in 1:max_depth) {
  bikes_tree = rpart(
    data = bikes_train,
    formula = bikes_formula,
    maxdepth = i,
    cp = -1
  )
  pred_tree = predict(bikes_tree, newdata = bikes_val)
  df_gamma$dev[i] = 1 - er_gamma(pred = pred_tree, obs = bikes_val$count)
} 

bikes_err$tree = df_gamma


### Random Forest ---------------------------------------------------------

ntree_list = c(10, 50, 100, 250, 500, 750)
mtry_list  = 1:length(bikes_features)

df_gamma = as.data.frame(
  matrix(0, ncol = 3,
         nrow = length(ntree_list) * length(mtry_list))
)
colnames(df_gamma) = c('ntree','features','dev')

j = 1
# Validation deviance across RF hyperparameters
for (ntree in ntree_list) {
  for (mfeat in mtry_list) {
    
    bikes_rf = randomForest(
      bikes_formula,
      data = bikes_train,
      ntree = ntree,
      mtry = mfeat,
      keep.forest = TRUE
    )
    pred_rf = predict(bikes_rf, newdata = bikes_val, type = 'response')
    
    df_gamma$ntree[j]    = ntree
    df_gamma$features[j] = mfeat
    df_gamma$dev[j]      = 1 - er_gamma(pred = pred_rf, obs = bikes_val$count)
    
    print(paste(ntree, mfeat, er_gamma(pred = pred_rf, obs = bikes_val$count)))
    j = j + 1
  }
}

bikes_err$rf = df_gamma 


### SVM ------------------------------------------------------------------

save(list = ls(), file = "checkpoints.RData")



# Hyperparameter grids for SVM regression
type_list   = c('nu','eps')
kernel_list = c('polynomial','radial','linear','sigmoid')
coef0_list  = c(-5, -2.5, 0, 2.5, 5)
gamma_list  = seq(0.05, 0.4, 0.05)
nu_list     = seq(0.1, 1, 0.1)
eps_list    = seq(0, 0.25, 0.05)
cost_list   = 2^((-2):7)
degree_list = 1:5

# First SVM grid search, writing detailed results to CSV files
for (x in type_list) {
  for (y in kernel_list) {
    
    cl <- makeCluster(n_cores)
    registerDoParallel(cl, cores = n_cores)
    
    print(paste(x, y))
    x_type = paste0(x, '-regression')
    x_cols = c('cost', x)
    if (y %in% c('polynomial','radial','sigmoid')) {
      x_cols = c('gamma', x_cols)
      if (y %in% c('polynomial','sigmoid')) {
        x_cols = c('coef0', x_cols)
        if (y %in% 'polynomial') {
          x_cols = c('degree', x_cols)
        }
      }
    }
    
    if (x == 'nu') x_list = nu_list else x_list = eps_list
    
    path      = paste0(getwd(), '/')
    path_full = paste0(path, "perf4_svm_", x, "_", y, ".csv")
    
    fileConn <- file(path_full)
    writeLines(paste0(c(x_cols, 'dev'), collapse = ","), fileConn)
    
    existing = read.csv(path_full)
    
    foreach(cost = cost_list, .packages = c('foreach')) %dopar% {
      foreach(a = x_list, .packages = c('foreach','e1071')) %dopar% {
        if (x == 'nu') {
          if (y == 'linear') {
            svm_model = svm(
              formula = bikes_formula,
              data = bikes_train,
              nu = a,
              cost = cost,
              type = x_type,
              kernel = y
            )
            pred_svm = predict(svm_model, newdata = bikes_val, type = 'response')
            pred_svm[pred_svm < 1] = 1
            dev = 1 - er_gamma(pred = pred_svm, obs = bikes_val$count)
            sink(path_full, append = TRUE)
            cat(paste0(paste(cost, a, dev, sep = ","), "\n"))
            sink()
          } 
          if (y == 'radial') {
            foreach(gamma = gamma_list, .packages = c('foreach','e1071')) %do% {
              svm_model = svm(
                formula = bikes_formula,
                data = bikes_train,
                nu = a,
                cost = cost,
                gamma = gamma,
                type = x_type,
                kernel = y
              )
              pred_svm = predict(svm_model, newdata = bikes_val, type = 'response')
              pred_svm[pred_svm < 1] = 1
              dev = 1 - er_gamma(pred = pred_svm, obs = bikes_val$count)
              sink(path_full, append = TRUE)
              cat(paste0(paste(gamma, cost, a, dev, sep = ","), "\n"))
              sink()
            }
          }
          if (y == 'sigmoid') {
            foreach(gamma = gamma_list, .packages = c('foreach','e1071')) %do% {
              foreach(coef0 = coef0_list, .packages = c('foreach','e1071')) %do% {
                svm_model = svm(
                  formula = bikes_formula,
                  data = bikes_train,
                  nu = a,
                  cost = cost,
                  gamma = gamma,
                  coef0 = coef0,
                  type = x_type,
                  kernel = y
                )
                pred_svm = predict(svm_model, newdata = bikes_val, type = 'response')
                pred_svm[pred_svm < 1] = 1
                dev = 1 - er_gamma(pred = pred_svm, obs = bikes_val$count)
                sink(path_full, append = TRUE)
                cat(paste0(paste(coef0, gamma, cost, a, dev, sep = ","), "\n"))
                sink()
              }
            }
          }
          if (y == 'polynomial') {
            foreach(gamma = gamma_list, .packages = c('foreach')) %do% {
              foreach(coef0 = coef0_list, .packages = c('foreach')) %do% {
                foreach(degree = degree_list, .packages = c('foreach','e1071')) %do% {
                  svm_model = svm(
                    formula = bikes_formula,
                    data = bikes_train,
                    nu = a,
                    degree = degree,
                    cost = cost,
                    gamma = gamma,
                    coef0 = coef0,
                    type = x_type,
                    kernel = y
                  )
                  pred_svm = predict(svm_model, newdata = bikes_val, type = 'response')
                  pred_svm[pred_svm < 1] = 1
                  dev = 1 - er_gamma(pred = pred_svm, obs = bikes_val$count)
                  sink(path_full, append = TRUE)
                  cat(paste0(paste(degree, coef0, gamma, cost, a, dev, sep = ","), "\n"))
                  sink()
                }
              }
            }
          }
        } else {
          if (y == 'linear') {
            svm_model = svm(
              formula = bikes_formula,
              data = bikes_train,
              epsilon = a,
              cost = cost,
              type = x_type
            )
            pred_svm = predict(svm_model, newdata = bikes_val, type = 'response')
            pred_svm[pred_svm < 1] = 1
            dev = 1 - er_gamma(pred = pred_svm, obs = bikes_val$count)
            sink(path_full, append = TRUE)
            cat(paste0(paste(cost, a, dev, sep = ","), "\n"))
            sink()
          } 
          if (y == 'radial') {
            foreach(gamma = gamma_list, .packages = c('foreach','e1071')) %do% {
              svm_model = svm(
                formula = bikes_formula,
                data = bikes_train,
                epsilon = a,
                cost = cost,
                gamma = gamma,
                type = x_type,
                kernel = y
              )
              pred_svm = predict(svm_model, newdata = bikes_val, type = 'response')
              pred_svm[pred_svm < 1] = 1
              dev = 1 - er_gamma(pred = pred_svm, obs = bikes_val$count)
              sink(path_full, append = TRUE)
              cat(paste0(paste(gamma, cost, a, dev, sep = ","), "\n"))
              sink()
            }
          }
          if (y == 'sigmoid') {
            foreach(gamma = gamma_list, .packages = c('foreach','e1071')) %do% {
              foreach(coef0 = coef0_list, .packages = c('foreach','e1071')) %do% {
                svm_model = svm(
                  formula = bikes_formula,
                  data = bikes_train,
                  epsilon = a,
                  cost = cost,
                  gamma = gamma,
                  coef0 = coef0,
                  type = x_type,
                  kernel = y
                )
                pred_svm = predict(svm_model, newdata = bikes_val, type = 'response')
                pred_svm[pred_svm < 1] = 1
                dev = 1 - er_gamma(pred = pred_svm, obs = bikes_val$count)
                sink(path_full, append = TRUE)
                cat(paste0(paste(coef0, gamma, cost, a, dev, sep = ","), "\n"))
                sink()
              }
            }
          }
          if (y == 'polynomial') {
            foreach(gamma = gamma_list, .packages = c('foreach')) %do% {
              foreach(coef0 = coef0_list, .packages = c('foreach')) %do% {
                foreach(degree = degree_list, .packages = c('foreach','e1071')) %do% {
                  svm_model = svm(
                    formula = bikes_formula,
                    data = bikes_train,
                    epsilon = a,
                    degree = degree,
                    cost = cost,
                    gamma = gamma,
                    coef0 = coef0,
                    type = x_type,
                    kernel = y
                  )
                  pred_svm = predict(svm_model, newdata = bikes_val, type = 'response')
                  pred_svm[pred_svm < 1] = 1
                  dev = 1 - er_gamma(pred = pred_svm, obs = bikes_val$count)
                  sink(path_full, append = TRUE)
                  cat(paste0(paste(degree, coef0, gamma, cost, a, dev, sep = ","), "\n"))
                  sink()
                }
              }
            }
          }
        }
      }
    }
    close(fileConn)
    stopCluster(cl)
  }
}

# Second SVM loop (completing / checking missing combinations)
for (x in type_list) {
  for (y in kernel_list) {
    
    print(paste(x, y))
    x_type = paste0(x, '-regression')
    x_cols = c('cost', x)
    if (y %in% c('polynomial','radial','sigmoid')) {
      x_cols = c('gamma', x_cols)
    }
    
    if (x == 'nu') x_list = nu_list else x_list = eps_list
    
    path      = paste0(getwd(), '/')
    path_full = paste0(path, "perf_svm_", x, "_", y, ".csv")
    fileConn  = file(path_full)
    existing  = read.csv(path_full)
    
    j = 0
    for (cost in cost_list) {
      for (a in x_list) {
        if (x == 'nu') {
          if (y == 'linear') {
            if (!paste0(cost, ',', a) %in%
                paste0(as.numeric(existing$cost), ',', as.numeric(existing[, x]))) {
              j = j + 1
              print(j)
              svm_model = svm(
                formula = bikes_formula,
                data = bikes_train,
                nu = a,
                cost = cost,
                type = x_type
              )
              pred_svm = predict(svm_model, newdata = bikes_val, type = 'response')
              pred_svm[pred_svm < 1] = 1
              dev = er_gamma(pred = pred_svm, obs = bikes_val$count)
              sink(path_full, append = TRUE)
              cat(paste0(paste(cost, a, dev, sep = ","), "\n"))
              sink()
            }
          } else {
            for (gamma in gamma_list) {
              if (!paste0(gamma, ',', cost, ',', a) %in%
                  paste0(as.numeric(existing$gamma), ',', as.numeric(existing$cost), ',', as.numeric(existing[, x]))) {
                j = j + 1
                print(j)
                svm_model = svm(
                  formula = bikes_formula,
                  data = bikes_train,
                  nu = a,
                  cost = cost,
                  gamma = gamma,
                  type = x_type
                )
                pred_svm = predict(svm_model, newdata = bikes_val, type = 'response')
                pred_svm[pred_svm < 1] = 1
                dev = er_gamma(pred = pred_svm, obs = bikes_val$count)
                sink(path_full, append = TRUE)
                cat(paste0(paste(gamma, cost, a, dev, sep = ","), "\n"))
                sink()
              }
            }
          }
        } else {
          if (y == 'linear') {
            if (!paste0(cost, ',', a) %in%
                paste0(as.numeric(existing$cost), ',', as.numeric(existing[, x]))) {
              j = j + 1
              print(j)
              svm_model = svm(
                formula = bikes_formula,
                data = bikes_train,
                epsilon = a,
                cost = cost,
                type = x_type
              )
              pred_svm = predict(svm_model, newdata = bikes_val, type = 'response')
              pred_svm[pred_svm < 1] = 1
              dev = er_gamma(pred = pred_svm, obs = bikes_val$count)
              sink(path_full, append = TRUE)
              cat(paste0(paste(cost, a, dev, sep = ","), "\n"))
              sink()
            }
            
          } else {
            for (gamma in gamma_list) {
              if (!paste0(gamma, ',', cost, ',', a) %in%
                  paste0(as.numeric(existing$gamma), ',', as.numeric(existing$cost), ',', as.numeric(existing[, x]))) {
                j = j + 1
                print(j)
                svm_model = svm(
                  formula = bikes_formula,
                  data = bikes_train,
                  epsilon = a,
                  cost = cost,
                  gamma = gamma,
                  type = x_type
                )
                pred_svm = predict(svm_model, newdata = bikes_val, type = 'response')
                pred_svm[pred_svm < 1] = 1
                dev = er_gamma(pred = pred_svm, obs = bikes_val$count)
                sink(path_full, append = TRUE)
                cat(paste0(paste(gamma, cost, a, dev, sep = ","), "\n"))
                sink()
              }
            }
          }
        }
      }
    }
    close(fileConn)
  }
}

# Collect SVM performance from precomputed CSVs in 'Performance' folder
df_gamma = lapply(c('nu','eps'), function(x) {
  df = lapply(
    list.files('Performance', pattern = x, full.names = TRUE),
    function(y) read.csv(y)
  )
  names(df) = c('linear','polynomial','radial','sigmoid')
  df
})
names(df_gamma) = c('nu','eps')
bikes_err$svm = df_gamma


### Test set performance --------------------------------------------------

bikes_algo = c('GLM','KNN','SVM','CART','RF','XGB')

# Container for test predictions
bikes_pred = list()

# GLM on test set (squared specification)
bikes_pred$glm = predict(
  bikes_lm$square,
  newdata = bikes_test,
  type = 'response'
)

# Best KNN configuration chosen from validation performance
bikes_err$knn[which.max(bikes_err$knn$dev), ]
bikes_pred$knn = knn.reg(
  train = sapply(
    bikes_train[, c('hour', bikes_quanti2, bikes_quali)],
    function(x) as.numeric(as.character(x))
  ),
  test = sapply(
    bikes_test[, c('hour', bikes_quanti2, bikes_quali)],
    function(x) as.numeric(as.character(x))
  ),
  y = bikes_train$count,
  k = 3,
  algorithm = 'kd_tree'
)$pred

# Inspect best SVM configurations by type and kernel
lapply(c('nu','eps'), function(y) {
  lapply(bikes_err$svm[[y]], function(x) {
    x[which.max(x$dev), ]
  })
})

# Final SVM model chosen from previous search
bikes_svm = svm(
  formula = bikes_formula,
  data = bikes_train,
  epsilon = 0,
  gamma = 0.1,
  cost = 1,
  kernel = 'radial',
  type = 'eps-regression'
)
bikes_pred$svm = predict(bikes_svm, newdata = bikes_test, type = 'response')
bikes_pred$svm[bikes_pred$svm < 1] = 1

# Compact SVM performance summary for display / analysis
bikes_df_svm_wide = as.data.frame(sapply(c('nu','eps'), function(y) {
  sapply(bikes_err$svm[[y]], function(x) {
    x[which.max(x$dev), 'dev']
  })
}))
bikes_df_svm = bikes_df_svm_wide
bikes_df_svm$kernel = rownames(bikes_df_svm)
bikes_df_svm = gather(bikes_df_svm, key = 'type', value = 'performance', nu:eps)

colnames(bikes_df_svm_wide) = c('Nu','Epsilon')
rownames(bikes_df_svm_wide) = c('Linear','Polynomial','Radial','Sigmoid')

# Final CART model on test set
bikes_tree = rpart(
  data = bikes_train,
  formula = bikes_formula,
  maxdepth = 11,
  cp = -1
)
bikes_pred$tree = predict(bikes_tree, newdata = bikes_test)

# Final RF model on test set
bikes_err$rf[which.max(bikes_err$rf$dev), ]
bikes_rf = randomForest(
  bikes_formula,
  data = bikes_train,
  ntree = 750,
  mtry = 8,
  keep.forest = TRUE
)
bikes_pred$rf = predict(
  bikes_rf,
  newdata = bikes_test,
  type = 'response'
)

# Final XGBoost predictions on test set (numeric version)
bikes_pred$xgb = predict(
  xgb.final,
  newdata = as.matrix(biknum_test[, feat_cols])
)
bikes_pred$xgb[bikes_pred$xgb < 1] = 1

# Aggregate performance metrics on test set (Gamma deviance and MSE based)
bikes_perf = sapply(bikes_pred, function(x) {
  c(
    'Deviance (Gamma)'              = dev_gamma(pred = x, obs = bikes_test$count),
    'Mean Square Error'             = sum((x - bikes_test$count)^2) / nrow(bikes_test),
    'Error Reduction Ratio (Gamma)' = 1 - er_gamma(pred = x, obs = bikes_test$count),
    'Error Reduction Ratio (MSE)'   = 1 - sum((x - bikes_test$count)^2) /
      sum((mean(bikes_test$count) - bikes_test$count)^2)
  )
})
bikes_perf = as.data.frame(bikes_perf)
colnames(bikes_perf) = 'Performance'
bikes_perf$Model = rownames(bikes_perf)

# Reorder models by performance for later display
bikes_perf = bikes_perf[order(bikes_perf$Performance, decreasing = FALSE), ]
bikes_perf$Model = factor(bikes_perf$Model, levels = bikes_perf$Model)
levels(bikes_perf$Model) = rev(c(
  'Boosted Trees ',
  'Random Forest ',
  'Regression Tree ',
  'Support Vector Machine ',
  'Generalized Linear Model ',
  'K Nearest Neighbors '
))

# Refine SVM performance table (labels only)
bikes_df_svm$type   = rep(c('Nu Regression','Epsilon Regression'), each = 4)
bikes_df_svm$kernel = paste0(bikes_df_svm$kernel, ' ')


# Cleanup and saving ------------------------------------------------------

# Remove large objects not needed in the saved RData
rm(list = c('bikes_rf','bikes_pred','bikes_svm','bikes_tree'))

# Mapping for qualitative features (for display in the application)
bikes_quali_names = list(
  'season' = c('Spring' = '1','Summer' = '2', 'Fall' = '3','Winter' = '4'),
  'weather' = c(
    'Clear, Few Clouds'      = '1',
    'Mist and Cloudy'        = '2',
    'Light Snow, Light Rain' = '3',
    'Heavy Snow, Heavy Rain' = '4'
  )
)

# Save all objects whose name starts with "bikes" for use in the Shiny app
save(list = ls(pattern = 'bikes'), file = 'Application/Saved/bikes.RData')
