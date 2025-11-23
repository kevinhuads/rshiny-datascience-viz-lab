## Data preparation for the probability section
## Simulate coin toss experiments and estimate rejection rates (power)
## for different true probabilities, sample sizes and p-value thresholds.

coins_prob = lapply(c(10, 25, 50, 100, 150, 200), function(N) {
  x = 0:N
  y = choose(N, x) * (0.5^N)               # Binomial pmf under H0: p = 0.5 for N tosses
  M = 20000                                # Number of Monte Carlo repetitions per true p
  
  dat_prob = sapply(seq(0.5, 1, by = 0.02), function(p) {
    
    z = sapply(1:M, function(i) {
      
      rand_ = rbinom(n = N, size = 1, prob = p)   # Simulated tosses: 0 = tail, 1 = head
      
      # Convert 0/1 vector to counts of heads and tails
      if (all(rand_ == 0)) {
        tails_ = N
        heads_ = 0
      } else {
        if (all(rand_ == 1)) {
          tails_ = 0
          heads_ = N
        } else {
          tab_ = table(rand_)
          tails_ = tab_[1]
          heads_ = tab_[2]
        }
      }
      rand_ = list('head' = heads_, 'tail' = tails_)  # Not used later, kept for clarity
      
      # One sided p value under H0: P(X >= observed heads) with X ~ Bin(N, 0.5)
      probs = sum(y[heads_:length(y)])
      
      # For each significance level, record if H0 is rejected
      sapply(c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1), function(p_val) {
        probs < p_val
      })
      
    }) %>%
      t() %>%
      as.data.frame() %>%
      sapply(sum)                            # Count rejections across M simulations
    
    return(c(p, z))                          # First element is true p, then counts per threshold
  }) %>%
    t() %>%
    as.data.frame()
  
  colnames(dat_prob) = c('prob', paste0('pval', 1:7))
  
  # Long format: one row per (true p, threshold) with rejection count
  dat_prob = gather(dat_prob, key = 'p_value', value = 'negative', -prob, factor_key = TRUE)
  levels(dat_prob$p_value) = c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1)
  dat_prob$p_value = as.numeric(as.character(dat_prob$p_value))
  
  # Convert counts to rejection probabilities (estimated power)
  dat_prob$negative = dat_prob$negative / M
  
  return(dat_prob)
})

names(coins_prob) = paste0('N', c(10, 25, 50, 100, 150, 200))

# Save all objects related to coin simulations for the application
save(list = ls(pattern = 'coins'), file = 'Application/Saved/coins.RData')