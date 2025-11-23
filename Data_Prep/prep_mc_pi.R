library(dplyr) # Data manipulation

pi_n = seq(1,7,0.25)      # Exponents for sample sizes (10^1 ... 10^7 by 0.25)
pi_n_launch = 5000        # Number of independent Monte Carlo runs

pi_rand = as.data.frame(t(sapply(1:pi_n_launch,function(k){  # Matrix of errors per run (rows) and sample size (cols)
  df = data.frame(
    x = runif(10^max(pi_n), min = -1, max = 1),             # X-coordinates in [-1,1]
    y = runif(10^max(pi_n), min = -1, max = 1)              # Y-coordinates in [-1,1]
  ) %>% 
    mutate(color = ifelse(x^2 + y^2 >= 1, 'blue', 'red'))   # Inside unit circle = 'red', outside = 'blue'
  
  sapply(pi_n,function(i){                                  # Error for each sample size 10^i
    err = pi - 4*sum(df$color[1:(10^i)] == 'red')/(10^i)    # π - Monte Carlo estimate
    return(err)
  })
})
))

save(list = ls(pattern = 'pi_'), file = 'Application/Saved/pi.RData') # Save all π-related objects