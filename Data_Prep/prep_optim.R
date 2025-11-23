## Data preparation for the optimisation section (TSP)
## Generate random city sets and random-tour distance distributions
## for different numbers of cities, used as baselines in the TSP visualisation.

optim_set = 8 # Number of independent city sets per n (used to average behaviour)
optim_throw= 50000 # Number of random tours simulated per set
optim_nvlist = c(25,50,100,150) # Different numbers of cities to consider
optim_dist = 100 # Side length of the square in which cities are uniformly sampled
optim_Tinit = 10000 # Initial temperature for simulated annealing (used elsewhere)

# Generate random city coordinates for each nv in optim_nvlist
optim_villes = lapply(optim_nvlist, function(nv) {
  y = lapply(1:optim_set,function(i){
    set.seed(1000*nv+i) ; villes = data.frame(x = optim_dist*runif(nv),y = optim_dist*runif(nv))
    villes$names = paste('City',1:nv)
    return(villes)
  })
  names(y) = paste0('Set',1:optim_set)
  return(y)
})
names(optim_villes) = paste0('cities',optim_nvlist)

# For each nv, compute random-tour distances for each city set
optim_rands = lapply(1:length(optim_nvlist), function(n){
  y = sapply(1:optim_set,function(i){
    villes = optim_villes[[n]][[i]][,1:2]
    nv = optim_nvlist[n]
    J = c()
    for (i in 1:optim_throw){
      # Random tour: always start at city 1, then random permutation of remaining, return to city 1
      sol_aleat=c(1,1+sample(nv-1),1)
      vil_aleat=villes[sol_aleat,]
      dist_aleat<-as.matrix(dist(vil_aleat))
      # Sum of distances along the tour (diagonal just above the main)
      J = c(J,sum(diag(dist_aleat[2:(nv+1),1:nv])))
    }
    return(J)
  })
  colnames(y) = paste0('Set',1:optim_set)
  return(as.data.frame(y))
})
names(optim_rands) = paste0('cities',optim_nvlist)

# Save all optimisation-related objects to RData
save(list = ls(pattern = 'optim'),file = 'Application/Saved/optim.RData')
