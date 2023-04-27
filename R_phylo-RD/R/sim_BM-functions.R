# params = [
#   [0.16, 0.08, 0.035, 0.065],
#   [0.14, 0.06, 0.035, 0.065],
# ]
#
#   Da, Db, f, k = param

# # ---------------
# # Arguments
# # Du(Da), Dv(Db), f, k
# # ---------------
# sim_pars <- list(
#   nSim = 1,
#   nTaxa = 10,
#   treeScale = 100,
#   birthRate = 1,
#   deathRate = 0,
#   #               Du(Da), Dv(Db), f, k
#   parsFixed   = c(0.16, NA, 0.035, 0.065),
#   rootValues  = c(0, 0.06, 0, 0.065),
#   mu          = c(0, 0, 0, 0),
#   sig2        = c(0, 0.001, 0, 0),
#
#   bmBounds = matrix(c(0, 0,
#                       0.05, 0.08,
#                       0, 0,
#                       0, 1), 4,2, byrow = TRUE)
# )
#
#
# # ---------------

# simTreeBM(sim_pars)
simTreeBM <- function(sim_pars){
  tree<-pbtree(n=sim_pars$nTaxa, scale=sim_pars$treeScale, b=sim_pars$birthRate, d=sim_pars$deathRate, nsim = 1)
  traits <- c()
  # Simulate Traits
  # i=1
  for (i in c(1:4)){
    if (is.na(sim_pars$parsFixed[i])){
      tip_values <- fastBM(tree, a=sim_pars$rootValues[i], mu=sim_pars$mu[i], sig2=sim_pars$sig2[i],
                           bounds=sim_pars$bmBounds[i,], internal=FALSE, nsim=1)
      # print(tip_values)
      traits <- cbind(traits, tip_values)
    } else if (is.numeric(sim_pars$parsFixed[i])) {
      tip_values <-rep(sim_pars$parsFixed[i], sim_pars$nTaxa)
      traits <- cbind(traits, tip_values)
    } else {
      print('Error: unknown format')
    }
  }
  colnames(traits) <- c('Du', 'Dv', 'f', 'k')
  lst <- split(traits, row(traits))
  names(lst) <- rownames(traits)
  # tree
  tree_str <- write.tree(tree, append = FALSE, digits = 10, tree.names = FALSE)
  data <- list(tree=tree_str, sim_pars = sim_pars,  tip_traits = lst)
  return(data)
}
