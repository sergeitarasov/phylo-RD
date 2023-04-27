# params = [
#   [0.16, 0.08, 0.035, 0.065],
#   [0.14, 0.06, 0.035, 0.065],
# ]
#
#   Da, Db, f, k = param

# ---------------
# Arguments
# Du(Da), Dv(Db), f, k
# ---------------
pars <- list(
  nSim = 1,
  nTaxa = 10,
  treeScale = 100,
  birthRate = 1,
  deathRate = 0,
  #               Du(Da), Dv(Db), f, k
  parsFixed   = c(0.16, NA, 0.035, NA),
  rootValues  = c(0, 0.08, 0, 0.065),
  mu          = c(0, 0, 0, 0),
  sig2        = c(0, 0.1, 0, 0.1),

  bmBounds = matrix(c(0, 0,
                      0, 1,
                      0, 0,
                      0, 1), 4,2, byrow = TRUE)
)


# ---------------


simTreeBM <- function(pars){
  tree<-pbtree(n=nTaxa, scale=treeScale, b=birthRate, d=deathRate, nsim = 1)

  traits <- c()
  # Simulate Traits
  # i=1
  for (i in c(1:4)){
    if (is.na(parsFixed[i])){
      tip_values <- fastBM(tree, a=rootValues[i], mu=mu[i], sig2=sig2[i], bounds=bmBounds[i,], internal=FALSE, nsim=1)
      # print(tip_values)
      traits <- cbind(traits, tip_values)
    } else if (is.numeric(parsFixed[i])) {
      tip_values <-rep(parsFixed[i], nTaxa)
      traits <- cbind(traits, tip_values)
    } else {
      print('Error: unknown format')
    }
  }
  colnames(traits) <- c('Du', 'Dv', 'f', 'k')
  return(t)
}
