library(phytools)
library(jsonlite)
source('R/sim_BM-functions.R')

# params = [
#   [0.16, 0.08, 0.035, 0.065],
#   [0.14, 0.06, 0.035, 0.065],
#   [0.16, 0.08, 0.06, 0.062],
#   [0.19, 0.05, 0.06, 0.062],
#   [0.16, 0.08, 0.02, 0.055],
#   [0.16, 0.08, 0.05, 0.065],
#   [0.16, 0.08, 0.054, 0.063],
#   [0.16, 0.08, 0.035, 0.06]
# ]
#
#   Da, Db, f, k = param

# ---------------
# Arguments
# Du(Da), Dv(Db), f, k
# ---------------
sim_pars <- list(
  nSim = 1,
  nTaxa = 10,
  treeScale = 100,
  birthRate = 1,
  deathRate = 0,
  #               Du(Da), Dv(Db), f, k
  parsFixed   = c(0.16, NA, 0.035, 0.065),
  rootValues  = c(0, 0.06, 0, 0.065),
  mu          = c(0, 0, 0, 0),
  sig2        = c(0, 0.001, 0, 0),

  bmBounds = matrix(c(0, 0,
                      0.05, 0.08,
                      0, 0,
                      0, 1), 4,2, byrow = TRUE)
)
# ---------------

data <- simTreeBM(sim_pars)
data
# convert the list to a JSON string with indentation
json_str <- toJSON(data, pretty = TRUE)
# write the JSON string to a file
write(json_str, "output/sim_BM.json")


#------- Read

json <- fromJSON("output/sim_BM.json")

#----


#---- Plot
plot(tree)
phenogram(tree, traits[,2], spread.labels=TRUE, spread.cost=c(1,0))
#--
