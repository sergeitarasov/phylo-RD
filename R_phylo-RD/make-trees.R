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
  nTaxa = 100,
  treeScale = 100,
  birthRate = 1,
  deathRate = 0,
  #               Du(Da), Dv(Db), f, k
  parsFixed   = c(NA, NA, 0.0545, 0.062),
  rootValues  = c(0.95, 0.5, 0.055, 0.062),
  mu          = c(0, 0, 0, 0),
  sig2        = c(0.002, 0.002, 0.002, 0.002),

  bmBounds = matrix(c(0.65, 1,
                      0.4, 0.6,
                      0.05, 0.06,
                      0.05, 0.070), 4,2, byrow = TRUE)
)
# ---------------

data <- simTreeBM(sim_pars)
data
# convert the list to a JSON string with indentation
json_str <- toJSON(data, pretty = TRUE, digits=NA)
# write the JSON string to a file
write(json_str, "output/sim_BM.json")


#------- Read

json <- fromJSON("output/sim_BM.json")

#----


#---- Plot
tree <- read.tree(text=data$tree)
trait <- lapply(data$tip_traits, function(x) x[2]) %>% unlist
plot(tree)
phenogram(tree, trait, spread.labels=TRUE, spread.cost=c(1,0))
#--

trait3 <- lapply(data$tip_traits, function(x) x[3]) %>% unlist
trait4 <- lapply(data$tip_traits, function(x) x[4]) %>% unlist
plot(trait3, trait4)
plot(trait3, trait3+trait4)
