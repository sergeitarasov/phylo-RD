library(phytools)
library(jsonlite)

# ---------------
# Arguments
# Du(Da), Dv(Db), f, k
# ---------------
nSim = 1
nTaxa = 10
treeScale = 100
birthRate = 1
deathRate = 0

#               Du(Da), Dv(Db), f, k
parsFixed   = c(0.16, NA, 0.035, NA)
rootValues  = c(0, 0.08, 0, 0.065)
mu          = c(0, 0, 0, 0)
sig2        = c(0, 0.1, 0, 0.1)

bmBounds = matrix(c(0, 0,
                    0, 1,
                    0, 0,
                    0, 1), 4,2, byrow = TRUE)
# ---------------

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

# write.tree(tree, append = FALSE, digits = 10, tree.names = FALSE)
#
# class(traits)
# as.list(t(traits))

lst <- split(traits, row(traits))
lst
names(lst) <- rownames(traits)

# tree
tree_str <- write.tree(tree, append = FALSE, digits = 10, tree.names = FALSE)
data <- list(tree=tree_str, tip_traits = lst)
write_json(data, "table.json")


#----



# create a data frame with the table data
df <- data.frame(
  Du = c(0.16, 0.16),
  Dv = c(0.9767619, 0.8705721),
  f = c(0.035, 0.035),
  k = c(0.82825596, 0.98663531)
)

# set the row names
rownames(df) <- c("t7", "t8")

# convert the data frame to a named list
data <- as.list(df)

# write the data to a JSON file
write_json(data, "table.json")


#----
plot(tree)
phenogram(tree, traits[,2], spread.labels=TRUE, spread.cost=c(1,0))
#--
