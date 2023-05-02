library(tidyverse) #to plot
library(reshape2) #to melt matrix into data frame
library(jsonlite)
library(ggplot2)
library(intRinsic)

#-- Read
json <- fromJSON("output/sim_pattern.json")
#--

tip_pattern <- json$tip_pattern
# to tibble
tip_pattern <- lapply(tip_pattern, function(x) setNames(as_tibble(x), c('x', 'y', 'B')))

# add a column to each tibble containing the name of the list
tib_list <- lapply(names(tip_pattern), function(n) cbind(tip_pattern[[n]], name = n))

# add a column to each tibble containing the name of the list
tib_list <- lapply(names(tip_pattern), function(n) {
  tib <- tip_pattern[[n]]
  tib$tip_name <- n
  tib$px <- c(1:nrow(tib))
  return(tib)
})
names(tib_list) <- names(tip_pattern)

# bind the rows of the tibbles to make one tibble
length(tib_list)
tib_all <- bind_rows(tib_list)
tib_all

# ---- Plot
plot <- ggplot(data = tib_all, aes(x = x, y = y, fill= B)) +
  geom_raster(interpolate = T) +
  coord_equal() +
  facet_wrap( ~ tip_name, ncol =10)

plot


# --- Dimension
dt <- spread(tib_all[3:5], key = tip_name, value = B) %>% t()
dim(dt)
dt <- dt[-1,]
dt <- as_tibble(dt)
dt

dim <- twonn(dt, method='mle', c_trimmed=.001)
dim$est

dim <- twonn(dt, method="bayes")
dim$est

# dist matrix
dm <- dist(dt, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
dim1 <- twonn(dist_mat = dm, method='mle')
dim1$est

dm <- dist(dt, method = "manhattan", diag = FALSE, upper = FALSE, p = 2)
dim2 <- twonn(dist_mat = dm, method='mle')
dim2$est

dm <- dist(dt, method = "minkowski", diag = FALSE, upper = FALSE, p = .1)
dim3 <- twonn(dist_mat = dm, method='mle')
dim3$est

# ---- PCA
pca <- prcomp(dt)
summary(pca)
plot(pca$x[, 1:2], col = as.factor(nn))

pca <- as_tibble(irispca$x[, 1:2])
ggplot(data =pca, aes(x = PC1, y = PC2) )+
  geom_point(aes(size=2 )) +
  geom_text(aes(label=nn),hjust=1, vjust=1)

d <- dist(pca)
hcl <- hclust(d)
#hcl
plot(hcl)


# Hidalgo
h_out        <- Hidalgo(dt, nsim = 1000, burn_in = 5000)
plot(h_out, type =  "B")
id_by_class(h_out, oracle)

# Per cell dimension
twonn(dt[,1], method='mle')
apply(dt[,c(1:100)], 2, function(x) twonn(x, method='mle'))


#------------------
X       <-  replicate(5,rnorm(10000,0,.1))
gride_evolution(X = X,vec_n1 = 2^(0:5),vec_n2 = 2^(1:6))


library(salso)
X            <- replicate(5,rnorm(500))
X[1:250,1:2] <- 0
h_out        <- Hidalgo(X)
clustering(h_out)

X            <- replicate(5,rnorm(500))
X[1:250,1:2] <- 0
X[1:250,]    <- X[1:250,] + 4
oracle       <- rep(1:2,rep(250,2))
# this is just a short example
# increase the number of iterations to improve mixing and convergence
h_out        <- Hidalgo(X, nsim = 500, burn_in = 500)
plot(h_out, type =  "B")
id_by_class(h_out, oracle)






