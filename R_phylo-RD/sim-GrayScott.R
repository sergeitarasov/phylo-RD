library(Rcpp) #to iterate fast
library(tidyverse) #to plot
library(reshape2) #to melt matrix into data frame
library(jsonlite)
source("R/Gray-Scott/rd.R")

#-- Read
json <- fromJSON("output/sim_BM.json")

# ---------------
# Arguments
# ---------------
dim = 50
B <- matrix(sample(c(0, 1), size = dim^2, replace = TRUE, prob = c(99,1)), ncol = dim)
dim(B)

# ---------------
i=1
for (i in 1:length(json$tip_traits)){
  # B <- matrix(sample(c(0, 1), size = dim^2, replace = TRUE, prob = c(99,1)), ncol = dim)
  tip_trait <- json$tip_traits[[i]]
  tip_name <- names(json$tip_traits[i])
  rdf <- rdiff(pixels=dim, DA=tip_trait[1], DB=tip_trait[2], f=tip_trait[3], k=tip_trait[4], B=B, iter=5000 )
  #rdf <- mutate(rdf, label=tip_name, px=c(1:nrow(rdf)))
  rdf <- as.matrix(rdf)
  json$tip_pattern[[tip_name]] <- rdf
}

# convert the list to a JSON string with indentation
json_str <- toJSON(json, pretty = TRUE, digits=NA)
# write the JSON string to a file
write(json_str, "output/sim_pattern.json")




#-----------------------
plot <- ggplot(data = rdf, aes(x = x, y = y, fill= B)) +
  geom_raster(interpolate = T) +
  coord_equal()
plot

my_list <- list(a = list(x = 1, y = 2), b = list(x = 3, y = 4))
names(my_list$a[[1]]) <- 'new_name'

#---- Plot
plot(tree)
phenogram(tree, traits[,2], spread.labels=TRUE, spread.cost=c(1,0))
#--

#-------------
ggplot(data = rdf, aes(x = x, y = y, fill= B)) +
  geom_raster(interpolate = T) +
  coord_equal()

#-- determenistic sample
n_squares <- 30 # n of species
f <- seq(0.04, 0.05, length.out =n_squares)
#k <- seq(0.06, 0.065, length.out =n_squares)
k <- seq(0.064, 0.065, length.out =1)
#id <-expand.grid(f.id = c(1:n_squares), k.id = c(1:n_squares))
id <-expand.grid(f.id = c(1:n_squares), k.id = c(1:1))

#-- stochastic sample
n_squares <- 20 # n of species
f <- runif(n_squares, min=0.04, max=0.05)
k <- seq(0.064, 0.065, length.out =1)
id <-expand.grid(f.id = c(1:n_squares), k.id = c(1:1))
#----

#id <-expand.grid(f.id = c(1:n_squares), k.id = c(1:n_squares))
#id <-expand.grid(f.id = c(1:n_squares), k.id = c(1:1))
pars <- cbind(id, expand.grid(f=f, k=k))
pars <- as_tibble(pars)
pars <-mutate(pars, fk=paste0(f.id,'x', k.id))
pars

out <- c()
# initial condition
B <- matrix(sample(c(0, 1), size = 100^2, replace = TRUE, prob = c(99,1)), ncol = pixels)
i=1
for (i in 1:nrow(pars)){
  rdf <- rdiff(pixels=100, DA=1, DB=0.5, f=pars$f[i], k=pars$k[i], B=B, iter=5000 )
  rdf <- mutate(rdf, label=pars$fk[i], px=c(1:nrow(rdf)))
  out <- bind_rows(out, rdf)
}

#----
B <- matrix(sample(c(0, 1), size = 100^2, replace = TRUE, prob = c(99,1)), ncol = 100)
dim(B)
rdf <- rdiff(pixels=100, DA=1, DB=0.5, f=0.04553253, k=0.064, B=B, iter=5000 )
# rdf <- mutate(rdf, label="1x1", px=c(1:nrow(rdf)))

plot <- ggplot(data = rdf, aes(x = x, y = y, fill= B)) +
  geom_raster(interpolate = T) +
  coord_equal()
plot
