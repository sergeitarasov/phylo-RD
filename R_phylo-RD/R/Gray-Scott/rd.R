# This scripts are from:
# https://github.com/aschinchon/reaction-diffusion
# https://www.r-bloggers.com/2019/12/reaction-diffusion/

# library(Rcpp) #to iterate fast
# library(tidyverse) #to plot
# library(reshape2) #to melt matrix into data frame

# Import C++ code
# Use Xcode  CommandLineTools 11.5 !!!!!
sourceCpp('R/Gray-Scott/cpp_funcs.cpp')

#B <- matrix(sample(c(0, 1), size = pixels^2, replace = TRUE, prob = c(99,1)), ncol = pixels)
rdiff <- function(pixels=400, DA=1, DB=0.5, f=0.0545, k=0.062, B, iter=5000){

  # The plot will be a 400x400 raster
  #pixels <- 400

  # Initialization of two matrix A and B that will form the tensor. Changing the way
  # of initializing B you will obtain different patterns. Try yourself!
  A <- matrix(1, pixels, pixels) #A is a zero matrix
  # B <- matrix(sample(c(0, 1),
  #                    size = pixels^2,
  #                    replace = TRUE,
  #                    prob = c(99,1)),
  #             ncol = pixels) #B is a binary one with much more 0's than 1's

  # Matrix L to perform convolutions
  L <- matrix(c(0.05, 0.2, 0.05,
                0.2,  -1, 0.2,
                0.05, 0.2, 0.05), nrow = 3)

  # DA and DB parameters
  #DA <- 1
  #DB <- 0.5

  # f and k parameters: play with them to obtain different patterns
  #f <- 0.0545
  #k <- 0.062

  # Create the tensor X
  X <- array(c(A, B) , dim = c(pixels, pixels, 2))

  # Perform interations of Gray-Scott algorithm (it may take a while)
  #X <- iterate_Gray_Scott(X, L, DA, DB, f, k, 5000)
  X <- iterate_Gray_Scott(X, L, DA, DB, f, k, iter)

  # Convert matrix B into  data frame preserving indexes
  df <- melt(X[,,2])
  colnames(df) <- c("x","y","B") # to name columns

  return(as_tibble(df))

}



