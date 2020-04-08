library("dml")


# https://cran.r-project.org/web/packages/dml/dml.pdf


data(iris)
k <- iris[, -5]
y <- iris[, 5]


# full gdm example
# set.seed(602)
# library(MASS)
# library(scatterplot3d)
# # generate simulated Gaussian data
# k = 100
# m <- matrix(c(1, 0.5, 1, 0.5, 2, -1, 1, -1, 3), nrow =3, byrow = T)
# x1 <- mvrnorm(k, mu = c(1, 1, 1), Sigma = m)
# x2 <- mvrnorm(k, mu = c(-1, 0, 0), Sigma = m)
# data <- rbind(x1, x2)
# # define similar constrains
# simi <- rbind(t(combn(1:k, 2)), t(combn((k+1):(2*k), 2)))
# temp <- as.data.frame(t(simi))
# tol <- as.data.frame(combn(1:(2*k), 2))
# # define disimilar constrains
# dism <- t(as.matrix(tol[!tol %in% simi]))
# # transform data using GdmDiag
# result <- GdmDiag(data, simi, dism)
# newData <- result$newData
# # plot original data
# color <- gl(2, k, labels = c("red", "blue"))
# par(mfrow = c(2, 1), mar = rep(0, 4) + 0.1)
# scatterplot3d(data, color = color, cex.symbols = 0.6,
# xlim = range(data[, 1], newData[, 1]),
# ylim = range(data[, 2], newData[, 2]),
# zlim = range(data[, 3], newData[, 3]),
# main = "Original Data")
# # plot GdmDiag transformed data
# scatterplot3d(newData, color = color, cex.symbols = 0.6,
# xlim = range(data[, 1], newData[, 1]),
# ylim = range(data[, 2], newData[, 2]),
# zlim = range(data[, 3], newData[, 3]),

# main = "Transformed Data")


# diag gmd

## Not run:
# set.seed(602)
library(MASS)
library(scatterplot3d)
# generate simulated Gaussian data
# k = 100
# # m <- matrix(c(1, 0.5, 1, 0.5, 2, -1, 1, -1, 3), nrow =3, byrow = T)
# # x1 <- mvrnorm(k, mu = c(1, 1, 1), Sigma = m)
# # x2 <- mvrnorm(k, mu = c(-1, 0, 0), Sigma = m)
# data <- rbind(x1, x2)

# data:  n * d data matrix. n is the number of data points, d is the dimension of the data.
# Each data point is a row in the matrix.

# simi:  n * 2 matrix describing the similar constrains. Each row of matrix is serial
# number of a similar pair in the original data. For example, pair(1, 3) represents
# the first observation is similar the 3th observation in the original data.

# dism:  n * 2 matrix describing the dissimilar constrains as simi. Each row of matrix
# is serial number of a dissimilar pair in the original data.

# C0:  numeric, the bound of similar constrains.

# threshold: numeric, the threshold of stoping the learning iteration.


data <- k
k <- 50

# a b c d target
# d d d d dkf
# d d d d dkfjd 
# d d d d dkf

# define similar constrains
simi <- rbind(t(combn(1:50, 2)), t(combn((51):(100), 2)), t(combn((101: 150), 2)))
temp <- as.data.frame(t(simi))

# all pairs
tol <- as.data.frame(combn(1:(150), 2))

# define disimilar constrains
dism <- t(as.matrix(tol[!tol %in% simi]))

# transform data using GdmDiag
result <- GdmDiag(data, simi, dism)

newData <- result$newData
# print(newData)

newData_ <- data.frame(newData)

# plot original data
color <- c("red", "blue", "yellow")
# par(mfrow = c(2, 1), mar = rep(0, 4) + 0.1)

get_colors <- function(groups, group.col = palette())
{
  groups <- as.factor(groups)
  ngrps <- length(levels(groups))
  if(ngrps > length(group.col)) 
    group.col <- rep(group.col, ngrps)
  color <- group.col[as.numeric(groups)]
  names(color) <- as.vector(groups)
  return(color)
}

# plot3d(data, col=get_colors(y), cex.symbols = 1,
# main = "Original Data")

# plot GdmDiag transformed data
# plot3d(newData[, 2:4], col=get_colors(y), cex.symbols = 1, main = "Transformed Data")
## End(Not run)


simi <- rbind(t(combn(1:50, 2)), t(combn((51):(100), 2)), t(combn((101: 150), 2)))
temp <- as.data.frame(t(simi))

# all pairs
tol <- as.data.frame(combn(1:(150), 2))

# define disimilar constrains
dism <- t(as.matrix(tol[!tol %in% simi]))

# transform data using GdmDiag
result_ <- GdmDiag(newData_[, 2:4], simi, dism)

newData__ <- result_$newData
print(newData__)

# https://arxiv.org/pdf/1509.04355.pdf: paper on metric learning