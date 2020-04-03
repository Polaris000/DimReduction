# # regular
# library(lfda)
# data(iris)
# # print(iris)
# x <- iris[, -5]
# y <- iris[, 5]
# print(y)
# r <- 3
# model <- lfda(x, y, r, metric = "plain")
# transformedData <- predict(model, iris[, -5])
# # plot(x = model, labels = iris[, 5])

# # with kernel
library(lfda)
library(rgl)
data(iris)
k <- kmatrixGauss(iris[, -5])
y <- iris[, 5]
r <- 3
model <- klfda(k, y, r, metric = "plain")

# plot(x = model, labels = iris[, 5])

# # visualize
# library(lfda)
# library(ggfortify)
# data(iris)
# model <- self(iris[, -5], iris[, 5], beta = 0.1, r = 3, metric = "plain")
# autoplot(model, data = iris, frame = TRUE, frame.colour = ’Species’, frame.type = ’norm’)

# classify output of fld
library(e1071)
## classification mode
# default with factor response:

# print(model$Z)
# print(model$T)
typeof(model$Z)
model__ = data.frame(model$Z)
model__$Species = y
print(model__)

model_ <- svm(Species ~., data=model__)
print(model_)

# prints light points on gray background
# rgl.open()
# rgl.points(model__$X1, model__$X2, model__$X3, color ="lightgray")

rgl_add_axes <- function(x, y, z, axis.col = "grey",
                xlab = "", ylab="", zlab="", show.plane = TRUE, 
                show.bbox = FALSE, bbox.col = c("#333377","black"))
  { 
  
  lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}
  # Add axes
  xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
  rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.col)
  
   # Add a point at the end of each axes to specify the direction
   axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0), 
                 c(0, 0, zlim[2]))
   rgl.points(axes, color = axis.col, size = 3)
  
  # Add axis labels
  rgl.texts(axes, text = c(xlab, ylab, zlab), color = axis.col,
             adj = c(0.5, -0.8), size = 2)
  
  # Add plane
  if(show.plane) 
    xlim <- xlim/1.1; zlim <- zlim /1.1
    rgl.quads( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
             z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
  
  # Add bounding box decoration
  if(show.bbox){
    rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5, 
          emission=bbox.col[1], specular=bbox.col[1], shininess=5, 
          xlen = 3, ylen = 3, zlen = 3) 
  }
}

get_colors <- function(groups, group.col = palette()){
  groups <- as.factor(groups)
  ngrps <- length(levels(groups))
  if(ngrps > length(group.col)) 
    group.col <- rep(group.col, ngrps)
  color <- group.col[as.numeric(groups)]
  names(color) <- as.vector(groups)
  return(color)
}


# rgl.points(model__$X1, model__$X2, model__$X3, 
#           color = get_colors(model__$Species)) 
# rgl_add_axes(model__$X1, model__$X2, model__$X3, show.bbox = TRUE)
# aspect3d(1,1,1)

# Plot original data
plot3d(model__[,1:3], col=get_colors(model__$Species))

# Get decision values for a new data grid
newdat.list = lapply(model__[,1:3], function(x) seq(min(x), max(x), len=50))
# newdat      = expand.grid(newdat.list)
newdat.pred = predict(model_, newdata=newdat, decision.values=TRUE)
newdat.dv   = attr(newdat.pred, 'decision.values')
newdat.dv   = array(newdat.dv, dim=rep(50, 3))

print(head(newdat.dv))

# Fit/plot an isosurface to the decision boundary
contour3d(newdat.dv, level=0, x=newdat.list$X1, y=newdat.list$X2, z=newdat.list$X3, add=TRUE)
# contour3d(newdat.dv, level=0, x=newdat.list$X1, y=newdat.list$X2, z=newdat.list$X3, add=T)


# plot(model_, labels = iris[, 5])

# # alternatively the traditional interface:
# x <- subset(iris, select = -Species)
# y <- Species
# model <- svm(x, y) 

# print(model)
# summary(model)

# uncomment to run..................
# # test with train data
# pred <- predict(model, x)
# # (same as:)
# pred <- fitted(model)

# # Check accuracy:
# table(pred, y)

# # compute decision values and probabilities:
# pred <- predict(model, x, decision.values = TRUE)
# attr(pred, "decision.values")[1:4,]

# # visualize (classes by color, SV by crosses):
# plot(cmdscale(dist(iris[,-5])),
#      col = as.integer(iris[,5]),
#      pch = c("o","+")[1:150 %in% model$index + 1])

# ## try regression mode on two dimensions

# # create data
# x <- seq(0.1, 5, by = 0.05)
# y <- log(x) + rnorm(x, sd = 0.2)

# # estimate model and predict input values
# m   <- svm(x, y)
# new <- predict(m, x)

# # visualize
# plot(x, y)
# points(x, log(x), col = 2)
# points(x, new, col = 4)

# ## density-estimation

# # create 2-dim. normal with rho=0:
# X <- data.frame(a = rnorm(1000), b = rnorm(1000))
# attach(X)

# # traditional way:
# m <- svm(X, gamma = 0.1)

# # formula interface:
# m <- svm(~., data = X, gamma = 0.1)
# # or:
# m <- svm(~ a + b, gamma = 0.1)

# # test:
# newdata <- data.frame(a = c(0, 4), b = c(0, 4))
# predict (m, newdata)

# # visualize:
# plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
# points(newdata, pch = "+", col = 2, cex = 5)

# ## weights: (example not particularly sensible)
# i2 <- iris
# levels(i2$Species)[3] <- "versicolor"
# summary(i2$Species)
# wts <- 100 / table(i2$Species)
# wts
# m <- svm(Species ~ ., data = i2, class.weights = wts)

# ## extract coefficients for linear kernel

# # a. regression
# x <- 1:100
# y <- x + rnorm(100)
# m <- svm(y ~ x, scale = FALSE, kernel = "linear")
# coef(m)
# plot(y ~ x)
# abline(m, col = "red")

# # b. classification
# # transform iris data to binary problem, and scale data
# setosa <- as.factor(iris$Species == "setosa")
# iris2 = scale(iris[,-5])

# # fit binary C-classification model
# m <- svm(setosa ~ Petal.Width + Petal.Length,
#         data = iris2, kernel = "linear")

# # plot data and separating hyperplane
# plot(Petal.Length ~ Petal.Width, data = iris2, col = setosa)
# (cf <- coef(m))
# abline(-cf[1]/cf[3], -cf[2]/cf[3], col = "red")

# # plot margin and mark support vectors
# abline(-(cf[1] + 1)/cf[3], -cf[2]/cf[3], col = "blue")
# abline(-(cf[1] - 1)/cf[3], -cf[2]/cf[3], col = "blue")
# points(m$SV, pch = 5, cex = 2)
# # }