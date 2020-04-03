# ## a simple example
# data(cats, package = "MASS")
# m <- svm(Sex~., data = cats)
# plot(m, cats)
# ## more than two variables: fix 2 dimensions
# data(iris)
# m2 <- svm(Species~., data = iris)
# plot(m2, iris, Petal.Width ~ Petal.Length,
# slice = list(Sepal.Width = 3, Sepal.Length = 4))
# ## plot with custom symbols and colors
# plot(m, cats, svSymbol = 1, dataSymbol = 2, symbolPalette = rainbow(4),
# color.palette = terrain.colors)


data(iris)
attach(iris)
## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris)
# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y, probability = TRUE)
print(model)
summary(model)
# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)
# compute decision values and probabilites
pred <- predict(model, x, decision.values = TRUE, probability = TRUE)
attr(pred, "decision.values")[1:4,]
attr(pred, "probabilities")[1:4,]
## try regression mode on two dimensions
# create data
x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)
# estimate model and predict input values
m <- svm(x, y)
new <- predict(m, x)
# visualize
plot (x, y)
points (x, log(x), col = 2)
points (x, new, col = 4)
## density-estimation
# create 2-dim. normal with rho=0:
X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)
# traditional way:
m <- svm(X, gamma = 0.1)
# formula interface:
m <- svm(~., data = X, gamma = 0.1)
# or:
m <- svm(~ a + b, gamma = 0.1)      
# test:
# newdata <- data.frame(a = c(0, 4), b = c(0, 4))
# predict (m, newdata)
# # visualize:
# plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
# points(newdata, pch = "+", col = 2, cex = 5)