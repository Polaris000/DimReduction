x1 <- rnorm(n = 50, mean = 100, sd = 10)
x2 <- rnorm(n = 50, mean = 100, sd = 10)

y1 <- rnorm(n = 50, mean = 150, sd = 10)
y2 <- rnorm(n = 50, mean = 150, sd = 10)

df1 <- do.call(rbind, Map(data.frame, A=x1, B=x2))
df2 <- do.call(rbind, Map(data.frame, A=y1, B=y2))

df1$type = 0
df2$type = 1

dat <- rbind(df1, df2)

get_colors <- function(groups, group.col = palette()){
  groups <- as.factor(groups)
  ngrps <- length(levels(groups))
  if(ngrps > length(group.col)) 
    group.col <- rep(group.col, ngrps)
  color <- group.col[as.numeric(groups)]
  names(color) <- as.vector(groups)
  return(color)
}

print(dat)
plot(dat[, 1:2], col=get_colors(dat$type), type="p")

library(lfda)
library(rgl)

k <- kmatrixGauss(dat[, 1:2])
y <- dat[, 3]
r <- 2
model <- klfda(k, y, r, metric = "plain")
print(model)    

library(e1071)

model__ = data.frame(model$Z)
print(model__)

model__$type = y
# par(new=TRUE)
# plot(model__[1:2], col=get_colors(model__$type))
