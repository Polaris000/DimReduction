library(lfda)
library(rgl)
data(iris)
k <- kmatrixGauss(iris[, -5])
y <- iris[, 5]
r <- 2
model <- klfda(k, y, r, metric = "plain")
print(model)

library(e1071)
## classification mode
# default with factor response:

model__ = data.frame(model$Z)
print(model__)

model__$Species = y
plot(model__[1:2], col=model__$Species)
# plot(model__, labels=model__$Species)
# print(model__, labels=model$Species)

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

plot(model_, model__)