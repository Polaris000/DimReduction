library(lfda)
library(rgl)
data(iris)


X <- NULL
y <- NULL
dim <- 3

# TODO:
# - train test split
# - add legend to charts
# - try on different dataset
# - generate subplots 

rgl_init <- function(new.device = FALSE, bg = "gray", width = 640)
{ 
  if( new.device | rgl.cur() == 0 ) 
  {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg)
  }

  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.5)
}

plotdata <- function(dat, cols, dim_)
{
    if (dim_ == 2)
        plot(dat, col=cols)
    else if (dim_ == 3)
    {
        rgl_init()
        rgl.spheres(dat[, 1], dat[, 2], dat[, 3], color = get_colors(dat$Species), r=0.01) 
        rgl_add_axes(dat[, 1], dat[, 2], dat[, 3], show.bbox = TRUE)
        aspect3d(1,1,1)
    }
}

plotclassified <- function(model, data)
{
    if (dim == 2)
        plot(model, data)
    else if (dim == 3)
    {
        par(mfrow = c(2, 2)) # 2-by-2 grid of plots
        par(oma = c(4, 4, 0, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
        par(mar = c(2, 2, 1, 1)) # make the plots be closer together
        plot(model, data, X1~X2)
        plot(model, data, X2~X3)
        plot(model, data, X3~X1)
        par(mfrow = c(1, 1))
    }
}

initialize <- function(dat, dim_)
{
    rows <- nrow(dat)
    cols <- ncol(dat)
    X <<- kmatrixGauss(dat[, -cols])
    y <<- dat[, cols]
    dim <<- dim_
}

fld <- function()
{
    model <- klfda(X, y, dim, metric = "plain")
    model__ = data.frame(model$Z)
    model__$Species = y
    return(model__)
}

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


initialize(iris, 3)

model__ <- fld()
plotdata(model__, iris$Species, 3)
model_ <- svm(Species ~., data=model__)

plotclassified(model_, model__)

