library(rgl)
data(iris)

X <- NULL
y <- NULL
dim <- 3

dat <- NULL

Xtrain <- NULL
Xtest <- NULL
ytrain <- NULL
ytest <- NULL


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
    print(cols)
    if (dim_ == 2)
        plot(dat[, -ncol(dat)], col=cols)

    else if (dim_ == 3)
    {
        rgl_init()
        rgl.spheres(dat[, 1], dat[, 2], dat[, 3], color = get_colors(cols), r=0.01) 
        rgl_add_axes(dat[, 1], dat[, 2], dat[, 3], show.bbox = TRUE)
        aspect3d(1,1,1)
    }
}

train_test_split <- function()
{
    select <- sample(1:nrow(dat), 0.9 * nrow(dat))
    Xtrain <<- X[select,]
    Xtest <<- X[-select,]

    ytrain <<- y[select]
    ytest <<- y[-select]

}

accuracy <- function(x)
{
    return (sum(diag(x)/(sum(rowSums(x)))) * 100)
}

normalize <-function(x)
{
    if(max(x) != min(x))
        x = (x -min(x))/(max(x)-min(x))
    else
        x = x-x
    return(x)
}

plotclassified <- function(model, data)
{
    if (dim == 2)
        plot(model, data)
    else if (dim == 3)
    {
        plot(model, data, Comp.1~Comp.2)
        plot(model, data, Comp.2~Comp.3)
        plot(model, data, Comp.3~Comp.1)
        par(mfrow = c(1, 1))
    }
}

initialize <- function(dat_, dim_)
{
    dat <<- dat_

    rows <- nrow(dat)
    cols <- ncol(dat)

    y <<- dat[, cols]

    # normalize datahead
    dat_norm <- as.data.frame(lapply(dat[,1:cols-1], normalize))

    # to kernelize the data
    # X <<- kmatrixGauss(dat_norm[, -cols])
    X <<- dat_norm[, -cols]
    dim <<- dim_
}

apply_pca <- function()
{
    mypca <- princomp(X, cor=FALSE, score=TRUE)
    model__ <- mypca$scores
    model__ = data.frame(mypca$scores)

    # selecting best components based on dim
    X <<- model__[, c(1:dim)]
    return(X)
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


main <- function()
{

    dat_ <- read.csv("audit_risk.csv")
    times_taken <- rep(0, ncol(dat_)-2)
    accuracies <- rep(0, ncol(dat_)-2)
    
    

    for(iter in c(2,3)){
        for(num in 10:10){

            start.time <- Sys.time()

            initialize(dat_, iter)

            apply_pca()

            train_test_split()

            traindata <- data.frame(cbind(Xtrain, ytrain))
                        
            model_ <- svm(ytrain ~ ., data=traindata, type="C-classification")
            
            if((iter == 2  || iter == 3) && num == 10){

                 plotdata(X,y, iter)   
                
                 plotclassified(model_, traindata)
            }    

            pr = predict(model_, newdata = Xtest , type ='class')
            
            tab <- table(pr, ytest)
            accuracies[iter-1] = accuracies[iter-1] + accuracy(tab)

            end.time <- Sys.time()
            times_taken[iter-1] = times_taken[iter-1] + end.time - start.time

            
        }
        print("iteration")
        print(accuracies[iter-1])
    }

    

    times_taken = times_taken/50
    accuracies = accuracies/50

    plot(3:ncol(dat_)-1,times_taken, type = "b",
     xlab = "Number of dimensions",
     ylab = "Average time taken in seconds",
     main = "Effect of CoD on time taken in PCA + SVM")

    plot(3:ncol(dat_)-1,accuracies, type = "b",
     xlab = "Number of dimensions",
     ylab = "Average accuracy",
     main = "Effect of CoD on accuracy in PCA + SVM")

    grid()        
}

main()
