library(lfda)
library(rgl)
data(iris)
library(e1071)
library(Metrics)


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
    if (dim_ == 2)
        plot(dat, col=cols)
    else if (dim_ == 3)
    {
        rgl_init()
        rgl.spheres(dat[, 1], dat[, 2], dat[, 3], color = get_colors(dat[, ncol(dat)]), r=0.05) 
        rgl_add_axes(dat[, 1], dat[, 2], dat[, 3], show.bbox = TRUE)
        aspect3d(1,1,1)
    }
}

plotclassified <- function(model, data_)
{
    print(typeof(data_))
    print(model)
    

    if (dim == 2)
        plot(model, data_)

    else if (dim == 3)
    {
        plot(model, data_, X1~X2)
        # plot(model, data, X2~X3)
        # plot(model, data, X3~X1)  
        par(mfrow = c(2, 2))
    }

    if (dim == 1)
        plot(model, data_, X1)
}

normalize <-function(x) 
{
    if(max(x) != min(x))
        x = (x -min(x))/(max(x)-min(x))
    else
        x = x-x
    return(x)
}

initialize <- function(dat_, dim_)
{
    dat <<- dat_

    rows <- nrow(dat)
    cols <- ncol(dat)

    y <<- dat[, cols]

    # normalize data
    dat_norm <- as.data.frame(lapply(dat[,1:cols-1], normalize))

    X <<- kmatrixGauss(dat_norm[, -cols])
    
    dim <<- dim_
}

train_test_split <- function()
{

    # train-test split
    select <- sample(1:nrow(dat), 0.9 * nrow(dat))
    Xtrain <<- X[select,]
    Xtest <<- X[-select,]

    ytrain <<- y[select]
    ytest <<- y[-select]
}

applyfld <- function()
{
    model <- klfda(X, y, dim, metric = "plain")
    print(head(model$Z))
    X <<- data.frame(model$Z)
    print(colnames(X))
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
        xlim <- xlim / 1.1
        zlim <- zlim / 1.1
        rgl.quads( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
                         z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
    
    # Add bounding box decoration
    if(show.bbox)
    {
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

accuracy <- function(x)
{
    return (sum(diag(x)/(sum(rowSums(x)))) * 100)
}

main <- function()
{
    dat_ <- read.csv("Hill_Valley_with_noise_Training.csv")
    times_taken <- rep(0, ncol(dat_)-2)
    accuracies <- rep(0, ncol(dat_)-2)

    for(iter in 3:ncol(dat_)-1){
        for(num in 1:10){
            
            start.time = Sys.time()

            initialize(dat_, iter)

            applyfld()

            train_test_split()
             
            traindata <- data.frame(cbind(Xtrain, ytrain))

            if (dim == 3)
                colnames(traindata) <- c("X1", "X2", "X3", "ytrain")
            
            if (dim == 2)
                colnames(traindata) <- c("X1", "X2", "ytrain")
            
            model_ <- svm(ytrain ~ ., data=traindata, type="C-classification")
            
            if((iter == 2  || iter == 3) && num == 10){

                plotdata(X,y, iter)
                
                plotclassified(model_, traindata)
            } 
            
            pr <- predict(model_, Xtest)
            
            tab <- table(pr,ytest)
            
            accuracies[iter-1] = accuracies[iter-1] + accuracy(tab)

            end.time <- Sys.time()
            times_taken[iter-1] = times_taken[iter-1] + end.time - start.time
            
        }   
    }  

    # times_taken = times_taken/10
    # accuracies = accuracies/10

    # plot(3:ncol(dat_)-1,times_taken, type = "b",
    #  xlab = "Number of dimensions",
    #  ylab = "Average time taken in seconds",
    #  main = "Effect of CoD on time taken in FLD + SVM")

    # plot(3:ncol(dat_)-1,accuracies, type = "b",
    #  xlab = "Number of dimensions",
    #  ylab = "Average accuracy",
    #  main = "Effect of CoD on accuracy in FLD + SVM")

    # grid()              
}

main()
