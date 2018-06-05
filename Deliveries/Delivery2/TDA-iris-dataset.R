#Daniel Salgado Rojo

data(iris)
head(iris)

# Consider only the subset of the FIRST THREE FEATURES, OMMIT THE SECOND ONE
data_iris = iris[, c(1,3,4)]
head(data_iris)
colnames(data_iris) <- c("Sepal length", "Petal length", "Petal width")
head(data_iris)

# Visualize the subseted dataset
pairs(data_iris, main = "Famous iris (sub)dateset from Fisher, 1936", pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])


# Simpify variable names
colnames(data_iris) <- c("x1", "x2", "x3")
X = data_iris


library(rgl)

# Plot both data
par(mfrow=c(1,2))
plot3d(X,xlab="Sepal length",ylab="Petal length", zlab = "Petal width",main="Iris")

summary(X)

# Define some parameters
Xlim <- c(4.3,7.9)
Ylim <- c(1.0, 6.9)
Zlim <- c(0.1,2.5)

by <- 0.05
Xseq <- seq(from = Xlim[1], to= Xlim[2], by = by)
Yseq <- seq(from = Ylim[1], to= Ylim[2], by = by)
Zseq <- seq(from = Zlim[1], to= Zlim[2], by = by)


Grid <- expand.grid(Xseq,Yseq,Zseq)

# Persitent homology on (x2,x3) with KDE function and VR
par(mfrow = c(1,3))
plot(X[, 2:3],xlab="Petal length",ylab="Petal width",main="Sample")
DiagKDE <- gridDiag(X=X[, 2:3], 
                    FUN = kde, h=0.3, sublevel=FALSE,
                    lim= cbind(Ylim,Zlim), by=by,
                    library="Dionysus",printProgress = FALSE)
plot(x = DiagKDE[["diagram"]],main="KDE Diagram")
DiagVR <- ripsDiag(X=X[, 2:3], 
                   maxdimension = 1, maxscale = 5, dist = 'euclidean',
                   library="GUDHI",printProgress = FALSE)
plot(x = DiagVR[["diagram"]],main="Vietoris-Rips Diagram")

# Persitent homology on (x1,x3) with KDE function and VR
par(mfrow = c(1,3))
plot(X[, c(1,3)],xlab="Sepal length",ylab="Petal width",main="Sample")
DiagKDE <- gridDiag(X=X[, c(1,3)], 
                    FUN = kde, h=0.3, sublevel=FALSE,
                    lim= cbind(Xlim,Zlim), by=by,
                    library="Dionysus",printProgress = FALSE)
plot(x = DiagKDE[["diagram"]],main="KDE Diagram")
DiagVR <- ripsDiag(X=X[, c(1,3)], 
                   maxdimension = 1, maxscale = 5, dist = 'euclidean',
                   library="GUDHI",printProgress = FALSE)
plot(x = DiagVR[["diagram"]],main="Vietoris-Rips Diagram")


# Persitent homology on (x1,x2) with KDE function and VR
par(mfrow = c(1,3))
plot(X[, c(1,2)],xlab="Sepal length",ylab="Petal length",main="Sample")
DiagKDE <- gridDiag(X=X[, c(1,2)], 
                    FUN = kde, h=0.3, sublevel=FALSE,
                    lim= cbind(Xlim,Ylim), by=by,
                    library="Dionysus",printProgress = FALSE)
plot(x = DiagKDE[["diagram"]],main="KDE Diagram")
DiagVR <- ripsDiag(X=X[, c(1,2)], 
                   maxdimension = 1, maxscale = 5, dist = 'euclidean',
                   library="GUDHI",printProgress = FALSE)
plot(x = DiagVR[["diagram"]],main="Vietoris-Rips Diagram")
