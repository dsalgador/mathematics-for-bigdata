### Exercise: repeat above analysis with a 2dimensional sphere:
library("TDA")

library(rgl)

set.seed(42)
###
# Tangent spheres
###
X <- sphereUnif(1000, 2, r = 1)

d = 2.0
displacement = data.frame(rep(0,1000),rep(d,1000),rep(0,1000))
X2 = X + displacement
colnames(X2) <- c("x1", "x2", "x3")


xrand <- runif(50,min=-1.3,max=1.3); yrand <- runif(50,min=-1.3,max=1.3+d)
zrand <- runif(50,min=-1.3,max=1.3)
Y = data.frame(x1=xrand,x2=yrand, x3 = zrand)
X <- as.data.frame(X)
colnames(X) <- c("x1", "x2", "x3")

X = rbind(X,X2)
colnames(X) <- colnames(Y)
#Xnoise <- rbind(as.data.frame(X),Y)

# Plot both data
par(mfrow=c(1,2))
plot3d(X,xlab="x-axis",ylab="y-axis", zlab="z-axis",main="Sample")
#plot3d(Xnoise,xlab="",ylab="",main="Sample with noise")

# Define some parameters
Xlim <- c(-1.6,1.6)
Ylim <- c(-1.6,1.6+d)
Zlim <- c(-1.6,1.6)

by <- 0.065
Xseq <- seq(from = Xlim[1], to= Xlim[2], by = by)
Yseq <- seq(from = Ylim[1], to= Ylim[2], by = by)
Zseq <- seq(from = Zlim[1], to= Zlim[2], by = by)

Grid <- expand.grid(Xseq,Yseq,Zseq)


# Persitent homology on X with KDE function and VR
par(mfrow = c(1,2))
plot(X[, 1:2],xlab="",ylab="",main="Sample")
DiagKDE <- gridDiag(X=X, 
                    FUN = kde, h=0.2, sublevel=FALSE,
                    lim= cbind(Xlim,Ylim,Zlim), by=by,
                    library="Dionysus",printProgress = FALSE)
plot(x = DiagKDE[["diagram"]],main="KDE Diagram")
# DiagVR <- ripsDiag(X=X, 
#                    maxdimension = 1, maxscale = 5, dist = 'euclidean',
#                    library="GUDHI",printProgress = FALSE)
# plot(x = DiagVR[["diagram"]],main="Vietoris-Rips Diagram")


###
# Overllapping spheres
###
set.seed(44)


X <- sphereUnif(1000, 2, r = 1)

d = 1.0
displacement = data.frame(rep(0,1000),rep(d,1000),rep(0,1000))
X2 = X + displacement
colnames(X2) <- c("x1", "x2", "x3")


xrand <- runif(50,min=-1.3,max=1.3); yrand <- runif(50,min=-1.3,max=1.3+d)
zrand <- runif(50,min=-1.3,max=1.3)
Y = data.frame(x1=xrand,x2=yrand, x3 = zrand)
X <- as.data.frame(X)
colnames(X) <- c("x1", "x2", "x3")

X = rbind(X,X2)
colnames(X) <- colnames(Y)
#Xnoise <- rbind(as.data.frame(X),Y)

# Plot both data
par(mfrow=c(1,2))
plot3d(X,xlab="x-axis",ylab="y-axis",zlab="z-axis",main="Sample")
#plot3d(Xnoise,xlab="",ylab="",main="Sample with noise")

# Define some parameters
Xlim <- c(-1.6,1.6)
Ylim <- c(-1.6,1.6+d)
Zlim <- c(-1.6,1.6)

by <- 0.065
Xseq <- seq(from = Xlim[1], to= Xlim[2], by = by)
Yseq <- seq(from = Ylim[1], to= Ylim[2], by = by)
Zseq <- seq(from = Zlim[1], to= Zlim[2], by = by)

Grid <- expand.grid(Xseq,Yseq,Zseq)


# Persitent homology on X with KDE function and VR
par(mfrow = c(1,2))
plot(X[, 1:2],xlab="",ylab="",main="Sample")
DiagKDE <- gridDiag(X=X, 
                    FUN = kde, h=0.2, sublevel=FALSE,
                    lim= cbind(Xlim,Ylim,Zlim), by=by,
                    library="Dionysus",printProgress = FALSE)
plot(x = DiagKDE[["diagram"]],main="KDE Diagram")
# DiagVR <- ripsDiag(X=X, 
#                    maxdimension = 1, maxscale = 5, dist = 'euclidean',
#                    library="GUDHI",printProgress = FALSE)
# plot(x = DiagVR[["diagram"]],main="Vietoris-Rips Diagram")