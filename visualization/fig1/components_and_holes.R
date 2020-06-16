rm(list=ls())

library(TDA)
library(ggplot2)
library(ggExtra)
library(MASS)
library(dplyr)
library(tidyr)
library(viridis)
library(akima)

filename <- "data/RTDA_diagrams/D-051-1_coordinate.txt"

X <- read.table(filename, sep=",")
colnames(X) <- c("x","y")
#X$y <- 999-X$y

Xlim <- c(0, 1400);
Ylim <- c(0, 1200);
by <- 10;
Xseq <- seq(Xlim[1], Xlim[2], by = by)
Yseq <- seq(Ylim[1], Ylim[2], by = by)
Grid <- expand.grid(Xseq, Yseq)

k <- 100
kNN <- knnDE(X = X, Grid = Grid, k = k)

#pdf("~/Dropbox/KOSE2019/fig1/D-051-1_3D.pdf")
#persp(Xseq, Yseq,
#       matrix(log(kNN) * 10, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
#       ylab = "", zlab = "", theta = -20, phi = 35, ltheta = 50,
#       col = 2, border = NA, main = "KDE", d = 0.5, scale = FALSE,
#       expand = 3, shade = 0.9)
#dev.off()

#persp(Xseq, Yseq,
#       matrix(log(kNN) * 10, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
#       ylab = "", zlab = "", theta = -20, phi = 35, ltheta = 50,
#       col = 2, border = NA, main = "KDE", d = 0.5, scale = FALSE,
#       expand = 3, shade = 0.9)


df <- cbind(Grid,kNN)
colnames(df)<- c("x","y","z")
df$z <- log(df$z)
x <- c(1:1400)
y <- c(1:1200)
inter_p  <- interp( df$x, df$y, df$z, x, y)
write.table(inter_p$z, "data/D-051-1_interpolated.txt" ,col.names=F,row.names=F,quote=F,sep="\t")

X_knn.Diag <- gridDiag(X = X, FUN = knnDE, k = 100,
                            lim = cbind(Xlim, Ylim),by = by,
                            sublevel = FALSE, library = "Dionysus",
                            printProgress = TRUE, location=TRUE)

D_one <- as.data.frame(X_knn.Diag$diagram[X_knn.Diag$diagram[,1]==1,])
D_one[, 2] <- log(D_one[, 2])
D_one[, 3] <- log(D_one[, 3])
D_one[, 4] <- (D_one[, 2] + D_one[, 3])/2
D_one[, 5] <- D_one[, 3] - D_one[, 2]
#D_one[, 6] <- rep("005",nrow(D_one))
#colnames(D_one)<- c("dimension", "Death","Birth", "midlife","lifetime", "id") 
colnames(D_one)<- c("dimension", "Death","Birth", "midlife","lifetime") 



D_zero <- as.data.frame(X_knn.Diag$diagram[X_knn.Diag$diagram[,1]==0,])
D_zero[, 2] <- log(D_zero[, 2])
D_zero[, 3] <- log(D_zero[, 3])
D_zero[, 4] <- (D_zero[, 2] + D_zero[, 3])/2
D_zero[, 5] <- D_zero[, 3] - D_zero[, 2]
#D_zero[, 6] <- rep("005",nrow(D_zero))
#colnames(D_zero)<- c("dimension", "Death","Birth", "midlife","lifetime", "id") 
colnames(D_zero)<- c("dimension", "Death","Birth", "midlife","lifetime") 


one <- which(X_knn.Diag[["diagram"]][, 1] == 1)
cycle <- X_knn.Diag[["cycleLocation"]]
cycle_one <- cycle[one]

zero <- which(X_knn.Diag[["diagram"]][, 1] == 0)
domain <- X_knn.Diag[["birthLocation"]]
domain_zero <- domain[zero,]


for (thr in c(-10, -14.5, -15, -15.5, -16.5)){
#for (thr in seq(-14,-17,length=7)){
	num1 <- which(thr <= D_one$Birth & D_one$Death <= thr)
	cycle_num <- cycle_one[num1]
	num0 <- which(thr <= D_zero$Birth & D_zero$Death <= thr)
	domain_num <- domain_zero[num0,]
	pdf(paste("data/D-051-1_cycles_", thr , ".pdf", sep=""))
	plot(X, pch = 20, cex = 0.0, xlab = "", ylab = "", main = paste("threshold = ",thr , sep="")) #cex = 0.05
	#plot(Z, pch = 20, cex = 0.05)#cex = 0.05
	for (i in seq(along = cycle_num)) {
		for (j in seq_len(dim(cycle_num[[i]])[1])) {
	 	 lines(cycle_num[[i]][j, , ], pch = 19, lwd=2, col = i)
		}
	}
	dev.off()
	if (is.matrix(domain_num)){
		pdf(paste("data/D-051-1_comps_", thr , ".pdf", sep=""))
			plot(X, pch = 20, cex = 0.0, xlab = "", ylab = "", main = paste("threshold = ",thr , sep="")) #cex = 0.05
			for (i in seq(along = domain_num[,1])){
				points(domain_num[i,1],domain_num[i,2], pch = 17, lwd=2, col = "black")
			}
		dev.off()
	} else {
			pdf(paste("data/D-051-1_comps_", thr , ".pdf", sep=""))
			plot(X, pch = 20, cex = 0.0, xlab = "", ylab = "", main = paste("threshold = ",thr , sep="")) #cex = 0.05
			for (i in seq(along = domain_num[1])){
				points(domain_num[1],domain_num[2], pch = 17, lwd=2, col = "black")
			}
		dev.off()
	}
}

