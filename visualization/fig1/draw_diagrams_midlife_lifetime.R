#load libraries
rm(list=ls())
library(TDA)
library(ggplot2)
library(MASS)
library(gridExtra)


get_density <- function(x, y, n = 100) {
  dens <- MASS::kde2d(x = x, y = y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

X <- read.table(filename, sep=",")
colnames(X) <- c("x","y")

#set a grid over the image and for each grid point calculate the density of white pixels by k Nearest Neibor method
Xlim <- c(0, 1400);
Ylim <- c(0, 1200);
by <- 10;
Xseq <- seq(Xlim[1], Xlim[2], by = by)
Yseq <- seq(Ylim[1], Ylim[2], by = by)
X_knn.Diag <- gridDiag(X = X, FUN = knnDE, k = 100,
                            lim = cbind(Xlim, Ylim),by = by,
                            sublevel = FALSE, library = "Dionysus",
                            printProgress = TRUE, location=TRUE)

#extract 0-dim topological features and calculate life-time and mid-life
D_zero <- as.data.frame(X_knn.Diag$diagram[X_knn.Diag$diagram[,1]==0,])
D_zero[, 2] <- log(D_zero[, 2])
D_zero[, 3] <- log(D_zero[, 3])
D_zero[, 4] <- (D_zero[, 2] + D_zero[, 3])/2 #mid-life
D_zero[, 5] <- (D_zero[, 3] - D_zero[, 2]) #life-time
D_zero <- D_zero[,c(2:5)]
colnames(D_zero)<- c("Death","Birth", "midlife","lifetime") 
D_zero$density <- get_density(D_zero$midlife, D_zero$lifetime)


#extract 1-dim topological features and calculate life-time and mid-life
D_one <- as.data.frame(X_knn.Diag$diagram[X_knn.Diag$diagram[,1]==1,])
D_one[, 2] <- log(D_one[, 2])
D_one[, 3] <- log(D_one[, 3])
D_one[, 4] <- (D_one[, 2] + D_one[, 3])/2
D_one[, 5] <- D_one[, 3] - D_one[, 2]
D_one <- D_one[,c(2:5)]
colnames(D_one)<- c("Death","Birth", "midlife","lifetime") 
D_one$density <- get_density(D_one$midlife, D_one$lifetime)


g0 <- ggplot() + geom_point(data=D_zero , aes(x=midlife, y=lifetime, color = density), size=1) + 
 scale_color_distiller(palette='Spectral', limits = c(0,5)) +
  labs(x="mid-life", y="life-time") +
  xlim(-18, -10) + ylim(0,8) +
  theme(panel.border = element_rect(fill = NA, size=1), 
  	panel.background = element_rect(fill = NA,color = NA), 
  	text=element_text(size=16, family="Arial"))


g1 <- ggplot() + geom_point(data=D_one , aes(x=midlife, y=lifetime, color = density), size=1) + 
 scale_color_distiller(palette='Spectral', limits = c(0,5)) +
  xlim(-17.5, -12) + ylim(0,3) +
  labs(x="mid-life", y="life-time") +
  theme(panel.border = element_rect(fill = NA, size=1), 
  	panel.background = element_rect(fill = NA,color = NA), 
  	text=element_text(size=16, family="Arial"))


quartz(type="pdf", file=output_0dim, sep=""))
g0
dev.off()

quartz(type="pdf", file=output_1dim, sep=""))
g1
dev.off()