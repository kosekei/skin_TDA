rm(list=ls())
library(TDA)
library(ggplot2)
library(ggExtra)
library(MASS)
library(dplyr)
library(tidyr)
library(viridis)
library(akima)

"D-051-1"
filename <- "data/RTDA_diagrams/D-051-1_coordinate.txt"
X <- read.table(filename, sep=",")

colnames(X) <- c("x","y")

Xlim <- c(0, 1400);
Ylim <- c(0, 1200);
by <- 10;

X_knn.Diag <- gridDiag(X = X, FUN = knnDE, k = 100,
                            lim = cbind(Xlim, Ylim),by = by,
                            sublevel = FALSE, library = "Dionysus",
                            printProgress = TRUE, location=TRUE)

D_zero1 <- as.data.frame(X_knn.Diag$diagram[X_knn.Diag$diagram[,1]==0,])
D_zero1[, 2] <- log(D_zero1[, 2])
D_zero1[, 3] <- log(D_zero1[, 3])
D_zero1[, 4] <- (D_zero1[, 2] + D_zero1[, 3])/2
D_zero1[, 5] <- D_zero1[, 3] - D_zero1[, 2]
D_zero1[, 6] <- rep("D-055-1",nrow(D_zero1))
colnames(D_zero1)<- c("dimension", "Death","Birth", "midlife","lifetime", "ID") 

D_one1 <- as.data.frame(X_knn.Diag$diagram[X_knn.Diag$diagram[,1]==1,])
D_one1[, 2] <- log(D_one1[, 2])
D_one1[, 3] <- log(D_one1[, 3])
D_one1[, 4] <- (D_one1[, 2] + D_one1[, 3])/2
D_one1[, 5] <- D_one1[, 3] - D_one1[, 2]
D_one1[, 6] <- rep("D-055-1",nrow(D_one1))
colnames(D_one1)<- c("dimension", "Death","Birth", "midlife","lifetime", "ID") 

"D-022-1"
filename2 <- "data/RTDA_diagrams/D-022-1_coordinate.txt"
Y <- read.table(filename2, sep=",")

Y_knn.Diag <- gridDiag(X = Y, FUN = knnDE, k = 100,
                            lim = cbind(Xlim, Ylim),by = by,
                            sublevel = FALSE, library = "Dionysus",
                            printProgress = TRUE, location=TRUE)

D_zero2 <- as.data.frame(Y_knn.Diag$diagram[Y_knn.Diag$diagram[,1]==0,])
D_zero2[, 2] <- log(D_zero2[, 2])
D_zero2[, 3] <- log(D_zero2[, 3])
D_zero2[, 4] <- (D_zero2[, 2] + D_zero2[, 3])/2
D_zero2[, 5] <- D_zero2[, 3] - D_zero2[, 2]
D_zero2[, 6] <- rep("A-076-4",nrow(D_zero2))
colnames(D_zero2)<- c("dimension", "Death","Birth", "midlife","lifetime", "ID") 
          
D_one2 <- as.data.frame(Y_knn.Diag$diagram[Y_knn.Diag$diagram[,1]==1,])
D_one2[, 2] <- log(D_one2[, 2])
D_one2[, 3] <- log(D_one2[, 3])
D_one2[, 4] <- (D_one2[, 2] + D_one2[, 3])/2
D_one2[, 5] <- D_one2[, 3] - D_one2[, 2]
D_one2[, 6] <- rep("A-076-4",nrow(D_one2))
colnames(D_one2)<- c("dimension", "Death","Birth", "midlife","lifetime", "ID") 

#D_one2$density <- get_density(D_one2$midlife, D_one2$lifetime)


"merge"
D_one3 <- rbind( D_one2,D_one1)
g <- ggplot() + geom_point(data=D_one3 , aes(x=midlife, y=lifetime, color = ID), size=1) + 
#  xlim(-17.5, -12) + ylim(0,3) +
  labs(x="Mid-life", y="Life-time") +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=16, family="Arial"),
    legend.position = 'none')

g2 <- ggMarginal(
  g,
  type = "density",
  margins = "both",
  size = 5,
  groupColour = TRUE,
  groupFill = TRUE
)

quartz(type="pdf", file="data/merged_diagrams_1dim.pdf")
g2
dev.off()


D_zero3 <- rbind( D_zero2,D_zero1)
g <- ggplot() + geom_point(data=D_zero3 , aes(x=midlife, y=lifetime, color = ID), size=1) + 
#  xlim(-17.5, -12) + ylim(0,3) +
  labs(x="Mid-life", y="Life-time") +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=16, family="Arial"),
    legend.position = 'none')

g2 <- ggMarginal(
  g,
  type = "density",
  margins = "both",
  size = 5,
  groupColour = TRUE,
  groupFill = TRUE
)

quartz(type="pdf", file="data/merged_diagrams_0dim.pdf")
g2
dev.off()