#execution: Rscript script_path coordinate_data_path output0 output1
#load libraries
library(TDA)
library(ggplot2)
library(MASS)

#load the coodinate data
args <- commandArgs(trailingOnly = T)
coordinate_data_path <- args[1]
output0 <- args[2]
output1 <- args[3]

X <- read.table(coordinate_data_path, sep=",")
colnames(X) <- c("x","y")

#set a grid over the image and for each grid point calculate the density of white pixels by k Nearest Neibor method
Xlim <- c(0, 1400);
Ylim <- c(0, 1200);
by <- 10;
Xseq <- seq(Xlim[1], Xlim[2], by = by)
Yseq <- seq(Ylim[1], Ylim[2], by = by)
X_knn.Diag <- gridDiag(X = X, FUN = knnDE, k = 10,
                            lim = cbind(Xlim, Ylim),by = by,
                            sublevel = FALSE, library = "Dionysus",
                            printProgress = TRUE, location=TRUE)

#extract 0-dim topological features and calculate life-time and mid-life
D_zero <- as.data.frame(X_knn.Diag$diagram[X_knn.Diag$diagram[,1]==0,])
D_zero[, 2] <- log(D_zero[, 2])
D_zero[, 3] <- log(D_zero[, 3])
D_zero[, 4] <- (D_zero[, 2] + D_zero[, 3])/2 #mid-life
D_zero[, 5] <- abs(D_zero[, 3] - D_zero[, 2]) #life-time
D_zero <- D_zero[,c(2:5)]
colnames(D_zero)<- c("Death","Birth", "midlife","lifetime") 
D_zero$density <- get_density(D_zero$midlife, D_zero$lifetime)
write.table(D_zero, output0, row.names=F,quote=F,append=F,col.names=T, sep="\t")

#extract 1-dim topological features and calculate life-time and mid-life
D_one <- as.data.frame(X_knn.Diag$diagram[X_knn.Diag$diagram[,1]==1,])
D_one[, 2] <- log(D_one[, 2])
D_one[, 3] <- log(D_one[, 3])
D_one[, 4] <- (D_one[, 2] + D_one[, 3])/2
D_one[, 5] <- D_one[, 3] - D_one[, 2]
D_one <- D_one[,c(2:5)]
colnames(D_one)<- c("Death","Birth", "midlife","lifetime") 
D_one$density <- get_density(D_one$midlife, D_one$lifetime)
write.table(D_one, output1 , row.names=F,quote=F,append=F,col.names=T, sep="\t")
