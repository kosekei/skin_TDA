rm(list=ls())

library(TDA)
library(ggplot2)
library(ggExtra)
library(MASS)
library(dplyr)
library(tidyr)
library(viridis)
library(akima)
library(gridExtra)

get_density <- function(x, y, h, n = 100) {
  dens <- MASS::kde2d(x = x, y = y, h=h, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

filename0 <- "data/homcloud_binary_intermediate/A-061-4_binary_diag0.txt"
filename1 <- "data/homcloud_binary_intermediate/A-061-4_binary_diag1.txt"

D_zero <- read.table(filename0, sep="\t",header=T)
D_zero$density <- get_density(D_zero$midlife, D_zero$lifetime)
D_zero$density <- log(D_zero$density)


D_one <- read.table(filename1, sep="\t",header=T)
D_one$density <- get_density(D_one$midlife, D_one$lifetime)
D_one$density <- log(D_one$density)


g0 <- ggplot() + geom_point(data=D_zero , aes(x=midlife, y=lifetime, color = density), size=4) +  xlim(-156.5, 6.0) + ylim(-5,89)+
 scale_color_distiller(palette='Spectral') +
  labs(x="midlife", y="lifetime") +
  theme(panel.border = element_rect(fill = NA, size=1), 
  	panel.background = element_rect(fill = NA,color = NA), 
  	text=element_text(size=16, family="Arial"))

quartz(type="pdf", file="data/A-061-4_binary_perdiag0.pdf")
g0
dev.off()

g1 <- ggplot() + geom_point(data=D_one , aes(x=midlife, y=lifetime, color = density), size=4) + xlim(-16, 27.5) + ylim(-2,36)+
 scale_color_distiller(palette='Spectral') +
  labs(x="midlife", y="lifetime") +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=16, family="Arial"))

quartz(type="pdf", file="data/A-061-4_binary_perdiag1.pdf")
g1
dev.off()
