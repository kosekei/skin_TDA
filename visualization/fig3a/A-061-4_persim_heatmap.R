library(ggplot2)
library(ggExtra)
library(MASS)
library(dplyr)
library(tidyr)
library(TDA)
library(viridis)
library(akima)
library(caret)

df0 = read.table("data/A-061-4_persim_0dim.txt", header=F)
colnames(df0) <- c("lifetime","midlife","density")
df0$density <- log(df0$density)
thr0 <- -10

cutoff0 <- function(x){
  if (x < thr0){
    return(thr0)
  } else {
    return(x)
  }
}
df0$density <- mapply(df0$density, FUN=cutoff0)

df0$midlife <- -156.5 + 8.125 * df0$midlife
df0$lifetime <- 4.45 * (19-df0$lifetime)

g0 <- ggplot(df0, aes(x=midlife, y=lifetime, fill=density)) + xlim(-156.5, 6.0) + ylim(-5,89)+
geom_tile()+
scale_fill_distiller(palette='Spectral')+
labs(title="density of 0-dim features", x="Mid-life", y="Life-time", fill="density") +
theme(panel.border = element_rect(fill = NA, size=1), panel.background = element_rect(fill = NA,color = NA), text=element_text(size=25, family="Arial"))
quartz(type="pdf", file="data/A-061-4_0dim_persim.pdf")
g0
dev.off()

df1 = read.table("data/A-061-4_persim_1dim.txt", header=F)
colnames(df1) <- c("lifetime","midlife","density")
df1$density <- log(df1$density)
thr1 <- -10
cutoff1 <- function(x){
  if (x < thr1){
    return(thr1)
  } else {
    return(x)
  }
}
df1$density <- mapply(df1$density, FUN=cutoff1)
df1$midlife <- -16 + 2.175 * df1$midlife
df1$lifetime <- 1.8 * (19-df1$lifetime)

g1 <- ggplot(df1, aes(x=midlife, y=lifetime, fill=density)) + xlim(-16, 27.5) + ylim(-2,36)+
geom_tile()+
scale_fill_distiller(palette='Spectral')+
labs(title="density of 1-dim features", x="Mid-life", y="Life-time", fill="density") +
theme(panel.border = element_rect(fill = NA, size=1), panel.background = element_rect(fill = NA,color = NA), text=element_text(size=25, family="Arial"))
quartz(type="pdf", file="data/A-061-4_1dim_persim.pdf")
g1
dev.off()
