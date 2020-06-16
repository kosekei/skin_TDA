library(ggplot2)
library(ggExtra)
library(MASS)
library(dplyr)
library(tidyr)
library(TDA)
library(viridis)
library(akima)
library(caret)

df = read.table("data/variable_importance_grid.txt", header=F)

name <- as.character(df[,1])
d <- strsplit(name,"_")
df1 <- data.frame()
for(i in 1:length(d)){
	row <- d[[i]]
  row2 <- as.data.frame(row)
  df1 <- rbind(df1,t(row2))
}
rownames(df1) <- 1:nrow(df1)
df2 <- cbind(df1,df[,2])
colnames(df2) <- c("dim","midlife","lifetime","importance")
df2$importance <- log(df2$importance)

cutoff <- function(x){
  if (x < -15){
    return(-15)
  } else {
    return(x)
  }
}
df2$importance <- mapply(df2$importance, FUN=cutoff)

df2$midlife <- as.numeric(as.character(df2$midlife))+1
df2$lifetime <- as.numeric(as.character(df2$lifetime))

df_dim0 <- df2[df2["dim"]=="comp",]
for (i in 1:20){
	df_dim0$midlife[df_dim0$midlife==i] <- -156.5 + 8.125 * (i-1)
	df_dim0$lifetime[df_dim0$lifetime==i] <- 4.45 * (i-1)
}
#write.table(df_dim0,file="~/Dropbox/皮膚解析/TDA関連/TDA_2020_4/important_region_dim0.txt",sep="\t",quote=FALSE, row.names = F)

df_dim1 <- df2[df2["dim"]=="hole",]
for (i in 1:20){
	df_dim1$midlife[df_dim1$midlife==i] <- -16 + 2.175 * (i-1)
	df_dim1$lifetime[df_dim1$lifetime==(21-i)] <- 1.8 * (20-i)
}
#write.table(df_dim1,file="~/Dropbox/皮膚解析/TDA関連/TDA_2020_4/important_region_dim1.txt",sep="\t",quote=FALSE, row.names = F)

g0 <- ggplot(df_dim0, aes(x=midlife, y=lifetime, fill=importance)) + xlim(-156.5, 6.0) + ylim(-5,89)+
geom_tile()+
scale_fill_distiller(palette='Spectral', limits = c(-15,-2.6))+
labs(title="Importance of 0-dim features", x="Mid-life", y="Life-time", fill="Importance") +
theme(panel.border = element_rect(fill = NA, size=1), panel.background = element_rect(fill = NA,color = NA), text=element_text(size=25, family="Arial"))
quartz(type="pdf", file="data/0dim_importance.pdf")
g0
dev.off()

g1 <- ggplot(df_dim1, aes(x=midlife, y=lifetime, fill=importance)) + xlim(-16, 27.5) + ylim(-2,36)+
geom_tile()+
scale_fill_distiller(palette='Spectral', limits = c(-15,-2.6))+
labs(title="Importance of 1-dim features", x="Mid-life", y="Life-time", fill="Importance") +
theme(panel.border = element_rect(fill = NA, size=1), panel.background = element_rect(fill = NA,color = NA), text=element_text(size=25, family="Arial"))
quartz(type="pdf", file="data/1dim_importance.pdf")
g1
dev.off()
