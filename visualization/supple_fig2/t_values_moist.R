rm(list=ls())

library(tidyr)
library(dplyr)
library(ggrepel)
library(coefplot)

df0 <- read.table("data/RTDA_count.txt",header=T)[,1:19]
df <- data.frame()
for(i in unique(df0$ID)){
  d <- df0[df0$ID==i,]
  ds <- d[d$num==1 | d$num==2 |d$num==3,]
  ds1 <- ds[1,c(1,3,4,5,6,7,8,9)]
  ds2 <- t(as.data.frame(apply(ds[,10:19],MARGIN=2,median)))
  ds3 <- cbind(ds1,ds2)
  dw <- d[d$num==4 | d$num==5 |d$num==6,]
  dw1 <- dw[1,c(1,3,4,5,6,7,8,9)]
  dw2 <- t(as.data.frame(apply(dw[,10:19],MARGIN=2,median)))
  dw3 <- cbind(dw1,dw2)
  df <- rbind(df,ds3,dw3)
}
df <- na.omit(df)

df$temperature <- rep(0, nrow(df))
df$humidity <- rep(0, nrow(df))
df[df$location=="Akita" & df$season=="S",]$temperature <- 25.0
df[df$location=="Akita" & df$season=="S",]$humidity <- 76
df[df$location=="Tokyo" & df$season=="S",]$temperature <- 28.1
df[df$location=="Tokyo" & df$season=="S",]$humidity <- 77
df[df$location=="Akita" & df$season=="W",]$temperature <- 2.7
df[df$location=="Akita" & df$season=="W",]$humidity <- 75
df[df$location=="Tokyo" & df$season=="W",]$temperature <- 8.3
df[df$location=="Tokyo" & df$season=="W",]$humidity <- 61

df2 <- data.frame()

LM <- lm(corneo ~ m0_mean ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("mean of 0-dim mid-life"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(corneo ~ m0_sd ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("standard deviation of 0-dim mid-life"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(corneo ~ l0_mean ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("mean of 0-dim life-time"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(corneo ~ l0_sd ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("standard deviation of 0-dim life-time"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(corneo ~ m1_mean ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("mean of 1-dim mid-life"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(corneo ~ m1_sd ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("standard deviation of 1-dim mid-life"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(corneo ~ l1_mean ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("mean of 1-dim life-time"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(corneo ~ l1_sd ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("standard deviation of 1-dim life-time"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(corneo ~ components ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("number of connected components"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(corneo ~ holes ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("number of holes"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(corneo ~ age ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("age"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(corneo ~ sex ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("sex"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(corneo ~ temperature ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("temperature"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(corneo ~ humidity ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("humidity"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

p_val <- df2$p_value

FDR <- p.adjust(p_val, "BH")
df2$FDR <- FDR

df2$flag <- rep("g", nrow(df2))
df2[df2$FDR < 0.01 & df2$t_value > 0,]$flag <- "r"
df2[df2$FDR < 0.01 & df2$t_value < 0,]$flag <- "b"

df3 <- df2[order(df2$t_value,decreasing=T),]

g <- ggplot(df3, aes(reorder(x = type, X = t_value, FUN = mean), y=t_value, fill=flag)) +
  geom_bar(stat="identity", width = 0.5)+ 
  coord_flip() + 
  labs(x="", y="t value") +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=16, family="Arial"), legend.position = 'none') +
  geom_hline(yintercept = 0) + 
  scale_fill_manual(values = c("blue", "gray", "red"))
quartz(type="pdf", file="data/corneo_coef.pdf")
g
dev.off()


df2 <- data.frame()

LM <- lm(skicon ~ m0_mean ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("mean of 0-dim mid-life"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(skicon ~ m0_sd ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("standard deviation of 0-dim mid-life"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(skicon ~ l0_mean ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("mean of 0-dim life-time"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(skicon ~ l0_sd ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("standard deviation of 0-dim life-time"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(skicon ~ m1_mean ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("mean of 1-dim mid-life"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(skicon ~ m1_sd ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("standard deviation of 1-dim mid-life"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(skicon ~ l1_mean ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("mean of 1-dim life-time"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(skicon ~ l1_sd ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("standard deviation of 1-dim life-time"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(skicon ~ components ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("number of connected components"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(skicon ~ holes ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("number of holes"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(skicon ~ age ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("age"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(skicon ~ sex ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("sex"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(skicon ~ temperature ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("temperature"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(skicon ~ humidity ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("humidity"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

p_val <- df2$p_value

FDR <- p.adjust(p_val, "BH")
df2$FDR <- FDR

df2$flag <- rep("g", nrow(df2))
df2[df2$FDR < 0.01 & df2$t_value > 0,]$flag <- "r"
df2[df2$FDR < 0.01 & df2$t_value < 0,]$flag <- "b"

df3 <- df2[order(df2$t_value,decreasing=T),]

g <- ggplot(df3, aes(reorder(x = type, X = t_value, FUN = mean), y=t_value, fill=flag)) +
  geom_bar(stat="identity", width = 0.5)+ 
  coord_flip() + 
  labs(x="", y="t value") +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=16, family="Arial"), legend.position = 'none') +
  geom_hline(yintercept = 0) + 
  scale_fill_manual(values = c("blue", "gray", "red"))
quartz(type="pdf", file="data/skicon_coef.pdf")
g
dev.off()