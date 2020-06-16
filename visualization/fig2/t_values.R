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

df2 <- data.frame()

LM <- lm(TEWL ~ m0_mean ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("mean of 0-dim mid-life"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(TEWL ~ m0_sd ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("standard deviation of 0-dim mid-life"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(TEWL ~ l0_mean ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("mean of 0-dim life-time"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(TEWL ~ l0_sd ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("standard deviation of 0-dim life-time"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(TEWL ~ m1_mean ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("mean of 1-dim mid-life"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(TEWL ~ m1_sd ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("standard deviation of 1-dim mid-life"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(TEWL ~ l1_mean ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("mean of 1-dim life-time"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(TEWL ~ l1_sd ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("standard deviation of 1-dim life-time"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(TEWL ~ components ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("number of connected components"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(TEWL ~ holes ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("number of holes"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(TEWL ~ age ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("age"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(TEWL ~ sex ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("sex"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(TEWL ~ temperature ,data=df)
t <- summary(LM)$coefficients[2,3]
p <- summary(LM)$coefficients[2,4]
d <- data.frame(type=c("temperature"), t_value = c(t), p_value=c(p))
df2 <- rbind(df2,d)

LM <- lm(TEWL ~ humidity ,data=df)
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
quartz(type="pdf", file="data/t_values.pdf")
g
dev.off()










TEWL_lm1 <-  glmnet(TEWL ~ s0_mean + s0_sd + p0_mean + p0_sd + s1_mean + s1_sd + p1_mean + p1_sd + age + num_comp+num_holes, data = df)


LM <- lm(TEWL ~ small_circles ,data=df)
summary(LM)
R2 <- summary(LM)$r.squared
MAE <- sum(abs(LM$residuals))/length(LM$residuals)
RMSE <- sqrt(c(crossprod(LM$residuals)) / length(LM$residuals))
d <- data.frame(type=c("holes"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))



LM <- lm(TEWL ~ medium_circles ,data=df)
summary(LM)
R2 <- summary(LM)$r.squared
MAE <- sum(abs(LM$residuals))/length(LM$residuals)
RMSE <- sqrt(c(crossprod(LM$residuals)) / length(LM$residuals))
d <- data.frame(type=c("holes"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))

LM <- lm(TEWL ~ large_circles ,data=df)
summary(LM)
R2 <- summary(LM)$r.squared
MAE <- sum(abs(LM$residuals))/length(LM$residuals)
RMSE <- sqrt(c(crossprod(LM$residuals)) / length(LM$residuals))
d <- data.frame(type=c("holes"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))

g <- ggplot() + 
  geom_point(data=df , aes(x=small_circles, y=TEWL, color=C_or_A, shape=sex), size=4) +
  geom_smooth(data=df , aes(x=small_circles, y=TEWL), method = "lm") +
  labs(x="Number of small holes", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child"), guide=FALSE ) +
#  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=FALSE ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=25, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_small_holes.pdf")
g
dev.off()

g <- ggplot() + 
  geom_point(data=df , aes(x=medium_circles, y=TEWL, color=C_or_A, shape=sex), size=4) +
  geom_smooth(data=df , aes(x=medium_circles, y=TEWL), method = "lm") +
  labs(x="Number of medium holes", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child"), guide=FALSE ) +
#  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=FALSE ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=25, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_medium_holes.pdf")
g
dev.off()


g <- ggplot() + 
  geom_point(data=df , aes(x=large_circles, y=TEWL, color=C_or_A, shape=sex), size=4) +
  geom_smooth(data=df , aes(x=large_circles, y=TEWL), method = "lm") +
  labs(x="Number of large holes", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child"), guide=FALSE ) +
#  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=FALSE ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=25, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_large_holes.pdf")
g
dev.off()


g <- ggplot() + 
  geom_point(data=df , aes(x=age, y=TEWL, color=C_or_A, shape=sex), size=4) +
  geom_smooth(data=df , aes(x=age, y=TEWL), method = "lm") +
  labs(x="Age", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child"), guide=FALSE ) +
#  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=FALSE ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=25, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_age.pdf")
g
dev.off()

g <- ggplot() + 
  geom_point(data=df , aes(x=num_holes, y=TEWL, color=C_or_A, shape=sex), size=4) +
  geom_smooth(data=df , aes(x=num_holes, y=TEWL), method = "lm") +
  labs(x="Number of holes", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child"), guide=FALSE ) +
#  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=FALSE ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=25, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_holes.pdf")
g
dev.off()

g <- ggplot() + 
  geom_point(data=df , aes(x=num_comp, y=TEWL, color=C_or_A, shape=sex), size=4) +
  geom_smooth(data=df , aes(x=num_comp, y=TEWL), method = "lm") +
  labs(x="Number of connected components", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child"), guide=FALSE ) +
#  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=FALSE ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=25, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_comp.pdf")
g
dev.off()









g <- ggplot() + 
  geom_point(data=df , aes(x=s1_mean, y=TEWL, color=C_or_A, shape=sex), size=4) +
  geom_smooth(data=df , aes(x=s1_mean, y=TEWL), method = "lm") +
  labs(x="Mean of (Birth + Death) of holes", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child"), guide=FALSE ) +
#  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=FALSE ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=25, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_s1_mean.pdf")
g
dev.off()


g <- ggplot() + 
  geom_point(data=df , aes(x=s1_sd, y=TEWL, color=C_or_A, shape=sex), size=4) +
  geom_smooth(data=df , aes(x=s1_sd, y=TEWL), method = "lm") +
  labs(x="Standard deviation of (Birth + Death) of holes", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child"), guide=FALSE ) +
#  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=FALSE ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=25, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_s1_sd.pdf")
g
dev.off()






ggplot(df1, aes(x=sum, y=TEWL)) + geom_point() + geom_smooth(method = "lm")

R2 <- summary(LM)$r.squared
MAE <- sum(abs(LM$residuals))/length(LM$residuals)
RMSE <- sqrt(c(crossprod(LM$residuals)) / length(LM$residuals))
d <- data.frame(type=c("holes"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))

g <- ggplot() + 
  geom_point(data=df , aes(x=num_holes, y=TEWL, color=C_or_A, shape=sex), size=3) +
  geom_smooth(data=df , aes(x=num_holes, y=TEWL), method = "lm") +
  labs(x="num_holes", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child") ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=16, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_holes.pdf")
g
dev.off()

g <- ggplot() + 
  geom_point(data=df , aes(x=num_comp, y=TEWL, color=C_or_A, shape=sex), size=3) +
  geom_smooth(data=df , aes(x=num_comp, y=TEWL), method = "lm") +
  labs(x="num_comp", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child") ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=16, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_components.pdf")
g
dev.off()


"""
cutoff <- function(x){
	if (x > 30){
		return(30)
	} else {
		return(x)
	}
}

df$TEWL <- sapply(df$TEWL, cutoff)
"""


TEWL_lm1 <-  lm(TEWL ~ s0_mean + s0_sd + p0_mean + p0_sd + s1_mean + s1_sd + p1_mean + p1_sd + age + num_comp+num_holes, data = df)
#coefs = as.data.frame(summary(TEWL_lm1)$coefficients[-1,1:2])
#names(coefs)[2] = "se" 
#coefs$vars = rownames(coefs)
#g <- ggplot(coefs, aes(vars, Estimate)) + 
#  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
#  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se), 
#                lwd=1, colour="red", width=0) +
#  geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se), 
#                lwd=2.5, colour="blue", width=0) +
#  geom_point(size=4, pch=21, fill="yellow") +
#  theme(panel.border = element_rect(fill = NA, size=1), 
#    panel.background = element_rect(fill = NA,color = NA), 
#    text=element_text(size=10, family="Arial"))
#quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig4/coefplot.pdf")
#g
#dev.off()

pdf("/Users/keitakoseki/Dropbox/TDA論文/fig/fig4/coefplot2.pdf")
coefplot(TEWL_lm1,intercept=F) + theme_classic()
dev.off()


summary(TEWL_lm1)
pdf("~/Dropbox/TDA論文/fig/fig4/coefplot.pdf")
coefplot(TEWL_lm1,intercept=F, title = "Coefficient Plot")+ theme_classic()
dev.off()

df2 <- data.frame()

LM <- lm(TEWL ~ s0_mean ,data=df)
R2 <- summary(LM)$r.squared
MAE <- sum(abs(LM$residuals))/length(LM$residuals)
RMSE <- sqrt(c(crossprod(LM$residuals)) / length(LM$residuals))
d <- data.frame(type=c("s0_mean"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)



LM2 <- lm(TEWL ~ s0_sd ,data=df)
R2 <- summary(LM2)$r.squared
MAE <- sum(abs(LM2$residuals))/length(LM2$residuals)
RMSE <- sqrt(c(crossprod(LM2$residuals)) / length(LM2$residuals))
d <- data.frame(type=c("s0_sd"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)



LM3 <- lm(TEWL ~ s0_skew ,data=df)
R2 <- summary(LM3)$r.squared
MAE <- sum(abs(LM3$residuals))/length(LM3$residuals)
RMSE <- sqrt(c(crossprod(LM3$residuals)) / length(LM3$residuals))
d <- data.frame(type=c("s0_skew"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

LM4 <- lm(TEWL ~ s0_kurt ,data=df)
R2 <- summary(LM4)$r.squared
MAE <- sum(abs(LM4$residuals))/length(LM4$residuals)
RMSE <- sqrt(c(crossprod(LM4$residuals)) / length(LM4$residuals))
d <- data.frame(type=c("s0_kurt"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

LM5 <- lm(TEWL ~ p0_mean ,data=df)
R2 <- summary(LM5)$r.squared
MAE <- sum(abs(LM5$residuals))/length(LM5$residuals)
RMSE <- sqrt(c(crossprod(LM5$residuals)) / length(LM5$residuals))
d <- data.frame(type=c("p0_mean"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

LM6 <- lm(TEWL ~ p0_sd ,data=df)
R2 <- summary(LM6)$r.squared
MAE <- sum(abs(LM6$residuals))/length(LM6$residuals)
RMSE <- sqrt(c(crossprod(LM6$residuals)) / length(LM6$residuals))
d <- data.frame(type=c("p0_sd"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

LM7 <- lm(TEWL ~ p0_skew ,data=df)
R2 <- summary(LM7)$r.squared
MAE <- sum(abs(LM7$residuals))/length(LM7$residuals)
RMSE <- sqrt(c(crossprod(LM7$residuals)) / length(LM7$residuals))
d <- data.frame(type=c("p0_skew"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

LM8 <- lm(TEWL ~ p0_kurt ,data=df)
R2 <- summary(LM8)$r.squared
MAE <- sum(abs(LM8$residuals))/length(LM8$residuals)
RMSE <- sqrt(c(crossprod(LM8$residuals)) / length(LM8$residuals))
d <- data.frame(type=c("p0_kurt"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

LM9 <- lm(TEWL ~ s1_mean ,data=df)
R2 <- summary(LM9)$r.squared
MAE <- sum(abs(LM9$residuals))/length(LM9$residuals)
RMSE <- sqrt(c(crossprod(LM9$residuals)) / length(LM9$residuals))
d <- data.frame(type=c("s1_mean"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

LM10 <- lm(TEWL ~ s1_sd ,data=df)
R2 <- summary(LM10)$r.squared
MAE <- sum(abs(LM10$residuals))/length(LM10$residuals)
RMSE <- sqrt(c(crossprod(LM10$residuals)) / length(LM10$residuals))
d <- data.frame(type=c("s1_sd"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

LM11 <- lm(TEWL ~ s1_skew ,data=df)
R2 <- summary(LM11)$r.squared
MAE <- sum(abs(LM11$residuals))/length(LM11$residuals)
RMSE <- sqrt(c(crossprod(LM11$residuals)) / length(LM11$residuals))
d <- data.frame(type=c("s1_skew"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

LM12 <- lm(TEWL ~ s1_kurt ,data=df)
R2 <- summary(LM12)$r.squared
MAE <- sum(abs(LM12$residuals))/length(LM12$residuals)
RMSE <- sqrt(c(crossprod(LM12$residuals)) / length(LM12$residuals))
d <- data.frame(type=c("s1_kurt"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

LM13 <- lm(TEWL ~ p1_mean ,data=df)
R2 <- summary(LM13)$r.squared
MAE <- sum(abs(LM13$residuals))/length(LM13$residuals)
RMSE <- sqrt(c(crossprod(LM13$residuals)) / length(LM13$residuals))
d <- data.frame(type=c("p1_mean"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

LM14 <- lm(TEWL ~ p1_sd ,data=df)
R2 <- summary(LM14)$r.squared
MAE <- sum(abs(LM14$residuals))/length(LM14$residuals)
RMSE <- sqrt(c(crossprod(LM14$residuals)) / length(LM14$residuals))
d <- data.frame(type=c("p1_sd"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

LM15 <- lm(TEWL ~ p1_skew ,data=df)
R2 <- summary(LM15)$r.squared
MAE <- sum(abs(LM15$residuals))/length(LM15$residuals)
RMSE <- sqrt(c(crossprod(LM15$residuals)) / length(LM15$residuals))
d <- data.frame(type=c("p1_skew"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

LM16 <- lm(TEWL ~ p1_kurt ,data=df)
R2 <- summary(LM16)$r.squared
MAE <- sum(abs(LM16$residuals))/length(LM16$residuals)
RMSE <- sqrt(c(crossprod(LM16$residuals)) / length(LM16$residuals))
d <- data.frame(type=c("p1_kurt"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

LM17 <- lm(TEWL ~ age ,data=df)
R2 <- summary(LM17)$r.squared
MAE <- sum(abs(LM17$residuals))/length(LM17$residuals)
RMSE <- sqrt(c(crossprod(LM17$residuals)) / length(LM17$residuals))
d <- data.frame(type=c("age"), R2 = c(R2), MAE=c(MAE), RMSE=c(RMSE))
df2 <- rbind(df2,d)

write.table(df2, "~/Dropbox/KOSE201809/lm_eval.txt" ,col.names=F,row.names=F,quote=F,sep="\t")


g <- ggplot() + 
  geom_point(data=df , aes(x=s1_mean, y=TEWL, color=C_or_A, shape=sex), size=3) +
  geom_smooth(data=df , aes(x=s1_mean, y=TEWL), method = "lm") +
  labs(x="Mean of (Birth + Death)", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child") ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
  	panel.background = element_rect(fill = NA,color = NA), 
  	text=element_text(size=16, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_s1_mean.pdf")
g
dev.off()

g <- ggplot() + 
  geom_point(data=df , aes(x=s1_sd, y=TEWL, color=C_or_A, shape=sex), size=3) +
  geom_smooth(data=df , aes(x=s1_sd, y=TEWL), method = "lm") +
  labs(x="s1_sd", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child") ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
  	panel.background = element_rect(fill = NA,color = NA), 
  	text=element_text(size=16, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_s1_sd.pdf")
g
dev.off()

g <- ggplot() + 
  geom_point(data=df , aes(x=p1_mean, y=TEWL, color=C_or_A, shape=sex), size=3) +
  geom_smooth(data=df , aes(x=p1_mean, y=TEWL), method = "lm") +
  labs(x="Mean of (Birth - Death)", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child") ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
  	panel.background = element_rect(fill = NA,color = NA), 
  	text=element_text(size=16, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_p1_mean.pdf")
g
dev.off()

g <- ggplot() + 
  geom_point(data=df , aes(x=p1_sd, y=TEWL, color=C_or_A, shape=sex), size=3) +
  geom_smooth(data=df , aes(x=p1_sd, y=TEWL), method = "lm") +
  labs(x="Standard Deviation of (Birth - Death)", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child") ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
  	panel.background = element_rect(fill = NA,color = NA), 
  	text=element_text(size=16, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_p1_sd.pdf")
g
dev.off()


g <- ggplot() + 
  geom_point(data=df , aes(x=age, y=TEWL, color=C_or_A, shape=sex), size=3) +
  geom_smooth(data=df , aes(x=age, y=TEWL), method = "lm") +
  labs(x="Age", y="TEWL", color="Adult/Child", shape="Sex") +
  scale_color_hue(name = "Adult/Child", labels = c(A = "Adult", C ="Child") ) +
  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
  theme(panel.border = element_rect(fill = NA, size=1), 
  	panel.background = element_rect(fill = NA,color = NA), 
  	text=element_text(size=16, family="Arial"))
quartz(type="pdf", file="~/Dropbox/TDA論文/fig/fig2,3/lm_age.pdf")
g
dev.off()






