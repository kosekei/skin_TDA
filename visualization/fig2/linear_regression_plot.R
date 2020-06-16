library(ggplot2)

df <- read.table("data/RTDA_count.txt",header=T)

df <- df[,c(1:19)]
df <- df[,c(-8,-9)]

d0 <- data.frame()
for (ID in unique(df$ID)){
  d <- df[df$ID==ID,]
  s <- d[d$season=="S",]
  ss <- as.data.frame(c(s[1,c(1:7)] , apply(s[,8:17], MARGIN=2, mean)))
  w <- d[d$season=="W",] 
  ww <- as.data.frame(c(w[1,c(1:7)] , apply(w[,8:17], MARGIN=2, mean)))
  d0 <- rbind(d0,ss,ww)
}
d0 <- na.omit(d0)

dA <- d0[d0$ID=="D-051" & d0$season=="S",]
dB <- d0[d0$ID=="D-055" & d0$season=="S",]
dC <- d0[d0$ID=="D-110" & d0$season=="S",]
dD <- d0[d0$ID=="D-096" & d0$season=="W",]
dE <- d0[d0$ID=="D-022" & d0$season=="S",]
dF <- d0[d0$ID=="A-076" & d0$season=="W",]

d1 <- rbind(dA, dB, dC, dD, dE, dF)
d1$sym <- c("A", "B", "C", "D", "E", "F")

d2 <- d0[c(-357, -365,-471,-444,-300,-150), ]

g <- ggplot() +
  geom_point(data=d2 , aes(x=m1_mean, y=TEWL, color=sex), size=2) +
  geom_smooth(data=d0 , aes(x=m1_mean, y=TEWL), method = "lm") +
  labs(x="Mean of mid-life of holes", y="TEWL") +
   scale_color_hue(name = "sex", labels = c(F = "F", M ="M")) +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=25, family="Arial"))
g2 <- g + geom_text(data=d1 , aes(x=m1_mean, y=TEWL, label=sym), size=6)

quartz(type="pdf", file="data/m1_mean_TEWL_0702.pdf")
g2
dev.off()


g <- ggplot() +
  geom_point(data=d2 , aes(x=m1_sd, y=TEWL, color=sex), size=2) +
  geom_smooth(data=d2 , aes(x=m1_sd, y=TEWL), method = "lm") +
  labs(x="Standard deviation of mid-life of holes", y="TEWL") +
   scale_color_hue(name = "sex", labels = c(F = "F", M ="M")) +
#  scale_shape_discrete(name = "Adult/Child", labels = c(A = "Adult", C ="Child")) +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=25, family="Arial"))
g2 <- g + geom_text(data=d1 , aes(x=m1_sd, y=TEWL, label=sym), size=6)
quartz(type="pdf", file="data/m1_sd_TEWL.pdf")
g2
dev.off()

cutoff <- function(x){
  if (x > 30){
    return(30)
  } else {
    return(x)
  }
}

d0$TE2 <- mapply(d0$TEWL, FUN=cutoff)
d1$TE2 <- mapply(d1$TEWL, FUN=cutoff)
d2$TE2 <- mapply(d2$TEWL, FUN=cutoff)

g <- ggplot() +
  geom_point(data=d2 , aes(x=m1_mean, y=m1_sd, color=TE2), size=2) +
  geom_smooth(data=d2 , aes(x=m1_mean, y=m1_sd), method = "lm") +
  labs(x="Mean of mid-life of holes", y="Standard deviation of mid-life of holes") +
 scale_color_distiller(palette='Spectral', limits=c(5,30)) +
#  scale_shape_discrete(name = "Adult/Child", labels = c(A = "Adult", C ="Child")) +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=25, family="Arial"))
g2 <- g + geom_text(data=d1 , aes(x=m1_mean, y=m1_sd, label=sym), size=6)
quartz(type="pdf", file="data/m1_mean_sd_TEWLcolor.pdf")
g2
dev.off()