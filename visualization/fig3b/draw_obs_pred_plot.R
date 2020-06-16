rm(list=ls())

library(caret)
library(ggplot2)
library(dplyr)

obs_pred4 <- read.table("data/obs_pred_binary.txt",header=T)
obs_pred4["sex"] <- lapply(obs_pred4["sex"], gsub, pattern="0", replacement = "M")
obs_pred4["sex"] <- lapply(obs_pred4["sex"], gsub, pattern="1", replacement = "F")


dA <- obs_pred4[obs_pred4$ID=="D-051" & obs_pred4$season=="S",]
dB <- obs_pred4[obs_pred4$ID=="D-055" & obs_pred4$season=="S",]
dC <- obs_pred4[obs_pred4$ID=="D-110" & obs_pred4$season=="S",]
dD <- obs_pred4[obs_pred4$ID=="D-096" & obs_pred4$season=="W",]
dE <- obs_pred4[obs_pred4$ID=="D-022" & obs_pred4$season=="S",]
dF <- obs_pred4[obs_pred4$ID=="A-076" & obs_pred4$season=="W",]
d1 <- rbind(dA, dB, dC, dD, dE, dF)
d1$sym <- c("A", "B", "C", "D", "E", "F")


g <- ggplot() + 
  geom_point(data=obs_pred4 , aes(x=pred, y=obs, color=sex), size=2) +
  xlim(0,35) + ylim(0,35) +
  labs(title="",x="Predicted TEWL", y="Observed TEWL") +
#  scale_color_hue(name = "Location", labels = c(A = "Akita", T ="Tokyo"), guide=TRUE ) +
 scale_color_hue(name = "sex", labels = c(F = "F", M ="M")) +
#  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
#  scale_shape_discrete(name = "Season", labels = c(S="S", W="W" ), guide=TRUE ) +
#  scale_shape_discrete(name = "Location", labels = c(A = "Akita", T ="Tokyo")) +
  theme(panel.border = element_rect(fill = NA, size=1), 
  	panel.background = element_rect(fill = NA,color = NA), 
  	text=element_text(size=25, family="Arial")) +
  geom_abline(intercept=0,slope=1)　+ coord_fixed(ratio=1)
g2 <-g + geom_text(data=d1 , aes(x=pred, y=obs, label=sym), size=6)
quartz(type="pdf", file="data/obs_pred_binary.pdf")
g2
dev.off()
