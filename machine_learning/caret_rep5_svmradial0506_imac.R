# install.packages("doParallel")
#install.packages("kernlab")

rm(list=ls())

library(doParallel)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
library(caret)
#library(ranger)
#library(e1071)
library(ggplot2)
#library(ranger)
library(dplyr)
library(NMF)
#library(e1071)
#library(DMwR)

df <- read.table("~/Dropbox/KOSE2019/data_rep5.txt",header=T)
#colnames(df)[1:7] <- c("ID", "num", "age", "sex", "location", "season", "TEWL")
#rownames(df)<- paste(df$ID, df$num, sep="_")
df <- df[,c(-8,-9)]
#df <- df[,c(1:17)]
#df <- df[,c(1:7,12,13,17)]
#df1 <- df
#df_grid <- df[,18:ncol(df)]
#su <- apply(df_grid, 2, sum)
#df_grid <- df_grid[, su!=0]

df_grid <- df[,8:ncol(df)]
su <- apply(df_grid, 2, sum)
df_grid <- df_grid[, su!=0]
sds <- apply(df_grid, MARGIN=2, sd)
df_grid2 <- df_grid[, sds > 10]
ncol(df_grid2)

df0 <- df[,1:7]

v <- rep("X", nrow(df0))
v[df0$age <= 12] <- "C"
v[df0$age > 12] <- "A"
df0$AC <- v

df1 <- data.frame(df0, df_grid2)

#pca
#rpca=prcomp(x=df_grid,scale=T)
#p <- rpca$x[,1:20]
#df1 <- data.frame(df[,1:7], p)
#summary(lm(formula = TEWL ~ PC9, data = df1))

#nmf
#スケーリングなし
#df_grid <- df_grid[,9:ncol(df_grid)]
#res <- nmf(df_grid, rank = 6, seed = 123456, .options = "t")
#w <- basis(res)
#df1 <- data.frame(df[,1:7], w)

#summary(lm(formula = TEWL ~ X1, data = df1))

#スケーリングあり
#f <- function(col){
#  m <- max(col)
#  return(col/m)
#}
#df_grid2 <- apply(df_grid, 2, f)
#res2 <- nmf(df_grid2, rank = 6, seed = 123456, .options = "t")
#w <- basis(res2)
#df1 <- data.frame(df[,1:7], w)
IDs <- unique(df1$ID)
set.seed(123456)
IDs2 <- sample(IDs,length(IDs)*0.7)
#write.table(IDs2,file="~/Dropbox/KOSE2019/IDs2.txt",sep="\t",quote=FALSE, row.names=FALSE, col.names=F)
#IDs2 <- read.table("~/Dropbox/KOSE2019/IDs2.txt",header=F)[,1]
train_data0 <- df1[df1$ID %in% IDs2,]

TEWL <- train_data0$TEWL
q <- quantile(TEWL,c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
TEWL_low <- q[2]
TEWL_high <- q[8]
v <- rep("X", length(TEWL))
v[TEWL < TEWL_low] <- "L"
v[TEWL > TEWL_low & TEWL < TEWL_high] <- "M"
v[TEWL > TEWL_high] <- "H"
train_data0$HML <- v
train_H <- train_data0[train_data0$HML=="H",]
train_M <- train_data0[train_data0$HML=="M",]
train_L <- train_data0[train_data0$HML=="L",]
#train_M2 <- train_M
train_M2 <- train_M[train_M$num==1 | train_M$num==4,]
#set.seed(10)
#Mrows <- sample(rownames(train_M), nrow(train_M) * 0.33)
#train_M2 <- train_M[Mrows,]
#set.seed(123456)
#Mrows <- sample(rownames(train_M), nrow(train_H))
#set.seed(123456)
#Lrows <- sample(rownames(train_L), nrow(train_H))
train_data02 <- rbind(train_H, train_M2, train_L)
#train_data02 <- rbind(train_H, train_M[Mrows,], train_L[Lrows,])
train_data <- train_data02[,c(-1,-2,-8, -ncol(train_data02))]
#train_data <- train_data[,c(-3,-4)]

test_data0 <- df1[!df1$ID %in% IDs2,]
test_data <- test_data0[,c(-1,-2,-8)]
#test_data <- test_data[,c(-3,-4)]

#train_data <- train_data[,1:5]
#test_data <- test_data[,1:5]


fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10,
                           savePredictions = TRUE)
"""
tGrid  <- expand.grid(mstop = c(0.1,1,10), maxdepth= c(1,10,100,1000), nu=c(0.1,1,10))

model <- train(TEWL ~ ., data = train_data, 
           method = "bstTree", 
           trControl = fitControl,
           tuneGrid = tGrid,
           maximize = FALSE,
           importance = TRUE,
           preProcess = c('center', 'scale'))
"""

tGrid  <- expand.grid(sigma = c(0.01,0.1,1), C= c(1,10,100))

model <- train(TEWL ~ ., data = train_data, 
	         method = "svmRadial", 
	         trControl = fitControl,
           tuneGrid = tGrid,
	         maximize = FALSE,
	         importance = TRUE,
	         preProcess = c('center', 'scale'))

saveRDS(model, file = "~/Dropbox/KOSE2019/svmradial_rep5.obj")
model <- readRDS("~/Dropbox/KOSE2019/svmradial_rep5.obj")


imp <- varImp(model,scale = FALSE)
quartz(type="pdf", file="~/Dropbox/KOSE2019/imp_rep5_svmradial.pdf")
plot(imp)
dev.off()

write.table(imp$importance,file="~/Dropbox/KOSE2019/imp_rep5_svmradial.txt",sep="\t",quote=FALSE)


pred <- predict(model, test_data)
obs <- test_data$TEWL
obs_pred <- as.data.frame(cbind(obs,pred))
obs_pred2 <- cbind(ID=test_data0$ID,num=test_data0$num,obs_pred)
ID <- obs_pred2$ID

obs_pred3 <- cbind(obs_pred2, age=test_data0$age, location=test_data0$location, sex=test_data0$sex, season=test_data0$season, AC=test_data0$AC)
d0 <- data.frame()
for (ID in unique(obs_pred3$ID)){
  d <- obs_pred3[obs_pred3$ID==ID,]
  s <- d[d$season=="S",]
  meds <- median(s$pred)
  ss <- cbind(s[1,], med=meds)
  w <- d[d$season=="W",] 
  medw <- median(w$pred)
  ww <- cbind(w[1,], med=medw)
  d0 <- rbind(d0,ss,ww)
}
obs_pred4 <- na.omit(d0)
obs_pred4 <- obs_pred4[,c(1,3,5,6,7,8,9,10)]
colnames(obs_pred4)<- c("ID","obs","age","location","sex","season","AC","pred")
obs_pred4$res <- obs_pred4$pred - obs_pred4$obs

write.table(obs_pred4,file="~/Dropbox/KOSE2019/obs_pred4_age_sex.txt",sep="\t",quote=FALSE,row.names=F)

#g1 <- ggplot(data=obs_pred3, aes(x=pred,y=obs, label=ID, color=C_or_A)) + geom_point() + geom_abline(intercept=0,slope=1)
#g2 <- ggplot(data=obs_pred3, aes(x=pred,y=obs, label=ID, color=C_or_A)) + geom_text() + geom_abline(intercept=0,slope=1)
#ggsave(filename="/Users/keitakoseki/Dropbox/KOSE201809/rf_point_sp.pdf", plot=g1)
#ggsave(filename="/Users/keitakoseki/Dropbox/KOSE201809/rf_text_sp.pdf", plot=g2)

g <- ggplot() + 
  geom_point(data=obs_pred4 , aes(x=pred, y=obs, color=sex, shape=AC), size=2) +
  xlim(0,40) + ylim(0,40) +
  labs(title="Support vector regression",x="Predicted TEWL", y="Observed TEWL") +
#  scale_color_hue(name = "Location", labels = c(A = "Akita", T ="Tokyo"), guide=TRUE ) +
 scale_color_hue(name = "sex", labels = c(F = "F", M ="M")) +
#  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
#  scale_shape_discrete(name = "Season", labels = c(S="S", W="W" ), guide=TRUE ) +
  scale_shape_discrete(name = "Adult/Child", labels = c(A = "Adult", C ="Child")) +
  theme(panel.border = element_rect(fill = NA, size=1), 
  	panel.background = element_rect(fill = NA,color = NA), 
  	text=element_text(size=25, family="Arial")) +
  geom_abline(intercept=0,slope=1)　+ coord_fixed(ratio=1)
quartz(type="pdf", file="~/Dropbox/KOSE2019/pred_obs_new_AC.pdf")
g
dev.off()

g <- ggplot() + 
  geom_text(data=obs_pred4 , aes(x=pred, y=obs, color=sex, label=ID), size=2) +
  xlim(0,40) + ylim(0,40) +
  labs(title="SVM regression",x="Predicted TEWL", y="Observed TEWL") +
#  scale_color_hue(name = "Location", labels = c(A = "Akita", T ="Tokyo"), guide=TRUE ) +
 scale_color_hue(name = "sex", labels = c(F = "F", M ="M")) +
#  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
#  scale_shape_discrete(name = "Season", labels = c(S="S", W="W" ), guide=TRUE ) +
  scale_shape_discrete(name = "Location", labels = c(A = "Akita", T ="Tokyo")) +
  theme(panel.border = element_rect(fill = NA, size=1), 
    panel.background = element_rect(fill = NA,color = NA), 
    text=element_text(size=25, family="Arial")) +
  geom_abline(intercept=0,slope=1)　+ coord_fixed(ratio=1)
quartz(type="pdf", file="~/Dropbox/KOSE2019/pred_obs_text_new_noTDA.pdf")
g
dev.off()
#g <- ggplot() + 
#  geom_point(data=obs_pred4 , aes(x=obs, y=res, color=sex, shape=location), size=2) +
#  xlim(0,40) + ylim(-10,10) +
#  labs(title="Random forest regression",x="Observed TEWL", y="Residual", color="sex", shape="Sex") +
#  scale_color_hue(name = "Location", labels = c(A = "Akita", T ="Tokyo"), guide=TRUE ) +
# scale_color_hue(name = "sex", labels = c(F = "F", M ="M")) +
#  scale_shape_discrete(name = "Sex", labels = c( W ="Female",M = "Male"), guide=guide_legend(reverse=TRUE) ) +
#  scale_shape_discrete(name = "Season", labels = c(S="S", W="W" ), guide=TRUE ) +
#  scale_shape_discrete(name = "Location", labels = c(A = "Akita", T ="Tokyo")) +
#  theme(panel.border = element_rect(fill = NA, size=1), 
#    panel.background = element_rect(fill = NA,color = NA), 
#    text=element_text(size=25, family="Arial")) +
#  geom_abline(intercept=0,slope=0)
#quartz(type="pdf", file="~/Dropbox/KOSE2019/pred_obs_1_5.pdf")
#g
#dev.off()

post_eval <- postResample(pred = obs_pred4$pred, obs = obs_pred4$obs)
write.table(post_eval,file="~/Dropbox/KOSE2019/eval_noTDA_new.txt",sep="\t",quote=FALSE)

