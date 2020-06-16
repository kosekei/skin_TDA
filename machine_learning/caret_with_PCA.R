rm(list=ls())

library(doParallel)
cl <- makePSOCKcluster(2)
registerDoParallel(cl)
library(caret)
library(ggplot2)
library(dplyr)

df <- read.table("data/RTDA_count.txt",header=T)
df <- df[,c(-8,-9)]
df_para <- df[,1:7]
df_para$temperature <- rep(0, nrow(df_para))
df_para$humidity <- rep(0, nrow(df_para))
df_para[df_para$location=="Akita" & df_para$season=="S",]$temperature <- 25.0
df_para[df_para$location=="Akita" & df_para$season=="S",]$humidity <- 76
df_para[df_para$location=="Tokyo" & df_para$season=="S",]$temperature <- 28.1
df_para[df_para$location=="Tokyo" & df_para$season=="S",]$humidity <- 77
df_para[df_para$location=="Akita" & df_para$season=="W",]$temperature <- 2.7
df_para[df_para$location=="Akita" & df_para$season=="W",]$humidity <- 75
df_para[df_para$location=="Tokyo" & df_para$season=="W",]$temperature <- 8.3
df_para[df_para$location=="Tokyo" & df_para$season=="W",]$humidity <- 61

rpca=prcomp(x=df_grid,scale=T)
p <- rpca$x[,1:20]
df1 <- data.frame(df[,1:7], p)

IDs2 <- as.vector(t(unique(read.table("data/IDs.txt",header=F))))
train_data0 <- df1[!df1$ID %in% IDs2,]
train_data <- train_data0[,c(-1,-2,-5,-6)]
test_data0 <- df1[df1$ID %in% IDs2,]
test_data <- test_data0[,c(-1,-2,-5,-6)]

fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10,
                           savePredictions = TRUE)

model_rf <- train(TEWL ~ ., data = train_data, 
           method = "rf", 
           trControl = fitControl,
           maximize = FALSE,
           importance = TRUE,
           preProcess = c('center', 'scale'))


model_svm <- train(TEWL ~ ., data = train_data, 
	         method = "svmRadial", 
	         trControl = fitControl,
	         maximize = FALSE,
	         importance = TRUE,
	         preProcess = c('center', 'scale'))

model_lm <- train(TEWL ~ ., data = train_data, 
           method = "lm", 
           trControl = fitControl,
           maximize = FALSE,
           importance = TRUE,
           preProcess = c('center', 'scale'))

model_enet <- train(TEWL ~ ., data = train_data, 
           method = "enet", 
           trControl = fitControl,
           maximize = FALSE,
           importance = TRUE,
           preProcess = c('center', 'scale'), linout = TRUE)

model_boost_linear <- train(TEWL ~ ., data = train_data, 
           method = "xgbLinear", 
           trControl = fitControl,
           maximize = FALSE,
           importance = TRUE,
           preProcess = c('center', 'scale'))

model_boost_tree <- train(TEWL ~ ., data = train_data, 
           method = "xgbTree", 
           trControl = fitControl,
           maximize = FALSE,
           importance = TRUE,
           preProcess = c('center', 'scale'))

model_nnet <- train(TEWL ~ ., data = train_data, 
           method = "nnet", 
           trControl = fitControl,
           maximize = FALSE,
           importance = TRUE,
           preProcess = c('center', 'scale'))

model_neuralnet <- train(TEWL ~ ., data = train_data, 
           method = "neuralnet", 
           trControl = fitControl,
           maximize = FALSE,
           importance = TRUE,
           preProcess = c('center', 'scale'))

d <- data.frame()
d <- rbind(d,postResample(pred = predict(model_rf, test_data), obs = test_data$TEWL))
d <- rbind(d,postResample(pred = predict(model_svm, test_data), obs = test_data$TEWL))
d <- rbind(d,postResample(pred = predict(model_lm, test_data), obs = test_data$TEWL))
d <- rbind(d,postResample(pred = predict(model_enet, test_data), obs = test_data$TEWL))
d <- rbind(d,postResample(pred = predict(model_boost_linear, test_data), obs = test_data$TEWL))
d <- rbind(d,postResample(pred = predict(model_boost_tree, test_data), obs = test_data$TEWL))
colnames(d) <- c("RMSE","R2","MAE")
rownames(d)<- c("RF", "SVM", "LM","ENET","Boost_Linear","Boost_Tree")
write.table(d,file="~/Dropbox/TDA_share/caret_comparison_with_PCA.txt",sep="\t",quote=FALSE)
