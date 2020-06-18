rm(list=ls())

df <- read.table("data/RTDA_count.txt",header=T)
IDs <- unique(df$ID)
set.seed(123456)
IDs2 <- sample(IDs,length(IDs)*0.7)
IDs3 <- setdiff(IDs, IDs2)
write.table(IDs3,file="data/IDs.txt",sep="\t",quote=FALSE,row.names=F, col.names=F)