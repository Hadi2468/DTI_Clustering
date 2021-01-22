library(sparcl)
library(RcppCNPy)
getwd()

##---------------------- LF_500 --------------------------------------------------------------------------------------------------
LF_500_py <- read.csv("../../Data/data_random_1/LF/LF_500_py.csv")
df_500 = subset(LF_500_py, select = -c(ID, class_e500))
df_500[1:5, 1:10]
dim(df_500)
km_500.perm <- KMeansSparseCluster.permute(df_500,  K = 4, wbounds = seq(53, 57, len = 15), nperms = 5)
print(km_500.perm)
K_4_e500_bestw <- km_500.perm$bestw
print(K_4_e500_bestw)         # K=4 for e500: bestw=53.85714
km_500.out <- KMeansSparseCluster(df_500, K = 4, wbounds = 53.85714)
print(km_500.out)
LF_500_r <- cbind(km_500.out[[1]]$Cs, df_500)
LF_500_r[1:10, 1:10]
dim(LF_500_r)
length(colnames(df_500))
colnames(LF_500_r) <- c("epoch_500", colnames(df_500))
LF_500_r[1:10, 1:10]

##---------------------- LF_1500 --------------------------------------------------------------------------------------------------
LF_1500_py <- read.csv("../../Data/data_random_1/LF/LF_1500_py.csv")
df_1500 = subset(LF_1500_py, select = -c(ID, class_e1500))
df_1500[1:10, 1:10]
dim(df_1500)
km_1500.perm <- KMeansSparseCluster.permute(df_1500, K = 4, wbounds = seq(58, 62, len = 15), nperms = 5)
print(km_1500.perm)
K_4_e1500_bestw <- km_1500.perm$bestw
print(K_4_e1500_bestw)         # K=4 for e1500: bestw=60.28571
km_1500.out <- KMeansSparseCluster(df_1500, K = 4, wbounds = 60.28571)
print(km_1500.out)
LF_1500_r <- cbind(km_1500.out[[1]]$Cs, df_1500)
LF_1500_r[1:10, 1:10]
dim(LF_1500_r)
length(colnames(df_1500))
colnames(LF_1500_r) <- c("epoch_1500", colnames(df_1500))
LF_1500_r[1:10, 1:10]

##---------------------- Mix together --------------------------------------------------------------------------------------------------
LF_1500_py[1:10, 1:10]
LF_500_r[1:10, 1:10]
LF_1500_r[1:10, 1:10]
LF_r <- cbind(LF_1500_py[1], LF_500_r[1], LF_1500_r)
LF_r[1:10, 1:10]
save(LF_r,file = "../../Data/data_random_1/LF/LF_r.Rda")
load(file="../../Data/data_random_1/LF/LF_r.Rda")
dim(LF_r)
class(LF_r)
write.csv(LF_r, "../../Data/data_random_1/LF/LF_r.csv", row.names = FALSE)
