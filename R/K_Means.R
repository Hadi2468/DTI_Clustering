library(sparcl)
getwd()    # setwd("/Users/shossein/GitHub/DTI_Clustering/R")

##---------------------- K_Means --------------------------------------------------------------------------_____------------------------

master_score <- read.csv("../../Data/data_random_1/Neurocognitive_Joins/master_score.csv")
LF_1500 <- read.csv("../../Data/data_random_1/LF/LF_1500_py.csv")
LF <- LF_1500[ , -c(ID, class_e1500)]    # raw Latent Features
dim(LF)

CL_2 <- kmeans(LF, 2)
Kmeans_2_score <- CL_2$cluster
length(Kmeans_2_score)

CL_4 <- kmeans(LF, 4)
Kmeans_4_score <- CL_4$cluster
length(Kmeans_4_score)

master_Kmeans <- data.frame(ID = LF_1500$ID, Kmeans_2 = Kmeans_2_score, Kmeans_4 = Kmeans_4_score)
head(master_Kmeans)

##---------------------- K_Means_Sparse ------------------------------------------------------------------------------------------------

km.perm <- KMeansSparseCluster.permute(LF, K = 2, wbounds = seq(54, 56, len = 10), nperms = 1)
print(km.perm)
K_2_bestw <- km.perm$bestw
print(K_2_bestw)         # K=2: bestw=54.88889

km.out <- KMeansSparseCluster(LF, K = 2, wbounds = 54.88889)
print(km.out)
master_Kmeans["Kmeans_Sparse_2"] <- km.out[[1]]$Cs

km.perm <- KMeansSparseCluster.permute(LF, K = 4, wbounds = seq(58, 62, len = 15), nperms = 5)
print(km.perm)
K_4_bestw <- kmd.perm$bestw
print(K_4_bestw)         # K=4: bestw=60.28571

km.out <- KMeansSparseCluster(LF, K = 4, wbounds = 60.28571)
print(km.out)
master_Kmeans["Kmeans_Sparse_4"] <- km.out[[1]]$Cs

##---------------------- Save data -----------------------------------------------------------------------------------------------------

write.csv(master_Kmeans, "../../Data/data_random_1/LF_Joins/master_kmeans.csv", row.names = FALSE)
master_data <- merge(x = master_Kmeans, y = master_score, by = "ID")
write.csv(master_data, "../../Data/data_random_1/LF_Joins/master_data.csv", row.names = FALSE)
