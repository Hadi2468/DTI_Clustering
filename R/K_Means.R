library(sparcl)
getwd()    # setwd("/Users/shossein/GitHub/DTI_Clustering/R")

##---------------------- K_Means --------------------------------------------------------------------------_____------------------------

master_score <- read.csv("../../Data/data_random_1/Neurocognitive_Joins/master_score.csv")
master_kmeans <- read.csv("../../Data/data_random_1/LF_Joins/master_kmeans.csv")
LF_1500 <- read.csv("../../Data/data_random_1/LF/LF_1500_py.csv")
LF <- LF_1500[-c(1:2)]    # raw Latent Features
dim(LF)

CL_2 <- kmeans(LF, 2)
Kmeans_2_score <- CL_2$cluster
length(Kmeans_2_score)
Kmeans_2_score

CL_4 <- kmeans(LF, 4)
Kmeans_4_score <- CL_4$cluster
length(Kmeans_4_score)
Kmeans_4_score

# mk <- master_kmeans
# mk[2] <- Kmeans_2_score
# mk[3] <- Kmeans_4_score
# mk
# mk[mk$Kmeans_4 == 1, ]$Kmeans_4 = 10
# mk[mk$Kmeans_4 == 2, ]$Kmeans_4 = 30
# mk[mk$Kmeans_4 == 3, ]$Kmeans_4 = 40
# mk[mk$Kmeans_4 == 4, ]$Kmeans_4 = 20
# mk
# mk[mk$Kmeans_4 == 10, ]$Kmeans_4 = 1
# mk[mk$Kmeans_4 == 20, ]$Kmeans_4 = 2
# mk[mk$Kmeans_4 == 30, ]$Kmeans_4 = 3
# mk[mk$Kmeans_4 == 40, ]$Kmeans_4 = 4
# mk
# mk[mk$Kmeans_4 != mk$Kmeans_Sparse_4, ]
# mk[mk$Kmeans_2 != mk$Kmeans_Sparse_2, ]
# write.csv(mk, "../../Data/data_random_1/LF_Joins/master_kmeans.csv", row.names = FALSE)

master_kmeans <- read.csv("../../Data/data_random_1/LF_Joins/master_kmeans.csv")
head(master_kmeans)
master_kmeans
master_kmeans[master_kmeans$Kmeans_4 != master_kmeans$Kmeans_Sparse_4, ]
master_kmeans[master_kmeans$Kmeans_2 != master_kmeans$Kmeans_Sparse_2, ]

##---------------------- K_Means_Sparse ------------------------------------------------------------------------------------------------

km.perm <- KMeansSparseCluster.permute(LF, K = 2, wbounds = seq(54, 56, len = 10), nperms = 1)
print(km.perm)
K_2_bestw <- km.perm$bestw
print(K_2_bestw)         # K=2: bestw=54.88889

km.out <- KMeansSparseCluster(LF, K = 2, wbounds = 54.88889)
print(km.out)
master_kmeans["Kmeans_Sparse_2"] <- km.out[[1]]$Cs

km.perm <- KMeansSparseCluster.permute(LF, K = 4, wbounds = seq(58, 62, len = 15), nperms = 5)
print(km.perm)
K_4_bestw <- kmd.perm$bestw
print(K_4_bestw)         # K=4: bestw=60.28571

km.out <- KMeansSparseCluster(LF, K = 4, wbounds = 60.28571)
print(km.out)
master_kmeans["Kmeans_Sparse_4"] <- km.out[[1]]$Cs

##---------------------- Save data -----------------------------------------------------------------------------------------------------

master_data <- merge(x = master_kmeans, y = master_score, by = "ID")
# colnames(master_data)[11] <- "Proc_Spd_diff"
# colnames(master_data)[15] <- "Proc_Spd_diff"
write.csv(master_data, "../../Data/data_random_1/LF_Joins/master_data.csv", row.names = FALSE)
