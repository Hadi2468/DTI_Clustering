library(sparcl)
getwd()    # setwd("/Users/shossein/GitHub/DTI_Clustering/R")
# setwd("/Users/shossein/GitHub/DTI_Clustering/R")

##---------------------- K_Means -------------------------------------------------------------------------------------------------------

# LF_500 <- read.csv("../../Data/data_random_1/LF/LF124_e500.csv")
LF_1500 <- read.csv("../../Data/data_random_1/LF/LF124_e1500.csv")
master_score <- read.csv("../../Data/data_random_1/Neurocognitive_Joins/master_score_union_124.csv")
is.data.frame(master_score)

# LF <- LF_500[-c(1)]    # raw Latent Features
mk <- LF_1500[c(1)]
LF <- LF_1500[-c(1)]    # raw Latent Features
dim(LF)
is.data.frame(LF)

CL_2 <- kmeans(LF, 2)
Kmeans_2_score <- CL_2$cluster
length(Kmeans_2_score)
Kmeans_2_score
mk$Kmeans_2 <- Kmeans_2_score

CL_3 <- kmeans(LF, 3)
Kmeans_3_score <- CL_3$cluster
length(Kmeans_3_score)
Kmeans_3_score
mk$Kmeans_3 <- Kmeans_3_score

CL_4 <- kmeans(LF, 4)
Kmeans_4_score <- CL_4$cluster
length(Kmeans_4_score)
Kmeans_4_score
mk$Kmeans_4 <- Kmeans_4_score

CL_5 <- kmeans(LF, 5)
Kmeans_5_score <- CL_5$cluster
length(Kmeans_5_score)
Kmeans_5_score
mk$Kmeans_5 <- Kmeans_5_score

names(mk)[1] <- "ID"
is.data.frame(mk)
head(mk)

##---------------------- K_Means_Sparse K=2 --------------------------------------------------------------------------------------------
km.perm <- KMeansSparseCluster.permute(LF, K = 2, wbounds = seq(40, 55, len = 31), nperms = 5)
print(km.perm)
K_2_bestw <- km.perm$bestw
print(K_2_bestw)         # K=2: bestw for e500 is 51.11111 and for e1500 is 44

km.out <- KMeansSparseCluster(LF, K = 2, wbounds = 44)
print(km.out)
mk["Kmeans_Sparse_2"] <- km.out[[1]]$Cs

##---------------------- K_Means_Sparse K=3 --------------------------------------------------------------------------------------------
km.perm <- KMeansSparseCluster.permute(LF, K = 3, wbounds = seq(50, 60, len = 21), nperms = 5)
print(km.perm)
K_3_bestw <- km.perm$bestw
print(K_3_bestw)         # K=3: bestw for e1500 is 52

km.out <- KMeansSparseCluster(LF, K = 3, wbounds = 52)
print(km.out)
mk["Kmeans_Sparse_3"] <- km.out[[1]]$Cs

##---------------------- K_Means_Sparse K=4 --------------------------------------------------------------------------------------------
km.perm <- KMeansSparseCluster.permute(LF, K = 4, wbounds = seq(50, 70, len = 41), nperms = 5)
print(km.perm)
K_4_bestw <- km.perm$bestw
print(K_4_bestw)         # K=4: bestw for e500 is 60.28571 and for e1500 is 65.5

km.out <- KMeansSparseCluster(LF, K = 4, wbounds = 65.5)
print(km.out)
mk["Kmeans_Sparse_4"] <- km.out[[1]]$Cs

##---------------------- K_Means_Sparse K=5 --------------------------------------------------------------------------------------------
km.perm <- KMeansSparseCluster.permute(LF, K = 5, wbounds = seq(50, 70, len = 41), nperms = 5)
print(km.perm)
K_5_bestw <- km.perm$bestw
print(K_5_bestw)         # K=5: bestw for e1500 is 65.5

km.out <- KMeansSparseCluster(LF, K = 5, wbounds = 65.5)
print(km.out)
mk["Kmeans_Sparse_5"] <- km.out[[1]]$Cs

##---------------------- data engineering ----------------------------------------------------------------------------------------------

mk <- mk[, c("ID", "Kmeans_2", "Kmeans_Sparse_2", "Kmeans_3", "Kmeans_Sparse_3", "Kmeans_4", "Kmeans_Sparse_4", "Kmeans_5", "Kmeans_Sparse_5")]

# mk[mk$Kmeans_2 == 1, ]$Kmeans_2 = 20
# mk[mk$Kmeans_2 == 2, ]$Kmeans_2 = 10
# mk[mk$Kmeans_2 == 10, ]$Kmeans_2 = 1
# mk[mk$Kmeans_2 == 20, ]$Kmeans_2 = 2
nrow(mk[mk$Kmeans_2 != mk$Kmeans_Sparse_2, ])

# mk[mk$Kmeans_3 == 1, ]$Kmeans_3 = 20
# mk[mk$Kmeans_3 == 2, ]$Kmeans_3 = 10
# mk[mk$Kmeans_3 == 3, ]$Kmeans_3 = 20
# mk[mk$Kmeans_3 == 10, ]$Kmeans_3 = 1
# mk[mk$Kmeans_3 == 20, ]$Kmeans_3 = 2
# mk[mk$Kmeans_3 == 30, ]$Kmeans_3 = 3
# mk[mk$Kmeans_Sparse_3 == 1,  ]$Kmeans_Sparse_3 = 20
# mk[mk$Kmeans_Sparse_3 == 2,  ]$Kmeans_Sparse_3 = 10
# mk[mk$Kmeans_Sparse_3 == 3,  ]$Kmeans_Sparse_3 = 20
# mk[mk$Kmeans_Sparse_3 == 10, ]$Kmeans_Sparse_3 = 1
# mk[mk$Kmeans_Sparse_3 == 20, ]$Kmeans_Sparse_3 = 2
# mk[mk$Kmeans_Sparse_3 == 30, ]$Kmeans_Sparse_3 = 3
nrow(mk[mk$Kmeans_3 != mk$Kmeans_Sparse_3, ])

# mk[mk$Kmeans_4 == 1, ]$Kmeans_4 = 20
# mk[mk$Kmeans_4 == 2, ]$Kmeans_4 = 30
# mk[mk$Kmeans_4 == 3, ]$Kmeans_4 = 40
# mk[mk$Kmeans_4 == 4, ]$Kmeans_4 = 10
# mk[mk$Kmeans_4 == 10, ]$Kmeans_4 = 1
# mk[mk$Kmeans_4 == 20, ]$Kmeans_4 = 2
# mk[mk$Kmeans_4 == 30, ]$Kmeans_4 = 3
# mk[mk$Kmeans_4 == 40, ]$Kmeans_4 = 4
# mk[mk$Kmeans_Sparse_4 == 1,  ]$Kmeans_Sparse_4 = 20
# mk[mk$Kmeans_Sparse_4 == 2,  ]$Kmeans_Sparse_4 = 40
# mk[mk$Kmeans_Sparse_4 == 3,  ]$Kmeans_Sparse_4 = 10
# mk[mk$Kmeans_Sparse_4 == 4,  ]$Kmeans_Sparse_4 = 30
# mk[mk$Kmeans_Sparse_4 == 10, ]$Kmeans_Sparse_4 = 1
# mk[mk$Kmeans_Sparse_4 == 20, ]$Kmeans_Sparse_4 = 2
# mk[mk$Kmeans_Sparse_4 == 30, ]$Kmeans_Sparse_4 = 3
# mk[mk$Kmeans_Sparse_4 == 40, ]$Kmeans_Sparse_4 = 4
# nrow(mk[mk$Kmeans_4 != mk$Kmeans_Sparse_4, ])

# mk[mk$Kmeans_5 == 1, ]$Kmeans_5 = 20
# mk[mk$Kmeans_5 == 2, ]$Kmeans_5 = 30
# mk[mk$Kmeans_5 == 3, ]$Kmeans_5 = 40
# mk[mk$Kmeans_5 == 4, ]$Kmeans_5 = 10
# mk[mk$Kmeans_5 == 5, ]$Kmeans_5 = 50
# mk[mk$Kmeans_5 == 10, ]$Kmeans_5 = 1
# mk[mk$Kmeans_5 == 20, ]$Kmeans_5 = 2
# mk[mk$Kmeans_5 == 30, ]$Kmeans_5 = 3
# mk[mk$Kmeans_5 == 40, ]$Kmeans_5 = 4
# mk[mk$Kmeans_5 == 50, ]$Kmeans_5 = 5
# mk[mk$Kmeans_Sparse_5 == 1,  ]$Kmeans_Sparse_5 = 20
# mk[mk$Kmeans_Sparse_5 == 2,  ]$Kmeans_Sparse_5 = 40
# mk[mk$Kmeans_Sparse_5 == 3,  ]$Kmeans_Sparse_5 = 10
# mk[mk$Kmeans_Sparse_5 == 4,  ]$Kmeans_Sparse_5 = 30
# mk[mk$Kmeans_Sparse_5 == 5,  ]$Kmeans_Sparse_5 = 50
# mk[mk$Kmeans_Sparse_5 == 10, ]$Kmeans_Sparse_5 = 1
# mk[mk$Kmeans_Sparse_5 == 20, ]$Kmeans_Sparse_5 = 2
# mk[mk$Kmeans_Sparse_5 == 30, ]$Kmeans_Sparse_5 = 3
# mk[mk$Kmeans_Sparse_5 == 40, ]$Kmeans_Sparse_5 = 4
# mk[mk$Kmeans_Sparse_5 == 50, ]$Kmeans_Sparse_5 = 5
# nrow(mk[mk$Kmeans_5 != mk$Kmeans_Sparse_5, ])

head(mk)

##---------------------- Save data -----------------------------------------------------------------------------------------------------

master_data_union <- merge(x = mk, y = master_score, by = "ID")

write.csv(master_score, "../../Data/data_random_1/Neurocognitive_Joins/master_score_union_124.csv", row.names = FALSE)
write.csv(mk, "../../Data/data_random_1/LF_Joins/master_kmeans_124.csv", row.names = FALSE)
write.csv(master_data_union, "../../Data/data_random_1/LF_Joins/master_data_union_124.csv", row.names = FALSE)

master_data_union_124 <- read.csv("../../Data/data_random_1/LF_Joins/master_data_union_124.csv")
master_kmeans_124 <- read.csv("../../Data/data_random_1/LF_Joins/master_kmeans_124.csv")
master_score_union_124 <- read.csv("../../Data/data_random_1/Neurocognitive_Joins/master_score_union_124.csv")

