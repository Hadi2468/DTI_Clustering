library(sparcl)
getwd()    # setwd("/Users/shossein/GitHub/DTI_Clustering/R")
# setwd("/Users/shossein/GitHub/DTI_Clustering/R")

##-------------------------------------------- Loading data ----------------------------------------------------------------------------

LF_1500 <- read.csv("../../Data/data_random_1/R_master/LF124_e1500.csv")

master_Kmeans <- LF_1500[c(1)]     # Start creating master K-means data with patient IDs
names(master_Kmeans)[1] <- "ID"
is.data.frame(master_Kmeans)

LF <- LF_1500[-c(1)]    # remove patient ID as raw Latent Features for K-means algorithm
dim(LF)
is.data.frame(LF)

master_score <- read.csv("../../Data/data_random_1/R_master/master_score.csv")

# master_score$Neuro_Cog_Score <- c(rep("score", 100))  # Adding a string columns for patients with neuro-cognitive score
# master_score <- master_score[c(1,14,2:13)]
is.data.frame(master_score)
dim(master_score)

##-------------------------------------------- Traditional K_Means ---------------------------------------------------------------------

CL_2 <- kmeans(LF, 2)
Kmeans_2_score <- CL_2$cluster
length(Kmeans_2_score)
Kmeans_2_score
master_Kmeans$Kmeans_2 <- Kmeans_2_score

CL_3 <- kmeans(LF, 3)
Kmeans_3_score <- CL_3$cluster
length(Kmeans_3_score)
Kmeans_3_score
master_Kmeans$Kmeans_3 <- Kmeans_3_score

CL_4 <- kmeans(LF, 4)
Kmeans_4_score <- CL_4$cluster
length(Kmeans_4_score)
Kmeans_4_score
master_Kmeans$Kmeans_4 <- Kmeans_4_score

CL_5 <- kmeans(LF, 5)
Kmeans_5_score <- CL_5$cluster
length(Kmeans_5_score)
Kmeans_5_score
master_Kmeans$Kmeans_5 <- Kmeans_5_score

head(master_Kmeans)

##-------------------------------------------- Sparse K_Means (K=2) --------------------------------------------------------------------

# km.perm <- KMeansSparseCluster.permute(LF, K = 2, wbounds = seq(40, 55, len = 31), nperms = 5)
# print(km.perm)
# K_2_bestw <- km.perm$bestw
# print(K_2_bestw)         # K=2: bestw is 44

km.out <- KMeansSparseCluster(LF, K = 2, wbounds = 44)
print(km.out)
master_Kmeans["Sparse_Kmeans_2"] <- km.out[[1]]$Cs

##-------------------------------------------- Sparse K_Means (K=3) --------------------------------------------------------------------

# km.perm <- KMeansSparseCluster.permute(LF, K = 3, wbounds = seq(50, 60, len = 21), nperms = 5)
# print(km.perm)
# K_3_bestw <- km.perm$bestw
# print(K_3_bestw)         # K=3: bestw for e1500 is 52

km.out <- KMeansSparseCluster(LF, K = 3, wbounds = 52)
print(km.out)
master_Kmeans["Sparse_Kmeans_3"] <- km.out[[1]]$Cs

##-------------------------------------------- Sparse K_Means (K=4) --------------------------------------------------------------------

# km.perm <- KMeansSparseCluster.permute(LF, K = 4, wbounds = seq(50, 70, len = 41), nperms = 5)
# print(km.perm)
# K_4_bestw <- km.perm$bestw
# print(K_4_bestw)         # K=4: bestw for e1500 is 65.5

km.out <- KMeansSparseCluster(LF, K = 4, wbounds = 65.5)
print(km.out)
master_Kmeans["Sparse_Kmeans_4"] <- km.out[[1]]$Cs

##-------------------------------------------- Sparse K_Means (K=5) --------------------------------------------------------------------

# km.perm <- KMeansSparseCluster.permute(LF, K = 5, wbounds = seq(50, 70, len = 41), nperms = 5)
# print(km.perm)
# K_5_bestw <- km.perm$bestw
# print(K_5_bestw)         # K=5: bestw for e1500 is 66

km.out <- KMeansSparseCluster(LF, K = 5, wbounds = 66)
print(km.out)
master_Kmeans["Sparse_Kmeans_5"] <- km.out[[1]]$Cs

##-------------------------------------------- Data Engineering ------------------------------------------------------------------------

master_Kmeans <- master_Kmeans[c(1, 2, 6, 3, 7, 4, 8, 5, 9)]

# master_Kmeans[master_Kmeans$Kmeans_2 == 1, ]$Kmeans_2 = 20
# master_Kmeans[master_Kmeans$Kmeans_2 == 2, ]$Kmeans_2 = 10
# master_Kmeans[master_Kmeans$Kmeans_2 == 10, ]$Kmeans_2 = 1
# master_Kmeans[master_Kmeans$Kmeans_2 == 20, ]$Kmeans_2 = 2
nrow(master_Kmeans[master_Kmeans$Kmeans_2 != master_Kmeans$Sparse_Kmeans_2, ])

# master_Kmeans[master_Kmeans$Kmeans_3 == 1, ]$Kmeans_3 = 20
# master_Kmeans[master_Kmeans$Kmeans_3 == 2, ]$Kmeans_3 = 10
# master_Kmeans[master_Kmeans$Kmeans_3 == 3, ]$Kmeans_3 = 30
# master_Kmeans[master_Kmeans$Kmeans_3 == 10, ]$Kmeans_3 = 1
# master_Kmeans[master_Kmeans$Kmeans_3 == 20, ]$Kmeans_3 = 2
# master_Kmeans[master_Kmeans$Kmeans_3 == 30, ]$Kmeans_3 = 3
# master_Kmeans[master_Kmeans$Sparse_Kmeans_3 == 1,  ]$Sparse_Kmeans_3 = 20
# master_Kmeans[master_Kmeans$Sparse_Kmeans_3 == 2,  ]$Sparse_Kmeans_3 = 10
# master_Kmeans[master_Kmeans$Sparse_Kmeans_3 == 3,  ]$Sparse_Kmeans_3 = 30
# master_Kmeans[master_Kmeans$Sparse_Kmeans_3 == 10, ]$Sparse_Kmeans_3 = 1
# master_Kmeans[master_Kmeans$Sparse_Kmeans_3 == 20, ]$Sparse_Kmeans_3 = 2
# master_Kmeans[master_Kmeans$Sparse_Kmeans_3 == 30, ]$Sparse_Kmeans_3 = 3
nrow(master_Kmeans[master_Kmeans$Kmeans_3 != master_Kmeans$Sparse_Kmeans_3, ])

# master_Kmeans[master_Kmeans$Kmeans_4 == 1, ]$Kmeans_4 = 40
# master_Kmeans[master_Kmeans$Kmeans_4 == 2, ]$Kmeans_4 = 20
# master_Kmeans[master_Kmeans$Kmeans_4 == 3, ]$Kmeans_4 = 40
# master_Kmeans[master_Kmeans$Kmeans_4 == 4, ]$Kmeans_4 = 30
# master_Kmeans[master_Kmeans$Kmeans_4 == 10, ]$Kmeans_4 = 1
# master_Kmeans[master_Kmeans$Kmeans_4 == 20, ]$Kmeans_4 = 2
# master_Kmeans[master_Kmeans$Kmeans_4 == 30, ]$Kmeans_4 = 3
# master_Kmeans[master_Kmeans$Kmeans_4 == 40, ]$Kmeans_4 = 4
# master_Kmeans[master_Kmeans$Sparse_Kmeans_4 == 1,  ]$Sparse_Kmeans_4 = 20
# master_Kmeans[master_Kmeans$Sparse_Kmeans_4 == 2,  ]$Sparse_Kmeans_4 = 40
# master_Kmeans[master_Kmeans$Sparse_Kmeans_4 == 3,  ]$Sparse_Kmeans_4 = 40
# master_Kmeans[master_Kmeans$Sparse_Kmeans_4 == 4,  ]$Sparse_Kmeans_4 = 30
# master_Kmeans[master_Kmeans$Sparse_Kmeans_4 == 10, ]$Sparse_Kmeans_4 = 1
# master_Kmeans[master_Kmeans$Sparse_Kmeans_4 == 20, ]$Sparse_Kmeans_4 = 2
# master_Kmeans[master_Kmeans$Sparse_Kmeans_4 == 30, ]$Sparse_Kmeans_4 = 3
# master_Kmeans[master_Kmeans$Sparse_Kmeans_4 == 40, ]$Sparse_Kmeans_4 = 4
nrow(master_Kmeans[master_Kmeans$Kmeans_4 != master_Kmeans$Sparse_Kmeans_4, ])

# master_Kmeans[master_Kmeans$Kmeans_5 == 1, ]$Kmeans_5 = 10
# master_Kmeans[master_Kmeans$Kmeans_5 == 2, ]$Kmeans_5 = 30
# master_Kmeans[master_Kmeans$Kmeans_5 == 3, ]$Kmeans_5 = 50
# master_Kmeans[master_Kmeans$Kmeans_5 == 4, ]$Kmeans_5 = 20
# master_Kmeans[master_Kmeans$Kmeans_5 == 5, ]$Kmeans_5 = 40
# master_Kmeans[master_Kmeans$Kmeans_5 == 10, ]$Kmeans_5 = 1
# master_Kmeans[master_Kmeans$Kmeans_5 == 20, ]$Kmeans_5 = 2
# master_Kmeans[master_Kmeans$Kmeans_5 == 30, ]$Kmeans_5 = 3
# master_Kmeans[master_Kmeans$Kmeans_5 == 40, ]$Kmeans_5 = 4
# master_Kmeans[master_Kmeans$Kmeans_5 == 50, ]$Kmeans_5 = 5
# master_Kmeans[master_Kmeans$Sparse_Kmeans_5 == 1,  ]$Sparse_Kmeans_5 = 20
# master_Kmeans[master_Kmeans$Sparse_Kmeans_5 == 2,  ]$Sparse_Kmeans_5 = 50
# master_Kmeans[master_Kmeans$Sparse_Kmeans_5 == 3,  ]$Sparse_Kmeans_5 = 30
# master_Kmeans[master_Kmeans$Sparse_Kmeans_5 == 4,  ]$Sparse_Kmeans_5 = 10
# master_Kmeans[master_Kmeans$Sparse_Kmeans_5 == 5,  ]$Sparse_Kmeans_5 = 40
# master_Kmeans[master_Kmeans$Sparse_Kmeans_5 == 10, ]$Sparse_Kmeans_5 = 1
# master_Kmeans[master_Kmeans$Sparse_Kmeans_5 == 20, ]$Sparse_Kmeans_5 = 2
# master_Kmeans[master_Kmeans$Sparse_Kmeans_5 == 30, ]$Sparse_Kmeans_5 = 3
# master_Kmeans[master_Kmeans$Sparse_Kmeans_5 == 40, ]$Sparse_Kmeans_5 = 4
# master_Kmeans[master_Kmeans$Sparse_Kmeans_5 == 50, ]$Sparse_Kmeans_5 = 5
nrow(master_Kmeans[master_Kmeans$Kmeans_5 != master_Kmeans$Sparse_Kmeans_5, ])

head(master_Kmeans)
master_kmeans_124 <- master_Kmeans

##-------------------------------------------- Save data -------------------------------------------------------------------------------

master_data_100 <- merge(x = master_kmeans_124, y = master_score, by = "ID")
master_data_124 <- merge(x = master_kmeans_124, y = master_score, by = "ID", all=TRUE)
master_data_100 <- master_data_100[c(1, 10, 2:9, 11:22)]
master_data_124 <- master_data_124[c(1, 10, 2:9, 11:22)]

# write.csv(master_score,      "../../Data/data_random_1/R_master/master_score.csv", row.names = FALSE)
write.csv(master_data_100,   "../../Data/data_random_1/R_master/master_data_100_original_labels.csv", row.names = FALSE)
write.csv(master_data_124,   "../../Data/data_random_1/R_master/master_data_124_original_labels.csv", row.names = FALSE)
write.csv(master_kmeans_124, "../../Data/data_random_1/R_master/master_kmeans_124_original_labels.csv", row.names = FALSE)

# write.csv(master_data_100,   "../../Data/data_random_1/R_master/master_data_100.csv", row.names = FALSE)
# write.csv(master_data_124,   "../../Data/data_random_1/R_master/master_data_124.csv", row.names = FALSE)
# write.csv(master_kmeans_124, "../../Data/data_random_1/R_master/master_kmeans_124.csv", row.names = FALSE)

##-------------------------------------------- Read data -----------------------------------------------------------------------

master_score      <- read.csv("../../Data/data_random_1/R_master/master_score.csv")
master_data_100   <- read.csv("../../Data/data_random_1/R_master/master_data_100.csv")
master_data_124   <- read.csv("../../Data/data_random_1/R_master/master_data_124.csv")
master_kmeans_124 <- read.csv("../../Data/data_random_1/R_master/master_kmeans_124.csv")

master_score      <- read.csv("../../Data/data_random_1/R_master/master_score.csv")
master_data_100   <- read.csv("../../Data/data_random_1/R_master/master_data_100_original_labels.csv")
master_data_124   <- read.csv("../../Data/data_random_1/R_master/master_data_124_original_labels.csv")
master_kmeans_124 <- read.csv("../../Data/data_random_1/R_master/master_kmeans_124_original_labels.csv")

