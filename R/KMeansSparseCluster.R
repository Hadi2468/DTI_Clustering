# generate data
set.seed(11)
x <- matrix(rnorm(12*230), ncol=70)
x[1:25,1:20] <- x[1:25,1:20]+1
x <- scale(x, TRUE, TRUE)
plot(x)
print(dim(x))
# choose tuning parameter
km.perm <- KMeansSparseCluster.permute(x, K=3, wbounds=seq(3,7,len=15), nperms=5)
print(km.perm)
plot(km.perm)
# run sparse k-means
print(km.perm$bestw)
km.out <- KMeansSparseCluster(x, K=3, wbounds=km.perm$bestw)
print(km.out)
plot(km.out)
# run sparse k-means for a range of tuning parameter values
km.out <- KMeansSparseCluster(x, K=3, wbounds=seq(1.3,4,len=8))
print(km.out)
plot(km.out)
# Run sparse k-means starting from a particular set of cluster centers in the k-means algorithm.
km.out <- KMeansSparseCluster(x, wbounds=2:7, centers=x[c(1,3,5),])

