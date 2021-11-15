d <- read.csv('./data/rua.csv', header =T)
row.names(d) <- d$Date
d <- d[,-1]
d <- Filter(function(x)!any(is.na(x)), d)
r <- diff(log(d[,1]))
for (i in 2:length(d)){
  r <- cbind(r,diff(log(d[,i])))
}
colnames(r) <- names(d)
PCA <- prcomp(r)
pca1 <- PCA$rotation[,1]
sort(pca1,decreasing = FALSE)[1:10]
weight <- abs(pca1)/sum(pca1)
myrs <- apply(r[,order(pca1, decreasing = FALSE)[1:10]],1,mean)
mycrs <- exp(cumsum(myrs))
mycrs
plot(mycrs, type='l')

