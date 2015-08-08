
control.data.1 <- NULL

n.cols <- 11
n <- 1000
for(i in 1:n.cols){
  this.col <- rnorm(n)
  if(i==n.cols){  this.col[this.col>=0] <- 1; this.col[this.col<0] <- 0 }
  control.data.1 <- cbind(control.data.1,this.col )
}

write.table(control.data.1, file = paste(downloaded.ml.data, "control1.data.txt",sep=""), quote = FALSE,row.names=FALSE, sep=",", na="", col.names = FALSE)

m.rank <- 11
mu <- rep(0,m.rank)
corr.matrix <- matrix(runif(m.rank*m.rank, min=0, max=1), nrow=m.rank, ncol=m.rank)
diag(corr.matrix)<-1

corr.matrix.t <- t(corr.matrix)

corr.matrix[lower.tri(corr.matrix)] <- corr.matrix.t[lower.tri(corr.matrix.t)]

corr.matrix <- as.matrix(genPositiveDefMat("unifcorrmat",dim=m.rank)$Sigma)

control.data.2 <- mvrnorm(n=1000, mu=mu, Sigma=corr.matrix)

control.data.2[,m.rank] <- ifelse(control.data.2[,m.rank]>0,1,0)

write.table(control.data.2, file = paste(downloaded.ml.data, "control2.data.txt",sep=""), quote = FALSE,row.names=FALSE, sep=",", na="",col.names = FALSE)
