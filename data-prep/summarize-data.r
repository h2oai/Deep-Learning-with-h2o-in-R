
master.file.df <- as.data.frame(fread(paste(input.local.data.dir,"master.file.csv",sep="")))
master.file.df <- subset(master.file.df, use.this == "yes")

# do summary of each dataset: size of data, predictors, distribition of continuous and categorical

dataset.names <- unique(master.file.df$dataset)

dataset.summaries <- NULL
for(this.dataset in dataset.names){
  
  print(this.dataset)
  this.dataset.df <- as.data.frame(fread(paste(input.data.dir,"ml-datasets/",this.dataset,".data.txt",sep="")))
  
  this.response.index <- ifelse(subset(master.file.df, dataset == this.dataset)$index.of.response == "last", ncol(this.dataset.df), 1)

  these.levels <- unique(this.dataset.df[,this.response.index])
  n.levels <- length(these.levels)
  this.summary <- data.frame(dataset = this.dataset, n=nrow(this.dataset.df), response.levels=paste(these.levels, collapse=";"),n.levels=n.levels, n.predictors=(ncol(this.dataset.df)-1))
  dataset.summaries <- rbind(dataset.summaries, this.summary)
  
}

dataset.summaries <- dataset.summaries[order(dataset.summaries$n), ]

write.table(dataset.summaries, file = paste(output.local.data.dir, "ml-dataset-summaries.csv",sep=""), quote = FALSE,row.names=FALSE, sep=",", na="")

########################################
#####    Cluster Into Groups      ######
########################################

n.clusters <- 5
method <- "n" 
file.prefix <- paste("ClusterVar-",method,"-K-", n.clusters, sep="")

if(method == "both"){
  k.means <- kmeans(dataset.summaries[,c("n", "n.predictors")] ,n.clusters)
}else{
  k.means <- kmeans(dataset.summaries[,c("n")],n.clusters)
}

dataset.summaries$cluster <- k.means$cluster

cluster.summary <- with(dataset.summaries, tapply(n, cluster, mean)) 
cluster.summary <- data.frame(value=cluster.summary, cluster=seq(1:nrow(cluster.summary)))
cluster.summary <- cluster.summary[order(-cluster.summary$value),]
cluster.summary$new_cluster1 <- seq(1,nrow(cluster.summary))
clusterMap <- hash(keys=as.character(cluster.summary$cluster), values= cluster.summary$new_cluster1)

dataset.summaries$dataset.cluster  <-  unlist(sapply( dataset.summaries[,"cluster"], function(x) { clusterMap[[as.character(x)]] } ))
dataset.summaries <- dataset.summaries[order(dataset.summaries$n),]

write.table(dataset.summaries, file = paste(output.local.data.dir, "ml-dataset-summaries-clustered.csv",sep=""), quote = FALSE,row.names=FALSE, sep=",", na="")

summaryObj <- summary(as.factor(dataset.summaries$dataset.cluster))
summary.df <- data.frame(cluster=names(summaryObj), count=summaryObj)
write.table(summary.df, file = paste(output.local.data.dir, "ml-dataset-summaries-cluster-summary.csv",sep=""), quote = FALSE,row.names=FALSE, sep=",", na="")

par(font=2)
pdf(file = paste(output.local.data.dir,"Dataset-Cluster-Quandrant-", file.prefix, ".pdf",sep=""))

ggplot() + 
  geom_point(data=dataset.summaries, mapping=aes(x=log(n), y=log(n.predictors),color=factor(dataset.cluster)), width=1.5, size=4, fill="white") + 
  labs(title = paste(file.prefix, sep=""),x = "Log[Dataset Size (n)]", y = "Log[Number of Predictors]") +
  theme_bw() +
  theme(legend.text = element_text(size = 14, colour = "black"),plot.title = element_text(size = 18),text = element_text(size=16))+
  scale_colour_brewer(name = "Cluster",palette="Set1")

dev.off();
par(font=1)


