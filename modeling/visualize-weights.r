
ml.datasets.df <- as.data.frame(fread(paste(output.local.data.dir,"ml-dataset-summaries-clustered.csv",sep="")))
dataset.names <- sort(unique(ml.datasets.df$dataset))

final.weights.viz.df <- NULL
for(this.dataset in dataset.names){
  
  # this.dataset <- dataset.names[1]
  print(paste0("Preparing: ", this.dataset))
  
  this.dataset.metadata <- subset(ml.datasets.df, dataset == this.dataset)

  load(file=paste0(output.data.dir, "saved-weights/", this.dataset, "-weights-list.RData"))
  load(file=paste0(output.data.dir, "saved-weights/", this.dataset, "-dropout-weights-list.RData"))

  activation.hash <- hash()
  .set(activation.hash, "rectifier", dnn.weights.list)
  .set(activation.hash, "RectifierWithDropout", dnn.dropout.weights.list)
  
  this.weights.df <- NULL
  activation.types <- keys(activation.hash)
  for(activation.type in activation.types){
 
    layer.index <- 0
    dnn.weights.list <- activation.hash[[activation.type]]
    n.layers <- length(dnn.weights.list)
    
    for(dnn.layer.weights in dnn.weights.list){
      
      layer.index <- layer.index + 1
      if(layer.index == 1 || layer.index == n.layers){
        next
      }else{
        n.cols <- ncol(dnn.layer.weights); n.rows <- nrow(dnn.layer.weights)
        prefix <- paste0("h",layer.index,"_w")
        for(col in 1:n.cols){
          this.label <- paste0(prefix, (col-1))
          these.labels <- paste(this.label, 0:(n.rows-1), sep="")
          these.values <- dnn.layer.weights[,col]
          this.weights.df <- rbind(this.weights.df, data.frame(weightid=these.labels, weight=these.values,activation.type=activation.type))
        }
      }
    }
  }
  
  this.weights.df <- cbind(this.weights.df, this.dataset.metadata)
  
  final.weights.viz.df <- rbind(final.weights.viz.df, this.weights.df)
  
}

final.weights.viz.df$layer[grepl("h2", final.weights.viz.df$weightid, fixed = TRUE)] <- "Hidden2"
final.weights.viz.df$layer[grepl("h3", final.weights.viz.df$weightid, fixed = TRUE)] <- "Hidden3"
final.weights.viz.df$layer[grepl("h4", final.weights.viz.df$weightid, fixed = TRUE)] <- "Hidden4"

write.table(final.weights.viz.df, file = paste(output.local.data.dir, "annotated-weights-data.csv",sep=""),row.names=FALSE, sep=",", na="")

final.weights.viz.df.metadata <- subset(final.weights.viz.df, select=-weight)
final.weights.viz.df.weights <- subset(final.weights.viz.df, select=c(weightid, weight))

final.weights.viz.df.weights$one <- 1
final.weights.viz.df.weights.wide <- final.weights.viz.df.weights %>% group_by(weightid) %>% mutate(cumsum = cumsum(one))
final.weights.viz.df.weights.wide <- subset(final.weights.viz.df.weights.wide, select=-one)
final.weights.viz.df.weights.wide <- arrange(final.weights.viz.df.weights.wide, weightid)
final.weights.viz.df.weights.wide <- as.data.frame(final.weights.viz.df.weights.wide)
final.weights.viz.df.weights.wide <- reshape(final.weights.viz.df.weights.wide, timevar = "cumsum",idvar = "weightid",v.names = "weight",direction = "wide")
head(final.weights.viz.df.weights.wide)

final.weights.viz.df.weights.wide$layer[grepl("h2", final.weights.viz.df.weights.wide$weightid, fixed = TRUE)] <- "Hidden2"
final.weights.viz.df.weights.wide$layer[grepl("h3", final.weights.viz.df.weights.wide$weightid, fixed = TRUE)] <- "Hidden3"
final.weights.viz.df.weights.wide$layer[grepl("h4", final.weights.viz.df.weights.wide$weightid, fixed = TRUE)] <- "Hidden4"

n.cols <- ncol(final.weights.viz.df.weights.wide)
final.weights.viz.df.weights.wide <- final.weights.viz.df.weights.wide[,c(1,n.cols, 2:(n.cols-1))]

 
 