
master.file.df <- as.data.frame(fread(paste(input.local.data.dir,"master.file.csv",sep="")))
master.file.df <- subset(master.file.df, use.this == "yes")

dataset.names <- sort(unique(master.file.df$dataset))
for(this.dataset in dataset.names){
  
  # this.dataset<- "horse-colic"; this.dataset <- "lung-cancer"
  print(this.dataset)
  suppressWarnings(this.dataset.df <- as.data.frame(fread(paste(input.data.dir,"ml-datasets/",this.dataset,".data.txt",sep=""))))

  this.response.index <- ifelse(subset(master.file.df, dataset == this.dataset)$index.of.response == "last", ncol(this.dataset.df), 1)
  
  this.dataset.df <- this.dataset.df[this.dataset.df[,this.response.index] != "?",]  
  
  these.levels <- sort(unique(this.dataset.df[,this.response.index]))
  n.levels <- length(these.levels)
  # these.levels <- c("foo", "bar")
  
  suppressWarnings(is.na <- is.na(as.numeric(these.levels[1])))
  
  response.hash <- NULL
  if(!is.na){
    
     these.levels <- as.character(these.levels)
     
     if(n.levels==2){
       map.values <- c("no","yes")
     }else{
      map.values <- letters[1:n.levels]
     }
     
     response.hash <- hash(keys = these.levels, values = map.values)
     print(paste(this.dataset, "- Mapping ", paste(these.levels, collapse=" ") ,"  to ", paste(letters[1:n.levels], collapse=" ")))
    
     this.dataset.df[,this.response.index] <- unlist(lapply(this.dataset.df[,this.response.index], function(x){ response.hash[[as.character(x)]] }))
     
  }
  
  this.split.data <- split.data(this.dataset.df, 0.7)
  
  this.holdout <- this.split.data[[1]]
  this.training <- this.split.data[[2]]
  
  write.table(this.holdout, file = paste(downloaded.ml.data, this.dataset, ".data.holdout.txt",sep=""), quote = FALSE,row.names=FALSE,col.names=FALSE, sep=",", na="")
  write.table(this.training, file = paste(downloaded.ml.data, this.dataset, ".data.training.txt",sep=""), quote = FALSE,row.names=FALSE,col.names=FALSE, sep=",", na="")
  
}
