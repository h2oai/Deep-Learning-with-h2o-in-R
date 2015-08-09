
h2o.server <- h2o.init(nthreads=-1)
h2o.server.status <- h2o.clusterStatus(h2o.server)

# h2o.shutdown(h2o.server)

master.file.df <- as.data.frame(fread(paste(input.local.data.dir,"master.file.csv",sep="")))
master.file.df <- subset(master.file.df, use.this == "yes")

dl.with.dropout <- TRUE
dataset.names <- sort(unique(master.file.df$dataset))

all.classification.errors <- NULL
for(this.dataset in dataset.names){
  
  # this.dataset <- "horse-colic" dataset.names[1]
  print(paste0("STARTING Deep Learning on: ", this.dataset))
  
  this.response.location <- subset(master.file.df, dataset == this.dataset)$index.of.response

  tryCatch((h2o.server.status <- h2o.clusterStatus(h2o.server)),
           error = function(e){ 
             print("Starting h2o Server")
             h2o.server <- h2o.init(nthreads=-1)
           }) 
  
  training.df <- h2o.importFile(h2o.server, path = paste(input.data.dir,"ml-datasets/",this.dataset,".data.training.txt",sep=""), header = F, sep = ',')
  test.df <- h2o.importFile(h2o.server, path = paste(input.data.dir,"ml-datasets/",this.dataset,".data.holdout.txt",sep=""), header = F, sep = ',')
  
  n.cols <- ncol(training.df)
  
  predictor.cols <- NULL
  response.col <- NULL

  if(this.response.location == "last"){
    response.col <- n.cols
    predictor.cols <- 1:(n.cols-1)
  }else{
    response.col <- 1
    predictor.cols <- 2:n.cols
  }
 
  column.names <- colnames(training.df)
  
  dl.results <- h2o.deeplearning(x=column.names[predictor.cols], 
                                 y=column.names[response.col], 
                                 training_frame=training.df, 
                                 validation_frame=test.df,
                                 hidden=c(10,10,10,10),
                                 epochs=0.1, 
                                 activation="Rectifier",
                                 loss = "CrossEntropy", #response defined by loss
                                 export_weights_and_biases = TRUE)
  
  validation.classification.error <- dl.results@model$validation_metrics@metrics$MSE  #documentation states this is classification error for c problems
  
  dnn.weights.list <- dl.results@model$weights
  weights.df.names <- c()
  for(dnn.weights in dnn.weights.list){ weights.df.names <- c(weights.df.names,dnn.weights$name) }
  
  dnn.weights.list <- list()
  for(weights.df.name in weights.df.names){
    dnn.weights.list[[weights.df.name]] <- as.data.frame(h2o.getFrame(weights.df.name, conn = h2o.getConnection(), linkToGC = FALSE))
  }
  
  save(dnn.weights.list, file=paste0(output.data.dir, "saved-weights/", this.dataset, "-weights-list.RData") )
  h2o.saveModel(dl.results, filename=paste0(output.data.dir, "saved-models/",this.dataset,"_model"), force=TRUE)
  
  validation.classification.error.dropout <- ""
  if(dl.with.dropout){
    
      dl.results.w.dropout <- h2o.deeplearning(x=column.names[predictor.cols], 
                                                y=column.names[response.col], 
                                                training_frame=training.df, 
                                                validation_frame=test.df,
                                                hidden=c(10,10,10,10),
                                                epochs=0.1,
                                                activation="RectifierWithDropout",  # "TanhWithDropout"
                                                loss = "CrossEntropy", #response defined by loss
                                                export_weights_and_biases = TRUE)
                
      dl.model.w.dropout <- dl.results.w.dropout@model
      
      validation.classification.error.dropout <- dl.results.w.dropout@model$validation_metrics@metrics$MSE  #documentation states this is classification error for c problems
      
      dnn.dropout.weights.list <- dl.results.w.dropout@model$weights
      weights.dropout.df.names <- c()
      for(dnn.weights in dnn.dropout.weights.list){ weights.dropout.df.names <- c(weights.dropout.df.names,dnn.weights$name) }
      
      dnn.dropout.weights.list <- list()
      for(weights.df.name in weights.dropout.df.names){
        dnn.dropout.weights.list[[weights.df.name]] <- as.data.frame(h2o.getFrame(weights.df.name, conn = h2o.getConnection(), linkToGC = FALSE))
      }
      
      save(dnn.dropout.weights.list, file=paste0(output.data.dir, "saved-weights/", this.dataset, "-dropout-weights-list.RData") )
      h2o.saveModel(dl.results.w.dropout, filename=paste0(output.data.dir, "saved-models/",this.dataset,"_dropout_model"), force=TRUE)
      
  }
  
  print(paste0("COMPLETED Deep Learning on: ", this.dataset))
  print(paste("Classification Error for Dropout: ", validation.classification.error.dropout))
  print(paste("Classification Error: ", validation.classification.error))
  
  these.classification.errors <- data.frame(dataset=this.dataset, 
                                            cross.entropy.from.dropout=round(validation.classification.error.dropout,2),
                                            cross.entropy = round(validation.classification.error, 2))
  
  all.classification.errors <- rbind(all.classification.errors, these.classification.errors)
}


write.table(all.classification.errors, file = paste(output.local.data.dir, "cross-entropy-all-datasets.csv",sep=""),row.names=FALSE, sep=",", na="")
 
