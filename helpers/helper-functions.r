
split.data <- function(in.file, holdout.percent){
  
  in.file$holdout <- ifelse(runif(nrow(in.file)) > holdout.percent,1,0)
  in.file.holdout <- subset(in.file,holdout==1)
  in.file.training <- subset(in.file,holdout==0) 
  
  in.file.holdout <- subset(in.file.holdout, select=-holdout)
  in.file.training <- subset(in.file.training, select=-holdout)
  
  return (c(list(in.file.holdout), list(in.file.training)))
  
}