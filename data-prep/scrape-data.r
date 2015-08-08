
# http://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/

uci.dataset.page <- html("http://archive.ics.uci.edu/ml/datasets.html")
links.i.need <- uci.dataset.page %>% html_nodes("a") 
names.i.need <- uci.dataset.page %>% html_nodes("a") %>% html_text()

index <- 1
total.datasets <- 0
dataset.sample.size <- Inf
dataset.status.df <- NULL
data.url.prefix <- "http://archive.ics.uci.edu/ml/datasets/" #e.g. datasets/Abalone

for(link in links.i.need){
  
  this.string <- as(link, "character")
  is.dataset <- grepl("datasets/",this.string)
  
  if(is.dataset && (total.datasets <= dataset.sample.size)){
    
    suffix.i.need <- base::strsplit(base::strsplit(this.string, "datasets/",fixed = TRUE)[[1]][2] , '"')[[1]][1]
    
    sub.link.to.check <- paste(data.url.prefix, suffix.i.need, sep="")
    
    uci.dataset.subpage <- html(sub.link.to.check)
    
    uci.dataset.subpage.text <- as(uci.dataset.subpage, "character")
    is.classification <- grepl('<td><p class="normal">Classification</p></td>',uci.dataset.subpage.text)
    is.multivariate <- grepl('<td><p class="normal">Multivariate</p></td>',uci.dataset.subpage.text)
    
    ds.prefix.i.need <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"

    if(is.multivariate && is.classification){

      first.subpage.split <- base::strsplit(uci.dataset.subpage.text, "/machine-learning-databases/",fixed = TRUE)[[1]]
      
      suffix.1 <- base::strsplit(first.subpage.split[2], '/"')[[1]][1] 
      suffix.2 <- base::strsplit(base::strsplit(first.subpage.split[3],".names")[[1]][1], "/")[[1]][2]
      
      suffix.1 <- gsub("[\n]", "", suffix.1);suffix.2 <- gsub("[\n]", "", suffix.2)
      
      final.dataset.url <- paste(ds.prefix.i.need, suffix.1, "/",suffix.2,".data",sep="")
      final.datanames.url <- paste(ds.prefix.i.need, suffix.1, "/",suffix.2,".names",sep="")
      
      data.write.path <- paste(downloaded.ml.data, suffix.1, ".data.text", sep="")
      metadata.write.path <- paste(downloaded.ml.data, suffix.1, ".names.txt", sep="")
      
      data.download.error <<- FALSE

      tryCatch(download.file(final.dataset.url, data.write.path),
               error = function(e){ 
                 data.download.error <<- TRUE
                 print(paste("Unable to download: ", suffix.1,"  ", e, sep=""))
                 file.remove(data.write.path)
               })
      
      if(!data.download.error){
        
        total.datasets <- total.datasets + 1
        
        tryCatch(download.file(final.datanames.url, metadata.write.path),
                 error = function(e){ 
                   print(paste("Unable to download: ", final.datanames.url, sep=""))
                 }) 
        
        dataset.status.df <- rbind(dataset.status.df, data.frame(dataset = suffix.1, status = "Downloaded", data.url = final.dataset.url, metadata.url = final.datanames.url))
      }else{
        dataset.status.df <- rbind(dataset.status.df, data.frame(dataset = suffix.1, status = "Error", data.url = final.dataset.url, metadata.url = final.datanames.url))
      }

      
    }

  }

}

dataset.status.df <- dataset.status.df[!duplicated(dataset.status.df$dataset),]
dataset.status.df <- dataset.status.df[order(dataset.status.df$status),]
print(paste("Total Datasets: ", nrow(dataset.status.df),sep=""))

write.table(dataset.status.df, file = paste(downloaded.ml.data, "_Datasets.csv",sep=""), quote = FALSE,row.names=FALSE, sep=",", na="")
