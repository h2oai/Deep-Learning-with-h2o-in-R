
this.is.first.run <- FALSE

if(this.is.first.run){
  
  # The following two commands remove any previously installed H2O packages for R.
  if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
  if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
  
  # Next, we download packages that H2O depends on.
  if (! ("methods" %in% rownames(installed.packages()))) { install.packages("methods") }
  if (! ("statmod" %in% rownames(installed.packages()))) { install.packages("statmod") }
  if (! ("stats" %in% rownames(installed.packages()))) { install.packages("stats") }
  if (! ("graphics" %in% rownames(installed.packages()))) { install.packages("graphics") }
  if (! ("RCurl" %in% rownames(installed.packages()))) { install.packages("RCurl") }
  if (! ("rjson" %in% rownames(installed.packages()))) { install.packages("rjson") }
  if (! ("tools" %in% rownames(installed.packages()))) { install.packages("tools") }
  if (! ("utils" %in% rownames(installed.packages()))) { install.packages("utils") }
  
  # Now we download, install and initialize the H2O package for R.
  install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-simons/4/R")))
  
  
  install.packages("deepnet")
  
  install.packages("rvest")
  install.packages("XML")
  
  install.packages("data.table")
  
  install.packages("clusterGeneration")

}

library("deepnet")

library("rvest")
library("XML")
library("data.table")

library("hash")
library("ggplot2")

library("MASS")
library("clusterGeneration")
library("h2o")
 
library("dplyr")  


# change to your input directory and remote data directory
root.dir <- "/Users/mercicle/_DL/Deep-Learning-with-h2o-in-R/"
remote.data.root.dir <- "/Users/mercicle/datasets/uc-irvine-ml/"

helper.dir <- paste(root.dir,"helpers/", sep="")
source(paste(helper.dir, "helper-functions.r", sep="")) 

## local data
input.local.data.dir <- paste(root.dir,"_in-data-local/", sep="")
dir.create(file.path(input.local.data.dir), showWarnings = FALSE)

output.local.data.dir <- paste(root.dir,"_out-data-local/", sep="")
dir.create(file.path(output.local.data.dir), showWarnings = FALSE)

## datasets not saved in repository
input.data.dir <- paste(remote.data.root.dir,"in-data/", sep="")
dir.create(file.path(input.data.dir), showWarnings = FALSE)

output.data.dir <- paste(remote.data.root.dir,"out-data/", sep="")
dir.create(file.path(output.data.dir), showWarnings = FALSE)

downloaded.ml.data <- paste(input.data.dir, "ml-datasets/",sep="")
dir.create(file.path(downloaded.ml.data), showWarnings = FALSE)


 
