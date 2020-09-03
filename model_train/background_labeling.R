# This .R file is used to transform the cropped background images to feature matrix 
# that can be used in neural network training.

setwd("/Users/apple/Work/RA/Prof_Nettleton/image_analysis/Github/Project1")

library(EBImage)
library(png)
library(jpeg)

img.to.mat.nbhd<-function(img){
  k<-1
  img.pad<-array(NA,dim=c(dim(img)[1]+2,dim(img)[2]+2,3))
  img.pad[,,1]<-cbind(0,rbind(0,img[,,1],0),0)
  img.pad[,,2]<-cbind(0,rbind(0,img[,,2],0),0)
  img.pad[,,3]<-cbind(0,rbind(0,img[,,3],0),0)
  img.mat<-matrix(NA,nrow=(dim(img)[1])*(dim(img)[2]),ncol=27)
  for(i in 2:(dim(img.pad)[1]-1)){
    for(j in 2:(dim(img.pad)[2]-1)){
      img.mat[k,]<-c(as.vector(img.pad[((i-1):(i+1)),((j-1):(j+1)),1]),
                     as.vector(img.pad[((i-1):(i+1)),((j-1):(j+1)),2]),
                     as.vector(img.pad[((i-1):(i+1)),((j-1):(j+1)),3]))
      k<-k+1
    }
  }
  return(img.mat)
}


## for .jpg images
file_names <- list.files(path = "model_train/data/background/jpg")
nn <- length(file_names)
feature1 <- NULL
for (i in 1:nn){
  train <- readJPEG(paste("model_train/data/background/jpg/", 
                          file_names[i], sep = ""))
  tr <- img.to.mat.nbhd(train)
  feature1 <- rbind(feature1, tr)
}


## for .png images
file_names <- list.files(path = "model_train/data/background/png")
nn <- length(file_names)
feature2 <- NULL
for (i in 1:nn){
  train <- readPNG(paste("model_train/data/background/png/", 
                          file_names[i], sep = ""))
  tr <- img.to.mat.nbhd(train)
  feature2 <- rbind(feature2, tr)
}

feature <- rbind(feature1, feature2)
label <- rep(0, dim(feature)[1])

Train0 <- list(feature = feature, label = label)
saveRDS(Train0, file = "model_train/Train0.rds")
