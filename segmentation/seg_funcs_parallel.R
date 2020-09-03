# this .R file applied our trained segmentation model to segment a sequence of
# field photos taken by one of our cameras. Parallel computing is used to increase
# the computation speed.

setwd("/Users/apple/Work/RA/Prof_Nettleton/image_analysis/Github/Project1")

library(doParallel)
library(keras)

cl <- makeCluster(7)
registerDoParallel(cl)


## segmentation for one folder (a sequence of plants) for .jpg
seg_seq_plant_jpg <- function(mlp, img_folder_path, seg_folder_path, dim_resize, thres=0.95){
  
  file_names <- list.files(path = img_folder_path)
  nn <- length(file_names)
  foreach(i=1:nn) %dopar%{
    
    library(EBImage)
    library(jpeg)
    library(keras)
    
    
    ################## Functions ##################
    
    ## rgb image to n x 27 (features) matrix
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
    
    ## segmentation prediction
    seg_pred <- function(nb, mlp, thres=0.5, dim_img){
      mlp <- unserialize_model(mlp)
      test.prob<-mlp %>% predict_proba(nb)
      pts.nn.nb<-rep(0,times=length(test.prob))
      pts.nn.nb[test.prob>=thres]<-1
      
      img <- matrix(NA,dim_img[1],dim_img[2])
      for(i in 1:dim_img[1]){
        img[i,]<-pts.nn.nb[((i-1)*dim_img[2]+1):(i*dim_img[2])]
      }  
      return(img)
    }
    
    o_image_name <- paste(img_folder_path, "/", file_names[i], sep = "")
    r_image_name <- paste(seg_folder_path, "/", file_names[i], sep = "")
    ts.img<-readJPEG(o_image_name) 
    ts.img.rs<-resize(ts.img, dim_resize[1], dim_resize[2]) 
    ts.nb<-img.to.mat.nbhd(ts.img.rs)  
    predicted.image <- seg_pred(ts.nb, mlp, thres, dim_resize)
    writeJPEG(predicted.image, r_image_name) 
  }
}



################## Test Functions ###############
mlp <- load_model_hdf5(file = "PlantMod2.hdf5")
img_folder_path <- "segmentation/CAM322"
seg_folder_path <- "segmentation/CAM322_seg"
mlp1 <- serialize_model(mlp)
seg_seq_plant_jpg(mlp1, img_folder_path, seg_folder_path, c(750, 1000))





