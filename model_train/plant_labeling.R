setwd("/Users/apple/Work/RA/Prof_Nettleton/image_analysis/Github/Project1")

library(EBImage)
library(png)
library(jpeg)

img.to.mat<-function(img){
  k<-1
  img.mat<-matrix(NA,nrow=dim(img)[1]*dim(img)[2],ncol=3)
  for(i in 1:dim(img)[1]){
    for(j in 1:dim(img)[2]){
      
      img.mat[k,]<-c(img[i,j,1],img[i,j,2],img[i,j,3])
      k<-k+1
    }
  }
  return(img.mat)
}

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


## for .jpg images  (cropped field images)
file_names <- list.files(path = "model_train/data/plant/jpg")
nn <- length(file_names)
feature1 <- NULL
for (i in 1:nn){
  train <- readJPEG(paste("model_train/data/plant/jpg/", 
                          file_names[i], sep = ""))
  tr <- img.to.mat.nbhd(train)
  feature1 <- rbind(feature1, tr)
}





## for .png images  (cropped greenhouse images)
## should apply K-means (K=3) algorithm to each images

train1_1 <- readPNG("model_train/data/plant/png/train1_1.png")
train1_2 <- readPNG("model_train/data/plant/png/train1_2.png")
train1_3 <- readPNG("model_train/data/plant/png/train1_3.png")
train1_4 <- readPNG("model_train/data/plant/png/train1_4.png")
train1_5 <- readPNG("model_train/data/plant/png/train1_5.png")
train1_6 <- readPNG("model_train/data/plant/png/train1_6.png")

t1 <- img.to.mat(train1_1)
t2 <- img.to.mat(train1_2)
t3 <- img.to.mat(train1_3)
t4 <- img.to.mat(train1_4)
t5 <- img.to.mat(train1_5)
t6 <- img.to.mat(train1_6)


##### photo 1 #####
set.seed(123)
km.t1<-kmeans(t1,centers=3)     
class1<-km.t1$cluster     

pred1<-rep(0,times=length(class1)) 
tclass1<-2                  
pred1[class1==tclass1]<-1    

p.image1<-matrix(NA,dim(train1_1)[1],dim(train1_1)[2])  
for(i in 1:dim(train1_1)[1]){
  p.image1[i,]<-pred1[((i-1)*dim(train1_1)[2]+1):(i*dim(train1_1)[2])]
}

writePNG(p.image1,"model_train/data/plant/png/PredictPlant1.png")  




##### photo 2 #####
set.seed(123)
km.t2<-kmeans(t2,centers=3)     
class2<-km.t2$cluster     

pred2<-rep(0,times=length(class2)) 
tclass2<-2                 
pred2[class2==tclass2]<-1    

p.image2<-matrix(NA,dim(train1_2)[1],dim(train1_2)[2])  
for(i in 1:dim(train1_2)[1]){
  p.image2[i,]<-pred2[((i-1)*dim(train1_2)[2]+1):(i*dim(train1_2)[2])]
}

writePNG(p.image2,"model_train/data/plant/png/PredictPlant2.png")  



##### photo 3 #####
set.seed(123)
km.t3<-kmeans(t3,centers=3)     
class3<-km.t3$cluster     

pred3<-rep(0,times=length(class3)) 
tclass3<-1               
pred3[class3==tclass3]<-1    

p.image3<-matrix(NA,dim(train1_3)[1],dim(train1_3)[2])  
for(i in 1:dim(train1_3)[1]){
  p.image3[i,]<-pred3[((i-1)*dim(train1_3)[2]+1):(i*dim(train1_3)[2])]
}

writePNG(p.image3,"model_train/data/plant/png/PredictPlant3.png")  






##### photo 4 #####
set.seed(123)
km.t4<-kmeans(t4,centers=3)     
class4<-km.t4$cluster     

pred4<-rep(0,times=length(class4)) 
tclass4<-3               
pred4[class4==tclass4]<-1    

p.image4<-matrix(NA,dim(train1_4)[1],dim(train1_4)[2])  
for(i in 1:dim(train1_4)[1]){
  p.image4[i,]<-pred4[((i-1)*dim(train1_4)[2]+1):(i*dim(train1_4)[2])]
}

writePNG(p.image4,"model_train/data/plant/png/PredictPlant4.png")  






##### photo 5 #####
set.seed(123)
km.t5<-kmeans(t5,centers=3)     
class5<-km.t5$cluster     

pred5<-rep(0,times=length(class5)) 
tclass5<-3               
pred5[class5==tclass5]<-1    

p.image5<-matrix(NA,dim(train1_5)[1],dim(train1_5)[2])  
for(i in 1:dim(train1_5)[1]){
  p.image5[i,]<-pred5[((i-1)*dim(train1_5)[2]+1):(i*dim(train1_5)[2])]
}

writePNG(p.image5,"model_train/data/plant/png/PredictPlant5.png")  



##### photo 6 #####
set.seed(123)
km.t6<-kmeans(t6,centers=3)     
class6<-km.t6$cluster     

pred6<-rep(0,times=length(class6)) 
tclass6<-2               
pred6[class6==tclass6]<-1    

p.image6<-matrix(NA,dim(train1_6)[1],dim(train1_6)[2])  
for(i in 1:dim(train1_6)[1]){
  p.image6[i,]<-pred6[((i-1)*dim(train1_6)[2]+1):(i*dim(train1_6)[2])]
}

writePNG(p.image6,"model_train/data/plant/png/PredictPlant6.png")  




feature2 <- rbind(
  img.to.mat.nbhd(train1_1),
  img.to.mat.nbhd(train1_2),
  img.to.mat.nbhd(train1_3),
  img.to.mat.nbhd(train1_4),
  img.to.mat.nbhd(train1_5),
  img.to.mat.nbhd(train1_6)
)

label2 <- c(pred1, pred2, pred3, pred4, pred5, pred6)
feature2 <- feature2[label2==1,]

feature <- rbind(feature1, feature2)
label <- rep(1, dim(feature)[1])

Train1 <- list(feature = feature, label = label)
saveRDS(Train1, file = "model_train/Train1.rds")


