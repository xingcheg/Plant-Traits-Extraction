# this .R file use Keras to train our segmentation model (neural network model).

setwd("/Users/apple/Work/RA/Prof_Nettleton/image_analysis/Github/Project1")
library(keras)

Train0 <- readRDS(file = "model_train/Train0.rds")
Train1 <- readRDS(file = "model_train/Train1.rds")

X <- rbind(Train0$feature, Train1$feature)
Y <- c(Train0$label, Train1$label)
Y.lab<-to_categorical(Y,2)

#####Define and Train Network#####
mlp<-keras_model_sequential()
mlp %>%
  layer_dense(units=1024,input_shape=27,activation = "relu") %>%
  layer_dropout(0.45) %>%
  layer_dense(units=512,activation="relu") %>%
  layer_dropout(0.35) %>%
  layer_dense(units=1,activation="sigmoid")

#summary(mlp)  #can use to view a description of the network before training

mlp %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(0.001),
  metrics = c('accuracy')
)                                      #define optimization algorithm and loss function

history <- mlp %>% fit(
  X, Y, 
  epochs = 20, batch_size = 1024, 
  validation_split = 0.01
)                                      #train network (takes about 40 minutes on my machine)


save_model_hdf5(mlp,"PlantMod2.hdf5") #saves the model trained here for future use

