# Plant-Traits-Extraction

R code and associated files/data for the analysis from the paper "Automatic Traits Extraction and Fitting for Field High throughput Phenotyping Systems". by Xingche Guo, Yumou Qiu, Dan Nettleton, Cheng-Ting Yeh, Zihao Zheng, Stefan Hey, and Patrick S. Schnable.

## PlantMod2.hdf5
The trained self-supervised learning model for field photo segmentation.

## model_train
Folder "model_train" includes the cropped plant and background images and R code that used to train our self-superviesd learning model for plant segmentation.

* model_train/data
training images (background and plant images).

#### model_train/background_labeling.R 
#### model_train/plant_labeling.R
run these two .R files to create plant and background feature and label for neural network training.

#### model_train/train_seg_model.R
run this .R files to train our segmentation model.

## segmentation
Folder "segmentation" includes the field photos captured by one of our cameras, the segmented photos, and all the R code that used to segment the images, measure plant heights, and fit for growth curves.
