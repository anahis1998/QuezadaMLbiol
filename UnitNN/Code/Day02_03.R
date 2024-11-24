#Day 2 
#In this script, I'm going to add more models to apply to my flower dataset. 
#Here. I'm calling the previous step from Code Day01_.R
#Dataset : Flowers's pictures

library(keras)
library(tensorflow)
library(recolorize)
library(jpeg)
library(png)

setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitNN/")
path <- ("Data")

train<- file.path(path, "train")
val<- file.path(path, "evaluation")

# Data Augmentation for training data
train_data<- image_data_generator(
  rescale = 1/255,            # Normalize pixel values
  shear_range = 0.2,          # Shear transformations
  zoom_range = 0.2,           # Random zoom
  horizontal_flip = TRUE      # Flip images horizontally
)
#rescaling size
validation_data <- image_data_generator(rescale = 1/255)

# Prepare training 
train_generator <- flow_images_from_directory(
  train,
  train_data,
  target_size = c(150, 150),   # Resize images to 150x150
  batch_size = 32,            # Number of images per batch
  class_mode = "categorical"  # Multiclass classification
)
#prepare evaluation data
validation_generator <- flow_images_from_directory(
  val,
  validation_data,
  target_size = c(150, 150),
  batch_size = 32,
  class_mode = "categorical"
)
