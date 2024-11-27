#Day 4 and day 5

#In this code, I'm going to include more arguments to improve the development 
#of my previous code. Since, I'm working with images, I can play more with 
#arguments such as rotation, brightness, zoom, color, size, and resize. 
#Fun fact--> It did not work, since I assume I do not have enough data to improve
#it. I'm going to continue in the next script (Day06)

#Structure 

#1. Libraries 
#2. Work directory 
#3. Preparing data 
#4. Model 1:VGG16 - adding more arguments
#5. Checking preliminary results through the console window. 

#1. Libraries 

#install.packages("BiocManager")
#BiocManager::install("EBImage")
library(keras)
library(tensorflow)
library(ggplot2)
library(recolorize)
library(jpeg)
library(png)
library(EBImage)
library(imager)
library(gridExtra)
library(ggplot2)

#2. Work directory 
setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitNN/")
path <- ("Data")

#3. Preparing data 
train<- file.path(path, "train")
val<- file.path(path, "evaluation")

# Data Augmentation for training data
train_data<- image_data_generator(
  rescale = 1/255,           
  shear_range = 0.2,          
  zoom_range = 0.2,           
  horizontal_flip = TRUE ,   
  rotation_range = 30,       
  width_shift_range = 0.2,   
  height_shift_range = 0.2,  
  brightness_range = c(0.8, 1.2)  
)

optimizer <- optimizer_adam(learning_rate = 0.0001)  

#rescaling size
validation_data <- image_data_generator(rescale = 1/255)

# Prepare training 
train_generator <- flow_images_from_directory(
  train,
  train_data,
  target_size = c(150, 150),   
  batch_size = 32,            
  class_mode = "categorical"  
)
#prepare evaluation data
validation_generator <- flow_images_from_directory(
  val,
  validation_data,
  target_size = c(150, 150),
  batch_size = 32,
  class_mode = "categorical"
)
#4. Model 1:VGG16 - adding more arguments
# Load the VGG16 model as a base 
base_model <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(150, 150, 3)
)

# Freeze the layers of the base model to retain pretrained weights
#freeze_weights(base_model)
unfreeze_weights(base_model, from = "block5_conv1")  
target_size = c(224, 224)

# Build the custom model using the functional API
x <- base_model$output %>%
  layer_global_average_pooling_2d() %>%
  layer_dense(units = 512, activation = 'relu') %>%  # Increase the number of units
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = length(train_generator$class_indices), activation = 'softmax')

callbacks <- list(
  callback_early_stopping(monitor = "val_loss", patience = 5),
  callback_model_checkpoint(filepath = "best_model.h5", save_best_only = TRUE)
)
# Define the build_and_train_model function
build_and_train_model <- function(model_name, model, train_generator, 
                                  validation_generator, epochs = 20,
                                  learning_rate = 0.001) {
  # Compile the model
  history <- model_vgg16 %>% fit(
    train_generator,
    steps_per_epoch = train_generator$n %/% train_generator$batch_size,
    epochs = 50,  # Higher epoch count
    validation_data = validation_generator,
    validation_steps = validation_generator$n %/% validation_generator$batch_size,
    callbacks = callbacks
  )
  
  # Get final evaluation metrics
  final_results <- model_vgg16 %>% evaluate(
    validation_generator,
    steps = validation_generator$n %/% validation_generator$batch_size
  )
  
  # Return results and training history
  list(
    name = model_name,
    results = final_results,
    history = history
  )
}
#5. Checking preliminar results through the console window. 
#-------STOP THE PREVIOUS PROCESS 
#NOT GOOD FITTING TO THE DATA ACCORDING TO THE GRAPH AND VALUES IN THE CONSOLE
#This model is not getting better. Since I'm looking through the graph and the 
#values from loss and accuracy. I think the model is over fitting or 
#underfitting. 
#Coming back to my code from code day 02_03, to perform a example of the 
#development of this code and check the outcomes. 
#After read a little bit of literature, maybe adding more arguments will 
#perform better my model but I do not have enough data to train well the model. 
