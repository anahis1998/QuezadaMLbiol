#Day 1: Fitting a first neural network
#Name: Anahi Quezada
#Date: 11/09/2024

#Identify a simpler and a slightly more complex dataset for yourself, as indicated 
#in class. For the simpler: orient yourself to the data and do whatever preparation 
#is needed; invent and code an appropriate simple neural network model; fit the 
#model appropriately; create plots and numeric results showing how well it
#fits and the progress of fitting. Script everything, of course.In your write up,
#please have: a data section with a brief description of the data you are using; 
#a brief description of your first model; some results about how well it fits, 
#including the results of plotting your history.

#Dataset : Flowers's pictures
#Link: https://www.kaggle.com/datasets/imsparsh/flowers-dataset
#Organization of the code 

#1. Libraries 
#2. Pre processing 
#3. NN Model 

#1. ------Libraries 
library(readxl)
library(keras)
library(tensorflow)
library(recolorize)
library(jpeg)
library(png)

#2. ------Pre processing 
setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitNN/")
path <- ("Data")
#Manually, I splitted the data of each folder, to create the train folder 
#and the validation folder (with enough equal sample for each class)

train<- file.path(path, "train")
val<- file.path(path, "evaluation")

# Pre processing the data 
#Using image_data_generator from keras::image_data_generator to normalize my data
train_data<- image_data_generator(
  rescale = 1/255,            # Normalize pixel values
  shear_range = 0.2,          # random shear transformation
  zoom_range = 0.2,           # in or out by 20%
  horizontal_flip = TRUE     
)
#rescaling size
validation_data <- image_data_generator(rescale = 1/255) #255 its the maximum 
#pixel value in 8 bit for RGB images 

# Prepare training 
train_generator <- flow_images_from_directory(
  train,
  train_data,
  target_size = c(150, 150),   # Resize images to 150x150
  batch_size = 32,            # Number of images per batch
  class_mode = "categorical"  # 5 flower classes
)
#prepare evaluation data
validation_generator <- flow_images_from_directory(
  val,
  validation_data,
  target_size = c(150, 150),
  batch_size = 32,
  class_mode = "categorical"
)

#3. -------NN Model 
# Model 1. Visual Geometry Group (VGG) 16 -> 16 layers
base_model <- application_vgg16(
  weights = "imagenet",       # Pre-trained on ImageNet
  include_top = FALSE,        # Exclude fully connected top layers
  input_shape = c(150, 150, 3) # Input image dimensions (pixels wide, pixels tall,
                               #3 color channels)
)

model <- keras_model_sequential() %>%
  base_model %>%
  layer_global_average_pooling_2d() %>% # Reduce dimensions 
  layer_dense(units = 256, activation = 'relu') %>% # Add dense layer
  layer_dropout(rate = 0.5) %>%          # Regularization
  layer_dense(units = length(train_generator$class_indices), 
              activation = 'softmax')  # Output layer for classes

# Compile the model
model %>% compile(
  optimizer = optimizer_adam(),
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

# Train the model
history <- model %>% fit(
  train_generator,
  steps_per_epoch = train_generator$samples %/% 32,
  epochs = 10, # Adjust epochs as needed
  validation_data = validation_generator,
  validation_steps = validation_generator$samples %/% 32
)

# Save and plot the training history
png("Results/VGG16_plot1.png", width = 800, height = 600)
plot(history)
dev.off()

# Evaluate on validation set
results <- model %>% evaluate(validation_generator)
print(results)
#Accuracy --> 0.2203636
#Loss --> 1.6180818



