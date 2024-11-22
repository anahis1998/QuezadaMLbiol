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
library(readxl)
library(keras)
library(tensorflow)
install.packages("recolorize")
library(recolorize)
install.packages("jpeg")  
install.packages("png")   
library(jpeg)
library(png)

setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitNN/")
path <- ("Data")
#fotos <- list.files(path="/Data/train",
         #           pattern = "/.(jpg|jpeg|png)$", full.names = TRUE)

train<- file.path(path, "train")
val<- file.path(path, "validation")

#training data
daisy_t <- file.path(train, "daisy")
dandelion_t <- file.path(train, "dandelion")
roses_t <- file.path(train, "roses")
sunflower_t <- file.path(train, "sunflowers")
tulips_t <- file.path(train, "tulips")

#evaluation data
daisy_v <- file.path(val, "daisy")
dandelion_v <- file.path(val, "dandelion")
roses_v <- file.path(val, "roses")
sunflower_v <- file.path(val, "sunflowers")
tulips_v <- file.path(val, "tulips")

list.files(dandelion_t)
list.files(dandelion_v)

train_datagen <- image_data_generator(
  rescale = 1/255,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE
)
validation_datagen <- image_data_generator(rescale = 1/255)

train_generator <- flow_images_from_directory(
  "Data/train",
  train_datagen,
  target_size = c(150, 150),
  batch_size = 32,
  class_mode = "categorical"
)

validation_generator <- flow_images_from_directory(
  "Data/validation",
  validation_datagen,
  target_size = c(150, 150),
  batch_size = 32,
  class_mode = "categorical"
)
#creating the model
base_model <- application_vgg16(weights = "imagenet", include_top = FALSE, 
                                input_shape = c(150, 150, 3))

model <- keras_model_sequential() %>%
  base_model %>%
  layer_global_average_pooling_2d() %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = length(train_generator$class_indices), 
              activation = 'softmax')

model %>% compile(
  optimizer = optimizer_adam(),
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)
#trainnig the model
history <- model %>% fit(
  train_generator,
  steps_per_epoch = train_generator$samples %/% 32,
  epochs = 10,
  validation_data = validation_generator,
  validation_steps = validation_generator$samples %/% 32
)
#save image 
plot(history)
png("Results/training_history_plot1.png", width = 800, height = 600)
plot(history)  
dev.off()

model %>% evaluate(validation_generator)
