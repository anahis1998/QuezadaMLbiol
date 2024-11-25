#Day 4 and day 5

#In this code, I'm going to include more arguments to improve the development 
#of my previous code. Since, I'm working with images, I can play more with 
#arguments such as rotation, brightness, zoom, color, size, and resize. 

#Structure 

#1. Libraries 
library(keras)
library(tensorflow)
library(ggplot2)
 
#Data 
setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitNN/")
path <- "Data"
train <- file.path(path, "train")
val <- file.path(path, "evaluation")

#preprocessing 
train_data <- image_data_generator(
  rescale = 1/255,
  rotation_range = 30,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  brightness_range = c(0.8, 1.2),
  shear_range = 0.2,
  zoom_range = 0.3,
  horizontal_flip = TRUE
)
validation_data <- image_data_generator(rescale = 1/255) 
#train data
train_generator <- flow_images_from_directory(
  train,
  train_data,
  target_size = c(224, 224),
  batch_size = 32,
  class_mode = "categorical"
)
#validation data 
val_generator <- flow_images_from_directory(
  val,
  validation_data,
  target_size = c(224, 224),
  batch_size = 32,
  class_mode = "categorical"
)

#Including Efficient Net to increase model depth, width and resolution
base_model <- tf$keras$applications$EfficientNetB0(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(224L, 224L, 3L)
)
# Freeze base model layers
freeze_weights(base_model)

# Build the model
model <- keras_model_sequential() %>%
  base_model %>%
  layer_global_average_pooling_2d() %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = length(train_generator$class_indices), 
              activation = "softmax")

# Compile the model
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# Add a learning rate scheduler and model checkpoint callback
callbacks <- list(
  callback_reduce_lr_on_plateau(
    monitor = "val_loss",
    factor = 0.5,
    patience = 3,
    verbose = 1
  ),
  callback_model_checkpoint(
    filepath = "best_model.h5",
    monitor = "val_accuracy",
    save_best_only = TRUE
  )
)

# Train the model
history <- model %>% fit(
  train_generator,
  epochs = 20,
  validation_data = val_generator,
  callbacks = callbacks
)

# Evaluate the model
evaluation <- model %>% evaluate(val_generator)

# Print results
cat("Validation Accuracy: ", evaluation[["accuracy"]], "\n")
#Validation Accuracy:  0.2203636 
cat("Validation Loss: ", evaluation[["loss"]], "\n")
#Validation Loss:  1.616201 

# Plot training history
plot(history)
