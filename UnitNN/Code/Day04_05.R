#Day 4 and day 5

#In this code, I'm going to include more arguments to improve the development 
#of my previous code. Since, I'm working with images, I can play more with 
#arguments such as rotation, brightness, zoom, color, size, and resize. 

#Structure 

#1. Libraries 
library(keras)
library(tensorflow)
library(ggplot2)
library(recolorize)
library(jpeg)
library(png)
install.packages("BiocManager")
BiocManager::install("EBImage")
library(EBImage)
library(imager)

setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitNN/")
path <- ("Data")

train<- file.path(path, "train")
val<- file.path(path, "evaluation")

# Data Augmentation for training data
train_data<- image_data_generator(
  rescale = 1/255,            # Normalize pixel values
  shear_range = 0.2,          # Shear transformations
  zoom_range = 0.2,           # Random zoom
  horizontal_flip = TRUE ,     # Flip images horizontally
  rotation_range = 30,       # Randomly rotate images by up to 30 degrees
  width_shift_range = 0.2,   # Shift images horizontally by 20% of width
  height_shift_range = 0.2,  # Shift images vertically by 20% of height
  brightness_range = c(0.8, 1.2)  # Randomly adjust brightness
)

optimizer <- optimizer_adam(learning_rate = 0.0001)  # Reduced learning rate

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

# Load the VGG16 model as a base (feature extractor)
base_model <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,  # Exclude the top fully connected layers
  input_shape = c(150, 150, 3)
)

# Freeze the layers of the base model to retain pretrained weights
#freeze_weights(base_model)
unfreeze_weights(base_model, from = "block5_conv1")  # Unfreeze last convolutional block
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

#-------STOP THE PREVIOUS PROCESS 
#NOT GOOD FITTING TO THE DATA ACCOI=RDING TO THE GRAPH AND VALUES IN THE CONSOLE
#This model is not getting better. Since I'm looking through the graph and the 
#values from loss and accuracy. I think the model is over fitting or 
#overfitting. 
#Coming back to my code from code day 02_03, to perform a example of the 
#development of this code and check the outcomes. 
#After read a little bit of literature, maybe adding more arguments will 
#perform better my model but I do not have enough data to train well the model. 

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
#training data=2295 images belonging to 5 classes

#prepare evaluation data
validation_generator <- flow_images_from_directory(
  val,
  validation_data,
  target_size = c(150, 150),
  batch_size = 32,
  class_mode = "categorical"
)

# Load the VGG16 model as a base (feature extractor)
base_model <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,  # Exclude the top fully connected layers
  input_shape = c(150, 150, 3)
)
#evaluation data=1375 images belonging to 5 classes

# Freeze the layers of the base model to retain pretrained weights
freeze_weights(base_model)

# Build the custom model using the functional API
x <- base_model$output %>%
  layer_global_average_pooling_2d() %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = length(train_generator$class_indices),
              activation = 'softmax')

# Define the build_and_train_model function
build_and_train_model <- function(model_name, model, train_generator, 
                                  validation_generator, epochs = 20,
                                  learning_rate = 0.001) {
  # Compile the model
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(learning_rate = learning_rate),
    metrics = c("accuracy")
  )
  
  # Train the model
  history <- model %>% fit(
    train_generator,
    steps_per_epoch = train_generator$n %/% train_generator$batch_size,
    epochs = epochs,
    validation_data = validation_generator,
    validation_steps = validation_generator$n %/% validation_generator$batch_size
  )
  
  # Get final evaluation metrics
  final_results <- model %>% evaluate(
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


# Define the final model
model_vgg16 <- keras_model(inputs = base_model$input, outputs = x)

results_vgg16 <- build_and_train_model("vgg16", model_vgg16, train_generator, 
                                       validation_generator)

results_df <- data.frame(
  Model = ("VGG16"),
  Loss = (results_vgg16$results[1]),
  Accuracy = (results_vgg16$results[2])
)
print(results_df)
#Again the results 
#Loss --> 0.5878066
#Accuracy -->0.7790179

#-----Checking my model 
imagen1 <- "Data/evaluation/sunflowers/9825716455_f12bcc8d4e_n.jpg"
img <- load.image(imagen1)
plot(img)

imagen1 <- "Data/evaluation/tulips/13903937027_44b9f2f5b8.jpg"
imagen1 <- "Data/evaluation/dandelion/16096748028_7876887ab2.jpg"
#arguments 
target_size = c(224, 224)

load_and_preprocess_image <- function(imagen1, target_size = c(150, 150)) {
  img <- image_load(imagen1, target_size = target_size)  # Load the image
  img_array <- image_to_array(img)  # Convert to array
  img_array <- array_reshape(img_array, c(1, target_size[1], target_size[2], 3)) 
  # Reshape for the model
  img_array <- img_array / 255  # Normalize the image
  return(img_array)
}
# Function to predict the class of the image
predict_flower_class <- function(imagen1, model, train_generator) {
  img_array <- load_and_preprocess_image(imagen1)  # Preprocess the image
  prediction <- model %>% predict(img_array)  # Get prediction
  # Get the index of the class with the highest probability
  predicted_class_index <- which.max(prediction)
  
  # Get the corresponding class label
  class_labels <- names(train_generator$class_indices)
  predicted_class <- class_labels[predicted_class_index]
  
  return(predicted_class)
}
predicted_class <- predict_flower_class(imagen1, model_vgg16, train_generator)
cat("The predicted class for the image is:", predicted_class, "\n")
#I think it works. Now. I'm going to create a loop with  more images. Just 
#to check 

#first path --> The predicted class for the image is: sunflowers 
#second path--> The predicted class for the image is: tulips 
#The predicted class for the image is: dandelion