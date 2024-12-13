#DAN:
#1) You need a readme
#2) This doc is titles "main" but it appears to be day 6? I don't see a main. R that actually sources the other scripts. 
#3) The code is a bit sloppy in many places, with scripts containing content they are not labelled to contain, etc. 
#4) Youve done the work, though, and gotten some decent results. Grade, S.


#Main script for Neural Network assignment 
#Name: Anahi Quezada
#Date: 11/27/2024

#Day 6 


#Structure 
#1. Libraries
#2. Setting the working directory 
#3. Preparing the data 
#4. Model VGG16 
#5. Compiling the model 
#6. Final model execution
#7. Checking the model with raw data
#7.1 Single pictures
#7.2 Creating a loop with raw images

#1. Libraries 
library(keras)
library(tensorflow)
library(recolorize)
library(jpeg)
library(png)
library(EBImage)
library(imager)
library(gridExtra)
library(ggplot2)

#2. Working directory 
setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitNN/")
path <- ("Data")

#3. Preparing the data 
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
#4. Model VGG16 
# Load the VGG16 model as a base (feature extractor)
base_model <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,  
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

#5. Compiling the model 

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

#6. Final model execution

# Define the final model
model_vgg16 <- keras_model(inputs = base_model$input, outputs = x)

results_vgg16 <- build_and_train_model("vgg16", model_vgg16, train_generator, 
                                       validation_generator)
#7. Results 
results_df <- data.frame(
  Model = ("VGG16"),
  Loss = (results_vgg16$results[1]),
  Accuracy = (results_vgg16$results[2])
)
print(results_df)
#Again the results 
#Loss --> 0.5923406
#Accuracy -->0.7790179

#7. Checking the model with raw data
#-----Checking my model 
#7.1 Single pictures
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
#third path --> The predicted class for the image is: dandelion

#7.2 Creating a loop with raw images
# Loop through the images and classify each one
mixpath <- "Data/mix"
image_files <- list.files(path = mixpath, pattern = "\\.(jpg|png|jpeg)$",
                          full.names = TRUE)
x11()
output_file <- "Results/results_plot.png"
png(output_file, width = 800, height = 800)
par(mfrow = c(7, 7), mar = c(1, 1, 1, 1))
for (mixpath in image_files) {
  img <- load.image(mixpath)  
  plot(img)  
}
for (mixpath in image_files) {
  predicted_class <- predict_flower_class(mixpath, model_vgg16, 
                                          train_generator)
  
  picture_sample <- load.image(mixpath)
  plot(picture_sample, main= paste(":",
                                   predicted_class))
  cat("The predicted class for", mixpath, "is:", predicted_class, "\n")
}
dev.off()
#End
