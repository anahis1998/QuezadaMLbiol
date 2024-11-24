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

# Load the VGG16 model as a base (feature extractor)
base_model <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,  # Exclude the top fully connected layers
  input_shape = c(150, 150, 3)
)

# Freeze the layers of the base model to retain pretrained weights
freeze_weights(base_model)

# Build the custom model using the functional API
x <- base_model$output %>%
  layer_global_average_pooling_2d() %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = length(train_generator$class_indices),
              activation = 'softmax')

# Define the final model
model_vgg16 <- keras_model(inputs = base_model$input, outputs = x)
results_vgg16 <- build_and_train_model("vgg16", model_vgg16, train_generator, 
                                       validation_generator)

# Model 2: Custom CNN
model_custom <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = 'relu', 
                input_shape = c(150, 150, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = length(train_generator$class_indices), 
              activation = 'softmax')

results_custom <- build_and_train_model("custom_cnn", model_custom, 
                                        train_generator, validation_generator)

# Model 3: Pre-trained ResNet50
# Load the ResNet50 model as a base (feature extractor)
base_model_resnet50 <- application_resnet50(
  weights = "imagenet",
  include_top = FALSE,  # Exclude the top fully connected layers
  input_shape = c(150, 150, 3)
)

# Freeze the layers of the base model to retain pretrained weights
freeze_weights(base_model_resnet50)

# Build the custom model using the functional API
x_resnet50 <- base_model_resnet50$output %>%
  layer_global_average_pooling_2d() %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = length(train_generator$class_indices), 
              activation = 'softmax')

# Define the final model
model_resnet50 <- keras_model(inputs = base_model_resnet50$input, 
                              outputs = x_resnet50)

results_resnet50 <- build_and_train_model("resnet50", model_resnet50, 
                                        train_generator, validation_generator)
# Compare and save results
results_df <- data.frame(
  Model = c("VGG16", "Custom CNN", "ResNet50"),
  Loss = c(results_vgg16$results[1], results_custom$results[1],
           results_resnet50$results[1]),
  Accuracy = c(results_vgg16$results[2], results_custom$results[2],
               results_resnet50$results[2])
)

print(results_df)
#Mode VGG16 is the best model. According to the highest accuracy (0.7709091) and
#the lowest loss function (0.6193798). 

# Save model performance as a CSV
write.csv(results_df, "Results/model_comparison_results.csv", row.names = FALSE)

# Save plots for each model
png("Results/vgg16.png", width = 800, height = 600)
plot(results_vgg16$history)
dev.off()

png("Results/custom_cnn.png", width = 800, height = 600)
plot(results_custom$history)
dev.off()

png("Results/resnet50.png", width = 800, height = 600)
plot(results_resnet50$history)
dev.off()
