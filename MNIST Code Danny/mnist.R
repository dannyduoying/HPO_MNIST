## 20230114 Building a function that takes input
## Learning_rate
## Epochs
## Batch_size
## Dropout
## Validation_split
## Hidden_layers
## Units
## And generate an output

library(keras)
library(tensorflow)

# Data Preparation ---------------------------------------------------

# The data, shuffled and split between train and test sets
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# Reshape
dim(x_train) <- c(nrow(x_train), 784)
dim(x_test) <- c(nrow(x_test), 784)

# Transform RGB values into [0,1] range
x_train <- x_train / 255
x_test <- x_test / 255

# Convert class vectors to binary class matrices
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

FLAGS <- flags(
  flag_numeric("dropout1", 0.4),
  flag_numeric("dropout2", 0.3)
)

# Define Fuction --------------------------------------------------------------
## Input 
## learning_rate, double (0.001)
## dropout, double (0.25)
## epochs, integer
## batch_size, integer
## validation_split, double
## hidden_layers, integer
## units, integer

## Note only doing 2 dimensional learning rate and dropout right now
NN_optimize <- function(x) 
{
  learning_rate <- x[1]
  dropout <- x[2]
  
  ## First round the integers
  epochs <- 20
  batch_size <- 128
  hidden_layers <- 1
  units <- 2^8
  validation_split <- 0.2
  
  ## Then build model
  model <- keras_model_sequential()
  model %>% ## using units, dropout and hidden layer
    layer_dense(units = units, activation = 'relu', input_shape = c(784)) %>%
    layer_dropout(rate = dropout) %>%
    layer_dense(units = units, activation = 'relu') %>%
    layer_dropout(rate = dropout) %>%
    layer_dense(units = 10, activation = 'softmax')
  
  model %>% compile( ## using learning rate
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(learning_rate = learning_rate),
    metrics = c('accuracy')
  )
  
  ## Next train and evaluate
  history <- model %>% fit( ## using batch_size and epochs and validation split
    x_train, y_train,
    batch_size = batch_size,
    epochs = epochs,
    verbose = 1,
    validation_split = validation_split
  )
  
  score <- model %>% evaluate(
    x_test, y_test,
    verbose = 0
  )
  
  return(-score[2]) 
}
