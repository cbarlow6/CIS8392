install.packages("hashFunction")

library(tidyverse)
library(keras)
library(hashFunction)

samples <- c("The cat sat on the mat.",
            "The dog ate my homework")

# Creates a tokenizer, configured to only take into 
# account the 1,000 most common words
tokenizer <- text_tokenizer(num_words = 1000) %>%
  fit_text_tokenizer(samples) # Builds the word index

# Turns strings into lists of integer indices
sequences <- texts_to_sequences(tokenizer, samples)
one_hot_results <- texts_to_matrix(tokenizer, samples, mode = "binary") 
word_index <- tokenizer$word_index
cat("Found", length(word_index), "unique tokens.\n")


aclImdb_dir <- "~/gsu/CIS8392/aclImdb" # change this to the right folder 
train_dir <- file.path(aclImdb_dir, "train")
labels <- c()
texts <- c()
for (label_type in c("neg", "pos")) {
  label <- switch(label_type, neg = 0, pos = 1)
  dir_name <- file.path(train_dir, label_type)
  for (fname in list.files(dir_name, pattern = glob2rx("*.txt"),
                           full.names = TRUE)) {
    texts <- c(texts, readChar(fname, file.info(fname)$size))
    labels <- c(labels, label) }
}

max_words <- 10000
maxlen <- 200
training_samples <- 200
validation_samples <- 5000
tokenizer <- text_tokenizer(num_words = max_words) %>%
  fit_text_tokenizer(texts)
sequences <- texts_to_sequences(tokenizer, texts) 
word_index = tokenizer$word_index
cat("Found", length(word_index), "unique tokens.\n")

glove_dir = "~/gsu/CIS8392/" # the folder contianing glove.6B.50d.txt 
lines <- readLines(file.path(glove_dir, "glove.6B.50d.txt"))
embeddings_index <- new.env(hash = TRUE, parent = emptyenv())
for (i in 1:length(lines)) {
  line <- lines[[i]]
  values <- strsplit(line, " ")[[1]]
  word <- values[[1]]
  embeddings_index[[word]] <- as.double(values[-1])
}
cat("Found", length(embeddings_index), "word vectors.\n")

embedding_dim <- 100
embedding_matrix <- array(0, dim = c(max_words, embedding_dim)) # 10k x 50 
for (word in names(word_index)) { # for every word
  index <- word_index[[word]] # get its index
  if (index < max_words) { # only consider the top 10k words
  # get the word's embedding vector from glove
  embedding_vector <- embeddings_index[[word]]
  if (!is.null(embedding_vector)) { # if glove has the embedding vector
    # index 1 isn't supposed to stand for any word or token 
    # --it's a placeholder. So we skip 1 here: 
    embedding_matrix[index+1,] <- embedding_vector
  } 
  }
}

#RNN

timesteps <- 100 # Number of timesteps in the input sequence 
input_features <- 32 # Dimensionality of the input feature space 
output_features <- 64 # Dimensionality of the output feature space
random_array <- function(dim) { 
  array(runif(prod(dim)), dim = dim)
}
# Input data: some random numbers for the sake of the example
inputs <- random_array(dim = c(timesteps, input_features))
state_t <- rep_len(0, length = c(output_features)) # Initial state: an all-zero vector
# Creates random weight matrices
W <- random_array(dim = c(output_features, input_features)) 
U <- random_array(dim = c(output_features, output_features)) 
b <- random_array(dim = c(output_features, 1))

output_sequence <- array(0, dim = c(timesteps, output_features)) 
for (i in 1:nrow(inputs)) {
  input_t <- inputs[i,] # input_t is a vector of shape (input_features).
  # Combines the input with the current state (the previous output)
  # to obtain the current output
  output_t <- tanh(as.numeric((W %*% input_t) + (U %*% state_t) + b)) 
  output_sequence[i,] <- as.numeric(output_t) # Updates the result matrix 
  state_t <- output_t # Updates the state of the network for the next timestep
}

model <- keras_model_sequential() %>%
  layer_embedding(input_dim = 10000, output_dim = 32) %>% 
  layer_simple_rnn(units = 32)
summary(model)

max_features <- 10000
maxlen <- 500
batch_size <- 32
imdb <- dataset_imdb(num_words = max_features) 
c(c(input_train, y_train), c(input_test, y_test)) %<-% imdb
cat(length(input_train), "train sequences\n")

cat(length(input_test), "test sequences")

cat("Pad sequences (samples x time)\n")

input_train <- pad_sequences(input_train, maxlen = maxlen) 
input_test <- pad_sequences(input_test, maxlen = maxlen) 
cat("input_train shape:", dim(input_train), "\n")

max_features <- 10000 # Number of words to consider as features 
maxlen <- 200 # Cuts off the text after this number of words

imdb <- dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb # Loads the data

# Turns the lists of integers into a 2D tensor of shape (samples, maxlen)
x_train <- pad_sequences(x_train, maxlen = maxlen) 
x_test <- pad_sequences(x_test, maxlen = maxlen)

#bidirectional LSTM
model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>%
  bidirectional(
    layer_lstm(units = 32)
  ) %>%  
    layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy", 
  metrics = c("acc")
)

history <- model %>% fit( x_train, y_train, 
                          epochs = 10,
                          batch_size = 128, 
                          validation_split = 0.2
)
plot(history) 

#LSTM
model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>% 
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy", metrics = c("acc")
)

history_lstm <- model %>% fit( input_train, y_train, epochs = 10,
                          batch_size = 128, validation_split = 0.2
)

#library(tidyverse)
write_rds(history, "my_history.rds")
history_new

max_features <- 10000
max_len <- 500
imdb <- dataset_imdb(num_words = max_features) 
c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb 
cat(length(x_train), "train sequences\n")

#Combining CNNs and RNNs to process long sequences
model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_features, output_dim = 128,
                    input_length = max_len) %>%
  layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu",
                input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_max_pooling_1d(pool_size = 3) %>%
  layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>% 
  layer_lstm(units = 32) %>%
  layer_dense(units = 1)

summary(model)

model %>% compile(
  optimizer = optimizer_rmsprop(lr = 1e-4), 
  loss = "binary_crossentropy",
  metrics = c("acc")
)

history <- model %>% fit( x_train, y_train, 
                          epochs = 10,
                          batch_size = 128, 
                          validation_split = 0.2
)

model <- keras_model_sequential() %>% layer_embedding(input_dim = max_features, output_dim = 128,
                                                      input_length = max_len) %>%
  layer_dense(units = 1) summary(model)
layer_conv_1d(filters = 32, kernel_size = 7, activation = "relu") %>% layer_max_pooling_1d(pool_size = 5) %>%
  layer_conv_1d(filters = 32, kernel_size = 7, activation = "relu") %>% layer_global_max_pooling_1