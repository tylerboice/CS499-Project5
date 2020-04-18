# Install packages if they are not already installed
if(!require("data.table"))
{
	install.packages("data.table")
}
if(!require("ggplot2"))
{
  install.packages("ggplot2")
}
if(!require("tensorflow"))
{
  install.packages("tensorflow")
}
if(!require("keras"))
{
  install.packages("keras")
}

# Load the installed packages
library(data.table)
library(ggplot2)
library(tensorflow)
library(keras)

# Download spam data set to local directory, if it is not present
if(!file.exists("spam.data"))
{
	download.file("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data", "spam.data")
}

# Read spam data set and convert to input matrix and scale inputs
spam.dt <- data.table::fread("spam.data")
N.obs <- nrow(spam.dt)
X.raw <- as.matrix(spam.dt[, -ncol(spam.dt), with=FALSE])
y.vec <- spam.dt[[ncol(spam.dt)]]
X.mat <- scale(X.raw)

# Divide data into 80% training data and 20% testing data
is.train <- sample(1 : nrow(X.mat), .8 * nrow(X.mat), replace = F)
is.test <- setdiff(1:nrow(X.mat), is.train)

# Divide train into a 50% training data and 50% validation data
is.subtrain <- sample(1 : (length(is.train)), (.5 * length(is.train)), replace = F)
validation <- setdiff(1:length(is.train), is.subtrain)
X.subtrain.mat <- X.mat[is.subtrain, ]
y.subtrain.vec <- y.vec[is.subtrain]

# Define a for loop over regularization parameter values, 
# and fit a neural network for each.
# X axis = # hidden units in a network with 1 hidden layer

# On the same plot, show the logistic loss as a function of the
# regularization parameter (use a different color for each set, e.g.
# subtrain=solid, validation=dashed). Draw a point to emphasize the minimum
# of each validation loss curve. As the strength of regularization decreases,
# the train loss should always decrease, whereas the validation loss should
# decrease up to a certain point, and then start increasing (overfitting).

hidden.units.vec <- 2^seq(1,10)

for( counter in seq_along(hidden.units.vec) )
{
	num.hidden <- hidden.units.vec[[counter]]
	model <- keras_model_sequential() %>%
		layer_dense( #hidden layer
			input_shape = ncol(X.subtrain.mat),
			units = num.hidden,
			activation = "sigmoid",
			use_bias=FALSE
		) %>%
		layer_dense(1, activation = "sigmoid", use_bias=FALSE) #output layer
	model %>%
		compile(
			loss = "binary_crossentropy",
			optimizer = "sgd",
			metrics = "accuracy"
		)
	result <- model %>%
		fit(
			x = X.subtrain.mat, y = y.subtrain.vec,
			epochs = 100,
			validation_split = 0.3,
			verbose = 2
		)
	print(plot(result))
}

# TODO: Define a variable called best_parameter_value which is the regularization 
#       parameter value which minimizes the validation loss.
best_parameter_value <- min(val_loss)

# TODO: Re-train the network on the entire train set (not just the subtrain set),
#       using the corresponding value of best_parameter_value.

# TODO: Finally use the learned model to make predictions on the test set.
#       What is the prediction accuracy? (percent correctly predicted labels 
#       in the test set) What is the prediction accuracy of the baseline model 
#       which predicts the most frequent class in the train labels?
y.tab <- table(is.train)
y.pred <- as.integer(names(y.tab[which.max(y.tab)]))
accuracy <- mean(is.subtrain == y.pred)
