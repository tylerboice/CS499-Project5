# install packages if they are not already installed
if(!require("data.table"))
{
	install.packages("data.table")
}
if(!require("ggplot2"))
{
  install.packages("ggplot2")
}

# load the installed packages
library(data.table)
library(ggplot2)

# download spam data set to local directory, if it is not present
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

