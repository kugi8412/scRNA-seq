library(ggplot2)
library(readr)
library(RColorBrewer)
library(qqplotr)
library(nortest)
library(caret)
library(parallel)
library(parallelMap)
library(glmnet)
library(dplyr)
library(tidyr)
library(tibble)
library(ggdist)
library(randomForest)
library(ranger)
set.seed(123)

# Load the data as number matrices
X_train <- as.matrix(read.delim("X_train.csv", sep = ',', dec = '.'))
X_test <- as.matrix(read.delim("X_test.csv", sep = ',', dec = '.'))
y_train <- read.delim("y_train.csv", sep = ',', dec = '.')
y <- y_train$CD36

# Check data type
is_integer_column <- function(column) {
  all(column == as.integer(column))
}
unique(unlist(lapply(X_train, class)))
integer_columns_X_train <- apply(X_train, 2, is_integer_column)
which(integer_columns_X_train)
unique(unlist(lapply(X_test, class)))
integer_columns_X_test <- apply(X_test, 2, is_integer_column)
which(integer_columns_X_test)
str(y_train)

# Check missing values in data
sum(is.na(X_train), is.null(X_train))
sum(is.na(X_test), is.null(X_test))
sum(is.na(y_train), is.null(y_train))

# Check data dimension
dim(X_train)
dim(X_test)
dim(y_train)

# Basic statistics
summary(y_train)
hist(y_train$CD36, breaks = "Sturges", main = "Histogram of the explained variable",
     xlab = "y value")
ggplot(data = data.frame(y_train$CD36), aes(x = y_train$CD36)) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Density estimator of the explained variable",
       x = "y value", y = "density")

# Create a corerlation matrix for 250 features
corelation_matrix <- cor(X_train, y_train$CD36)
top_values <- order(abs(corelation_matrix[, 1]), decreasing = TRUE)[1:250]
selected_values <- X_train[, top_values]
selected_order <- cor(selected_values, selected_values[, 1])
selected_order <- selected_order[order(selected_order, decreasing = TRUE),]
selected_order <- names(selected_order)
corelation_selected <- cor(X_train[, selected_order])
# Create a heatmap
heatmap(corelation_selected, Rowv = NA, Colv = NA, scale = "none",
        col=colorRampPalette(brewer.pal(8, "Spectral"))(20))
legend(x = "bottomright", legend=c("(-1.0, -0.75)", "(-0.75, -0.50)",
                                   "(-0.50, -0.25)", "(-0.25, 0.00)",
                                   "(0.00, 0.25)", "(0.25, 0.50)",
                                   "(0.50, 0.75)", "(0.75, 1.00)"),
       fill=colorRampPalette(brewer.pal(8, "Spectral"))(8), cex = 0.8)

# Quantile diagram
ggplot(y_train, aes(sample = y)) +
  stat_qq_band(distribution = "norm", alpha = 0.2) +
  stat_qq_line(distribution = "norm", col = "red", size = 2) +
  stat_qq_point(distribution = "norm") +
  labs(title = "QQ Plot of explained variable vs. normal distribution",
       x="Theoretical Quantiles",
       y="Sample Quantiles") +
  theme_minimal()

# Statistical test for the explained variable
ad.test(y)

# Test for variable most closely correlated with the explained variable
most_corelated_variable <- 
  names(corelation_matrix[, 1])[which.max(abs(corelation_matrix[, 1]))]
most_corelated <- X_train[, most_corelated_variable]
data <- data.frame(CD36 = y, BLVRB = most_corelated)
hist(data$BLVRB, breaks="Sturges", main="Histogram of most corelated variable",
xlab="BLVRB")
lambda <- 4000
ks.test(data$BLVRB, "pexp", rate = lambda)
shapiro.exp.test<-function(x, nrepl=2000) {
  DNAME <- deparse(substitute(x))
  l<-0
  n<-length(x)
  x<-sort(x)
  y<-mean(x)
  w<-n*(y-x[1])^2/((n-1)*sum((x-y)^2))
  for(i in 1:nrepl) {
    s<-rexp(n)
    s<-sort(s)
    y<-mean(s)
    W<-n*(y-s[1])^2/((n-1)*sum((s-y)^2))
    if (W<w) l=l+1
  }
  p.value<-l/nrepl
  RVAL<-list(statistic=c(W=w), p.value=p.value,
  method="Shapiro-Wilk test for exponentiality",
  data.name = DNAME)
  class(RVAL)<-"htest"
  return(RVAL)
}
shapiro.exp.test(data$BLVRB)

# ElasticNet
alphas <- seq(0, 1, by=0.1)
lambdas <- seq(1, 0, by=-0.1)
k <- 10
folds <- createFolds(y, k)
parallelStartSocket(cpus=detectCores())
en_train_errors <- matrix(NA, nrow = length(alphas), ncol = length(lambdas))
en_validation_errors <- matrix(NA, nrow = length(alphas), ncol = length(lambdas))

for (i in 1:length(alphas)) {
  train_errors <- c(1:length(lambdas))
  validation_errors <- c(1:length(lambdas))
  
  for (fold in 1:k) {
    train_index <- unlist(folds[-fold])
    validation_index <- folds[[fold]]
    
    x_train_fold <- X_train[train_index, ]
    x_validation_fold <- X_train[validation_index, ]
    y_train_fold <- y[train_index]
    y_validation_fold <- y[validation_index]
    
    en_model <- glmnet(x_train_fold, y_train_fold, alpha = alphas[i], lambda = lambdas)
    pred <- predict(en_model, X_train, s = lambdas)
    
    # predictions
    train_pred <- pred[train_index, ]
    validation_pred <- pred[validation_index, ]
    
    # RMSE
    for (j in 1:length(lambdas)) {
      train_errors[j] <- sqrt(mean((train_pred[j] - y_train_fold)^2))
      validation_errors[j] <- sqrt(mean((validation_pred[j] - y_validation_fold)^2))
    }
  }
  en_train_errors[i, ] <- train_errors
  en_validation_errors[i, ] <- validation_errors
}

rownames(en_train_errors) <- alphas
colnames(en_train_errors) <- lambdas
rownames(en_validation_errors) <- alphas
colnames(en_validation_errors) <- lambdas
parallelStop()

# Violin plot
en_validation_errors_violin <- as.data.frame(en_validation_errors)
rownames(en_validation_errors_violin) <- alphas
colnames(en_validation_errors_violin) <- lambdas
en_validation_errors_violin <- en_validation_errors_violin %>%
  rownames_to_column(var = "alphas") %>%
  pivot_longer(-alphas, names_to = "lambdas", values_to = "mse") %>%
  mutate(alphas = as.numeric(alphas), lambdas = as.factor(lambdas))

ggplot(en_validation_errors_violin, aes(x = alphas, y = mse, color = lambdas)) +
  geom_violin(fill = "lightblue", color = "red", draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(size = 2, width = 0.1) +  # MSE
  scale_color_manual(values=c("#00B358", "#0969A2", "#74E600", "#FF9473", "#A62800",
                              "#FF8C00", "#FFC073", "#A65B00", "#CFA127", "#454416",
                              "#000000")) +
  labs(
    title = "Violin plot for Elastic Net",
    x = "Alphas",
    y = "Mean square error (MSE)",
    color = "Lambdas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ElasticNet model
index <- which(en_validation_errors == min(en_validation_errors), arr.ind = TRUE)
alphas[index[1]]
lambdas[index[2]]
en_train_error <- en_train_errors[index]
en_train_error <- en_train_error[1]
en_validation_error <- en_validation_errors[index]
en_validation_error <- en_validation_error[1]
en_train_error
en_validation_error

# Random forest
rf_hyperparams_grid <- expand.grid(
  mtry = floor(ncol(X_train) * c(.01, .1, .2, .333)),
  min.node.size = c(5, 10, 15, 20),
  max.depth = c(10, 25, 50, NULL),
  train_rmse = NA,
  validation_rmse = NA
)
parallelStartSocket(cpus=detectCores())
for(i in seq_len(nrow(rf_hyperparams_grid))) {
  train_errors <- c(1:k)
  validation_errors <- c(1:k)
  
  for (fold in 1:k) {
    train_index <- unlist(folds[-fold])
    validation_index <- folds[[fold]]
    
    x_train_fold <- X_train[train_index, ]
    y_train_fold <- y[train_index]
    x_validation_fold <- X_train[validation_index, ]
    y_validation_fold <- y[validation_index]
    
    rf_model <- ranger(
      x = x_train_fold,
      y = y_train_fold,
      seed = 20,
      verbose = FALSE,
      num.trees = 100,
      mtry = rf_hyperparams_grid$mtry[i],
      min.node.size = rf_hyperparams_grid$min.node.size[i],
      max.depth = rf_hyperparams_grid$max.depth[i]
    )

    validation_pred <- predict(rf_model, x_validation_fold)$predictions
    train_errors[fold] <- sqrt(model$prediction.error)
    validation_errors[fold] <- sqrt(mean((validation_pred - y_validation_fold)^2))  
  }
  
  rf_hyperparams_grid$train_rmse[i] <- mean(train_errors)
  rf_hyperparams_grid$validation_rmse[i] <- mean(validation_errors)
}

parallelStop()

# Box plot
rf_hyperparams_grid_boxplot <- rf_hyperparams_grid
rf_hyperparams_grid_boxplot$max.depth <- as.factor(rf_hyperparams_grid_boxplot$max.depth)
rf_hyperparams_grid_boxplot$mtry <- as.factor(rf_hyperparams_grid_boxplot$mtry)
ggplot(rf_hyperparams_grid_boxplot, aes(x = min.node.size, y = validation_rmse)) +
  geom_boxplot(aes(group = min.node.size), alpha = 0.5, outlier.shape = NA,
               linetype = "solid", varwidth = TRUE, color = "black") +
  geom_jitter(aes(color = factor(max.depth), shape = factor(mtry)), 
              position = position_jitter(width = 0.2), size = 3, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Box plot for RandomForest",
    x = "min.node.size",
    y = "validation_rmse"
  ) +
  scale_color_brewer(palette = "Set3", name = "max.depth") +
  scale_shape_manual(values = c(15, 16, 17, 18), name = "mtry") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Random forest model
index <- which.min(rf_hyperparams_grid$validation_rmse)
rf_hyperparams_grid[index, ]
rf_best_params <- rf_hyperparams_grid[index, 1:3]
rf_train_error <- rf_hyperparams_grid$train_rmse[index]
rf_train_error <- rf_train_error[1]
rf_validation_error <- rf_hyperparams_grid$validation_rmse[index]
rf_validation_error <- rf_validation_error[1]
rf_train_error
rf_validation_error

# Summary
train_errors <- c(1:k)
validation_errors <- c(1:k)

# Reference model
reference_y <- mean(y)

for (fold in 1:k) {
  train_index <- unlist(folds[-fold])
  validation_index <- folds[[fold]]
  
  x_train_fold <- X_train[train_index, ]
  y_train_fold <- y[train_index]
  x_validation_fold <- X_train[validation_index, ]
  y_validation_fold <- y[validation_index]
  
  train_errors[fold] <- sqrt(mean((reference_y - y_train_fold)^2)) 
  validation_errors[fold] <- sqrt(mean((reference_y - y_validation_fold)^2)) 
}

index <- which.min(validation_errors)
ref_train_error <- train_errors[index]
ref_train_error <- ref_train_error[1]
ref_validation_error <- validation_errors[index]
ref_validation_error <- ref_validation_error[1]

# Models comparison
comparison <- matrix(c(ref_train_error, ref_validation_error, en_train_error,
                       en_validation_error, rf_train_error, rf_validation_error),
                     nrow=2)
colnames(comparison) <- c("Reference", "ElasticNet", "RandomForest")
rownames(comparison) <- c("training", "validation")
comparison
