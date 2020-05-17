# get data from web
iris_dat <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE)
colnames(iris_dat) <- c("sepal_length", "sepal_width", "petal_length", "petal_width", "species")

head(iris_dat)

library(nnet)

set.seed(123)
nall = nrow(iris_dat)
ntrain = 75
nvalid = 75
index = seq(1:nall)
train_index = sample(index, ntrain)
valid_index = index[-train_index]

train_data <- iris_dat[train_index, ]
valid_data <- iris_dat[valid_index, ]


# build regression model
model <- multinom(species ~ sepal_length + sepal_width + petal_length + petal_width, data = train_data)

summary(model)

# determine model accuracy
predicted_vals <- predict(model, valid_data)
head(predicted_vals)

observed_species <- valid_data$species

accuracy <- mean(observed_species == predicted_vals)
accuracy

# confusion matrix
table(observed_species, predicted_vals)

# get coefficients and exponentiate to find odds ratios
coefs <- coef(model)
coefs
odds <- exp(coefs)

# show odds
odds

anova(model)
