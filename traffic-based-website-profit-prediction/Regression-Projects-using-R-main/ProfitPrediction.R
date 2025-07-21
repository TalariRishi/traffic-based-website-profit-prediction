############# PROFIT PREDICTION USING LINEAR REGRESSION ################

# Loading the data set
library("readxl")
data <- read_excel("revenue.xlsx")
data

# Lets see the summary of the data set
summary(data)

# Lets see the first six rows
head(data)

# Lets visualize the data
plot(data)

# Splitting the data into train and test data

# Load necessary library
library(caTools)

# Set seed for reproducibility
set.seed(2)

# Split the data: 70% for training and 30% for testing
split <- sample.split(data$Profit, SplitRatio = 0.7)

# Create training and testing datasets
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)


# Model training
model <- lm(Profit~.,data = train)

# Lets see the model summary
summary(model)

# Prediction
pred <- predict(model,test)
pred

# Predicted vs Actual
plot(test$Profit,type = "l",lty=1.8,col="red") # Test data
lines(pred,type="l",col="blue") # Predicted value vs actual
plot(pred,type="l",lty=1.8,col="blue") # Predicted

# Finding the Accuracy
rmse <- sqrt(mean(pred-data$Profit)^2)
rmse


