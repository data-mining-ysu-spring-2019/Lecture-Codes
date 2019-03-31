
x1 <- c(1,2,3)
x2 <- c(2,4,6)
x3 <- c(4,7,9)

A <- matrix( c(x1,x2,x3), nrow = 3, byrow= TRUE)
A
At<-t(A)
AtA <- At %*% A
qr(A)$rank
qr(AtA)$rank 
library(matlib)
inv(AtA) # Error: X is numerically singular
# Creating close to singulare matrix
x4<-c(1,2,3.5)
B <- matrix( c(x4,x2,x3), nrow = 3, byrow= TRUE)
B
qr(B)$rank
Bt<-t(B)
BtB <- Bt %*% B
inv(BtB)
qr(BtB)$rank
set.seed(2708)
C <- matrix( rnorm(9), nrow = 3, byrow= TRUE)
C
qr(C)$rank
Ct<-t(C)
CtC <- Ct %*% C
inv(CtC)
y <- c(8,10,15)
df <- data.frame(x1,x2,x3,y)
lm(y~., data = df)
x <- c( 2,5, 0.5,0.5,4,5,6,4)
y <- c( 1,1.5, 2,0.5,1,1,1.5,2)

type <- factor(c(0,0,1,1,1,1,1,1), levels = c(0,1), labels = c("train", "test"))

df <-data.frame(x,y, type)
df.test <- df[3:8,]
df.train<-df[1:2,]

cf1 <- coef(lm(y~x, data = df.train))
cf2 <- coef(lm(y~x, data = df.test))

ggplot(df, aes(x=x, y=y, color =type))+
  geom_point(alpha=0.4, size = 5)+
  geom_abline(slope = cf1[2], intercept = cf1[1], size=1, col ="coral")+labs(color = "")


ggplot(df, aes(x=x, y=y, color =type))+
  geom_point(alpha=0.4, size = 5)+
  geom_abline(slope = cf1[2], intercept = cf1[1], size=1, col ="coral",linetype = 2)+
  geom_abline(slope = cf2[2], intercept = cf2[1], size=1, col ="lightblue")+labs(color = "")

ggplot(df, aes(x=x, y=y, color =type))+
  geom_point(alpha=0.4, size = 5)+
  geom_abline(slope = cf1[2]-0.1, intercept = cf1[1], size=1, col ="coral",linetype = 2)+
  geom_abline(slope = cf2[2], intercept = cf2[1], size=2, col ="lightblue")+labs(color = "")


library(gridExtra)

g1 <- ggplot()+geom_abline(slope = 1, intercept =0, size=1, col ="coral",linetype = 1)+
  scale_y_continuous(limits=c(0,4))+
  scale_x_continuous(limits=c(0,4))

g2 <- ggplot()+geom_abline(slope = 2, intercept =0, size=1, col ="lightblue",linetype = 1)+
  scale_y_continuous(limits=c(0,4))+
  scale_x_continuous(limits=c(0,4))
g3 <- ggplot()+geom_abline(slope = 0.5, intercept =0, size=1, col ="lightgreen",linetype = 1)+
  scale_y_continuous(limits=c(0,4))+
  scale_x_continuous(limits=c(0,4))

grid.arrange(g1, g2, g3,nrow=1)
# to calculate
xsam <- c(1,1,2) #15, 90/6
ysam <- c(10,20,30)
library(tidyverse)
library(caret)
library(glmnet)

# Load the data
data("Boston", package = "MASS")
# Split the data into training and test set
set.seed(123)
library(dplyr)
training.samples <- Boston$medv %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]
# Predictor variables
x <- model.matrix(medv~., train.data)[,-1]
head(x)
# Outcome variable
y <- train.data$medv
library(glmnet)
m1<-glmnet(x, y, alpha = 0, lambda = 0)
m1$a0
m1$beta
?Boston
lreg <- lm(y~.-medv, data =train.data)
car::vif(lreg)
ridge_tune_by_hand <- glmnet(x, y, alpha = 0, lambda = 0:1000)
coef <- as.matrix(ridge_tune_by_hand$beta)
head(coef[, 2:5])
plot(ridge_tune_by_hand, label = TRUE)
plot (ridge_tune_by_hand, xvar = "lambda")
# Find the best lambda using cross-validation
set.seed(123) 
cv <- cv.glmnet(x, y, alpha = 0)
plot(cv)
# Display the best lambda value
cv$lambda.min
cv$lambda.1se
# Fit the final model on the training data

model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)
# Make predictions on the test data
x.test <- model.matrix(medv ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)
lasso1 <- glmnet(x,y, lambda = seq(0, 10, by = 0.1), alpha = 1)
plot(lasso1, xvar = "lambda")
plot(lasso1, xvar = "dev")
#print(lasso1)[1:5,]
# Find the best lambda using cross-validation
set.seed(123) 
cvl <- cv.glmnet(x, y, alpha = 1)
plot(cvl)
# Display the best lambda value
cvl$lambda.min
cvl$lambda.1se
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, lambda = cvl$lambda.min)
# Dsiplay regression coefficients
coef(model)

# Make predictions on the test data
x.test <- model.matrix(medv ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)
# Build the model using the training set
set.seed(123)
model <- train(
  medv ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Best tuning parameter
model$bestTune
# Coefficient of the final model. You need
# to specify the best lambda
coef(model$finalModel, model$bestTune$lambda)
# Make predictions on the test data
x.test <- model.matrix(medv ~., test.data)[,-1]
predictions <- model %>% predict(x.test)
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)

cvel <- cv.glmnet(x,y, alpha =0.5)
elnet <- glmnet(x,y, alpha = 0.5, lambda =1:25)
par( mfrow = c(3,2))

plot(cv)
plot(ridge_tune_by_hand, xvar = "lambda")
plot(cvl)
plot(lasso1, xvar = "lambda")
plot(cvel)
plot(elnet, xvar = "lambda")
