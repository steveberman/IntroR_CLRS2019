##### Load data ----------------------------------------------------------------------------------------------

diamonds <- read.csv("C:/Users/scott.sobel/Documents/Docs/CAS/2019 CLRS/diamonds.csv", stringsAsFactors = FALSE)

dim(diamonds)

head(diamonds)


##### Prep data ----------------------------------------------------------------------------------------------

str(diamonds)

diamonds$cut <- as.factor(diamonds$cut)
diamonds$color <- as.factor(diamonds$color)
diamonds$clarity <- as.factor(diamonds$clarity)

str(diamonds)


##### Split into training vs. test ---------------------------------------------------------------------------

indexes <- 1:nrow(diamonds)

set.seed(42)
trn.idx <- sample(indexes, size = nrow(diamonds) * 0.70)
head(trn.idx)

tst.idx <- indexes[!(indexes %in% trn.idx)]
head(tst.idx)

train <- diamonds[trn.idx, ]
test <- diamonds[tst.idx, ]


##### Linear Regression --------------------------------------------------------------------------------------

model.lm <- lm(price ~  ., data = train)

summary(model.lm)
# Adj R2 = 0.9202

plot(model.lm)

par(mfrow = c(2,2))
plot(model.lm)
par(mfrow = c(1,1))

pred.train.lm <- predict(model.lm, train)
pred.test.lm <- predict(model.lm, test)

plot(train$price, pred.train.lm, pch = 20, col = rgb(0,0,0,0.1))
abline(0,1)

(cor.train.lm <- cor(train$price, pred.train.lm))
# 0.959302

plot(test$price, pred.test.lm, pch = 20, col = rgb(0,0,0,0.1))
abline(0,1)

(cor.test.lm <- cor(test$price, pred.test.lm))
# 0.9584838

(rmse.train.lm <- sqrt(mean((train$price - pred.train.lm)^2)))
# 1123.775

(rmse.test.lm <- sqrt(mean((test$price - pred.test.lm)^2)))
# 1144.133


##### Generalized Linear Regression --------------------------------------------------------------------------

hist(diamonds$price, breaks = 50)
hist(log(diamonds$price), breaks = 50)

model.lm2 <- lm(I(log(price)) ~  ., data = train)

summary(model.lm2)
# Adj R2 = 0.9719

plot(model.lm2)

par(mfrow = c(2,2))
plot(model.lm2)
par(mfrow = c(1,1))

pred.train.lm2 <- predict(model.lm2, train)
pred.train.lm2 <- exp(pred.train.lm2)
pred.test.lm2 <- predict(model.lm2, test)
pred.test.lm2 <- exp(pred.test.lm2)

plot(train$price, pred.train.lm2, pch = 20, col = rgb(0,0,0,0.1))
abline(0,1)

(cor.train.lm2 <- cor(train$price, pred.train.lm2))
# 0.9686457

plot(test$price, pred.test.lm2, pch = 20, col = rgb(0,0,0,0.1))
abline(0,1)

(cor.test.lm2 <- cor(test$price, pred.test.lm2))
# 0.8836964

(rmse.train.lm2 <- sqrt(mean((train$price - pred.train.lm2)^2)))
# 1038.167

(rmse.test.lm2 <- sqrt(mean((test$price - pred.test.lm2)^2)))
# 2128.293


##### Random Forest ------------------------------------------------------------------------------------------

install.packages("randomForest")
library("randomForest")

set.seed(42)
model.rf <- randomForest(price ~ ., data = train, ntree = 10, do.trace = TRUE)

model.rf
# % Var explained = 97.08%

plot(model.rf)

pred.train.rf <- predict(model.rf, train)
pred.test.rf <- predict(model.rf, test)

plot(train$price, pred.train.rf, pch = 20, col = rgb(0,0,0,0.1))
abline(0,1)

(cor.train.rf <- cor(train$price, pred.train.rf))
# 0.9970156

plot(test$price, pred.test.rf, pch = 20, col = rgb(0,0,0,0.1))
abline(0,1)

(cor.test.rf <- cor(test$price, pred.test.rf))
# 0.9876107

(rmse.train.rf <- sqrt(mean((train$price - pred.train.rf)^2)))
# 309.39

(rmse.test.rf <- sqrt(mean((test$price - pred.test.rf)^2)))
# 631.4559


##### Ranger -------------------------------------------------------------------------------------------------

install.packages("ranger")
library("ranger")

set.seed(42)
model.rngr <- ranger(price ~ ., data = train, num.trees = 500, verbose = TRUE)

model.rngr
# R2 = 0.97785

pred.train.rngr <- predict(model.rngr, train)$predictions
pred.test.rngr <- predict(model.rngr, test)$predictions

plot(train$price, pred.train.rngr, pch = 20, col = rgb(0,0,0,0.1))
abline(0,1)

(cor.train.rngr <- cor(train$price, pred.train.rngr))
# 0.9975425

plot(test$price, pred.test.rngr, pch = 20, col = rgb(0,0,0,0.1))
abline(0,1)

(cor.test.rngr <- cor(test$price, pred.test.rngr))
# 0.9881632

(rmse.train.rngr <- sqrt(mean((train$price - pred.train.rngr)^2)))
# 281.4372

(rmse.test.rngr <- sqrt(mean((test$price - pred.test.rngr)^2)))
# 617.4125


##### Compare ------------------------------------------------------------------------------------------------

barplot(c(rmse.train.lm, rmse.test.lm, rmse.train.lm2, rmse.test.lm2, rmse.train.rf, rmse.test.rf,
          rmse.train.rngr, rmse.test.rngr),
        names.arg = c("train.lm","test.lm","train.lm2","test.lm2","train.rf","test.rf","train.rngr","test.rngr"),
        main = "Compare RMSE Results", ylab = "RMSE", col = "light blue")
grid()


##### END CODE ###############################################################################################