### File: modeling.R
### Purpose: provide an introduction to modeling in R
### by Scott Sobel
### modified by Steve Berman
### created 13Aug2019

PATH <- "C:/Users/spadhya/Documents/GitHub/IntroR_CLRS2019"

###############################################
### data load 
###############################################
diamonds <- read.csv(paste0(PATH, "/diamonds.csv"), stringsAsFactors = FALSE)

# basic data review
dim(diamonds)
head(diamonds)

# variable X is just an index - drop this
diamonds$X <- NULL

# levels are Ideal, Premium, Very Good, Good, Fair
unique(diamonds$cut)

# Info on clarity:
# * Flawless (FL) No inclusions and no blemishes visible under 10x magnification
# * Internally Flawless (IF) No inclusions visible under 10x magnification
# * Very, Very Slightly Included (VVS1 and VVS2) Inclusions so slight they are 
#     difficult for a skilled grader to see under 10x magnification
# * Very Slightly Included (VS1 and VS2) Inclusions are observed with effort 
#     under 10x magnification, but can be characterized as minor
# * Slightly Included (SI1 and SI2) Inclusions are noticeable under 10x magnification
# * Included (I1, I2, and I3) Inclusions are obvious under 10x magnification
#     which may affect transparency and brilliance

# color: ranges from D (best) to J (worst)

# x = length in mm
# y = width in mm
# z = depth in mm

###############################################
##### Prep data
###############################################

str(diamonds)

# turn into factorsw
diamonds$cut <- as.factor(diamonds$cut)
diamonds$color <- as.factor(diamonds$color)
diamonds$clarity <- as.factor(diamonds$clarity)

str(diamonds)


###############################################
##### Split into training vs. test using random sampling
###############################################

indexes <- 1:nrow(diamonds)

set.seed(42)
trn.idx <- sample(indexes, size = nrow(diamonds) * 0.70)
head(trn.idx)

tst.idx <- indexes[!(indexes %in% trn.idx)]
head(tst.idx)

train <- diamonds[trn.idx, ]
test <- diamonds[tst.idx, ]

print(paste0("Train size=",nrow(train)))
print(paste0("Test size=",nrow(test)))

###############################################
##### Modeling: Linear Regression 
###############################################

### throw all variables into the model
model.lm <- lm(price ~  ., data = train)

# show model parameters, significance.  Each factor is a variable
summary(model.lm)
# Adj R2 = 0.9202

# show plots in a 2x2 grid
par(mfrow = c(2,2))
plot(model.lm)
par(mfrow = c(1,1))

# results:
# Residuals vs. fitted - do residuals have non-linear patterns?
# Normal Q-Q - should follow a straight line
# Scale-location - check if residuals spread evenly, test of variance
# Residuals vs. Leverage - find influential outliers

pred.train.lm <- predict(model.lm, train)
pred.test.lm <- predict(model.lm, test)

# show actual vs. predictions of price on training data
plot(train$price, pred.train.lm, pch = 20, col = rgb(0,0,0,0.1))
abline(0, 1, col="red", lty="dashed")

# get correlation on train
(cor.train.lm <- cor(train$price, pred.train.lm))
# 0.959302

# do same for test - this is the real validation of the model
plot(test$price, pred.test.lm, pch = 20, col = rgb(0,0,0,0.1))
abline(0, 1, col="red", lty="dashed")

(cor.test.lm <- cor(test$price, pred.test.lm))
# 0.9584838

# mean squared error

(rmse.train.lm <- sqrt(mean((train$price - pred.train.lm)^2)))
# 1123.775

(rmse.test.lm <- sqrt(mean((test$price - pred.test.lm)^2)))
# 1144.133


###############################################
##### Modeling: Linear Regression with transformed target 
###############################################

hist(diamonds$price, breaks = 50)
hist(log(diamonds$price), breaks = 50)

model.lm2 <- lm(log(price) ~  ., data = train)
summary(model.lm2)
# Adj R2 = 0.9719

# much better!
par(mfrow = c(2,2))
plot(model.lm2)
par(mfrow = c(1,1))

# predictions are log(price) so bring back to dollars
pred.train.lm2 <- exp(predict(model.lm2, train))
pred.test.lm2 <- exp(predict(model.lm2, test))

plot(train$price, pred.train.lm2, 
     pch = 20, col = rgb(0,0,0,0.1))
abline(0,1, col="red", lty="dashed")

(cor.train.lm2 <- cor(train$price, pred.train.lm2))
# 0.9686457

plot(test$price, pred.test.lm2, 
     pch = 20, col = rgb(0,0,0,0.1))
abline(0,1, col="red", lty="dashed")

(cor.test.lm2 <- cor(test$price, pred.test.lm2))
# 0.8836964

(rmse.train.lm2 <- sqrt(mean((train$price - pred.train.lm2)^2)))
# 1038.167

(rmse.test.lm2 <- sqrt(mean((test$price - pred.test.lm2)^2)))
# 2128.293


###############################################
##### Modeling: Decision Tree 
###############################################

library(rpart)
library(rpart.plot)
library(rattle)

set.seed(42)
model.tree <- rpart(price ~ ., 
                    data = train, method = "anova",
                    maxdepth=3)

model.tree
# % Var explained = 97.08%

#rpart.plot(model.tree, shadow.col = "gray")
# top number is price, n = count and percent, split values below
fancyRpartPlot(model.tree)

pred.train.tree <- predict(model.tree, train)
pred.test.tree <- predict(model.tree, test)

plot(train$price, pred.train.tree, pch = 20, col = rgb(0,0,0,0.1))
abline(0,1)

(cor.train.tree <- cor(train$price, pred.train.tree))
# 0.9389457

plot(test$price, pred.test.tree, pch = 20, col = rgb(0,0,0,0.1))
abline(0,1)

(cor.test.tree <- cor(test$price, pred.test.tree))
# 0.9361821

(rmse.train.tree <- sqrt(mean((train$price - pred.train.tree)^2)))
# 1369.25

(rmse.test.tree <- sqrt(mean((test$price - pred.test.tree)^2)))
# 1410.474


###############################################
##### Random Forest 
###############################################

library("randomForest")

set.seed(42)
model.rf <- randomForest(price ~ ., data = train, 
                         ntree = 10, do.trace = TRUE)

model.rf
# % Var explained = 97.08%

# error reduced with increased tree count
plot(model.rf)

# this is part of randomForest
varImpPlot(model.rf, sort = TRUE, n.var = min(30, nrow(model.rf$importance)))

par(mfrow = c(3,2))
# partial dependence plots also in randomForest
# gives marginal effect of a variable on the response here
partialPlot(model.rf, train, "carat", n.pt = 50, rug = TRUE)
partialPlot(model.rf, train, "y", n.pt = 50, rug = TRUE)
partialPlot(model.rf, train, "z", n.pt = 50, rug = TRUE)
partialPlot(model.rf, train, "x", n.pt = 50, rug = TRUE)
partialPlot(model.rf, train, "clarity", n.pt = 50, rug = TRUE)
partialPlot(model.rf, train, "color", n.pt = 50, rug = TRUE)
par(mfrow = c(1,1))


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


###############################################
##### Compare 
###############################################

RMSEs <- c(rmse.train.lm, rmse.test.lm, 
           rmse.train.lm2, rmse.test.lm2, 
           rmse.train.tree, rmse.test.tree,
           rmse.train.rf, rmse.test.rf)

labels <- c("train.lm","test.lm",
            "train.lm2","test.lm2",
            "train.tree","test.tree",
            "train.rf","test.rf")

barplot(RMSEs, names.arg = labels, main = "Compare RMSE Results", 
        ylab = "RMSE", col = "light blue")


##### END CODE ##################################