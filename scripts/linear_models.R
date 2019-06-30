# install.packages("MASS")
library(MASS)
data(Boston)
dim(Boston)
head(Boston)
str(Boston)
distinctvals <- lapply(Boston, unique)

lengthdistinctvals <- lapply(distinctvals, length)

lengthdistinctvals <- unlist(lengthdistinctvals)
lengthdistinctvals
barplot(table(Boston$chas))

barplot(table(Boston$rad))
Boston$chas <- as.character(Boston$chas)
Boston$rad <- as.character(Boston$rad)
summary(Boston$medv)
hist(Boston$medv, breaks = 30)

plot(sort(Boston$medv), main = "medv in ascending order", ylab = "medv")
grid()
plot(Boston$crim, Boston$medv, main = "crim vs. medv", xlab = "crim", ylab = "medv")
grid()
cor(Boston$crim, Boston$medv)

plot(Boston$rm, Boston$medv, main = "rm vs. medv", xlab = "rm", ylab = "medv")
grid()
cor(Boston$rm, Boston$medv)
correlations <- cor(subset(Boston, select = -c(chas, rad)))
correlations

# install.packages("corrplot")
library(corrplot)

corrplot(correlations, method = "ellipse", type = "full", title = "Correlations", diag = TRUE, mar = c(2, 2, 2, 2))

corrplot(correlations, method = "ellipse", type = "upper", title = "Correlations", diag = FALSE, mar = c(2, 2, 2, 2), addCoef.col = "black", number.cex = 0.7)
model0 <- lm(medv ~ ., data = Boston)
summary(model0)
# Adj R2 = 0.7396
# par(mfrow = c(2,2))
plot(model0)
# par(mfrow = c(1,1))
model1 <- lm(medv ~ . -indus -age, data = Boston)
summary(model1)
# Adj R2 = 0.7405
anova(model1, model0)
model2 <- lm(medv ~ . -indus -age -rad, data = Boston)
summary(model2)
# Adj R2 = 0.7234
model3 <- lm(medv ~ . -indus -age -rad -tax, data = Boston)
summary(model3)
# Adj R2 = 0.7239
fwd <- stepAIC(model0, direction = "forward", trace = TRUE)

fwd$anova
bkwd <- stepAIC(model0, direction = "backward", trace = TRUE)

bkwd$anova
# install.packages("leaps")
library(leaps)

exhaustive <- regsubsets(medv ~ ., data = Boston, nbest = 1)
pred <- predict(model1, newdata = Boston)
plot(Boston$medv, pred, main = "Actual vs. Predicted from Linear Regression", xlab = "medv", ylab = "pred")
abline(0,1)
grid()
model4 <- lm(medv ~ . +rm:lstat +poly(lstat, 10), data = Boston)
summary(model4)
# Adj R2 = 0.8129
model5 <- lm(medv ~ . -zn -indus -age -rad +rm:lstat +poly(lstat, 5), data = Boston)
summary(model5)
# Adj R2 = 0.7991
model6 <- lm(medv ~ . -zn -indus -age -rad +rm:lstat +poly(lstat, 5) -tax, data = Boston)
summary(model6)
# Adj R2 = 0.7993
# install.packages("randomForest")
library(randomForest)

rf <- randomForest(medv ~ ., data = Boston)
plot(rf)
pred.rf <- predict(rf, Boston)
plot(Boston$medv, pred.rf, main = "Actual vs. Predicted from Random Forest", xlab = "medv", ylab = "pred")
abline(0,1)
grid()
cor(Boston$medv, pred.rf)^2
plot(Boston$medv, pred, main = "Actual vs. Predicted from Linear Regression", xlab = "medv", ylab = "pred")
abline(0,1)
grid()
cor(Boston$medv, pred)^2
