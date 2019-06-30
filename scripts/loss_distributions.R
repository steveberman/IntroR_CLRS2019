source('common.R')
set.seed(8910)
sims <- 5e3

scale <- 250
shape <- 8
severity <- rgamma(sims, shape, scale = scale)

summary(severity)
data.frame(x = severity) %>% 
  ggplot(aes(x)) + 
  geom_histogram()
log_like_gamma <- function(x, shape, scale) {
  n <- length(x)
  log_like <- sum(x) / scale + n * shape * log(scale) + n * log(gamma(shape))
  log_like <- (shape - 1) * sum(log(x)) - log_like
  log_like
}
log_like_gamma(severity, c(0.5, 2), c(500, 1000))
tbl_log_like <- data.frame(
  shape = seq(0.05, 10, length.out = 500)
)

tbl_log_like <- tbl_log_like %>% 
  mutate(
      scale = mean(severity) / shape
    , log_like = log_like_gamma(severity, shape, scale)
  )
tbl_log_like %>% 
  ggplot(aes(shape, log_like)) + 
  geom_line()
tbl_log_like <- data.frame(
  scale = seq(0.5 * min(severity), max(severity), length.out = 500)
)

tbl_log_like <- tbl_log_like %>% 
  mutate(
      shape = mean(severity) / scale
    , log_like = log_like_gamma(severity, shape, scale)
  )
tbl_log_like %>% 
  ggplot(aes(scale, log_like)) + 
  geom_line()
tbl_log_like <- expand.grid(
    scale = seq(0.5 * min(severity), max(severity), length.out = 250)
  , shape = seq(0.05, 10, length.out = 250)
) %>% 
  mutate(
    log_like = log_like_gamma(severity, shape, scale)
  )
tbl_log_like %>% 
  ggplot(aes(scale, shape)) + 
  geom_raster(aes(fill = log_like), interpolate = TRUE) +
  scale_fill_continuous(low = 'red', high = 'green')
library(MASS)

fitGamma <- fitdistr(severity, "gamma")
fitLognormal <- fitdistr(severity, "lognormal")
fitWeibull <- fitdistr(severity, "Weibull")

fitGamma
fitLognormal
fitWeibull
## probabilities = seq_len(sims)/(sims + 1)
## 
## weibullQ <- qweibull(probabilities, coef(fitWeibull)[1], coef(fitWeibull)[2])
## lnQ <- qlnorm(probabilities, coef(fitLognormal)[1], coef(fitLognormal)[2])
## gammaQ <- qgamma(probabilities, coef(fitGamma)[1], coef(fitGamma)[2])
## 
## sampleLogMean <- fitLognormal$estimate[1]
## sampleLogSd <- fitLognormal$estimate[2]
## 
## sampleShape <- fitGamma$estimate[1]
## sampleRate <- fitGamma$estimate[2]
## 
## sampleShapeW <- fitWeibull$estimate[1]
## sampleScaleW <- fitWeibull$estimate[2]
## 
## sortedSeverity <- sort(severity)
## oldPar <- par(mfrow = c(1,3))
## plot(sort(weibullQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Weibull Fit")
## abline(0,1)
## 
## plot(sort(lnQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Lognormal Fit")
## abline(0,1)
## 
## plot(sort(gammaQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Gamma Fit")
## abline(0,1)
## 
## par(oldPar)
probabilities = seq_len(sims)/(sims + 1)

weibullQ <- qweibull(probabilities, coef(fitWeibull)[1], coef(fitWeibull)[2])
lnQ <- qlnorm(probabilities, coef(fitLognormal)[1], coef(fitLognormal)[2])
gammaQ <- qgamma(probabilities, coef(fitGamma)[1], coef(fitGamma)[2])

sampleLogMean <- fitLognormal$estimate[1]
sampleLogSd <- fitLognormal$estimate[2]

sampleShape <- fitGamma$estimate[1]
sampleRate <- fitGamma$estimate[2]

sampleShapeW <- fitWeibull$estimate[1]
sampleScaleW <- fitWeibull$estimate[2]

sortedSeverity <- sort(severity)
oldPar <- par(mfrow = c(1,3))
plot(sort(weibullQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Weibull Fit")
abline(0,1)

plot(sort(lnQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Lognormal Fit")
abline(0,1)

plot(sort(gammaQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Gamma Fit")
abline(0,1)

par(oldPar)
## x <- seq(0, max(severity), length.out=500)
## yLN <- dlnorm(x, sampleLogMean, sampleLogSd)
## yGamma <- dgamma(x, sampleShape, sampleRate)
## yWeibull <- dweibull(x, sampleShapeW, sampleScaleW)
## 
## hist(severity, freq=FALSE, ylim=range(yLN, yGamma))
## 
## lines(x, yLN, col="blue")
## lines(x, yGamma, col="red")
## lines(x, yWeibull, col="green")
x <- seq(0, max(severity), length.out=500)
yLN <- dlnorm(x, sampleLogMean, sampleLogSd)
yGamma <- dgamma(x, sampleShape, sampleRate)
yWeibull <- dweibull(x, sampleShapeW, sampleScaleW)

hist(severity, freq=FALSE, ylim=range(yLN, yGamma))

lines(x, yLN, col = "blue")
lines(x, yGamma, col = "red")
lines(x, yWeibull, col = "green")
## sampleCumul <- seq(1, length(severity)) / length(severity)
## stepSample  <- stepfun(sortedSeverity, c(0, sampleCumul), f = 0)
## yGamma <- pgamma(sortedSeverity, sampleShape, sampleRate)
## yWeibull <- pweibull(sortedSeverity, sampleShapeW, sampleScaleW)
## yLN <- plnorm(sortedSeverity, sampleLogMean, sampleLogSd)
## 
## plot(stepSample, col = "black", main = "K-S Gamma")
## lines(sortedSeverity, yGamma, col = "blue")
## 
## plot(stepSample, col = "black", main = "K-S Weibull")
## lines(sortedSeverity, yWeibull, col = "blue")
## 
## plot(stepSample, col = "black", main = "K-S Lognormal")
## lines(sortedSeverity, yLN, col = "blue")
sampleCumul <- seq(1, length(severity)) / length(severity)
stepSample  <- stepfun(sortedSeverity, c(0, sampleCumul), f = 0)
yGamma <- pgamma(sortedSeverity, sampleShape, sampleRate)
yWeibull <- pweibull(sortedSeverity, sampleShapeW, sampleScaleW)
yLN <- plnorm(sortedSeverity, sampleLogMean, sampleLogSd)

plot(stepSample, col = "black", main = "K-S Gamma")
lines(sortedSeverity, yGamma, col = "blue")
plot(stepSample, col = "black", main = "K-S Weibull")
lines(sortedSeverity, yWeibull, col = "blue")
plot(stepSample, col = "black", main = "K-S Lognormal")
lines(sortedSeverity, yLN, col = "blue")
testGamma <- ks.test(severity, "pgamma", sampleShape, sampleRate)
testLN <- ks.test(severity, "plnorm", sampleLogMean, sampleLogSd)
testWeibull <- ks.test(severity, "pweibull", sampleShapeW, sampleScaleW)

testGamma
testLN
testWeibull
