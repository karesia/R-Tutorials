# LINEAR REGRESSION

# Read data
y <-c(84.33, 87.80, 82.20, 78.21, 78.44, 80.01, 83.53, 79.46, 75.22, 76.58, 77.90, 78.80, 80.67, 86.60, 78.20)
x <-c(603.40, 582.50, 556.20, 594.60, 558.90, 575.20, 580.10, 451.20, 404.00, 484.00, 452.40, 448.40, 334.80, 320.30, 350.30)

# Simple Linear Regression model
lrtest <-lm(y ~ x)
summary(lrtest)

# Diagnostic plots to check assumptions
# Errors are normally distributed
# Errors have the same variance
# Observations and errors are independent

x11()
split.screen(c(2,2))
# Check Normality using qqplot
screen(1)
qqnorm(lrtest$residuals, main = "1. Normality", pch=20)
qqline(lrtest$residuals)

# Check Homogeneity of Variances
screen(2)
plot(lrtest$fitted.values, lrtest$residuals, main = "2. Homogeneity of Variances",
xlab="Fitted values", ylab="Residuals", pch=20)
abline(h=0, lty=2)

# Check that observations are Independent
screen(3)
plot(1:15, lrtest$residuals, main = "3. Independence", 
xlab="Obs number (time order)", ylab="Residuals", pch =20)
abline(h=0, lty=2)

# Correlation test
cor.test(x, y, method="pearson")

# Scatterplot
plot(x, y, main="Scatterplot", pch=20)

# Fitting a quadratic regression model of the form y = alpha + beta1 x1 + beta2 x2
# where x2 = x^2
x2 <- x^2

quad <-lm(y ~ x + x2)
summary(quad)

# Diagnostic plots to check assumptions for new model
x11()
split.screen(c(2,2))
# Check Normality using qqplot
screen(1)
qqnorm(quad$residuals, main = "1. Normality", pch=20)
qqline(quad$residuals)

# Check Homogeneity of Variances
screen(2)
plot(quad$fitted.values, quad$residuals, main = "2. Homogeneity of Variances",
xlab="Fitted values", ylab="Residuals", pch=20)
abline(h=0, lty=2)

# Check that observations are Independent
screen(3)
plot(1:15, quad$residuals, main = "3. Independence", 
xlab="Obs number (time order)", ylab="Residuals", pch =20)
abline(h=0, lty=2)