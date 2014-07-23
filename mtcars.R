setwd("~/Github/RegressData_Assessment")

library(knitr)
data(mtcars)

mtcars2 <- within(mtcars, {
    # cyl <- as.factor(cyl)
    am <- factor(am, labels = c("automatic", "manual"))
    vs <- factor(vs, labels = c("V engine", "I engine"))
    # gear <- as.factor(gear)
    # carb <- as.factor(carb)
})

summary(mtcars2)

boxplot(mpg ~ am, data = mtcars2)
stripchart(mpg ~ am, data = mtcars2, vertical = T, method = "jitter",
           pch =20, add = T)

pairs(mtcars)
pairs(mtcars[, 1:6])
pairs(mtcars[, c(1,7:11)])

fit1 <- lm(mpg ~ am, data = mtcars2)
summary(fit1)$coef

fit2 <- lm(mpg ~ ., data = mtcars2)
summary(fit2)
fit3 <- lm(mpg ~ disp + hp + drat + wt + am + gear, data = mtcars2)
summary(fit3)
fit5 <- lm(mpg ~ hp + drat + wt + am, data = mtcars2)
summary(fit5)
fit4 <- lm(mpg ~ hp + wt + am, data = mtcars2)
summary(fit4)
a <- anova(fit4, fit3)
anova(fit4, fit5)

op <- par(mfrow = c(2,2), mar = c(4,2,2,1))
plot(fit4)
par(op)

outliers <- c("Chrysler Imperial", "Fiat 128", "Toyota Corolla")
mtcars3 <- mtcars2[!is.element(rownames(mtcars2), outliers),]
fit5 <- lm(mpg ~ hp + wt + am, data = mtcars3)

op <- par(mfrow = c(2,2), mar = c(4,2,2,1))
plot(fit5)
par(op)

fit5_sum <- summary(fit5)
