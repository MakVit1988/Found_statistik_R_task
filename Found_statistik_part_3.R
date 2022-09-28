# Линейнаяя регресия. Трансформацияя

fit1 <- lm(mpg~hp, mtcars)
fit2 <- lm(mpg~I(-hp^-0.7), mtcars)

summary(fit1)
summary(fit2)

qplot(x=log(hp), y= log(mpg), data = mtcars)

fit3 <- lm(log(mpg)~log(hp), mtcars)
summary(fit3)

y1<- 120*log(100)+3
y2<- 120*log(110)+3
y1-y2

hist(fit3$residuals)

# Задачи первой части
# Задача 1
hetero_test <-  function(test_data){
  fit <- lm(test_data[,1]~., data=test_data[,-1])
  fit_uait_test <- lm((fit$residuals)^2 ~ ., data = test_data[, c(-1), drop = F])
  summary(fit_uait_test)$r.squared 
  
}