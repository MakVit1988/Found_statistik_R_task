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

# Задача 2
VIF <- function(test_data) {
  
  test_df<-data.frame(test_data[,-1])
  
  VIF_df<-numeric(0)
  
  for (i in 1:ncol(test_df)) {
    
    model<-lm(test_df[,i]~as.matrix(test_df[,-i]), data=test_df)
    
    VIF_test<-1/(1-summary(model)$r.squared)
    
    VIF_df<-c(VIF_df,VIF_test)
    
    names(VIF_df)[i]<-names(test_df)[i] }
  
  return(VIF_df) }

# Задача 3
VIF <- function(data) {
  X = data[-1]
  vifs = unlist(lapply(colnames(X), function(c) summary(lm(as.matrix(X[c]) ~ as.matrix(X[colnames(X) != c])))$r.squared))
  vifs = 1/(1-vifs)
  names(vifs) = colnames(X)
  
  vifs
}

smart_model <- function(data) {
  d = data
  
  while(TRUE) {
    vifs = VIF(d)
    
    if (max(vifs) > 10) {
      worst_x = which.max(vifs)
      d = d[-(worst_x + 1)]
      
      if (ncol(d) == 2) {
        break;
      }
    } else {
      break;
    }
  }
  
  lm(d)$coefficients
}

# Задача 4

test_data <- as.data.frame(list(y = c(9.83, 8.68, 8.43, 9.78, 9.9, 9.18, 10.38, 10.37, 10.74, 10.32, 
                                      9.6, 9.04, 9.94, 11.72, 10.04, 8.58, 9.33, 11.58, 10.64, 10.26),
                                x = c(9258.44, 2937.4, 2296.99, 8817.83, 9935.54, 4829.49, 16094.77, 
                                      15910.81, 23104.82, 15116.96, 7388.04, 4208.42, 10338.87, 61353.07, 
                                      11422.53, 2650.55, 5627.92, 53710.28, 20795.77, 14278.86)))


transform_x <- function(data) {
  vec_neg <- seq(-2, -0.1, by = 0.1)
  vec_pos <- seq(0.1, 2, by = 0.1)
  new_vars <- do.call(rbind, c(lapply(vec_neg, function(i) -(data$x ^ i)),
                               lapply(vec_pos, function(i) data$x ^ i)))
  cor_var_zero <- cor.test( ~ y + log(data$x), data)$estimate
  cor <- apply(new_vars, 1, function(i) cor.test( ~ y + i, data)$estimate)
  n <- which.max(abs(c(cor, cor_var_zero)))
  if (n <= 40) {
    result <- new_vars[n,]
  } else {
    result <- log(data$x)
  }
  return(result)
}






























