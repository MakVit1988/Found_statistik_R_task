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


