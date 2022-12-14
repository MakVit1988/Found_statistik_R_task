# Неделя 1
# Задача 1
# Давайте реализуем простейший вариант теста для проверки наличия гетероскедастичности.  
# Напишите функцию hetero_test, которая получает на вход набор данных. Первая колонка в данных - 
#   зависимая переменная, остальные колонки - независимые. Функция строит регрессионную модель, 
# используя эти переменные, а затем проверяет, есть ли в данных  гетероскедастичность.
# Для этого функция строит вспомогательную регрессию, в которой зависимая переменная - это квадраты 
# остатков исходной модели, а независимые переменные - это предикторы из исходной модели. Функция должна 
# возвращать значение R квадрат этой вспомогательной модели.
hetero_test <-  function(test_data){
  fit <- lm(test_data[,1]~., data=test_data[,-1])
  fit_uait_test <- lm((fit$residuals)^2 ~ ., data = test_data[, c(-1), drop = F])
  summary(fit_uait_test)$r.squared 
  
}

# Задача 2
# Самостоятельно реализуйте расчет показателя vif.  Напишите функцию VIF, которая получает на вход набор 
# данных. Первая колонка в данных - зависимая переменная, остальные колонки - независимые. Функция строит 
# регрессионную модель, используя эти переменные, а затем для каждой  независимой переменной рассчитывает 
# показатель vif.
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
# Усложним предыдущую задачу, а также используем нашу функцию VIF. Напишите функцию smart_model, 
# которая на вход получает набор данных. Первая колонка в данных - зависимая переменная, остальные 
# колонки - независимые. Функция строит регрессию с этими переменными и проверяет есть ли в модели 
# переменные с показателем vif больше 10. Если хотя бы у одной переменной vif > 10, то из регрессионной 
# модели удаляется переменная с максимальным показателем vif, если после этого в новой модели все еще 
# остались переменные с vif больше 10, то мы опять исключаем из модели переменную с максимальным vif. 
# Таким образом, мы исключаем по одной переменной за раз, пока в модели не останутся независимые 
# переменные с vif не больше 10.
# Особая ситуация - это когда в модели два предиктора, и для обоих vif одинаковый и больше 10, в этом 
# случае можно исключить любой из предикторов.
# Функция должна возвращать коэффициенты регрессии финальной модели.
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
# Функция должна вернуть трансформированную переменную x. Обратите внимание, что вместо 
# возведения в нулевую степень мы будем логарифмировать переменную. В этом примере давайте 
# ограничимся показателями степени от -2 до 2 с шагом 0.1.  
# Гарантируется, что значения переменных в данных неотрицательные.
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


# Неделя 2
# Смешанные регрессинонные модели
install.packages('lme4')
install.packages('mlmRev')
install.packages('lmerTest')

# Задачча 1
# Постройте график, отображающий различие в высоте голоса (frequency)
# для каждого предложения (scenario) в зависимости от типа социальной ситуации (attitude). 
# Не забудьте перевести переменную scenario в фактор.
exp_data <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
exp_data$scenario<-factor(exp_data$scenario)
plot_1 <- ggplot(exp_data, aes(x = as.factor(scenario), y = frequency, fill = attitude))+
  geom_boxplot(na.rm = TRUE)
plot_1

# Задача 2
# Визуализируйте распределение высоты голоса у испытуемых в зависимости от пола и номера испытуемого:
plot_2 <- ggplot(exp_data, aes(x=frequency, fill=subject, alpha=0.3), na.rm = TRUE)+
  geom_density()+
  facet_grid(gender ~ .)

# Задача 3 
# Итак, ваша задача — построить следующую регрессионную модель: 
# зависимая переменная — frequency, 
# фиксированный эффект — attitude,
# а также добавлен случайный intercept для переменных subject и scenario.
# Сохраните модель в переменную fit_1. Данные записаны в переменной exp_data.
fit_1 <- lmer(frequency~attitude + (1+1|subject) + (1+1|scenario), data = exp_data)
summary(fit_1)

# Задача 4
# Ваша задача — построить следующую регрессионную модель:
# зависимая переменная — frequency,
# фиксированный эффект — attitude,
# фиксированный эффект — gender,
# а также добавлен случайный intercept для переменных subject и scenario.
# * вы уже, наверное, догадались, что не существует четкой границы между фиксированными и 
# случайными эффектами. В каком-то исследовании пол может выступать как случайный эффект, 
# однако в этом исследовании авторы включили его в список основных эффектов. 
fit_2 <- lmer(frequency~attitude + gender + (1+1|subject) + (1+1|scenario), data = exp_data)
summary(fit_2)

# Задача 5
# Ваша задача — построить следующую регрессионную модель:
# зависимая переменная — frequency,
# фиксированный эффект — attitude,
# фиксированный эффект — gender,
# а также добавлен случайный intercept и slope для переменных subject и scenario. 
# Теперь, когда в модели два фиксированных эффекта, мы можем по разному записать случайные эффекты. 
# В этом исследовании нас интересует случайный эффект для социальной ситуации (attitude).
fit_3 <- lmer(frequency~attitude + gender + (1+attitude|subject) + (1+attitude|scenario), data = exp_data)


# Неделя 3
# Выразите нижнюю границу базового доверительного интервала через перцентильный доверительный 
# интервал и выборочное среднее.
X_bar = 10
x_barstar = rnorm(700, x_bar, 1)
high = quantile(x_barstar, 0.95)
low = quantile(x_barstar, 0.05)
m = mean(x_barstar)
delta = (h-l)/2

ggplot()+
  geom_histogram(aes(x_barstar), binwidth = 0.1)+
  geom_vline(xintercept = h, col ="red") +
  geom_vline(xintercept = l, col ="red") +
  geom_histogram(aes(x_bar-x_barstar), binwidth = 0.1)+
  geom_vline(xintercept =-1* (low +(high - low)/2 - X_bar) - (high - low)/2 + X_bar, col ="blue") +
  theme_bw()
# Правтичексие задания
# Задача 1
