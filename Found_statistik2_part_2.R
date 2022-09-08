# Задачи ко второму разделу
# Задача 1
test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv") # Данные для проверки
test_data <- transform(test_data, x = factor(x), y = factor(y)) # Переводим данные в фактор

get_coefficients <- function(dataset){
  return(exp(glm(y~x, test_data, family = "binomial")$coefficients))
}

get_coefficients(test_data)


# Задача 2
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")

var_names = c('X4', "X2", "X1")

# Функция через for
centered <- function(test_data, var_names){
  for (i in 1:length(var_names)) {
    test_data[,colnames(test_data)==var_names[i]] <- test_data[,colnames(test_data)==var_names[i]] -  mean(test_data[,colnames(test_data)==var_names[i]])
  }
  return(test_data)
}

# Решение через sapply
centered <- function(test_data, var_names){
  test_data[var_names] <- sapply(test_data[var_names], FUN = function(x) x-mean(x))
  return(test_data)
}

centered(test_data, var_names)


#Задача 3
dataset <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")

dataset$is_prohibited<-factor(dataset$is_prohibited) # преобразовываем в фактор, в задаче ничего не сказано
dataset$type <- factor(dataset$type)

get_features <- function(dataset){
  fit <- glm(is_prohibited~., dataset, family = "binomial") # лог регрессия
  p_vector <-anova(fit, test = "Chisq") # тест анова
  name <- rownames(p_vector) # получаем вектор с именами предикторов
  p_i<-which(((p_vector$`Pr(>Chi)`))<0.05) # получаем номера позиций значимых предикторов
  if (length(name[p_i])==0){ # если длина вектора с именами значимых предикторов равна 0
    return("Prediction makes no sense") # то выводим сообщение
  } else {
    return(name[p_i]) # иначе выводим вектор с именами значимых предикторов
  }
}

get_features(dataset)

get_features <- function(dataset){
  tests <- glm(is_prohibited ~ ., test_data, family = 'binomial')
  y <- anova(tests, test = 'Chisq')[5]
  if (all(y > 0.05, na.rm = T)) return('Prediction makes no sense')
  rownames(y)[which(y < 0.05)]
}

# Задача 4
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")

data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")

most_suspicious <- function(test_data, data_for_predict){
  log_mod <- glm(factor(is_prohibited)~., test_data, family = "binomial") # строим логичтическую модель
  probability <- predict(log_mod, newdata = data_for_predict, type = "response") # на основании построенной модели, получаем вероятность для новых данных
  index <- which.max(probability) # определяем индекс с максимальным значением вероятности для новых данных
  result <- data_for_predict$passangers[index] # получаем имя пассажира с макисмальным значением вероятности для новых данных
  return(result)
}

most_suspicious(test_data, data_for_predict)

# Задача 5
dataset <- read.csv("https://stepic.org/media/attachments/course/524/test.csv")

normality_test <- function(dataset){
  
  # x1 <- dataset[sapply(dataset,  FUN = is.numeric)] - определяем данные для вычисления
  # return <- apply(x1, MARGIN = 2, FUN=shapiro.test)  - получаем результаты Шапиро-тест для всех полученных данных
  # unlist(lapply(return, function(x) x$p.value)) - преобразуем список всех p.value значений нашего теста в вектор
  
  result <- apply(dataset[sapply(dataset,  FUN = is.numeric)], MARGIN = 2, FUN=shapiro.test)
  unlist(lapply(result, function(x) x$p.value)) 
}

normality_test(dataset)
