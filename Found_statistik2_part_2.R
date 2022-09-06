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
