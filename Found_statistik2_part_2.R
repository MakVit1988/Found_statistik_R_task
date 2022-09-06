# Задачи ко второму разделу
# Задача 1
test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv") # Данные для проверки
test_data <- transform(test_data, x = factor(x), y = factor(y)) # Переводим данные в фактор

get_coefficients <- function(dataset){
  return(exp(glm(y~x, test_data, family = "binomial")$coefficients))
}

get_coefficients(test_data)