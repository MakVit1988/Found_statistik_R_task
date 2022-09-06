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
  fit <- glm(is_prohibited~weight+length+width+type, dataset, family = "binomial") # лог регрессия
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
