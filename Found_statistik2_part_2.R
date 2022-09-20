# Практические задания 2.8
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

normality_test <- function(dataset){    
  numeric_var <- sapply(dataset, is.numeric)  
  sapply(dataset[numeric_var], function(x) shapiro.test(x)$p.value)    
}

# Задача 6
test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")
test_data <- as.data.frame(list(x = c(-0.18, -1.43, 0.57, 0.52, -1.51, -0.89, 0.71, 0.28, -0.53, 1.11, 2.9, 0.61, 0.42, -0.4, -0.11, 0.15, 0.55, 0.7, -1.13, 0.28, 0.36, 0.88, 0.61, 0.05, 0.31, -1.63, 0.52, 0.12, 0.21, 0.47), y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)))
test_data$y <- factor(test_data$y)

smart_anova <- function(test_data){
  norm <- aggregate(x~y, test_data, function(x) shapiro.test(x)$p.value)$x
  gom <- c(bartlett.test(x~y, test_data)$p.value)
  if (norm[which.min(norm)]<0.05 | gom <0.05){
    p_valuekw <- kruskal.test(x~y, test_data)$p.value
    kw <- c("KW" = p_valuekw)
    return(kw)
  } else {
    fit <- aov(x~y, test_data)
    p_value <- summary(fit)[[1]]$'Pr(>F)'[1]
    anv <- c("ANOVA" = p_value)
    return(anv)
  }
  
}
smart_anova(test_data)

# Задача 7
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_for_norm.csv")

normality_by <- function(test){
  p_value <- aggregate(x~y+z, test_data, function(x) shapiro.test(x)$p.value)
  names(p_value)[names(p_value) == 'x'] <- 'p_value'
  return(p_value)
}

normality_by(test_data)


normality_by <- function(test){    
  grouped_data <- aggregate(test[,1],by=list(test[,2], test[,3]),                                  
                            FUN = function(x) {shapiro.test(x)$p.value})                                  
  names(grouped_data) <- c(colnames(test)[2:3],'p_value')                                  
  return(grouped_data)    
}


#Используя dplyr (при условии, что мы знаем имена переменных в данных):
  
  library(dplyr)    
normality_by <- function(test_data){    
  result <- test_data %>% group_by(y, z) %>%     
    summarize(p_value = shapiro.test(x)$p.value)     
  return(result)    
}


#Более общее решение с dplyr:
  
  library(dplyr)    
get_p_value <- function(x){      
  shapiro.test(x)$p.value    
}    
normality_by <- function(test){    
  grouped <- test %>%    
    group_by_(.dots = colnames(.)[2:3]) %>%         
    summarise_each(funs(get_p_value))         
  names(grouped)[3] <- 'p_value'         
  return(grouped)         
}

# Задача 8
test_data <- iris
library(ggplot2)
obj <- ggplot(iris, aes(x=Sepal.Length,fill=Species))+    
  geom_density(alpha = 0.2)

qplot(Sepal.Length, data = test_data, geom = "density", fill=Species, alpha=I(1/4)) 

# Задачи к 3 разделу
# Пример кластаризации K-means
library(ggplot2)
d <- iris[, c("Sepal.Length", "Petal.Width")]

fit <- kmeans(d, 5)
d$clusters <- factor(fit$cluster)

ggplot(d, aes(Sepal.Length, Petal.Width, col = clusters))+
  geom_point(size = 2)+
  theme_bw() 

x <- c(-3, 1, 2, 3, 5, 6, 7)
y <- c(3, 4, 6, 8, 2, 11, 1)
exemp <- data.frame(X = x, Y = y)
exemp$xm <- mean(exemp$X)
exemp$ym <- mean(exemp$Y)
exemp$r <- (exemp$xm-exemp$X)^2 + (exemp$ym-exemp$Y)^2
sum(exemp$r)

ggplot() + 
  geom_point(data = exemp, aes(X, Y))+
  geom_point(data = table_mean, aes(Xm ,Ym), col="red", size = 3)

# Иерархическая кластаризация
library(ggplot2) 
library(ggrepel) # для симпатичной подписи точек на графике

x <- rnorm(10)
y <- rnorm(10)
test_data <- data.frame(x, y)
test_data$labels <- 1:10

ggplot(test_data, aes(x, y, label = labels))+
  geom_point()+
  geom_text_repel()

d = dist(test_data)
fit <- hclust(d, method = "single")
plot(fit, labels = test_data$labels)
rect.hclust(fit, 5)


library(ape)
set.seed(222)
tr <- rtree(20, tip.label = c("B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U")) 
#левое дерево
plot.phylo(tr) 
#правое дерево 
plot.phylo(tr, use.edge.length=FALSE)

# Практические задания 3.6
# Задача 1
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")


smart_hclust<-  function(test_data, cluster_number){
  dist_test_data <- dist(test_data)
  fit <- hclust(dist_test_data, method = "complete", members = NULL)
  cluster <- cutree(fit, cluster_number)
  test_data$cluster <- factor(cluster)
  return(test_data)
}

smart_hclust(test_data, 3)

# Задача 2
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")


get_difference<-  function(test_data, cluster_number){
  dist_test_data <- dist(test_data)
  fit <- hclust(dist_test_data, method = "complete", members = NULL)
  cluster <- cutree(fit, cluster_number)
  test_data$cluster <- factor(cluster)
  result <-c()
  for (i in 1:(length(colnames(test_data))-1)) {
    res<-sapply(test_data[, i], function(x) summary(aov(test_data[,i]~cluster, test_data))[[1]]$'Pr(>F)'[1])
    if(res[i]<0.05){
      result<-append(result, colnames(test_data)[i], i)
    }
  }
  return(result)
  
}
get_difference(test_data, 3)

get_difference <- function(test_data, n_cluster){    
  dist_matrix <- dist(test_data)    
  fit <- hclust(dist_matrix)    
  test_data$cluster <- as.factor(cutree(fit, n_cluster))    
  p_val <- sapply(test_data[,-ncol(test_data)],    
                  function(x) {summary(aov(x~cluster, test_data))[[1]][1,'Pr(>F)']})    
  return(names(p_val)[p_val < 0.05])    
}

# Задача 3
test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")

get_pc <- function(d){
  t<-summary(prcomp(test_data, rank = 2))$x
  test_data$PC1 <- t[,1]
  test_data$PC2 <- t[,2]
  return(test_data)
}
get_pc(test_data)

get_pc <- function(test){    
  fit <- prcomp(test)    
  test<- cbind(test, fit$x[,1:2])    
  return(test)    
}

# Задача 4
data <- swiss

get_pca2 <- function(data){
  fit <- summary(prcomp(data)) # 
  res<- which(fit$importance[3,] > 0.9)
  data<-cbind(data, fit$x[,1:res[1]])
  return(data)
}

result<-get_pca2(data)
str(result)

get_pca2 <- function(test_data){    
  fit <- prcomp(test_data)    
  cum_prop <- summary(fit)importance['Cumulative Proportion',]    
  test_data <- cbind(test_data, fitimportance['Cumulative Proportion',])
  test_data<- cbind(test_data,fitx[,1:min(which(cum_prop>0.9))])    
  return(test_data)    
}
