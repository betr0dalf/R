# Установка необходимых библиотек ------------------------------------------

if (!require("caret")) install.packages("caret", dependencies=TRUE)
if (!require("cluster")) install.packages("cluster", dependencies=TRUE)
if (!require("clusterSim")) install.packages("clusterSim", dependencies=TRUE)
if (!require("corrplot")) install.packages("corrplot", dependencies=TRUE)
if (!require("data.table")) install.packages("data.table", dependencies=TRUE)
if (!require("dbscan")) install.packages("dbscan", dependencies=TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies=TRUE)
if (!require("factoextra")) install.packages("factoextra", dependencies=TRUE)
if (!require("fpc")) install.packages("fpc", dependencies=TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
if (!require("ggpubr")) install.packages("ggpubr", dependencies=TRUE)
if (!require("mclust")) install.packages("mclust", dependencies=TRUE)
if (!require("plotly")) install.packages("plotly", dependencies=TRUE)
if (!require("Rtsne")) install.packages("Rtsne", dependencies=TRUE)
if (!require("tidyr")) install.packages("tidyr", dependencies=TRUE)
if (!require("umap")) install.packages("umap", dependencies=TRUE)

library(caret)
library(cluster)
library(clusterSim)
library(corrplot)
library(data.table)
library(dbscan)
library(dplyr)
library(factoextra)
library(fpc)
library(ggplot2)
library(ggpubr)
library(mclust)
library(plotly)
library(Rtsne)
library(tidyr)
library(umap)

# Загрузка и предобработка данных -----------------------------------------

data <- fread("bank_transactions.csv")
head(data)

# Очистка пустых значений
colSums(is.na(data))
data <- na.omit(data)
colSums(is.na(data))
summary(data)
head(data)

# Просмотр типов данных
str(data)

# Преобразуем столбец CustGender в строковый формат
data$CustGender <- as.character(data$CustGender)

# Удаляем строки, в которых значение в столбце CustGender не равно F или M
data <- data[data$CustGender %in% c("F", "M"), ]

# Заменяем F на 0, а M на 1 в столбце CustGender
data$CustGender <- ifelse(data$CustGender == "F", 0, 1)

# Преобразуем столбец CustGender в числовой формат
data$CustGender <- as.numeric(data$CustGender)

# Переименовываем столбец TransactionAmount (INR) в TransactionAmount
colnames(data)[colnames(data) == "TransactionAmount (INR)"] <- "TransactionAmount"

# Удалим ненужные столбцы
data <- subset(data, select = -c(CustomerDOB, CustLocation, TransactionDate))
head(data)

# Убираем первую букву в ID и переводим столбец в числовой тип
data$TransactionID <- as.numeric(gsub("^T", "", data$TransactionID))
data$CustomerID <- as.numeric(gsub("^C", "", data$CustomerID))

# Убираем операции на 0 рупий
data <- subset(data, data$TransactionAmount != 0.0)

# Считаем статистические данные
data_stats <- data %>%
  group_by(CustomerID) %>%
  summarise(
    totalTransactionAmount = sum(TransactionAmount),
    avgTransactionAmount = mean(TransactionAmount),
    numOfTransactions = n(),
    stdTransactionAmount = sd(TransactionAmount),
    quantile25 = quantile(TransactionAmount, 0.25),
    dataMediana = median(TransactionAmount),
    quantile75 = quantile(TransactionAmount, 0.75),
    minTransactionAmount = min(TransactionAmount),
    maxTransactionAmount = max(TransactionAmount),
    dispersionTransactionAmount = var(TransactionAmount)
  ) %>%
  mutate(stdTransactionAmount = ifelse(is.na(stdTransactionAmount), 0, stdTransactionAmount),
         dispersionTransactionAmount = ifelse(is.na(dispersionTransactionAmount), 0, dispersionTransactionAmount))

# Проверка результата
head(data)
head(data_stats)

# Выбираем клиентов с более чем 1 транзакцией
data_stats <- subset(data_stats, data_stats$numOfTransactions > 1)
head(data_stats)

# Анализ данных -----------------------------------------------------------

# Количество уникальных клиентов
num_unique_customers <- length(unique(data_stats$CustomerID))
cat("Количество уникальных клиентов:", num_unique_customers, "\n")

# Описательная статистика суммы транзакций
cat("Описательная статистика суммы транзакций:\n")
summary(data$TransactionAmount)

# Описательная статистика для avgTransactionAmount
cat("Описательная статистика для среднего размера транзакций:\n")
summary(data_stats$avgTransactionAmount)

# Описательная статистика для numOfTransactions
cat("Описательная статистика для количества транзакций:\n")
summary(data_stats$numOfTransactions)

# Корреляционная матрица для данных data_stats
correlation_matrix <- cor(data_stats[, -1])  
cat("Корреляционная матрица:\n")
print(correlation_matrix)

# Визуализация корреляционной матрицы
corrplot(correlation_matrix, method = "circle", type = "upper", tl.cex = 0.8)

# Проверка распределения количества транзакций
ggdensity(data_stats$numOfTransactions, main = "Плотность распределения количества транзакций", xlab = "Количество транзакций")

# Графики анализа данных data ---------------------------------------------

# 1. Распределение суммы транзакций по гендеру
ggplot(data, aes(x = as.factor(CustGender), y = TransactionAmount)) +
  geom_boxplot() +
  labs(title = "Распределение суммы транзакций по гендеру", x = "Гендер (0 - Женщины, 1 - Мужчины)", y = "Сумма транзакции")

# 2. Круговая диаграмма распределения гендеров
gender_distribution <- data %>%
  group_by(CustGender) %>%
  summarise(count = n()) %>%
  mutate(percentage = round((count / sum(count)) * 100, 1))

ggplot(gender_distribution, aes(x = "", y = percentage, fill = as.factor(CustGender))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Распределение гендеров", x = "", y = "Процент", fill = "Гендер") +
  theme(axis.text.x = element_blank())

print(gender_distribution)

# 3. Распределение суммы транзакций с логарифмической шкалой
ggplot(data, aes(x = TransactionAmount)) +
  geom_histogram(bins = 50, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_log10() +
  labs(title = "Распределение суммы транзакций (логарифмическая шкала)", x = "Сумма транзакции (log10)", y = "Частота")

# Графики анализа данных data_stats ---------------------------------------

# 1. Распределение среднего размера транзакций (ограниченный диапазон)
ggplot(data_stats, aes(x = avgTransactionAmount)) +
  geom_histogram(bins = 50, fill = "purple", color = "black", alpha = 0.7) +
  scale_x_continuous(limits = c(0, 10000)) +  # Ограничиваем ось x до 10000
  labs(title = "Распределение среднего размера транзакций", x = "Средняя сумма транзакции", y = "Частота")

# 2. Распределение количества транзакций
ggplot(data_stats, aes(x = numOfTransactions)) +
  geom_histogram(bins = 50, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Распределение количества транзакций", x = "Количество транзакций", y = "Частота")

# 3. Распределение суммы транзакций по количеству транзакций
ggplot(data_stats, aes(x = numOfTransactions, y = totalTransactionAmount)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Распределение суммы транзакций по количеству транзакций", x = "Количество транзакций", y = "Сумма транзакций")

# 4. Взаимосвязь между средней и стандартной ошибкой суммы транзакций (убираем нулевые std)
filtered_data <- data_stats %>% filter(stdTransactionAmount != 0)
ggplot(filtered_data, aes(x = avgTransactionAmount, y = stdTransactionAmount)) +
  geom_point(color = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Линейная регрессия
  labs(title = "Взаимосвязь между средней и стандартной ошибкой", x = "Средняя сумма транзакции", y = "Стандартное отклонение суммы транзакции")

# Рассчитаем коэффициент линейной регрессии
model <- lm(stdTransactionAmount ~ avgTransactionAmount, data = filtered_data)
k <- coef(model)[2]
cat("Коэффициент k для линейной зависимости:", k, "\n")

# Нормализация данных -----------------------------------------------------

# Данные для визуализации
data_stats <- subset(data_stats, select = -c(CustomerID, numOfTransactions))

normalized_data <- scale(data_stats[, c("totalTransactionAmount",
  "avgTransactionAmount", "quantile25", "dataMediana",
  "quantile75", "minTransactionAmount", "maxTransactionAmount")])

normalized_data_unique <- unique(normalized_data)

set.seed(2007)
sample_indices <- sample(1:nrow(normalized_data_unique), 5000)
sample_normalized_data <- normalized_data_unique[sample_indices, ]

kNNdistplot(sample_normalized_data, k = 3)  # k = minPts - 1
abline(h = 1, col = "red")  # Примерная линия для eps

# Подбор параметров для DBSCAN --------------------------------------------

set.seed(2007)
results <- data.frame()

for (eps in seq(0.1, 2.0, by = 0.05)) {
  cat("eps:\n")
  print(eps)
  for (MinPts in 3:10) {
    cat("MinPts:\n")
    print(MinPts)
    dbscan_result <- dbscan(sample_normalized_data, eps = eps, MinPts = MinPts)
    
    # Для того, чтобы исключить огромные кластеры
    if (length(unique(dbscan_result$cluster)) > 4) {  
      # Расчет метрик
      sil <- silhouette(dbscan_result$cluster, dist(sample_normalized_data))
      mean_sil <- mean(sil[, 3], na.rm = TRUE)
      
      noise_percentage <- sum(dbscan_result$cluster == 0) / nrow(sample_normalized_data)
      
      db_index <- index.DB(sample_normalized_data, dbscan_result$cluster)$DB
      
      num_clusters <- length(unique(dbscan_result$cluster)) - 1  # исключаем шумовой кластер
      
      results <- rbind(results, data.frame(eps = eps, MinPts = MinPts, sil_score = mean_sil, noise_percentage = noise_percentage, db_index = db_index, num_clusters = num_clusters))
    }
  }
}

best_params <- results[which.max(results$sil_score),]
eps_best <- best_params$eps
MinPts_best <- best_params$MinPts
results <- results[!is.na(results$db_index), ]
results <- results[results$sil_score >= 0, ]
head(results)
print(eps_best)
print(MinPts_best)

# DBSCAN, после t-SNE и Umap --------------------------------------------

dbscan_result <- dbscan(sample_normalized_data, eps = 1.50, MinPts = 3)
#dbscan_result <- dbscan(normalized_data_unique, eps = eps_best, MinPts = MinPts_best)

head(dbscan_result)

# t-SNE -------------------------------------------------------------------

# Применение t-SNE для уменьшения размерности
tsne_result <- Rtsne(sample_normalized_data, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)
tsne_data <- as.data.frame(tsne_result$Y)

# Добавление кластерных меток
tsne_data$cluster <- factor(dbscan_result$cluster)

# Визуализация t-SNE результатов
ggplot(tsne_data, aes(x = V1, y = V2, color = cluster)) +
  geom_point(alpha = 0.5) +
  labs(title = "t-SNE Clustering of Transactions",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2")

# Силуэтный коэффициент
sil <- silhouette(dbscan_result$cluster, dist(sample_normalized_data))
mean_sil <- mean(sil[, 3])

# Индекс Дэвиcа-Болдуина
db_index <- index.DB(sample_normalized_data, dbscan_result$cluster)$DB

# Процент шумовых точек
noise_percentage <- sum(dbscan_result$cluster == 0) / nrow(sample_normalized_data)

print(paste("Средний силуэтный коэффициент:", mean_sil))
print(paste("Индекс Дэвиса-Болдина:", db_index))
print(paste("Процент шумовых точек:", noise_percentage))

# Umap -------------------------------------------------------------------

# Применение UMAP для уменьшения размерности
umap_result <- umap(sample_normalized_data, n_neighbors = 15, min_dist = 0.1, n_components = 2)
umap_data <- as.data.frame(umap_result$layout)

# Добавление кластерных меток
umap_data$cluster <- factor(dbscan_result$cluster)

# Визуализация UMAP результатов
ggplot(umap_data, aes(x = V1, y = V2, color = cluster)) +
  geom_point(alpha = 0.5) +
  labs(title = "UMAP Clustering of Transactions",
       x = "UMAP Dimension 1",
       y = "UMAP Dimension 2")

# Силуэтный коэффициент
sil <- silhouette(dbscan_result$cluster, dist(sample_normalized_data))
mean_sil <- mean(sil[, 3])

# Индекс Дэвиcа-Болдуина
db_index <- index.DB(sample_normalized_data, dbscan_result$cluster)$DB

# Процент шумовых точек
noise_percentage <- sum(dbscan_result$cluster == 0) / nrow(sample_normalized_data)

print(paste("Средний силуэтный коэффициент:", mean_sil))
print(paste("Индекс Дэвиса-Болдина:", db_index))
print(paste("Процент шумовых точек:", noise_percentage))

# Сначала T-sne и umap, потом DBSCAN ----------------------------------------

# t-SNE -------------------------------------------------------------------

# Уменьшение размерности с помощью t-SNE
tsne_result <- Rtsne(sample_normalized_data, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)
tsne_data <- tsne_result$Y

# Подбор коэффциентов для T-sne
set.seed(2007)
results_tsne <- data.frame()

for (eps in seq(0.1, 2.0, by = 0.05)) {
  cat("eps:\n")
  print(eps)
  for (MinPts in 3:10) {
    cat("MinPts:\n")
    print(MinPts)
    dbscan_tsne <- dbscan(tsne_data, eps = eps, MinPts = MinPts)
    
    if (length(unique(dbscan_tsne$cluster)) > 1) {  # исключаем случаи, когда все точки в одном кластере
      # Расчет метрик
      sil <- silhouette(dbscan_tsne$cluster, dist(tsne_data))
      mean_sil <- mean(sil[, 3], na.rm = TRUE)
      
      noise_percentage <- sum(dbscan_tsne$cluster == 0) / nrow(tsne_data)
      
      db_index <- index.DB(tsne_data, dbscan_tsne$cluster)$DB
      
      num_clusters <- length(unique(dbscan_tsne$cluster)) - 1  # исключаем шумовой кластер

      results_tsne <- rbind(results_tsne, data.frame(eps = eps, MinPts = MinPts, sil_score = mean_sil, noise_percentage = noise_percentage, db_index = db_index, num_clusters = num_clusters))
    }
  }
}
best_params_tsne <- results_tsne[which.max(results_tsne$sil_score),]
results_tsne <- results_tsne[!is.na(results_tsne$db_index), ]
results_tsne <- results_tsne[results_tsne$sil_score >= 0, ]
eps_best_tsne <- best_params_tsne$eps
MinPts_best_tsne <- best_params_tsne$MinPts
head(results_tsne)
print(eps_best_tsne)
print(MinPts_best_tsne)

# Кластеризация DBSCAN на данных после уменьшения размерности с помощью t-SNE
dbscan_tsne <- dbscan(tsne_data, eps = 0.55, MinPts = 3)

# Визуализация результатов кластеризации t-SNE
ggplot(data = as.data.frame(tsne_data), aes(x = V1, y = V2, color = factor(dbscan_tsne$cluster))) +
  geom_point(alpha = 0.5) +
  labs(title = "DBSCAN Clustering on t-SNE Data", x = "t-SNE 1", y = "t-SNE 2") +
  theme_minimal() +
  scale_color_discrete(name = "Cluster")

# Оценка кластеризации DBSCAN после t-SNE ----------------------------------------------------

# Силуэтный коэффициент
sil <- silhouette(dbscan_tsne$cluster, dist(tsne_data))
mean_sil <- mean(sil[, 3], na.rm = TRUE)

# Индекс Дэвиса-Болдуина
db_index <- index.DB(tsne_data, dbscan_tsne$cluster)$DB

# Процент шумовых точек
noise_percentage <- sum(dbscan_tsne$cluster == 0) / nrow(tsne_data)

# Вывод результатов
print(paste("Средний силуэтный коэффициент:", mean_sil))
print(paste("Индекс Дэвиса-Болдуина:", db_index))
print(paste("Процент шумовых точек:", noise_percentage))

# U-MAP -------------------------------------------------------------------

# Уменьшение размерности с помощью UMAP
umap_result <- umap(sample_normalized_data, n_components = 2)
umap_data <- umap_result$layout

set.seed(2007)
results_umap <- data.frame()

for (eps in seq(0.1, 2.0, by = 0.05)) {
  cat("eps:\n")
  print(eps)
  for (MinPts in 3:10) {
    cat("MinPts:\n")
    print(MinPts)
    dbscan_umap <- dbscan(umap_data, eps = eps, MinPts = MinPts)
    
    if (length(unique(dbscan_umap$cluster)) > 1) {  # исключаем случаи, когда все точки в одном кластере
      # Расчет метрик
      sil <- silhouette(dbscan_umap$cluster, dist(umap_data))
      mean_sil <- mean(sil[, 3], na.rm = TRUE)
      
      noise_percentage <- sum(dbscan_umap$cluster == 0) / nrow(umap_data)
      
      db_index <- index.DB(umap_data, dbscan_umap$cluster)$DB
      
      num_clusters <- length(unique(dbscan_tsne$cluster)) - 1  # исключаем шумовой кластер
      
      results_umap <- rbind(results_umap, data.frame(eps = eps, MinPts = MinPts, sil_score = mean_sil, noise_percentage = noise_percentage, db_index = db_index, num_clusters = num_clusters))
    }
  }
}
best_params_umap <- results_umap[which.max(results_umap$sil_score),]
results_umap <- results_umap[!is.na(results_umap$db_index), ]
results_umap <- results_umap[results_umap$sil_score >= 0, ]
eps_best_umap <- best_params_umap$eps
MinPts_best_umap <- best_params_umap$MinPts
head(results_umap)
print(eps_best_umap)
print(MinPts_best_umap)

# Кластеризация DBSCAN на данных после уменьшения размерности с помощью UMAP
dbscan_umap <- dbscan(umap_data, eps = 0.15, MinPts = 3)

# Визуализация результатов кластеризации UMAP
ggplot(data = as.data.frame(umap_data), aes(x = V1, y = V2, color = factor(dbscan_umap$cluster))) +
  geom_point(alpha = 0.5) +
  labs(title = "DBSCAN Clustering on UMAP Data", x = "UMAP 1", y = "UMAP 2") +
  theme_minimal() +
  scale_color_discrete(name = "Cluster")

# Оценка кластеризации DBSCAN после Umap ----------------------------------------------------

# Силуэтный коэффициент
sil <- silhouette(dbscan_umap$cluster, dist(umap_data))
mean_sil <- mean(sil[, 3], na.rm = TRUE)

# Индекс Дэвиса-Болдуина
db_index <- index.DB(umap_data, dbscan_umap$cluster)$DB

# Процент шумовых точек
noise_percentage <- sum(dbscan_umap$cluster == 0) / nrow(umap_data)

# Вывод результатов
print(paste("Средний силуэтный коэффициент:", mean_sil))
print(paste("Индекс Дэвиса-Болдуина:", db_index))
print(paste("Процент шумовых точек:", noise_percentage))
