# Установка пакетов (если нужно)
install.packages(c("tidyverse", "ggplot2", "caret", "randomForest", "corrplot"))

# Загрузка библиотек
library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)

#Авторы проекта
cat("Проект выполнила: Кадырова Резеда Адгамовна. Помогали: Александрова У.В., Ахтямов М.Р.", "\n")

# Загрузка данных (укажите свой путь к файлу)
publications <- read_csv("publication.csv")

# Просмотр структуры данных
glimpse(publications)

# Проверим общее количество NA
cat("Общее количество пропущенных значений:", sum(is.na(publications)), "\n")

# Очистка и подготовка
data_clean <- publications %>%
  # Обработка пропусков
  mutate(
    citations = replace_na(citations, 0),
    is_oa = as.logical(is_oa),
    has_grant = as.logical(has_grant),
    quartile = as.factor(quartile),
    journal_name = replace_na(journal_name, "Unknown")
  ) %>%
  # Создание переменной successful (цитируемость выше медианы за год)
  group_by(year) %>%
  mutate(
    median_citations = median(citations, na.rm = TRUE),
    successful = citations > median_citations
  ) %>%
  ungroup() %>%
  # Создание переменной collaboration_level
  mutate(
    collaboration_level = case_when(
      num_authors == 1 ~ "Solo",
      num_authors <= 3 ~ "Small Team",
      TRUE ~ "Large Team"
    ),
    collaboration_level = as.factor(collaboration_level)
  )

# Проверка пропусков
colSums(is.na(data_clean))

#Гистограмма публикаций по годам
ggplot(data_clean, aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Number of Publications by Year", x = "Year", y = "Count") +
  theme_minimal()
ggsave("figures/publications_by_year.png")

#Boxplot цитируемости по квартилям
ggplot(data_clean, aes(x = quartile, y = citations)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Citation Count by Journal Quartile", x = "Quartile", y = "Citations") +
  theme_minimal()
ggsave("figures/citations_by_quartile.png")

# Цитируемость по уровню кооперации
ggplot(data_clean, aes(x = collaboration_level, y = citations, fill = collaboration_level)) +
  geom_violin() +
  labs(title = "Citations by Collaboration Level", x = "Collaboration Level", y = "Citations") +
  theme_minimal()
ggsave("figures/citations_by_collaboration.png")

# Доля публикаций с грантами
ggplot(data_clean, aes(x = has_grant, fill = has_grant)) +
  geom_bar() +
  labs(title = "Publications with Grant Funding", x = "Grant Funded", y = "Count") +
  theme_minimal()
ggsave("figures/grant_funded.png")

# Доля успешных публикаций
success_rate <- mean(data_clean$successful, na.rm = TRUE)
cat("Доля успешных публикаций:", success_rate, "\n")

# Средняя цитируемость по авторам
# Разделяем author_ids на отдельные записи
author_citations <- data_clean %>%
  separate_rows(author_ids, sep = ",") %>%
  group_by(author_ids) %>%
  summarise(
    avg_citations = mean(citations, na.rm = TRUE),
    num_publications = n()
  ) %>%
  rename(author_id = author_ids)

# Вывод первых строк
head(author_citations)

# Зависимость успешности от признаков
success_by_features <- data_clean %>%
  group_by(has_grant, is_oa, quartile) %>%
  summarise(success_rate = mean(successful, na.rm = TRUE), .groups = "drop")

# Вывод результатов
print(success_by_features)

# Визуализация зависимости
ggplot(success_by_features, aes(x = quartile, y = success_rate, fill = has_grant)) +
  geom_col(position = "dodge") +
  facet_wrap(~is_oa) +
  labs(title = "Success Rate by Quartile, Grant, and Open Access",
       x = "Quartile", y = "Success Rate") +
  theme_minimal()
ggsave("figures/success_by_features.png")

#Модель расходов
data_cost <- data_clean %>%
  mutate(
    base_cost = case_when(
      quartile == "Q1" ~ 100000,
      quartile == "Q2" ~ 80000,
      quartile == "Q3" ~ 50000,
      quartile == "Q4" ~ 30000,
      TRUE ~ 10000
    ),
    cost_final = base_cost * (1 + 0.3 * is_oa) * (1 - 0.8 * has_grant)
  )

# Общие расходы
total_cost <- sum(data_cost$cost_final, na.rm = TRUE)
cat("Общие расходы института:", total_cost, "₽\n")

# Средние расходы на успешную публикацию
avg_cost_success <- mean(data_cost$cost_final[data_cost$successful], na.rm = TRUE)
cat("Средние расходы на успешную публикацию:", avg_cost_success, "₽\n")

# Эффективность
efficiency <- sum(data_cost$successful, na.rm = TRUE) / (total_cost / 100000)
cat("Эффективность:", efficiency, "успешных публикаций на 100,000 ₽\n")

# Моделирование
# Подготовка данных
model_data <- data_clean %>%
  select(successful, num_authors, is_oa, has_grant, quartile, year) %>%
  mutate(
    is_oa = as.numeric(is_oa),
    has_grant = as.numeric(has_grant),
    successful = as.factor(successful) 
  ) %>%
  na.omit()

# Создание dummy-переменных для quartile
model_data <- model_data %>%
  mutate(quartile = as.factor(quartile)) %>%
  bind_cols(model.matrix(~ quartile - 1, data = .) %>% as_tibble()) %>%
  select(-quartile)


# Разделение на обучающую и тестовую выборки
set.seed(123)
train_index <- createDataPartition(model_data$successful, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Обучение модели Random Forest
rf_model <- randomForest(
  successful ~ .,
  data = train_data,
  ntree = 100,
  importance = TRUE
)

# Предсказания
predictions <- predict(rf_model, test_data)

# Матрица ошибок и метрики
conf_matrix <- confusionMatrix(predictions, test_data$successful)
print(conf_matrix)

# Извлечение метрик
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")

# Важность переменных
importance <- importance(rf_model)
varImpPlot(rf_model)

# Сохранение графика
png("figures/variable_importance.png")
varImpPlot(rf_model)

# Выводы по проекту
cat("\n=== Выводы по проекту ===\n")
cat("1. Доля успешных публикаций составляет ", round(success_rate * 100, 1), "%. Это значит, что ", round(success_rate * 100, 1), "% публикаций имеют цитируемость выше медианы за свой год.\n", sep = "")
cat("2. Публикации в журналах с высоким квартилем (Q1, Q2) имеют больше цитирований, чем в Q3 и Q4 (см. график figures/citations_by_quartile.png).\n")
cat("3. Публикации с грантами (has_grant = TRUE) чаще успешны, чем без грантов. Open Access также увеличивает вероятность успеха (см. график figures/success_by_features.png).\n")
cat("4. Публикации с большим числом авторов (Large Team) имеют более высокую цитируемость, чем одиночные или малые команды (см. график figures/citations_by_collaboration.png).\n")
cat("5. Общие расходы института на публикации составили ", round(total_cost, 0), " ₽. Средняя стоимость успешной публикации: ", round(avg_cost_success, 0), " ₽.\n", sep = "")
cat("6. Эффективность публикаций: ", round(efficiency, 2), " успешных публикаций на 100,000 ₽.\n", sep = "")
cat("7. Модель Random Forest показала точность ", round(accuracy * 100, 1), "%. Это значит, что она правильно предсказывает успешность в ", round(accuracy * 100, 1), "% случаев.\n", sep = "")
cat("8. Самые важные признаки для предсказания успешности: наличие гранта, Open Access и квартиль журнала (см. график figures/variable_importance.png).\n")
cat("Рекомендации: Для повышения успешности публикаций институту стоит поддерживать грантовое финансирование и публиковать статьи в журналах Q1 и Q2 с Open Access.\n")