
# ЗАГРУЗКА ДАННЫХ

# Установка и подключение библиотеки для Excel
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)

# Выбор Excel-файла
cat("Выберите Excel-файл со столбцом dt...\n")
file_path <- file.choose()

# НАСТРОЙКИ ЗАГРУЗКИ

sheet_number <- 1   # Номер листа Excel
column_number <- 1  # Номер столбца с данными

# Чтение Excel
date <- read_excel(file_path, sheet = sheet_number)

# ПРОВЕРКА ЗАГРУЖЕННОГО ФАЙЛА

cat("\nВыбран лист №", sheet_number, "\n")

cat("\nСтолбцы Excel:\n")
for (j in 1:ncol(date)) {
  cat(j, "-", names(date)[j], "\n")
}

cat("\nПервые 10 строк Excel:\n")
print(head(date, 10))

# Проверка номера столбца
if (column_number < 1 || column_number > ncol(date)) {
  stop("Неверный номер столбца Excel")
}

# Первый выбранный столбец данных
dt_text <- as.character(date[[column_number]])

# Замена десятичной запятой на точку
dt_text <- gsub(",", ".", dt_text, fixed = TRUE)

# Преобразование в числа
dt <- suppressWarnings(as.numeric(dt_text))

# Проверка количества распознанных чисел
cat("\nВсего строк в столбце =", length(dt), "\n")
cat("Распознано чисел =", sum(!is.na(dt)), "\n")

# Удаление пропущенных значений
dt <- dt[!is.na(dt)]

# Количество наблюдений
n <- length(dt)

# ПРОВЕРКА ДАННЫХ

if (n < 6 || any(!is.finite(dt))) {
  stop(paste(
    "Распознано", n, "числовых наблюдений.",
    "Нужно минимум 6.",
    "Проверьте номер листа и столбца Excel."
  ))
}

# Проверка нулевых значений для MAPE
if (any(dt[2:n] == 0)) {
  stop("Нельзя вычислить MAPE: есть нулевые фактические значения")
}

cat("\nДанные успешно загружены!\n")
cat("Количество наблюдений =", n, "\n")
cat("Исходные значения:\n")
print(dt)


# МЕТОД 1. ПРОГНОЗ ПО ПРЕДЫДУЩЕМУ ЗНАЧЕНИЮ

# ПОСТРОЕНИЕ ПРОГНОЗА

ut <- rep(NA_real_, n)

for (i in 2:n) {
  ut[i] <- dt[i-1]
}

# РАСЧЁТ ОШИБОК

et <- dt - ut
etm <- abs(et)

MAE <- 0
MAPE <- 0
MSE <- 0

for (i in 2:n) {
  MAE <- MAE + etm[i]
  MSE <- MSE + etm[i]^2
}

MAE <- MAE / (n-1)
MSE <- MSE / (n-1)
RMSE <- sqrt(MSE)

# Расчёт MAPE

for (i in 2:n) {
  MAPE <- MAPE + etm[i] / abs(dt[i])
}

MAPE <- MAPE / (n-1) * 100

# ВЫВОД РЕЗУЛЬТАТОВ

cat("\nМЕТОД 1\n")
cat("Количество наблюдений =", n, "\n")
cat("MAE =", MAE, "\n")
cat("MAPE =", MAPE, "%\n")
cat("MSE =", MSE, "\n")
cat("RMSE =", RMSE, "\n")


# МЕТОД 2. ПРОГНОЗ ПО СРЕДНЕМУ ЗНАЧЕНИЮ

# ПОСТРОЕНИЕ ПРОГНОЗА

m <- mean(dt)

ut2 <- rep(NA_real_, n)

for (i in 2:n) {
  ut2[i] <- m
}

# РАСЧЁТ ОШИБОК

et2 <- dt - ut2
etm2 <- abs(et2)

MAE2 <- 0
MAPE2 <- 0
MSE2 <- 0

for (i in 2:n) {
  MAE2 <- MAE2 + etm2[i]
  MAPE2 <- MAPE2 + etm2[i] / abs(dt[i])
  MSE2 <- MSE2 + etm2[i]^2
}

MAE2 <- MAE2 / (n-1)
MAPE2 <- MAPE2 / (n-1) * 100
MSE2 <- MSE2 / (n-1)
RMSE2 <- sqrt(MSE2)

# ПРОГНОЗ СЛЕДУЮЩЕГО ЗНАЧЕНИЯ

ut_next2 <- m

# ВЫВОД РЕЗУЛЬТАТОВ

cat("\nМЕТОД 2\n")
cat("MAE =", MAE2, "\n")
cat("MAPE =", MAPE2, "%\n")
cat("MSE =", MSE2, "\n")
cat("RMSE =", RMSE2, "\n")
cat("Прогноз следующего значения =", ut_next2, "\n")


# МЕТОД 3. СКОЛЬЗЯЩЕЕ СРЕДНЕЕ (ОКНО 2..5)

# ПОДБОР ЛУЧШЕГО ОКНА

MAPE_best3 <- Inf
window_best <- 2

for (window in 2:5) {
  
  ut_temp <- rep(NA_real_, n)
  
  # Прогноз по предыдущим значениям
  for (i in (window+1):n) {
    ut_temp[i] <- mean(dt[(i-window):(i-1)])
  }
  
  # Расчёт MAPE
  MAPE_temp <- 0
  
  for (i in (window+1):n) {
    MAPE_temp <- MAPE_temp +
      abs(dt[i] - ut_temp[i]) / abs(dt[i])
  }
  
  MAPE_temp <- MAPE_temp / (n-window) * 100
  
  # Выбор наименьшей ошибки
  if (MAPE_temp < MAPE_best3) {
    MAPE_best3 <- MAPE_temp
    window_best <- window
  }
}

# ПОСТРОЕНИЕ ПРОГНОЗА С ЛУЧШИМ ОКНОМ

ut3 <- rep(NA_real_, n)

for (i in (window_best+1):n) {
  ut3[i] <- mean(dt[(i-window_best):(i-1)])
}

# РАСЧЁТ ОШИБОК

et3 <- dt - ut3
etm3 <- abs(et3)

MAE3 <- 0
MAPE3 <- 0
MSE3 <- 0

for (i in (window_best+1):n) {
  MAE3 <- MAE3 + etm3[i]
  MAPE3 <- MAPE3 + etm3[i] / abs(dt[i])
  MSE3 <- MSE3 + etm3[i]^2
}

k <- n - window_best

MAE3 <- MAE3 / k
MAPE3 <- MAPE3 / k * 100
MSE3 <- MSE3 / k
RMSE3 <- sqrt(MSE3)

# ПРОГНОЗ СЛЕДУЮЩЕГО ЗНАЧЕНИЯ

ut_next3 <- mean(dt[(n-window_best+1):n])

# ВЫВОД РЕЗУЛЬТАТОВ

cat("\nМЕТОД 3\n")
cat("Лучшее окно =", window_best, "\n")
cat("MAE =", MAE3, "\n")
cat("MAPE =", MAPE3, "%\n")
cat("MSE =", MSE3, "\n")
cat("RMSE =", RMSE3, "\n")
cat("Прогноз следующего значения =", ut_next3, "\n")


# МЕТОД 4. ЭКСПОНЕНЦИАЛЬНОЕ СГЛАЖИВАНИЕ

# ПОДБОР ЛУЧШЕГО ПАРАМЕТРА

MAPE_best4 <- Inf
alpha_best4 <- 0

for (j in 1:999) {
  
  alpha <- j / 1000
  
  ut_temp <- numeric(n)
  ut_temp[1] <- dt[1]
  
  # Построение прогноза
  for (i in 2:n) {
    ut_temp[i] <- alpha * dt[i-1] +
      (1-alpha) * ut_temp[i-1]
  }
  
  # Расчёт MAPE
  MAPE_temp <- 0
  
  for (i in 2:n) {
    MAPE_temp <- MAPE_temp +
      abs(dt[i] - ut_temp[i]) / abs(dt[i])
  }
  
  MAPE_temp <- MAPE_temp / (n-1) * 100
  
  # Выбор лучшего параметра
  if (MAPE_temp < MAPE_best4) {
    MAPE_best4 <- MAPE_temp
    alpha_best4 <- alpha
  }
}

# ПОСТРОЕНИЕ ПРОГНОЗА

ut4 <- numeric(n)
ut4[1] <- dt[1]

for (i in 2:n) {
  ut4[i] <- alpha_best4 * dt[i-1] +
    (1-alpha_best4) * ut4[i-1]
}

# РАСЧЁТ ОШИБОК

et4 <- dt - ut4
etm4 <- abs(et4)

MAE4 <- 0
MAPE4 <- 0
MSE4 <- 0

for (i in 2:n) {
  MAE4 <- MAE4 + etm4[i]
  MAPE4 <- MAPE4 + etm4[i] / abs(dt[i])
  MSE4 <- MSE4 + etm4[i]^2
}

MAE4 <- MAE4 / (n-1)
MAPE4 <- MAPE4 / (n-1) * 100
MSE4 <- MSE4 / (n-1)
RMSE4 <- sqrt(MSE4)

# ПРОГНОЗ СЛЕДУЮЩЕГО ЗНАЧЕНИЯ

ut_next4 <- alpha_best4 * dt[n] +
  (1-alpha_best4) * ut4[n]

# ВЫВОД РЕЗУЛЬТАТОВ

cat("\nМЕТОД 4\n")
cat("Лучший alpha =", alpha_best4, "\n")
cat("MAE =", MAE4, "\n")
cat("MAPE =", MAPE4, "%\n")
cat("MSE =", MSE4, "\n")
cat("RMSE =", RMSE4, "\n")
cat("Прогноз следующего значения =", ut_next4, "\n")


# МЕТОД 5. АДАПТИВНАЯ МОДЕЛЬ

# ПОДБОР ЛУЧШЕГО ПАРАМЕТРА

MAPE_best5 <- Inf
alpha_best5 <- 0

for (j in 1:999) {
  
  alpha <- j / 1000
  
  ut_temp <- numeric(n)
  bt_temp <- numeric(n)
  et_temp <- numeric(n)
  
  # Начальные значения
  ut_temp[1] <- dt[1]
  bt_temp[1] <- dt[2] - dt[1]
  et_temp[1] <- 0
  
  # Построение модели
  for (i in 2:n) {
    
    bt_temp[i] <- bt_temp[i-1] +
      et_temp[i-1] * (1-alpha)^2
    
    ut_temp[i] <- ut_temp[i-1] + bt_temp[i-1] +
      (1-alpha^2) * et_temp[i-1]
    
    et_temp[i] <- dt[i] - ut_temp[i]
  }
  
  # Расчёт MAPE
  MAPE_temp <- 0
  
  for (i in 2:n) {
    MAPE_temp <- MAPE_temp +
      abs(et_temp[i]) / abs(dt[i])
  }
  
  MAPE_temp <- MAPE_temp / (n-1) * 100
  
  # Выбор лучшего параметра
  if (MAPE_temp < MAPE_best5) {
    MAPE_best5 <- MAPE_temp
    alpha_best5 <- alpha
  }
}

# ПОСТРОЕНИЕ ПРОГНОЗА

ut5 <- numeric(n)
bt5 <- numeric(n)
et5 <- numeric(n)

ut5[1] <- dt[1]
bt5[1] <- dt[2] - dt[1]
et5[1] <- 0

for (i in 2:n) {
  
  bt5[i] <- bt5[i-1] +
    et5[i-1] * (1-alpha_best5)^2
  
  ut5[i] <- ut5[i-1] + bt5[i-1] +
    (1-alpha_best5^2) * et5[i-1]
  
  et5[i] <- dt[i] - ut5[i]
}

# РАСЧЁТ ОШИБОК

etm5 <- abs(et5)

MAE5 <- 0
MAPE5 <- 0
MSE5 <- 0

for (i in 2:n) {
  MAE5 <- MAE5 + etm5[i]
  MAPE5 <- MAPE5 + etm5[i] / abs(dt[i])
  MSE5 <- MSE5 + etm5[i]^2
}

MAE5 <- MAE5 / (n-1)
MAPE5 <- MAPE5 / (n-1) * 100
MSE5 <- MSE5 / (n-1)
RMSE5 <- sqrt(MSE5)

# ПРОГНОЗ СЛЕДУЮЩЕГО ЗНАЧЕНИЯ

ut_next5 <- ut5[n] + bt5[n] +
  (1-alpha_best5^2) * et5[n]

# ВЫВОД РЕЗУЛЬТАТОВ

cat("\nМЕТОД 5\n")
cat("Лучший alpha =", alpha_best5, "\n")
cat("MAE =", MAE5, "\n")
cat("MAPE =", MAPE5, "%\n")
cat("MSE =", MSE5, "\n")
cat("RMSE =", RMSE5, "\n")
cat("Прогноз следующего значения =", ut_next5, "\n")


# 6. СРАВНЕНИЕ ВСЕХ МЕТОДОВ

# Прогноз следующего значения для метода 1
ut_next1 <- dt[n]

# Значения MAPE всех методов
MAPE_all <- c(MAPE, MAPE2, MAPE3, MAPE4, MAPE5)

# Проверка результатов
if (any(!is.finite(MAPE_all))) {
  stop("Не удалось сравнить MAPE. Проверьте данные.")
}

# ВЫВОД MAPE ПО КАЖДОМУ МЕТОДУ

cat("\nСРАВНЕНИЕ МЕТОДОВ\n")

for (i in 1:5) {
  cat("Метод", i, ": MAPE =", MAPE_all[i], "%\n")
}

# ВЫБОР ЛУЧШЕГО МЕТОДА

best_number <- which.min(MAPE_all)
MAPE_best <- MAPE_all[best_number]

cat("\nЛучший метод =", best_number, "\n")
cat("Минимальная MAPE =", MAPE_best, "%\n")


# 6a. ПАРАМЕТРЫ МЕТОДА 3

cat("\nМЕТОД 3. СКОЛЬЗЯЩЕЕ СРЕДНЕЕ\n")
cat("Лучшее окно =", window_best, "\n")
cat("MAPE =", MAPE3, "%\n")


# 6b. ПАРАМЕТРЫ МЕТОДА 4

cat("\nМЕТОД 4. ЭКСПОНЕНЦИАЛЬНОЕ СГЛАЖИВАНИЕ\n")
cat("Лучший alpha =", alpha_best4, "\n")
cat("MAPE =", MAPE4, "%\n")


# 6c. ПАРАМЕТРЫ МЕТОДА 5

cat("\nМЕТОД 5. АДАПТИВНАЯ МОДЕЛЬ\n")
cat("Лучший alpha =", alpha_best5, "\n")
cat("MAPE =", MAPE5, "%\n")


# ПОДГОТОВКА ДАННЫХ ЛУЧШЕГО МЕТОДА

if (best_number == 1) {
  ut_best <- ut
  et_best <- et
  ut_next_best <- ut_next1
}

if (best_number == 2) {
  ut_best <- ut2
  et_best <- et2
  ut_next_best <- ut_next2
}

if (best_number == 3) {
  ut_best <- ut3
  et_best <- et3
  ut_next_best <- ut_next3
}

if (best_number == 4) {
  ut_best <- ut4
  et_best <- et4
  ut_next_best <- ut_next4
}

if (best_number == 5) {
  ut_best <- ut5
  et_best <- et5
  ut_next_best <- ut_next5
}

cat("\nПрогноз следующего значения =",
    ut_next_best, "\n")


# 7. ПОСТРОЕНИЕ ГРАФИКОВ

# Четыре графика в одном окне
par(mfrow = c(2, 2))


# 1. ИСХОДНЫЙ ВРЕМЕННОЙ РЯД

plot(dt, type = "o", col = "blue", pch = 16,
     main = "Исходный ряд dt",
     xlab = "Индекс", ylab = "Значение")

grid()


# 2. ФАКТ И ПРОГНОЗ ЛУЧШЕГО МЕТОДА

plot(dt, type = "o", col = "blue", pch = 16,
     xlim = c(1, n+1),
     ylim = range(c(dt, ut_best, ut_next_best),
                  na.rm = TRUE),
     main = paste("Лучший метод:", best_number),
     xlab = "Индекс", ylab = "Значение")

# Линия прогноза
lines(ut_best, type = "o", col = "red", pch = 17)

# Прогноз следующего значения
points(n+1, ut_next_best,
       col = "darkred", pch = 18, cex = 1.6)

# Обозначения
legend("topleft",
       legend = c("Факт", "Прогноз", "Следующее значение"),
       col = c("blue", "red", "darkred"),
       pch = c(16, 17, 18),
       lty = c(1, 1, NA))

grid()


# 3. ОШИБКИ ЛУЧШЕГО МЕТОДА

plot(et_best, type = "o",
     col = "darkgreen", pch = 16,
     main = "Ошибки лучшего метода",
     xlab = "Индекс", ylab = "Ошибка")

# Горизонтальная линия нулевой ошибки
abline(h = 0, lty = 2, col = "gray")

grid()


# 4. СРАВНЕНИЕ MAPE ВСЕХ МЕТОДОВ

barplot(MAPE_all,
        names.arg = c("M1", "M2", "M3", "M4", "M5"),
        col = c("steelblue", "orange",
                "tomato", "seagreen", "purple"),
        main = "Сравнение MAPE",
        ylab = "MAPE (%)")

# Линия минимальной ошибки
abline(h = MAPE_best, col = "red", lty = 2)

# Возвращаем режим одного графика
par(mfrow = c(1, 1))
