date <- read.csv(file.choose())
dt = date[[1]] # первый столбец данных извлекается
ut <- c(NA) # вектор ut
n <- length(dt) # возвращает число элементов в векторе dt в n
for (i in 2:n)
  {
  ut[i] = dt[i-1]
  }
et  <- dt - ut
etm <- abs(et)
MAE <- 0
MAPE <- 0
MSE <- 0
for (i in 2:n) # Накопление суммы
  {
  MAE  = MAE  + etm[i]
  MAPE = MAPE + etm[i]/abs(dt[i])
  MSE  = MSE  + etm[i]^2
  }
# Деление на количество наблюдений
MAE  = MAE  * (1/(n-1))
MAPE = MAPE * (1/(n-1)*100)
MSE  = MSE  * (1/(n-1))
RMSE = sqrt(MSE)
# Вывод результатов
cat("MAE =", MAE, "\n")
cat("MAPE =", MAPE, "%\n")
cat("MSE =", MSE, "\n")
cat("RMSE =", RMSE, "\n")
# ПОСТРОЕНИЕ ГРАФИКОВ
par(mfrow = c(2, 2))

# 1. Исходный временной ряд
plot(dt, type = "o", col = "blue", pch = 16,
     main = "Исходный ряд dt",
     xlab = "Индекс", ylab = "Значение")
grid()

# 2. Сравнение факта и прогноза
plot(dt, type = "o", col = "blue", pch = 16,
     ylim = range(c(dt, ut), na.rm = TRUE),
     main = "Факт и прогноз",
     xlab = "Индекс", ylab = "Значение")

lines(ut, type = "o", col = "red", pch = 17)

legend("topleft",
       legend = c("Факт dt", "Прогноз ut"),
       col = c("blue", "red"),
       pch = c(16, 17), lty = 1)
grid()

# 3. Ошибки прогнозирования
plot(et, type = "o", col = "darkgreen", pch = 16,
     main = "Ошибки прогноза",
     xlab = "Индекс", ylab = "Ошибка")
abline(h = 0, lty = 2, col = "gray")
grid()

# 4. Метрики качества
metrics <- c(MAE = MAE, MAPE = MAPE,
             MSE = MSE, RMSE = RMSE)

colors <- c("steelblue", "orange",
            "tomato", "seagreen")

valid <- is.finite(metrics)

barplot(metrics[valid],
        col = colors[valid],
        main = "Метрики качества",
        ylab = "Значение")

# Возврат к одному графику
par(mfrow = c(1, 1))