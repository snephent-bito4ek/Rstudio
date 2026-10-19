# ============================================================ 
# 0. БИБЛИОТЕКА 
# ============================================================ 
if (!require("readxl")) install.packages("readxl") 
library(readxl) 
# ============================================================ 
# 1. ВЫБОР ФАЙЛА И ЛИСТА 
# ============================================================ 
file_path <- file.choose() 
sheets    <- excel_sheets(file_path) 
cat("Листы:\n"); for (i in seq_along(sheets)) cat(i, ":", sheets[i], "\n") 
sheet_num  <- as.integer(readline("Номер листа: ")) 
sheet_name <- sheets[sheet_num] 
raw <- read_excel(file_path, sheet = sheet_name) 
# ============================================================ 
# 2. ИЗВЛЕЧЕНИЕ x (с 6-й строки, столбец B) 
# ============================================================ 
x <- suppressWarnings(as.numeric(raw[6:nrow(raw), 2][[1]])) 
x <- x[!is.na(x)] 
n <- length(x) 
cat("n =", n, "\n") 
if (n < 6) stop("Мало данных") 
# ============================================================ 
# 3. БЕЗОПАСНЫЙ MAPE (нули исключаются) 
# ============================================================ 
safe_mape <- function(ae, a) { 
  idx <- a != 0 
  if (length(idx) == 0) return(NA) 
  mean(ae[idx] / a[idx]) * 100 
} 
# ============================================================ 
# 4. M1: ПРЕДЫДУЩЕЕ ЗНАЧЕНИЕ 
# ============================================================ 
u1 <- c(NA, x[1:(n-1)]); e1 <- x - u1 
m1 <- list( 
  name  = "Метод среднего (предыдущее)", 
  param = "-", 
  mape  = safe_mape(abs(e1[2:n]), x[2:n]), 
  rmse  = sqrt(mean(e1[2:n]^2)), 
  nxt   = x[n], 
  u     = u1, e = e1 
) 

# ============================================================ 
# 5. M2: СРЕДНЕЕ РЯДА 
# ============================================================ 
u2 <- c(NA, rep(mean(x), n-1)); e2 <- x - u2 
m2 <- list( 
  name  = "Метод среднего (mean)", 
  param = "-", 
  mape  = safe_mape(abs(e2[2:n]), x[2:n]), 
  rmse  = sqrt(mean(e2[2:n]^2)), 
  nxt   = mean(x), 
  u     = u2, e = e2 
) 

# ============================================================ 
# 6. M3: СКОЛЬЗЯЩЕЕ СРЕДНЕЕ (w = 2..5) 
# ============================================================ 
best3 <- list(mape = Inf) 
for (w in 2:min(5, n-1)) { 
  u <- rep(NA, n) 
  for (i in w:n) u[i] <- mean(x[(i-w+1):i]) 
  e   <- x - u 
  idx <- which(!is.na(e)) 
  mp  <- safe_mape(abs(e[idx]), x[idx]) 
  if (!is.na(mp) && mp < best3$mape) 
    best3 <- list(mape=mp, w=w, u=u, e=e, idx=idx) 
} 
m3 <- list( 
  name  = "Moving (скользящее среднее)", 
  param = paste0("w=", best3$w), 
  mape  = best3$mape, 
  rmse  = sqrt(mean(best3$e[best3$idx]^2)), 
  nxt   = mean(x[(n-best3$w+1):n]), 
  u     = best3$u, e = best3$e 
) 

# ============================================================ 
# 7. M4: ЭКСПОНЕНЦИАЛЬНОЕ СГЛАЖИВАНИЕ 
# ============================================================ 
best4 <- list(mape = Inf) 
for (a in seq(0.01, 0.99, 0.01)) { 
  u <- numeric(n); u[1] <- x[1] 
  for (i in 2:n) u[i] <- a*x[i-1] + (1-a)*u[i-1] 
  e  <- x - u 
  mp <- safe_mape(abs(e[2:n]), x[2:n]) 
  if (!is.na(mp) && mp < best4$mape) 
    best4 <- list(mape=mp, a=a, u=u, e=e) 
} 
m4 <- list( 
  name  = "Brown (экспоненциальное сглаживание)", 
  param = paste0("a=", best4$a), 
  mape  = best4$mape, 
  rmse  = sqrt(mean(best4$e[2:n]^2)), 
  nxt   = best4$a*x[n] + (1-best4$a)*best4$u[n], 
  u     = best4$u, e = best4$e 
) 

# ============================================================ 
# 8. M5: БРАУН ЛИНЕЙНЫЙ 
# ============================================================ 
best5 <- list(mape = Inf) 
for (a in seq(0.01, 0.99, 0.01)) { 
  u <- numeric(n); b <- numeric(n); e <- numeric(n) 
  u[1] <- x[1]; b[1] <- x[2]-x[1]; e[1] <- 0 
  for (i in 2:n) { 
    b[i] <- b[i-1] + e[i-1]*(1-a)^2 
    u[i] <- u[i-1] + b[i-1] + (1-a^2)*e[i-1] 
    e[i] <- x[i] - u[i] 
  } 
  mp <- safe_mape(abs(e[2:n]), x[2:n]) 
  if (!is.na(mp) && mp < best5$mape) 
    best5 <- list(mape=mp, a=a, u=u, b=b, e=e) 
} 
m5 <- list( 
  name  = "Brown linear (линейный метод)", 
  param = paste0("a=", best5$a), 
  mape  = best5$mape, 
  rmse  = sqrt(mean(best5$e[2:n]^2)), 
  nxt   = best5$u[n] + best5$b[n] + (1-best5$a^2)*best5$e[n], 
  u     = best5$u, e = best5$e 
) 

# ============================================================ 
# 9. ТАБЛИЦА И ПОБЕДИТЕЛЬ 
# ============================================================ 
all_m <- list(m1, m2, m3, m4, m5) 

tab <- data.frame( 
  Метод    = sapply(all_m, function(r) r$name), 
  Параметр = sapply(all_m, function(r) r$param), 
  MAPE     = round(sapply(all_m, function(r) r$mape), 4), 
  RMSE     = round(sapply(all_m, function(r) r$rmse), 4), 
  Прогноз  = round(sapply(all_m, function(r) r$nxt), 4), 
  stringsAsFactors = FALSE 
) 

cat("\n===== ТАБЛИЦА =====\n") 
print(tab, row.names = FALSE) 

has_zeros <- any(x == 0) 

if (has_zeros) { 
  cat("\nВ ряду есть нули → победитель по RMSE\n") 
  crit      <- sapply(all_m, function(r) r$rmse) 
  crit_name <- "RMSE" 
} else { 
  cat("\nПобедитель по MAPE\n") 
  crit      <- sapply(all_m, function(r) r$mape) 
  crit_name <- "MAPE" 
} 

i_best <- which.min(crit) 
best   <- all_m[[i_best]] 

cat("\n===== ПОБЕДИТЕЛЬ =====\n") 
cat("Критерий:", crit_name, "\n") 
cat("Лучший метод:", best$name, "\n") 
cat("Минимальный", crit_name, ":", round(crit[i_best], 4), "\n") 
cat("Прогноз на t+1:", round(best$nxt, 4), "\n") 

# ============================================================ 
# 10. ГРАФИКИ 
# ============================================================ 
par(mfrow = c(2, 2)) 

plot(x, type = "o", col = "blue", pch = 16, 
     main = paste("Ряд:", sheet_name), xlab = "t", ylab = "x") 
grid() 

plot(x, type = "o", col = "blue", pch = 16, 
     ylim = range(c(x, best$u, best$nxt), na.rm = TRUE), 
     main = paste("Лучший:", best$name), xlab = "t", ylab = "x") 
lines(best$u, col = "red", type = "o", pch = 17) 
points(n + 1, best$nxt, col = "darkred", pch = 18, cex = 1.6) 
legend("topleft", 
       legend = c("факт", "прогноз", "t+1"), 
       col    = c("blue", "red", "darkred"), 
       pch    = c(16, 17, 18), lty = c(1, 1, NA)) 
grid() 

plot(best$e, type = "o", col = "darkgreen", pch = 16, 
     main = "Ошибки лучшего метода", xlab = "t", ylab = "e") 
abline(h = 0, lty = 2, col = "gray") 
grid() 

if (has_zeros) { 
  barplot(tab$RMSE, names.arg = c("M1","M2","M3","M4","M5"), 
          col = c("steelblue","orange","tomato","seagreen","purple"), 
          main = "RMSE по методам", ylab = "RMSE") 
  abline(h = min(tab$RMSE), col = "red", lty = 2) 
} else { 
  barplot(tab$MAPE, names.arg = c("M1","M2","M3","M4","M5"), 
          col = c("steelblue","orange","tomato","seagreen","purple"), 
          main = "MAPE по методам, %", ylab = "MAPE, %") 
  abline(h = min(tab$MAPE), col = "red", lty = 2) 
} 