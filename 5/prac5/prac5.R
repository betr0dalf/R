# Загрузка данных ---------------------------------------------------------

if ("quantmod" %in% rownames(installed.packages()) == FALSE) {
  install.packages("quantmod") }
library(quantmod)
if ("stringr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("stringr") }
library(stringr)

# Мы хотим загрузить акции с данными наименованиями в yahoo
# "ATVI" не загружался, поэтоу загружаем только "^IXIC"
downloadable_stocks <- c("^IXIC")

# Функция получения фреймов с данными
quantmod::getSymbols(Symbols = downloadable_stocks,
                     src = "yahoo",
                     from = as.Date.character("1900-01-01"))

# Функция get() позволяет получить содержимое объекта по его названию-строке
# Мы можем и не знать названия акций в скрипте, но всё равно работать с ними
# при пользовательском вводе названий
df <- data.frame(get('IXIC'))

# Применяем регулярное выражение для поиска и удаления ненужных символов
downloadable_stocks <- stringr::str_remove(downloadable_stocks,
                                           "[:punct:\\^]")
# Удалим полученные объекты
rm(list = downloadable_stocks)

# Часть 2 -----------------------------------------------------------------

# Функция Альтера-Джонса --------------------------------------------------

# Задание 1 ---------------------------------------------------------------

out_of_trend = function(x, dt, method = "Arifm") {
  if (!is.numeric(x) || !is.numeric(dt)) {
    stop("Input vectors must be numeric")
  }
  if (length(x) < 3) {
    stop("Input vector length must be at least 3")
  }
  if (any(dt >= ceiling(length(x) / 2) - 1)) {
    stop("dt incorrect")
  }
  
  min_value = min(x) + 1
  x_shifted = x + min_value
  y = numeric(length(x))
  
  for (i in 1:length(x)) {
    if (method == "Arifm") {
      if (i - dt >= 1 && i + dt <= length(x)) {
        y[i] = log((x_shifted[i - dt] + x_shifted[i + dt]) / (2 * x_shifted[i]))
      }
    } else if (method == "Geom") {
      if (i - dt >= 1 && i + dt <= length(x)) {
        y[i] = log((x_shifted[i - dt] * x_shifted[i + dt]) / (x_shifted[i]**2))
      }
    } else if (method == "Garm") {
      if (i - dt >= 1 && i + dt <= length(x)) {
        y[i] = log(2 * (x_shifted[i - dt] * x_shifted[i + dt]) / (x_shifted[i] * (x_shifted[i - dt] + x_shifted[i + dt])))
      }
    } else {
      stop("Invalid method specified")
    }
  }
  
  return(y[y != 0])
}

first_local_min = function(x, dx) {
  for (i in (1 + dx):(length(x) - dx)) {
    if (x[i - dx] > x[i] & x[i] < x[i + dx]) {
      return(i)
    }
  }
}

t = seq(0, 10, 0.1)
x = 2 * t + 3 + sin(2 * t)

mean(x)

xn = out_of_trend(x, 10)

mean(xn)

alter_johns = function(y) {
  
  a = numeric(length(y))
  
  for (tau in 1:(length(y) - 1)) {
    
    val = 0.0
    
    for (i in 1:(length(y) - tau)) {
      val = val + abs(y[i + tau] - y[i])
    }
    
    val = val * (1 / (length(y) - tau))
    a[tau] = val
  }
  
  return(a[a != 0])
}

xn_a = alter_johns(xn)

first_local_min(xn_a, 5)

a = out_of_trend(unlist(df['IXIC.Adjusted']), 1000)

a_alter = alter_johns(a)

plot(unlist(df['IXIC.Adjusted']))
plot(a)
plot(a_alter)

# Создаем пустой график с нужным нам диапазоном значений по оси Y
plot(unlist(df['IXIC.Adjusted']), type='n', ylim=c(min(unlist(df['IXIC.Adjusted'])), max(unlist(df['IXIC.Adjusted']), a, a_alter)), xlab='Index', ylab='IXIC.Adjusted', main='Combined Plot')

# Добавляем первый график
lines(unlist(df['IXIC.Adjusted']), col='blue')

# Добавляем второй график с отдельной осью Y
par(new=TRUE)
plot(1:length(a), a, type='l', col='red', axes=FALSE, xlab='', ylab='')
axis(side=4, col='red', ylim=c(min(a), max(a)))
mtext("a", side=4, line=3, col='red')

# Добавляем третий график с отдельной осью Y
par(new=TRUE)
plot(1:length(a_alter), a_alter, type='l', col='green', axes=FALSE, xlab='', ylab='')
axis(side=4, col='green', ylim=c(min(a_alter), max(a_alter)))
mtext("a_alter", side=4, line=2, col='green')

first_local_min(a_alter, 3)

# Часть 3 -----------------------------------------------------------------

SIM = function(A, u0, f, n_iter = 1e5, eps = 1e-2) {
  if (length(u0) != ncol(A) || length(f) != nrow(A)) {
    stop("Incorrect input")
  }
  
  maximum = max(A)
  A = A / maximum
  f = f / maximum
  
  B = diag(1, ncol(A), ncol(A)) - A
  
  u = u0
  iter = 0
  
  while (iter < n_iter) {
    u_new = B %*% u + f
    
    if (max(abs(u_new - u)) < eps) {
      print(iter+1)
      return(u_new)
    }
    
    u = u_new
    iter = iter + 1
  }
  
  return(u)
}

A = A = diag(x = c(4, 9), nrow = 2, ncol = 2)
u0 <- c(0, 0)
f <- c(4, 2)
SIM(A, u0, f)
