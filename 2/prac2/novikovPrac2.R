# Часть 1 -----------------------------------------------------------------

# Задание 1 ---------------------------------------------------------------

A <- matrix(3, 3, 4)
A
A[1,3] <- 4
A[2,1] <- 1
A[3,2] <- NA
A[3,4] <- 1
A

# Задание 2 ---------------------------------------------------------------

a <- c(1, 3, 4, 9, NA)
b <- c(5, 6, 7, 0, 2)
c <- c(9, 10, 13, 1, 20)

B <- matrix(c(a, b, c), nrow =  5, byrow = T, dimnames = list(c("A1", "A2", "A3", "A4", "A5"), c("B1", "B2", "B3")))
B
C <- matrix(c(a, b, c), nrow =  3, byrow = T, dimnames = list(c("row1", "row2", "row3"), c("col1", "col2", "col3", "col4", "col5")))
C

# Задание 3 ---------------------------------------------------------------

names <- c("Jane", "Michael", "Mary", "George")
ages <- c(8, 6, 28, 45)
gender <- c(0, 1, 0, 1)

D <- cbind(names, ages, gender)
D
ages_sq <- ages^2
D <- cbind(D, ages_sq)
D
D[,1]
as.integer(D[, 2])
as.integer(D[, 3])
as.integer(D[, 4])

# Задание 4 ---------------------------------------------------------------

info <- list(names = names, ages = ages, gender = gender)
info$names[info$names == "Michael"]
info$gender
names(info) <- c("name", "age", "gender")
info$name
drinks <- c("juice", "tea", "rum", "coffee")
info$drinks <- drinks
new <- list(names = "John", age = 2, gender = 1, drinks = "milk")
info$name[5] <- new$name
info$age[5] <- new$age
info$gender[5] <- new$gender
info$drinks[5] <- new$drinks
info

# Задание 5 ---------------------------------------------------------------

s <- "a,b,c,d"
let <- strsplit(s, ",")
class(let)
s <- unlist(let)
s

index <- "0,72;0,38;0,99;0,81;0,15;0,22;0,16;0,4;0,24"
temp <- strsplit(index, ";")
temp
class(temp)
temp <- sub(",", ".", unlist(temp))
temp
I <- as.numeric(temp)
I

# Часть 2 -----------------------------------------------------------------

# Задание 1 ---------------------------------------------------------------

A <- diag(c(4, 9), 2, 2)
rownames(A) <- c("eq1", "eq2")
colnames(A) <- c("x1", "x2")
A

# Задание 2 ---------------------------------------------------------------

e <- eigen(A)
e$values
# собственные значения равны 9 и 4
# так как являются решением уравнения(4-λ)*(9-λ) = 0

# Задание 3 ---------------------------------------------------------------

I <- diag(2)
I
B <- I - A
B

# Задание 4 ---------------------------------------------------------------

f <- c(4, 2)
f
u <- c(0.2, -0.3)
u

# Задание 5 ---------------------------------------------------------------

u_result <- solve(A, f)
u_result

# Задание 6 ---------------------------------------------------------------

results <- matrix(0, nrow = 2, ncol = 7)

for (i in 1:7) {
  u <- B %*% u + f
  results[, i] <- u
}

results

# Задание 7 ---------------------------------------------------------------

difference <- abs(results[, 7] - u_result)
difference

# Задание 8 ---------------------------------------------------------------

max_val <- max(A)
max_val
A
A <- A / max_val
A
f
f <- f / max_val
f

# Задание 9 ---------------------------------------------------------------

u2 <- c(0.2, -0.3)

e2 <- eigen(A)
e2$values

I2 <- diag(2)
I2
B2 <- A - I
B2

u_result2 <- solve(A, f)
u_result2

results2 <- matrix(0, nrow = 2, ncol = 7)

for (i in 1:7) {
  u2 <- B2 %*% u2 + f
  results2[, i] <- u2
}

results2

difference2 <- abs(results2[, 7] - u_result2)
difference2

# Задание 10 --------------------------------------------------------------

difference3 <- abs(difference - difference2)
difference3

# Часть 3 -----------------------------------------------------------------

step <- 1 # Шаг сетки
dekart_begin <- -5 # Начало сетки
dekart_end <- 5 # Конец сетки

# Задание сеточной поверхности
x <- seq(from = dekart_begin, to = dekart_end, by = step)
y <- x

# Задание двумерной функции на координатной сетке
surface_matrix <- outer(X = x,
                        Y = y,
                        FUN = function(x,y) Re(exp(-1i * 0.5 * x * y)))
dimnames(surface_matrix) <- list(x, y)

# Задание 1 ---------------------------------------------------------------

# Количество элементов матрицы
write(paste("number of matrix elements:", length(surface_matrix)), "summary.txt")

file_conn <- file("summary.txt", "a")

# Размерность строк и столбцов
writeLines(paste("number of rows:", nrow(surface_matrix)), file_conn)
writeLines(paste("number of cols:", ncol(surface_matrix)), file_conn)

# Сумма элементов главной диагонали
writeLines(paste("sum of main diag elements:", sum(diag(surface_matrix))), file_conn)

# Сумма элементов серединного среза по строкам
writeLines(paste("sum of middle row elements:", sum(surface_matrix[nrow(surface_matrix) %/% 2, ])), file_conn)

# Сумма элементов серединного среза по столбцам
writeLines(paste("sum of middle column elements:", sum(surface_matrix[, ncol(surface_matrix) %/% 2])), file_conn)

# Суммы строк матрицы
writeLines(paste("row sums:", rowSums(surface_matrix)), file_conn)

# Суммы столбцов матрицы
writeLines(paste("col sums:", colSums(surface_matrix)), file_conn)

close(file_conn)

# Задание 2 ---------------------------------------------------------------

# Для генерации отчёта закомментировал эти строки
# step2 <- as.numeric(readline(prompt = "Введите шаг сетки: ")) # Шаг сетки
# dekart_begin2 <- as.numeric(readline(prompt = "Введите начало сетки: ")) # Начало сетки
# dekart_end2 <- as.numeric(readline(prompt = "Введите конец сетки: ")) # Конец сетки

# Введём пример данных, которые может ввести пользователь:
step2 <- 2 # Шаг сетки
dekart_begin2 <- -18 # Начало сетки
dekart_end2 <- 18 # Конец сетки

# Задание сеточной поверхности
x2 <- seq(from = dekart_begin2, to = dekart_end2, by = step2)
y2 <- x2

# Задание двумерной функции на координатной сетке
surface_matrix2 <- outer(X = x2,
                        Y = y2,
                        FUN = function(x2,y2) Re(exp(-1i * 0.5 * x2 * y2)))
dimnames(surface_matrix2) <- list(x2, y2)

# Количество элементов матрицы
write(paste("number of matrix elements:", length(surface_matrix2)), "summary2.txt")

file_conn <- file("summary2.txt", "a")

# Размерность строк и столбцов
writeLines(paste("number of rows:", nrow(surface_matrix2)), file_conn)
writeLines(paste("number of cols:", ncol(surface_matrix2)), file_conn)

# Сумма элементов главной диагонали
writeLines(paste("sum of main diag elements:", sum(diag(surface_matrix2))), file_conn)

# Суммы строк матрицы
writeLines(paste("row sums:", rowSums(surface_matrix2)), file_conn)

# Суммы столбцов матрицы
writeLines(paste("col sums:", colSums(surface_matrix2)), file_conn)

close(file_conn)

# Задание 3 ---------------------------------------------------------------

# Считываем данные из файла "inputs.txt"
params <- scan("inputs.txt", what = double())
params

# Извлекаем параметры сетки
step_x3 <- params[1]
dekart_begin_x3 <- params[2]
dekart_end_x3 <- params[3]
step_y3 <- params[4]
dekart_begin_y3 <- params[5]
dekart_end_y3 <- params[6]

# Создаем координатную сетку
x3 <- seq(from = dekart_begin_x3, to = dekart_end_x3, by = step_x3)
y3 <- seq(from = dekart_begin_y3, to = dekart_end_y3, by = step_y3)

# Создаем матрицу функции на сетке
surface_matrix3 <- outer(X = x3,
                         Y = y3,
                         FUN = function(x3,y3) Re(exp(-1i * 0.5 * x3 * y3)))
dimnames(surface_matrix3) <- list(x3, y3)

# Количество элементов матрицы
write(paste("number of matrix elements:", length(surface_matrix3)), "summary3.txt")

file_conn <- file("summary3.txt", "a")

# Размерность строк и столбцов
writeLines(paste("number of rows:", nrow(surface_matrix3)), file_conn)
writeLines(paste("number of cols:", ncol(surface_matrix3)), file_conn)

# Сумма элементов главной диагонали
writeLines(paste("sum of main diag elements:", sum(diag(surface_matrix3))), file_conn)

# Суммы строк матрицы
writeLines(paste("row sums:", rowSums(surface_matrix3)), file_conn)

# Суммы столбцов матрицы
writeLines(paste("col sums:", colSums(surface_matrix3)), file_conn)

close(file_conn)

# Машины ------------------------------------------------------------------

cars_matrix <- as.matrix(cars)
cars_matrix

# Задание 1 ---------------------------------------------------------------

cars_speed <- cbind(rep(1, nrow(cars_matrix)), cars_matrix[, 1])
cars_speed

cars_dist <- cars_matrix[, 2]
cars_dist

alpha <- solve(t(cars_speed) %*% cars_speed) %*% t(cars_speed) %*% cars_dist
alpha

class(alpha)
typeof(alpha)
alpha <- as.vector(alpha)
alpha

alpha_c <- alpha[1]
alpha_x <- alpha[2]
cat("alpha_c = ", alpha_c, "\n")
cat("alpha_x = ", alpha_x, "\n")

cars_speed_lm <- cars_matrix[,1]
cars_speed_lm

cars_dist_lm <- alpha_c + cars_speed_lm * alpha_x
cars_dist_lm

dist_residuals <- cars_dist_lm - cars_dist
dist_residuals

mean_res <- mean(dist_residuals)
mean_res
sd_res <- sd(dist_residuals)
sd_res

sorted_cars_dist_lm <- sort(cars_dist_lm)
print(sorted_cars_dist_lm)

cat("Среднее отклонение вектора dist_residuals: ", mean_res, "\n")
cat("Стандартное отклонение вектора dist_residuals: ", sd_res, "\n")