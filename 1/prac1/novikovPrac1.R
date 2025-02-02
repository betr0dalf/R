# Часть 1 -----------------------------------------------------------------

# Задание 1 ---------------------------------------------------------------

x <- 2
y <- 4
x
y
z <- x
x <- y
y <-z
x
y

x2 <- 2
y2 <- 4
x2
y2
x2 <- x2 + y2
y2 <- x2 - y2
x2 <- x2 - y2
x2
y2

# Задание 2 ---------------------------------------------------------------

x <- 3.5
y <- "2,6"
z <- 1.78
h <- TRUE

typeof(x)
typeof(y)
class(z)
class(h)

h <- as.integer(h)
h

y <- sub(",", ".", y)
y <- as.numeric(y)
y

x <- as.character(x)
x

# Задание 3 ---------------------------------------------------------------

dohod <- 1573

dohod <- log(dohod)

# Задание 4 ---------------------------------------------------------------

N <- readLines(con = "test.txt", n = 1, encoding = "UTF-8")
N <- as.numeric(N)
print(2*N-1)

# Часть 2 -----------------------------------------------------------------

# Задание 1 ---------------------------------------------------------------

g <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)
g[1]
g[length(g)]
g[3:5]
g[g == 2 & !is.na(g)]
g[g > 4 & !is.na(g)]
g[g %% 3 == 0 & !is.na(g)]
g[g > 4 & g %% 3 == 0 & !is.na(g)]
g[(g < 1 | g > 5) & !is.na(g)]
which(g == 0)
which(g >= 2 & g <= 8)
sort(g[g != 2], na.last = T)

# Задание 2 ---------------------------------------------------------------

vec1 <- c(1, 2, 3, 4)
vec2 <- 1:10
vec1[length(vec1)] <- NA
vec1
vec2[length(vec2)] <- NA
vec2

# Задание 3 ---------------------------------------------------------------

vec3 <- c(0, NA, 2, 3, NA, NA, 8, 9)
which(is.na(vec3))

# Задание 4 ---------------------------------------------------------------

f <- c(NA, 0, NA, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)
sum(is.na(f))

# Задание 5 ---------------------------------------------------------------

id1 <- 1:100
id1

id2 <- sample(1:100)
id2

# Задание 6 ---------------------------------------------------------------

country  <- c("France", "Italy", "Spain")
country
years <- 2017:2020
years

# Задание 7 ---------------------------------------------------------------

income <- c(10000, 32000, 28000, 150000, 65000, 1573)
income
avg <- sum(income) / length(income)
avg
income_class <- ifelse(income < avg, 0, 1)
income_class

# Задание 8 ---------------------------------------------------------------

coords <- as.numeric(readLines("coords.txt", 14))
coords
p <- 5.76
result <- sum(abs(coords)^p)^(1/p)
result
write(result, "result.txt")

# Задание 9 ---------------------------------------------------------------

coords <- as.numeric(readLines("coords.txt", 14))
if(length(coords) <= 2){
  print("Невозможно посчитать разности, слишком мало значений.")
} else{
  coords
  first_diff <- diff(coords)
  first_diff
  second_diff <- diff(coords, 2)
  second_diff
  all_diff <-c(first_diff, second_diff)
  all_diff
  write(all_diff, "diff_vectors.txt")
}
