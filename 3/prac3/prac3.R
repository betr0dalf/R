# Часть 1 -----------------------------------------------------------------

# Задание 1 ---------------------------------------------------------------
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

# Задание 2 ---------------------------------------------------------------

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

if (!require("randomNames")) install.packages("randomNames")
library(randomNames)

# Задание 2 ---------------------------------------------------------------

set.seed(1234)
names <- randomNames(100, which.names = "first", ethnicity = 4)
names

# Задание 3 ---------------------------------------------------------------

ages <- sample(16:75, 100, replace = TRUE)
views <- c("right", "left", "moderate", "indifferent")
polit <- sample(views, 100, replace = TRUE)
data <- data.frame(names, ages, polit)
data

# Задание 4 ---------------------------------------------------------------

data$id <- 1:nrow(data)
data

# Задание 5 ---------------------------------------------------------------

people_25_30 <- sum(data$ages >= 25 & data$ages <= 30)
people_25_30

percentage_people_25_30 <- round(people_25_30 / nrow(data) * 100, 1)
percentage_people_25_30

# Задание 6 ---------------------------------------------------------------

data$polit_views <- as.factor(data$polit)
data

levels(data$polit_views)

# Часть 3 -----------------------------------------------------------------

# Задание 1 ---------------------------------------------------------------

if ("car" %in% installed.packages() == FALSE) {
  install.packages("car")
}

library(car)

firms <- Ornstein
firms
View(firms)

# Задание 2 ---------------------------------------------------------------

dim(firms)
names(firms)

# Задание 3 ---------------------------------------------------------------

complete_rows <- complete.cases(firms)
sum(complete_rows)
firms[!complete_rows, ]

# Задание 4 ---------------------------------------------------------------

# a
filtered_firms_a <- subset(firms, assets >= 10000 & assets <= 20000)
filtered_firms_a

# b
filtered_firms_b <- subset(firms, interlocks <= 30)
filtered_firms_b

# c
filtered_firms_c <- subset(firms, sector == "TRN" & nation == "CAN")
filtered_firms_c

# Задание 5 ---------------------------------------------------------------

firms$log_assets <- log(firms$assets)
firms

# Задание 6 ---------------------------------------------------------------

if (!require("VIM")) install.packages("VIM")
library(VIM)
VIM::aggr(firms, numbers = TRUE, cex.axis = 0.6, cex.names = 0.6, las = 1, ylab = "Variables", xlab = "Missingness", ylabs = c("Proportion of missing values", "Number of missing values"))

# Задание 7 ---------------------------------------------------------------

firms <- na.omit(firms)
firms

# Задание 8 ---------------------------------------------------------------

if (!require("foreign")) install.packages("foreign")
library(foreign)
write.dta(firms, "Firms.dta")

# Часть 3 -----------------------------------------------------------------

if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("stringr")) install.packages("stringr")
library(dplyr)
library(readr)
library(stringr)

# Задание 1 ---------------------------------------------------------------

covid19_data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
View(covid19_data)

# Задание 2 ---------------------------------------------------------------

dim(covid19_data)
names(covid19_data)
str(covid19_data)

# Задание 3 ---------------------------------------------------------------

if (!require("tidyr")) install.packages("tidyr")
library(tidyr)

covid19_data <- covid19_data %>%
  unite("Country.Region", c(Country.Region, Province.State), sep = ", ", remove = TRUE)
View(covid19_data)

# Задание 4 ---------------------------------------------------------------

# Преобразование данных
covid19_data_melted <- covid19_data %>%
  select(-c(Lat, Long)) %>%
  pivot_longer(-c(Country.Region), names_to = "Date", values_to = "Confirmed")
View(covid19_data_melted)

# Вычисление суммы, среднего и стандартного отклонения числа заболевших
covid19_data_summary <- covid19_data_melted %>%
  group_by(Country.Region) %>%
  summarise(
    Sum_Confirmed = sum(Confirmed, na.rm = TRUE),
    Mean_Confirmed = mean(Confirmed, na.rm = TRUE),
    SD_Confirmed = sd(Confirmed, na.rm = TRUE)
  )
View(covid19_data_summary)

# Объединение исходных данных с вычисленными значениями
covid19_data_table1 <- covid19_data %>%
  select(Country.Region, Lat, Long) %>%
  left_join(covid19_data_summary, by = c("Country.Region" = "Country.Region"))
View(covid19_data_table1)

# Задание 5 ---------------------------------------------------------------

# Преобразование данных
covid19_data_table2 <- covid19_data %>%
  select(-c(Lat, Long)) %>%
  pivot_longer(-c(Country.Region), names_to = "Date", values_to = "Confirmed") %>%
  mutate(Date = as.Date(gsub("/", "-", Date), format = "%m-%d-%y")) %>%
  pivot_wider(names_from = Country.Region, values_from = Confirmed, values_fn = sum)

# Переименование столбцов
names(covid19_data_table2) <- c("Date", str_replace_all(names(covid19_data_table2)[-1], " ", ""))
View(covid19_data_table2)

# Задание 6 ---------------------------------------------------------------

if (!dir.exists("data_output")) {
  dir.create("data_output")
}

write.table(covid19_data_table2, file = "data_output/covid19_data_table2.txt", sep = "\t", row.names = FALSE)

write.csv(covid19_data_table2, file = "data_output/covid19_data_table2.csv", row.names = FALSE)

if (!require("writexl")) install.packages("writexl")
library(writexl)

write_xlsx(covid19_data_table2, path = "data_output/covid19_data_table2.xlsx")
