# Установка библиотек --------------------------------------------

if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("ggplot2") }
library(ggplot2)

if("readxl" %in% rownames(installed.packages()) == FALSE) {
  install.packages("readxl") }
library(readxl)

if("dplyr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("dplyr") }
library(dplyr)


# Часть 1 -----------------------------------------------------------------

# Задание 1 ---------------------------------------------------------------

# 1 -----------------------------------------------------------------------

demography = read.csv("https://raw.githubusercontent.com/allatambov/R-programming-3/master/seminars/sem8-09-02/demography.csv", sep=",")
head(demography)

str(demography)

# 2 -----------------------------------------------------------------------

demography$young_share <- demography$young_total / demography$popul_total * 100
demography$trud_share <- demography$wa_total / demography$popul_total * 100
demography$old_share <- (demography$popul_total - demography$young_total - demography$wa_total) / demography$popul_total * 100
tail(demography[, c(2, (ncol(demography) - 2):ncol(demography))])

# 3 -----------------------------------------------------------------------

median_trud_share <- median(demography$trud_share)
ggplot(demography, aes(x = trud_share)) +
  geom_histogram(fill = "steelblue", binwidth = 1, color = "white") +
  geom_rug(alpha = 0.5, sides = "b") +
  geom_vline(xintercept = median_trud_share, color = "red", linetype = "dashed", size = 1) +
  labs(x = "Доля трудоспособного населения, %", y = "Частота", title = "Гистограмма доли трудоспособного населения") +
  scale_x_continuous(limits = c(50, 60)) + 
  theme_minimal()

# 4 -----------------------------------------------------------------------

ggplot(demography, aes(x = trud_share, fill = region, alpha = region)) +
  geom_density(color = "black") +
  scale_fill_manual(values = c("red", "green")) + 
  scale_alpha_manual(values = c(0.5, 0.5)) +
  labs(x = "Доля трудоспособного населения, %",
       y = "Плотность",
       fill = "Регион",
       alpha = "Регион", 
       title = "Графики плотности распределения доли трудоспособного населения") +
  theme_minimal()

ggplot(demography, aes(x = region, y = trud_share, fill = region, alpha = region)) +
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("red", "green")) +
  scale_alpha_manual(values = c(0.5, 0.5)) +
  labs(x = "Регион",
       y = "Доля трудоспособного населения, %",
       fill = "Регион",
       alpha = "Регион",
       title = "Скрипичные диаграммы доли трудоспособного населения") +
  theme_minimal()

ggplot(demography, aes(x = region, y = trud_share, fill = region, alpha = region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "green")) +
  scale_alpha_manual(values = c(0.5, 0.5)) +
  labs(x = "Регион",
       y = "Доля трудоспособного населения, %",
       fill = "Регион",
       alpha = "Регион",
       title = "Ящики с усами доли трудоспособного населения") +
  theme_minimal()

# 5 -----------------------------------------------------------------------

ggplot(demography, aes(x = young_share, y = old_share)) +
  geom_point(color = "steelblue", shape = 17, size = 3) +
  labs(x = "Процент молодого населения, %",
  y = "Процент пожилых людей, %",
  title = "Диаграмма рассеяния для переменных young_share и old_share") +
  theme_minimal()

cor.test(demography$young_share,demography$old_share, method = "spearman")

print("Заметна статистически значимая отрицательная корреляция")

# 6 -----------------------------------------------------------------------

demography$male_share <- ((demography$ret_male + demography$wa_male + demography$young_male) / 
                            demography$popul_total) * 100
demography$male <- ifelse(demography$male_share > (100 - demography$male_share), 1, 0)
head(demography[, c(2, (ncol(demography) - 1):ncol(demography))])

# 7 -----------------------------------------------------------------------

ggplot(demography, aes(x = young_share, y = old_share, size = male_share, color = factor(male))) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(1, 10), name = "Доля мужчин") +
  labs(x = "Доля молодого населения",
  y = "Доля пожилого населения",
  color = "Преобладание мужчин") +
  theme_minimal()

regions <- data.frame(table(demography$region))
colnames(regions) <- c("region", "count")
regions$label <- paste0("Количество: ", regions$count)

ggplot(regions, aes(x = region, y = count, fill = region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label), vjust = -0.5, size = 4) +
  labs(x = "Область", y = "Количество районов") +
  theme_minimal()

# Часть 2 -----------------------------------------------------------------

# Задание 1 ---------------------------------------------------------------

data = mtcars
head(data)

data$am_factor <- factor(mtcars$am, levels = c(0, 1), labels = c("ручная", "автоматическая"))

ggplot(data, aes(x = hp, y = wt, size = cyl, color = am_factor)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(name = "Число цилиндров") +
  scale_color_manual(name = "Тип коробки передач", values = c("ручная" = "red", "автоматическая" = "green")) +
  labs(x = "Мощность двигателя (hp)", y = "Вес автомобиля (wt)") +
  ggtitle("Связь между мощностью двигателя и весом автомобиля") +
  theme_minimal()

# Задание 2 ---------------------------------------------------------------

if("gridExtra" %in% rownames(installed.packages()) == FALSE) {
  install.packages("gridExtra") }
library(gridExtra)

p1 = ggplot(subset(data, am == 0), aes(x = hp)) +
  geom_histogram(binwidth = 65, fill = "brown", color = "black") +
  scale_x_continuous(limits = c(0, 375)) +
  labs(title = "Automatic") + 
  theme_minimal() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

p2 = ggplot(subset(data, am == 1), aes(x = hp)) +
  geom_histogram(binwidth = 54, fill = "brown", color = "black") +
  scale_x_continuous(limits = c(0, 375)) +
  labs(title = "Mechanic") + 
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

grid.arrange(p1, p2, ncol = 2, top = "Gross HorsePower",
             left = "count", bottom = "HorsePower")

# Задание 3 ---------------------------------------------------------------

data("sleep")
sleep

ggplot(sleep, aes(x = group, y = extra, fill = group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "green")) +
  labs(x = "Группа испытуемых", y = "Дополнительное время сна", title = "Распределение дополнительного времени сна по группам испытуемых") +
  theme_minimal()

# Часть 3 -----------------------------------------------------------------

# Задание 1 ---------------------------------------------------------------

covid <- read_excel("covid.xlsx")
View(covid)

# Азербайджан -------------------------------------------------------------

ggplot(data=covid, mapping = aes(x=1:nrow(covid), y=covid[[20]])) +
  geom_line(lwd=I(0.5), lty=1) +
  geom_point(cex=0.5) +
  coord_cartesian(xlim=c(1,nrow(covid)), ylim=range(covid[[20]])) +
  labs(title="График заболеваемости Covid19 Азербайджан",
       subtitle = "Данные от 22-01-2020",
       x = "Кол-во дней с начала пандемии",
       y = "Суммарное число заболевших") +
  scale_x_continuous(breaks = seq(1,nrow(covid),365)) +
  scale_y_continuous(breaks = seq(0,400000,50000)) +
  theme_bw()

ggplot(data=covid, mapping = aes(covid[[20]])) +
  geom_histogram(bins=40, color="black", fill="grey") +
  geom_freqpoly(bins=40, color="red4", lwd=I(1.1)) +
  labs(title="Гистограмма заболевших Азербайджан",
       subtitle = "Данные от 22-01-2020",
       x = "Кол-во дней с начала пандемии",
       y = "Суммарное число заболевших") +
  theme_bw()

# Чад ----------------------------------------------------------------

ggplot(data=covid, mapping = aes(x=1:nrow(covid), y=covid[[59]])) +
  geom_line(lwd=I(0.5), lty=1) +
  geom_point(cex=0.5) +
  coord_cartesian(xlim=c(1,nrow(covid)), ylim=range(covid[[59]])) +
  labs(title="График заболеваемости Covid19 Чад",
       subtitle = "Данные от 22-01-2020",
       x = "Кол-во дней с начала пандемии",
       y = "Суммарное число заболевших") +
  scale_x_continuous(breaks = seq(1,nrow(covid),365)) +
  scale_y_continuous(breaks = seq(0,400000,50000)) +
  theme_bw()

ggplot(data=covid, mapping = aes(covid[[59]])) +
  geom_histogram(bins=40, color="black", fill="grey") +
  geom_freqpoly(bins=40, color="red4", lwd=I(1.1)) +
  labs(title="Гистограмма заболевших Чад",
       subtitle = "Данные от 22-01-2020",
       x = "Кол-во дней с начала пандемии",
       y = "Суммарное число заболевших") +
  theme_bw()

# Гуандун, Китай ----------------------------------------------------------------

ggplot(data=covid, mapping = aes(x=1:nrow(covid), y=covid[[66]])) +
  geom_line(lwd=I(0.5), lty=1) +
  geom_point(cex=0.5) +
  coord_cartesian(xlim=c(1,nrow(covid)), ylim=range(covid[[66]])) +
  labs(title="График заболеваемости Covid19 Гуандун, Китай",
       subtitle = "Данные от 22-01-2020",
       x = "Кол-во дней с начала пандемии",
       y = "Суммарное число заболевших") +
  scale_x_continuous(breaks = seq(1,nrow(covid),365)) +
  scale_y_continuous(breaks = seq(0,400000,50000)) +
  theme_bw()

ggplot(data=covid, mapping = aes(covid[[66]])) +
  geom_histogram(bins=40, color="black", fill="grey") +
  geom_freqpoly(bins=40, color="red4", lwd=I(1.1)) +
  labs(title="Гистограмма заболевших Гуандун, Китай",
       subtitle = "Данные от 22-01-2020",
       x = "Кол-во дней с начала пандемии",
       y = "Суммарное число заболевших") +
  theme_bw()
