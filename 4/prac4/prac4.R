# Задание 1 ---------------------------------------------------------------

# 1 -----------------------------------------------------------------------
if (!require("readxl")) install.packages("readxl")
if (!require("lubridate")) install.packages("lubridate")
library(readxl)
library(lubridate)
gas <- read_excel('GAZ.xlsx',
                  col_types = c('date', 'numeric', 'numeric', 'numeric',
                                'numeric', 'numeric', 'text', 'text', 'text'))
colnames(gas) <- c("Date", "Preasure_MPa", "Temp_C", "Gas_Prod_m3_per_d",
                   "Cond_m3_per_d", "Water_m3_per_d", "ID", "Bush", "Group")

# 2 -----------------------------------------------------------------------

nrow(gas)
gas <- gas[complete.cases(gas), ]
nrow(gas)

# 3 -----------------------------------------------------------------------

colnames(gas)[colnames(gas) == "Temp_C"] <- "Temp_K"
gas$Temp_K <- gas$Temp_K + 273.15
head(gas)

# 4 -----------------------------------------------------------------------

gas$ID <- as.factor(gas$ID)
gas$Bush <- as.factor(gas$Bush)
gas$Group <- as.factor(gas$Group)
str(gas)

# 5 -----------------------------------------------------------------------

gas$Gas_Cond_Ratio <- gas$Gas_Prod_m3_per_d / gas$Cond_m3_per_d
gas$Gas_Water_Ratio <- gas$Gas_Prod_m3_per_d / gas$Water_m3_per_d
gas$Water_Cond_Ratio <- gas$Water_m3_per_d / gas$Cond_m3_per_d
head(gas)

# 6 -----------------------------------------------------------------------

gas_2018 <- subset(gas, year(Date) == 2018)
nrow(gas_2018)

# 7 -----------------------------------------------------------------------

gas_111 = gas[gas$ID == 111, ]
nrow(gas_111)

# 8 -----------------------------------------------------------------------

k_more_than_2 <- table(subset(gas, Water_m3_per_d > 2)$ID)
ID_less_than_2 <- names(k_more_than_2[k_more_than_2 == 0])
ID_less_than_2
length(ID_less_than_2)

# 9 -----------------------------------------------------------------------

k_less_than_1000 <- table(subset(gas, Gas_Prod_m3_per_d < 1000)$ID)
ID_more_than_1000 <- names(k_less_than_1000[k_less_than_1000 == 0])
ID_more_than_1000
length(ID_more_than_1000)

max <- 12500
k_less_than_1000 <- table(subset(gas, Gas_Prod_m3_per_d < max)$Bush)
Bush_more_than_1000 <- names(k_less_than_1000[k_less_than_1000 == 0])
Bush_more_than_1000
max

# 10 ----------------------------------------------------------------------

gas_2018 <- subset(gas, year(Date) == 2018)
agg_data <- aggregate(Gas_Prod_m3_per_d ~ Group, data = gas_2018, sum)
colnames(agg_data)[2] <- "Total_Prod_2018"
agg_data
max_group <- agg_data[which.max(agg_data$Total_Prod_2018), 1]
max_group

# 11 ----------------------------------------------------------------------

gas_2018 <- subset(gas, year(Date) == 2018)
agg_data <- aggregate(Water_m3_per_d ~ Bush, data = gas_2018, sum)
colnames(agg_data)[2] <- "Total_Prod_2018"
agg_data
max_bush <- agg_data[which.max(agg_data$Total_Prod_2018), 1]
max_bush

# 12 ----------------------------------------------------------------------

gas_data <- gas[is.finite(gas$Gas_Water_Ratio), ]
agg_data <- aggregate(Gas_Water_Ratio ~ Bush, data = gas_data, mean)
colnames(agg_data)[2] <- "Mean_Gas_Water_Ratio"
agg_data
max_mean_bush <- agg_data[which.max(agg_data$Mean_Gas_Water_Ratio), 1]
max_mean_bush
