# Задание 1 ---------------------------------------------------------------

# Обработка данных --------------------------------------------------------

download.file(url = 'https://raw.githubusercontent.com/qwerty29544/RpracticeBook/master/2Data/01FlatTables/ECG_yurchenkov.txt', 
              destfile = 'my_file.txt')

df = read.csv('my_file.txt', skip=47, sep='\t', header=F)

colnames(df) = c('мс', 'ЭКГ', 'ФПГ1', 'ФПГ2', 'КГР', 'РД')

print(paste0('Num of observations: ', sum(df['мс'] == 0)))

split_indices <- c(which(df$мс == 0), nrow(df)+1)
split_indices

sub_datasets <- list()

for (i in 1:(length(split_indices)-1)) {
  sub_df <- df[(split_indices[i]):((split_indices[i+1])-1), ]
  sub_datasets[[i]] <- sub_df
}

for (i in 1:(length(sub_datasets) - 1)) {
  sub_datasets[[i]] = sub_datasets[[i]][1:(nrow(sub_datasets[[i]]) - 6),]
}

for (i in 1:length(sub_datasets)) {
  for (col in names(sub_datasets[[i]])) {
    sub_datasets[[i]][[col]] <- gsub(",", ".", sub_datasets[[i]][[col]])
  }
}

for (i in 1:length(sub_datasets)) {
  for (col in names(sub_datasets[[i]])) {
    sub_datasets[[i]][[col]] <- as.numeric(sub_datasets[[i]][[col]])
  }
}

all_df = data.frame()

for (cur_df in sub_datasets) {
  all_df = rbind(all_df, cur_df)
}

head(all_df)

# Вывод гистограмм --------------------------------------------------------

hist(unlist(all_df['ЭКГ']),
     main = "ЭКГ общее",
     xlab = "Values",
     ylab = "Frequency",
     col = "skyblue",
     border = "black"
)

hist(unlist(sub_datasets[[1]]['ЭКГ']),
     main = "ЭКГ эксперимент 1",
     xlab = "Values",
     ylab = "Frequency",
     col = "skyblue",
     border = "black"
)

hist(unlist(all_df['РД']),
     main = "РД общее",
     xlab = "Values",
     ylab = "Frequency",
     col = "skyblue",
     border = "black"
)

hist(unlist(sub_datasets[[13]]['РД']),
     main = "РД эксперимент 13",
     xlab = "Values",
     ylab = "Frequency",
     col = "skyblue",
     border = "black"
)

par(mfrow = c(1, 3))

for (i in 1:3) {
  hist(unlist(sub_datasets[[i]]['ФПГ2']), 
       main = paste("ФПГ2 эксперимент", i),
       xlab = "Values",
       ylab = "Frequency",
       col = "skyblue",
       border = "black"
  )
}

par(mfrow = c(1, 1))

# Статистика --------------------------------------------------------------

print('Статистика для ЭКГ (все эксперименты)')
summary(unlist(all_df['ЭКГ']))

for (i in 1:13) {
  print(paste0('Статистика для ЭКГ (эксперимент ', i, ')'))
  print(summary(unlist(sub_datasets[[i]]['ЭКГ'])))
}

# Моя функция -------------------------------------------------------------

myfunc = function(y) {
  
  a = numeric(length(y))
  
  for (tau in 1:(length(y) - 2)) {
    
    val = 0.0
    
    for (i in 1:(length(y) - tau)) {
      val = val + abs(y[i + tau] - y[i])
    }
    
    val = val * (2 / (length(y) - tau))
    a[tau] = val
  }
  
  return(a[a != 0])
}

line = unlist(sub_datasets[[1]]['ЭКГ'])
plot(line, type='l')

line = myfunc(line)
plot(line, type='l')

line = unlist(sub_datasets[[1]]['РД'])
plot(line, type='l')

line = myfunc(line)
plot(line, type='l')
