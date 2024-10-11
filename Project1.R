main_dir <- dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(main_dir)
library(dplyr)
library(ggplot2)
library(tidyverse)
data_day <- read.csv("day.csv") 
str(data_day)
summary(data_day)

# 1) EDA
# Преобразуем переменную dteday из character в класс Date
data_day$dteday <- as.Date(data_day$dteday)
str(data_day)

# NA
# Заменим возможные пустые ячейки на NA
data_day_na <- data_day %>% 
  mutate(across(where(~ !is.Date(.)), ~ ifelse(. == "", NA, .)))
summary(data_day_na) # Как можно заметить пустых ячеек не было

# Попробуем убрать строки с NA 
sum(is.na(data_day_na)) #Всего у нас 9 пропусков. NA значения есть в 4 столбцах: temp, hum, windspeed, registered
data_day_na[!complete.cases(data_day),] # посмотрим строки, содержащие NA значения
bicycle_rent_clean <- na.omit(data_day_na)
summary(bicycle_rent_clean)

# Переименуем столбцы с временем года
bicycle_rent_clean_fix <- bicycle_rent_clean %>% mutate(season = case_when(
  season == 1 ~ "Winter",
  season == 2 ~ "Spring",
  season == 3 ~ "Summer",
  season== 4 ~ "Autumn",
  TRUE ~ as.factor(season)))

# Выбросы
# Посмотрим season, mnth
ggplot(bicycle_rent_clean_fix, aes(x = season, y =mnth))+
  geom_point()
filter(bicycle_rent_clean_fix, season==1 & instant > 600)

# 12 месяц относится также к зиме
# Посмотрим season, atemp
ggplot(bicycle_rent_clean_fix, aes(x = as.factor(season), y = atemp,  fill = as.factor(season))) +
  geom_boxplot() +
  labs(x = "Season", y = "Sensation Temperature", fill = "Season") # Можно заметить выброс летом в температуре ощущений

filter(bicycle_rent_clean_fix, season==3 & atemp < 0.4) # Действительно,это летний день, погода ясная, температура 29.6. Однако температура ощущений 12.1.
bicycle_rent_clean_fix_f1 <- filter(bicycle_rent_clean_fix, !(season == 'Summer' & atemp < 0.4)) # Уберем его на всякий случай

# Посмотрим humidity, month

ggplot(bicycle_rent_clean_fix_f1, aes(x = factor(mnth), y = hum)) +
  geom_boxplot(aes(fill = factor(season))) +
  labs(x = "Month", y = "Humidity", fill = "Season")
# Есть нулевая влажность в третьем месяце, удалим ее
bicycle_rent_clean_fix_f2 <-  filter(bicycle_rent_clean_fix_f1, !( mnth == 3 & hum < 0.1))

table(bicycle_rent_clean_fix_f2$holiday)


# temp, humidity
ggplot(bicycle_rent_clean_fix_f2, aes(x = temp, y = hum)) +
  geom_point() +  
  labs(x = "Temperature", y = "Humidity", title = "Temperature vs Humidity")


# Корреляция между temp и hum зимой
bicycle_rent_clean_fix_f2 %>%
  filter(season == 'Winter') %>%
  summarise(correlation = cor(temp, hum)) %>%
  pull(correlation)

# Получим отдельные датасеты в зависимости от сезона
winter_bicycle_rent_clean <-  bicycle_rent_clean_fix_f2 %>% filter(season == 'Winter')
summer_bicycle_rent_clean <- bicycle_rent_clean_fix_f2 %>% filter(season == 'Summer')
spring_bicycle_rent_clean <- bicycle_rent_clean_fix_f2 %>% filter(season == 'Spring')
autumn_bicycle_rent_clean <- bicycle_rent_clean_fix_f2 %>% filter(season == 'Autumn')



ggplot(winter_bicycle_rent_clean, aes(x = factor(1), y = temp)) +
  geom_boxplot(aes(fill = "Temperature")) +  # Боксплот для температуры
  geom_boxplot(aes(y = hum, fill = "Humidity")) +  # Боксплот для влажности
  labs(x = "", y = "Values", title = "Temperature and Humidity in Winter") +
  scale_fill_manual(values = c("Temperature" = "blue", "Humidity" = "green")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "top")

bicycle_rent_clean_fix <-  bicycle_rent_clean %>% mutate(yr = case_when(
  yr == 0 ~ 2011,
  yr == 1 ~ 2012,
  TRUE ~ as.numeric(yr)))

### Гипотезы
### Проверим гипотезу о росте популярности проката с годами
year_2011_bicycle_rent_clean <- bicycle_rent_clean_fix %>% filter(yr == 2011)
year_2012_bicycle_rent_clean <- bicycle_rent_clean_fix %>% filter(yr == 2012)

bicycle_rent_clean_fix <-  bicycle_rent_clean_fix %>%
  mutate(yr = as.numeric(yr))


t_test_result <- t.test(year_2011_bicycle_rent_clean$registered,   year_2012_bicycle_rent_clean$registered)
print(t_test_result)

sum(year_2011_bicycle_rent_clean$registered)
sum(year_2012_bicycle_rent_clean$registered)



### Посмотрим какие есть корреляции в данных по годам

year_2011_bicycle_rent_num <- year_2011_bicycle_rent_clean %>% select((where(is.numeric)))
cor(year_2011_bicycle_rent_num)

# Видим положительную корреляцию между количеством пользователей и температурой в этот день

correlation_1 <-  cor.test(year_2011_bicycle_rent_num $temp, year_2011_bicycle_rent_num$cnt) # подтвежаедтся также t-test


# Проверим гипотезу, что летом количество пользователей больше, чем зимой в 2011 году

t_test_2 <- t.test (year_2011_bicycle_rent_num$cnt[year_2011_bicycle_rent_num$season =='3'],
                           year_2011_bicycle_rent_num$cnt[year_2011_bicycle_rent_num$season =='1'])
print(t_test_2$p.value)
