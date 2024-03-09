#====================================Часть 1====================================
#___________________________________Задание 1___________________________________
names <- c("Jane", "Michael", "Mary", "George")
ages <- c(8, 6, 28, 45)
gender <- c(0, 1, 0, 1)
info <- list(names, ages, gender)
info

info[[1]][2]

info[[3]]

names(info) <- c("name", "age", "gender")
info$name

info$drinks <- c("juice", "tea", "rum", "coffee")
info

info$name <- c(info$name, "John")
info$age <- c(info$age, 2)
info$gender <- c(info$gender, 1)
info$drinks <- c(info$drinks, "milk")
info

#___________________________________Задание 2___________________________________
index <- "0,72;0,38;0,99;0,81;0,15;0,22;0,16;0,4;0,24"
I <- as.numeric(unlist(strsplit(gsub(",", ".", index), ";")))
I


#====================================Часть 2====================================
#___________________________________Задание 1___________________________________
# install.packages("randomNames")
library(randomNames)

#___________________________________Задание 2___________________________________
set.seed(1234)
names <- randomNames(100, which.names = "first", ethnicity = 4)
names

#___________________________________Задание 3___________________________________
ages <- sample(16:75, 100, replace = TRUE)
views <- c("right", "left", "moderate", "indifferent")
polit <- sample(views, 100, replace = TRUE)
df <- data.frame(name = names, age = ages, polit = polit)
df

#___________________________________Задание 4___________________________________
# install.packages("dplyr")
library(dplyr)

df <- df %>% 
    mutate(new_column = sample(1:100, n(), replace = TRUE)) %>% 
    select(new_column, everything())
df

#___________________________________Задание 5___________________________________
age_25_30 <- nrow(filter(df, age >= 25 & age < 30))
age_25_30

percentage_25_30 <- round((age_25_30 / nrow(df)) * 100, 1)
percentage_25_30

#___________________________________Задание 6___________________________________
polit_views <- factor(df$polit, levels = views)
polit_views

num_levels <- nlevels(polit_views)
num_levels

df <- mutate(df, polit_views)
df

#====================================Часть 3====================================
#___________________________________Задание 1___________________________________
install.packages("car")
library("car")
df <- Ornstein
df

#___________________________________Задание 2___________________________________
nrow(df)
ncol(df)
names(df)

#___________________________________Задание 3___________________________________
nrow(df[!apply(is.na(df), 1, any), ])
df[apply(is.na(df), 1, any), ]

#___________________________________Задание 4___________________________________
filter(df, assets >= 10000 & assets <= 20000)

filter(df, interlocks <= 30)

filter(df, sector == "TRN" & nation == "CAN")

#___________________________________Задание 5___________________________________
df$log_assets <- log(df$assets)
df

#====================================Часть 4====================================
# install.packages(c("readr", "stringr"))
library("readr")
library("stringr")

#___________________________________Задание 1___________________________________
covid_data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covid_data

#___________________________________Задание 2___________________________________
dim(covid_data)
colnames(covid_data)
str(covid_data)

#___________________________________Задание 3___________________________________
library(tidyr)

covid_data <- unite(covid_data, Region, Country.Region, Province.State,
                    sep = "/")
covid_data

#___________________________________Задание 4___________________________________
row_sd <- function(row) {
    row <- as.numeric(row)
    sd(row, na.rm = TRUE)
}

frame1 <- covid_data %>% 
    select(Region, Lat, Long) %>% 
    mutate(TotalCases = rowSums(select(covid_data, -(Region:Long)),
                                na.rm = TRUE)) %>% 
    mutate(Average = rowMeans(select(covid_data, -(Region:Long)),
                                na.rm = TRUE)) %>% 
    mutate(SD = apply(select(covid_data, -(Region:Long)), 1, row_sd))

#___________________________________Задание 5___________________________________
frame2 <- covid_data %>% select(-(Lat: Long)) %>% 
    gather(key = "Date", value = "X", -Region) %>%
    pivot_wider(names_from = "Region", values_from = "X", names_sep = "_")

frame2$Date <- str_replace_all(frame2$Date, "X", "0")
# frame2$Date <- as.Date(frame2$Date, format = "%m.%d.%y")
frame2$Date <- format(as.Date(frame2$Date, format = "%m.%d.%y"), "%Y-%m-%d")

#___________________________________Задание 6___________________________________
# install.packages("openxlsx")
library("openxlsx")

if (!file.exists("data_output")) {
    dir.create("data_output")
}

write.table(frame1, file = "data_output/frame1.txt", sep = "\t", row.names = FALSE)
write.csv(frame1, file = "data_output/frame1.csv", row.names = FALSE)
write.xlsx(frame1, file = "data_output/frame1.xlsx", rowNames = FALSE)

write.table(frame2, file = "data_output/frame2.txt", sep = "\t", row.names = FALSE)
write.csv(frame2, file = "data_output/frame2.csv", row.names = FALSE)
write.xlsx(frame2, file = "data_output/frame2.xlsx", rowNames = FALSE)

