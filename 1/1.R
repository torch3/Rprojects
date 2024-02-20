#==========Часть 1==========
#=========Задание 1=========
x <- 2
y <- 4
c <- x
x <- y
y <- c
x
y

#=========Задание 2=========
x <- 3.5
y <- "2,6"
z <- 1.78
h <- TRUE

class(x)
class(y)
class(z)
class(h)

h <- as.integer(h)
class(h)

y <- as.numeric(sub(",", ".", y))
class(y)

x <- as.character(x)
class(x)

#=========Задание 3=========
dohod <- 1573
dohod <- log(dohod)
dohod

#=========Задание 4=========
write(5, "1_4/Вариант.txt")
N <- as.numeric(readLines(con="1_4/Вариант.txt"))
N
2 * N - 1

#______________________________________________________________________________________
#==========Часть 2==========
#=========Задание 1=========
vec <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)

cat("Первый элемент вектора:\n", vec[1])

cat("Последний элемент вектора:\n", tail(vec, 1))

cat("Элементы вектора с третьего по пятый включительно:\n",
    vec[3:5])

cat("Элементы вектора, которые равны 2:\n",
    vec[vec == 2 & !is.na(vec)])

cat("Элементы вектора, которые больше 4:\n",
    vec[vec > 4 & !is.na(vec)])

cat("Элементы вектора, которые кратны 3:\n",
    vec[vec %% 3 == 0 & !is.na(vec)])

cat("Элементы вектора, которые больше 4 и кратны 3:\n",
    vec[vec > 4 & vec %% 3 == 0 & !is.na(vec)])

cat("Элементы вектора, которые или меньше 1, или больше 5:\n",
    vec[vec < 1 | vec > 5 & !is.na(vec)])

cat("Индексы элементов, которые равны 0:\n",
    which(vec == 0))

cat("Индексы элементов, которые не меньше 2 и не больше 8:\n",
    which(vec >= 2 & vec <= 8))

cat("Элементы вектора по возрастанию, с пропущенными значениями в конце без цифр 2:\n",
    c(sort(vec[vec != 2 & !is.na(vec)]), vec[is.na(vec)]))

#=========Задание 2=========
vec <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)
vec[length(vec)] <- NA
vec

#=========Задание 3=========
vec <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)
cat("Индексы пропущенных значений в векторе:\n", which(is.na(vec)))


#=========Задание 4=========
vec <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)
cat("Количество пропущенных значений в векторе:\n", sum(is.na(vec)))

#=========Задание 5=========
respondent_ids <- 1:100
respondent_ids

#=========Задание 6=========
countries <- c("France", "France", "France", "France", "France", "Italy", "Italy", "Italy", "Italy", "Italy", "Spain", "Spain", "Spain", "Spain", "Spain")
years <- c(2019, 2020, 2020, 2018, 2017, 2019, 2020, 2020, 2018, 2017, 2019, 2020, 2020, 2018, 2017)
data_table <- data.frame(country = countries, year = years)
data_table

#=========Задание 7=========
income <- c(10000, 32000, 28000, 150000, 65000, 1573)
average_income <- sum(income) / length(income)
income_class <- ifelse(income < average_income, 0, 1)
income
average_income
income_class

#=========Задание 8=========
x <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, 9)
write(x, "2_8/coords.txt", ncolumns=1)
x <- scan("2_8/coords.txt")
x
P <- 2.32
Lp <- sum(x**P)
write(Lp, "2_8/result.txt")

#=========Задание 9=========
x <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, 9)
write(x, "2_9/coords.txt", ncolumns=1)
x <- scan("2_9/coords.txt")
x
diff_x <- diff(x)
diff_x
diff2_x <- diff(diff_x)
diff2_x
cat(diff_x, "\n", diff2_x, file = "2_9/diff_vectors.txt")
