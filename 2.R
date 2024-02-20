#==========Часть 1==========
arr <- array(3, c(3, 4))
arr
arr[1, 3] <- 4
arr[2, 1] <- 1
arr[3, 2] <- NA
arr[3, 4] <- 1
arr

a <- c(1, 3, 4, 9, NA)
b <- c(5, 6, 7, 0, 2)
c <- c(9, 10, 13, 1, 20)

arr <- cbind(a, b, c)
arr
t(arr)
rownames(arr) <- paste0("row", 1:5)
colnames(arr) <- paste0("col", 1:3)
arr

names <- c("Jane", "Michael", "Mary", "George")
ages <- c(8, 6, 28, 45)
gender <- c(0, 1, 0, 1)

m_cols <- cbind(names, ages, gender)
m_cols




z <- 1:30
dim(z) <- c(3, 10)
z









