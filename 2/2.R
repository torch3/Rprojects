#==========Часть 1==========
#=========Задание 1=========
arr <- array(3, c(3, 4))
arr
arr[1, 3] <- 4
arr[2, 1] <- 1
arr[3, ] <- c(3, NA, 3, 1)
arr

#=========Задание 2=========
a <- c(1, 3, 4, 9, NA)
b <- c(5, 6, 7, 0, 2)
c <- c(9, 10, 13, 1, 20)
arr <- cbind(a, b, c)
arr
t(arr)
rownames(arr) <- paste0("row", 1:5)
colnames(arr) <- paste0("col", 1:3)
arr

#=========Задание 3=========
names <- c("Jane", "Michael", "Mary", "George")
ages <- c(8, 6, 28, 45)
gender <- c(0, 1, 0, 1)
arr <- cbind(names, ages, gender)
arr
arr[, "ages"] <- as.integer(arr[, "ages"])
arr
arr <- data.frame(names, ages, gender)
arr
arr$age_sq <- ages^2
arr

#=========Задание 4=========
info <- list(names, ages, gender)
info
info[[1]][2]
info[[3]]
names(info) <- c("names", "ages", "gender")
info
info$names
info$drinks <- c("juice", "tea", "rum", "coffee")
info
info$name <- c(info$name, "John")
info$age <- c(info$age, 2)
info$gender <- c(info$gender, 1)
info$drinks <- c(info$drinks, "milk")
info

#=========Задание 5=========
index <- "0,72;0,38;0,99;0,81;0,15;0,22;0,16;0,4;0,24"
I <- as.numeric(unlist(strsplit(gsub(",", ".", index), ";")))
I

#_________________________________________________________________________________________
#==========Часть 2==========
#=========Задание 1=========
A <- diag(c(4, 9), nrow = 2, ncol = 2)
rownames(A) <- c("eq1", "eq2")
colnames(A) <- c("x1", "x2")
A

#=========Задание 2=========
eigen_values <- eigen(A)$values
eigen_values

#=========Задание 3=========
B <- diag(1, 2, 2) - A
B

#=========Задание 4=========
f <- c(4, 2)
u <- c(0.2, -0.3)

#=========Задание 5=========
u_result <- solve(A, f)
u_result

#=========Задание 6=========
u1 = B %*% u + f
u2 = B %*% u1 + f
u3 = B %*% u2 + f
u4 = B %*% u3 + f
u5 = B %*% u4 + f
u6 = B %*% u5 + f
u7 = B %*% u6 + f
u7

#=========Задание 7=========
diff1 <- u7 - u_result

#=========Задание 8=========
A <- A / max(A)
f <- f / max(A)

#=========Задание 9=========
eigen_values <- eigen(A)$values
eigen_values
B <- diag(1, 2, 2) - A
B
u_result <- solve(A, f)
u_result
u1 = B %*% u + f
u2 = B %*% u1 + f
u3 = B %*% u2 + f
u4 = B %*% u3 + f
u5 = B %*% u4 + f
u6 = B %*% u5 + f
u7 = B %*% u6 + f
u7
diff2 <- u7 - u_result
diff2 < diff1

#_________________________________________________________________________________________
#==========Часть 3==========
step <- 1
dekart_begin <- -5
dekart_end <- 5
x <- seq(from = dekart_begin, to = dekart_end, by = step)
y <- x
surface_matrix <- outer(X = x,
                        Y = y,
                        FUN = function(x,y) Re(exp(-1i * 0.5 * x * y)))
dimnames(surface_matrix) <- list(x, y)
surface_matrix

#=========Задание 1=========
file <- file("3_1/summary.txt", open = "w")
cat("number of matrix elements:",
    length(surface_matrix), "\n", file = file)
cat("number of rows:",
    nrow(surface_matrix), "\n", file = file)
cat("number of cols:",
    ncol(surface_matrix), "\n", file = file)
cat("sum of main diag elements:",
    sum(diag(surface_matrix)), "\n", file = file)
cat("sum of middle row elements:",
    sum(surface_matrix[nrow(surface_matrix) %/% 2, ]), "\n", file = file)
cat("sum of middle column elements:",
    sum(surface_matrix[, ncol(surface_matrix) %/% 2]), "\n", file = file)
cat("row sums:",
    apply(surface_matrix, 1, sum), "\n", file = file)
cat("col sums:",
    apply(surface_matrix, 2, sum), "\n", file = file)
close(file)

#=========Задание 2=========
dekart_begin <- as.numeric(readline())
dekart_end <- as.numeric(readline())
step <- as.numeric(readline())
x <- seq(from = dekart_begin, to = dekart_end, by = step)
y <- x
surface_matrix <- outer(X = x,
                        Y = y,
                        FUN = function(x,y) Re(exp(-1i * 0.5 * x * y)))
dimnames(surface_matrix) <- list(x, y)
surface_matrix
file <- file("3_2/summary2.txt", open = "w")
cat("number of matrix elements:",
    length(surface_matrix), "\n", file = file)
cat("number of rows:",
    nrow(surface_matrix), "\n", file = file)
cat("number of cols:",
    ncol(surface_matrix), "\n", file = file)
cat("sum of main diag elements:",
    sum(diag(surface_matrix)), "\n", file = file)
cat("row sums:",
    apply(surface_matrix, 1, sum), "\n", file = file)
cat("col sums:",
    apply(surface_matrix, 2, sum), "\n", file = file)
close(file)

#=========Задание 3=========
input <- scan("3_3/input.txt")
start_row <- input[1]
end_row <- input[2]
step_row <- input[3]
start_col <- input[4]
end_col <- input[5]
step_col <- input[6]
x <- seq(start_row, end_row, step_row)
y <- seq(start_col, end_col, step_col)
x
y
surface_matrix <- outer(X = x,
                        Y = y,
                        FUN = function(x,y) Re(exp(-1i * 0.5 * x * y)))
dimnames(surface_matrix) <- list(x, y)
surface_matrix
file <- file("3_3/summary3.txt", open = "w")
cat("number of matrix elements:",
    length(surface_matrix), "\n", file = file)
cat("number of rows:",
    nrow(surface_matrix), "\n", file = file)
cat("number of cols:",
    ncol(surface_matrix), "\n", file = file)
cat("sum of main diag elements:",
    sum(diag(surface_matrix)), "\n", file = file)
cat("row sums:",
    apply(surface_matrix, 1, sum), "\n", file = file)
cat("col sums:",
    apply(surface_matrix, 2, sum), "\n", file = file)
close(file)

#_________________________________________________________________________________________
#==========Часть 4==========
cars_matrix <- as.matrix(cars)

#=========Задание 1=========
cars_speed <- cbind(1, cars_matrix[, 1])
cars_speed

#=========Задание 2=========
cars_dist <- cars_matrix[, 2]
cars_dist

#=========Задание 3=========
alpha <- solve(t(cars_speed) %*% cars_speed) %*% t(cars_speed) %*% cars_dist
alpha
if (!is.vector(alpha)) {
    alpha <- as.vector(alpha)
}
class(alpha)

#=========Задание 5=========
alpha_c <- alpha[1]
alpha_x <- alpha[2]
cat("alpha_c = ", alpha_c)
cat("alpha_x = ", alpha_x)

#=========Задание 6=========
cars_speed_lm <- cars_matrix[, 1]
cars_speed_lm

#=========Задание 7=========
cars_dist_lm <- alpha_c + cars_speed_lm * alpha_x

#=========Задание 8=========
dist_residuals <- cars_dist_lm - cars_speed_lm

#=========Задание 9=========
mean <- mean(dist_residuals)
sd <- sd(dist_residuals)

#=========Задание 10=========
cars_dist_lm

#=========Задание 11=========
mean
sd
