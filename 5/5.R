#====================================Часть 1====================================
#___________________________________Задание 1___________________________________
count_variable_types <- function(data) {
    numeric_count <- sum(sapply(data, is.numeric))
    factor_count <- sum(sapply(data, is.factor))
    character_count <- sum(sapply(data, is.character))
    
    result <- c("numeric" = numeric_count, 
                "factor" = factor_count, 
                "character" = character_count)
    return(result)
}

id <- 1:3
country <- c("Flatland", "Wonderland", "Sphereland")
craziness <- c(20, 15, 18)
region_type <- c("A", "B", "A")
author <- c("Abbot", "Carroll", "Burger")
size <- c(10, 100, 30)
df <- data.frame(id, country, craziness, region_type, author, size)

result <- count_variable_types(df)
result

#___________________________________Задание 2___________________________________
select_numeric_variables <- function(data) {
    numeric_columns <- data[, sapply(data, is.numeric)]
    return(numeric_columns)
}

numeric_df <- select_numeric_variables(df)
numeric_df

#___________________________________Задание 3___________________________________
count_variable_types <- function(data) {
    numeric_count <- 0
    factor_count <- 0
    character_count <- 0
    
    for (col in names(data)) {
        if (is.numeric(data[[col]])) {
            numeric_count <- numeric_count + 1
        } else if (is.factor(data[[col]])) {
            factor_count <- factor_count + 1
        } else if (is.character(data[[col]])) {
            character_count <- character_count + 1
        }
    }
    
    result <- c("numeric" = numeric_count, 
                "factor" = factor_count, 
                "character" = character_count)
    return(result)
}

select_numeric_variables <- function(data) {
    numeric_columns <- data.frame(matrix(nrow = nrow(data), ncol = 0))
    
    for (col in names(data)) {
        if (is.numeric(data[[col]])) {
            numeric_columns[[col]] <- data[[col]]
        }
    }
    
    return(numeric_columns)
}

result <- count_variable_types(df)
result
numeric_df <- select_numeric_variables(df)
numeric_df

#___________________________________Задание 4___________________________________
compute_median <- function(vector) {
    if (is.numeric(vector)) {
        return(median(vector))
    } else {
        print("Vector is not numeric, cannot compute the median")
    }
}

numeric_vector <- c(1, 2, 3, 4, 5)
non_numeric_vector <- c("a", "b", "c")

compute_median(numeric_vector)
compute_median(non_numeric_vector)

#====================================Часть 2====================================
#___________________________________Задание 1___________________________________
if ("quantmod" %in% rownames(installed.packages()) == FALSE) {
    install.packages("quantmod") }
library(quantmod)
if ("stringr" %in% rownames(installed.packages()) == FALSE) {
    install.packages("stringr") }
library(stringr)
downloadable_stocks <- c("AAPL", "^IXIC")
quantmod::getSymbols(Symbols = downloadable_stocks,
                     src = "yahoo",
                     from = as.Date.character("1900-01-01"))
df <- data.frame(get(downloadable_stocks[1]))
downloadable_stocks <- str_remove(downloadable_stocks, 
                                           "[:punct:\\^]")
rm(list = downloadable_stocks)
head(df)

#___________________________________Задание 2___________________________________
out_of_trend <- function(x, dt, method = "Arifm") {
    if (!is.numeric(x)) {
        stop("Input vector x must be numeric.")
    }
    if (!is.numeric(dt)) {
        stop("Trial displacement dt must be numeric.")
    }
    if (length(x) < 3) {
        stop("Input vector x must have length at least 3.")
    }
    if (length(unique(x)) == 1) {
        stop("Input vector x has no trend.")
    }
    if (dt > ceiling(length(x) / 2) - 1) {
        stop("Trial displacement dt exceeds the maximum allowed value.")
    }
    x <- x + min(x) + 1
    n <- length(x)
    if (method == "Arifm") {
        out <- 
            log((x[1:(n - 2 * dt)] + x[(1 + 2 * dt):n]) / 
                    (2 * x[(1 + dt):(n - dt)]))
    } else if (method == "Geom") {
        out <- 
            log((x[1:(n - 2 * dt)] * x[(1 + 2 * dt):n]) / 
                    x[(1 + dt):(n - dt)]^2)
    } else if (method == "Garm") {
        out <- 
            log((2 * x[1:(n - 2 * dt)] * x[(1 + 2 * dt):n]) / 
                    (x[(1 + dt):(n - dt)] * 
                    (x[1:(n - 2 * dt)] + x[(1 + 2 * dt):n])))
    } else {
        stop("Invalid method specified. Choose from 'Arifm', 'Geom', or 'Garm'.")
    }
    return(out)
}

#___________________________________Задание 3___________________________________
t <- seq(0, 10, 0.1)
x <- 2 * t + 3 + sin(2 * t)
mean(x)
xn <- out_of_trend(x, 10)
mean(xn)

#___________________________________Задание 4___________________________________
alter_jones <- function(y, tau) {
    n <- length(y)
    out <- 1 / (y - tau) * sum(abs(y[(1 + tau):n] - y[1:(n - tau)]))
    return(out)
}

#___________________________________Задание 5___________________________________
taus <- 1:(length(xn) - 1)
values <- sapply(taus, function(t) alter_jones(xn, t))
taus[which.min(values)]

#___________________________________Задание 6___________________________________
# install.packages("ggplot2")
library(ggplot2)
res_Arifm <- alter_jones(out_of_trend(df$AAPL.High, 1000, "Arifm"), 101)
res_Geom <- alter_jones(out_of_trend(df$AAPL.High, 1000, "Geom"), 343)
res_Garm <- alter_jones(out_of_trend(df$AAPL.High, 1000, "Garm"), 100)
xmax <-  max(length(res_Arifm), length(res_Geom), length(res_Garm))
ymax <- max(res_Arifm, res_Geom, res_Garm)

plot <- ggplot() + 
    geom_line(data = as.data.frame(res_Arifm), 
              aes(x = 1:length(res_Arifm), y = res_Arifm), 
              color = "green") + 
    geom_line(data = as.data.frame(res_Geom), 
              aes(x = 1:length(res_Geom), y = res_Geom), 
              color = "blue") + 
    geom_line(data = as.data.frame(res_Garm), 
              aes(x = 1:length(res_Garm), y = res_Garm), 
              color = "red") + 
    labs(x = "Index", y = "Value")
plot

#====================================Часть 3====================================
#___________________________________Задание 1___________________________________
SIM <- function(A, u0, f, n_iter = 10^5, eps = 10^(-7)) {
    if (!is.matrix(A) || nrow(A) != ncol(A)) {
        stop("Matrix A must be a square matrix.")
    }
    if (length(f) != nrow(A)) {
        stop("Dimensions of matrix A and vector f do not match.")
    }
    if (length(u0) != length(f)) {
        stop("Dimensions of initial approximation u0 and vector f do not match.")
    }
    if (n_iter <= 0) {
        stop("Number of iterations must be a positive integer.")
    }
    if (eps <= 0) {
        stop("Epsilon must be a positive number.")
    }
    u <- u0
    for (i in 1:n_iter) {
        u_new <- A %*% u + f
        if (max(abs(u_new - u)) < eps) {
            return(u_new)
        }
        u <- u_new
    }
    warning("Maximum number of iterations reached without convergence.")
    return(u)
}

A <- diag(c(4, 9), nrow = 2, ncol = 2)
I = diag(2)
B <- I - A

f <- c(4, 2)
u <- c(0.2, -0.3)

u_result <- SIM(A, u, f)
u_result

#_______________________________________________________________________________