#====================================Часть 1====================================
#___________________________________Задание 1___________________________________
# install.packages("xslx")
# install.packages("lubridate")
library(xlsx)
library(lubridate)
column_classes <- c("Date", "numeric", "numeric", "numeric", "numeric",
                    "numeric", "numeric", "character", "character")
column_names <- c("Date", "Preasure_MPa", "Temp_°C", "Gas_Prod_m3_per_d", 
                  "Cond_m3_per_d", "Water_m3_per_d", "ID", "Bush", "Group")
gas <- read.xlsx("GAZ.xlsx", 
                 sheetIndex = 1, 
                 colNames = FALSE, 
                 colClasses = column_classes, 
                 skipEmptyCols = TRUE)
colnames(gas) <- column_names
gas <- gas[, 1:9]
filtered_rows <- day(gas$Date) < 13
gas$Date <- format(as.Date(gas$Date, format = "%Y-%m-%d"), format = "%d-%m-%Y")
gas$Date[filtered_rows] <- format(as.Date(gas$Date[filtered_rows],
                                          format = "%d-%m-%Y"),
                                  format = "%m-%d-%Y")
gas$Date <- as.Date(gas$Date, format = "%d-%m-%Y")
head(gas)

#___________________________________Задание 2___________________________________
nrow(gas)
gas <- gas[complete.cases(gas), ]
nrow(gas)

#___________________________________Задание 3___________________________________
colnames(gas)[colnames(gas) == "Temp_°C"] <- "Temp_K"
gas$Temp_K <- gas$Temp_K + 273.15
head(gas)

#___________________________________Задание 4___________________________________
gas$ID <- as.factor(gas$ID)
gas$Bush <- as.factor(gas$Bush)
gas$Group <- as.factor(gas$Group)
str(gas)

#___________________________________Задание 5___________________________________
gas$Gas_Cond_Ratio <- gas$Gas_Prod_m3_per_d / gas$Cond_m3_per_d
gas$Gas_Water_Ratio <- gas$Gas_Prod_m3_per_d / gas$Water_m3_per_d
gas$Water_Cond_Ratio <- gas$Water_m3_per_d / gas$Cond_m3_per_d
head(gas)

#___________________________________Задание 6___________________________________
gas_2018 <- subset(gas, year(Date) == 2018)
agg_data <- aggregate(Gas_Prod_m3_per_d ~ Bush, data = gas_2018, sum)
colnames(agg_data)[2] <- "Total_Prod_2018"
agg_data

#___________________________________Задание 7___________________________________
gas_111 = gas[gas$ID == 111, ]
head(gas_111)

#___________________________________Задание 8___________________________________
k_more_than_2 <- table(subset(gas, Water_m3_per_d > 2)$ID)
ID_less_than_2 <- names(k_more_than_2[k_more_than_2 == 0])
ID_less_than_2

#___________________________________Задание 9___________________________________
k_less_than_1000 <- table(subset(gas, Gas_Prod_m3_per_d < 1000)$ID)
ID_more_than_1000 <- names(k_less_than_1000[k_less_than_1000 == 0])
ID_more_than_1000

k_less_than_1000 <- table(subset(gas, Gas_Prod_m3_per_d < 13750)$Bush)
Bush_more_than_1000 <- names(k_less_than_1000[k_less_than_1000 == 0])
Bush_more_than_1000

#___________________________________Задание 10__________________________________
gas_2018 <- subset(gas, year(Date) == 2018)
agg_data <- aggregate(Gas_Prod_m3_per_d ~ Group, data = gas_2018, sum)
colnames(agg_data)[2] <- "Total_Prod_2018"
agg_data
max_group <- agg_data[which.max(agg_data$Total_Prod_2018), 1]
max_group

#___________________________________Задание 11__________________________________
gas_2018 <- subset(gas, year(Date) == 2018)
agg_data <- aggregate(Water_m3_per_d ~ Bush, data = gas_2018, sum)
colnames(agg_data)[2] <- "Total_Prod_2018"
agg_data
max_bush <- agg_data[which.max(agg_data$Total_Prod_2018), 1]
max_bush

#___________________________________Задание 12__________________________________
gas_data <- gas[is.finite(gas$Gas_Water_Ratio), ]
agg_data <- aggregate(Gas_Water_Ratio ~ Bush, data = gas_data, mean)
colnames(agg_data)[2] <- "Mean_Gas_Water_Ratio"
agg_data
max_mean_bush <- agg_data[which.max(agg_data$Mean_Gas_Water_Ratio), 1]
max_mean_bush

#_______________________________________________________________________________