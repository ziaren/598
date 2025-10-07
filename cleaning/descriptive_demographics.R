library(nlme)

APHAB <- read.csv("/cloud/project/TheInfluenceOfPatien-MainOutcomes_DATA_2025-09-09_1312.csv", header = TRUE)

library(dplyr)

hist(x$age)
hist(x$gender)
plot(colMeans((x[,9:16]), na.rm = T))
hist(x[,20])
plot(colMeans(x[,20:29], na.rm = T))
plot(colMeans(x[,31:40], na.rm = T))
plot(colMeans(x[,48:72], na.rm = T))