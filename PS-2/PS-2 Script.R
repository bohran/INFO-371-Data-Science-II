setwd("/Users/nicolebohra/INFO371/PS-2")
data <- read.csv("progresa_sample.csv", header = TRUE)

#data[data == ""] = NA

stat <- subset(data, select = -c(poor, year,folnum, village, progresa))
mean <- sapply(stat, mean, na.rm=TRUE)
mean <- round(mean)
standard_dev <- sapply(stat, sd, na.rm=TRUE)
missing_values <- colSums(is.na(stat))
stat_table <- cbind(mean, standard_dev, missing_values)
stat_table <- data.frame(stat_table)
stat_table <- stat_table[order(row.names(stat_table)),]
