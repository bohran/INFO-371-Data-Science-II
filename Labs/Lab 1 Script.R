n <- 1000
norm <- rnorm(n)
average <- mean(norm)
variance_1 <- mean((norm - mean(norm))^2)
variance_1
variance_2 <- mean(norm^2) - (average * norm)^2
variance_2
variance_3 <- var(norm)
standardDev <- sd(norm)

lowerBound <- average - (1.96 * standardDev)
upperBound <- average + (1.96 * standardDev)

hist(norm)
abline(v=average, col = "blue", lwd = 2)
abline(v = lowerBound, col = "red", lwd = 2)
abline(v = upperBound, col = "green", lwd = 2)
2. 
n2<- 3
m <- 1000
x <- sapply(1:m, function(n2) mean(rnorm(n2)))
var(x)
sd(x)

n3 <- 300
x1 <- sapply(1:m, function(n3) mean(rnorm(n3)))
var(x1)
sd(x1)           
              