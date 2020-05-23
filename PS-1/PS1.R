install.packages("VGAM")
install.packages("ggplot2")
install.packages("dplyr")
library("ggplot2")
library("VGAM")
library("dplyr")

#1 Outliers in Different Distributions
#1.1 Normal Distribution 
r <- 1000
n <- 1000

#2.
x <- sapply(1:n, function(s) mean(rnorm(n)))
x

#3.
mean <- mean(mean(x))
sd <- sd(x)
left <- quantile(x, 0.025)
left
right <- quantile(x, 0.975)
right

r2 <- 1000
n2 <- 10
x2 <- sapply(1:n2, function(s) mean(rnorm(n2)))
mean(mean(x2))
sd(x2)
left <- quantile(x2, 0.025)
left
right <- quantile(x2, 0.975)
right

#1.2 Pareto Distribution
n <- 10000
k1 <- 100
k2 <- 10
k3 <- 1
s1 <- VGAM::rpareto(n, 1, k1)
m1 <- mean(s1)
hist(s1)
abline(v = m1, col = "blue", lwd = 2)

s2 <- VGAM::rpareto(n, 1, k2)
m2 <- mean(s2)
hist(s2)
abline(v = m2, col = "red", lwd = 2)

s3 <- rpareto(n, 1, k3)
m3 <- mean(s3)
hist(s3)
abline(v = m3, col = "green", lwd = 2)

# Second Pareto Dist
k_less <- 0.5
k_greater <- 2
r <- 1000
n1 <- 10
n2 <- 1000
n3 <- 10000

x_pareto_less <- sapply(1:n1, function(s) mean(rpareto(n1, 1, k_less)))
mean_less <- mean(mean(x_pareto_less))
sd(x_pareto_less)
left_less <- quantile(x_pareto_less, 0.025)
right_less <- quantile(x_pareto_less, 0.975)
left_less
right_less


x_pareto_greater <- sapply(1:n1, function(s) mean(rpareto(n1, 1, k_greater)))
mean_greater <- mean(mean(x_pareto_greater))
sd(x_pareto_greater)
left_greater <- quantile(x_pareto_greater, 0.025)
right_greater <- quantile(x_pareto_greater, 0.975)
left_greater
right_greater


x_pareto_less_2 <- sapply(1:n2, function(s) mean(rpareto(n2, 1, k_less)))
mean_less_2 <- mean(mean(x_pareto_less_2))
sd(x_pareto_less_2)
left_less_2 <- quantile(x_pareto_less_2, 0.025)
right_less_2 <- quantile(x_pareto_less_2, 0.975)
left_less_2
right_less_2

x_pareto_greater_2 <- sapply(1:n2, function(s) mean(rpareto(n2, 1, k_greater)))
mean_greater_2 <- mean(mean(x_pareto_greater_2))
sd(x_pareto_greater_2)
left_greater_2 <- quantile(x_pareto_greater_2, 0.025)
right_greater_2 <- quantile(x_pareto_greater_2, 0.975)
left_greater_2
right_greater_2

#Section 2: Linear Transformation of Images
#2.1 Rotate Matrices
#1. 
A <- matrix(c(0,0, 0,2, 1,1), 3, 2, byrow = TRUE)
plot(A[,1], A[,2], type ="l", asp=1)
Rot <- function(alpha) {
  a <- alpha*pi/180
  matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
}

A_1 <- A%*%Rot(45)
plot(A_1[,1], A_1[,2], type ="l", asp=1)

A_2 <- A%*%Rot(-225)
plot(A_2[,1], A_2[,2], type ="l", asp=1)

A_3 <- A%*%Rot(60)
plot(A_3[,1], A_3[,2], type ="l", asp=1)


#2. 
B <- matrix(c(2,3, 5,6, 10,-12), 3, 2, byrow = TRUE)
B
plot(B[,1], B[,2], type="b", asp=1 )

B_1 <- B%*%Rot(60)
plot(B_1[,1], B_1[,2], type="b", asp=1)

B_2 <- B%*%Rot(120)
plot(B_2[,1], B_2[,2], type="b", asp=1)

B_3 <- B%*%Rot(-180)
plot(B_3[,1], B_3[,2], type="b", asp=1)


#2.2 Rotate Data
setwd("/Users/nicolebohra/INFO371")
data <- read.table('crazy_hat.tsv', header = TRUE)
ggplot(data, aes(x = x, y = y, group = group)) + geom_path()

mat <- data.matrix(data)
Rot <- function(alpha) {
  a <- alpha*pi/180
  matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
}
transMatrix1 <- mat[,1:2]%*%Rot(45)
transMatrix1.df <- cbind(transMatrix1[,1:2], mat[,3])
transMatrix1.df <- as.data.frame(transMatrix1.df)
ggplot(transMatrix1.df, aes(x = V1, y = V2, group = V3)) + geom_path()

transMatrix2 <- mat[,1:2]%*%Rot(-120)
transMatrix2.df <- cbind(transMatrix2[,1:2], mat[,3])
transMatrix2.df <- as.data.frame(transMatrix2.df)
ggplot(transMatrix2.df, aes(x=V1, y = V2, group = V3)) + geom_path()

transMatrix3 <- mat[,1:2]%*%Rot(85)
transMatrix3.df <- cbind(transMatrix3[,1:2], mat[,3])
transMatrix3.df <- as.data.frame(transMatrix3.df)
ggplot(transMatrix3.df, aes(x=V1, y = V2, group = V3)) + geom_path()


#2.3
#1. 
X <- matrix(c(1,0, 1,3, 2,3), 3, 2, byrow = TRUE)
plot(x = X[,1], y =X[,2], type="l", asp=1)

X_flip<- X%*%matrix(c(-1, 0, 0, 1), 2, 2)
plot(X_flip[,1], X_flip[,2], type="l", asp=1)

#2.
X_stretch <- X%*%matrix(c(1, 0, 0, 2), 2, 2) 
plot(X_stretch[,1], X_stretch[,2], type="l", asp=1)

#3.
flipStretchMatrix <- function(mat, alpha, stretchFactor){
  mat <- mat[,1:2] %*% Rot(alpha)
  mat[,2] <- mat[,2] * stretchFactor
  return(mat)
}
flipStretchData <- function(data, alpha, stretchFactor){
  matrix_flip <- data.matrix(data)
  matrix_flip <- flipStretchMatrix(matrix_flip, alpha, stretchFactor)
  data$x <- matrix_flip[,1]
  data$y <- matrix_flip[,2]
  ggplot(data, aes(x, y, group=group)) + geom_path()
}
flipStretchData(data, 45, 3)

# trans_mat <- mat[,1:2]%*%Rot(-45) 
# mat[,2] <- mat[,2]%*%matrix(c(1, 0, 0, 2), 2, 2) 
# #trans_mat <- mat[,1:2]%*%Rot(45)
# trans_mat.df <- as.data.frame(trans_mat)
# trans_mat.df <- cbind(trans_mat.df[,1:2], mat[,3])
# 
# ggplot(trans_mat.df, aes(x=V1, y = V2, group = mat[,3])) + geom_path()
