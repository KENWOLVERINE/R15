i <- readline(prompt="Enter a number: ")
i <- as.integer(i)
if((i %% 2)==0){
print(paste("The number",i,"is Even"))
}else {
print(paste("The number",i,"is Odd"))
}

n = as.integer(readline(prompt = "Enter a
number: "))
for (i in 1:10){
print(paste(n,"*",i,'=',n*i))
}

f = as.integer(readline(prompt = "Enter a
number to find factorial: "))
fact = 1
for ( i in 1:f){
fact = fact * i
}
print(paste("Factorial of",f,"is:",fact))

prime = function(number){
if(number <= 1){
return (FALSE)
}
for ( i in 2:sqrt(number)){
if(number %% i == 0){
return(FALSE)
}
}
return(TRUE)
}
number = as.integer(readline(prompt="Enter a
check the number is prime or not: "))
if(prime(number)){
print(paste(number,"is a prime number"))
}else{
print(paste(number,"is not a prime
number"))
}

num = as.integer(readline(prompt="Enter a number: "))
sum = 0
temp = num
while(temp > 0) {
digit = temp %% 10
sum = sum + (digit ^ 3)
temp = floor(temp / 10)
}
if(num == sum) {
print(paste(num, "is an Armstrong number"))
} else {
print(paste(num, "is not an Armstrong number"))
}

# Load dataset
data <- mtcars
# Descriptive statistics
summary(data)
mean_mpg <- mean(data$mpg)
median_mpg <- median(data$mpg)
sd_mpg <- sd(data$mpg)
var_mpg <- var(data$mpg)
fivenum_mpg <- fivenum(data$mpg)
# Print results
cat("Mean MPG:", mean_mpg, "\n")
cat("Median MPG:", median_mpg, "\n")
cat("Standard Deviation MPG:", sd_mpg, "\n")
cat("Variance MPG:", var_mpg, "\n")
cat("Five-number summary MPG:", fivenum_mpg, "\n")
# Data visualization
hist(data$mpg, main="Histogram of Miles Per Gallon", xlab="MPG", col="blue", border="black")
boxplot(data$mpg, main="Boxplot of Miles Per Gallon", ylab="MPG")
plot(data$mpg, data$wt, main="Scatter plot of MPG vs Weight", xlab="Weight", ylab="MPG", pch=19)
barplot(table(data$cyl), main="Barplot of Cylinder Counts", xlab="Number of Cylinders",
ylab="Frequency", col="green")

library(factoextra)
df <- mtcarsdf <- na.omit(df)
df <- scale(df)
png(file = "kmeans1.png")
km <- Kmeans(df,centers=4,nstart=25)
fvitz-cluster(km,data=df)
dev.off()png(file = "kmeans2.png")
km <- Kmeans(df,centers=5,nstart=25)
fvitz-cluster(km,data=df)
dev.off()


if (!require(arules)) install.packages("arules")
library(arules)
transactions <- data.frame(
transaction = c("T1", "T1", "T2", "T2", "T3", "T3"),
item = c("bread", "milk", "cereal", "beer", "bread", "cheese"))
tx <- as(split(transactions$item,transactions$transaction),"transactions")
min_support <- 0.2
min_confidence <- 0.6
rules <- apriori(tx, minlen=2, minval=min_support, confidence=min_confidence)
inspect(rules)

library(stats)
x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 5, 4, 5)
model <- lm(y ~ x)
summary(model)
new_x <- 6
predicted_y <- predict(model, newdata = data.frame(x = new_x))
cat("Predicted y for x =", new_x, ":", predicted_y)