1. i <- readline(prompt="Enter a number: ")
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

2.# Load dataset
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

3.data(iris)
str(iris)
install.packages("ClusterR")
install.packages("cluster")
library(ClusterR)
library(cluster)
iris_1 <- iris[, -5]
set.seed(240)
kmeans.re <- kmeans(iris_1, centers = 3, nstart = 20)
kmeans.re
kmeans.re$cluster
cm <- table(iris$Species, kmeans.re$cluster)
cm
plot(iris_1[c("Sepal.Length", "Sepal.Width")])
plot(iris_1[c("Sepal.Length", "Sepal.Width")], 
     col = kmeans.re$cluster)
plot(iris_1[c("Sepal.Length", "Sepal.Width")], 
     col = kmeans.re$cluster, 
     main = "K-means with 3 clusters")
kmeans.re$centers
kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")]

points(kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")], 
       col = 1:3, pch = 8, cex = 3) 
y_kmeans <- kmeans.re$cluster
clusplot(iris_1[, c("Sepal.Length", "Sepal.Width")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster iris"),
         xlab = 'Sepal.Length',
         ylab = 'Sepal.Width')



4.if (!require(arules)) install.packages("arules")
library(arules)
transactions <- data.frame(
transaction = c("T1", "T1", "T2", "T2", "T3", "T3"),
item = c("bread", "milk", "cereal", "beer", "bread", "cheese"))
tx <- as(split(transactions$item,transactions$transaction),"transactions")
min_support <- 0.2
min_confidence <- 0.6
rules <- apriori(tx, minlen=2, minval=min_support, confidence=min_confidence)
inspect(rules)

5.if (!require(stats)) install.packages("stats")
library(stats)
x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 5, 4, 5)
model <- lm(y ~ x)
summary(model)
new_x <- 6
predicted_y <- predict(model, newdata = data.frame(x = new_x))
cat("Predicted y for x =", new_x, ":", predicted_y)

6.library(dplyr)
library(pROC)
set.seed(123)
data <- data.frame(
  outcome = as.factor(sample(c(0, 1), size = 100, replace = TRUE)), 
  predictor1 = rnorm(100, mean = 50, sd = 10),                     
  predictor2 = rnorm(100, mean = 0, sd = 1)                        
)
head(data)
model <- glm(outcome ~ predictor1 + predictor2, data = data, family = binomial)
summary(model)
predictions <- predict(model, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predicted_classes, Actual = data$outcome)
print("Confusion Matrix:")
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")
roc_curve <- roc(data$outcome, predictions)
plot(roc_curve, main = "ROC Curve")
cat("AUC:", auc(roc_curve), "\n")
odds_ratios <- exp(coef(model))
cat("Odds Ratios:\n")
print(odds_ratios)
new_data <- data.frame(
  predictor1 = c(45, 55),
  predictor2 = c(0.8, 1.2)
)
new_predictions <- predict(model, newdata = new_data, type = "response")
cat("Predictions for new data:\n")
print(new_predictions)

7.install.packages("naivebayes")
library(naivebayes)
data(iris)
set.seed(123)
train_indices <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]
model <- naive_bayes(Species ~ ., data = train_data)
predictions <- predict(model, test_data)
confusion_matrix <- table(test_data$Species, predictions)
print(confusion_matrix)

8.install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
data(iris)
set.seed(123)
train_indices <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]
model <- rpart(Species ~ ., data = train_data, method = "class")
predictions <- predict(model, test_data, type = "class")
confusion_matrix <- table(test_data$Species, predictions)
print(confusion_matrix)
rpart.plot(model)

9.install.packages("ggplot2")
install.packages("factoextra")
library(ggplot2)
library(factoextra)
data(iris)
iris_data <- iris[, -5]
iris_scaled <- scale(iris_data)
pca_result <- prcomp(iris_scaled, center = TRUE, scale. = TRUE)
fviz_eig(pca_result, addlabels = TRUE)
fviz_pca_biplot(pca_result, geom.ind = "point", pointshape = 21, 
pointsize = 2, fill.ind = iris$Species, col.ind = "black",
                palette = "jco", addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE)
fviz_pca_ind(pca_result, geom.ind = "point", pointshape = 21, 
             pointsize = 2, fill.ind = iris$Species, col.ind = "black",
             palette = "jco", addEllipses = TRUE, repel = TRUE)
fviz_pca_var(pca_result, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

10.A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
svd_result <- svd(A)
U <- svd_result$u
D <- svd_result$d
V <- svd_result$v
cat("Matrix A:\n")
print(A)
cat("\nMatrix U:\n")
print(U)
cat("\nSingular Values (D):\n")
print(D)
cat("\nMatrix V:\n")
print(V)

