#1
test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
test_data  <- transform(test_data, x = factor(x), y = factor(y)) 
lmt <- glm(data = test_data, formula = y ~ x, family = "binomial")
exp(coef(lmt))

#2
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
var_names = c("X4", "X2", "X1")
test_data[var_names]
test_data[var_names] <- sapply(test_data[var_names], function(x) x <-  x - mean(x))
test_data

#3
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv")
#test_data  <- transform(test_data, weight = factor(weight), length = factor(length), width = factor(width)) 
lmt <- glm(data = test_data, formula = is_prohibited ~ weight + length + width + type, family = "binomial")
result <- anova(lmt, test = "Chisq")
res <- row.names(subset(result, result$`Pr(>Chi)` < 0.05 ))
if(length(res) == 0)
  c("Prediction makes no sense")
else
  res

#4
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")
lmt <- glm(data = test_data, formula = is_prohibited ~ weight + length + width + type, family = "binomial")
data_for_predict$prd <- predict(lmt, newdata = data_for_predict, type = "response")
res <- subset(data_for_predict, data_for_predict$prd == max(data_for_predict$prd))
as.character(res$passangers)
