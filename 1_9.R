NA_position <- function(x ,y){
  all(is.na(x) == is.na(y))
}
v1  <- c(NA, 2, 2)
v2  <- c(3, 4, NA)
all(is.na(v1) == is.na(v2))
NA_position(v1,v2)

v1  <- c(1, 2, 3)
v2  <- c(3, 4, 6)
NA_position(v1,v2)

v1  <- c(1, 2, NA)
v2  <- c(3, 4, NA)
NA_position(v1,v2)

v1  <- c(NA, 2, NA)
v2  <- c(3, 4, NA)
NA_position(v1,v2)
#1
x <- table(mtcars[,c("am", "vs")])
y <- table(mtcars[1:20,c("am", "vs")])
cx <- chisq.test(table(mtcars[,c("am", "vs")]))
cy <- fisher.test(table(mtcars[1:20,c("am", "vs")]))
cx$p.value
cx$parameter[["df"]]
cx$statistic[["X-squared"]]
cy$p.value
min(x)
min(y)
smart_test <- function(x){
  x <- table(x)
  if(min(x) > 5){
    cx <- chisq.test(x)
    ret_f <- c(cx$statistic[["X-squared"]], cx$parameter[["df"]], cx$p.value)
  }
  else{
    cx <- fisher.test(x)
    ret_f <- cx$p.value 
  }
  return(ret_f)
}
smart_test(x)
smart_test(y)

#2
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)
test_data$V4 <- test_data$V3
x <- sapply(test_data, table)
y <- apply(x, 2, function(x) chisq.test(x)$p.value)
min_p <- min(y)
names(y[y==min_p])

#3
x <- sapply(iris[,1:4], function(x) x > mean(x))
x <- apply(x, 1, sum)
iris$important_cases <- factor(ifelse(x >= 3, 1, 0),labels = c("No", "Yes"))

#4
num_cols <- sapply(iris, is.numeric)
5 %/%  2
get_important_cases <- function(x){
  num_cols <- sapply(x, is.numeric)
  more_then_m <- sapply(x[num_cols], function(x) x > mean(x))
  more_then_m <- apply(more_then_m, 1, sum)
  x$important_cases <- factor(ifelse(more_then_m > (sum(num_cols) %/% 2), 1, 0),levels = c(0,1),labels = c("No", "Yes"))
  return(x)
  
}
test_data <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))
get_important_cases(test_data)

#5
v <- c(1, 2, 3, 3, 3, 4, 5)
v <- c(1, 1, 1, 2, 3, 3, 3)
x <- unique(v)
names(x) <- x
mx <- sapply(x, function(x,y) sum(y == x), v, USE.NAMES = T)
as.numeric(names(mx[mx == max(mx)]))

#6
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")
str(test_data)

ct <- chisq.test(x = test_data$Drugs, y = test_data$Result)
resd <- ct$residuals
resd <- as.data.frame(resd)
c(as.character.factor(resd$test_data.Drugs[resd$Freq == max(resd$Freq)])
  , as.character.factor(resd$test_data.Result[resd$Freq == max(resd$Freq)]))

#7
library("ggplot2")
d <- diamonds
ggplot(d, aes(x = color, fill = cut)) +
  geom_bar(position=position_dodge())
