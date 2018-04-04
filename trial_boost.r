# data import
url <- c("http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/y_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_test.csv")
download.file(url, c("x_train.csv", "y_train.csv", "x_test.csv"))
x_train <- read.csv("./x_train.csv", header=TRUE)
y_train <- read.csv("./y_train.csv", header=TRUE)
x_test <- read.csv("./x_test.csv", header=TRUE)
