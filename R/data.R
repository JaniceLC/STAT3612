#####################
# Data manipulation #
#####################

library(dplyr)
library(recipes)

# data set import
url <- c("http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/y_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_test.csv")
train.x <- read.csv(url[1], header=TRUE, row.names=1)
train.y <- read.csv(url[2], header=TRUE, row.names=1)
test.x <- read.csv(url[3], header=TRUE, row.names=1)

# recast data types
train.x$Gender <- as.factor(train.x$Gender)
train.x$Region <- as.factor(train.x$Region)
train.x$NumBook <- as.numeric(train.x$NumBook)
train.x$NumDevice <- as.numeric(train.x$NumDevice)
train.x$EdMother <- as.ordered(train.x$EdMother)
train.x$EdFather <- as.ordered(train.x$EdFather)
train.y$FlagAIB <- as.factor(train.y$FlagAIB)

# complete train data
train <- cbind(train.y, train.x)

# levels(train.y$FlagAIB) <- c("A", "B")

# feature engineering
# distinguish Asia and Non-Asia countries and regions
# train.x.aug <- train.x %>% mutate(Continent=ifelse(Region %in% c("HKG", "JPN", "SGP", "TWN"), 
#                                                "ASIA", "NASIA")) %>% as.data.frame()

# dummy/binary coding
# recipe can be recreated on test data
library(recipes)
rcp <- recipe(~., data=train.x) %>% 
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  #step_pca(all_numeric(), threshold=0.9) %>%
  step_dummy(Gender, Region, EdFather, EdMother) %>%
  prep(training=train.x)

# bake train set
train.x.bin <- bake(rcp, newdata=train.x) %>% as.data.frame()
train.bin <- cbind(FlagAIB=train.y$FlagAIB, train.x.bin)

# bake test set
test.x.bin <- bake(rcp, newdata=test.x) %>% as.data.frame()
