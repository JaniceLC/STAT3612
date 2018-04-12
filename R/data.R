#####################
# Data manipulation #
#####################

if(!require(dplyr)) install.packages("dplyr")
if(!require(recipes)) install.packages("recipes")


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
  step_ordinalscore(EdMother, EdFather) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  # step_pca(all_numeric(), threshold=0.9) %>%
  step_dummy(Gender, Region) %>%
  prep(training=train.x)

#recipe1 to add EdFather*Edmather 
rcp.fm <- 
  recipe(~., data=train.x) %>%
  step_ordinalscore(EdMother, EdFather) %>% 
  step_interact(terms = ~ EdMother:EdFather, sep = "_x_" ) %>% #interaction bewteen Edmother and Edfather
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  # step_pca(all_numeric(), threshold=0.9) %>%
  step_dummy(Gender, Region) %>%
  prep(training=train.x)


#recipe1 to add abs(EdFather-Edmather)
train.x.fd = train.x
train.x.fd$M_d_F = abs(as.numeric(train.x.fd$EdFather) - as.numeric(train.x.fd$EdMother))
rcp.fd = recipe(~., data=train.x.fd) %>%
  step_ordinalscore(EdMother, EdFather) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  # step_pca(all_numeric(), threshold=0.9) %>%
  step_dummy(Gender, Region) %>%
  prep(training=train.x.fd)

train.x.fd.bin <- bake(rcp.fd, newdata = train.x) %>% as.data.frame() 
#rcp %>% step_interact(terms = ~ (abs(EdFather - EdMother)), sep = "_d_") #d: difference

# bake train set
train.x.bin <- bake(rcp, newdata=train.x) %>% as.data.frame()
train.bin <- cbind(FlagAIB=train.y$FlagAIB, train.x.bin)
train.x.fm.bin <- bake(rcp.fm, newdata = train.x) %>% as.data.frame()
train.fm.bin <- cbind(FlagAIB=train.y$FlagAIB, train.x.fm.bin)
train.x.fd.bin = bake(rcp.fd, newdata =train.x.fd) %>% as.data.frame()
train.fd.bin = cbind(FlagAIB=train.y$FlagAIB, train.x.fd.bin)

# bake test set
test.x.bin <- bake(rcp, newdata=test.x) %>% as.data.frame()
test.x.fm.bin <- bake(rcp.fm, newdata = test.x) %>% as.data.frame()

test.x.fd = test.x
test.x.fd$M_d_F = abs(as.numeric(test.x.fd$EdFather) - as.numeric(test.x.fd$EdMother))
test.x.fd.bin = bake(rcp.fd, newdata =test.x.fd) %>% as.data.frame()
test.fd.bin = cbind(FlagAIB=test.y$FlagAIB, test.x.fd.bin)


