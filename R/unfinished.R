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
train.x <- train.x %>% mutate(Continent=ifelse(Region %in% c("HKG", "JPN", "SGP", "TWN"), 
                                               "ASIA", "NASIA")) %>% as.data.frame()

train <- cbind(train.y, train.x)

rcp2 <- recipe(~., data=train) %>%
  step_ordinalscore(EdMother, EdFather) %>%
  step_interact(terms = ~ EdMother:EdFather+
                  Teacher_1:all_numeric()+
                  NumBook:all_numeric(), sep = "x" ) %>%
  step_classdist(all_numeric(), class = "train.y$FlagAIB", role = "predictor" ) %>%
  #step_ns(all_cnumeric(), df=3) %>%
  #step_center(all_numeric()) %>%
  #step_scale(all_numeric()) %>%
  #step_pca(all_numeric(), threshold=0.9) %>%
  step_dummy(Gender, Region, Continent) %>%
  step_interact(terms=~contains("Gender"):contains("Continent")) %>%
  prep(training=train.x)

train.x.d.bin <- bake(rcp2, newdata=train.x) %>% as.data.frame()
train.x.d.bin = train.d.bin
train.x.d.bin$FlagAIB = NULL


library(dplyr)
train.x0 = train %>% filter(FlagAIB=='0')%>% 
  select(which(sapply(., is.numeric)))
s1 = cov(train.x0)
u1 = apply(train.x0,2,mean)
n1 = nrow(train.x0)

train.x1 = train %>% filter(FlagAIB=='1')%>% 
  select(which(sapply(., is.numeric)))
s2 = cov(train.x1)
u2 = apply(train.x1,2,mean)
n2 = nrow(train.x1)
s = ((n1 -1)*s1 +(n2 - 1)*s2)/(n1 + n2 - 2)
train.n = train.x %>% select(which(sapply(., is.numeric)))

d = data.frame(d1 = mahalanobis(train.n, u1, s),
               d2 = mahalanobis(train.n, u2, s))
train.x$Dist = apply(d, 1, which.max)
classDistance = function(x){
  d1 = 
  d2 = 
  
}

test.x$Dist = 

