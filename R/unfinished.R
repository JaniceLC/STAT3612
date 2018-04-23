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

test.x$Gender <- as.factor(test.x$Gender)
test.x$Region <- as.factor(test.x$Region)
test.x$NumBook <- as.numeric(test.x$NumBook)
test.x$NumDevice <- as.numeric(test.x$NumDevice)
test.x$EdMother <- as.ordered(test.x$EdMother)
test.x$EdFather <- as.ordered(test.x$EdFather)
meanEdMo = round(mean(as.numeric(train.x$EdMother)))
meanEdFa = round(mean(as.numeric(train.x$EdFather)))
for( i in 1 : nrow(train.x) ){
  train.x$EdMother[i] = ifelse(train.x$EdMother[i] == 8, meanEdMo, train.x$EdMother[i])
  train.x$EdFather[i] = ifelse(train.x$EdFather[i] == 8, meanEdFa, train.x$EdFather[i])
}

for( i in 1 : nrow(test.x) ){
  test.x$EdMother[i] = ifelse(test.x$EdMother[i] == 8, meanEdMo, test.x$EdMother[i])
  test.x$EdFather[i] = ifelse(test.x$EdFather[i] == 8, meanEdFa, test.x$EdFather[i])
}

# complete train data
train.x <- train.x %>% mutate(Continent=ifelse(Region %in% c("HKG", "JPN", "SGP", "TWN"), "ASIA", "NASIA")) %>% as.data.frame()
test.x <- test.x %>% mutate(Continent=ifelse(Region %in% c("HKG", "JPN", "SGP", "TWN"), "ASIA", "NASIA")) %>% as.data.frame()
train <- cbind(train.y, train.x)

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


classDistance = function(x){
  xn = x %>% select(which(sapply(., is.numeric)))
  d = data.frame( d1 = mahalanobis(xn, u1, s),
                  d2 = mahalanobis(xn, u2, s))
  x_modify = x
  x_modify$Dist = as.factor(apply(d, 1, which.max))
  return(x_modify)
}

train.x = classDistance(train.x)
test.x = classDistance(test.x)




rcp <- recipe(~., data=train.x) %>%
  step_ordinalscore(EdMother, EdFather) %>%
  step_interact(terms = ~ EdMother:EdFather+
                  Teacher_1:all_numeric()+
                  Region:starts_with("Ed") + 
                  Region:starts_with("ExMotif")+
                  Region:starts_with("InMotif")+
                  Region:Gender+
                  NumBook:all_numeric(), sep = "x" ) %>%
  #step_bs(all_numeric())%>%
  step_ns(all_numeric(), df=3) %>%
  #step_center(all_numeric()) %>%
  #step_scale(all_numeric()) %>%
  #step_pca(all_numeric(), threshold=0.9) %>%
  step_dummy(Gender, Region, Continent, Dist) %>%
  step_interact(terms=~contains("Gender"):contains("Continent")) %>%
  prep(training=train.x)

train.x.bin <- bake(rcp, newdata=train.x) %>% as.data.frame()
train.bin = cbind(FlagAIB = train.y$FlagAIB, train.x.bin)

test.x.bin <- bake(rcp, newdata = test.x) %>% as.data.frame()


