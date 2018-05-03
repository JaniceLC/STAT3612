#####################
# DATA MANIPULATION #
#####################

if(!require(dplyr)) install.packages("dplyr")
if(!require(recipes)) install.packages("recipes")
if(!require(car)) install.packages("car")

library(dplyr)
library(recipes)
library(car)

#################################################
#################################################
# data set import
url <- c("http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/y_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_test.csv")
train.x <- read.csv(url[1], header=TRUE, row.names=1)
train.y <- read.csv(url[2], header=TRUE, row.names=1)
test.x <- read.csv(url[3], header=TRUE, row.names=1)

# set randomization seed
set.seed(2018)

# separate a validation set of 20% of original data
val.index <- sample(c(1:nrow(train.x)), size=floor(0.2*nrow(train.x)), replace=FALSE)
val.x <- train.x[val.index,]
val.y <- train.y[val.index,]
train.x <- train.x[-val.index,]
train.y <- train.y[-val.index,]

# recast data types
recast.type <- function(df){
  df$Gender <- as.factor(df$Gender)
  df$Region <- as.factor(df$Region)
  df$NumBook <- as.numeric(df$NumBook)
  df$NumDevice <- as.numeric(df$NumDevice)
  df$EdMother <- as.numeric(df$EdMother)
  df$EdFather <- as.numeric(df$EdFather)
  df$EdMother[df$EdMother==8] <- 3
  df$EdFather[df$EdFather==8] <- 3
  return(df)
}

# merging regions
add.didactic <- function(df){
  df <- df %>% 
    mutate(Didactic=ifelse(Region %in% c("HKG", "JPN", "TWN"), "ASIA", 
                           ifelse(Region %in% c("USA", "ENG"), "NASIA", "SGP"))) %>% 
    as.data.frame()
  df$Didactic <- as.factor(df$Didactic)
  return(df)
}

income.women <- data.frame(
  EdMother=rep(1:7, 6),
  Region=rep(c("USA", "ENG", "HKG", "JPN", "TWN", "SGP"), each=7),
  InMother=c(c(77, 77, 100, 100, 134, 165, 205),
    c(61, 61, 100, 100, 125, 168, 234),
    rep(c(80, 80, 100, 100, 126, 154, 215), 4))
)

income.men <- data.frame(
  EdFather=rep(1:7, 6),
  Region=rep(c("USA", "ENG", "HKG", "JPN", "TWN", "SGP"), each=7),
  InFather=c(c(68, 68, 100, 100, 113, 171, 244),
             c(72, 72, 100, 100, 124, 148, 174),
             rep(c(75, 75, 100, 100, 109, 135, 170), 4))
)

add.income <- function(df){
  df <- df %>% 
    left_join(income.women) %>%
    left_join(income.men) %>%
    mutate(Income=InMother+InFather) %>%
    as.data.frame()
  return(df)
}

schooling.women <- data.frame(
  EdMother=c(1:7),
  ScMother=c(6, 9, 12, 13.5, 14.5, 16, 20.5)
)

schooling.men <- data.frame(
  EdFather=c(1:7),
  ScFather=c(6, 9, 12, 13.5, 14.5, 16, 20.5)
)

add.schooling <- function(df){
  df <- df %>%
    left_join(schooling.women) %>%
    left_join(schooling.men) %>%
    as.data.frame()
  return(df)
}

# set up recipe
make.data <- function(df){
  df <- df %>% 
    recast.type() %>% 
    add.didactic() %>%
    add.schooling() 
  #  select(-one_of(c("Region", "EdMother", "EdFather", "InMother", "InFather")))
  df$Region = NULL
  df$EdMother= NULL
  df$EdFather = NULL
  df$InMother = NULL
  df$InFather = NULL 
  df <- recipe(~., data=df) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    #step_ns(all_numeric(), df = 3) %>%
    step_dummy(Gender) %>%
    step_dummy(Didactic) %>%
    step_interact(terms=~contains("Didactic"):starts_with("Sc")+
                    contains("Didactic"):starts_with("ExMotif")+
                    contains("Didactic"):starts_with("InMotif")+
                    contains("Didactic"):contains("Gender")  ) %>%
    prep(training=df) %>%
    bake(newdata=df)
  return(df)
}

#####################################
#####################################

# data treatment
train.x.bin <- make.data(train.x)
train.bin = cbind(FlagAIB = train.y, train.x.bin)
val.x.bin <- make.data(val.x)
val.x.ns <- make.data.ns(val.x)
test.x <- make.data(test.x)



# remove outliers from train.x
# quantify through logistic regression
model <- glm(FlagAIB~., data=train.bin, family="binomial")

cooks.dist <- cooks.distance(model)
cutoff=quantile(cooks.dist, prob=0.99)
# plot cooks distance and cutoff
plot(cooks.dist, pch=19, cex=0.5)
abline(h=cutoff, col="red", lwd=2)

train.bin = train.bin[which(cooks.dist<cutoff),]
train.x.bin <- train.bin[,-1]
train.y.bin <- train.bin[,1]




make.data.ns <- function(df){
  df <- df %>% 
    recast.type() %>% 
    add.didactic() %>%
    add.schooling() 
   # select(-one_of(c("Region", "EdMother", "EdFather", "InMother", "InFather")))
  df$Region = NULL
  df$EdMother= NULL
  df$EdFather = NULL
  df$InMother = NULL
  df$InFather = NULL 
  df <- recipe(~., data=df) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_ns(all_numeric(), df = 3) %>%
    step_dummy(Gender) %>%
    step_dummy(Didactic) %>%
    step_interact(terms=~contains("Didactic"):starts_with("Sc")+ 
                    contains("Didactic"):starts_with("ExMotif")+
                    contains("Didactic"):starts_with("InMotif")+
                    contains("Didactic"):contains("Gender")) %>%
    prep(training=df) %>%
    bake(newdata=df)
  return(df)
}

train.x.ns <- make.data.ns(train.x)
train.ns = cbind(FlagAIB = train.y, train.x.ns)
val.x.ns = make.data.ns(val.x)
#no evident outlier found. 
model.ns <- glm(FlagAIB~., data=train.ns, family="binomial")
cooks.dist.ns <- cooks.distance(model.ns)
cutoff=quantile(cooks.dist.ns, prob=0.995)

# plot cooks distance and cutoff
plot(cooks.dist.ns, pch=19, cex=0.5)
abline(h=cutoff, col="red", lwd=2)

train.ns= train.ns[which(cooks.dist<cutoff),]
train.x.ns <- train.ns[,-1]
train.y.ns <- train.ns[,1]



