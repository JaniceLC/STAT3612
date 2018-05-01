#####################
# Data manipulation #
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

val.index <- sample(c(1:nrow(train.x)), size =floor(0.2*nrow(train.x)), replace=FALSE)
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

impute.value <- function(df){
  asia.mother.index <- intersect(which(df$Didactic=="ASIA"), which(is.na(df$EdMother)))
  nasia.mother.index <- intersect(which(df$Didactic=="NASIA"), which(is.na(df$EdMother)))
  sgp.mother.index <- intersect(which(df$Didactic=="SGP"), which(is.na(df$EdMother)))
  asia.father.index <- which(df$Didactic=="ASIA" && is.na(df$EdFather))
  nasia.father.index <- which(df$Didactic=="NASIA" && is.na(df$EdFather))
  sgp.father.index <- which(df$Didactic=="SGP" && is.na(df$EdFather))
  asia.mother.fill <- which.max(table(df$EdMother))[[1]]
  nasia.mother.fill <- which.max(table(df$EdMother))[[1]]
  sgp.mother.fill <- which.max(table(df$EdMother))[[1]]
  asia.father.fill <- which.max(table(df$EdFather))[[1]]
  nasia.father.fill <- which.max(table(df$EdFather))[[1]]
  sgp.father.fill <- which.max(table(df$EdFather))[[1]]
  imputed=c(asia.mother.fill, nasia.mother.fill, sgp.mother.fill,
            asia.father.fill, nasia.father.fill, sgp.father.fill)
  return(imputed)
}

# impute by the most frequent value in each group
impute.ed <- function(df, imputed){
  asia.mother.index <- intersect(which(df$Didactic=="ASIA"), which(is.na(df$EdMother)))
  nasia.mother.index <- intersect(which(df$Didactic=="NASIA"), which(is.na(df$EdMother)))
  sgp.mother.index <- intersect(which(df$Didactic=="SGP"), which(is.na(df$EdMother)))
  asia.father.index <- which(df$Didactic=="ASIA" && is.na(df$EdFather))
  nasia.father.index <- which(df$Didactic=="NASIA" && is.na(df$EdFather))
  sgp.father.index <- which(df$Didactic=="SGP" && is.na(df$EdFather))
  df$EdMother[asia.mother.index] <- imputed[1]
  df$EdMother[nasia.mother.index] <- imputed[2]
  df$EdMother[sgp.mother.index] <- imputed[3]
  df$EdFather[asia.father.index] <- imputed[4]
  df$EdFather[nasia.father.index] <- imputed[5]
  df$EdFather[sgp.father.index] <- imputed[6]
  return(df)
}

# set up recipe
make.data <- function(df, imputed){
  df <- df %>% 
    recast.type() %>% 
    add.didactic() 
   # impute.ed(imputed)
  
  rcp <- recipe(~.-Region, data=df) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    # step_ns(all_numeric(), df = 3) %>%
    step_interact(terms = ~contains("EdMother"):contains("EdFather"), sep ="x") %>%
    step_dummy(Gender) %>%
    step_dummy(Didactic) %>%
    step_interact(terms=~contains("Didactic"):starts_with("Ed")+ 
                    contains("Didactic"):starts_with("ExMotif")+
                    contains("Didactic"):starts_with("InMotif")+
                    contains("Didactic"):contains("Gender")) %>%
    prep(training=df)
  df <- bake(rcp, newdata=df)
  df$Region <- NULL
  return(df)
}

#####################################
#####################################

#imputed <- train.x %>% recast.type() %>% add.didactic() %>% impute.value()
train.x <- make.data(train.x, imputed)

train = cbind(FlagAIB = train.y, train.x)
val.x <- make.data(val.x, imputed)
test.x <- make.data(test.x, imputed)



#########Remove Outlier from train.x 
# quantify through logistic regression
model <- glm(FlagAIB~., data=train, family="binomial")
# cooks.dist <- cooks.distance(model, infl=influence(model, do.coef=TRUE))
cooks.dist <- cooks.distance(model)
cutoff=quantile(cooks.dist, prob=0.995)
plot(cooks.dist, pch=19, cex=0.5)
abline(h=cutoff, col="red", lwd=2)
train = train[which(cooks.dist<cutoff),]
train.x <- train[,-1]
train.y <- train[,1]

