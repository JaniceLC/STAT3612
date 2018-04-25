#####################
# Data manipulation #
#####################

if(!require(dplyr)) install.packages("dplyr")
if(!require(recipes)) install.packages("recipes")

library(dplyr)
library(recipes)

#################################################\
#################################################
# data set import
url <- c("http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/y_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_test.csv")
train.x <- read.csv(url[1], header=TRUE, row.names=1)
train.y <- read.csv(url[2], header=TRUE, row.names=1)
test.x <- read.csv(url[3], header=TRUE, row.names=1)

# recast data types
recast.type <- function(df){
  df$Gender <- as.factor(df$Gender)
  df$Region <- as.factor(df$Region)
  df$NumBook <- as.numeric(df$NumBook)
  df$NumDevice <- as.numeric(df$NumDevice)
  df$EdMother <- as.numeric(df$EdMother)
  df$EdFather <- as.numeric(df$EdFather)
  df$EdMother[df$EdMother==8] <- NA
  df$EdFather[df$EdFather==8] <- NA
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
# set up recipe
make.data <- function(df){
  df <- df %>% 
    recast.type() %>% 
    add.didactic()
  rcp <- recipe(~.-Region, data=df) %>%
    step_meanimpute(EdMother, EdFather) %>%
    step_interact(terms = ~EdMother:EdFather, sep ="x") %>%
    step_dummy(Gender, one_hot=TRUE) %>%
    step_dummy(Didactic, one_hot=TRUE) %>%
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

train.x.bin <- make.data(train.x)
test.x.bin <- make.data(test.x)