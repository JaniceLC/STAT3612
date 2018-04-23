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
rcp <- recipe(~.-Region, data=train.x) %>%
  #step_ordinalscore(EdMother, EdFather) %>%
  step_meanimpute(EdMother, EdFather) %>%
  step_interact(terms = ~EdMother:EdFather+
                  Teacher_1:all_numeric()+
                  NumBook:all_numeric(),sep ="x") %>%
  #step_bs(all_numeric())%>%
  step_ns(all_numeric(), df=3) %>%
  #step_center(all_numeric()) %>%
  #step_scale(all_numeric()) %>%
  #step_pca(all_numeric(), threshold=0.9) %>%
  step_dummy(Gender, Didactic) %>%
  step_interact(terms=~contains("Didactic"):starts_with("Ed")+ 
                  contains("Didactic"):starts_with("ExMotif")+
                  contains("Didactic"):starts_with("InMotif")+
                  contains("Didactic"):contains("Gender")) %>%
  #step_ns(all_numeric(), df=3) %>%
  prep(training=train.x)

make.data <- function(df){
  df <- df %>% 
        recast.type() %>% 
        add.didactic()
  df <- bake(rcp, newdata=df)
  return(df)
}

#####################################
#####################################

train.x.bin <- make.data(train.x)
test.x.bin <- make.data(test.x)
train.y$FlagAIB <- as.factor(train.y$FlagAIB)