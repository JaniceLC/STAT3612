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