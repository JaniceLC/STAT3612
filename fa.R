train.x.bin$Continent_NASIA <- NULL
install.packages("polycor")
library(polycor)
het.mat <- hetcor(train.x.bin)$cor
fa.1 <- factanal(covmat=het.mat, factors=4, rotation="varimax")
install.packages("psych")
library(psych)
fa.2 <- fa(r=het.mat, nfactors=3, n.obs=nrow(train.x.bin), rotate="varimax")
