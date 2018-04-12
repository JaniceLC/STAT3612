#############################
# Exploratory Data Analysis #
#############################

library(ggplot2)
library(gridExtra)
library(ggcorrplot)

# barplot (FlagAIB)
ggplot(train, aes(x=FlagAIB, fill=FlagAIB)) +
  geom_bar(stat="count") +
  geom_label(stat="count", aes(label=..count..)) +
  theme_bw()

# barplot (Gender)
p1 <- ggplot(train, aes(x=Gender, fill=Gender)) + 
  geom_bar(stat="count") +
  geom_label(stat="count", aes(label=..count..)) +
  theme_bw()
p2 <- ggplot(train, aes(x=Gender, fill=FlagAIB)) +
  geom_bar(stat="count", position="dodge") +
  geom_label(stat="count", aes(label=..count..)) +
  theme_bw()
grid.arrange(p1, p2, ncol=2)

# barplot (EdMother)
p1 <- ggplot(train, aes(x=EdMother, fill=EdMother)) +
  geom_bar(stat="count") +
  geom_label(stat="count", aes(label=..count..)) +
  theme_bw()
p2 <- ggplot(train, aes(x=EdMother, fill=FlagAIB)) +
  geom_bar(stat="count", position="dodge") +
  geom_label(stat="count", aes(label=..count..)) +
  theme_bw()

# barplot (EdFather)
p3 <- ggplot(train, aes(x=EdFather, fill=EdFather)) +
  geom_bar(stat="count") +
  geom_label(stat="count", aes(label=..count..)) +
  theme_bw()
p4 <- ggplot(train, aes(x=EdFather, fill=FlagAIB)) +
  geom_bar(stat="count", position="dodge") +
  geom_label(stat="count", aes(label=..count..)) +
  theme_bw()
grid.arrange(p1, p2, p3, p4, ncol=2)

# barplot (EdMother/EdFather)
ggplot(train, aes(x=EdMother, fill=FlagAIB)) +
  geom_bar(stat="count", position="fill") +
  facet_grid(.~EdFather) +
  theme_bw()

# barplot (EdFather/NumDevice)
ggplot(train, aes(x=NumDevice, fill=EdFather)) +
  geom_bar(stat="count", position="fill") +
  theme_bw()

# correlation plot
corr <- train.x.bin %>% select(-starts_with("Region")) %>% cor()
p1 <- ggcorrplot(corr, type="lower", outline.col="white", lab=TRUE)
p2 <- ggcorrplot(corr, type="lower", outline.col="white",
                 insig="blank", method="circle")
grid.arrange(p1, p2, ncol=2)

# density (InMotif)
p1 <- ggplot(yx_train, aes(x=InMotif_1, fill=FlagAIB)) +
  geom_density(alpha=0.5) +
  theme_bw()
p2 <- ggplot(yx_train, aes(x=InMotif_2, fill=FlagAIB)) +
  geom_density(alpha=0.5) +
  theme_bw()
p3 <- ggplot(yx_train, aes(x=InMotif_3, fill=FlagAIB)) +
  geom_density(alpha=0.5) +
  theme_bw()
grid.arrange(p1, p2, p3)

# density (ExMotif)
p1 <- ggplot(yx_train, aes(x=ExMotif_1, fill=FlagAIB)) +
  geom_density(alpha=0.5) +
  theme_bw()
p2 <- ggplot(yx_train, aes(x=ExMotif_2, fill=FlagAIB)) +
  geom_density(alpha=0.5) +
  theme_bw()
p3 <- ggplot(yx_train, aes(x=ExMotif_3, fill=FlagAIB)) +
  geom_density(alpha=0.5) +
  theme_bw()
grid.arrange(p1, p2, p3)
