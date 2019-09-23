# Install and load beeswarm package
install.packages("beeswarm")
install.packages("ggbeeswarm")

library(beeswarm)
library(ggplot2)
library(ggbeeswarm)

# Install and load readxl package
install.packages("readxl")

library(readxl)

# Get working directory
getwd()

# Load data
data <- read_excel("/Users/yasintuncbilek/Desktop/FoForTho - Doeldenkers/Scouting visualisations/Def_duels.xlsx")

# Make beeswarm plot
beeswarm(data$`Succ. def. per 90`, col = "#f00000", pch = 19, method = "swarm", horizontal = TRUE, cex = 1.5, bty = "n",
         main = "Succesvolle verdedigende acties/90")
bxplot(data$`Succ. def. per 90`, add = TRUE)

ggplot(data, aes('Succ. def. per 90', col = "Red")) + 
  geom_beeswarm() +
  stat_summary(aes(ymin = ..y.., max = ..y..), fun.y = 'mean', geom = 'crossbar', color = 'firebrick')

ggplot(data, aes("", `Succ. def. per 90`)) +
  geom_beeswarm() +
  coord_flip()


