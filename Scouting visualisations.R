# Install relevant packages
install.packages("beeswarm")
install.packages("ggbeeswarm")


# Load relevant packages
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

# Install ggbeeswarm package
install.packages("ggbeeswarm")

# Load packages
library(ggplot2)
library(ggbeeswarm)
library(readxl)

# Get working directory
getwd()

# Set working directory
setwd("C:/Users/yt/Downloads")

# Load data
data <- read_xlsx("dribbles.xlsx")

# Make seperate point of score of Finn Ole Becker
Becker = data.frame()

# Create plot
ggplot(data = data, aes(x = factor(0), y = data$`Dribbles per 90`)) +
  geom_quasirandom(colour = "#57ceae", size = 22, adjust = 0.2) +
  ggtitle("Dribbles per 90 minutes") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "#333333"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 25, vjust = 3, colour = "#333333", face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.x = element_line(colour = "#333333", size = 2)) +
  stat_summary(fun.y = median,
               fun.ymin = median,
               fun.ymax = median,
               geom = "crossbar", 
               colour = "#333333",
               width = 1.2,
               size = 1.5) +
  scale_y_continuous(limits = c(0, 7)) +
  geom_point(aes(y = 4.94), colour = "#002560", size = 22) +
  coord_flip() 


