# Install relevant packages
install.packages("beeswarm")
install.packages("ggbeeswarm")
install.packages("readxl")
install.packages("ggpubr")

# Load relevant packages
library(beeswarm)
library(ggplot2)
library(ggbeeswarm)
library(readxl)
library(ggpubr)
library(ggrepel)
library(dplyr)

# Get working directory
getwd()

# Load data
data <- read_excel("Dribbles.xlsx")
data2 <- read_excel("Through_passes.xlsx")
data3 <- read_excel("Passes.xlsx")

# Highlight data of Finn Ole Becker
Becker <- subset(data$`Dribbles per 90`, data$Player == "F. Becker")

# Create plot
ggplot(data = data, aes(x = factor(0), y = data$`Dribbles per 90`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#57ceae", colour = "#002560", size = 12) +
  ggtitle("Dribbels per 90 minuten") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "#333333"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 25, vjust = 3, colour = "#333333", face = "bold"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
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
  scale_y_continuous(limits = c(0, 7), breaks = c(0, 1, 2, 3, 4, 5, 6, 7)) +
  geom_point(aes(y = Becker), shape = 21, colour = "#57ceae", fill = "#002560", size = 12) +
  coord_flip() +
  geom_label_repel(data = data %>% filter(Player == "F. Becker"), 
                  aes(y = `Dribbles per 90`),
                  label = "Player X",
                  vjust = 0.25,
                  box.padding   = 0.35, 
                  point.padding = 5,
                  size =3,
                  segment.color = 'grey50',
                  segment.curvature = 1,
                  arrow = arrow(length = unit(0.015, "npc"), type = "closed", ends = "first"),
                  force = 5) 


test <- ggarrange(dribbles, dribbles, dribbles, dribbles, dribbles, dribbles,
                    ncol = 3, nrow = 2)
test

annotate_figure(test,
                top = text_grob("Template: central midfielder", color = "#333333", face = "bold", size = 30, just = "center"))

                