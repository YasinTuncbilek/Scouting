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
library(here)
library(magrittr)
library(grid)
library(magick)

# Get working directory
getwd()

# Load data
data <- read_excel("Dribbles.xlsx")
data2 <- read_excel("Through_passes.xlsx")
passes <- read_excel("Passes.xlsx")



# Highlight data of Finn Ole Becker
passes_becker <- subset(passes$`Passes per 90`, passes$Player == "F. Becker")

# Create plot
plot_passes <- ggplot(data = passes, aes(x = factor(0), y = passes$`Passes per 90`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#093F86", colour = "white", size = 12) +
  ggtitle("Passes per 90 minuten", subtitle = "Op basis van negen speelrondes in het seizoen 2019/2020 in de 2. Bundesliga.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 25, vjust = 3, colour = "black", face = "bold"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.x = element_line(colour = "black", size = 2)) +
  stat_summary(fun.y = median,
               fun.ymin = median,
               fun.ymax = median,
               geom = "crossbar", 
               colour = "black",
               width = 1.2,
               size = 1.3) +
  scale_y_continuous(limits = c(15, 65), breaks = c(20, 30, 40, 50, 60)) +
  geom_point(aes(y = passes_becker), shape = 21, colour = "white", fill = "#62150F", size = 14) +
  coord_flip() +
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 12, height = 8, dpi = 300)

# Upload PSV logo
logo_raw <- image_read("Logo FoForTho.png")
logo <- logo_raw %>%
  image_scale("100") %>% 
  image_background("grey", flatten = TRUE) %>%
  image_border("grey", "600x10") %>%
  image_annotate("Gemaakt in R; FoForTho", color = "white", size = 20, 
                 location = "+10+50", gravity = "northeast")

# Stack them on top of each other
final_plot <- image_append(image_scale(c(plot, logo), "2000"), stack = TRUE)

# And overwrite the plot without a logo
image_write(final_plot, paste0(here("/"), last_plot()$labels$title, ".png"))






+
  geom_label_repel(data = passes %>% filter(Player == "F. Becker"), 
                  aes(y = `Passes per 90`),
                  label = "Player X",
                  vjust = 0.25,
                  box.padding   = 0.35, 
                  point.padding = 1,
                  size = 3,
                  segment.color = 'grey50',
                  segment.curvature = 1,
                  arrow = arrow(length = unit(0.015, "npc"), type = "closed", ends = "first"),
                  force = 5) 



test <- ggarrange(dribbles, dribbles, dribbles, dribbles, dribbles, dribbles,
                    ncol = 3, nrow = 2)
test

annotate_figure(test,
                top = text_grob("Template: central midfielder", color = "#333333", face = "bold", size = 30, just = "center"))

                