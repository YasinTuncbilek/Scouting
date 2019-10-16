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
passes <- read_excel("Passes.xlsx")
actions <- read_excel("Actions.xlsx")
dribbels <- read_excel("Dribbels.xlsx")
packing <- read_excel("Key_passes.xlsx")
defensive <- read_excel("Defensive.xlsx")

# Highlight data of Finn Ole Becker
passes_becker <- subset(passes$`Passes per 90`, passes$Player == "F. Becker")
actions_becker <- subset(actions$`Succ. att. actions`, actions$Player == "F. Becker")
dribbels_becker <- subset(dribbels$`Dribbles per 90`, dribbels$Player == "F. Becker")
forward_passes_becker <- subset(all_passes$ratio_forward, all_passes$Player == "F. Becker")
smart_passes_becker <- subset(packing$`Smt passes per 90`, packing$Player == "F. Becker")
through_passes_becker <- subset(packing$`Thru passes per 90`, packing$Player == "F. Becker")
progressive_passes_becker <- subset(packing$`Progressive passes per 90`, packing$Player == "F. Becker")
finalthird_passes_becker <- subset(packing$`Final 3rd passes per 90`, packing$Player == "F. Becker")
key_passes_becker <- subset(packing$`Key passes per 90`, packing$Player == "F. Becker")
deepcompleted_passes_becker <- subset(packing$`Deep completed passes per 90`, packing$Player == "F. Becker")
duels_becker <- subset(defensive$`Def duels per 90`, defensive$Player == "F. Becker")
succesfull_duels_becker <- subset(defensive$`Succ. def. per 90`, defensive$Player == "F. Becker")

# Create plot passes
plot_passes <- ggplot(data = passes, aes(x = factor(0), y = passes$`Passes per 90`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#929292", colour = "white", size = 12) +
  ggtitle("Passes per 90 minuten", 
  subtitle = "Ten opzichte van spelers spelend op een vergelijkbare positie.") +
  labs(caption = "Data is tot en met de negende speelronde van het seizoen 2019/2020 in de 2. Bundesliga. Bron: Wyscout.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 25, vjust = 3, colour = "black", face = "bold"),
        plot.caption = element_text(vjust = -6, face = "italic"),
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

# Create plot ations
plot_actions <- ggplot(data = actions, aes(x = factor(0), y = actions$`Succ. att. actions`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#929292", colour = "white", size = 12) +
  ggtitle("Succesvolle aanvallende acties per 90 minuten", 
          subtitle = "Ten opzichte van spelers spelend op een vergelijkbare positie.") +
  labs(caption = "Data is tot en met de negende speelronde van het seizoen 2019/2020 in de 2. Bundesliga. Bron: Wyscout.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 25, vjust = 3, colour = "black", face = "bold"),
        plot.caption = element_text(vjust = -6, face = "italic"),
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
  scale_y_continuous(limits = c(0, 7), breaks = c(0, 1, 2, 3, 4, 5, 6, 7)) +
  geom_point(aes(y = actions_becker), shape = 21, colour = "white", fill = "#62150F", size = 14) +
  coord_flip() +
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 12, height = 8, dpi = 300)

# Create plot dribbels
plot_dribbels <- ggplot(data = dribbels, aes(x = factor(0), y = dribbels$`Dribbles per 90`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#929292", colour = "white", size = 12) +
  ggtitle("Dribbels per 90 minuten", 
          subtitle = "Ten opzichte van spelers spelend op een vergelijkbare positie.") +
  labs(caption = "Data is tot en met de negende speelronde van het seizoen 2019/2020 in de 2. Bundesliga. Bron: Wyscout.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 25, vjust = 3, colour = "black", face = "bold"),
        plot.caption = element_text(vjust = -6, face = "italic"),
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
  scale_y_continuous(limits = c(0, 9), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  geom_point(aes(y = dribbels_becker), shape = 21, colour = "white", fill = "#62150F", size = 14) +
  coord_flip() +
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 12, height = 8, dpi = 300)

# Load data of all passes
all_passes <- read_excel("All_passes.xlsx")

# Calculate ratio forward passes / all passes (%)
all_passes$passes <- (all_passes$`Fwd passes per 90` + all_passes$`Lat passes per 90` + all_passes$`Back passes per 90`)
all_passes$ratio_forward <- (all_passes$`Fwd passes per 90` / all_passes$passes)*100
all_passes$ratio_latteral <- (all_passes$`Lat passes per 90` / all_passes$passes)*100
all_passes$ratio_backward <- (all_passes$`Back passes per 90`/ all_passes$passes)*100 

# Create plot percentage forward passes
plot_forward_passes <- ggplot(data = all_passes, aes(x = factor(0), y = all_passes$ratio_forward)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#929292", colour = "white", size = 12) +
  ggtitle("Percentage voorwaartse passes per 90 minuten", 
          subtitle = "Ten opzichte van spelers spelend op een vergelijkbare positie.") +
  labs(caption = "Data is tot en met de negende speelronde van het seizoen 2019/2020 in de 2. Bundesliga. Bron: Wyscout.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, vjust = 3, colour = "black", face = "bold"),
        plot.caption = element_text(vjust = -6, face = "italic"),
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
  scale_y_continuous(limits = c(20, 60), breaks = c(20, 30, 40, 50, 60)) +
  geom_point(aes(y = forward_passes_becker), shape = 21, colour = "white", fill = "#62150F", size = 14) +
  coord_flip() +
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 12, height = 8, dpi = 300)

plot_forward_passes

# Create plot smart passes
plot_smart_passes <- ggplot(data = packing, aes(x = factor(0), y = packing$`Smt passes per 90`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#929292", colour = "white", size = 12) +
  ggtitle("'Slimme' passes per 90 minuten", 
          subtitle = "Ten opzichte van spelers spelend op een vergelijkbare positie.") +
  labs(caption = "Data is tot en met de negende speelronde van het seizoen 2019/2020 in de 2. Bundesliga. Bron: Wyscout.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, vjust = 3, colour = "black", face = "bold"),
        plot.caption = element_text(vjust = -6, face = "italic"),
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
  scale_y_continuous(limits = c(0, 3.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)) +
  geom_point(aes(y = smart_passes_becker), shape = 21, colour = "white", fill = "#62150F", size = 14) +
  coord_flip() +
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 12, height = 8, dpi = 300)

plot_smart_passes

# Create plot through passes
plot_through_passes <- ggplot(data = packing, aes(x = factor(0), y = packing$`Thru passes per 90`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#929292", colour = "white", size = 12) +
  ggtitle("Steekpasses per 90 minuten", 
          subtitle = "Ten opzichte van spelers spelend op een vergelijkbare positie.") +
  labs(caption = "Data is tot en met de negende speelronde van het seizoen 2019/2020 in de 2. Bundesliga. Bron: Wyscout.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, vjust = 3, colour = "black", face = "bold"),
        plot.caption = element_text(vjust = -6, face = "italic"),
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
  scale_y_continuous(limits = c(0, 2), breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)) +
  geom_point(aes(y = through_passes_becker), shape = 21, colour = "white", fill = "#62150F", size = 14) +
  coord_flip() +
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 12, height = 8, dpi = 300)

plot_through_passes

# Create plot progressive passes
plot_progressive_passes <- ggplot(data = packing, aes(x = factor(0), y = packing$`Progressive passes per 90`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#929292", colour = "white", size = 12) +
  ggtitle("Progressieve passes per 90 minuten", 
          subtitle = "Ten opzichte van spelers spelend op een vergelijkbare positie.") +
  labs(caption = "Data is tot en met de negende speelronde van het seizoen 2019/2020 in de 2. Bundesliga. Bron: Wyscout.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, vjust = 3, colour = "black", face = "bold"),
        plot.caption = element_text(vjust = -6, face = "italic"),
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
  scale_y_continuous(limits = c(1, 11), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)) +
  geom_point(aes(y = progressive_passes_becker), shape = 21, colour = "white", fill = "#62150F", size = 14) +
  coord_flip() +
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 12, height = 8, dpi = 300)

plot_progressive_passes

# Combine plots
plot_packing <- ggarrange(plot_forward_passes, plot_smart_passes, plot_through_passes, plot_progressive_passes,
                  ncol = 2, nrow = 2)

plot_packing

# Create plot final third passes
plot_finalthird_passes <- ggplot(data = packing, aes(x = factor(0), y = packing$`Final 3rd passes per 90`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#929292", colour = "white", size = 12) +
  ggtitle("Final third passes per 90 minuten", 
          subtitle = "Ten opzichte van spelers spelend op een vergelijkbare positie.") +
  labs(caption = "Data is tot en met de negende speelronde van het seizoen 2019/2020 in de 2. Bundesliga. Bron: Wyscout.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, vjust = 3, colour = "black", face = "bold"),
        plot.caption = element_text(vjust = -6, face = "italic"),
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
  scale_y_continuous(limits = c(1, 13), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)) +
  geom_point(aes(y = finalthird_passes_becker), shape = 21, colour = "white", fill = "#62150F", size = 14) +
  coord_flip() +
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 12, height = 8, dpi = 300)

plot_finalthird_passes

# Create plot deep completed passes
plot_deepcompleted_passes <- ggplot(data = packing, aes(x = factor(0), y = packing$`Deep completed passes per 90`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#929292", colour = "white", size = 12) +
  ggtitle("Deep completed passes per 90 minuten", 
          subtitle = "Ten opzichte van spelers spelend op een vergelijkbare positie.") +
  labs(caption = "Data is tot en met de negende speelronde van het seizoen 2019/2020 in de 2. Bundesliga. Bron: Wyscout.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, vjust = 3, colour = "black", face = "bold"),
        plot.caption = element_text(vjust = -6, face = "italic"),
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
  scale_y_continuous(limits = c(0, 3.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)) +
  geom_point(aes(y = deepcompleted_passes_becker), shape = 21, colour = "white", fill = "#62150F", size = 14) +
  coord_flip() +
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 12, height = 8, dpi = 300)

plot_deepcompleted_passes

# Create plot key passes
plot_key_passes <- ggplot(data = packing, aes(x = factor(0), y = packing$`Key passes per 90`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#929292", colour = "white", size = 12) +
  ggtitle("Key passes per 90 minuten", 
          subtitle = "Ten opzichte van spelers spelend op een vergelijkbare positie.") +
  labs(caption = "Data is tot en met de negende speelronde van het seizoen 2019/2020 in de 2. Bundesliga. Bron: Wyscout.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, vjust = 3, colour = "black", face = "bold"),
        plot.caption = element_text(vjust = -6, face = "italic"),
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
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  geom_point(aes(y = key_passes_becker), shape = 21, colour = "white", fill = "#62150F", size = 14) +
  coord_flip() +
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 12, height = 8, dpi = 300)

plot_key_passes

# Combine plots
plot_finalthird <- ggarrange(plot_deepcompleted_passes, plot_finalthird_passes,
                          ncol = 1, nrow = 2)

plot_finalthird

# Create plot defensieve duels
plot_defensive_duels <- ggplot(data = defensive, aes(x = factor(0), y = defensive$`Def duels per 90`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#929292", colour = "white", size = 12) +
  ggtitle("Defensieve duels per 90 minuten", 
          subtitle = "Ten opzichte van spelers spelend op een vergelijkbare positie.") +
  labs(caption = "Data is tot en met de negende speelronde van het seizoen 2019/2020 in de 2. Bundesliga. Bron: Wyscout.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, vjust = 3, colour = "black", face = "bold"),
        plot.caption = element_text(vjust = -6, face = "italic"),
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
  scale_y_continuous(limits = c(1, 22), breaks = c(1, 4, 7, 10, 13, 16, 19, 22)) +
  geom_point(aes(y = duels_becker), shape = 21, colour = "white", fill = "#62150F", size = 14) +
  coord_flip() +
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 12, height = 8, dpi = 300)

plot_defensive_duels

# Create plot succesvolle defensieve duels
plot_sucdefensive_duels <- ggplot(data = defensive, aes(x = factor(0), y = defensive$`Succ. def. per 90`)) +
  geom_quasirandom(shape = 21, varwidth = TRUE, fill = "#929292", colour = "white", size = 12) +
  ggtitle("Succesvolle defensieve duels per 90 minuten", 
          subtitle = "Ten opzichte van spelers spelend op een vergelijkbare positie.") +
  labs(caption = "Data is tot en met de negende speelronde van het seizoen 2019/2020 in de 2. Bundesliga. Bron: Wyscout.") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20, vjust = 3, colour = "black", face = "bold"),
        plot.caption = element_text(vjust = -6, face = "italic"),
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
  scale_y_continuous(limits = c(1, 17), breaks = c(1, 5, 9, 13, 17)) +
  geom_point(aes(y = succesfull_duels_becker), shape = 21, colour = "white", fill = "#62150F", size = 14) +
  coord_flip() +
  ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
         width = 12, height = 8, dpi = 300)
plot_sucdefensive_duels


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





                