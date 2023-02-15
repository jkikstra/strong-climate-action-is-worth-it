#' Make figure for News & Views piece "Climate Action is Worth It"
#' Authors: Jarmo S. Kikstra and Paul Waidelich

try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
library(here)
here::i_am("climate-action-is-worth-it.Rproj")

library(tidyverse)

temps <- read_csv(here("data.csv")) %>% 
  rename(Year=`Publication Year`)

p <- 
  ggplot(mapping=aes(x=Year, y=Value)) +
  annotate(geom = "rect", xmin = 2010, xmax = 2015, ymin = 0, ymax = 2,
           fill = "#00BA38", colour = "white", alpha = 0.2) +
  annotate(geom = "rect", xmin = 2015, xmax = 2025, ymin = 1.5, ymax = 2,
           fill = "#00BA38", colour = "white", alpha = 0.1) +
  annotate(geom = "rect", xmin = 2015, xmax = 2025, ymin = 0, ymax = 1.5,
           fill = "#00BA38", colour = "white", alpha = 0.2) +
  
  geom_segment(aes(x = 2010, xend = 2015, y = 2, yend = 2), colour = "#00BA38", linetype = "dashed") +
  geom_segment(aes(x = 2010, xend = 2010, y = 0, yend = 2), colour = "#00BA38", linetype = "dashed") +
  geom_segment(aes(x = 2015, xend = 2024, y = 2, yend = 2), colour = "#00BA38", linetype = "dashed", alpha = 0.5) +
  geom_segment(aes(x = 2015, xend = 2024, y = 1.5, yend = 1.5), colour = "#00BA38", linetype = "dashed") +
  geom_segment(aes(x = 2015, xend = 2015, y = 0, yend = 2), colour = "#00BA38", linetype = "dashed") +
  
  # arrows - commented out and done manually in PowerPoint/Illustrator
  # annotate("segment", x = 2010, xend = 2011, y = 1.7, yend = 1.7,
  #          colour = "#00BA38", arrow = arrow(
  #            #length = unit(.3,"cm")
  #          ), size = 1) +
  # annotate("segment", x = 2015, xend = 2016, y = 1.2, yend = 1.2,
  #          colour = "#00BA38", arrow = arrow(
  #            #length = unit(.3,"cm")
  #          ), size = 1) +
  
  annotate(geom = "text", 
           x = 2010.2, y = 1.9, label = "CANCUN AGREEMENTS", alpha = 0.9, colour = "#005c1c",
           hjust = 0) +
  annotate(geom = "text", 
           x = 2015.2, y = 1.4, label = "PARIS AGREEMENT", alpha = 0.9, colour = "#005c1c", hjust = 0) +
  
  
  geom_point(
    data = temps %>% filter(Year != 2023 & !str_detect(Study, "Dietz et al\\. 2021")), # exclude new Van der Wijst et al. study, and Dietz et al. 2021 who did not set out to find a 'new' temperature estimate but rather investigated how much realistic climate modules alter DICE-2016 results.
    aes(colour = "Previous studies"),
    size = 1.5
  ) +
  geom_point(
    data = temps %>% filter(Year==2023),
    mapping = aes(colour = paste0("Van der Wijst et al. - ", Model, " model")),
    size = 3
  ) +
  
  ylab("Global warming in 2100") +
  xlab(NULL) +
  
  theme_classic() +
  scale_colour_manual(values = c("black", "#4dbbd5b2", "#e64b35ff", "#3c5488ff")) +
  scale_x_continuous(expand = c(0.05,0,0.05,0), breaks = c(2010,2015,2020)) +
  scale_y_continuous(expand = c(0,0,0.1,0), labels = ~ paste(.x, "\u00B0C")) +
  coord_cartesian(xlim = c(2010,2023), ylim = c(0,3.5)) +
  guides(color = guide_legend(override.aes = list(fill = NA), reverse = T)) +
  labs(
    subtitle = "Economically optimal warming estimates lag behind global climate targets",
    colour = NULL
  ) +
  theme(legend.position = c(0.76, 0.15),
        plot.subtitle = element_text(face = "bold"),
        legend.background = element_rect(fill = alpha("white", 0.5)),
        legend.key = element_blank())

p

ggsave(
  filename = here("fig_1.pdf"), device = cairo_pdf,
  plot = p,
  width = 155,
  height = 130,
  unit = "mm"
)  
ggsave(
  filename = here("fig_1.png"), 
  plot = p,
  width = 155,
  height = 130,
  unit = "mm"
)
