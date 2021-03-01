#### (2021 - Data Visualization - Himalayan database) #### 
#### ----------------------------------------- ####

#### set up ####

#### Libraries ####
library(readr)
library(here)
library(tidyverse)
library(grid)
library(ggtext)
library(systemfonts)

#### Directory ####
# create directory to save all progress plots
dir.create(here("images", "progress", "imgs_2021_himalaya"), recursive = TRUE, mode = "0755")

#### Read data ####
expeditions <- read_csv("./himalayan_expeditions/ddbb_2021_himalaya/expeditions.csv")
members <- read_csv("./himalayan_expeditions/ddbb_2021_himalaya/members.csv")
peaks <- read_csv("./himalayan_expeditions/ddbb_2021_himalaya/peaks.csv")

#### Data wrangling ####
Expeditions <- expeditions %>% group_by(peak_name, highpoint_meters) %>% 
                               count() %>% 
                               filter(n >= 1 & !is.na(highpoint_meters)) %>%
                               left_join(peaks, by = c("peak_name" = "peak_name")) %>% 
                               filter(str_detect(Location, "Dhaulagiri Himal|Annapurna Himal|Manaslu Himal|Langtang Himal|Jugal Himal|Rolwaling Himal|Khumbu Himal")) %>%
                               separate(Location,c("Himal", NA)) %>%
                               group_by(peak_name) %>%
                               mutate(attempts = sum(n),
                                      Himal = factor(Himal, levels=c("Dhaulagiri", "Annapurna", "Manaslu", "Langtang", "Jugal", "Rolwaling", "Khumbu"))) %>%
                               filter(attempts >= 3) %>%
                               arrange(Himal) %>%
                               mutate(peak_name = factor(peak_name, levels=unique(peak_name))) %>%
                               ungroup()

#### Plot background ####
g <- rasterGrob(colorRampPalette(c("#606F84","#6E86A6","#8FA5C2"), alpha = TRUE)(5), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

#### Plot ####
ggplot(Expeditions, aes(peak_name, highpoint_meters, size=n)) + 
  # Layers 
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_line(aes(peak_name, height_meters, group=1), color = "#5C606A", size =0.3, linejoin = "round", lineend = "round", alpha=1) +
  geom_jitter(data = Expeditions[which(Expeditions$highpoint_meters != Expeditions$height_meters & Expeditions$highpoint_meters < 8000),], aes(peak_name, highpoint_meters, color="< 8000", width = 0.3/(highpoint_meters/height_meters)), height = 0) +
  geom_jitter(data = Expeditions[which(Expeditions$highpoint_meters != Expeditions$height_meters & Expeditions$highpoint_meters >= 8000),], aes(peak_name, highpoint_meters, color=">= 8000", width = 0.2/(highpoint_meters/height_meters)), height = 0) +
  geom_point(data = Expeditions[which(Expeditions$highpoint_meters == Expeditions$height_meters),], aes(peak_name, highpoint_meters, color="Peak")) +
  geom_point(data = Expeditions[which(Expeditions$highpoint_meters == Expeditions$height_meters & Expeditions$height_meters >= 8000),], aes(peak_name, highpoint_meters), color="black", shape=21, stroke = 1) +
  # Annotations
  annotate(geom = "point", x = "Dhaulagiri VI", y = 10000, fill="#FFFF00", color= "black", shape=21, stroke = 1, size=4) +
  annotate(geom='richtext', x= "Tukuche", y= 10000, fill = NA, label.color = NA, label="<span style='color:white'>Peaks over 8000</span>", family= "Chalkduster") +
  annotate(geom = "curve", x = "Lunag Ri", y = 2000, xend = "Everest", yend =5290, curvature = .3, arrow = arrow(length = unit(3, "mm")), color= "#8c0000", lineend = "butt", linejoin = "round") +
  annotate(geom='richtext', x= "Chobuje", y= 2000, fill = NA, label.color = NA, label="<span style='color:white'>The pressure of oxygen at 5500 m (Everest base camp)<br> is approximately 50% of its value at sea level, <br> while at 8900 m (Everest summit) it is 30%</span>", family= "Chalkduster") +
  annotate(geom = "curve", x = "Kwangde", y = 10000, xend = "Everest", yend =9000, curvature = -.2, arrow = arrow(length = unit(3, "mm")), color= "#8c0000", lineend = "butt", linejoin = "round") +
  annotate(geom='richtext', x= "Chobuje", y= 10000, fill = NA, label.color = NA, label="<span style='color:white'>A quarter of all Himalayan expeditions <br> are guided to Everest</span>", family= "Chalkduster") +
  # Scales
  scale_y_continuous(breaks = seq(0, 8000, 2000), limits =(c(0, 10000))) +
  scale_x_discrete(limits = factor(unique(Expeditions$peak_name))) +
  scale_size_continuous(range = c(.5, 4), trans ="log10") +
  scale_color_manual(values = c("#DE9E48","#FAFAFA","#FFFF00"), labels=c("< 8000",">= 8000","Peak")) +
  # Guides
  guides(size = guide_legend(title.position = "top", override.aes = list(color = "#FFFF00"), order=1)) +
  guides(color = guide_legend(title.position = "top", override.aes = list(size = 4)), order=2) +
  # Theme
  theme(# Text
    text=element_text(family="Founders Grotesk",colour = "white", size = 12),
    # Axis
    axis.title.x = element_text(face = "plain", color = "white", size = 14, hjust = 0.5, vjust = 0.0, angle = 0),
    axis.title.y = element_text(face = "plain", color = "white", size = 14, hjust = 0.5, vjust = 3.0, angle = 90),
    axis.text.x = element_text(face = "plain", color = "white", size = 12, hjust = 1, vjust = 0.0, angle = 90),
    axis.text.y = element_text(face = "plain", color = "white", size = 12, hjust = 0.5, vjust = 0.5, angle = 0),
    axis.line.x = element_line(colour = "#606F84"),
    axis.ticks.x = element_line(colour = "#606F84"),
    axis.line.y = element_line(colour = "#606F84"),
    axis.ticks.y = element_line(colour = "white"),
    # Panel Grid
    panel.grid.major = element_line(color = NA),
    panel.grid.major.y = element_line(color = NA),
    panel.grid.minor = element_line(color = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.border = element_blank(),
    # Plot Aesthetic
    plot.background = element_rect(fill = "#606F84", color = NA),
    plot.caption = element_markdown(color = "white", family="Menlo", hjust = 1, vjust = -55),
    # Legend
    legend.title = element_markdown(face = "bold", colour = "white", size = 14, hjust = 0.5),
    legend.text = element_text(color = "white", size = 12),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.spacing = unit(3.0, 'cm'),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent", colour = NA),
    legend.justification = c("right", "center"),
    # Titles
    plot.title=element_markdown(face = "bold", color = "white", family = "Founders Grotesk", size = 18, hjust = 0),
    plot.subtitle=element_text(face = "bold", color = "white", family = "Founders Grotesk", size = 16, angle = 0),
    # Margin
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
  # Labels
  labs(title = "<span style='font-family: \"Font Awesome 5 Free Solid\"'>&#xf6fc;</span> Himalayan Peaks",
       subtitle = "Number of expeditions by peak highpoint",
       x = NULL,
       y = "Height Meters",
       size = "<span style='font-family: \"Font Awesome 5 Free Solid\"'>&#xf6ec;</span> Expeditions",
       color = "<span style='font-family: \"Font Awesome 5 Free Solid\"'>&#xf091;</span> Achievement", 
       caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                  <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                  Source: The Himalayan Database (Spring 2020)") 

#### Progress ####
ggsave(here("images", "progress", "imgs_2021_himalaya", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 300, scale = 1, width = 19.2, height = 9, type = 'cairo')
