#### (2021 - tidytuesday - Himalayan database - Plot 2) #### 
#### ----------------------------------------- ####

#### set up ####
#### Libraries ####
library(readr)
library(here)
library(tidyverse)
library(ggtext)
library(systemfonts)
library(hrbrthemes)
library(waffle)

#### Directory ####
# create directory to save all progress plots
dir.create(here("images", "progress", "imgs_2021_himalaya_P2"), recursive = TRUE, mode = "0755")

#### Read data ####
expeditions <- read_csv("./himalayan_expeditions/ddbb_2021_himalaya/expeditions.csv")

#### Data wrangling ####
Expeditions_top <- expeditions %>% 
                   count(peak_name) %>%
                   slice_max(n, n=10)

Expeditions_top_hight <- expeditions %>% 
                         group_by(peak_name) %>%
                         slice_max(highpoint_meters, n=1) %>%
                         filter(peak_name %in% Expeditions_top$peak_name) %>%
                         distinct(peak_name,highpoint_meters) %>%
                         arrange(desc(highpoint_meters))

Expeditions <- expeditions %>% 
               mutate(termination_reason = case_when(termination_reason == "Success (main peak)" | termination_reason == "Success (claimed)" ~ "Success",
                                                     termination_reason == "Success (subpeak)" ~ "Subpeak",
                                                     termination_reason == "Bad weather (storms, high winds)" | termination_reason == "Bad conditions (deep snow, avalanching, falling ice, or rock)" ~ "Bad conditions or weather", 
                                                     termination_reason == "Lack (or loss) of supplies or equipment" | termination_reason == "Lack of time" ~ "Lack of supplies, equipment or time",
                                                     termination_reason == "Route technically too difficult, lack of experience, strength, or motivation" ~ "Lack of experience, route too difficult",
                                                     termination_reason == "Illness, AMS, exhaustion, or frostbite" | termination_reason == "Accident (death or serious injury)" ~ "Accident (death or serious injury), or Illness",
                                                     termination_reason == "Unknown" | termination_reason == "Did not attempt climb" | termination_reason == "Did not reach base camp" | termination_reason == "Attempt rumoured" | termination_reason == "Other" ~ "Other")) %>%
               add_count(peak_name, name = "exped", sort = T) %>% 
               filter(exped >= unique(exped)[10]) %>%
               add_count(peak_name, termination_reason, name = "exped_term", sort = T) %>%
               distinct(peak_name, termination_reason, exped, exped_term) %>%
               mutate(peak_name = factor(peak_name, levels = Expeditions_top_hight$peak_name),
                      termination_reason = factor(termination_reason, levels = c("Success","Subpeak","Accident (death or serious injury), or Illness","Bad conditions or weather","Lack of experience, route too difficult","Lack of supplies, equipment or time","Other"))) %>%
               mutate(peak_name = recode(peak_name, "Everest" = "Everest (8850m)",
                                                    "Lhotse" = "Lhotse (8516m)",
                                                    "Makalu" = "Makalu (8485m)",
                                                    "Cho Oyu" = "Cho Oyu (8201m)",
                                                    "Dhaulagiri I" = "Dhaulagiri I (8167m)",
                                                    "Manaslu" = "Manaslu (8163m)",
                                                    "Annapurna I" = "Annapurna I (8163m)",
                                                    "Baruntse" = "Baruntse (7152m)",
                                                    "Pumori" = "Pumori (7138m)",
                                                    "Ama Dablam" = "Ama Dablam (6814m)"))

#### Plot Palette ####
Background <- c("#00010D")
Title <- c("#E9F1F2")
Text <- c("#F2F2F2")
Caption <- c("grey60")
Palette <- c("Success" = "#405F73",
             "Subpeak" = "#4192D9",
             "Bad conditions or weather" = "#D9A362",
             "Lack of supplies, equipment or time" = "#A68B03",
             "Other" = "#03A696", 
             "Accident (death or serious injury), or Illness" = "#A60303",
             "Lack of experience, route too difficult" = "#8C034E")

#### Plot ####
Expeditions %>% ggplot(aes(fill = termination_reason, values = exped_term/10)) +
                geom_waffle(n_rows = 5, size = .8, color = Background, flip = TRUE, scales = "fixed", height = 1, width = 1) +
                scale_fill_manual(values = Palette) +
                scale_y_continuous(breaks = c(10,20,30,40), labels = c(".5k","1k","1.5k","2k")) +
                expand_limits(x=c(0,0), y=c(0,0)) +
                facet_wrap(~ peak_name, nrow = 1, strip.position = "bottom", labeller = label_wrap_gen(width = 13, multi_line = TRUE)) +
                coord_cartesian() +
                ### Theme ###
                theme_void() +
                theme_enhance_waffle() +
                theme(
                # Text
                text=element_text(color = Text, size = 12),
                # Strip
                strip.text = element_text(face = "bold", color = Text, size = 12, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 1, margin = margin(.1, 0, .1, 0, "cm")),
                strip.placement = "outside",
                # Text
                axis.text.x = element_blank(),
                axis.text.y = element_text(face = "bold", color = Text, size = 12, hjust = 0.5, vjust = 0.5, angle = 0),
                # Panel
                panel.background = element_rect(fill = "transparent", color = NA),
                panel.spacing.x=unit(3, "lines"),
                # Plot Aesthetic
                plot.background = element_rect(fill = Background, color = NA),
                plot.caption = element_markdown(color = Caption, family="Menlo", hjust = 1, vjust = 0),
                plot.tag.position = c(0.15, 0.05),
                # Legend
                legend.position = "none",
                # Titles
                plot.title=element_markdown(face = "bold", color = Title, family = "Founders Grotesk", size = 18, hjust = 0),
                plot.subtitle=element_markdown(face = "bold", 
                                               color = Title, 
                                               family = "Founders Grotesk", 
                                               size = 14, angle = 0,
                                               padding = margin(t = 0.5, r = 0, b = 0.5, l = 0, unit = "cm"),
                                               margin = margin(t = 0.5, r = 0.5, b = 0, l = 0, unit = "cm")),
                plot.tag = element_markdown(face = "plain", family ="Roboto", colour = Caption, size = 12),
                # Margin
                plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
                # Labels
                labs(title = "<span style='font-family: \"Font Awesome 5 Free Solid\"'>&#xf6fc;</span> Himalayan Top 10 Peaks",
                     subtitle = "<span> Numbers of successful <b style='color:#405F73;'>main peak</b> or
                                 <b style='color:#4192D9;'>subpeak</b> expeditions to the Himalayan most attempted <br>mountains. 
                                 Other expeditions and their termination reasons are also shown. The most common <br>
                                 termination reasons are related to the route <b style='color:#D9A362;'>Bad conditions or weather</b>,unexpected events, such as <br>
                                 <b style='color:#A60303;'>Accident (death or serious injury), or Illness</b>, the expedition lack of preparation or luck, <b style='color:#A68B03;'>Lack of supplies, <br> 
                                 equipment or time</b>, the members technical skills, <b style='color:#8C034E;'>Lack of experience, route too difficult</b>, or <b style='color:#03A696;'>Other</b>.</span>",
                     x = NULL,
                     y = NULL,
                     size = NULL,
                     fill = NULL,
                     color = NULL, 
                     tag = "Note: Each rectangle represent ten expeditions <span style='font-family: \"Font Awesome 5 Free Solid\"'>&#xf6ec;</span>",
                     caption = "<br>
                                <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                                <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                                Source: The Himalayan Database (Spring 2020)") 

#### Progress ####
ggsave(here("images", "progress", "imgs_2021_himalaya_P2", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 300, scale = 1, width = 18, height = 9, type = 'cairo')
