#### (2021 - Data Visualization - Himalayan database - Plot 3) #### 
#### ----------------------------------------- ####

#### set up ####
#### Libraries ####
library(readr)
library(here)
library(tidyverse)
library(ggtext)
library(systemfonts)

#### Directory ####
# create directory to save all progress plots
dir.create(here("images", "progress", "imgs_2021_himalaya_P3"), recursive = TRUE, mode = "0755")

#### Read data ####
expeditions <- read_csv("./himalayan_expeditions/ddbb_2021_himalaya/expeditions.csv")

#### Data wrangling ####
Expeditions <- expeditions %>% 
               filter(peak_name == "Everest") %>%
               mutate(termination_reason = case_when(termination_reason == "Success (main peak)" | termination_reason == "Success (claimed)" ~ "Successful",
                                                     termination_reason == "Success (subpeak)" ~ "Not Successful",
                                                     termination_reason == "Bad weather (storms, high winds)" | termination_reason == "Bad conditions (deep snow, avalanching, falling ice, or rock)" ~ "Not Successful", 
                                                     termination_reason == "Lack (or loss) of supplies or equipment" | termination_reason == "Lack of time" ~ "Not Successful",
                                                     termination_reason == "Route technically too difficult, lack of experience, strength, or motivation" ~ "Not Successful",
                                                     termination_reason == "Illness, AMS, exhaustion, or frostbite" | termination_reason == "Accident (death or serious injury)" ~ "Accident (death or serious injury), or Illness",
                                                     termination_reason == "Unknown" | termination_reason == "Did not attempt climb" | termination_reason == "Did not reach base camp" | termination_reason == "Attempt rumoured" | termination_reason == "Other" ~ "Not Successful"),
                      termination_reason = factor(termination_reason, levels = c("Successful","Not Successful","Accident (death or serious injury), or Illness")))

#### Plot Palette ####
Background <- c("#0A1C40")
Title <- c("#D5E2F2")
Text <- c("#D5E2F2")
Caption <- c("grey60")
Palette <- c("Successful" = "#118C8C",
             "Not Successful" = "#FFF1CE",
             "Accident (death or serious injury), or Illness" = "#D64700")

### Annotation ###
text_1 <- "Mount Everest is the world's highest mountain with 8.848 m above sea level. <br> 
           It is situated in the Mahalangur section of the Himalayas. The mountain's <br> 
           summit straddles the borderline between China and Nepal. <br>
           The more than two thousand expeditions have been classified according to <br>
           three criteria: <br>
           - <span style='color:#118C8C'>**Successful**</span> if they reach the highest point,<br>
           - <span style='color:#FFF1CE'>**Not Successful**</span> termination if they didn't,<br>
           - or forced termination by <span style='color:#D64700'>**Accident (death or serious injury), or Illness**</span>.<br>"

text_2 <- "<span style='text-center'> 
           **Mount Everest milestones**: <br>
           - (**May 29, 1953**) Edmund Hillary and Tenzing Norgay <br>
             reached the Mount Everest summit for first time. <br>
           - (**May 16, 1975**) Junko Tabei of Japan becomes <br>
              the first woman to summit Everest. <br>
           - (**August 20, 1980**) Reinhold Messner is the first <br> 
              person to reach the summit solo.</span>"  

text_3 <- "**Mount Everest tragedies**:<br>
           In **2014** an avalanche killed sixteen sherpa and forced the sherpas to <br>
           announce they would not work on the 2014 season with respect for the victims. <br>
           In **2015**, an earthquake (7.8Mw) triggered an avalanche that killed nineteen <br>
           climbers surpassing 2014 as the deadliest disaster at the mountain."

#### Plot ####
Expeditions %>% group_by(year) %>%
                arrange(termination_reason) %>%
                mutate(number = row_number()) %>%
                ggplot(aes(x = year, y = number, color = termination_reason)) +
                geom_point(size = 1.5) +
                scale_x_continuous(limits = c(1921,2020), breaks = c(1921, seq(1930,2000,10), seq(2000,2020,5)), expand = c(0.01, 0.01)) +
                scale_color_manual(values = Palette) +
                annotate("richtext", x = 1922, y = 86, label = text_1, hjust = 0, color = Title, size = 4, fill = NA, label.color = NA, lineheight = unit(1.3, "line")) +
                annotate("richtext", x = 1940, y = 38, label = text_2, hjust = 0, color = Title, size = 4, fill = NA, label.color = NA, lineheight = unit(1.3, "line")) +
                annotate("richtext", x = 1970, y = 86, label = text_3, hjust = 0, color = Title, size = 4, fill = NA, label.color = NA, lineheight = unit(1.3, "line")) +
                ### Theme ###
                theme_void() +
                theme(
                # Text
                text = element_text(color = Text, size = 12),
                axis.text.x = element_text(face = "bold", color = Text, size = 12, hjust = 0.5, vjust = 0.5, angle = 0),
                axis.text.y = element_text(face = "bold", color = Text, size = 12, hjust = 0.5, vjust = 0.5, angle = 0),
                # Panel
                panel.background = element_rect(fill = "transparent", color = NA),
                panel.spacing.x = unit(3, "lines"),
                panel.grid.major.y = element_line(color = "#1f57c8", size = .1),
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
                labs(title = "<span style='font-family: \"Font Awesome 5 Free Solid\"'>&#xf6fc;</span> One hundred years of Everest expeditions",
                     caption = "<br>
                                <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                                <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                                Source: The Himalayan Database (Spring 2020)") 

#### Progress ####
ggsave(here("images", "progress", "imgs_2021_himalaya_P3", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 300, scale = 1, width = 18, height = 9, type = 'cairo')
