#### (2021 - Data Visualization - Himalayan database - Plot 6) #### 
#### ----------------------------------------- ####

#### set up ####
#### Libraries ####
library(readr)
library(here)
library(tidyverse)
library(ggtext)
library(systemfonts)
library(BBmisc)
library(ggbump)

#### Directory ####
# create directory to save all progress plots
dir.create(here("images", "progress", "imgs_2021_himalaya_P6"), recursive = TRUE, mode = "0755")

#### Read data ####
peaks <- read_csv("./himalayan_expeditions/ddbb_2021_himalaya/peaks.csv")
members <- read_csv("./himalayan_expeditions/ddbb_2021_himalaya/members.csv")

#### Data wrangling ####
peaks <- peaks %>% 
         filter(peak_name %in% c("Everest", "Kangchenjunga", "Lhotse", "Makalu", "Cho Oyu", "Dhaulagiri I", "Manaslu", "Annapurna I")) %>%       # filter peaks by Eight-thousanders Himalayan mountains name
         select(peak_name, height_meters, first_ascent_year) %>% 
         arrange(desc(height_meters))

Members <- members %>% 
           filter(peak_name %in% factor(peaks$peak_name) & hired == FALSE) %>%
           group_by(peak_name) %>% 
           add_count(sex, name = "sex_number") %>%
           distinct(peak_name, sex, sex_number) %>% 
           left_join(peaks, by = "peak_name") %>%
           group_by(peak_name) %>%
           mutate(prop = sex_number / sum(sex_number),
                  perc = round(prop * 100),
                  sex = case_when(sex == "M" ~ "Men",
                                  sex == "F" ~ "Women"),
                  height_label = case_when(height_meters == "8163" | height_meters == "8167" ~ "8165",
                                           TRUE ~ as.character(height_meters)),
                  height_label = glue::glue("{height_label}m")) %>% 
           arrange(prop)

Members_Women <- Members %>% 
                 filter(sex == "Women")

ranking <- Members_Women %>%
           bind_cols(tibble(ranking = normalize(rank(Members_Women$prop), range = c(8150, 8650), method = "range"),            # Create possition ranking by female prop
                            Peak = Members_Women$peak_name,
                            xend = 2020,                                                                                       # Where segment lines should finish
                            x_axis_start = xend - 30,                                                                          # Where segment lines should begin
                            possition_x = first(x_axis_start) + Members_Women$prop * (2020-first(x_axis_start)),               # Adjust x axis position, to scale it to the prop 
                            number_txt = paste0(format(Members_Women$perc, digits = 0, nsmall = 0), "%"),
                            number_txt2 = if_else(Peak == "Manaslu", paste0(number_txt, " Women Mountaineers"), number_txt),
                            Peak_txt = if_else(Peak == "Manaslu" | Peak == "Dhaulagiri I", paste0(Peak, "*"), Peak))) %>% 
             arrange(desc(prop))

#### Plot Palette ####
Background <- c("#F2EEB3") 
Everest <- c("#FDF0D5") 
Title <- c("#3EA8A6")
Text <- c("#3EA8A6")
Caption <- c("#3EA8A6")
Palette <- viridis::plasma(begin = .2, end = .6, direction = 1, n=8)

#### Plot ####
ggplot() + geom_point(data = Members_Women, aes(x=first_ascent_year, y=height_meters, color=peak_name), size = 4) +
           geom_sigmoid(data = ranking, aes(x=first_ascent_year, y=height_meters, xend = x_axis_start - .2, yend = ranking, group = Peak, color = peak_name), alpha = .6, smooth = 10, size = 1) +   # Connecting line between       
           geom_segment(data = ranking, aes(x = x_axis_start, y = ranking, xend = 2020, yend = ranking, color = peak_name), alpha = .2, size = 1, lineend = "round") +                               # Line/bar from xstart to 100%
           geom_segment(data = ranking, aes(x = x_axis_start, y = ranking, xend = possition_x, yend = ranking, color = peak_name), alpha = .6, size = 1, lineend = "round") +                        # Line/bar from xstart to value
           geom_text(data = ranking, aes(x = x_axis_start, y = ranking, label = Peak_txt, color = peak_name), hjust = 1, size = 3.0, nudge_x = -1, nudge_y = -15) +                                  # Peak text
           geom_text(data = ranking, aes(x = possition_x, y = ranking, label = number_txt2, color = peak_name), hjust = 0, size = 3, nudge_x = .8, nudge_y = 15) +                                   # Mountaniers number text
           scale_x_continuous(limits = c(1950, 2020), breaks = Members$first_ascent_year) +
           scale_y_continuous(limits = c(8050, 8850), breaks = c(8091,8165,8188,8485,8516,8586,8850), labels = unique(sort(Members$height_label))) +
           scale_color_manual(values = Palette, breaks = ranking$peak_name) +
           scale_fill_manual(values = Palette, breaks = ranking$peak_name) + 
           ### Theme ###
           theme_void() +
           theme(
             ## Text ##
             text = element_text(color = Text, family="Roboto", size = 12),
             ## Panel ##
             panel.background = element_rect(fill = "transparent", color = NA),
             axis.text.x = element_text(face = "plain", color = "#7259A0",  size = 12, hjust = 0, angle = 90),
             axis.text.y = element_text(face = "plain", color = "#D67C17",  size = 12, hjust = 0, angle = 0),
             ## Plot Aesthetic ##
             plot.background = element_rect(fill = Background, color = NA),
             plot.caption.position = "plot",
             plot.caption = element_markdown(color = Caption, family="Menlo", hjust = 1, vjust = 0),
             plot.tag = element_markdown(face = "plain", family ="Roboto", colour = Title, size = 12),
             plot.tag.position = c(0.18, 0.02),
             ## Legend ##
             legend.position = "none",
             ## Titles ##
             plot.title.position = "plot",
             plot.title = element_markdown(face = "plain", color = Title, family = "Ubuntu Condensed", size = 18, hjust = 0),
             plot.subtitle = element_markdown(face = "plain", color = Title, family = "Ubuntu Condensed", size = 16, angle = 0),
             ## Margin ##
             plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
           ## Labels ##
           labs(title = "Eight-thousanders Himalayan Mountains by -non-hired- Mountaineers Sex",
             subtitle = "Himalayan -non-hired- mountaineers are grouped by peaks and ranked accordingly with their female mountaineers' percentage. <br>
                         Their starting points to the Eight-thousanders mountains 
                         <span style='color:#D67C17'>height in meters</span> 
                         and the year of the 
                         <span style='color:#7259A0'>first ascension</span>.",
                  tag = "(*) Note: Manaslu & Dhaulagiri I official heights are 8163m and 8167m, respectively.",
              caption = "<br>
                         <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                         <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;&nbsp;</span>TamayoLeivaJ<br><br> 
                         Source: The Himalayan Database (Spring 2020)")
           
#### Progress ####
ggsave(here("images", "progress", "imgs_2021_himalaya_P6", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 300, scale = 1, width = 18, height = 9, type = 'cairo')
