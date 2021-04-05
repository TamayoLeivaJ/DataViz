#### (2021 - Data Visualization - Himalayan database - Plot 5) #### 
#### ----------------------------------------- ####

#### set up ####
#### Libraries ####
library(readr)
library(here)
library(tidyverse)
library(ggtext)
library(systemfonts)
library(sf)
library(rnaturalearthdata)
library(BBmisc)

#### Directory ####
# create directory to save all progress plots
dir.create(here("images", "progress", "imgs_2021_himalaya_P5"), recursive = TRUE, mode = "0755")

#### Read data ####
members <- read_csv("./himalayan_expeditions/ddbb_2021_himalaya/members.csv")

#### Data wrangling ####
Members <- members %>% 
           filter(peak_name == "Everest" & !is.na(citizenship) & hired == FALSE) %>%
           add_count(citizenship, name = "citizenship_number") %>%
           distinct(citizenship, citizenship_number) %>%
           arrange(-citizenship_number)

countries <- rnaturalearthdata::countries110 %>% 
             st_as_sf() %>% 
             st_crop(xmin = -120, xmax = 180, ymin = -75, ymax = 75) %>% 
             mutate(citizenship = case_when(admin == "United States of America" ~ "USA",
                                            admin == "South Korea" ~ "S Korea" ,
                                            admin == "South Africa" ~ "S Africa",
                                            admin == "United Kingdom" ~ "UK",
                                            T ~ admin))  %>%
             right_join(Members, by = c("citizenship" = "citizenship")) %>% 
             add_count(region_un,  wt = citizenship_number, name = "continent_number") %>%
             slice_max(citizenship_number, n = 30)

ranking <- st_geometry(countries) %>% 
           st_point_on_surface() %>% 
           st_coordinates() %>% 
           as_tibble() %>% 
           bind_cols(tibble(ranking = normalize(rank(countries$citizenship_number), range = c(-75, 0), method = "range"),
                            country = countries$citizenship,
                            xend = -30,
                            x_axis_start = xend + 10,
                            ranking_x = normalize(countries$citizenship_number, range = c(first(x_axis_start), 70), method = "range"),
                            number_txt = paste0(format(countries$citizenship_number, digits = 0, nsmall = 2)),
                            number_txt2 = if_else(country == "USA", paste0(number_txt, " Mountaniers"), number_txt)))

data_map <- rnaturalearthdata::countries110 %>% 
            st_as_sf() %>% 
            st_crop(xmin = -120, xmax = 180, ymin = -75, ymax = 75) %>%
            filter(!admin %in% c("Antarctica", "Greenland", countries$citizenship))

#### Plot Palette ####
Background <- c("#152238") 
Everest <- c("#FDF0D5") 
Title <- c("#669BBC")
Text <- c("#F2F2F2")
Caption <- c("grey60")
Palette <- viridis::viridis(begin = 0.2, end = 1, n=10)  # Viridis default color palette but avoiding deep-purple, and deep-blue

#### Plot ####
### Heatmap ###
countries <- countries %>% 
             bind_cols(ranking %>% select(ranking))

ggplot() + geom_sf(data = data_map, size = .3, fill = "grey10", color = "grey20") +
           geom_sf(data = countries, size = .3, aes(fill = ranking), color = "grey20") +
           geom_segment(data = ranking, aes(x = x_axis_start, y = ranking, xend = ranking_x, yend = ranking, color = ranking), alpha = .6, size = 1, lineend = "round") +                                     # Line/bar from xstart to value
           geom_segment(data = ranking, aes(x = x_axis_start -.5, y = min(ranking) -1, xend = x_axis_start -.5, yend = max(ranking) +1), alpha = 1, size = .9, color = Background, lineend = "round") +       # Ranking Y axis - color line
           geom_text(data = ranking, aes(x = x_axis_start -.6, y = ranking, label = country, color = ranking), hjust = 1, size = 3.0, nudge_x = -.5, nudge_y = .5) +                                          # Country text
           geom_text(data = ranking, aes(x = ranking_x, y = ranking, label = number_txt2, color = ranking), hjust = 0, size = 3, nudge_x = .8) +                                                              # Mountaniers number text
           geom_point(data = ranking %>% filter(country == "Nepal"), aes(x = X, y = Y), color = Background, fill = Everest, size = 2.5, shape = 24) +                                                         # Everest colored triangle 
           coord_sf(clip = "off") +
           scale_color_gradientn(colours = Palette) +
           scale_fill_gradientn(colours = Palette) +
           ### Theme ###
           theme_void() +
           theme(
           ## Text ##
                 text = element_text(color = Text, family="Roboto", size = 12),
           ## Panel ##
                 panel.background = element_rect(fill = "transparent", color = NA),
           ## Plot Aesthetic ##
                 plot.background = element_rect(fill = Background, color = NA),
                 plot.caption.position = "plot",
                 plot.caption = element_markdown(color = Caption, family="Menlo", hjust = 1, vjust = 0),
           ## Legend ##
                 legend.position = "none",
           ## Titles ##
                 plot.title.position = "plot",
                 plot.title = element_markdown(face = "plain", color = Title, family = "Ubuntu Condensed", size = 18, hjust = 0),
                 plot.subtitle = element_markdown(face = "plain", color = Title, family = "Ubuntu Condensed", size = 16, angle = 0),
           ## Margin ##
                 plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")) +
           ## Labels ##
                 labs(title = "<span style='font-size:20pt;color:#FDF0D5'>Everest</span> mountaniers nationalities",
                   subtitle = "Top 30 accumulated nationalities of <span style='font-size:18pt;color:#FDF0D5'>Everest</span> -non-hired- mountaineers",
                    caption = "<br>
                               <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                               <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                               Source: The Himalayan Database (Spring 2020)") 

#### Progress ####
ggsave(here("images", "progress", "imgs_2021_himalaya_P5", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 300, scale = 1, width = 18, height = 9, type = 'cairo')
