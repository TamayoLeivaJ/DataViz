#### (2021 - Data Visualization - Himalayan database - Plot 4) #### 
#### ----------------------------------------- ####

#### set up ####
#### Libraries ####
library(readr)
library(here)
library(tidyverse)
library(lubridate)
library(ggtext)
library(systemfonts)
library(ggbump)
library(ggsci)
library(grid)
library(gridtext)
library(patchwork)

#### Directory ####
# create directory to save all progress plots
dir.create(here("images", "progress", "imgs_2021_himalaya_P4"), recursive = TRUE, mode = "0755")

#### Read data ####
expeditions <- read_csv("./himalayan_expeditions/ddbb_2021_himalaya/expeditions.csv")
members <- read_csv("./himalayan_expeditions/ddbb_2021_himalaya/members.csv")

#### Data wrangling ####
##### Everest Tourists Citizenship Summary (top 20 nationalities)
citizenship_summary <- members %>%
                       filter(peak_name == "Everest" & !is.na(citizenship) & hired == FALSE) %>%
                       select(citizenship) %>%
                       add_count(citizenship, name = "citizenship_number") %>%
                       distinct(citizenship, citizenship_number) %>%
                       slice_max(order_by = citizenship_number, n = 20) %>%                         # Select top 20 nationalities
                       arrange(citizenship_number)

countries <- factor(levels = citizenship_summary$citizenship) 

##### Everest Tourists Top 20 Citizenship Number by Year since 1921 to 2020
citizenship <- expeditions %>% 
               left_join(members, by = c("expedition_id","peak_id","peak_name","year","season","highpoint_meters","oxygen_used")) %>%
               filter(peak_name == "Everest" & !is.na(citizenship) & hired == FALSE) %>%            # Exclude members without noted nationality or hired personnel
               select(citizenship, year) %>% 
               add_count(year, citizenship, name = "citizenship_by_year") %>%
               distinct(citizenship, year, citizenship_by_year) %>%
               filter(citizenship %in% levels(countries)) %>%                                       # Filter only the top 20 nationalities  
               mutate(citizenship = fct_relevel(citizenship, levels(countries))) %>%                # Relevel citizenship by the higher total number of people of each country
               complete(citizenship, year = full_seq(year,1), fill = list(citizenship_by_year = 0)) # Complete implicitly, i.e. simply not present in the data observation

##### Everest Tourists Top 10 Citizenship Rank per Years Period. 
# Ranks above 10 will be set to 11.
citizenship_rk <- citizenship %>%
                  #filter(citizenship %in% levels(countries)[10:20]) %>%
                  mutate(Period = case_when(year < 2000 ~ "1921 - 1999",
                                            year >= 2000 & year <= 2005 ~ "2000 - 2005",
                                            year > 2005 & year <= 2010 ~ "2006 - 2010",
                                            year > 2010 & year <= 2015 ~ "2011 - 2015",
                                            year > 2015 & year <= 2020 ~ "2016 - 2020")) %>%
                  mutate(order = as.numeric(recode(Period, "1921 - 1999" = "1",
                                                           "2000 - 2005" = "2",
                                                           "2006 - 2010" = "3",
                                                           "2011 - 2015" = "4",
                                                           "2016 - 2020" = "5"))) %>%
                  group_by(Period, order, citizenship) %>%
                  summarise(citizenship_by_period = sum(citizenship_by_year)) %>%
                  mutate(rank = rank(-citizenship_by_period, ties.method = "random")) %>%
                  group_by(citizenship) %>%
                  mutate(any_top_10 = any(rank <= 10)) %>% 
                  ungroup() %>%
                  mutate(rank = if_else(rank > 10, 11L, rank)) %>% 
                  filter(any_top_10 == TRUE)

citizenship_rk <- citizenship_rk %>%
                  group_by(citizenship) %>%
                  mutate(first_top10 = min(order[rank <= 10]),
                         last_top10 = max(order[rank <= 10]),
                         d_first_top10 = if_else(order == first_top10, 1, 0)) %>%
                  filter(!is.na(first_top10), order >= first_top10, order <= last_top10) %>%
                  ungroup() %>%
                  arrange(citizenship, order) %>%
                  group_by(citizenship) %>%
                  mutate(lag_zero = if_else(lag(rank) %in% c(11, NA) & rank <= 10, 1, 0, 0)) %>% 
                  ungroup() %>% 
                  mutate(group = cumsum(lag_zero))


#### Plot Aesthetics ####
Background <- c("#0A1C40")
Title <- c("#E9F1F2")
Text <- c("#F2F2F2")
Caption <- c("grey60")

#### Plot ####
### Heatmap ###
left <- citizenship %>% ggplot(aes(x = year, y = citizenship, fill = citizenship_by_year)) +
                         geom_tile(color = Background, size = 0.25) +
                         scale_x_continuous(limits = c(1921,2020), breaks = c(1921, seq(1930,2020,10)), expand=c(0,0)) +
                         scale_fill_viridis_c(option = "inferno", trans ="sqrt") +
                         guides(fill = guide_colorbar(title.position = "bottom", title.hjust = .5, barwidth = unit(20, "lines"), barheight = unit(.5, "lines"))) +
                         coord_cartesian(clip = "off") +
                         ### Theme ###
                         theme_void() +
                         theme(
                         ## Text ##
                               text = element_text(color = Text, size = 12, family = "Oswald"),
                               axis.text.x = element_text(face = "bold", color = Text, size = 12, hjust = 0.5, vjust = 0.5, angle = 0),
                               axis.text.y = element_text(face = "bold", color = Text, size = 12, hjust = 1, vjust = 0.5, angle = 0),
                         ## Panel ##
                               panel.background = element_rect(fill = "transparent", color = NA),
                               axis.ticks.x.bottom = element_line(color = Text),
                               axis.ticks.length.x.bottom = unit(.25, "cm"),
                         ## Plot Aesthetic ##
                               plot.background = element_rect(fill = Background, color = NA),
                         ## Legend ##
                               legend.position = "bottom",
                               legend.margin = margin(t = 0.3, r = 0.1, b = 0.1, l = 0.1, unit = "cm"),
                         ## Margin ##
                               plot.margin = margin(t = 0.3, r = 0.1, b = 0.1, l = 0.1, unit = "cm")) +
                         labs(fill = "Number of Mountaineers")
 
### Rank ###
right <- citizenship_rk %>% ggplot(aes(x = order, y = rank, color = citizenship, group = citizenship)) +
                             geom_bump(smooth = 15, size = 2, alpha = 0.2) +
                             geom_bump(data = citizenship_rk %>% filter(rank <= 10), 
                                       aes(order, rank, group = group, color = citizenship), smooth = 15, size = 2, inherit.aes = F) +
                             geom_segment(data = citizenship_rk %>% filter(rank <=10), 
                                          aes(x = order, xend = order, y = rank, yend = rank), size = 2, lineend = "round") +
                             geom_point(data = citizenship_rk %>% filter(d_first_top10 == 1), aes(x = order), size = 5) +
                             geom_text(data = citizenship_rk %>% filter(d_first_top10 == 1 & citizenship != "Norway"), family="Oswald", 
                                       aes(label = citizenship, x = order), color = "white", nudge_y = -.43, nudge_x = 0, size = 3.0, fontface = 2, hjust = 0.5) +
                             geom_text(data = citizenship_rk %>% filter(order == max(order)), family="Oswald",
                                       aes(label = citizenship), color = "white", nudge_x = .1, hjust = 0, size = 3.5, fontface = 2) +
                             geom_point(data = tibble(x = 0.55, y = 1:10), aes(x = x, y = y), inherit.aes = F, color = "white", size = 10, pch = 21) +
                             geom_text(data = tibble(x = .55, y = 1:10), aes(x = x, y = y, label = y), family="Oswald", inherit.aes = F, color = "white", fontface= 2) +
                             scale_y_reverse(breaks =seq(1, 10, 1), limits =c(11, 1)) +
                             scale_x_continuous(breaks = citizenship_rk$order %>% unique() %>% sort(),
                                                labels = citizenship_rk %>% distinct(order, Period) %>% arrange(order) %>% pull(Period)) +
                             scale_color_d3("category20c") +
                             coord_cartesian(clip = "off") +
                             ### Theme ###
                             theme_void() +
                             theme(
                             ## Text ##
                                   text=element_text(color = Text, size = 12, family = "Oswald"),
                                   axis.text.x = element_text(face = "bold", color = Text, size = 12, hjust = 0.5, vjust = 0.5, angle = 0),
                                   axis.text.y = element_blank(),
                             ## Panel ##
                                   panel.background = element_rect(fill = "transparent", color = NA),
                             ## Plot Aesthetic ##
                                   plot.background = element_rect(fill = Background, color = NA),
                             ## Legend ##
                             legend.position = "none",
                             ## Margin ##
                                   plot.margin = margin(t = 0.3, r = 0.1, b = 0.1, l = 0.1, unit = "cm"))

### Annotation ###
Annotation <- richtext_grob("The **Himalayan mountain** span about **2,400 km** (1.500 miles), stretching <br>
                             across **Southeast Asia** throughout the nations of **India**, **Pakistan**, **Afgh-<br>
                             anistan**, **China**, **Bhutan**, and **Nepal**. Over **six thousand Nepalese** have <br>
                             been **hired** to work at **Everest**. However, Nepalese people only have been <br>
                             in the top 10 tourists in the last 15 years, where they have been a rising <br>
                             nation. Chinese and Indian have been also hired (just above four hun-<br>
                             dred.) But in the XXI century, they have been claiming their position <br>
                             in the region, as the top nations at the Everest.",
                             gp=gpar(fontfamily = "Roboto Slab", fontsize = 14, col = Title),
                             halign = 0,
                             padding = unit(c(10, 10, 10, 10), "pt"),
                             margin = unit(c(0, 20, 0, 80), "pt"))
                   
## Final Plot ##
### Desing ###
design <- "12
           12
           12
           13"

### Dimmension ###
left_dim <- get_dim(left)
set_dim(right, left_dim)

### Patchwork ###
(left | right | wrap_elements(full = Annotation) + 
                theme(plot.background = element_rect(fill = Background, color = NA))) +
                plot_layout(design = design) +
                plot_annotation(  title = "<span style='font-family: \"Font Awesome 5 Free Solid\"'>&#xf6fc;</span> Nations on Everest: Mountaineers nationalities throughout the time", 
                               subtitle = "English has been the second language, but locals are claiming their places. Top 20 accumulated nationalities <br>
                                           of Everest -non-hired- mountaineers (left.) Top 10 mountaineers nationalities throughout major periods (right.)",
                                caption = "<br>
                                           <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> @TamayoLeiva_J<br>
                                           <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> TamayoLeivaJ<br><br> 
                                           Source: The Himalayan Database (Spring 2020)",
                                  theme = theme(
                                                ## Titles ##
                                                  plot.title.position = "plot",
                                                  plot.title=element_markdown(face = "bold", color = Title, family = "Bebas Neue", size = 24, hjust = 0),
                                                  plot.subtitle =element_markdown(face = "bold", color = Title, family = "Bebas Neue", size = 18, hjust = 0),
                                               ## Plot Aesthetic ##
                                                  plot.background = element_rect(fill = Background, color = NA),
                                                  plot.caption = element_markdown(color = Caption, family="Menlo", hjust = 1, vjust = 0),
                                                  plot.caption.position = "plot",
                                               ## Margin ##
                                                  plot.margin = margin(t = 0.25, r = 0.8, b = 0.25, l = 0.4, unit = "cm")))
               
#### Progress ####
ggsave(here("images", "progress", "imgs_2021_himalaya_P4", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 300, scale = 1, width = 18, height = 9, type = 'cairo')
