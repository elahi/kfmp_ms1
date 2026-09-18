################################################################################
##' @title associations between Patiria/Macrocystis counts and rock cover
##' @author Julia DiCicco
##' @date 2026-05-03
##' @log 
##' 2026-07-29: cleaned up code
##' 2026-09-03: cleaned up code and folder structure
##' 2026-09-08: small edits by Robin Elahi
################################################################################

# File paths #
here::i_am("fig_habitat_association/habitat_plot.R")
library(here)
folder <- "fig_habitat_association"
file_name <- "habitat_plot"

##### PACKAGES, DATA #####
library(ggplot2)
library(readxl)
library(patchwork)

source(here("R", "ggplot_settings.R")) # loads kelp and patiria fill colors

# Load raw data #
rock_data <- read_excel(here("data", "habitat_association_data_260910.xlsx"), na = "NA")

##### WRANGLE DATA #####

# Subset data to include only variables used in figure creation
vars <- c("percent_rock", "video_patiria_min_counts", "video_macro_individuals")
small_rock_data <- na.omit(rock_data[vars])
no_barren_data <- subset(small_rock_data, video_macro_individuals > 0)

##### PLOT COUNTS BY PERCENT ROCK #####
# Percent rock cover and Macrosystis invididuals
kelp_panel <- ggplot(aes(x = percent_rock*100, y = video_macro_individuals), 
                     data = small_rock_data) +
  geom_point(size = 3, alpha = 0.7, pch = 21, fill = kelp_col) + 
  # geom_point(color = ifelse(small_rock_data$video_macro_individuals > 0, 
  #                           "black", "darkgray")) +
  # theme(plot.title = element_text(face = "italic")) +
  # labs(title = "Macrocystis pyrifera") + 
  geom_text(aes(x = -Inf, y = Inf, label = "A"), 
            hjust = -0.5, #right - left shifting (more - = farther right)
            vjust = 1.5, size = 5, fontface = "bold") +
  expand_limits(x = 0, y = 0) + 
  labs(x = "Rocky substratum (%)", 
       y = expression(Total~count ~ (per ~ 60 ~ m^2)))

kelp_panel

# Percent rock cover and Patiria individuals
star_panel <- ggplot(aes(x = percent_rock*100, y = video_patiria_min_counts), 
                     data = small_rock_data) +
  geom_point(size = 3, alpha = 0.7, pch = 21, fill = pati_col) + 
  # geom_point(color = ifelse(small_rock_data$video_macro_individuals > 0, 
  #                           "black", "darkgray")) +
  # theme(plot.title = element_text(face = "italic")) +
  # labs(title = "Patiria miniata") + 
  geom_text(aes(x = -Inf, y = Inf, label = "B"), 
            hjust = -0.5, #right - left shifting (more - = farther right)
            vjust = 1.5, size = 5, fontface = "bold") +
  expand_limits(x = 0, y = 0) + 
  labs(x = "Rocky substratum (%)", 
       y = expression(Total~count ~ (per ~ 60 ~ m^2)))

star_panel

# combine kelp and star panels into one figure using the patchwork package
kelp_panel + star_panel + plot_layout(axes = "collect") 

ggsave(paste(folder, "/figs/", file_name, "_a.pdf", sep = ""), height = 3.5, width = 7)
ggsave(paste(folder, "/figs/", file_name, "_a.jpg", sep = ""), height = 3.5, width = 7)

##### STATISTICAL ANALYSIS #####
# Correlation for each panel
cor.test(x = small_rock_data$percent_rock, y = small_rock_data$video_macro_individuals)
summary(lm(small_rock_data$video_macro_individuals 
           ~ small_rock_data$percent_rock)) 

cor.test(x = small_rock_data$percent_rock, y = small_rock_data$video_patiria_min_counts)
summary(lm(small_rock_data$video_patiria_min_counts 
           ~ small_rock_data$percent_rock)) 

# Exclue 0 kelp transects (all caused by urchins? not confirmed; do not use)
summary(lm(no_barren_data$video_macro_individuals 
           ~ no_barren_data$percent_rock)) 
