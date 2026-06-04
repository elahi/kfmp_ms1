################################################################################
##' @title Summarize video species
##' @author Robin Elahi
##' @date 2026-01-03
##' @log 
################################################################################

#### File paths ####
here::i_am("kfmp_transect_video_species/summarize_video_species1.R")
library(here)
folder <- "kfmp_transect_video_species"
file_name <- "summarize_video_species1"

##### PACKAGES, DATA #####
library(tidyverse)
library(readxl)
source(here("R", "ggplot_settings.R"))

# Load data from video analysis
dat <- read_excel("data/kfmp_transect_video_species_260103.xlsx") 
glimpse(dat)
head(dat)

##### WRANGLE DATA #####
d <- dat |> 
  group_by(view) |> 
  summarize(
    across(.cols = Alaria_marginata:Watersipora_subtorquata, 
           .fns = sum)
  )

d

# Remove numeric columns that contain values <= 0
d2 <- d %>%
  select(where(~ !is.numeric(.) || any(. > 0)))

d_wide <- d2 %>%
  tibble::column_to_rownames(var = "view") %>% # Move 'Type' column to row names
  t() %>%                                      # Transpose the data frame (becomes a matrix)
  as.data.frame() %>%                          # Convert the matrix back to a data frame
  tibble::rownames_to_column(var = "taxon") |> 
  tibble() |> 
  mutate(total = b + k) |> 
  arrange(desc(total))

d_wide

write.csv(d_wide, here("data_output", "kb_video_species1.csv"))

#ggsave(paste(folder, "/figs/", file_name, "_b.pdf", sep = ""), height = 7, width = 11)
