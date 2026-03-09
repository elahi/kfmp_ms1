################################################################################
##' @title Summarize video species
##' @author Robin Elahi
##' @date 2026-01-05
##' @log 
################################################################################

here::i_am("kfmp_transect_video_species/summarize_video_species2.R")
library(here)

# Load processed raw data
source(here("kfmp_transect_video_species", "_process_raw_data.R"))
d # complete processed dataset
d_sub # only species with at least one observation
m # species matrix for vegan
m_meta # metadata

# File paths 
folder <- "kfmp_transect_video_species"
file_name <- "summarize_video_species2"

##### PACKAGES, DATA #####

##### WRANGLE DATA #####
names(d)

d_view <- d |> 
  group_by(view) |> 
  summarize(
    across(.cols = Arenicolid_egg:Semicossyphus_pulcher, 
           .fns = sum)
  )

d_view

d_view_wide <- d_view %>%
  tibble::column_to_rownames(var = "view") %>% # Move 'Type' column to row names
  t() %>%                                      # Transpose the data frame (becomes a matrix)
  as.data.frame() %>%                          # Convert the matrix back to a data frame
  tibble::rownames_to_column(var = "taxon") |> 
  tibble() |> 
  mutate(total = b + k) |> 
  arrange(desc(total))

d_view_wide

write.csv(d_view_wide, here("data_output", "kb_video_species2.csv"))
