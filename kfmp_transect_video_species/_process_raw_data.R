################################################################################
##' @title Process raw data
##' @author Robin Elahi
##' @date 2026-01-05
##' @log 
################################################################################

#### File paths ####
here::i_am("kfmp_transect_video_species/_process_raw_data.R")
library(here)
folder <- "kfmp_transect_video_species"
file_name <- "_process_raw_data.R"

##### PACKAGES, DATA #####
library(tidyverse)
library(readxl)
source(here("R", "ggplot_settings.R"))

# Load raw data from video analysis
dat <- read_excel("data/kfmp_transect_video_species_260104.xlsx", na = "NA") 
glimpse(dat)
head(dat)
names(dat)

# Load video metadata
dat_meta <- read_excel("data/kfmp_transect_video_species_260104.xlsx", 
                       sheet = "metadata", na = "NA") 
glimpse(dat_meta)

##### WRANGLE DATA #####

# Create new taxon column, lumping species
dat <- dat |> 
  mutate(taxon = ifelse(is.na(lump1), taxon, lump1))
glimpse(dat)

lump_presence <- function(x)  {ifelse(x == 0, 0, 1)}
x <- c(0, 0, 1, 3, 2)
lump_presence(x)

# Summarize to new taxa
dat <- dat |> 
  group_by(taxon, phylum, trophic_level) |> 
  summarise(
    across(.cols = `bird_rock_2023-08-30_t1_k`:`hms110_2025-09-11_t1_b`, 
           .fns = sum)
  ) |> 
  ungroup() |> 
  arrange(phylum, taxon)

# dat |> filter(taxon == "red_aca_fg") |> View()

# Change to presence-absence
dat <- dat |> 
  mutate(across(.cols = `bird_rock_2023-08-30_t1_k`:`hms110_2025-09-11_t1_b`, 
                .fns = lump_presence))
  
# dat |> filter(taxon == "red_aca_fg") |> View()

##### MAKE VIDEO x SPECIES MATRIX #####
d_spp <- dat |> 
  select(-c(phylum:trophic_level)) |> 
  tibble::column_to_rownames(var = "taxon") %>% # Move 'Type' column to row names
  t() |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "video") |> 
  tibble() 

##### JOIN METADATA WITH NEW MATRIX #####

d <- left_join(dat_meta, d_spp, by = "video")

##### PREP FOR MDS BASED ON RAW DATA MATRIX #####

# Remove numeric columns that contain values >= 0
d_sub <- d %>%
  select(where(~ !is.numeric(.) || any(. > 0)))

## Create matrix (species x sample matrix) metadata
names(d_sub)
m_meta <- d_sub %>% select(video:sand)
m <- d_sub %>% select(-c(video:sand))
m_meta
m

