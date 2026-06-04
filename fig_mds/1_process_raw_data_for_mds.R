################################################################################
##' @title Process raw community data, lump to categories in manuscript
##' @author Robin Elahi
##' @date 2026-05-26
##' @log 
##' 2026-06-04: cleaned up code and folder structure
################################################################################

#### File paths ####
here::i_am("fig_mds/1_process_raw_data_for_mds.R")
library(here)
folder <- "fig_mds"
file_name <- "1_process_raw_data_for_mds.R"

##### PACKAGES, DATA #####
library(tidyverse)
library(readxl)
source(here("R", "ggplot_settings.R"))

# Load raw data from video analysis
dat <- read_excel("data/kfmp_transect_video_species_260526.xlsx", na = "NA") 
glimpse(dat)
head(dat)
names(dat)

# Load video metadata
dat_meta <- read_excel("data/kfmp_transect_video_species_260526.xlsx", 
                       sheet = "metadata", na = "NA") 
glimpse(dat_meta)

##### WRANGLE DATA #####

# Remove taxa
dat <- dat |> 
  filter(ms1 == "yes")

# Create new taxon column, lumping species
# lump_ms1
dat <- dat |> 
  mutate(taxon = ifelse(is.na(lump_ms1), taxon, lump_ms1))
glimpse(dat)

lump_presence <- function(x)  {ifelse(x == 0, 0, 1)}
x <- c(0, 0, 1, 3, 2)
lump_presence(x)

# Summarize to new taxa
dat <- dat |> 
  group_by(taxon, phylum, trophic_level, view) |> 
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
  
# Separate into benthic vs kelp datasets
dat_k <- dat |> 
  filter(view == "kelp")

dat_b <- dat |> 
  filter(view == "benthic")

##### MAKE VIDEO x SPECIES MATRIX - KELP #####
dat_k_wide <- dat_k |> 
  select(-c(phylum:view)) |> 
  tibble::column_to_rownames(var = "taxon") %>% # Move 'Type' column to row names
  t() |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "video") |> 
  tibble() 

dat_k_wide2 <- dat_meta |> 
  filter(view == "kelp") |> 
  left_join(dat_k_wide, by = "video")

d_wide <- dat_k_wide2

# Remove numeric columns that contain values >= 0
d_sub <- d_wide %>%
  select(where(~ !is.numeric(.) || any(. > 0)))

# Create matrix (species x sample matrix) metadata
names(d_sub)
m_meta <- d_sub %>% select(video:sand)
m <- d_sub %>% select(-c(video:sand))
m_meta
m

# Rename
kelp_meta <- m_meta
kelp_m <- m

##### MAKE VIDEO x SPECIES MATRIX - BENTHIC #####
dat_b_wide <- dat_b |> 
  select(-c(phylum:view)) |> 
  tibble::column_to_rownames(var = "taxon") %>% # Move 'Type' column to row names
  t() |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "video") |> 
  tibble() 

dat_b_wide2 <- dat_meta |> 
  filter(view == "benthic") |> 
  left_join(dat_b_wide, by = "video")

d_wide <- dat_b_wide2

# Remove numeric columns that contain values >= 0
d_sub <- d_wide %>%
  select(where(~ !is.numeric(.) || any(. > 0)))

# Create matrix (species x sample matrix) metadata
names(d_sub)
m_meta <- d_sub %>% select(video:sand)
m <- d_sub %>% select(-c(video:sand))
m_meta
m

# Rename
benthic_meta <- m_meta
benthic_m <- m

##### MAKE VIDEO x SPECIES MATRIX - COMBINED #####
d_spp <- dat |> 
  select(-c(phylum:view)) |> 
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

